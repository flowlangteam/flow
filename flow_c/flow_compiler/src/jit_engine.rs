use crate::{CompilerConfig, CompilerError, Result};
use cranelift::codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift::codegen::ir::{BlockArg, MemFlags, StackSlot};
use cranelift::codegen::settings;
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use flow_ast::*;
use std::collections::{HashMap, HashSet};
use std::mem;
use thiserror::Error;

// External C functions for standard library
extern "C" {
    fn putchar(c: i32) -> i32;
    fn printf(format: *const i8, ...) -> i32;
    fn malloc(size: usize) -> *mut u8;
    fn free(ptr: *mut u8);
    fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8;
}

/// Information about an external module's functions
#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub name: String,
    pub mangled_name: String,
    pub params: Vec<flow_ast::Type>,
    pub return_type: Option<flow_ast::Type>,
    pub namespace: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ExternalModule {
    pub name: String,
    pub functions: Vec<ExternalFunction>,
}

/// Module registry for tracking compiled modules and their exports
#[derive(Debug, Clone)]
pub struct ModuleRegistry {
    modules: HashMap<String, ExternalModule>,
}

impl Default for ModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleRegistry {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Register a compiled module with its exported functions
    pub fn register_module(&mut self, module: ExternalModule) {
        self.modules.insert(module.name.clone(), module);
    }

    /// Look up a module by name
    pub fn get_module(&self, name: &str) -> Option<&ExternalModule> {
        self.modules.get(name)
    }

    /// Find a function across all registered modules
    pub fn find_function(&self, name: &str) -> Option<&ExternalFunction> {
        self.modules
            .values()
            .find_map(|module| module.functions.iter().find(|func| func.name == name))
    }
}

#[derive(Debug, Error)]
pub enum JitError {
    #[error("Cranelift error: {0}")]
    Cranelift(String),
    #[error("Module registry error: {0}")]
    ModuleRegistry(String),
    #[error("{0}")]
    Message(String),
}

pub type JitResult<T> = std::result::Result<T, JitError>;

impl From<JitError> for CompilerError {
    fn from(err: JitError) -> Self {
        CompilerError::JitError(err.to_string())
    }
}

impl From<String> for JitError {
    fn from(err: String) -> Self {
        JitError::Message(err)
    }
}

impl From<&str> for JitError {
    fn from(err: &str) -> Self {
        JitError::Message(err.to_string())
    }
}

#[derive(Debug, Clone)]
struct StructLayout {
    fields: Vec<(String, flow_ast::Type, usize)>,
    total_size: usize,
    alignment: usize,
}

#[derive(Clone)]
enum VariableBinding {
    Immutable {
        value: Value,
        flow_type: Option<flow_ast::Type>,
    },
    Mutable {
        slot: StackSlot,
        ty: types::Type,
        flow_type: Option<flow_ast::Type>,
    },
}

impl VariableBinding {
    fn read(&self, builder: &mut FunctionBuilder) -> Value {
        match self {
            VariableBinding::Immutable { value, .. } => *value,
            VariableBinding::Mutable { slot, ty, .. } => builder.ins().stack_load(*ty, *slot, 0),
        }
    }

    fn flow_type(&self) -> Option<&flow_ast::Type> {
        match self {
            VariableBinding::Immutable { flow_type, .. }
            | VariableBinding::Mutable { flow_type, .. } => flow_type.as_ref(),
        }
    }
}

struct Environment {
    scopes: Vec<HashMap<String, VariableBinding>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(HashMap::new());
        }
    }

    fn insert(&mut self, name: String, binding: VariableBinding) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, binding);
        }
    }

    fn get(&self, name: &str) -> Option<&VariableBinding> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

struct CodegenState<'a, M: Module> {
    module: &'a mut M,
    builder_context: &'a mut FunctionBuilderContext,
    ctx: &'a mut Context,
    struct_layouts: &'a mut HashMap<String, StructLayout>,
    struct_defs: &'a mut HashMap<String, Struct>,
    string_data: &'a mut HashMap<String, DataId>,
    string_counter: &'a mut usize,
    function_ids: &'a mut HashMap<String, FuncId>,
    function_sigs: &'a mut HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    current_namespace: &'a mut Option<String>,
    module_registry: &'a mut ModuleRegistry,
    pointer_type: types::Type,
}

impl<'a, M: Module> CodegenState<'a, M> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        module: &'a mut M,
        builder_context: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
        struct_layouts: &'a mut HashMap<String, StructLayout>,
        struct_defs: &'a mut HashMap<String, Struct>,
        string_data: &'a mut HashMap<String, DataId>,
        string_counter: &'a mut usize,
        function_ids: &'a mut HashMap<String, FuncId>,
        function_sigs: &'a mut HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
        current_namespace: &'a mut Option<String>,
        module_registry: &'a mut ModuleRegistry,
    ) -> Self {
        let pointer_type = module.target_config().pointer_type();
        Self {
            module,
            builder_context,
            ctx,
            struct_layouts,
            struct_defs,
            string_data,
            string_counter,
            function_ids,
            function_sigs,
            current_namespace,
            module_registry,
            pointer_type,
        }
    }

    fn pointer_type(&self) -> types::Type {
        self.pointer_type
    }

    fn pointer_size(&self) -> usize {
        self.pointer_type.bytes() as usize
    }

    fn register_runtime_intrinsics(&mut self) -> JitResult<()> {
        let byte_type = flow_ast::Type::U8;
        let pointer_to_byte = flow_ast::Type::Pointer(Box::new(byte_type.clone()));
        let mut_pointer_to_byte = flow_ast::Type::MutPointer(Box::new(byte_type));

        self.register_external_function(
            "malloc",
            vec![flow_ast::Type::U64],
            Some(mut_pointer_to_byte.clone()),
        )?;
        self.register_external_function("free", vec![mut_pointer_to_byte.clone()], None)?;
        self.register_external_function(
            "memcpy",
            vec![
                mut_pointer_to_byte.clone(),
                pointer_to_byte.clone(),
                flow_ast::Type::U64,
            ],
            Some(mut_pointer_to_byte),
        )?;

        Ok(())
    }

    fn register_external_function(
        &mut self,
        name: &str,
        params: Vec<flow_ast::Type>,
        return_type: Option<flow_ast::Type>,
    ) -> JitResult<()> {
        let mut sig = self.module.make_signature();
        let pointer_type = self.pointer_type();

        for param_type in &params {
            let cranelift_type = type_to_cranelift(param_type, pointer_type, None)?;
            sig.params.push(AbiParam::new(cranelift_type));
        }

        if let Some(ret_type) = &return_type {
            let cranelift_ret_type = type_to_cranelift(ret_type, pointer_type, None)?;
            sig.returns.push(AbiParam::new(cranelift_ret_type));
        }

        let func_id = self
            .module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| {
                JitError::Cranelift(format!(
                    "failed to declare external function '{}': {}",
                    name, e
                ))
            })?;

        self.function_ids.insert(name.to_string(), func_id);
        self.function_sigs
            .insert(name.to_string(), (params, return_type));

        Ok(())
    }

    fn find_function_entry(&self, name: &str) -> Option<(String, FuncId)> {
        if let Some(id) = self.function_ids.get(name) {
            return Some((name.to_string(), *id));
        }

        let resolved = self.resolve_function_name(name);
        if let Some(id) = self.function_ids.get(&resolved) {
            return Some((resolved, *id));
        }

        None
    }

    fn resolve_function_name(&self, name: &str) -> String {
        if name.contains("::") {
            name.to_string()
        } else if let Some(namespace) = self.current_namespace.as_ref() {
            format!("{}::{}", namespace, name)
        } else {
            name.to_string()
        }
    }

    fn reset_struct_state(&mut self) {
        self.struct_defs.clear();
        self.struct_layouts.clear();
    }

    fn register_struct_definitions(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                self.struct_defs
                    .insert(struct_def.name.clone(), struct_def.clone());
            }
        }
    }

    fn compute_all_struct_layouts(&mut self, program: &Program) -> JitResult<()> {
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                let layout = self
                    .compute_struct_layout(struct_def)
                    .map_err(JitError::from)?;
                self.struct_layouts.insert(struct_def.name.clone(), layout);
            }
        }
        Ok(())
    }

    fn declare_all_functions(&mut self, program: &Program) -> JitResult<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func).map_err(JitError::from)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.declare_function(method).map_err(JitError::from)?;
                        self.register_method_aliases(&impl_block.struct_name, method)
                            .map_err(JitError::from)?;
                    }
                }
                Item::ExternBlock(block) => {
                    for extern_item in &block.items {
                        self.register_external_function(
                            &extern_item.name,
                            extern_item.params.clone(),
                            extern_item.return_type.clone(),
                        )?;
                    }
                }
                Item::Use(_) | Item::Attribute(_) => {}
                _ => {}
            }
        }
        Ok(())
    }

    fn compile_all_functions(&mut self, program: &Program) -> JitResult<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.compile_function(func).map_err(JitError::from)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.compile_function(method).map_err(JitError::from)?;
                    }
                }
                Item::Struct(_) | Item::ExternBlock(_) | Item::Use(_) | Item::Attribute(_) => {}
                Item::Import(import) => {
                    let module_name = import.path.join("::");

                    if let Some(module_info) = self.module_registry.get_module(&module_name) {
                        let functions_to_register: Vec<_> = module_info
                            .functions
                            .iter()
                            .map(|func| {
                                (
                                    func.name.clone(),
                                    func.params.clone(),
                                    func.return_type.clone(),
                                )
                            })
                            .collect();

                        for (func_name, params, return_type) in functions_to_register {
                            self.register_external_function(&func_name, params, return_type)?;
                        }
                    } else {
                        return Err(JitError::ModuleRegistry(format!(
                            "module '{}' not found in module registry",
                            module_name
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    fn compile_program(&mut self, program: &Program) -> JitResult<String> {
        if let Some(namespace_decl) = &program.namespace {
            *self.current_namespace = Some(namespace_decl.namespace.clone());
        }

        self.reset_struct_state();
        self.register_struct_definitions(program);
        self.compute_all_struct_layouts(program)?;
        self.declare_all_functions(program)?;
        self.compile_all_functions(program)?;

        Ok(self.resolve_function_name("main"))
    }

    fn compile_module(&mut self, program: &Program) -> JitResult<()> {
        if let Some(namespace_decl) = &program.namespace {
            *self.current_namespace = Some(namespace_decl.namespace.clone());
        }

        self.reset_struct_state();
        self.register_struct_definitions(program);
        self.compute_all_struct_layouts(program)?;
        self.declare_all_functions(program)?;
        self.compile_all_functions(program)?;

        if let Some(namespace) = self.current_namespace.clone() {
            let external_module = self
                .extract_module_metadata(&namespace, program)
                .map_err(JitError::from)?;
            self.module_registry.register_module(external_module);
        }

        Ok(())
    }

    fn extract_module_metadata(
        &self,
        namespace: &str,
        program: &Program,
    ) -> std::result::Result<ExternalModule, String> {
        let mut functions = Vec::new();

        for item in &program.items {
            match item {
                Item::Function(func) if func.is_pub => {
                    let external_func = ExternalFunction {
                        name: func.name.clone(),
                        mangled_name: self.resolve_function_name(&func.name),
                        params: func.params.iter().map(|p| p.ty.clone()).collect(),
                        return_type: func.return_type.clone(),
                        namespace: Some(namespace.to_string()),
                    };
                    functions.push(external_func);
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        if method.is_pub {
                            let external_func = ExternalFunction {
                                name: method.name.clone(),
                                mangled_name: self.resolve_function_name(&method.name),
                                params: method.params.iter().map(|p| p.ty.clone()).collect(),
                                return_type: method.return_type.clone(),
                                namespace: Some(namespace.to_string()),
                            };
                            functions.push(external_func);
                        }
                    }
                }
                Item::Attribute(_) => {}
                _ => {}
            }
        }

        Ok(ExternalModule {
            name: namespace.to_string(),
            functions,
        })
    }

    fn compute_struct_layout(
        &mut self,
        struct_def: &Struct,
    ) -> std::result::Result<StructLayout, String> {
        if let Some(layout) = self.struct_layouts.get(&struct_def.name) {
            return Ok(layout.clone());
        }

        let mut visited = HashSet::new();
        self.compute_struct_layout_inner(struct_def, &mut visited)
    }

    fn compute_struct_layout_inner(
        &mut self,
        struct_def: &Struct,
        visited: &mut HashSet<String>,
    ) -> std::result::Result<StructLayout, String> {
        if !visited.insert(struct_def.name.clone()) {
            return Err(format!(
                "Recursive struct definition '{}' is not supported",
                struct_def.name
            ));
        }

        let mut offset = 0usize;
        let mut max_alignment = 1usize;
        let mut fields_layout = Vec::with_capacity(struct_def.fields.len());

        for field in &struct_def.fields {
            let (field_size, field_alignment) = self.compute_type_layout(&field.ty, visited)?;

            if field_alignment == 0 {
                return Err(format!(
                    "Field '{}' in struct '{}' has invalid alignment",
                    field.name, struct_def.name
                ));
            }

            offset = align_to(offset, field_alignment);
            fields_layout.push((field.name.clone(), field.ty.clone(), offset));
            offset += field_size;
            max_alignment = max_alignment.max(field_alignment);
        }

        let total_size = if struct_def.fields.is_empty() {
            0
        } else {
            align_to(offset, max_alignment)
        };

        visited.remove(&struct_def.name);

        let layout = StructLayout {
            fields: fields_layout,
            total_size,
            alignment: max_alignment.max(1),
        };

        self.struct_layouts
            .insert(struct_def.name.clone(), layout.clone());

        Ok(layout)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn compute_type_layout(
        &mut self,
        ty: &flow_ast::Type,
        visited: &mut HashSet<String>,
    ) -> std::result::Result<(usize, usize), String> {
        use flow_ast::Type;

        let pointer_size = self.pointer_size();

        match ty {
            Type::I8 | Type::U8 | Type::Bool => Ok((1, 1)),
            Type::I16 | Type::U16 => Ok((2, 2)),
            Type::I32 | Type::U32 | Type::F32 => Ok((4, 4)),
            Type::I64 | Type::U64 | Type::F64 | Type::Char => Ok((8, 8)),
            Type::String
            | Type::Function(_, _)
            | Type::Pointer(_)
            | Type::MutPointer(_)
            | Type::Slice(_) => Ok((pointer_size, pointer_size)),
            Type::Array(inner, len) => {
                let (elem_size, elem_align) = self.compute_type_layout(inner, visited)?;
                let aligned_elem_size = align_to(elem_size, elem_align);
                Ok((aligned_elem_size * len, elem_align))
            }
            Type::Named(name) => {
                if !self.struct_layouts.contains_key(name) && !self.struct_defs.contains_key(name) {
                    return Err(format!("Unknown struct type '{}'", name));
                }

                Ok((pointer_size, pointer_size))
            }
            Type::Unit => Ok((0, 1)),
            Type::TypeVar(name) => Err(format!(
                "Type variable '{}' cannot be used in layout computation",
                name
            )),
            Type::I128 | Type::U128 => {
                Err("128-bit integers are not yet supported in struct layouts".to_string())
            }
        }
    }

    fn declare_function(&mut self, func: &Function) -> std::result::Result<(), String> {
        let pointer_type = self.pointer_type();

        let mut sig = self.module.make_signature();

        let mut param_types = Vec::new();
        for param in &func.params {
            let param_type = type_to_cranelift(&param.ty, pointer_type, Some(self.struct_layouts))
                .map_err(|e| format!("Failed to convert parameter type: {}", e))?;
            sig.params.push(AbiParam::new(param_type));
            param_types.push(param.ty.clone());
        }

        let return_type = if let Some(return_type) = &func.return_type {
            let ret_type = type_to_cranelift(return_type, pointer_type, Some(self.struct_layouts))
                .map_err(|e| format!("Failed to convert return type: {}", e))?;
            sig.returns.push(AbiParam::new(ret_type));
            Some(return_type.clone())
        } else {
            None
        };

        let symbol_name = self.resolve_function_name(&func.name);

        let func_id = self
            .module
            .declare_function(&symbol_name, Linkage::Export, &sig)
            .map_err(|e| format!("Failed to declare function: {}", e))?;

        let signature_entry = (param_types.clone(), return_type.clone());

        self.function_ids.insert(symbol_name.clone(), func_id);
        self.function_ids.insert(func.name.clone(), func_id);
        self.function_sigs
            .insert(symbol_name, signature_entry.clone());
        self.function_sigs
            .insert(func.name.clone(), signature_entry);

        Ok(())
    }

    fn register_method_aliases(
        &mut self,
        struct_name: &str,
        method: &Function,
    ) -> std::result::Result<(), String> {
        let alias = format!("{}::{}", struct_name, method.name);
        let (base_key, func_id) = self
            .find_function_entry(&method.name)
            .ok_or_else(|| format!("Method {} not declared", method.name))?;

        let signature = self
            .function_sigs
            .get(&base_key)
            .cloned()
            .ok_or_else(|| format!("No signature found for method {}", base_key))?;

        self.function_ids.insert(alias.clone(), func_id);
        self.function_sigs.insert(alias.clone(), signature.clone());

        if let Some(namespace) = self.current_namespace.as_ref() {
            let namespaced_alias = format!("{}::{}", namespace, alias);
            self.function_ids.insert(namespaced_alias.clone(), func_id);
            self.function_sigs.insert(namespaced_alias, signature);
        }

        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> std::result::Result<(), String> {
        let (lookup_key, func_id) = self
            .find_function_entry(&func.name)
            .ok_or_else(|| format!("Function {} not declared", func.name))?;

        let (param_types, return_type) = self
            .function_sigs
            .get(&lookup_key)
            .cloned()
            .ok_or_else(|| format!("No signature found for function {}", lookup_key))?;

        let pointer_type = self.pointer_type();

        let mut sig = self.module.make_signature();
        for param_type in &param_types {
            let cranelift_type =
                type_to_cranelift(param_type, pointer_type, Some(self.struct_layouts))
                    .map_err(|e| format!("Failed to convert parameter type: {}", e))?;
            sig.params.push(AbiParam::new(cranelift_type));
        }
        if let Some(ret_type) = &return_type {
            let cranelift_ret_type =
                type_to_cranelift(ret_type, pointer_type, Some(self.struct_layouts))
                    .map_err(|e| format!("Failed to convert return type: {}", e))?;
            sig.returns.push(AbiParam::new(cranelift_ret_type));
        }

        self.ctx.clear();
        self.ctx.func = cranelift::codegen::ir::Function::with_name_signature(
            cranelift::codegen::ir::UserFuncName::user(0, 0),
            sig,
        );

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, self.builder_context);

        let entry_block = builder.create_block();

        for param_type in &param_types {
            let cranelift_type =
                type_to_cranelift(param_type, pointer_type, Some(self.struct_layouts))
                    .map_err(|e| format!("Failed to convert parameter type: {}", e))?;
            builder.append_block_param(entry_block, cranelift_type);
        }

        builder.switch_to_block(entry_block);

        let params = builder.block_params(entry_block).to_vec();
        let mut variables = Environment::new();
        for (i, param) in func.params.iter().enumerate() {
            if i < params.len() {
                let value = params[i];
                let flow_ty = param_types.get(i).cloned();

                variables.insert(
                    param.name.clone(),
                    VariableBinding::Immutable {
                        value,
                        flow_type: flow_ty,
                    },
                );
            }
        }

        builder.seal_block(entry_block);

        let result_value = {
            let mut expr_ctx = ExpressionContext {
                module: self.module,
                string_data: self.string_data,
                string_counter: self.string_counter,
                function_ids: self.function_ids,
                function_sigs: self.function_sigs,
                namespace: self.current_namespace.as_deref(),
                pointer_type,
                struct_layouts: self.struct_layouts,
                current_return_type: return_type.as_ref(),
            };

            compile_expression(
                &func.body,
                &mut builder,
                &mut variables,
                &mut expr_ctx,
                return_type.as_ref(),
            )?
        };

        if return_type.is_some() {
            builder.ins().return_(&[result_value]);
        } else {
            builder.ins().return_(&[]);
        }

        builder.finalize();

        self.module
            .define_function(func_id, self.ctx)
            .map_err(|e| format!("Failed to define function: {}", e))?;

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct EngineSettings {
    opt_level: settings::OptLevel,
    enable_verifier: bool,
    is_pic: bool,
}

impl Default for EngineSettings {
    fn default() -> Self {
        Self {
            opt_level: settings::OptLevel::None,
            enable_verifier: false,
            // JIT mode always requires is_pic=false
            is_pic: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ObjectCompileOutput {
    pub object_bytes: Vec<u8>,
    pub struct_layouts: HashMap<String, (usize, usize)>,
    pub namespace: Option<String>,
    pub entry_symbol: Option<String>,
}

pub struct ObjectEngine {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    module: ObjectModule,
    struct_layouts: HashMap<String, StructLayout>,
    struct_defs: HashMap<String, Struct>,
    string_data: HashMap<String, DataId>,
    string_counter: usize,
    function_ids: HashMap<String, FuncId>,
    function_sigs: HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    current_namespace: Option<String>,
    module_registry: ModuleRegistry,
    settings: EngineSettings,
}

impl ObjectEngine {
    pub fn new() -> Self {
        Self::new_with_settings(EngineSettings::default())
            .expect("failed to create object engine with default settings")
    }

    fn new_with_settings(engine_settings: EngineSettings) -> Result<Self> {
        let module = Self::create_module(&engine_settings)?;

        let mut engine = Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            struct_layouts: HashMap::new(),
            struct_defs: HashMap::new(),
            string_data: HashMap::new(),
            string_counter: 0,
            function_ids: HashMap::new(),
            function_sigs: HashMap::new(),
            current_namespace: None,
            module_registry: ModuleRegistry::new(),
            settings: engine_settings,
        };

        engine.reset_state()?;
        Ok(engine)
    }

    fn map_error(err: CompilerError) -> CompilerError {
        match err {
            CompilerError::JitError(msg) => CompilerError::AotError(msg),
            other => other,
        }
    }

    fn map_jit(err: JitError) -> CompilerError {
        CompilerError::AotError(err.to_string())
    }

    fn create_module(settings: &EngineSettings) -> Result<ObjectModule> {
        let mut flag_builder = settings::builder();
        settings
            .apply_flags(&mut flag_builder)
            .map_err(Self::map_error)?;

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let flags = settings::Flags::new(flag_builder);
        let isa = isa_builder
            .finish(flags)
            .map_err(|err| CompilerError::AotError(err.to_string()))?;

        let builder = ObjectBuilder::new(
            isa,
            "flow_object".to_string(),
            cranelift_module::default_libcall_names(),
        )
        .map_err(|err| CompilerError::AotError(err.to_string()))?;

        Ok(ObjectModule::new(builder))
    }

    fn with_state<R>(&mut self, f: impl FnOnce(&mut CodegenState<'_, ObjectModule>) -> R) -> R {
        let mut state = CodegenState::new(
            &mut self.module,
            &mut self.builder_context,
            &mut self.ctx,
            &mut self.struct_layouts,
            &mut self.struct_defs,
            &mut self.string_data,
            &mut self.string_counter,
            &mut self.function_ids,
            &mut self.function_sigs,
            &mut self.current_namespace,
            &mut self.module_registry,
        );
        f(&mut state)
    }

    fn reset_state(&mut self) -> Result<()> {
        self.ctx = self.module.make_context();
        self.builder_context = FunctionBuilderContext::new();
        self.struct_layouts.clear();
        self.struct_defs.clear();
        self.string_data.clear();
        self.string_counter = 0;
        self.function_ids.clear();
        self.function_sigs.clear();
        self.current_namespace = None;

        self.with_state(|state| state.register_runtime_intrinsics())
            .map_err(Self::map_jit)?;
        Ok(())
    }

    pub fn apply_compiler_config(&mut self, config: &CompilerConfig) -> Result<()> {
        let desired = EngineSettings::from_config(config);
        if desired == self.settings {
            return Ok(());
        }

        let module_registry = self.module_registry.clone();
        let mut refreshed = Self::new_with_settings(desired)?;
        refreshed.module_registry = module_registry;
        *self = refreshed;
        Ok(())
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<ObjectCompileOutput> {
        let entry_symbol = self
            .with_state(|state| state.compile_program(program))
            .map_err(Self::map_jit)?;
        self.finalize_object(Some(entry_symbol))
    }

    pub fn compile_module(&mut self, program: &Program) -> Result<ObjectCompileOutput> {
        self.with_state(|state| state.compile_module(program))
            .map_err(Self::map_jit)?;
        self.finalize_object(None)
    }

    pub fn get_module_registry(&self) -> &ModuleRegistry {
        &self.module_registry
    }

    pub fn pointer_size(&self) -> usize {
        self.module.target_config().pointer_type().bytes() as usize
    }

    fn finalize_object(&mut self, entry_symbol: Option<String>) -> Result<ObjectCompileOutput> {
        let namespace = self.current_namespace.clone();
        let raw_layouts = mem::take(&mut self.struct_layouts);
        let struct_layouts = raw_layouts
            .into_iter()
            .map(|(name, layout)| (name, (layout.total_size, layout.alignment)))
            .collect();

        let new_module = Self::create_module(&self.settings)?;
        let finished_module = mem::replace(&mut self.module, new_module);
        let product = finished_module.finish();
        let object_bytes = product
            .emit()
            .map_err(|err| CompilerError::AotError(err.to_string()))?;

        self.reset_state()?;

        Ok(ObjectCompileOutput {
            object_bytes,
            struct_layouts,
            namespace,
            entry_symbol,
        })
    }
}

impl EngineSettings {
    fn from_config(config: &CompilerConfig) -> Self {
        let opt_level = match config.optimization_level {
            0 => settings::OptLevel::None,
            1 => settings::OptLevel::Speed,
            2 => settings::OptLevel::Speed,
            3 => settings::OptLevel::SpeedAndSize,
            _ => settings::OptLevel::SpeedAndSize,
        };

        // For JIT mode, is_pic must be false
        // For AOT mode, is_pic should be true on most platforms for relocatable code
        let default_is_pic = match config.target {
            crate::CompilationTarget::Native => {
                // AOT compilation: use PIC on Linux, not on macOS
                !cfg!(target_os = "macos")
            }
            crate::CompilationTarget::Jit => {
                // JIT requires is_pic=false
                false
            }
            _ => {
                // Other targets (transpilation) don't use Cranelift
                false
            }
        };

        let mut settings = Self {
            opt_level,
            enable_verifier: config.debug_info,
            is_pic: default_is_pic,
        };

        // Allow explicit override via flags
        if let Some(pic_flag) = config.flags.get("pic") {
            let normalized = pic_flag.trim().to_ascii_lowercase();
            settings.is_pic = matches!(normalized.as_str(), "1" | "true" | "yes" | "on");
        }

        settings
    }

    fn apply_flags(&self, builder: &mut settings::Builder) -> Result<()> {
        builder
            .set("opt_level", Self::opt_level_flag(self.opt_level))
            .map_err(|err| CompilerError::JitError(err.to_string()))?;
        builder
            .set(
                "enable_verifier",
                if self.enable_verifier {
                    "true"
                } else {
                    "false"
                },
            )
            .map_err(|err| CompilerError::JitError(err.to_string()))?;
        builder
            .set("is_pic", if self.is_pic { "true" } else { "false" })
            .map_err(|err| CompilerError::JitError(err.to_string()))?;
        Ok(())
    }

    fn opt_level_flag(level: settings::OptLevel) -> &'static str {
        match level {
            settings::OptLevel::None => "none",
            settings::OptLevel::Speed => "speed",
            settings::OptLevel::SpeedAndSize => "speed_and_size",
        }
    }
}

pub struct JitEngine {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    module: JITModule,
    struct_layouts: HashMap<String, StructLayout>,
    struct_defs: HashMap<String, Struct>,
    string_data: HashMap<String, DataId>,
    string_counter: usize,
    function_ids: HashMap<String, FuncId>,
    function_sigs: HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    current_namespace: Option<String>,
    module_registry: ModuleRegistry,
    settings: EngineSettings,
}

impl Default for JitEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl JitEngine {
    pub fn new() -> Self {
        Self::new_with_settings(EngineSettings::default())
            .expect("failed to create JIT engine with default settings")
    }

    fn new_with_settings(engine_settings: EngineSettings) -> Result<Self> {
        let mut flag_builder = settings::builder();
        engine_settings.apply_flags(&mut flag_builder)?;

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let flags = settings::Flags::new(flag_builder);
        let isa = isa_builder
            .finish(flags)
            .map_err(|err| CompilerError::JitError(err.to_string()))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        builder.symbol("putchar", putchar as *const u8);
        builder.symbol("printf", printf as *const u8);
        builder.symbol("malloc", malloc as *const u8);
        builder.symbol("free", free as *const u8);
        builder.symbol("memcpy", memcpy as *const u8);

        let module = JITModule::new(builder);

        let mut engine = Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            struct_layouts: HashMap::new(),
            struct_defs: HashMap::new(),
            string_data: HashMap::new(),
            string_counter: 0,
            function_ids: HashMap::new(),
            function_sigs: HashMap::new(),
            current_namespace: None,
            module_registry: ModuleRegistry::new(),
            settings: engine_settings,
        };

        engine.with_state(|state| state.register_runtime_intrinsics())?;
        Ok(engine)
    }

    pub fn apply_compiler_config(&mut self, config: &CompilerConfig) -> Result<()> {
        let desired = EngineSettings::from_config(config);
        if desired == self.settings {
            return Ok(());
        }

        let module_registry = self.module_registry.clone();
        let mut refreshed = Self::new_with_settings(desired)?;
        refreshed.module_registry = module_registry;
        *self = refreshed;
        Ok(())
    }

    fn with_state<R>(&mut self, f: impl FnOnce(&mut CodegenState<'_, JITModule>) -> R) -> R {
        let mut state = CodegenState::new(
            &mut self.module,
            &mut self.builder_context,
            &mut self.ctx,
            &mut self.struct_layouts,
            &mut self.struct_defs,
            &mut self.string_data,
            &mut self.string_counter,
            &mut self.function_ids,
            &mut self.function_sigs,
            &mut self.current_namespace,
            &mut self.module_registry,
        );
        f(&mut state)
    }

    fn pointer_type(&self) -> types::Type {
        self.module.target_config().pointer_type()
    }

    /// Register an external function that can be called from JIT code
    pub fn register_external_function(
        &mut self,
        name: &str,
        params: Vec<flow_ast::Type>,
        return_type: Option<flow_ast::Type>,
    ) -> Result<()> {
        self.with_state(|state| state.register_external_function(name, params, return_type))
            .map_err(CompilerError::from)
    }

    /// Compile a complete program and return the main function pointer
    pub fn compile(&mut self, program: &Program) -> Result<*const u8> {
        let main_symbol = self
            .with_state(|state| state.compile_program(program))
            .map_err(CompilerError::from)?;
        let main_id = self
            .module
            .get_name(&main_symbol)
            .ok_or_else(|| CompilerError::JitError("No main function found".to_string()))?;

        let main_func_id = match main_id {
            cranelift_module::FuncOrDataId::Func(id) => id,
            _ => {
                return Err(CompilerError::JitError(
                    "main is not a function".to_string(),
                ))
            }
        };

        self.module
            .finalize_definitions()
            .map_err(|e| CompilerError::JitError(e.to_string()))?;

        let code_ptr = self.module.get_finalized_function(main_func_id);
        Ok(code_ptr)
    }

    /// Compile a module (library) without requiring a main function
    pub fn compile_module(&mut self, program: &Program) -> Result<()> {
        self.with_state(|state| state.compile_module(program))
            .map_err(CompilerError::from)
    }

    pub fn get_module_registry(&self) -> &ModuleRegistry {
        &self.module_registry
    }

    pub fn get_module_registry_mut(&mut self) -> &mut ModuleRegistry {
        &mut self.module_registry
    }

    /// Finalize any pending function definitions and return the function pointer for the
    /// requested symbol if it exists.
    pub fn get_function_pointer(&mut self, name: &str) -> Result<Option<*const u8>> {
        self.module
            .finalize_definitions()
            .map_err(|e| CompilerError::JitError(e.to_string()))?;

        let mut candidates = Vec::new();
        candidates.push(name.to_string());

        if !name.contains("::") {
            if let Some(namespace) = &self.current_namespace {
                candidates.push(format!("{}::{}", namespace, name));
            }
        }

        for candidate in candidates {
            if let Some(func_id) = self.function_ids.get(&candidate) {
                let ptr = self.module.get_finalized_function(*func_id);
                return Ok(Some(ptr));
            }
        }

        Ok(None)
    }

    pub fn struct_layout_info(&self, name: &str) -> Option<(usize, usize)> {
        self.struct_layouts
            .get(name)
            .map(|layout| (layout.total_size, layout.alignment))
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_type().bytes() as usize
    }
}

// Helper function to convert Flow types to Cranelift types
fn type_to_cranelift(
    ty: &flow_ast::Type,
    pointer_type: types::Type,
    struct_layouts: Option<&HashMap<String, StructLayout>>,
) -> std::result::Result<types::Type, String> {
    match ty {
        flow_ast::Type::I8 => Ok(types::I8),
        flow_ast::Type::I16 => Ok(types::I16),
        flow_ast::Type::I32 => Ok(types::I32),
        flow_ast::Type::I64 => Ok(types::I64),
        flow_ast::Type::I128 => Err("I128 not supported in Cranelift".to_string()),
        flow_ast::Type::U8 => Ok(types::I8), // Cranelift treats unsigned as signed
        flow_ast::Type::U16 => Ok(types::I16),
        flow_ast::Type::U32 => Ok(types::I32),
        flow_ast::Type::U64 => Ok(types::I64),
        flow_ast::Type::U128 => Err("U128 not supported in Cranelift".to_string()),
        flow_ast::Type::F32 => Ok(types::F32),
        flow_ast::Type::F64 => Ok(types::F64),
        flow_ast::Type::Bool => Ok(types::I8),
        flow_ast::Type::Char => Ok(types::I32), // Unicode char is 32-bit
        flow_ast::Type::String => Ok(pointer_type),
        flow_ast::Type::Unit => Err("Unit type cannot be used as parameter".to_string()),
        flow_ast::Type::Named(name) => {
            if let Some(layouts) = struct_layouts {
                if !layouts.contains_key(name) {
                    return Err(format!("Struct type '{}' has no computed layout", name));
                }
            }
            Ok(pointer_type)
        }
        flow_ast::Type::Function(_, _) => Ok(pointer_type),
        flow_ast::Type::Pointer(_) => Ok(pointer_type),
        flow_ast::Type::MutPointer(_) => Ok(pointer_type),
        flow_ast::Type::Array(_, _) => Ok(pointer_type),
        flow_ast::Type::Slice(_) => Ok(pointer_type),
        flow_ast::Type::TypeVar(_) => Err("Type variables not yet implemented".to_string()),
    }
}

struct ExpressionContext<'a, 'b, M: Module> {
    module: &'a mut M,
    string_data: &'a mut HashMap<String, DataId>,
    string_counter: &'a mut usize,
    function_ids: &'b HashMap<String, FuncId>,
    function_sigs: &'b HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    namespace: Option<&'b str>,
    pointer_type: types::Type,
    struct_layouts: &'b HashMap<String, StructLayout>,
    current_return_type: Option<&'b flow_ast::Type>,
}

impl<'a, 'b, M: Module> ExpressionContext<'a, 'b, M> {
    fn pointer_type(&self) -> types::Type {
        self.pointer_type
    }

    fn resolve_function_name(&self, name: &str) -> String {
        if name.contains("::") {
            name.to_string()
        } else if let Some(namespace) = self.namespace {
            format!("{}::{}", namespace, name)
        } else {
            name.to_string()
        }
    }

    fn find_function_entry(&self, name: &str) -> Option<(String, FuncId)> {
        if let Some(id) = self.function_ids.get(name) {
            return Some((name.to_string(), *id));
        }

        let resolved = self.resolve_function_name(name);
        if let Some(id) = self.function_ids.get(&resolved) {
            return Some((resolved, *id));
        }

        None
    }

    fn get_or_create_string_constant(
        &mut self,
        value: &str,
    ) -> std::result::Result<DataId, String> {
        if let Some(id) = self.string_data.get(value) {
            return Ok(*id);
        }

        let symbol_name = format!("__str_{}", self.string_counter);
        *self.string_counter += 1;

        let mut data_ctx = DataDescription::new();
        let mut bytes = value.as_bytes().to_vec();
        bytes.push(0);
        data_ctx.define(bytes.into_boxed_slice());

        let data_id = self
            .module
            .declare_data(&symbol_name, Linkage::Local, false, false)
            .map_err(|e| format!("Failed to declare string constant '{}': {}", value, e))?;

        self.module
            .define_data(data_id, &data_ctx)
            .map_err(|e| format!("Failed to define string constant '{}': {}", value, e))?;

        self.string_data.insert(value.to_string(), data_id);
        Ok(data_id)
    }
}

fn align_to(value: usize, alignment: usize) -> usize {
    if alignment <= 1 {
        value
    } else {
        let rem = value % alignment;
        if rem == 0 {
            value
        } else {
            value + (alignment - rem)
        }
    }
}

fn infer_flow_type(
    expr: &Expr,
    variables: &Environment,
    context: &ExpressionContext<'_, '_, impl Module>,
) -> Option<flow_ast::Type> {
    if let Some(struct_name) = resolve_struct_name(expr, variables, context) {
        return Some(flow_ast::Type::Named(struct_name));
    }

    match expr {
        Expr::StructInit { name, .. } => Some(flow_ast::Type::Named(name.clone())),
        _ => None,
    }
}

fn resolve_struct_name(
    expr: &Expr,
    variables: &Environment,
    context: &ExpressionContext<'_, '_, impl Module>,
) -> Option<String> {
    match expr {
        Expr::Ident(name) => variables.get(name).and_then(|binding| {
            binding.flow_type().and_then(|ty| match ty {
                flow_ast::Type::Named(struct_name) => Some(struct_name.clone()),
                _ => None,
            })
        }),
        Expr::StructInit { name, .. } => Some(name.clone()),
        Expr::Field { expr, field } => {
            let base_struct = resolve_struct_name(expr, variables, context)?;
            let layout = context.struct_layouts.get(&base_struct)?;
            layout
                .fields
                .iter()
                .find(|(name, _, _)| name == field)
                .and_then(|(_, ty, _)| match ty {
                    flow_ast::Type::Named(name) => Some(name.clone()),
                    _ => None,
                })
        }
        Expr::Call { func, .. } => {
            if let Expr::Ident(func_name) = func.as_ref() {
                context
                    .find_function_entry(func_name)
                    .and_then(|(key, _)| context.function_sigs.get(&key))
                    .and_then(|(_, return_type)| return_type.clone())
                    .and_then(|ty| match ty {
                        flow_ast::Type::Named(name) => Some(name),
                        _ => None,
                    })
            } else {
                None
            }
        }
        Expr::Method { expr, method, .. } => {
            let base_struct = resolve_struct_name(expr, variables, context)?;
            let method_key = format!("{}::{}", base_struct, method);

            context
                .find_function_entry(&method_key)
                .and_then(|(key, _)| context.function_sigs.get(&key))
                .and_then(|(_, return_type)| return_type.clone())
                .and_then(|ty| match ty {
                    flow_ast::Type::Named(name) => Some(name),
                    _ => None,
                })
        }
        _ => None,
    }
}

fn compile_expression(
    expr: &Expr,
    builder: &mut FunctionBuilder,
    variables: &mut Environment,
    context: &mut ExpressionContext<'_, '_, impl Module>,
    expected_type: Option<&flow_ast::Type>,
) -> std::result::Result<cranelift::prelude::Value, String> {
    let pointer_type = context.pointer_type();

    match expr {
        Expr::Integer(value) => {
            let ty = select_int_type(pointer_type, expected_type);
            Ok(builder.ins().iconst(ty, *value))
        }
        Expr::Float(value) => match select_float_type(expected_type) {
            types::F32 => Ok(builder.ins().f32const(*value as f32)),
            _ => Ok(builder.ins().f64const(*value)),
        },
        Expr::Bool(value) => Ok(builder.ins().iconst(types::I8, if *value { 1 } else { 0 })),
        Expr::String(value) => {
            let data_id = context.get_or_create_string_constant(value)?;
            let global = context.module.declare_data_in_func(data_id, builder.func);
            Ok(builder.ins().global_value(pointer_type, global))
        }
        Expr::Unit => Ok(builder.ins().iconst(types::I8, 0)),
        Expr::Ident(name) => variables
            .get(name)
            .map(|binding| binding.read(builder))
            .ok_or_else(|| format!("Variable '{}' not found", name)),
        Expr::Unary { op, expr } => {
            let value = compile_expression(expr, builder, variables, context, expected_type)?;
            let value_ty = builder.func.dfg.value_type(value);

            match op {
                UnOp::Neg => {
                    if value_ty.is_float() {
                        Ok(builder.ins().fneg(value))
                    } else {
                        Ok(builder.ins().ineg(value))
                    }
                }
                UnOp::Not => {
                    let cmp = builder.ins().icmp_imm(IntCC::Equal, value, 0);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
            }
        }
        Expr::Assign { target, value } => {
            let target_name = if let Expr::Ident(name) = target.as_ref() {
                name
            } else {
                return Err("Only simple identifier assignments are supported for now".to_string());
            };

            let (slot, binding_ty, flow_hint) = match variables.get(target_name) {
                Some(VariableBinding::Mutable {
                    slot,
                    ty,
                    flow_type,
                }) => (*slot, *ty, flow_type.clone()),
                Some(VariableBinding::Immutable { .. }) => {
                    return Err(format!(
                        "Cannot assign to immutable binding '{}'",
                        target_name
                    ))
                }
                None => return Err(format!("Variable '{}' not found", target_name)),
            };

            let value_hint = flow_hint.as_ref();
            let new_value = compile_expression(value, builder, variables, context, value_hint)?;
            let new_value_ty = builder.func.dfg.value_type(new_value);

            if new_value_ty != binding_ty {
                return Err(format!("Type mismatch in assignment to '{}'", target_name));
            }

            builder.ins().stack_store(new_value, slot, 0);
            Ok(builder.ins().stack_load(binding_ty, slot, 0))
        }
        Expr::Let {
            name,
            mutable,
            ty,
            value,
            then,
        } => {
            let mut flow_type_annotation = ty.clone();
            if flow_type_annotation.is_none() {
                flow_type_annotation = infer_flow_type(value, variables, context);
            }

            let value_hint = flow_type_annotation.as_ref();
            let value_val = compile_expression(value, builder, variables, context, value_hint)?;

            let cranelift_ty = if let Some(explicit_ty) = ty {
                type_to_cranelift(explicit_ty, pointer_type, Some(context.struct_layouts))
                    .map_err(|e| format!("Failed to convert declared type for '{}': {}", name, e))?
            } else {
                builder.func.dfg.value_type(value_val)
            };

            let binding = if *mutable {
                let slot_size = cranelift_ty.bytes();
                if slot_size == 0 {
                    return Err(format!(
                        "Cannot allocate storage for zero-sized binding '{}'",
                        name
                    ));
                }
                let max_align = pointer_type.bytes().max(1);
                let align = slot_size.next_power_of_two().min(max_align);
                let align_shift = align.trailing_zeros() as u8;
                let slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    slot_size,
                    align_shift,
                ));
                builder.ins().stack_store(value_val, slot, 0);
                VariableBinding::Mutable {
                    slot,
                    ty: cranelift_ty,
                    flow_type: flow_type_annotation.clone(),
                }
            } else {
                VariableBinding::Immutable {
                    value: value_val,
                    flow_type: flow_type_annotation.clone(),
                }
            };

            variables.insert(name.clone(), binding);
            compile_expression(then, builder, variables, context, expected_type)
        }
        Expr::Block(expressions) => {
            variables.enter_scope();
            let mut last_value: Option<cranelift::prelude::Value> = None;

            for (idx, expr) in expressions.iter().enumerate() {
                let is_last = idx + 1 == expressions.len();
                let hint = if is_last { expected_type } else { None };
                last_value = Some(compile_expression(expr, builder, variables, context, hint)?);
            }

            variables.exit_scope();

            if let Some(value) = last_value {
                Ok(value)
            } else {
                Ok(builder.ins().iconst(types::I8, 0))
            }
        }
        Expr::While { cond, body } => {
            let header_block = builder.create_block();
            let body_block = builder.create_block();
            let exit_block = builder.create_block();

            // Jump to header to evaluate condition
            builder.ins().jump(header_block, &[]);
            builder.switch_to_block(header_block);

            let cond_value = compile_expression(cond, builder, variables, context, None)?;
            let cond_bool = builder.ins().icmp_imm(IntCC::NotEqual, cond_value, 0);

            builder.ins().brif(cond_bool, body_block, &[], exit_block, &[]);
            // builder.seal_block(header_block); // Moved down

            builder.switch_to_block(body_block);
            compile_expression(body, builder, variables, context, None)?;

            // Jump back to header
            builder.ins().jump(header_block, &[]);
            builder.seal_block(header_block); // Seal header after back-edge is added
            builder.seal_block(body_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // While loops return Unit
            Ok(builder.ins().iconst(types::I8, 0))
        }
        Expr::For { init, cond, update, body } => {
            // Execute init expression once
            compile_expression(init, builder, variables, context, None)?;

            let header_block = builder.create_block();
            let body_block = builder.create_block();
            let update_block = builder.create_block();
            let exit_block = builder.create_block();

            // Jump to header to evaluate condition
            builder.ins().jump(header_block, &[]);
            builder.switch_to_block(header_block);

            let cond_value = compile_expression(cond, builder, variables, context, None)?;
            let cond_bool = builder.ins().icmp_imm(IntCC::NotEqual, cond_value, 0);

            builder.ins().brif(cond_bool, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);
            compile_expression(body, builder, variables, context, None)?;

            // Jump to update
            builder.ins().jump(update_block, &[]);
            builder.seal_block(body_block);

            builder.switch_to_block(update_block);
            compile_expression(update, builder, variables, context, None)?;

            // Jump back to header
            builder.ins().jump(header_block, &[]);
            builder.seal_block(header_block); // Seal header after back-edge is added
            builder.seal_block(update_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // For loops return Unit
            Ok(builder.ins().iconst(types::I8, 0))
        }
        Expr::If { cond, then, else_ } => {
            if else_.is_none() {
                if let Some(expected) = expected_type {
                    if !matches!(expected, flow_ast::Type::Unit) {
                        return Err("If expression without else cannot produce a value".to_string());
                    }
                }
            }

            let cond_value = compile_expression(cond, builder, variables, context, None)?;
            let cond_bool = builder.ins().icmp_imm(IntCC::NotEqual, cond_value, 0);

            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            let else_block = else_.as_ref().map(|_| builder.create_block());

            if let Some(else_blk) = else_block {
                let empty_args: &[BlockArg] = &[];
                builder
                    .ins()
                    .brif(cond_bool, then_block, empty_args, else_blk, empty_args);
            } else {
                let empty_args: &[BlockArg] = &[];
                builder
                    .ins()
                    .brif(cond_bool, then_block, empty_args, merge_block, empty_args);
            }

            let mut merge_param_ty: Option<types::Type> = None;

            builder.switch_to_block(then_block);
            let then_value = compile_expression(then, builder, variables, context, expected_type)?;
            let then_ty = builder.func.dfg.value_type(then_value);
            if let Some(expected_ty) = merge_param_ty {
                if expected_ty != then_ty {
                    return Err("Type mismatch between if branches".to_string());
                }
            } else if else_block.is_some() {
                builder.append_block_param(merge_block, then_ty);
                merge_param_ty = Some(then_ty);
            }
            if else_block.is_some() {
                let then_args = [BlockArg::from(then_value)];
                builder.ins().jump(merge_block, then_args.as_slice());
            } else {
                let empty_args: &[BlockArg] = &[];
                builder.ins().jump(merge_block, empty_args);
            }
            builder.seal_block(then_block);

            if let Some(else_expr) = else_ {
                let else_blk = else_block.expect("else block to exist");
                builder.switch_to_block(else_blk);
                let else_value =
                    compile_expression(else_expr, builder, variables, context, expected_type)?;
                let else_ty = builder.func.dfg.value_type(else_value);

                if let Some(expected_ty) = merge_param_ty {
                    if expected_ty != else_ty {
                        return Err("Type mismatch between if branches".to_string());
                    }
                } else {
                    builder.append_block_param(merge_block, else_ty);
                    merge_param_ty = Some(else_ty);
                }

                let else_args = [BlockArg::from(else_value)];
                builder.ins().jump(merge_block, else_args.as_slice());
                builder.seal_block(else_blk);
            }

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);

            if merge_param_ty.is_some() {
                let params = builder.block_params(merge_block);
                params
                    .first()
                    .copied()
                    .ok_or_else(|| "If expression missing merge value".to_string())
            } else {
                Ok(builder.ins().iconst(types::I8, 0))
            }
        }
        Expr::Binary { op, left, right } => {
            let left_val = compile_expression(left, builder, variables, context, expected_type)?;
            let right_val = compile_expression(right, builder, variables, context, expected_type)?;

            let expect_float = expected_type
                .map(|ty| matches!(ty, flow_ast::Type::F32 | flow_ast::Type::F64))
                .unwrap_or(false);

            match op {
                BinOp::Add => {
                    if expect_float {
                        Ok(builder.ins().fadd(left_val, right_val))
                    } else {
                        Ok(builder.ins().iadd(left_val, right_val))
                    }
                }
                BinOp::Sub => {
                    if expect_float {
                        Ok(builder.ins().fsub(left_val, right_val))
                    } else {
                        Ok(builder.ins().isub(left_val, right_val))
                    }
                }
                BinOp::Mul => {
                    if expect_float {
                        Ok(builder.ins().fmul(left_val, right_val))
                    } else {
                        Ok(builder.ins().imul(left_val, right_val))
                    }
                }
                BinOp::Div => {
                    if expect_float {
                        Ok(builder.ins().fdiv(left_val, right_val))
                    } else {
                        Ok(builder.ins().sdiv(left_val, right_val))
                    }
                }
                BinOp::Mod => Ok(builder.ins().srem(left_val, right_val)),
                BinOp::Eq => {
                    let cmp = builder.ins().icmp(IntCC::Equal, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::NotEq => {
                    let cmp = builder.ins().icmp(IntCC::NotEqual, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::Lt => {
                    let cmp = builder
                        .ins()
                        .icmp(IntCC::SignedLessThan, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::LtEq => {
                    let cmp = builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::Gt => {
                    let cmp = builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThan, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::GtEq => {
                    let cmp =
                        builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, left_val, right_val);
                    Ok(builder.ins().uextend(types::I8, cmp))
                }
                BinOp::And => Ok(builder.ins().band(left_val, right_val)),
                BinOp::Or => Ok(builder.ins().bor(left_val, right_val)),
            }
        }
        Expr::Call { func, args } => {
            let func_name = if let Expr::Ident(name) = func.as_ref() {
                name
            } else {
                return Err("Only direct function identifiers are supported in calls".to_string());
            };

            let (lookup_key, func_id) = context
                .find_function_entry(func_name)
                .ok_or_else(|| format!("Function '{}' not declared", func_name))?;

            let (param_types, return_type) = context
                .function_sigs
                .get(&lookup_key)
                .cloned()
                .ok_or_else(|| format!("No signature found for function {}", lookup_key))?;

            if args.len() != param_types.len() {
                return Err(format!(
                    "Function '{}' expected {} arguments but received {}",
                    func_name,
                    param_types.len(),
                    args.len()
                ));
            }

            let func_ref = context.module.declare_func_in_func(func_id, builder.func);

            let mut compiled_args = Vec::with_capacity(args.len());
            for (idx, arg_expr) in args.iter().enumerate() {
                let arg_value = compile_expression(
                    arg_expr,
                    builder,
                    variables,
                    context,
                    param_types.get(idx),
                )?;
                compiled_args.push(arg_value);
            }

            let call_inst = builder.ins().call(func_ref, &compiled_args);
            if return_type.is_some() {
                let results = builder.inst_results(call_inst);
                results
                    .first()
                    .copied()
                    .ok_or_else(|| format!("Call to '{}' did not produce a value", func_name))
            } else {
                Ok(builder.ins().iconst(pointer_type, 0))
            }
        }
        Expr::StructInit { name, fields } => {
            let layout = context
                .struct_layouts
                .get(name)
                .ok_or_else(|| format!("Unknown struct '{}'", name))?;

            let min_size = layout
                .total_size
                .max(context.pointer_type().bytes() as usize);
            let allocation_size = align_to(min_size, layout.alignment);
            let size_val = builder
                .ins()
                .iconst(context.pointer_type(), allocation_size as i64);

            let (_, malloc_id) = context
                .find_function_entry("malloc")
                .ok_or_else(|| "malloc intrinsic not registered".to_string())?;
            let malloc_ref = context.module.declare_func_in_func(malloc_id, builder.func);
            let call_inst = builder.ins().call(malloc_ref, &[size_val]);
            let struct_ptr = builder
                .inst_results(call_inst)
                .first()
                .copied()
                .ok_or_else(|| "malloc call did not return a pointer".to_string())?;

            for (field_name, field_ty, offset) in &layout.fields {
                let field_expr = fields.get(field_name).ok_or_else(|| {
                    format!("Missing field '{}' in struct initializer", field_name)
                })?;

                let field_value =
                    compile_expression(field_expr, builder, variables, context, Some(field_ty))?;

                let field_offset = builder.ins().iconst(context.pointer_type(), *offset as i64);
                let field_addr = builder.ins().iadd(struct_ptr, field_offset);
                builder
                    .ins()
                    .store(MemFlags::new(), field_value, field_addr, 0);
            }

            Ok(struct_ptr)
        }
        Expr::Method {
            expr: receiver,
            method,
            args,
        } => {
            let struct_name =
                resolve_struct_name(receiver, variables, context).ok_or_else(|| {
                    format!("Receiver for method '{}' must be a struct instance", method)
                })?;
            let method_key = format!("{}::{}", struct_name, method);

            let (lookup_key, func_id) =
                context.find_function_entry(&method_key).ok_or_else(|| {
                    format!("Method '{}' not found for struct '{}'", method, struct_name)
                })?;

            let (param_types, return_type) = context
                .function_sigs
                .get(&lookup_key)
                .cloned()
                .ok_or_else(|| format!("No signature found for method {}", method_key))?;

            if param_types.is_empty() {
                return Err(format!(
                    "Method '{}' must take at least one parameter (self)",
                    method_key
                ));
            }

            let receiver_ty = &param_types[0];
            let receiver_matches = match receiver_ty {
                flow_ast::Type::Named(name) => name == &struct_name,
                flow_ast::Type::Pointer(inner) | flow_ast::Type::MutPointer(inner) => {
                    matches!(inner.as_ref(), flow_ast::Type::Named(name) if name == &struct_name)
                }
                _ => false,
            };

            if !receiver_matches {
                return Err(format!(
                    "Method '{}' expects receiver of type {:?}, but was called on '{}'",
                    method_key, receiver_ty, struct_name
                ));
            }

            if args.len() + 1 != param_types.len() {
                return Err(format!(
                    "Method '{}' expected {} arguments but received {}",
                    method_key,
                    param_types.len() - 1,
                    args.len()
                ));
            }

            let mut compiled_args = Vec::with_capacity(param_types.len());
            let self_value =
                compile_expression(receiver, builder, variables, context, Some(receiver_ty))?;
            compiled_args.push(self_value);

            for (idx, arg_expr) in args.iter().enumerate() {
                let param_ty = param_types
                    .get(idx + 1)
                    .ok_or_else(|| format!("Missing parameter type for argument {}", idx + 1))?;
                let arg_value =
                    compile_expression(arg_expr, builder, variables, context, Some(param_ty))?;
                compiled_args.push(arg_value);
            }

            let func_ref = context.module.declare_func_in_func(func_id, builder.func);
            let call_inst = builder.ins().call(func_ref, &compiled_args);
            if return_type.is_some() {
                let results = builder.inst_results(call_inst);
                results.first().copied().ok_or_else(|| {
                    format!("Call to method '{}' did not produce a value", method_key)
                })
            } else {
                Ok(builder.ins().iconst(context.pointer_type(), 0))
            }
        }
        Expr::Field { expr, field } => {
            let struct_name = resolve_struct_name(expr, variables, context)
                .ok_or_else(|| "Field access requires struct-typed expression".to_string())?;

            let layout = context
                .struct_layouts
                .get(&struct_name)
                .ok_or_else(|| format!("Unknown struct '{}' for field access", struct_name))?;

            let (_, field_ty, offset) = layout
                .fields
                .iter()
                .find(|(name, _, _)| name == field)
                .ok_or_else(|| {
                    format!("Field '{}' not found on struct '{}'", field, struct_name)
                })?;

            let base_ptr = compile_expression(expr, builder, variables, context, None)?;
            let field_offset = builder.ins().iconst(context.pointer_type(), *offset as i64);
            let field_addr = builder.ins().iadd(base_ptr, field_offset);
            let cranelift_field_ty = type_to_cranelift(
                field_ty,
                context.pointer_type(),
                Some(context.struct_layouts),
            )?;
            Ok(builder
                .ins()
                .load(cranelift_field_ty, MemFlags::new(), field_addr, 0))
        }
        Expr::Return(expr_opt) => {
            if let Some(expr) = expr_opt {
                let ret_ty = context.current_return_type;
                let val = compile_expression(expr, builder, variables, context, ret_ty)?;
                builder.ins().return_(&[val]);
            } else {
                builder.ins().return_(&[]);
            }

            // Create a new block for the unreachable code to satisfy the requirement
            // that compile_expression returns a Value, which requires emitting an instruction.
            // Since the previous block is filled with a return, we must start a new one.
            let unreachable_block = builder.create_block();
            builder.switch_to_block(unreachable_block);
            builder.seal_block(unreachable_block);

            // Return a dummy value matching expected_type if possible to satisfy the type system
            // even though this code is unreachable.
            let dummy_ty = if let Some(ty) = expected_type {
                type_to_cranelift(ty, context.pointer_type(), Some(context.struct_layouts))
                    .unwrap_or(types::I64)
            } else {
                types::I64
            };

            if dummy_ty.is_float() {
                if dummy_ty == types::F32 {
                    Ok(builder.ins().f32const(0.0))
                } else {
                    Ok(builder.ins().f64const(0.0))
                }
            } else {
                Ok(builder.ins().iconst(dummy_ty, 0))
            }
        }
        _ => Err(format!("Expression type {:?} not yet implemented", expr)),
    }
}

fn select_int_type(
    pointer_type: types::Type,
    expected_type: Option<&flow_ast::Type>,
) -> types::Type {
    if let Some(ty) = expected_type {
        match ty {
            flow_ast::Type::I8 | flow_ast::Type::U8 => types::I8,
            flow_ast::Type::I16 | flow_ast::Type::U16 => types::I16,
            flow_ast::Type::I32 | flow_ast::Type::U32 => types::I32,
            flow_ast::Type::I64 | flow_ast::Type::U64 => types::I64,
            flow_ast::Type::String
            | flow_ast::Type::Pointer(_)
            | flow_ast::Type::MutPointer(_)
            | flow_ast::Type::Array(_, _)
            | flow_ast::Type::Slice(_) => pointer_type,
            _ => pointer_type,
        }
    } else {
        pointer_type
    }
}

fn select_float_type(expected_type: Option<&flow_ast::Type>) -> types::Type {
    if let Some(ty) = expected_type {
        match ty {
            flow_ast::Type::F32 => types::F32,
            _ => types::F64,
        }
    } else {
        types::F64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use flow_analyzer::Analyzer;
    use flow_ast::{Field, Program, Struct, Type};
    use flow_parser::Parser;

    fn parse_and_analyze(source: &str) -> Program {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("failed to parse test program");

        let mut analyzer = Analyzer::new();
        if let Err(errors) = analyzer.analyze(&program) {
            panic!("analysis failed: {:?}", errors);
        }

        program
    }

    #[test]
    fn compute_struct_layout_aligns_fields() {
        let mut engine = JitEngine::new();
        let struct_def = Struct {
            name: "Pair".to_string(),
            fields: vec![
                Field {
                    name: "first".to_string(),
                    ty: Type::I64,
                    is_pub: true,
                },
                Field {
                    name: "second".to_string(),
                    ty: Type::I32,
                    is_pub: true,
                },
            ],
            is_pub: true,
            attributes: vec![],
        };

        let layout = engine
            .with_state(|state| state.compute_struct_layout(&struct_def))
            .expect("struct layout computation should succeed");

        assert_eq!(layout.fields.len(), 2);
        assert_eq!(layout.fields[0].2, 0);
        assert_eq!(layout.fields[1].2, 8);
        assert_eq!(layout.total_size, 16);
        assert_eq!(layout.alignment, 8);
    }

    #[test]
    fn compute_struct_layout_handles_named_fields_as_pointers() {
        let mut engine = JitEngine::new();

        let inner = Struct {
            name: "Inner".to_string(),
            fields: vec![Field {
                name: "value".to_string(),
                ty: Type::I64,
                is_pub: true,
            }],
            is_pub: true,
            attributes: vec![],
        };

        let outer = Struct {
            name: "Outer".to_string(),
            fields: vec![Field {
                name: "inner".to_string(),
                ty: Type::Named("Inner".to_string()),
                is_pub: true,
            }],
            is_pub: true,
            attributes: vec![],
        };

        engine.struct_defs.insert(inner.name.clone(), inner.clone());
        engine.struct_defs.insert(outer.name.clone(), outer.clone());

        engine
            .with_state(|state| state.compute_struct_layout(&inner))
            .expect("inner layout should succeed");

        let outer_layout = engine
            .with_state(|state| state.compute_struct_layout(&outer))
            .expect("outer layout should succeed");

        let pointer_size = engine.pointer_type().bytes() as usize;

        assert_eq!(outer_layout.fields.len(), 1);
        assert_eq!(outer_layout.fields[0].2, 0);
        assert_eq!(
            outer_layout.total_size,
            align_to(pointer_size, pointer_size)
        );
        assert_eq!(outer_layout.alignment, pointer_size);
    }

    #[test]
    fn jit_executes_struct_program() {
        let program = parse_and_analyze(
            r#"
struct Pair {
    pub first: i64,
    pub second: i64,
}

func make_pair(a: i64, b: i64) -> Pair {
    Pair { first: a, second: b }
}

func main() -> i64 {
    let pair = make_pair(21, 21);
    pair.first + pair.second
}
        "#,
        );

        let mut engine = JitEngine::new();
        let entry_ptr = engine
            .compile(&program)
            .expect("JIT compilation should succeed");

        let compiled_main: unsafe extern "C" fn() -> i64 =
            unsafe { std::mem::transmute(entry_ptr) };
        let result = unsafe { compiled_main() };

        assert_eq!(result, 42);
    }
}
