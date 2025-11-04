use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, DataId, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use flow_ast::*;
use std::collections::HashMap;

pub struct Compiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    struct_layouts: HashMap<String, StructLayout>,
    string_data: Vec<DataId>,
    lambda_counter: usize,
    function_ids: HashMap<String, cranelift_module::FuncId>,
    function_sigs: HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>, // params, return type
    current_namespace: Option<String>, // Current namespace for symbol name mangling
}

pub struct AOTCompiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: ObjectModule,
    struct_layouts: HashMap<String, StructLayout>,
    string_data: Vec<DataId>,
    lambda_counter: usize,
    function_ids: HashMap<String, cranelift_module::FuncId>,
    function_sigs: HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>, // params, return type
    current_namespace: Option<String>, // Current namespace for symbol name mangling
}

#[derive(Debug, Clone)]
struct StructLayout {
    fields: Vec<(String, flow_ast::Type, usize)>, // name, type, offset
    total_size: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        
        // Add standard library functions
        builder.symbol("putchar", putchar as *const u8);
        builder.symbol("printf", printf as *const u8);
        builder.symbol("malloc", malloc as *const u8);
        builder.symbol("free", free as *const u8);
        builder.symbol("memcpy", memcpy as *const u8);

        let module = JITModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            struct_layouts: HashMap::new(),
            string_data: Vec::new(),
            lambda_counter: 0,
            function_ids: HashMap::new(),
            function_sigs: HashMap::new(),
            current_namespace: None,
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<*const u8, String> {
        // Handle namespace declaration
        if let Some(namespace_decl) = &program.namespace {
            self.current_namespace = Some(namespace_decl.namespace.clone());
        }

        // First pass: compute struct layouts
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                self.compute_struct_layout(struct_def)?;
            }
        }
        
        // Second pass: declare all functions (including methods)
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.declare_function(method)?;
                    }
                }
                Item::Use(_) => {
                    // Use declarations are handled at analysis time
                    // The referenced functions should be available through linking
                }
                _ => {}
            }
        }

        // Third pass: compile function bodies
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.compile_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.compile_function(method)?;
                    }
                }
                Item::Struct(_) => {
                    // Already processed in first pass
                }
                Item::ExternBlock(_) => {
                    // External functions are linked
                }
                Item::Import(_) => {
                    // Imports are resolved at link time
                }
                Item::Use(_) => {
                    // Use declarations don't generate code
                }
            }
        }

        // Get the main function pointer
        let main_id = self.module
            .get_name("main")
            .ok_or_else(|| "No main function found".to_string())?;
        
        // Convert FuncOrDataId to FuncId
        let main_func_id = match main_id {
            cranelift_module::FuncOrDataId::Func(id) => id,
            _ => return Err("main is not a function".to_string()),
        };
        
        self.module.finalize_definitions().unwrap();
        
        let code_ptr = self.module.get_finalized_function(main_func_id);
        Ok(code_ptr)
    }

    /// Compile a module (library) without requiring a main function
    pub fn compile_module(&mut self, program: &Program) -> Result<(), String> {
        // Handle namespace declaration
        if let Some(namespace_decl) = &program.namespace {
            self.current_namespace = Some(namespace_decl.namespace.clone());
        }

        // First pass: compute struct layouts
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                self.compute_struct_layout(struct_def)?;
            }
        }
        
        // Second pass: declare all functions (including methods)
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.declare_function(method)?;
                    }
                }
                Item::Use(_) => {
                    // Use declarations are handled at analysis time
                    // The referenced functions should be available through linking
                }
                _ => {}
            }
        }

        // Third pass: compile function bodies
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.compile_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.compile_function(method)?;
                    }
                }
                Item::Struct(_) => {
                    // Already processed in first pass
                }
                Item::ExternBlock(_) => {
                    // External functions are linked
                }
                Item::Import(_) => {
                    // Imports are resolved at link time
                }
                Item::Use(_) => {
                    // Use declarations don't generate code
                }
            }
        }

        // For modules, we don't need to finalize until all modules are compiled
        Ok(())
    }

    /// Get a compiled function pointer by name
    /// Must be called after compile() has been called
    pub fn get_function(&self, name: &str) -> Option<*const u8> {
        let func_id = self.function_ids.get(name)?;
        Some(self.module.get_finalized_function(*func_id))
    }

    /// Generate a mangled symbol name based on namespace
    fn mangle_symbol_name(&self, name: &str) -> String {
        match &self.current_namespace {
            Some(namespace) => format!("{}::{}", namespace, name),
            None => name.to_string(),
        }
    }

    /// Resolve a potentially namespaced function name to the actual symbol name
    fn resolve_function_name(&self, name: &str) -> String {
        if name.contains("::") {
            // Already fully qualified
            name.to_string()
        } else if let Some(namespace) = &self.current_namespace {
            // Add current namespace if not already qualified
            format!("{}::{}", namespace, name)
        } else {
            // No namespace, use as-is
            name.to_string()
        }
    }

    fn declare_function(&mut self, func: &Function) -> Result<(), String> {
        // Create a new signature with the ISA's default calling convention
        let call_conv = self.module.isa().default_call_conv();
        let mut sig = Signature::new(call_conv);

        for param in &func.params {
            let cranelift_type = type_to_cranelift(&param.ty)?;
            sig.params.push(AbiParam::new(cranelift_type));
        }

        if let Some(ref return_type) = func.return_type {
            let cranelift_type = type_to_cranelift(return_type)?;
            sig.returns.push(AbiParam::new(cranelift_type));
        }

        let linkage = if func.is_pub {
            Linkage::Export
        } else {
            Linkage::Local
        };

        // Use mangled name for function declaration unless it's main
        let symbol_name = if func.name == "main" {
            func.name.clone()
        } else {
            self.mangle_symbol_name(&func.name)
        };

        let func_id = self.module
            .declare_function(&symbol_name, linkage, &sig)
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        
        // Now update the context signature for compilation
        self.ctx.func.signature = sig;

        // Store the function ID and signature for later retrieval
        // Use the mangled name as the key for lookups
        self.function_ids.insert(symbol_name.clone(), func_id);
        
        let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
        self.function_sigs.insert(symbol_name, (param_types, func.return_type.clone()));

        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> Result<(), String> {
        // Use mangled name for function lookup unless it's main
        let symbol_name = if func.name == "main" {
            func.name.clone()
        } else {
            self.mangle_symbol_name(&func.name)
        };

        let func_or_data_id = self.module
            .get_name(&symbol_name)
            .ok_or_else(|| format!("Function {} not declared", symbol_name))?;

        let id = match func_or_data_id {
            cranelift_module::FuncOrDataId::Func(id) => id,
            _ => return Err(format!("{} is not a function", symbol_name)),
        };

        self.ctx.func.clear();
        
        // Create signature with proper calling convention
        let call_conv = self.module.isa().default_call_conv();
        let mut sig = Signature::new(call_conv);

        for param in &func.params {
            let cranelift_type = type_to_cranelift(&param.ty)?;
            sig.params.push(AbiParam::new(cranelift_type));
        }

        if let Some(ref return_type) = func.return_type {
            let cranelift_type = type_to_cranelift(return_type)?;
            sig.returns.push(AbiParam::new(cranelift_type));
        }
        
        self.ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut func_compiler = FunctionCompiler {
            builder,
            module: &mut self.module,
            variables: HashMap::new(),
            var_index: 0,
            struct_layouts: &self.struct_layouts,
            memory_tracker: Vec::new(),
            function_sigs: &self.function_sigs,
            current_namespace: &self.current_namespace,
        };

        // Declare parameters as variables
        for (i, param) in func.params.iter().enumerate() {
            let var = Variable::new(func_compiler.var_index);
            func_compiler.var_index += 1;
            
            let cranelift_type = type_to_cranelift(&param.ty)?;
            func_compiler.builder.declare_var(var, cranelift_type);
            
            let param_val = func_compiler.builder.block_params(entry_block)[i];
            func_compiler.builder.def_var(var, param_val);
            
            func_compiler.variables.insert(param.name.clone(), var);
        }

        let return_value = func_compiler.compile_expr(&func.body)?;

        if func.return_type.is_some() {
            func_compiler.builder.ins().return_(&[return_value]);
        } else {
            func_compiler.builder.ins().return_(&[]);
        }

        func_compiler.builder.finalize();

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| format!("Failed to define function: {}", e))?;

        self.module.clear_context(&mut self.ctx);

        Ok(())
    }
}

fn type_to_cranelift(ty: &flow_ast::Type) -> Result<types::Type, String> {
    match ty {
        // Signed integers
        flow_ast::Type::I8 => Ok(types::I8),
        flow_ast::Type::I16 => Ok(types::I16),
        flow_ast::Type::I32 => Ok(types::I32),
        flow_ast::Type::I64 => Ok(types::I64),
        flow_ast::Type::I128 => Ok(types::I128),
        
        // Unsigned integers
        flow_ast::Type::U8 => Ok(types::I8),
        flow_ast::Type::U16 => Ok(types::I16),
        flow_ast::Type::U32 => Ok(types::I32),
        flow_ast::Type::U64 => Ok(types::I64),
        flow_ast::Type::U128 => Ok(types::I128),
        
        // Floating point
        flow_ast::Type::F32 => Ok(types::F32),
        flow_ast::Type::F64 => Ok(types::F64),
        
        // Other primitives
        flow_ast::Type::Bool => Ok(types::I8),
        flow_ast::Type::Char => Ok(types::I32), // Unicode scalar value
        flow_ast::Type::String => Ok(types::I64), // Pointer
        flow_ast::Type::Unit => Ok(types::I8), // Placeholder
        
        // Composite types
        flow_ast::Type::Named(_) => Ok(types::I64), // Pointer to struct
        flow_ast::Type::Function(_, _) => Ok(types::I64), // Function pointer
        flow_ast::Type::Pointer(_) => Ok(types::I64), // Pointer
        flow_ast::Type::MutPointer(_) => Ok(types::I64), // Mutable pointer
        flow_ast::Type::Array(_, _) => Ok(types::I64), // Pointer to array
        flow_ast::Type::Slice(_) => Ok(types::I64), // Pointer to slice (fat pointer)
        
        // Type variables
        flow_ast::Type::TypeVar(_) => Err("Type variables not yet supported in codegen".to_string()),
    }
}

struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    variables: HashMap<String, Variable>,
    var_index: usize,
    struct_layouts: &'a HashMap<String, StructLayout>,
    memory_tracker: Vec<Value>,
    function_sigs: &'a HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    current_namespace: &'a Option<String>,
}

impl<'a> FunctionCompiler<'a> {
    /// Generate a mangled symbol name based on namespace
    fn mangle_symbol_name(&self, name: &str) -> String {
        match self.current_namespace {
            Some(namespace) => format!("{}::{}", namespace, name),
            None => name.to_string(),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Integer(n) => {
                Ok(self.builder.ins().iconst(types::I64, *n))
            }
            Expr::Float(f) => {
                Ok(self.builder.ins().f64const(*f))
            }
            Expr::Bool(b) => {
                let val = if *b { 1 } else { 0 };
                Ok(self.builder.ins().iconst(types::I8, val))
            }
            Expr::Unit => {
                Ok(self.builder.ins().iconst(types::I8, 0))
            }
            Expr::String(s) => {
                // Allocate memory for the string and copy it
                let len = s.len();
                let size_val = self.builder.ins().iconst(types::I64, (len + 1) as i64);
                
                // Declare malloc
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                sig.returns.push(AbiParam::new(types::I64));
                
                let malloc_id = self.module
                    .declare_function("malloc", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare malloc: {}", e))?;
                let malloc_ref = self.module.declare_func_in_func(malloc_id, self.builder.func);
                
                let call = self.builder.ins().call(malloc_ref, &[size_val]);
                let ptr = self.builder.inst_results(call)[0];
                
                // Write string bytes to memory
                for (i, &byte) in s.as_bytes().iter().enumerate() {
                    let offset = self.builder.ins().iconst(types::I64, i as i64);
                    let addr = self.builder.ins().iadd(ptr, offset);
                    let byte_val = self.builder.ins().iconst(types::I8, byte as i64);
                    self.builder.ins().store(MemFlags::new(), byte_val, addr, 0);
                }
                
                // Null terminator
                let offset = self.builder.ins().iconst(types::I64, len as i64);
                let addr = self.builder.ins().iadd(ptr, offset);
                let zero = self.builder.ins().iconst(types::I8, 0);
                self.builder.ins().store(MemFlags::new(), zero, addr, 0);
                
                Ok(ptr)
            }
            Expr::Ident(name) => {
                let var = self.variables.get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                Ok(self.builder.use_var(*var))
            }
            Expr::Binary { op, left, right } => {
                let left_val = self.compile_expr(left)?;
                let right_val = self.compile_expr(right)?;
                
                let result = match op {
                    BinOp::Add => self.builder.ins().iadd(left_val, right_val),
                    BinOp::Sub => self.builder.ins().isub(left_val, right_val),
                    BinOp::Mul => self.builder.ins().imul(left_val, right_val),
                    BinOp::Div => self.builder.ins().sdiv(left_val, right_val),
                    BinOp::Mod => self.builder.ins().srem(left_val, right_val),
                    BinOp::Eq => {
                        let cmp = self.builder.ins().icmp(IntCC::Equal, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::NotEq => {
                        let cmp = self.builder.ins().icmp(IntCC::NotEqual, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::Lt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::Gt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::LtEq => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::GtEq => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_val, right_val);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                    BinOp::And => self.builder.ins().band(left_val, right_val),
                    BinOp::Or => self.builder.ins().bor(left_val, right_val),
                };
                
                Ok(result)
            }
            Expr::Unary { op, expr } => {
                let val = self.compile_expr(expr)?;
                
                let result = match op {
                    UnOp::Neg => self.builder.ins().ineg(val),
                    UnOp::Not => {
                        let zero = self.builder.ins().iconst(types::I8, 0);
                        let cmp = self.builder.ins().icmp(IntCC::Equal, val, zero);
                        self.builder.ins().uextend(types::I8, cmp)
                    }
                };
                
                Ok(result)
            }
            Expr::Let { name, value, then, .. } => {
                let val = self.compile_expr(value)?;
                
                let var = Variable::new(self.var_index);
                self.var_index += 1;
                
                // Determine type from value (simplified)
                self.builder.declare_var(var, types::I64);
                self.builder.def_var(var, val);
                self.variables.insert(name.clone(), var);
                
                self.compile_expr(then)
            }
            Expr::If { cond, then, else_ } => {
                let cond_val = self.compile_expr(cond)?;
                
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                
                self.builder.append_block_param(merge_block, types::I64);
                
                self.builder.ins().brif(cond_val, then_block, &[], else_block, &[]);
                
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr(then)?;
                self.builder.ins().jump(merge_block, &[then_val]);
                
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_ {
                    self.compile_expr(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, 0)
                };
                self.builder.ins().jump(merge_block, &[else_val]);
                
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                
                Ok(self.builder.block_params(merge_block)[0])
            }
            Expr::Block(exprs) => {
                let mut last_val = self.builder.ins().iconst(types::I64, 0);
                for expr in exprs {
                    last_val = self.compile_expr(expr)?;
                }
                Ok(last_val)
            }
            Expr::Call { func, args } => {
                if let Expr::Ident(name) = &**func {
                    // Resolve the function name properly with namespace handling
                    let function_name = if name.contains("::") {
                        // Already fully qualified namespace call
                        name.clone()
                    } else {
                        // Add current namespace if not already qualified and not a main function
                        if name == "main" {
                            name.clone()
                        } else {
                            self.mangle_symbol_name(name)
                        }
                    };
                    
                    let func_or_data_id = self.module
                        .get_name(&function_name)
                        .ok_or_else(|| format!("Function {} not found", name))?;
                    
                    let func_id = match func_or_data_id {
                        cranelift_module::FuncOrDataId::Func(id) => id,
                        _ => return Err(format!("{} is not a function", name)),
                    };
                    
                    let local_func = self.module.declare_func_in_func(func_id, self.builder.func);
                    
                    let mut arg_vals = Vec::new();
                    for arg in args {
                        arg_vals.push(self.compile_expr(arg)?);
                    }
                    
                    let call_inst = self.builder.ins().call(local_func, &arg_vals);
                    let results = self.builder.func.dfg.inst_results(call_inst);
                    
                    if results.is_empty() {
                        Ok(self.builder.ins().iconst(types::I64, 0))
                    } else {
                        Ok(results[0])
                    }
                } else {
                    // @TODO: Expand function call support beyond simple cases (complex expressions as function names)
                    Err("Only simple function calls supported for now".to_string())
                }
            }
            Expr::Return(val) => {
                if let Some(expr) = val {
                    let ret_val = self.compile_expr(expr)?;
                    self.builder.ins().return_(&[ret_val]);
                } else {
                    self.builder.ins().return_(&[]);
                }
                // Return a dummy value (unreachable)
                Ok(self.builder.ins().iconst(types::I64, 0))
            }
            Expr::Pipe { left, right } => {
                // Transform x |> f into f(x)
                let left_val = self.compile_expr(left)?;
                
                // The right side should be a function call or identifier
                match &**right {
                    Expr::Ident(func_name) => {
                        // Simple case: x |> f becomes f(x)
                        // Resolve the function name properly with namespace handling
                        let resolved_name = if func_name.contains("::") {
                            // Already fully qualified namespace call
                            func_name.clone()
                        } else {
                            // Add current namespace if not already qualified and not a main function
                            if func_name == "main" {
                                func_name.clone()
                            } else {
                                self.mangle_symbol_name(func_name)
                            }
                        };

                        // Get the function signature to ensure proper calling convention
                        let (param_types, return_type) = self.function_sigs
                            .get(&resolved_name)
                            .ok_or_else(|| format!("Function {} signature not found", resolved_name))?
                            .clone();
                        
                        // Verify the signature matches what we're passing
                        if param_types.len() != 1 {
                            return Err(format!(
                                "Pipe operator requires function with exactly 1 parameter, {} has {}",
                                func_name,
                                param_types.len()
                            ));
                        }
                        
                        let func_or_data_id = self.module
                            .get_name(&resolved_name)
                            .ok_or_else(|| format!("Function {} not found", resolved_name))?;
                        
                        let func_id = match func_or_data_id {
                            cranelift_module::FuncOrDataId::Func(id) => id,
                            _ => return Err(format!("{} is not a function", func_name)),
                        };
                        
                        let local_func = self.module.declare_func_in_func(func_id, self.builder.func);
                        let call_inst = self.builder.ins().call(local_func, &[left_val]);
                        
                        // Get the results properly using the DFG
                        let results = self.builder.func.dfg.inst_results(call_inst);
                        
                        if return_type.is_none() || results.is_empty() {
                            Ok(self.builder.ins().iconst(types::I64, 0))
                        } else {
                            Ok(results[0])
                        }
                    }
                    Expr::Call { func, args } => {
                        // x |> f(a, b) becomes f(x, a, b)
                        if let Expr::Ident(func_name) = &**func {
                            // Resolve the function name properly with namespace handling
                            let resolved_name = if func_name.contains("::") {
                                // Already fully qualified namespace call
                                func_name.clone()
                            } else {
                                // Add current namespace if not already qualified and not a main function
                                if func_name == "main" {
                                    func_name.clone()
                                } else {
                                    self.mangle_symbol_name(func_name)
                                }
                            };

                            let func_or_data_id = self.module
                                .get_name(&resolved_name)
                                .ok_or_else(|| format!("Function {} not found", resolved_name))?;
                            
                            let func_id = match func_or_data_id {
                                cranelift_module::FuncOrDataId::Func(id) => id,
                                _ => return Err(format!("{} is not a function", func_name)),
                            };
                            
                            let local_func = self.module.declare_func_in_func(func_id, self.builder.func);
                            
                            let mut arg_vals = vec![left_val];
                            for arg in args {
                                arg_vals.push(self.compile_expr(arg)?);
                            }
                            
                            let call_inst = self.builder.ins().call(local_func, &arg_vals);
                            let results = self.builder.func.dfg.inst_results(call_inst);
                            
                            if results.is_empty() {
                                Ok(self.builder.ins().iconst(types::I64, 0))
                            } else {
                                Ok(results[0])
                            }
                        } else {
                            Err("Pipe right-hand side must be a simple function call".to_string())
                        }
                    }
                    Expr::Pipe { .. } => {
                        // Nested pipe: x |> (y |> z) - just recursively compile
                        let right_val = self.compile_expr(right)?;
                        Ok(right_val)
                    }
                    _ => Err("Pipe operator requires function or call on right-hand side".to_string()),
                }
            }
            Expr::Lambda { params: _, body: _ } => {
                // @TODO: Implement lambda expressions with proper closure support
                Err("Lambda expressions require closure support (not yet fully implemented)".to_string())
            }
            Expr::Match { expr, arms } => {
                let match_val = self.compile_expr(expr)?;
                
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);
                
                // Create all blocks first
                let mut arm_blocks = Vec::new();
                for _ in arms.iter() {
                    arm_blocks.push(self.builder.create_block());
                }
                
                // Build the pattern matching chain
                for (i, arm) in arms.iter().enumerate() {
                    match &arm.pattern {
                        Pattern::Integer(n) => {
                            let pattern_val = self.builder.ins().iconst(types::I64, *n);
                            let cmp = self.builder.ins().icmp(IntCC::Equal, match_val, pattern_val);
                            
                            let next_block = if i + 1 < arms.len() {
                                self.builder.create_block()
                            } else {
                                // No more patterns - should not happen if wildcard is last
                                merge_block
                            };
                            
                            self.builder.ins().brif(cmp, arm_blocks[i], &[], next_block, &[]);
                            
                            // Seal and fill the arm block
                            self.builder.switch_to_block(arm_blocks[i]);
                            self.builder.seal_block(arm_blocks[i]);
                            let result = self.compile_expr(&arm.body)?;
                            self.builder.ins().jump(merge_block, &[result]);
                            
                            // Move to next check block
                            if i + 1 < arms.len() {
                                self.builder.switch_to_block(next_block);
                                self.builder.seal_block(next_block);
                            }
                        }
                        Pattern::Bool(b) => {
                            let pattern_val = self.builder.ins().iconst(types::I8, if *b { 1 } else { 0 });
                            let cmp = self.builder.ins().icmp(IntCC::Equal, match_val, pattern_val);
                            
                            let next_block = if i + 1 < arms.len() {
                                self.builder.create_block()
                            } else {
                                merge_block
                            };
                            
                            self.builder.ins().brif(cmp, arm_blocks[i], &[], next_block, &[]);
                            
                            self.builder.switch_to_block(arm_blocks[i]);
                            self.builder.seal_block(arm_blocks[i]);
                            let result = self.compile_expr(&arm.body)?;
                            self.builder.ins().jump(merge_block, &[result]);
                            
                            if i + 1 < arms.len() {
                                self.builder.switch_to_block(next_block);
                                self.builder.seal_block(next_block);
                            }
                        }
                        Pattern::Wildcard | Pattern::Ident(_) => {
                            // Wildcard always matches - jump directly
                            self.builder.ins().jump(arm_blocks[i], &[]);
                            
                            self.builder.switch_to_block(arm_blocks[i]);
                            self.builder.seal_block(arm_blocks[i]);
                            
                            if let Pattern::Ident(name) = &arm.pattern {
                                let var = Variable::new(self.var_index);
                                self.var_index += 1;
                                self.builder.declare_var(var, types::I64);
                                self.builder.def_var(var, match_val);
                                self.variables.insert(name.clone(), var);
                            }
                            
                            let result = self.compile_expr(&arm.body)?;
                            self.builder.ins().jump(merge_block, &[result]);
                            break;
                        }
                        Pattern::Struct { name, fields } => {
                            // @TODO: Implement struct pattern matching
                            
                            // Get struct layout for the pattern
                            let layout = self.struct_layouts.get(name)
                                .ok_or_else(|| format!("Unknown struct in pattern: {}", name))?;
                            
                            // For now, assume the matched value is always of the correct type
                            // (proper implementation would check type compatibility)
                            self.builder.ins().jump(arm_blocks[i], &[]);
                            
                            self.builder.switch_to_block(arm_blocks[i]);
                            self.builder.seal_block(arm_blocks[i]);
                            
                            // Extract and bind each field according to the pattern
                            for (field_name, field_pattern) in fields {
                                // Find field offset in the struct layout
                                let (_, field_type, offset) = layout.fields.iter()
                                    .find(|(n, _, _)| n == field_name)
                                    .ok_or_else(|| format!("Unknown field {} in struct {}", field_name, name))?;
                                
                                // Calculate field address
                                let offset_val = self.builder.ins().iconst(types::I64, *offset as i64);
                                let field_addr = self.builder.ins().iadd(match_val, offset_val);
                                
                                // Load field value 
                                let field_cranelift_type = type_to_cranelift(field_type)?;
                                let field_val = self.builder.ins().load(field_cranelift_type, MemFlags::new(), field_addr, 0);
                                
                                // Handle field pattern (currently only supports identifier patterns)
                                match field_pattern {
                                    Pattern::Ident(var_name) => {
                                        let var = Variable::new(self.var_index);
                                        self.var_index += 1;
                                        self.builder.declare_var(var, field_cranelift_type);
                                        self.builder.def_var(var, field_val);
                                        self.variables.insert(var_name.clone(), var);
                                    }
                                    Pattern::Wildcard => {
                                        // Ignore the field value
                                    }
                                    _ => {
                                        return Err(format!("Complex field patterns not yet supported in struct patterns"));
                                    }
                                }
                            }
                            
                            let result = self.compile_expr(&arm.body)?;
                            self.builder.ins().jump(merge_block, &[result]);
                            break;
                        }
                    }
                }
                
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }
            Expr::Field { expr, field } => {
                let _struct_ptr = self.compile_expr(expr)?;
                
                // @TODO: Implement field access with proper type information and offset computation
                // For now, we need type information to compute field offsets
                // This is a simplified implementation
                Err(format!("Field access not yet fully implemented (field: {})", field))
            }
            Expr::Method { expr, method, args } => {
                // Methods are just functions with self as first parameter
                let self_val = self.compile_expr(expr)?;
                
                // Look up the method function
                let func_or_data_id = self.module
                    .get_name(&method)
                    .ok_or_else(|| format!("Method {} not found", method))?;
                
                let func_id = match func_or_data_id {
                    cranelift_module::FuncOrDataId::Func(id) => id,
                    _ => return Err(format!("{} is not a function", method)),
                };
                
                let local_func = self.module.declare_func_in_func(func_id, self.builder.func);
                
                let mut arg_vals = vec![self_val];
                for arg in args {
                    arg_vals.push(self.compile_expr(arg)?);
                }
                
                let call = self.builder.ins().call(local_func, &arg_vals);
                let results = self.builder.inst_results(call);
                
                if results.is_empty() {
                    Ok(self.builder.ins().iconst(types::I64, 0))
                } else {
                    Ok(results[0])
                }
            }
            Expr::StructInit { name, fields } => {
                // Get struct layout
                let layout = self.struct_layouts.get(name)
                    .ok_or_else(|| format!("Unknown struct: {}", name))?;
                
                // Allocate memory for the struct
                let size_val = self.builder.ins().iconst(types::I64, layout.total_size as i64);
                
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                sig.returns.push(AbiParam::new(types::I64));
                
                let malloc_id = self.module
                    .declare_function("malloc", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare malloc: {}", e))?;
                let malloc_ref = self.module.declare_func_in_func(malloc_id, self.builder.func);
                
                let call = self.builder.ins().call(malloc_ref, &[size_val]);
                let struct_ptr = self.builder.inst_results(call)[0];
                
                // Initialize fields
                for (field_name, field_expr) in fields {
                    let field_val = self.compile_expr(field_expr)?;
                    
                    // Find field offset
                    let (_, _, offset) = layout.fields.iter()
                        .find(|(n, _, _)| n == field_name)
                        .ok_or_else(|| format!("Unknown field {} in struct {}", field_name, name))?;
                    
                    let offset_val = self.builder.ins().iconst(types::I64, *offset as i64);
                    let field_addr = self.builder.ins().iadd(struct_ptr, offset_val);
                    self.builder.ins().store(MemFlags::new(), field_val, field_addr, 0);
                }
                
                Ok(struct_ptr)
            }
            Expr::Alloc { ty, count } => {
                let size = type_size(ty)?;
                let total_size = if let Some(count_expr) = count {
                    let count_val = self.compile_expr(count_expr)?;
                    let size_val = self.builder.ins().iconst(types::I64, size as i64);
                    self.builder.ins().imul(size_val, count_val)
                } else {
                    self.builder.ins().iconst(types::I64, size as i64)
                };
                
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                sig.returns.push(AbiParam::new(types::I64));
                
                let malloc_id = self.module
                    .declare_function("malloc", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare malloc: {}", e))?;
                let malloc_ref = self.module.declare_func_in_func(malloc_id, self.builder.func);
                
                let call = self.builder.ins().call(malloc_ref, &[total_size]);
                let ptr = self.builder.inst_results(call)[0];
                
                // Track allocation for temp scope cleanup
                self.memory_tracker.push(ptr);
                
                Ok(ptr)
            }
            Expr::Free { ptr } => {
                let ptr_val = self.compile_expr(ptr)?;
                
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                
                let free_id = self.module
                    .declare_function("free", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare free: {}", e))?;
                let free_ref = self.module.declare_func_in_func(free_id, self.builder.func);
                
                self.builder.ins().call(free_ref, &[ptr_val]);
                
                Ok(self.builder.ins().iconst(types::I64, 0))
            }
            Expr::Ref { expr } => {
                // Taking a reference requires the value to be in memory
                // For simplicity, we allocate and store the value
                let val = self.compile_expr(expr)?;
                
                // Allocate space for one value (assuming i64)
                let size_val = self.builder.ins().iconst(types::I64, 8);
                
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                sig.returns.push(AbiParam::new(types::I64));
                
                let malloc_id = self.module
                    .declare_function("malloc", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare malloc: {}", e))?;
                let malloc_ref = self.module.declare_func_in_func(malloc_id, self.builder.func);
                
                let call = self.builder.ins().call(malloc_ref, &[size_val]);
                let ptr = self.builder.inst_results(call)[0];
                
                // Store the value
                self.builder.ins().store(MemFlags::new(), val, ptr, 0);
                
                Ok(ptr)
            }
            Expr::Deref { expr } => {
                let ptr = self.compile_expr(expr)?;
                let val = self.builder.ins().load(types::I64, MemFlags::new(), ptr, 0);
                Ok(val)
            }
            Expr::TempScope { body } => {
                let old_tracker_len = self.memory_tracker.len();
                
                // Compile body
                let result = self.compile_expr(body)?;
                
                // Free all allocations made in this scope
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                
                let free_id = self.module
                    .declare_function("free", Linkage::Import, &sig)
                    .map_err(|e| format!("Failed to declare free: {}", e))?;
                let free_ref = self.module.declare_func_in_func(free_id, self.builder.func);
                
                while self.memory_tracker.len() > old_tracker_len {
                    if let Some(ptr) = self.memory_tracker.pop() {
                        self.builder.ins().call(free_ref, &[ptr]);
                    }
                }
                
                Ok(result)
            }
            Expr::Unsafe { body } => {
                // Unsafe blocks don't change codegen, just type checking
                self.compile_expr(body)
            }
        }
    }
}

// External C functions
unsafe extern "C" {
    fn putchar(c: i32) -> i32;
    fn printf(format: *const u8, ...) -> i32;
    fn malloc(size: usize) -> *mut u8;
    fn free(ptr: *mut u8);
    fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8;
}

impl Compiler {
    fn compute_struct_layout(&mut self, struct_def: &Struct) -> Result<(), String> {
        let mut offset = 0;
        let mut fields = Vec::new();
        
        for field in &struct_def.fields {
            let size = type_size(&field.ty)?;
            let align = type_alignment(&field.ty)?;
            
            // Align offset
            offset = (offset + align - 1) & !(align - 1);
            
            fields.push((field.name.clone(), field.ty.clone(), offset));
            offset += size;
        }
        
        // Align total size to largest alignment
        let max_align = struct_def.fields.iter()
            .map(|f| type_alignment(&f.ty).unwrap_or(8))
            .max()
            .unwrap_or(8);
        offset = (offset + max_align - 1) & !(max_align - 1);
        
        self.struct_layouts.insert(
            struct_def.name.clone(),
            StructLayout {
                fields,
                total_size: offset,
            },
        );
        
        Ok(())
    }
}

fn type_size(ty: &flow_ast::Type) -> Result<usize, String> {
    match ty {
        flow_ast::Type::I8 | flow_ast::Type::U8 | flow_ast::Type::Bool => Ok(1),
        flow_ast::Type::I16 | flow_ast::Type::U16 => Ok(2),
        flow_ast::Type::I32 | flow_ast::Type::U32 | flow_ast::Type::F32 | flow_ast::Type::Char => Ok(4),
        flow_ast::Type::I64 | flow_ast::Type::U64 | flow_ast::Type::F64 => Ok(8),
        flow_ast::Type::I128 | flow_ast::Type::U128 => Ok(16),
        flow_ast::Type::Pointer(_) | flow_ast::Type::MutPointer(_) | flow_ast::Type::String => Ok(8),
        flow_ast::Type::Function(_, _) => Ok(8),
        flow_ast::Type::Named(_) => Ok(8), // Pointer to struct
        flow_ast::Type::Array(elem_ty, count) => {
            let elem_size = type_size(elem_ty)?;
            Ok(elem_size * count)
        }
        flow_ast::Type::Slice(_) => Ok(16), // Fat pointer: ptr + length
        flow_ast::Type::Unit => Ok(0),
        flow_ast::Type::TypeVar(_) => Err("Cannot compute size of type variable".to_string()),
    }
}

fn type_alignment(ty: &flow_ast::Type) -> Result<usize, String> {
    match ty {
        flow_ast::Type::I8 | flow_ast::Type::U8 | flow_ast::Type::Bool => Ok(1),
        flow_ast::Type::I16 | flow_ast::Type::U16 => Ok(2),
        flow_ast::Type::I32 | flow_ast::Type::U32 | flow_ast::Type::F32 | flow_ast::Type::Char => Ok(4),
        flow_ast::Type::I64 | flow_ast::Type::U64 | flow_ast::Type::F64 => Ok(8),
        flow_ast::Type::I128 | flow_ast::Type::U128 => Ok(16),
        flow_ast::Type::Pointer(_) | flow_ast::Type::MutPointer(_) | flow_ast::Type::String => Ok(8),
        flow_ast::Type::Function(_, _) => Ok(8),
        flow_ast::Type::Named(_) => Ok(8),
        flow_ast::Type::Array(elem_ty, _) => type_alignment(elem_ty),
        flow_ast::Type::Slice(_) => Ok(8),
        flow_ast::Type::Unit => Ok(1),
        flow_ast::Type::TypeVar(_) => Err("Cannot compute alignment of type variable".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Integer(42),
                is_pub: true,
                span: flow_ast::Span::new(0, 0),
            })],
        };

        let mut compiler = Compiler::new();
        let result = compiler.compile(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Integer(5)),
                    right: Box::new(Expr::Integer(3)),
                },
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 8);
    }

    #[test]
    fn test_pipe_operator() {
        // Test: 5 |> double becomes double(5)
        let program = Program {
            namespace: None,
            items: vec![
                Item::Function(Function {
                    name: "double".to_string(),
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: flow_ast::Type::I64,
                    }],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(2)),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Pipe {
                        left: Box::new(Expr::Integer(5)),
                        right: Box::new(Expr::Ident("double".to_string())),
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 10);
    }

    #[test]
    fn test_function_call() {
        // Test regular function call: main() calls double(5)
        let program = Program {
            namespace: None,
            items: vec![
                Item::Function(Function {
                    name: "double".to_string(),
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: flow_ast::Type::I64,
                    }],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(2)),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Call {
                        func: Box::new(Expr::Ident("double".to_string())),
                        args: vec![Expr::Integer(5)],
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 10);
    }

    #[test]
    fn test_match_expression() {
        // Test match with integer patterns
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Match {
                    expr: Box::new(Expr::Integer(42)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Integer(0),
                            guard: None,
                            body: Expr::Integer(100),
                        },
                        MatchArm {
                            pattern: Pattern::Integer(42),
                            guard: None,
                            body: Expr::Integer(200),
                        },
                        MatchArm {
                            pattern: Pattern::Wildcard,
                            guard: None,
                            body: Expr::Integer(300),
                        },
                    ],
                },
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 200);
    }

    #[test]
    fn test_memory_alloc_free() {
        // Test alloc and free
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Block(vec![
                    Expr::Let {
                        name: "ptr".to_string(),
                        mutable: false,
                        ty: None,
                        value: Box::new(Expr::Alloc {
                            ty: flow_ast::Type::I64,
                            count: None,
                        }),
                        then: Box::new(Expr::Block(vec![
                            Expr::Free {
                                ptr: Box::new(Expr::Ident("ptr".to_string())),
                            },
                            Expr::Integer(42),
                        ])),
                    },
                ]),
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 42);
    }

    #[test]
    fn test_ref_deref() {
        // Test reference and dereference
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Let {
                    name: "ptr".to_string(),
                    mutable: false,
                    ty: None,
                    value: Box::new(Expr::Ref {
                        expr: Box::new(Expr::Integer(99)),
                    }),
                    then: Box::new(Expr::Deref {
                        expr: Box::new(Expr::Ident("ptr".to_string())),
                    }),
                },
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 99);
    }

    #[test]
    fn test_struct_init() {
        use std::collections::HashMap;
        
        // Test struct initialization
        let program = Program {
            namespace: None,
            items: vec![
                Item::Struct(Struct {
                    name: "Point".to_string(),
                    fields: vec![
                        Field {
                            name: "x".to_string(),
                            ty: flow_ast::Type::I64,
                            is_pub: true,
                        },
                        Field {
                            name: "y".to_string(),
                            ty: flow_ast::Type::I64,
                            is_pub: true,
                        },
                    ],
                    is_pub: true,
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Let {
                        name: "p".to_string(),
                        mutable: false,
                        ty: None,
                        value: Box::new(Expr::StructInit {
                            name: "Point".to_string(),
                            fields: {
                                let mut map = HashMap::new();
                                map.insert("x".to_string(), Expr::Integer(10));
                                map.insert("y".to_string(), Expr::Integer(20));
                                map
                            },
                        }),
                        then: Box::new(Expr::Integer(42)),
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 42);
    }

    #[test]
    fn test_temp_scope() {
        // Test temporary scope with automatic cleanup
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::TempScope {
                    body: Box::new(Expr::Block(vec![
                        Expr::Alloc {
                            ty: flow_ast::Type::I64,
                            count: None,
                        },
                        Expr::Integer(55),
                    ])),
                },
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 55);
    }

    #[test]
    fn test_unsafe_block() {
        // Test unsafe block (should compile same as regular code)
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(flow_ast::Type::I64),
                body: Expr::Unsafe {
                    body: Box::new(Expr::Integer(77)),
                },
                is_pub: true,
            })],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 77);
    }

    #[test]
    fn test_chained_pipe() {
        // Test: 5 |> double |> add_ten
        let program = Program {
            namespace: None,
            items: vec![
                Item::Function(Function {
                    name: "double".to_string(),
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: flow_ast::Type::I64,
                    }],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(2)),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "add_ten".to_string(),
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: flow_ast::Type::I64,
                    }],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Add,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(10)),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Some(flow_ast::Type::I64),
                    body: Expr::Pipe {
                        left: Box::new(Expr::Pipe {
                            left: Box::new(Expr::Integer(5)),
                            right: Box::new(Expr::Ident("double".to_string())),
                        }),
                        right: Box::new(Expr::Ident("add_ten".to_string())),
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut compiler = Compiler::new();
        let code_ptr = compiler.compile(&program).unwrap();
        let main_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        assert_eq!(main_fn(), 20); // (5 * 2) + 10 = 20
    }
}

impl AOTCompiler {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = ObjectBuilder::new(
            isa,
            "main",
            cranelift_module::default_libcall_names(),
        ).unwrap();
        let module = ObjectModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            struct_layouts: HashMap::new(),
            string_data: Vec::new(),
            lambda_counter: 0,
            function_ids: HashMap::new(),
            function_sigs: HashMap::new(),
            current_namespace: None,
        }
    }

    pub fn compile_to_object(mut self, program: &Program) -> Result<Vec<u8>, String> {
        // Handle namespace declaration
        if let Some(namespace_decl) = &program.namespace {
            self.current_namespace = Some(namespace_decl.namespace.clone());
        }

        // First pass: compute struct layouts
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                self.compute_struct_layout(struct_def)?;
            }
        }
        
        // Second pass: declare all functions (including methods)
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.declare_function(method)?;
                    }
                }
                Item::Use(_) => {
                    // Use declarations are handled at analysis time
                    // The referenced functions should be available through linking
                }
                _ => {}
            }
        }

        // Third pass: compile function bodies
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.compile_function(func)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.compile_function(method)?;
                    }
                }
                Item::Struct(_) => {
                    // Already processed in first pass
                }
                Item::ExternBlock(_) => {
                    // External functions are linked
                }
                Item::Import(_) => {
                    // Imports are resolved at link time
                }
                Item::Use(_) => {
                    // Use declarations don't generate code
                }
            }
        }

        // Finalize the module and return object code
        let product = self.module.finish();
        product.emit().map_err(|e| format!("Failed to emit object code: {}", e))
    }

    /// Generate a mangled symbol name based on namespace  
    fn mangle_symbol_name(&self, name: &str) -> String {
        match &self.current_namespace {
            Some(namespace) => format!("{}::{}", namespace, name),
            None => name.to_string(),
        }
    }

    fn declare_function(&mut self, func: &Function) -> Result<(), String> {
        // Create a new signature with the ISA's default calling convention
        let call_conv = self.module.isa().default_call_conv();
        let mut sig = Signature::new(call_conv);

        for param in &func.params {
            let cranelift_type = type_to_cranelift(&param.ty)?;
            sig.params.push(AbiParam::new(cranelift_type));
        }

        if let Some(ref return_type) = func.return_type {
            let cranelift_type = type_to_cranelift(return_type)?;
            sig.returns.push(AbiParam::new(cranelift_type));
        }

        let linkage = if func.is_pub || func.name == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        };

        // Use mangled name for function declaration unless it's main
        let symbol_name = if func.name == "main" {
            func.name.clone()
        } else {
            self.mangle_symbol_name(&func.name)
        };

        let func_id = self.module
            .declare_function(&symbol_name, linkage, &sig)
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        
        // Now update the context signature for compilation
        self.ctx.func.signature = sig;

        // Store the function ID and signature for later retrieval
        // Use the mangled name as the key for lookups
        self.function_ids.insert(symbol_name.clone(), func_id);
        
        let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
        self.function_sigs.insert(symbol_name, (param_types, func.return_type.clone()));

        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> Result<(), String> {
        // Use mangled name for function lookup unless it's main
        let symbol_name = if func.name == "main" {
            func.name.clone()
        } else {
            self.mangle_symbol_name(&func.name)
        };

        let func_or_data_id = self.module
            .get_name(&symbol_name)
            .ok_or_else(|| format!("Function {} not declared", symbol_name))?;

        let id = match func_or_data_id {
            cranelift_module::FuncOrDataId::Func(id) => id,
            _ => return Err(format!("{} is not a function", symbol_name)),
        };

        self.ctx.func.clear();
        
        // Create signature with proper calling convention
        let call_conv = self.module.isa().default_call_conv();
        let mut sig = Signature::new(call_conv);

        for param in &func.params {
            let cranelift_type = type_to_cranelift(&param.ty)?;
            sig.params.push(AbiParam::new(cranelift_type));
        }

        if let Some(ref return_type) = func.return_type {
            let cranelift_type = type_to_cranelift(return_type)?;
            sig.returns.push(AbiParam::new(cranelift_type));
        }
        
        self.ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut func_compiler = AOTFunctionCompiler {
            builder,
            module: &mut self.module,
            variables: HashMap::new(),
            var_index: 0,
            struct_layouts: &self.struct_layouts,
            memory_tracker: Vec::new(),
            function_sigs: &self.function_sigs,
            function_ids: &self.function_ids,
        };

        // Declare parameters as variables
        for (i, param) in func.params.iter().enumerate() {
            let var = Variable::new(func_compiler.var_index);
            func_compiler.var_index += 1;
            
            let cranelift_type = type_to_cranelift(&param.ty)?;
            func_compiler.builder.declare_var(var, cranelift_type);
            
            let param_val = func_compiler.builder.block_params(entry_block)[i];
            func_compiler.builder.def_var(var, param_val);
            
            func_compiler.variables.insert(param.name.clone(), var);
        }

        let return_value = func_compiler.compile_expr(&func.body)?;

        if func.return_type.is_some() {
            func_compiler.builder.ins().return_(&[return_value]);
        } else {
            func_compiler.builder.ins().return_(&[]);
        }

        func_compiler.builder.finalize();

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| format!("Failed to define function: {}", e))?;

        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    fn compute_struct_layout(&mut self, struct_def: &Struct) -> Result<(), String> {
        let mut offset = 0;
        let mut fields = Vec::new();
        
        for field in &struct_def.fields {
            let size = type_size(&field.ty)?;
            let align = type_alignment(&field.ty)?;
            
            // Align offset
            offset = (offset + align - 1) & !(align - 1);
            
            fields.push((field.name.clone(), field.ty.clone(), offset));
            offset += size;
        }
        
        // Align total size to largest alignment
        let max_align = struct_def.fields.iter()
            .map(|f| type_alignment(&f.ty).unwrap_or(8))
            .max()
            .unwrap_or(8);
        offset = (offset + max_align - 1) & !(max_align - 1);
        
        self.struct_layouts.insert(
            struct_def.name.clone(),
            StructLayout {
                fields,
                total_size: offset,
            },
        );
        
        Ok(())
    }

    pub fn generate_executable(object_code: &[u8], output_path: &str) -> Result<(), String> {
        use std::process::Command;
        use std::fs::File;
        use std::io::Write;

        // Write object code to a temporary file
        let obj_file = format!("{}.o", output_path);
        let mut file = File::create(&obj_file)
            .map_err(|e| format!("Failed to create object file: {}", e))?;
        file.write_all(object_code)
            .map_err(|e| format!("Failed to write object file: {}", e))?;
        
        // Link the object file to create an executable
        let output = Command::new("cc")
            .args(&["-o", output_path, &obj_file])
            .output()
            .map_err(|e| format!("Failed to run linker: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("Linker failed: {}", stderr));
        }

        // Clean up the temporary object file
        std::fs::remove_file(&obj_file)
            .map_err(|e| format!("Failed to remove temporary object file: {}", e))?;

        Ok(())
    }
}

struct AOTFunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
    variables: HashMap<String, Variable>,
    var_index: usize,
    struct_layouts: &'a HashMap<String, StructLayout>,
    memory_tracker: Vec<Value>,
    function_sigs: &'a HashMap<String, (Vec<flow_ast::Type>, Option<flow_ast::Type>)>,
    function_ids: &'a HashMap<String, FuncId>,
}

impl<'a> AOTFunctionCompiler<'a> {
    fn compile_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Integer(n) => {
                Ok(self.builder.ins().iconst(types::I64, *n))
            }
            Expr::Float(f) => {
                Ok(self.builder.ins().f64const(*f))
            }
            Expr::Bool(b) => {
                let val = if *b { 1 } else { 0 };
                Ok(self.builder.ins().iconst(types::I8, val))
            }
            Expr::Unit => {
                Ok(self.builder.ins().iconst(types::I8, 0))
            }
            Expr::Ident(name) => {
                if let Some(&var) = self.variables.get(name) {
                    Ok(self.builder.use_var(var))
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expr::Binary { op, left, right } => {
                let left_val = self.compile_expr(left)?;
                let right_val = self.compile_expr(right)?;
                
                match op {
                    BinOp::Add => Ok(self.builder.ins().iadd(left_val, right_val)),
                    BinOp::Sub => Ok(self.builder.ins().isub(left_val, right_val)),
                    BinOp::Mul => Ok(self.builder.ins().imul(left_val, right_val)),
                    BinOp::Div => Ok(self.builder.ins().sdiv(left_val, right_val)),
                    BinOp::Mod => Ok(self.builder.ins().srem(left_val, right_val)),
                    BinOp::Eq => {
                        let cmp = self.builder.ins().icmp(IntCC::Equal, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::NotEq => {
                        let cmp = self.builder.ins().icmp(IntCC::NotEqual, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Lt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Gt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::LtEq => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::GtEq => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_val, right_val);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::And => Ok(self.builder.ins().band(left_val, right_val)),
                    BinOp::Or => Ok(self.builder.ins().bor(left_val, right_val)),
                }
            }
            Expr::Call { func, args } => {
                // Look up the function
                if let Expr::Ident(func_name) = func.as_ref() {
                    if let Some((_params, _return_type)) = self.function_sigs.get(func_name) {
                        // It's a user-defined function
                        let func_ref = self.module.declare_func_in_func(*self.function_ids.get(func_name).unwrap(), &mut self.builder.func);
                        
                        let mut arg_values = Vec::new();
                        for arg in args.iter() {
                            let arg_val = self.compile_expr(arg)?;
                            arg_values.push(arg_val);
                        }
                        
                        let call = self.builder.ins().call(func_ref, &arg_values);
                        return Ok(self.builder.inst_results(call)[0]);
                    } else {
                        return Err(format!("Unknown function: {}", func_name));
                    };
                } else {
                    return Err("Function calls must use identifiers".to_string());
                }
            }
            Expr::Let { name, ty: _, value, then, mutable: _ } => {
                let val = self.compile_expr(value)?;
                
                let var = Variable::new(self.var_index);
                self.var_index += 1;
                
                // Determine the type from the value
                let val_type = self.builder.func.dfg.value_type(val);
                self.builder.declare_var(var, val_type);
                self.builder.def_var(var, val);
                
                self.variables.insert(name.clone(), var);
                
                self.compile_expr(then)
            }
            Expr::If { cond, then, else_ } => {
                let cond_val = self.compile_expr(cond)?;
                
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                
                // Add a parameter to the merge block for the result
                self.builder.append_block_param(merge_block, types::I64);
                
                // Branch based on condition
                self.builder.ins().brif(cond_val, then_block, &[], else_block, &[]);
                
                // Compile then branch
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr(then)?;
                self.builder.ins().jump(merge_block, &[then_val]);
                
                // Compile else branch
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_ {
                    self.compile_expr(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, 0)
                };
                self.builder.ins().jump(merge_block, &[else_val]);
                
                // Switch to merge block
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                
                Ok(self.builder.block_params(merge_block)[0])
            }
            Expr::Block(exprs) => {
                let mut last_val = self.builder.ins().iconst(types::I64, 0);
                for expr in exprs {
                    last_val = self.compile_expr(expr)?;
                }
                Ok(last_val)
            }
            _ => {
                Err(format!("Expression not yet implemented in AOT compiler: {:?}", expr))
            }
        }
    }
}

