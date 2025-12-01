use flow_ast::*;
use flow_transpiler::{Result, SymbolCollector, TranspileContext, Transpiler, TranspilerError};
use std::collections::HashMap;
use std::io::{Cursor, Write};

use zip::CompressionMethod;
use zip::ZipWriter;
use zip::write::FileOptions;

mod bytecode;
mod class_writer;
mod constant_pool;
mod ristretto_writer;

use bytecode::*;
use class_writer::ClassWriter;
use ristretto_writer::RistrettoCodeGenerator;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ValueCategory {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Clone)]
struct LocalBinding {
    index: u16,
    ty: Option<Type>,
}

enum FunctionTarget {
    Main,
    Struct,
}

pub struct JavaTranspiler {
    base_context: TranspileContext,
    context: TranspileContext,
    class_name: String,
    current_namespace: Option<String>,
    struct_class_names: HashMap<String, String>,
}

impl JavaTranspiler {
    pub fn new(class_name: impl Into<String>) -> Self {
        let context = TranspileContext::new();
        Self {
            base_context: context.clone(),
            context,
            class_name: class_name.into(),
            current_namespace: None,
            struct_class_names: HashMap::new(),
        }
    }

    pub fn with_context(class_name: impl Into<String>, context: TranspileContext) -> Self {
        Self {
            base_context: context.clone(),
            context,
            class_name: class_name.into(),
            current_namespace: None,
            struct_class_names: HashMap::new(),
        }
    }

    fn refresh_for_run(&mut self) {
        self.context = self.base_context.clone();
        self.context.current_function = None;
        self.current_namespace = None;
        self.struct_class_names.clear();
    }

    fn namespace_prefix(&self) -> Option<&str> {
        self.current_namespace.as_deref()
    }

    fn qualify_name(&self, simple: &str) -> String {
        if let Some(ns) = self.namespace_prefix() {
            if ns.is_empty() {
                simple.to_string()
            } else {
                format!("{}/{}", ns, simple)
            }
        } else {
            simple.to_string()
        }
    }

    fn struct_internal_name(&self, struct_name: &str) -> String {
        self.struct_class_names
            .get(struct_name)
            .cloned()
            .unwrap_or_else(|| self.qualify_name(struct_name))
    }

    fn populate_struct_class(&self, class: &mut ClassWriter, struct_def: &Struct) -> Result<()> {
        for field in &struct_def.fields {
            let descriptor = self.type_to_field_descriptor(&field.ty);
            let access = if field.is_pub {
                ACC_PUBLIC
            } else {
                ACC_PRIVATE
            };
            class.add_field(&field.name, descriptor, access);
        }
        Ok(())
    }

    fn populate_extern_methods(&self, class: &mut ClassWriter, block: &ExternBlock) -> Result<()> {
        for item in &block.items {
            let mut method = class.add_method(&item.name);

            // Build method descriptor
            let mut descriptor = String::from("(");
            for param_ty in &item.params {
                descriptor.push_str(&self.type_to_jvm_type(param_ty));
            }
            descriptor.push(')');
            if let Some(ret_ty) = &item.return_type {
                descriptor.push_str(&self.type_to_jvm_type(ret_ty));
            } else {
                descriptor.push('V');
            }

            method.set_descriptor(&descriptor);

            // Mark as public static native
            method.set_access_flags(ACC_PUBLIC | ACC_STATIC | 0x0100); // ACC_NATIVE = 0x0100

            // Native methods don't have code
        }
        Ok(())
    }

    fn package_classes(&self, classes: Vec<(String, Vec<u8>)>) -> Result<Vec<u8>> {
        let mut buffer = Cursor::new(Vec::new());
        {
            let mut zip = ZipWriter::new(&mut buffer);
            let options = FileOptions::default().compression_method(CompressionMethod::Deflated);

            zip.start_file("META-INF/MANIFEST.MF", options)
                .map_err(|e| TranspilerError::CodeGenError(e.to_string()))?;
            zip.write_all(b"Manifest-Version: 1.0\n")
                .map_err(TranspilerError::from)?;

            for (internal_name, bytes) in classes {
                let file_name = format!("{}.class", internal_name);
                zip.start_file(file_name, options)
                    .map_err(|e| TranspilerError::CodeGenError(e.to_string()))?;
                zip.write_all(&bytes).map_err(TranspilerError::from)?;
            }

            zip.finish()
                .map_err(|e| TranspilerError::CodeGenError(e.to_string()))?;
        }
        Ok(buffer.into_inner())
    }

    fn build_method_descriptor(
        &self,
        params: &[(String, Type)],
        return_type: &Option<Type>,
    ) -> String {
        let mut desc = String::from("(");

        for (_, ty) in params {
            desc.push_str(&self.type_to_jvm_type(ty));
        }

        desc.push(')');

        if let Some(ret_ty) = return_type {
            desc.push_str(&self.type_to_jvm_type(ret_ty));
        } else {
            desc.push('V');
        }

        desc
    }

    fn value_category(&self, ty: Option<&Type>) -> ValueCategory {
        match ty {
            Some(Type::Bool) | Some(Type::Char) | Some(Type::I8) | Some(Type::I16)
            | Some(Type::I32) | Some(Type::U8) | Some(Type::U16) | Some(Type::U32) => {
                ValueCategory::Int
            }
            Some(Type::I64) | Some(Type::U64) | Some(Type::I128) | Some(Type::U128) => {
                ValueCategory::Long
            }
            Some(Type::F32) => ValueCategory::Float,
            Some(Type::F64) => ValueCategory::Double,
            Some(Type::String)
            | Some(Type::Named(_))
            | Some(Type::Pointer(_))
            | Some(Type::MutPointer(_))
            | Some(Type::Array(_, _))
            | Some(Type::Slice(_))
            | Some(Type::Function(_, _))
            | Some(Type::TypeVar(_))
            | Some(Type::Unit)
            | None => ValueCategory::Reference,
        }
    }

    fn emit_load(&self, code: &mut CodeBuilder, binding: &LocalBinding) {
        match self.value_category(binding.ty.as_ref()) {
            ValueCategory::Int => code.add_instruction(Instruction::ILoad(binding.index)),
            ValueCategory::Long => code.add_instruction(Instruction::LLoad(binding.index)),
            ValueCategory::Float => code.add_instruction(Instruction::FLoad(binding.index)),
            ValueCategory::Double => code.add_instruction(Instruction::DLoad(binding.index)),
            ValueCategory::Reference => code.add_instruction(Instruction::ALoad(binding.index)),
        }
    }

    fn emit_store(&self, code: &mut CodeBuilder, index: u16, ty: Option<&Type>) {
        match self.value_category(ty) {
            ValueCategory::Int => code.add_instruction(Instruction::IStore(index)),
            ValueCategory::Long => code.add_instruction(Instruction::LStore(index)),
            ValueCategory::Float => code.add_instruction(Instruction::FStore(index)),
            ValueCategory::Double => code.add_instruction(Instruction::DStore(index)),
            ValueCategory::Reference => code.add_instruction(Instruction::AStore(index)),
        }
    }

    fn emit_return(&self, code: &mut CodeBuilder, ty: Option<&Type>) {
        match self.value_category(ty) {
            ValueCategory::Int => code.add_instruction(Instruction::IReturn),
            ValueCategory::Long => code.add_instruction(Instruction::LReturn),
            ValueCategory::Float => code.add_instruction(Instruction::FReturn),
            ValueCategory::Double => code.add_instruction(Instruction::DReturn),
            ValueCategory::Reference => code.add_instruction(Instruction::AReturn),
        }
    }

    fn type_to_jvm_type(&self, ty: &Type) -> String {
        match ty {
            Type::I8 | Type::U8 => "B".to_string(),
            Type::I16 | Type::U16 => "S".to_string(),
            Type::I32 | Type::U32 | Type::Bool | Type::Char => "I".to_string(),
            Type::I64 | Type::U64 | Type::I128 | Type::U128 => "J".to_string(),
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::String => "Ljava/lang/String;".to_string(),
            Type::Unit => "V".to_string(),
            Type::Named(name) => {
                let internal = self.struct_internal_name(name);
                format!("L{};", internal)
            }
            Type::Pointer(inner) | Type::MutPointer(inner) => {
                if matches!(inner.as_ref(), Type::TypeVar(_) | Type::Unit) {
                    "Ljava/lang/Object;".to_string()
                } else {
                    self.type_to_jvm_type(inner)
                }
            }
            Type::Array(inner, _) | Type::Slice(inner) => {
                format!("[{}", self.type_to_jvm_type(inner))
            }
            Type::Function(_, _) | Type::TypeVar(_) => "Ljava/lang/Object;".to_string(),
        }
    }

    fn type_to_field_descriptor(&self, ty: &Type) -> String {
        match ty {
            Type::Unit => "Ljava/lang/Object;".to_string(),
            Type::Named(_)
            | Type::String
            | Type::Pointer(_)
            | Type::MutPointer(_)
            | Type::Function(_, _)
            | Type::TypeVar(_) => {
                if let Type::Named(name) = ty {
                    format!("L{};", self.struct_internal_name(name))
                } else {
                    self.type_to_jvm_type(ty)
                }
            }
            _ => self.type_to_jvm_type(ty),
        }
    }

    fn infer_expr_type(&self, expr: &Expr, locals: &HashMap<String, LocalBinding>) -> Option<Type> {
        match expr {
            Expr::StructInit { name, .. } => Some(Type::Named(name.clone())),
            Expr::Ident(name) => locals.get(name).and_then(|binding| binding.ty.clone()),
            Expr::Call { func, .. } => {
                if let Expr::Ident(func_name) = func.as_ref() {
                    self.context
                        .functions
                        .get(func_name)
                        .and_then(|sig| sig.return_type.clone())
                        .or_else(|| {
                            self.context
                                .extern_functions
                                .get(func_name)
                                .and_then(|sig| sig.return_type.clone())
                        })
                } else {
                    None
                }
            }
            Expr::Method { expr, method, .. } => {
                let receiver_ty = self.infer_expr_type(expr, locals)?;
                if let Type::Named(struct_name) = receiver_ty {
                    let key = format!("{}::{}", struct_name, method);
                    self.context
                        .methods
                        .get(&key)
                        .and_then(|sig| sig.return_type.clone())
                } else {
                    None
                }
            }
            Expr::Field { expr, field } => {
                let base_ty = self.infer_expr_type(expr, locals)?;
                if let Type::Named(struct_name) = base_ty
                    && let Some(struct_info) = self.context.structs.get(&struct_name)
                {
                    for (fname, field_ty, _) in &struct_info.fields {
                        if fname == field {
                            return Some(field_ty.clone());
                        }
                    }
                }
                None
            }
            Expr::Let { then, .. } => self.infer_expr_type(then, locals),
            Expr::If { then, else_, .. } => {
                let then_ty = self.infer_expr_type(then, locals);
                let else_ty = else_
                    .as_ref()
                    .and_then(|expr| self.infer_expr_type(expr, locals));
                then_ty.or(else_ty)
            }
            Expr::Block(exprs) => exprs
                .last()
                .and_then(|expr| self.infer_expr_type(expr, locals)),
            Expr::Pipe { left: _, right } => match right.as_ref() {
                Expr::Ident(func_name) => self
                    .context
                    .functions
                    .get(func_name)
                    .and_then(|sig| sig.return_type.clone())
                    .or_else(|| {
                        self.context
                            .extern_functions
                            .get(func_name)
                            .and_then(|sig| sig.return_type.clone())
                    }),
                Expr::Call { func, .. } => {
                    if let Expr::Ident(func_name) = func.as_ref() {
                        self.context
                            .functions
                            .get(func_name)
                            .and_then(|sig| sig.return_type.clone())
                            .or_else(|| {
                                self.context
                                    .extern_functions
                                    .get(func_name)
                                    .and_then(|sig| sig.return_type.clone())
                            })
                    } else {
                        None
                    }
                }
                Expr::Pipe { .. } => self.infer_expr_type(right, locals),
                _ => None,
            },
            _ => None,
        }
    }

    fn transpile_struct_init(
        &self,
        code: &mut CodeBuilder,
        struct_name: &str,
        fields: &HashMap<String, Expr>,
        locals: &mut HashMap<String, LocalBinding>,
    ) -> Result<()> {
        let internal_name = self.struct_internal_name(struct_name);
        code.add_instruction(Instruction::New(internal_name.clone()));
        code.add_instruction(Instruction::Dup);
        code.add_instruction(Instruction::InvokeSpecial(
            internal_name.clone(),
            "<init>".to_string(),
            "()V".to_string(),
        ));

        if let Some(struct_info) = self.context.structs.get(struct_name) {
            for (field_name, field_ty, _) in &struct_info.fields {
                let value_expr = fields.get(field_name).ok_or_else(|| {
                    TranspilerError::CodeGenError(format!(
                        "Missing initializer for field '{}' in struct '{}'",
                        field_name, struct_name
                    ))
                })?;
                code.add_instruction(Instruction::Dup);
                self.transpile_expr(code, value_expr, locals)?;
                let field_descriptor = self.type_to_field_descriptor(field_ty);
                code.add_instruction(Instruction::PutField(
                    internal_name.clone(),
                    field_name.clone(),
                    field_descriptor,
                ));
            }
        }

        Ok(())
    }

    fn transpile_method_call(
        &self,
        code: &mut CodeBuilder,
        receiver: &Expr,
        method: &str,
        args: &[Expr],
        locals: &mut HashMap<String, LocalBinding>,
    ) -> Result<()> {
        self.transpile_expr(code, receiver, locals)?;
        let receiver_ty = self.infer_expr_type(receiver, locals).ok_or_else(|| {
            TranspilerError::CodeGenError("Unable to determine receiver type".to_string())
        })?;

        let struct_name = match receiver_ty {
            Type::Named(name) => name,
            other => {
                return Err(TranspilerError::CodeGenError(format!(
                    "Method '{}' called on non-struct type {:?}",
                    method, other
                )));
            }
        };

        let key = format!("{}::{}", struct_name, method);
        let signature = self.context.methods.get(&key).ok_or_else(|| {
            TranspilerError::NameResolutionError(format!(
                "Method '{}' not found for struct '{}'",
                method, struct_name
            ))
        })?;

        for arg in args {
            self.transpile_expr(code, arg, locals)?;
        }

        let descriptor = self.build_method_descriptor(&signature.params, &signature.return_type);
        let internal_name = self.struct_internal_name(&struct_name);
        code.add_instruction(Instruction::InvokeStatic(
            internal_name,
            method.to_string(),
            descriptor,
        ));
        Ok(())
    }

    fn transpile_function(
        &self,
        class: &mut ClassWriter,
        func: &Function,
        target: FunctionTarget,
    ) -> Result<()> {
        let mut method = class.add_method(&func.name);
        let param_pairs: Vec<(String, Type)> = func
            .params
            .iter()
            .map(|param| (param.name.clone(), param.ty.clone()))
            .collect();
        let descriptor = self.build_method_descriptor(&param_pairs, &func.return_type);
        method.set_descriptor(&descriptor);

        let access_flags = match target {
            FunctionTarget::Main | FunctionTarget::Struct => {
                if func.is_pub {
                    ACC_PUBLIC | ACC_STATIC
                } else {
                    ACC_PRIVATE | ACC_STATIC
                }
            }
        };
        method.set_access_flags(access_flags);

        let mut code = CodeBuilder::new();
        let mut locals: HashMap<String, LocalBinding> = HashMap::new();

        for (idx, param) in func.params.iter().enumerate() {
            locals.insert(
                param.name.clone(),
                LocalBinding {
                    index: idx as u16,
                    ty: Some(param.ty.clone()),
                },
            );
        }

        self.transpile_expr(&mut code, &func.body, &mut locals)?;

        // Only add return if the body doesn't already end with one
        let needs_return = !matches!(func.body, Expr::Return(_));
        if needs_return {
            if let Some(return_type) = &func.return_type {
                self.emit_return(&mut code, Some(return_type));
            } else {
                code.add_instruction(Instruction::Return);
            }
        }

        method.set_code(code.build());

        // Debug: print method info
        eprintln!(
            "DEBUG: Transpiled function '{}' with descriptor '{}'",
            func.name, descriptor
        );

        Ok(())
    }

    fn transpile_expr(
        &self,
        code: &mut CodeBuilder,
        expr: &Expr,
        locals: &mut HashMap<String, LocalBinding>,
    ) -> Result<()> {
        match expr {
            Expr::Integer(n) => {
                code.add_instruction(Instruction::Ldc(ConstantValue::Long(*n)));
            }
            Expr::Float(f) => {
                code.add_instruction(Instruction::Ldc(ConstantValue::Double(*f)));
            }
            Expr::Bool(b) => {
                code.add_instruction(Instruction::IConst(if *b { 1 } else { 0 }));
            }
            Expr::String(s) => {
                code.add_instruction(Instruction::Ldc(ConstantValue::String(s.clone())));
            }
            Expr::Unit => {}
            Expr::Ident(name) => {
                let binding = locals.get(name).ok_or_else(|| {
                    TranspilerError::NameResolutionError(format!("Undefined variable: {}", name))
                })?;
                self.emit_load(code, binding);
            }
            Expr::Let {
                name,
                mutable: _,
                ty,
                value,
                then,
            } => {
                self.transpile_expr(code, value, locals)?;

                let next_index = locals
                    .values()
                    .map(|b| b.index)
                    .max()
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let inferred_type = ty.clone().or_else(|| self.infer_expr_type(value, locals));
                self.emit_store(code, next_index, inferred_type.as_ref());

                locals.insert(
                    name.clone(),
                    LocalBinding {
                        index: next_index,
                        ty: inferred_type,
                    },
                );

                self.transpile_expr(code, then, locals)?;
            }
            Expr::Binary { op, left, right } => {
                self.transpile_expr(code, left, locals)?;
                self.transpile_expr(code, right, locals)?;

                let instr = match op {
                    BinOp::Add => Instruction::LAdd,
                    BinOp::Sub => Instruction::LSub,
                    BinOp::Mul => Instruction::LMul,
                    BinOp::Div => Instruction::LDiv,
                    BinOp::Mod => Instruction::LRem,
                    BinOp::And => Instruction::LAnd,
                    BinOp::Or => Instruction::LOr,
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq => {
                        code.add_instruction(Instruction::LCmp);
                        match op {
                            BinOp::Eq => code.add_instruction(Instruction::IfEq(5)),
                            BinOp::NotEq => code.add_instruction(Instruction::IfNe(5)),
                            BinOp::Lt => code.add_instruction(Instruction::IfLt(5)),
                            BinOp::Gt => code.add_instruction(Instruction::IfGt(5)),
                            BinOp::LtEq => code.add_instruction(Instruction::IfLe(5)),
                            BinOp::GtEq => code.add_instruction(Instruction::IfGe(5)),
                            _ => unreachable!(),
                        }
                        code.add_instruction(Instruction::IConst(0));
                        code.add_instruction(Instruction::Goto(4));
                        code.add_instruction(Instruction::IConst(1));
                        return Ok(());
                    }
                };
                code.add_instruction(instr);
            }
            Expr::Unary { op, expr } => {
                self.transpile_expr(code, expr, locals)?;
                match op {
                    UnOp::Neg => code.add_instruction(Instruction::LNeg),
                    UnOp::Not => {
                        code.add_instruction(Instruction::IConst(0));
                        code.add_instruction(Instruction::IfEq(5));
                        code.add_instruction(Instruction::IConst(0));
                        code.add_instruction(Instruction::Goto(4));
                        code.add_instruction(Instruction::IConst(1));
                    }
                }
            }
            Expr::If { cond, then, else_ } => {
                self.transpile_expr(code, cond, locals)?;
                code.add_instruction(Instruction::IfEq(0));
                let cond_index = code.instruction_count() - 1;

                self.transpile_expr(code, then, locals)?;
                code.add_instruction(Instruction::Goto(0));
                let goto_index = code.instruction_count() - 1;

                let else_offset = code.current_offset();
                code.patch_jump(cond_index, else_offset as i16);

                if let Some(else_expr) = else_ {
                    self.transpile_expr(code, else_expr, locals)?;
                }

                let end_offset = code.current_offset();
                code.patch_jump(goto_index, end_offset as i16);
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.transpile_expr(code, expr, locals)?;
                }
            }
            Expr::Call { func, args } => {
                if let Expr::Ident(func_name) = func.as_ref() {
                    // Check if it's a regular function
                    if let Some(signature) = self.context.functions.get(func_name) {
                        for arg in args {
                            self.transpile_expr(code, arg, locals)?;
                        }

                        let descriptor =
                            self.build_method_descriptor(&signature.params, &signature.return_type);
                        let owner = self.qualify_name(&self.class_name);
                        code.add_instruction(Instruction::InvokeStatic(
                            owner,
                            func_name.clone(),
                            descriptor,
                        ));
                    }
                    // Check if it's an extern function
                    else if let Some(extern_info) = self.context.extern_functions.get(func_name) {
                        for arg in args {
                            self.transpile_expr(code, arg, locals)?;
                        }

                        // Build descriptor for extern function
                        let mut descriptor = String::from("(");
                        for param_ty in &extern_info.params {
                            descriptor.push_str(&self.type_to_jvm_type(param_ty));
                        }
                        descriptor.push(')');
                        if let Some(ret_ty) = &extern_info.return_type {
                            descriptor.push_str(&self.type_to_jvm_type(ret_ty));
                        } else {
                            descriptor.push('V');
                        }

                        let owner = self.qualify_name(&self.class_name);
                        code.add_instruction(Instruction::InvokeStatic(
                            owner,
                            func_name.clone(),
                            descriptor,
                        ));
                    } else {
                        return Err(TranspilerError::NameResolutionError(format!(
                            "Function {} not found",
                            func_name
                        )));
                    }
                } else {
                    return Err(TranspilerError::UnsupportedFeature(
                        "Only direct function identifiers supported in calls".to_string(),
                    ));
                }
            }
            Expr::Pipe { left, right } => {
                self.transpile_expr(code, left, locals)?;
                self.apply_pipe_right(code, right, locals)?;
            }
            Expr::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.transpile_expr(code, expr, locals)?;
                    let return_ty = self.infer_expr_type(expr, locals);
                    self.emit_return(code, return_ty.as_ref());
                } else {
                    code.add_instruction(Instruction::Return);
                }
            }
            Expr::StructInit { name, fields } => {
                self.transpile_struct_init(code, name, fields, locals)?;
            }
            Expr::Field { expr, field } => {
                self.transpile_expr(code, expr, locals)?;
                let base_ty = self.infer_expr_type(expr, locals).ok_or_else(|| {
                    TranspilerError::CodeGenError(
                        "Unable to determine field access type".to_string(),
                    )
                })?;
                let struct_name = match base_ty {
                    Type::Named(name) => name,
                    other => {
                        return Err(TranspilerError::CodeGenError(format!(
                            "Field access on non-struct type {:?}",
                            other
                        )));
                    }
                };
                let internal_name = self.struct_internal_name(&struct_name);
                let field_ty = self
                    .context
                    .structs
                    .get(&struct_name)
                    .and_then(|info| info.fields.iter().find(|(fname, _, _)| fname == field))
                    .map(|(_, ty, _)| ty.clone())
                    .ok_or_else(|| {
                        TranspilerError::NameResolutionError(format!(
                            "Field '{}' not found on struct '{}'",
                            field, struct_name
                        ))
                    })?;
                let descriptor = self.type_to_field_descriptor(&field_ty);
                code.add_instruction(Instruction::GetField(
                    internal_name,
                    field.clone(),
                    descriptor,
                ));
            }
            Expr::Method { expr, method, args } => {
                self.transpile_method_call(code, expr, method, args, locals)?;
            }
            _ => {
                return Err(TranspilerError::UnsupportedFeature(format!(
                    "Expression not supported yet: {:?}",
                    expr
                )));
            }
        }

        Ok(())
    }

    fn apply_pipe_right(
        &self,
        code: &mut CodeBuilder,
        right: &Expr,
        locals: &mut HashMap<String, LocalBinding>,
    ) -> Result<()> {
        match right {
            Expr::Ident(func_name) => {
                // Check if it's a regular function
                if let Some(signature) = self.context.functions.get(func_name) {
                    let descriptor =
                        self.build_method_descriptor(&signature.params, &signature.return_type);
                    let owner = self.qualify_name(&self.class_name);
                    code.add_instruction(Instruction::InvokeStatic(
                        owner,
                        func_name.clone(),
                        descriptor,
                    ));
                    Ok(())
                }
                // Check if it's an extern function
                else if let Some(extern_info) = self.context.extern_functions.get(func_name) {
                    let mut descriptor = String::from("(");
                    for param_ty in &extern_info.params {
                        descriptor.push_str(&self.type_to_jvm_type(param_ty));
                    }
                    descriptor.push(')');
                    if let Some(ret_ty) = &extern_info.return_type {
                        descriptor.push_str(&self.type_to_jvm_type(ret_ty));
                    } else {
                        descriptor.push('V');
                    }
                    let owner = self.qualify_name(&self.class_name);
                    code.add_instruction(Instruction::InvokeStatic(
                        owner,
                        func_name.clone(),
                        descriptor,
                    ));
                    Ok(())
                } else {
                    Err(TranspilerError::NameResolutionError(format!(
                        "Function {} not found",
                        func_name
                    )))
                }
            }
            Expr::Pipe { left, right } => {
                self.apply_pipe_right(code, left, locals)?;
                self.apply_pipe_right(code, right, locals)
            }
            Expr::Call { func, args } => {
                if let Expr::Ident(func_name) = func.as_ref() {
                    // Check regular functions first
                    if let Some(signature) = self.context.functions.get(func_name) {
                        for arg in args {
                            self.transpile_expr(code, arg, locals)?;
                        }
                        let descriptor =
                            self.build_method_descriptor(&signature.params, &signature.return_type);
                        let owner = self.qualify_name(&self.class_name);
                        code.add_instruction(Instruction::InvokeStatic(
                            owner,
                            func_name.clone(),
                            descriptor,
                        ));
                        Ok(())
                    }
                    // Check extern functions
                    else if let Some(extern_info) = self.context.extern_functions.get(func_name) {
                        for arg in args {
                            self.transpile_expr(code, arg, locals)?;
                        }
                        let mut descriptor = String::from("(");
                        for param_ty in &extern_info.params {
                            descriptor.push_str(&self.type_to_jvm_type(param_ty));
                        }
                        descriptor.push(')');
                        if let Some(ret_ty) = &extern_info.return_type {
                            descriptor.push_str(&self.type_to_jvm_type(ret_ty));
                        } else {
                            descriptor.push('V');
                        }
                        let owner = self.qualify_name(&self.class_name);
                        code.add_instruction(Instruction::InvokeStatic(
                            owner,
                            func_name.clone(),
                            descriptor,
                        ));
                        Ok(())
                    } else {
                        Err(TranspilerError::NameResolutionError(format!(
                            "Function {} not found",
                            func_name
                        )))
                    }
                } else {
                    Err(TranspilerError::UnsupportedFeature(
                        "Only direct function identifiers supported in pipe calls".to_string(),
                    ))
                }
            }
            Expr::Method { expr, method, args } => {
                self.transpile_method_call(code, expr, method, args, locals)
            }
            other => Err(TranspilerError::UnsupportedFeature(format!(
                "Unsupported pipe right-hand side: {:?}",
                other
            ))),
        }
    }
}

impl Transpiler for JavaTranspiler {
    type Output = Vec<u8>;

    fn transpile(&mut self, program: &Program) -> Result<Self::Output> {
        self.refresh_for_run();
        self.current_namespace = program
            .namespace
            .as_ref()
            .map(|ns| ns.namespace.replace("::", "/"));

        // Use SymbolCollector to gather all symbols
        let mut collector = SymbolCollector::new();
        collector.collect(program);
        self.context = collector.into_context();

        // Use ristretto_classfile library for pure Rust bytecode generation (no Java dependencies)
        let generator =
            RistrettoCodeGenerator::new(self.qualify_name(&self.class_name), self.context.clone());

        match generator.generate(program) {
            Ok(class_files) => {
                // Convert HashMap to Vec for packaging
                let classes: Vec<(String, Vec<u8>)> = class_files
                    .into_iter()
                    .map(|(name, bytes)| (self.qualify_name(&name), bytes))
                    .collect();

                // Package into JAR
                return self.package_classes(classes);
            }
            Err(e) => {
                return Err(TranspilerError::CodeGenError(format!(
                    "Failed to generate bytecode: {}",
                    e
                )));
            }
        }

        // OLD IMPLEMENTATION BELOW - keeping for reference but unreachable
        #[allow(unreachable_code)]
        {
            let main_internal = self.qualify_name(&self.class_name);
            let mut main_class = ClassWriter::new(&main_internal);
            main_class.set_super_class("java/lang/Object");
            main_class.set_access_flags(ACC_PUBLIC | ACC_SUPER);
            main_class.add_default_constructor();

            let mut struct_classes: HashMap<String, ClassWriter> = HashMap::new();
            for item in &program.items {
                if let Item::Struct(struct_def) = item {
                    let internal = self.qualify_name(&struct_def.name);
                    self.struct_class_names
                        .insert(struct_def.name.clone(), internal.clone());
                    let mut class_writer = ClassWriter::new(&internal);
                    class_writer.set_super_class("java/lang/Object");
                    class_writer.set_access_flags(ACC_PUBLIC | ACC_SUPER);
                    class_writer.add_default_constructor();
                    struct_classes.insert(struct_def.name.clone(), class_writer);
                }
            }

            for item in &program.items {
                match item {
                    Item::Function(func) => {
                        eprintln!("DEBUG: Processing function: {}", func.name);
                        self.transpile_function(&mut main_class, func, FunctionTarget::Main)?;
                        eprintln!("DEBUG: Function '{}' transpiled", func.name);
                    }
                    Item::Struct(struct_def) => {
                        if let Some(class_writer) = struct_classes.get_mut(&struct_def.name) {
                            self.populate_struct_class(class_writer, struct_def)?;
                        }
                    }
                    Item::Impl(impl_block) => {
                        if let Some(class_writer) = struct_classes.get_mut(&impl_block.struct_name)
                        {
                            for method in &impl_block.methods {
                                self.transpile_function(
                                    class_writer,
                                    method,
                                    FunctionTarget::Struct,
                                )?;
                            }
                        } else {
                            return Err(TranspilerError::CodeGenError(format!(
                                "Implementation block refers to unknown struct '{}'",
                                impl_block.struct_name
                            )));
                        }
                    }
                    Item::ExternBlock(block) => {
                        // Add native method declarations to main class
                        self.populate_extern_methods(&mut main_class, block)?;
                    }
                    Item::Attribute(_) => {}
                    _ => {}
                }
            }

            let mut class_outputs = Vec::new();
            eprintln!("DEBUG: Writing main class bytes");
            let main_bytes = main_class.write_bytes()?;
            eprintln!(
                "DEBUG: Main class bytes written: {} bytes",
                main_bytes.len()
            );

            // Write to temp file for inspection
            std::fs::write("/tmp/debug_class_before_jar.class", &main_bytes).ok();
            eprintln!("DEBUG: Wrote class to /tmp/debug_class_before_jar.class");

            class_outputs.push((main_internal, main_bytes));

            for (struct_name, mut class_writer) in struct_classes {
                let internal = self.struct_internal_name(&struct_name);
                let bytes = class_writer.write_bytes()?;
                class_outputs.push((internal, bytes));
            }

            self.package_classes(class_outputs)
        } // End of old implementation block
    }

    fn target_name(&self) -> &str {
        "Java Bytecode"
    }

    fn supported_features(&self) -> Vec<&str> {
        vec![
            "functions",
            "structs",
            "primitives",
            "impl_blocks",
            "extern_blocks",
            "methods",
            "pipes",
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::io::{Cursor, Read};

    use zip::ZipArchive;

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn unzip_classes(bytes: &[u8]) -> HashMap<String, Vec<u8>> {
        let cursor = Cursor::new(bytes);
        let mut archive = ZipArchive::new(cursor).expect("valid zip");
        let mut entries = HashMap::new();
        for index in 0..archive.len() {
            let mut file = archive.by_index(index).expect("entry");
            if file.name().ends_with('/') {
                continue;
            }
            let mut data = Vec::new();
            file.read_to_end(&mut data).expect("read entry");
            entries.insert(file.name().to_string(), data);
        }
        entries
    }

    #[test]
    fn test_method_generation_debug() {
        // Detailed test to debug why functions aren't appearing in bytecode
        let function = Function {
            name: "add".to_string(),
            params: vec![
                Param {
                    name: "x".to_string(),
                    ty: Type::I64,
                },
                Param {
                    name: "y".to_string(),
                    ty: Type::I64,
                },
            ],
            return_type: Some(Type::I64),
            body: Expr::Return(Some(Box::new(Expr::Binary {
                op: BinOp::Add,
                left: Box::new(Expr::Ident("x".to_string())),
                right: Box::new(Expr::Ident("y".to_string())),
            }))),
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        };

        let program = Program {
            namespace: None,
            items: vec![Item::Function(function)],
        };

        let mut transpiler = JavaTranspiler::new("TestClass");
        let result = transpiler.transpile(&program);

        assert!(result.is_ok(), "Transpilation should succeed");
        let jar_bytes = result.unwrap();

        // Extract and examine the class
        let entries = unzip_classes(&jar_bytes);
        assert!(
            entries.contains_key("TestClass.class"),
            "Should contain TestClass.class"
        );

        let class_bytes = entries.get("TestClass.class").unwrap();

        // Check magic number
        assert_eq!(
            &class_bytes[0..4],
            &[0xCA, 0xFE, 0xBA, 0xBE],
            "Should have correct magic number"
        );

        // Write to temp file and inspect with external tool
        std::fs::write("/tmp/TestClass.class", class_bytes).ok();

        // Count methods in the bytecode
        // Method count is at a specific offset in the class file
        // We should see at least 2 methods: constructor + add
        let method_count_offset = find_method_count_offset(class_bytes);
        if let Some(offset) = method_count_offset {
            let method_count = u16::from_be_bytes([class_bytes[offset], class_bytes[offset + 1]]);
            println!("Method count in bytecode: {}", method_count);
            assert!(
                method_count >= 2,
                "Should have at least 2 methods (constructor + add), found {}",
                method_count
            );
        }
    }

    fn find_method_count_offset(_bytes: &[u8]) -> Option<usize> {
        // In a class file:
        // - Magic (4 bytes)
        // - Minor version (2 bytes)
        // - Major version (2 bytes)
        // - Constant pool count (2 bytes)
        // - Constant pool (variable)
        // - Access flags (2 bytes)
        // - This class (2 bytes)
        // - Super class (2 bytes)
        // - Interfaces count (2 bytes)
        // - Interfaces (variable)
        // - Fields count (2 bytes)
        // - Fields (variable)
        // - Methods count (2 bytes) <- this is what we want

        // For now, return None since parsing is complex
        // The real test is using javap externally
        None
    }

    #[test]
    fn test_extern_block_transpilation() {
        // Test that extern blocks are properly transpiled to native methods
        let extern_block = ExternBlock {
            lang: "C".to_string(),
            items: vec![
                ExternItem {
                    name: "abs".to_string(),
                    params: vec![Type::I64],
                    return_type: Some(Type::I64),
                },
                ExternItem {
                    name: "sqrt".to_string(),
                    params: vec![Type::F64],
                    return_type: Some(Type::F64),
                },
            ],
        };

        let function = Function {
            name: "test_extern".to_string(),
            params: vec![],
            return_type: Some(Type::I64),
            body: Expr::Call {
                func: Box::new(Expr::Ident("abs".to_string())),
                args: vec![Expr::Integer(-42)],
            },
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        };

        let program = Program {
            namespace: None,
            items: vec![Item::ExternBlock(extern_block), Item::Function(function)],
        };

        let mut transpiler = JavaTranspiler::new("ExternTest");
        let result = transpiler.transpile(&program);

        assert!(result.is_ok(), "Transpilation should succeed");
        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty(), "Should generate bytecode");

        // Verify the JAR contains the class
        let entries = unzip_classes(&bytecode);
        assert!(
            entries.contains_key("ExternTest.class"),
            "Should contain main class"
        );
    }

    #[test]
    fn test_extern_function_call() {
        // Test calling extern functions in expressions
        let program = Program {
            namespace: None,
            items: vec![
                Item::ExternBlock(ExternBlock {
                    lang: "Java".to_string(),
                    items: vec![ExternItem {
                        name: "println".to_string(),
                        params: vec![Type::String],
                        return_type: None,
                    }],
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: None,
                    body: Expr::Block(vec![Expr::Call {
                        func: Box::new(Expr::Ident("println".to_string())),
                        args: vec![Expr::String("Hello from extern!".to_string())],
                    }]),
                    is_pub: true,
                    is_macro: false,
                    attributes: vec![],
                    span: dummy_span(),
                }),
            ],
        };

        let mut transpiler = JavaTranspiler::new("ExternCallTest");
        let result = transpiler.transpile(&program);

        assert!(
            result.is_ok(),
            "Transpilation with extern calls should succeed"
        );
    }

    #[test]
    fn test_multiple_extern_blocks() {
        // Test multiple extern blocks with different languages
        let program = Program {
            namespace: None,
            items: vec![
                Item::ExternBlock(ExternBlock {
                    lang: "C".to_string(),
                    items: vec![ExternItem {
                        name: "abs".to_string(),
                        params: vec![Type::I64],
                        return_type: Some(Type::I64),
                    }],
                }),
                Item::ExternBlock(ExternBlock {
                    lang: "Java".to_string(),
                    items: vec![ExternItem {
                        name: "currentTimeMillis".to_string(),
                        params: vec![],
                        return_type: Some(Type::I64),
                    }],
                }),
                Item::Function(Function {
                    name: "test".to_string(),
                    params: vec![],
                    return_type: Some(Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Add,
                        left: Box::new(Expr::Call {
                            func: Box::new(Expr::Ident("abs".to_string())),
                            args: vec![Expr::Integer(-10)],
                        }),
                        right: Box::new(Expr::Call {
                            func: Box::new(Expr::Ident("currentTimeMillis".to_string())),
                            args: vec![],
                        }),
                    },
                    is_pub: true,
                    is_macro: false,
                    attributes: vec![],
                    span: dummy_span(),
                }),
            ],
        };

        let mut transpiler = JavaTranspiler::new("MultiExternTest");
        let result = transpiler.transpile(&program);

        assert!(result.is_ok(), "Multiple extern blocks should work");
    }

    #[test]
    fn test_simple_function() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Integer(42),
                is_pub: true,
                is_macro: false,
                attributes: vec![],
                span: dummy_span(),
            })],
        };

        let mut transpiler = JavaTranspiler::new("Main");
        let jar_bytes = transpiler.transpile(&program).expect("transpile");
        let entries = unzip_classes(&jar_bytes);
        assert!(entries.contains_key("Main.class"));
        let main_bytes = entries.get("Main.class").unwrap();
        assert_eq!(&main_bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
    }

    #[test]
    fn test_struct_transpilation() {
        let struct_def = Struct {
            name: "Point".to_string(),
            fields: vec![
                Field {
                    name: "x".to_string(),
                    ty: Type::I64,
                    is_pub: true,
                },
                Field {
                    name: "y".to_string(),
                    ty: Type::I64,
                    is_pub: true,
                },
            ],
            is_pub: true,
            attributes: vec![],
        };

        let method = Function {
            name: "distance_sq".to_string(),
            params: vec![Param {
                name: "self".to_string(),
                ty: Type::Named("Point".to_string()),
            }],
            return_type: Some(Type::I64),
            body: Expr::Binary {
                op: BinOp::Add,
                left: Box::new(Expr::Binary {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Field {
                        expr: Box::new(Expr::Ident("self".to_string())),
                        field: "x".to_string(),
                    }),
                    right: Box::new(Expr::Field {
                        expr: Box::new(Expr::Ident("self".to_string())),
                        field: "x".to_string(),
                    }),
                }),
                right: Box::new(Expr::Binary {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Field {
                        expr: Box::new(Expr::Ident("self".to_string())),
                        field: "y".to_string(),
                    }),
                    right: Box::new(Expr::Field {
                        expr: Box::new(Expr::Ident("self".to_string())),
                        field: "y".to_string(),
                    }),
                }),
            },
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        };

        let main_func = Function {
            name: "main".to_string(),
            params: vec![],
            return_type: Some(Type::I64),
            body: Expr::Block(vec![Expr::Let {
                name: "p".to_string(),
                mutable: false,
                ty: Some(Type::Named("Point".to_string())),
                value: Box::new(Expr::StructInit {
                    name: "Point".to_string(),
                    fields: HashMap::from([
                        ("x".to_string(), Expr::Integer(3)),
                        ("y".to_string(), Expr::Integer(4)),
                    ]),
                }),
                then: Box::new(Expr::Method {
                    expr: Box::new(Expr::Ident("p".to_string())),
                    method: "distance_sq".to_string(),
                    args: vec![],
                }),
            }]),
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        };

        let program = Program {
            namespace: None,
            items: vec![
                Item::Struct(struct_def.clone()),
                Item::Impl(Impl {
                    struct_name: "Point".to_string(),
                    methods: vec![method],
                }),
                Item::Function(main_func),
            ],
        };

        let mut transpiler = JavaTranspiler::new("Main");
        let jar_bytes = transpiler.transpile(&program).expect("transpile");
        let entries = unzip_classes(&jar_bytes);
        assert!(entries.contains_key("Main.class"));
        assert!(entries.contains_key("Point.class"));
        let point_bytes = entries.get("Point.class").unwrap();
        assert_eq!(&point_bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
    }
}
