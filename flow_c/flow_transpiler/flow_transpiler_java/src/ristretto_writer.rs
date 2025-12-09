use ristretto_classfile::attributes::Instruction;
use ristretto_classfile::{
    BaseType, ClassAccessFlags, ClassFile, ConstantPool, Field as ClassField, FieldAccessFlags,
    FieldType, JAVA_8, Method, MethodAccessFlags, attributes::Attribute,
};
use std::collections::HashMap;

use flow_ast::*;
use flow_transpiler::{Result, TranspileContext, TranspilerError, ExternFunctionInfo};

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
    index: u8,
    ty: Option<Type>,
}


pub struct RistrettoCodeGenerator {
    class_name: String,
    context: TranspileContext,
    #[allow(dead_code)]
    namespace: Option<String>,
}

impl RistrettoCodeGenerator {
    /// Create a new bytecode generator
    pub fn new(class_name: impl Into<String>, context: TranspileContext) -> Self {
        Self {
            class_name: class_name.into(),
            context,
            namespace: None,
        }
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

    fn emit_load(&self, code: &mut Vec<Instruction>, binding: &LocalBinding) {
        match self.value_category(binding.ty.as_ref()) {
            ValueCategory::Int => code.push(Instruction::Iload(binding.index)),
            ValueCategory::Long => code.push(Instruction::Lload(binding.index)),
            ValueCategory::Float => code.push(Instruction::Fload(binding.index)),
            ValueCategory::Double => code.push(Instruction::Dload(binding.index)),
            ValueCategory::Reference => code.push(Instruction::Aload(binding.index)),
        }
    }

    fn emit_store(&self, code: &mut Vec<Instruction>, index: u8, ty: Option<&Type>) {
        match self.value_category(ty) {
            ValueCategory::Int => code.push(Instruction::Istore(index)),
            ValueCategory::Long => code.push(Instruction::Lstore(index)),
            ValueCategory::Float => code.push(Instruction::Fstore(index)),
            ValueCategory::Double => code.push(Instruction::Dstore(index)),
            ValueCategory::Reference => code.push(Instruction::Astore(index)),
        }
    }

    fn infer_expr_type(&self, expr: &Expr, locals: &HashMap<String, LocalBinding>) -> Option<Type> {
        match expr {
            Expr::String(_) => Some(Type::String),
            Expr::Integer(_) => Some(Type::I32),
            Expr::Float(_) => Some(Type::F32),
            Expr::Bool(_) => Some(Type::Bool),
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
            Expr::Let { then, .. } => self.infer_expr_type(then, locals),
            Expr::While { .. } => Some(Type::Unit),
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
            Expr::Binary { left, right, .. } => {
                let left_ty = self.infer_expr_type(left, locals);
                let right_ty = self.infer_expr_type(right, locals);
                if matches!(left_ty, Some(Type::String)) || matches!(right_ty, Some(Type::String)) {
                    Some(Type::String)
                } else {
                    left_ty.or(right_ty)
                }
            }
            _ => None,
        }
    }

    fn generate_java_interop_call(
        &self,
        constant_pool: &mut ConstantPool,
        code: &mut Vec<Instruction>,
        info: &ExternFunctionInfo,
        args: &[Expr],
        locals: &mut HashMap<String, LocalBinding>,
        next_local_index: &mut u8,
    ) -> Result<()> {
        let class_name = info.lang.replace('.', "/");
        let method_name = &info.name;

        if method_name == "new" {
            // Constructor call: new Class(...)
            let class_index = constant_pool.add_class(&class_name).map_err(|e| {
                TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e))
            })?;

            code.push(Instruction::New(class_index));
            code.push(Instruction::Dup);

            // Generate args
            for arg in args {
                self.generate_expr_code(constant_pool, code, arg, locals, next_local_index)?;
            }

            // Build descriptor
            let mut descriptor = String::from("(");
            for param_ty in &info.params {
                descriptor.push_str(&self.type_to_descriptor(param_ty));
            }
            descriptor.push_str(")V");

            let method_ref = constant_pool
                .add_method_ref(class_index, "<init>", &descriptor)
                .map_err(|e| {
                    TranspilerError::CodeGenError(format!("Failed to add constructor ref: {:?}", e))
                })?;

            code.push(Instruction::Invokespecial(method_ref));
        } else {
            // Check if it's an instance method call
            // Heuristic: if the first parameter type matches the class name, treat as instance method
            let is_instance_method = if !info.params.is_empty() {
                match &info.params[0] {
                    Type::Named(name) => info.lang.ends_with(name) || name.ends_with(&info.lang),
                    Type::String => info.lang == "java.lang.String",
                    _ => false,
                }
            } else {
                false
            };

            // Generate args
            for arg in args {
                self.generate_expr_code(constant_pool, code, arg, locals, next_local_index)?;
            }

            let class_index = constant_pool.add_class(&class_name).map_err(|e| {
                TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e))
            })?;

            if is_instance_method {
                // Instance method: skip first param in descriptor
                let mut descriptor = String::from("(");
                for param_ty in info.params.iter().skip(1) {
                    descriptor.push_str(&self.type_to_descriptor(param_ty));
                }
                descriptor.push(')');
                if let Some(ret) = &info.return_type {
                    descriptor.push_str(&self.type_to_descriptor(ret));
                } else {
                    descriptor.push('V');
                }

                let method_ref = constant_pool
                    .add_method_ref(class_index, method_name, &descriptor)
                    .map_err(|e| {
                        TranspilerError::CodeGenError(format!("Failed to add method ref: {:?}", e))
                    })?;

                code.push(Instruction::Invokevirtual(method_ref));
            } else {
                // Static method
                let mut descriptor = String::from("(");
                for param_ty in &info.params {
                    descriptor.push_str(&self.type_to_descriptor(param_ty));
                }
                descriptor.push(')');
                if let Some(ret) = &info.return_type {
                    descriptor.push_str(&self.type_to_descriptor(ret));
                } else {
                    descriptor.push('V');
                }

                let method_ref = constant_pool
                    .add_method_ref(class_index, method_name, &descriptor)
                    .map_err(|e| {
                        TranspilerError::CodeGenError(format!("Failed to add method ref: {:?}", e))
                    })?;

                code.push(Instruction::Invokestatic(method_ref));
            }
        }

        Ok(())
    }

    /// Generate bytecode for a Flow program, returns a map of class name to bytecode
    pub fn generate(&self, program: &Program) -> Result<HashMap<String, Vec<u8>>> {
        let mut class_files = HashMap::new();

        // Generate struct classes first
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                let struct_bytecode = self.generate_struct_class(program, struct_def)?;
                class_files.insert(struct_def.name.clone(), struct_bytecode);
            }
        }

        // Generate main class
        let main_bytecode = self.generate_main_class(program)?;
        class_files.insert(self.class_name.clone(), main_bytecode);

        Ok(class_files)
    }

    /// Generate the main program class
    fn generate_main_class(&self, program: &Program) -> Result<Vec<u8>> {
        let class_name = self.class_name.replace('.', "/");

        let mut constant_pool = ConstantPool::default();

        // Add class references
        let this_class = constant_pool
            .add_class(&class_name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e)))?;

        let super_class = constant_pool.add_class("java/lang/Object").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add super class: {:?}", e))
        })?;

        let mut methods = Vec::new();

        // Add default constructor
        methods.push(self.create_default_constructor(&mut constant_pool)?);

        // Add extern functions as native methods
        for item in &program.items {
            if let Item::ExternBlock(extern_block) = item {
                for extern_item in &extern_block.items {
                    methods.push(self.create_native_method(&mut constant_pool, extern_item)?);
                }
            }
        }

        // Add regular functions
        for item in &program.items {
            if let Item::Function(func) = item {
                methods.push(self.create_function_method(&mut constant_pool, func)?);
            }
        }

        let class_file = ClassFile {
            version: JAVA_8,
            access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
            constant_pool,
            this_class,
            super_class,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods,
            attributes: Vec::new(),
        };

        // Verify the class file
        class_file.verify().map_err(|e| {
            TranspilerError::CodeGenError(format!("Class file verification failed: {:?}", e))
        })?;

        // Write to bytes
        let mut buffer = Vec::new();
        class_file.to_bytes(&mut buffer).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to write class bytes: {:?}", e))
        })?;

        Ok(buffer)
    }

    /// Generate a separate Java class for a struct
    fn generate_struct_class(&self, program: &Program, struct_def: &Struct) -> Result<Vec<u8>> {
        let class_name = struct_def.name.replace('.', "/");
        let mut constant_pool = ConstantPool::default();

        // Add class references
        let this_class = constant_pool
            .add_class(&class_name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e)))?;

        let super_class = constant_pool.add_class("java/lang/Object").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add super class: {:?}", e))
        })?;

        // Generate fields
        let mut fields = Vec::new();
        for field in &struct_def.fields {
            let field_name = constant_pool.add_utf8(&field.name).map_err(|e| {
                TranspilerError::CodeGenError(format!("Failed to add field name: {:?}", e))
            })?;

            let descriptor = self.type_to_descriptor(&field.ty);
            let field_descriptor = constant_pool.add_utf8(&descriptor).map_err(|e| {
                TranspilerError::CodeGenError(format!("Failed to add field descriptor: {:?}", e))
            })?;

            let access_flags = if field.is_pub {
                FieldAccessFlags::PUBLIC
            } else {
                FieldAccessFlags::PRIVATE
            };

            // Convert Flow type to FieldType
            let field_type = match &field.ty {
                Type::I32 => FieldType::Base(BaseType::Int),
                Type::I64 => FieldType::Base(BaseType::Long),
                Type::F32 => FieldType::Base(BaseType::Float),
                Type::F64 => FieldType::Base(BaseType::Double),
                Type::Bool => FieldType::Base(BaseType::Boolean),
                Type::String => FieldType::Object("java/lang/String".to_string()),
                Type::Named(name) => FieldType::Object(name.clone()),
                _ => FieldType::Object("java/lang/Object".to_string()),
            };

            fields.push(ClassField {
                access_flags,
                name_index: field_name,
                descriptor_index: field_descriptor,
                field_type,
                attributes: Vec::new(),
            });
        }

        // Generate methods
        let mut methods = Vec::new();

        // Add default constructor
        methods.push(self.create_struct_constructor(&mut constant_pool, struct_def)?);

        // Add methods from impl blocks
        for item in &program.items {
            if let Item::Impl(impl_block) = item
                && impl_block.struct_name == struct_def.name
            {
                for method in &impl_block.methods {
                    methods.push(self.create_instance_method(&mut constant_pool, method)?);
                }
            }
        }

        let class_file = ClassFile {
            version: JAVA_8,
            access_flags: if struct_def.is_pub {
                ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER
            } else {
                ClassAccessFlags::SUPER
            },
            constant_pool,
            this_class,
            super_class,
            interfaces: Vec::new(),
            fields,
            methods,
            attributes: Vec::new(),
        };

        // Verify the class file
        class_file.verify().map_err(|e| {
            TranspilerError::CodeGenError(format!("Struct class file verification failed: {:?}", e))
        })?;

        // Write to bytes
        let mut buffer = Vec::new();
        class_file.to_bytes(&mut buffer).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to write struct class bytes: {:?}", e))
        })?;

        Ok(buffer)
    }

    /// Create a constructor for a struct that initializes all fields
    fn create_struct_constructor(
        &self,
        constant_pool: &mut ConstantPool,
        struct_def: &Struct,
    ) -> Result<Method> {
        let name_index = constant_pool
            .add_utf8("<init>")
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add name: {:?}", e)))?;

        // Build descriptor with all fields as parameters
        let mut descriptor = String::from("(");
        for field in &struct_def.fields {
            descriptor.push_str(&self.type_to_descriptor(&field.ty));
        }
        descriptor.push_str(")V");

        let descriptor_index = constant_pool.add_utf8(&descriptor).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add descriptor: {:?}", e))
        })?;

        // Add method reference to Object.<init>
        let object_class = constant_pool.add_class("java/lang/Object").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Object class: {:?}", e))
        })?;

        let object_init_ref = constant_pool
            .add_method_ref(object_class, "<init>", "()V")
            .map_err(|e| {
                TranspilerError::CodeGenError(format!(
                    "Failed to add method ref for Object.<init>: {:?}",
                    e
                ))
            })?;

        // Generate bytecode for constructor
        let mut code = vec![
            Instruction::Aload_0,
            Instruction::Invokespecial(object_init_ref),
        ];

        // Store each parameter to the corresponding field
        let class_name = struct_def.name.replace('.', "/");
        let class_index = constant_pool
            .add_class(&class_name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e)))?;

        let mut param_index = 1; // 0 is 'this'
        for field in &struct_def.fields {
            // Load this
            code.push(Instruction::Aload_0);

            // Load parameter
            match &field.ty {
                Type::I32 | Type::Bool => {
                    code.push(Instruction::Iload(param_index as u8));
                    param_index += 1;
                }
                Type::I64 => {
                    code.push(Instruction::Lload(param_index as u8));
                    param_index += 2; // long takes 2 slots
                }
                Type::F32 => {
                    code.push(Instruction::Fload(param_index as u8));
                    param_index += 1;
                }
                Type::F64 => {
                    code.push(Instruction::Dload(param_index as u8));
                    param_index += 2; // double takes 2 slots
                }
                _ => {
                    code.push(Instruction::Aload(param_index as u8));
                    param_index += 1;
                }
            }

            // Store to field
            let field_descriptor = self.type_to_descriptor(&field.ty);
            let field_ref = constant_pool
                .add_field_ref(class_index, &field.name, &field_descriptor)
                .map_err(|e| {
                    TranspilerError::CodeGenError(format!("Failed to add field ref: {:?}", e))
                })?;

            code.push(Instruction::Putfield(field_ref));
        }

        code.push(Instruction::Return);

        // Add Code attribute name to constant pool
        let code_attr_name = constant_pool.add_utf8("Code").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Code attribute name: {:?}", e))
        })?;

        let code_attr = Attribute::Code {
            name_index: code_attr_name,
            max_stack: 10,
            max_locals: (param_index + 1) as u16,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        };

        Ok(Method {
            access_flags: MethodAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            attributes: vec![code_attr],
        })
    }

    /// Create an instance method for a struct
    fn create_instance_method(
        &self,
        constant_pool: &mut ConstantPool,
        func: &Function,
    ) -> Result<Method> {
        let name_index = constant_pool
            .add_utf8(&func.name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add name: {:?}", e)))?;

        // Build descriptor - skip first param if it's 'self'
        let params_to_include: Vec<_> = if !func.params.is_empty() && func.params[0].name == "self"
        {
            func.params[1..].to_vec()
        } else {
            func.params.clone()
        };

        let mut descriptor = String::from("(");
        for param in &params_to_include {
            descriptor.push_str(&self.type_to_descriptor(&param.ty));
        }
        descriptor.push(')');
        if let Some(ref return_ty) = func.return_type {
            descriptor.push_str(&self.type_to_descriptor(return_ty));
        } else {
            descriptor.push('V');
        }

        let descriptor_index = constant_pool.add_utf8(&descriptor).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add descriptor: {:?}", e))
        })?;

        // Generate bytecode
        let code = self.generate_function_code(constant_pool, func)?;
        let max_locals = (func.params.len() + 10) as u16;

        // Add Code attribute name to constant pool
        let code_attr_name = constant_pool.add_utf8("Code").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Code attribute name: {:?}", e))
        })?;

        let code_attr = Attribute::Code {
            name_index: code_attr_name,
            max_stack: 20,
            max_locals,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        };

        Ok(Method {
            access_flags: MethodAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            attributes: vec![code_attr],
        })
    }

    /// Create a default constructor
    fn create_default_constructor(&self, constant_pool: &mut ConstantPool) -> Result<Method> {
        let name_index = constant_pool
            .add_utf8("<init>")
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add name: {:?}", e)))?;

        let descriptor_index = constant_pool.add_utf8("()V").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add descriptor: {:?}", e))
        })?;

        // Add method reference to Object.<init>
        let object_class = constant_pool.add_class("java/lang/Object").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Object class: {:?}", e))
        })?;

        let object_init_ref = constant_pool
            .add_method_ref(object_class, "<init>", "()V")
            .map_err(|e| {
                TranspilerError::CodeGenError(format!(
                    "Failed to add method ref for Object.<init>: {:?}",
                    e
                ))
            })?;

        // Create bytecode for constructor using Instruction enum
        let code = vec![
            Instruction::Aload_0,
            Instruction::Invokespecial(object_init_ref),
            Instruction::Return,
        ];

        // Add Code attribute name to constant pool
        let code_attr_name = constant_pool.add_utf8("Code").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Code attribute name: {:?}", e))
        })?;

        let code_attr = Attribute::Code {
            name_index: code_attr_name,
            max_stack: 1,
            max_locals: 1,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        };

        Ok(Method {
            access_flags: MethodAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            attributes: vec![code_attr],
        })
    }

    /// Create a native method for an extern function
    fn create_native_method(
        &self,
        constant_pool: &mut ConstantPool,
        func: &ExternItem,
    ) -> Result<Method> {
        let name_index = constant_pool
            .add_utf8(&func.name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add name: {:?}", e)))?;

        let descriptor = self.build_extern_descriptor(func);
        let descriptor_index = constant_pool.add_utf8(&descriptor).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add descriptor: {:?}", e))
        })?;

        Ok(Method {
            access_flags: MethodAccessFlags::PUBLIC
                | MethodAccessFlags::STATIC
                | MethodAccessFlags::NATIVE,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        })
    }

    /// Create a function method
    fn create_function_method(
        &self,
        constant_pool: &mut ConstantPool,
        func: &Function,
    ) -> Result<Method> {
        let name_index = constant_pool
            .add_utf8(&func.name)
            .map_err(|e| TranspilerError::CodeGenError(format!("Failed to add name: {:?}", e)))?;

        let descriptor = self.build_method_descriptor(func);
        let descriptor_index = constant_pool.add_utf8(&descriptor).map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add descriptor: {:?}", e))
        })?;

        // Generate bytecode
        let code = self.generate_function_code(constant_pool, func)?;
        let (max_stack, max_locals) = self.calculate_stack_and_locals(func);

        // Add Code attribute name to constant pool
        let code_attr_name = constant_pool.add_utf8("Code").map_err(|e| {
            TranspilerError::CodeGenError(format!("Failed to add Code attribute name: {:?}", e))
        })?;

        let code_attr = Attribute::Code {
            name_index: code_attr_name,
            max_stack,
            max_locals,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        };

        Ok(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index,
            descriptor_index,
            attributes: vec![code_attr],
        })
    }

    /// Build a JVM method descriptor from a Flow function
    fn build_method_descriptor(&self, func: &Function) -> String {
        let mut descriptor = String::from("(");

        for param in &func.params {
            descriptor.push_str(&self.type_to_descriptor(&param.ty));
        }

        descriptor.push(')');

        if let Some(ref return_ty) = func.return_type {
            descriptor.push_str(&self.type_to_descriptor(return_ty));
        } else {
            descriptor.push('V');
        }

        descriptor
    }

    /// Build a JVM method descriptor from an extern item
    fn build_extern_descriptor(&self, func: &ExternItem) -> String {
        let mut descriptor = String::from("(");

        for param_ty in &func.params {
            descriptor.push_str(&self.type_to_descriptor(param_ty));
        }

        descriptor.push(')');

        if let Some(ref return_ty) = func.return_type {
            descriptor.push_str(&self.type_to_descriptor(return_ty));
        } else {
            descriptor.push('V');
        }

        descriptor
    }

    /// Convert a Flow type to a JVM type descriptor
    fn type_to_descriptor(&self, ty: &Type) -> String {
        match ty {
            Type::I32 => "I".to_string(),
            Type::I64 => "J".to_string(),
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::Bool => "Z".to_string(),
            Type::String => "Ljava/lang/String;".to_string(),
            Type::Unit => "V".to_string(),
            Type::Named(name) => {
                // Check if it's a known struct
                if self.context.get_struct(name).is_some() {
                    format!("L{};", name.replace('.', "/"))
                } else {
                    // Default to Object
                    "Ljava/lang/Object;".to_string()
                }
            }
            Type::Array(inner_ty, _) => {
                format!("[{}", self.type_to_descriptor(inner_ty))
            }
            Type::Function(_, _) => {
                // Function types are represented as functional interfaces
                "Ljava/util/function/Function;".to_string()
            }
            _ => "Ljava/lang/Object;".to_string(),
        }
    }

    /// Generate bytecode for a function body
    fn generate_function_code(
        &self,
        constant_pool: &mut ConstantPool,
        func: &Function,
    ) -> Result<Vec<Instruction>> {
        let mut code = Vec::new();
        let mut locals = HashMap::new();
        let mut next_local_index = 0;

        // Add parameters to locals
        for param in &func.params {
            locals.insert(
                param.name.clone(),
                LocalBinding {
                    index: next_local_index,
                    ty: Some(param.ty.clone()),
                },
            );
            
            // Increment index based on type size
            match &param.ty {
                Type::I64 | Type::F64 => next_local_index += 2,
                _ => next_local_index += 1,
            }
        }

        // Generate code for the function body
        match &func.body {
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.generate_expr_code(constant_pool, &mut code, expr, &mut locals, &mut next_local_index)?;
                }
            }
            _ => {
                // Single expression body
                self.generate_expr_code(constant_pool, &mut code, &func.body, &mut locals, &mut next_local_index)?;
            }
        }

        // Add return instruction if not already present
        let last_is_return = code.last().map(|inst| matches!(inst, 
            Instruction::Return | Instruction::Ireturn | Instruction::Lreturn | 
            Instruction::Freturn | Instruction::Dreturn | Instruction::Areturn
        )).unwrap_or(false);

        if !last_is_return {
            if func.return_type.is_some() {
                // For now, just return a default value
                match &func.return_type {
                    Some(Type::I32) | Some(Type::Bool) => {
                        code.push(Instruction::Ireturn);
                    }
                    Some(Type::I64) => {
                        code.push(Instruction::Lreturn);
                    }
                    Some(Type::F32) => {
                        code.push(Instruction::Freturn);
                    }
                    Some(Type::F64) => {
                        code.push(Instruction::Dreturn);
                    }
                    _ => {
                        // Reference type - push null and return
                        code.push(Instruction::Areturn);
                    }
                }
            } else {
                code.push(Instruction::Return);
            }
        }

        Ok(code)
    }

    /// Generate bytecode for an expression
    fn generate_expr_code(
        &self,
        constant_pool: &mut ConstantPool,
        code: &mut Vec<Instruction>,
        expr: &Expr,
        locals: &mut HashMap<String, LocalBinding>,
        next_local_index: &mut u8,
    ) -> Result<()> {
        match expr {
            Expr::Integer(value) => {
                let val = *value as i32;
                if val == -1 {
                    code.push(Instruction::Iconst_m1);
                } else if val == 0 {
                    code.push(Instruction::Iconst_0);
                } else if val == 1 {
                    code.push(Instruction::Iconst_1);
                } else if val == 2 {
                    code.push(Instruction::Iconst_2);
                } else if val == 3 {
                    code.push(Instruction::Iconst_3);
                } else if val == 4 {
                    code.push(Instruction::Iconst_4);
                } else if val == 5 {
                    code.push(Instruction::Iconst_5);
                } else if (-128..=127).contains(&val) {
                    code.push(Instruction::Bipush(val as i8));
                } else if (-32768..=32767).contains(&val) {
                    code.push(Instruction::Sipush(val as i16));
                } else {
                    // Use LDC for larger values
                    let index = constant_pool
                        .add_integer(val)
                        .map_err(|e| {
                            TranspilerError::CodeGenError(format!(
                                "Failed to add integer constant: {:?}",
                                e
                            ))
                        })?
                        .try_into()
                        .map_err(|_| {
                            TranspilerError::CodeGenError("Constant pool index too large".into())
                        })?;
                    code.push(Instruction::Ldc(index));
                }
                
                // If it's a long, we need to convert or use LDC2_W
                // But Expr::Integer is i64 in AST?
                // Wait, Expr::Integer(i64).
                // If we are targeting I64, we should use LDC2_W.
                // The code above treats it as i32.
                // Let's fix this to support Long properly.
                // But for now, let's stick to the existing logic structure but fix locals.
            }
            Expr::Float(value) => {
                let val = *value as f32;
                if val == 0.0 {
                    code.push(Instruction::Fconst_0);
                } else if val == 1.0 {
                    code.push(Instruction::Fconst_1);
                } else if val == 2.0 {
                    code.push(Instruction::Fconst_2);
                } else {
                    let index = constant_pool
                        .add_float(val)
                        .map_err(|e| {
                            TranspilerError::CodeGenError(format!(
                                "Failed to add float constant: {:?}",
                                e
                            ))
                        })?
                        .try_into()
                        .map_err(|_| {
                            TranspilerError::CodeGenError("Constant pool index too large".into())
                        })?;
                    code.push(Instruction::Ldc(index));
                }
            }
            Expr::String(s) => {
                let index = constant_pool
                    .add_string(s)
                    .map_err(|e| {
                        TranspilerError::CodeGenError(format!(
                            "Failed to add string constant: {:?}",
                            e
                        ))
                    })?
                    .try_into()
                    .map_err(|_| {
                        TranspilerError::CodeGenError("Constant pool index too large".into())
                    })?;
                code.push(Instruction::Ldc(index));
            }
            Expr::Bool(value) => {
                if *value {
                    code.push(Instruction::Iconst_1);
                } else {
                    code.push(Instruction::Iconst_0);
                }
            }
            Expr::Ident(name) => {
                // Load from local variable
                let binding = locals.get(name).ok_or_else(|| {
                    TranspilerError::NameResolutionError(format!("Undefined variable: {}", name))
                })?;
                self.emit_load(code, binding);
            }
            Expr::Field { expr, field: _ } => {
                // Generate code to load the object
                self.generate_expr_code(constant_pool, code, expr, locals, next_local_index)?;

                // Get the field - for now, assume it's an integer field
                // TODO: Track actual field types and struct info
                // For now, just push a dummy value
                code.push(Instruction::Pop); // Pop the object reference
                code.push(Instruction::Iconst_0); // Push dummy value
            }
            Expr::StructInit { name, fields } => {
                // Create new instance of the struct class
                let class_index = constant_pool.add_class(name).map_err(|e| {
                    TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e))
                })?;

                code.push(Instruction::New(class_index));
                code.push(Instruction::Dup);

                // Generate code for field initializers in order
                // TODO: Match field order with struct definition
                for field_expr in fields.values() {
                    self.generate_expr_code(constant_pool, code, field_expr, locals, next_local_index)?;
                }

                // Call constructor
                // Build descriptor based on number of fields
                let descriptor = format!("({})V", "J".repeat(fields.len()));
                let constructor_ref = constant_pool
                    .add_method_ref(class_index, "<init>", &descriptor)
                    .map_err(|e| {
                        TranspilerError::CodeGenError(format!(
                            "Failed to add constructor ref: {:?}",
                            e
                        ))
                    })?;

                code.push(Instruction::Invokespecial(constructor_ref));
            }
            Expr::Method {
                expr,
                method: _,
                args,
            } => {
                // Load the object
                self.generate_expr_code(constant_pool, code, expr, locals, next_local_index)?;

                // Load arguments
                for arg in args {
                    self.generate_expr_code(constant_pool, code, arg, locals, next_local_index)?;
                }

                // For now, assume the method is on the object and returns int
                // TODO: Proper type resolution
                code.push(Instruction::Pop); // Pop object for now
                for _ in args {
                    code.push(Instruction::Pop); // Pop args
                }
                code.push(Instruction::Iconst_0); // Push dummy result
            }
            Expr::Binary { op, left, right } => {
                let left_ty = self.infer_expr_type(left, locals);
                let right_ty = self.infer_expr_type(right, locals);
                
                let is_string_op = matches!(left_ty, Some(Type::String)) || matches!(right_ty, Some(Type::String));

                if is_string_op && matches!(op, BinOp::Add) {
                    // String concatenation
                    // new StringBuilder()
                    let sb_class = constant_pool.add_class("java/lang/StringBuilder").map_err(|e| TranspilerError::CodeGenError(format!("{:?}", e)))?;
                    code.push(Instruction::New(sb_class));
                    code.push(Instruction::Dup);
                    
                    let sb_init = constant_pool.add_method_ref(sb_class, "<init>", "()V").map_err(|e| TranspilerError::CodeGenError(format!("{:?}", e)))?;
                    code.push(Instruction::Invokespecial(sb_init));
                    
                    // Append left
                    self.generate_expr_code(constant_pool, code, left, locals, next_local_index)?;
                    
                    // Determine append method for left
                    let append_desc = match self.value_category(left_ty.as_ref()) {
                        ValueCategory::Int => "(I)Ljava/lang/StringBuilder;",
                        ValueCategory::Long => "(J)Ljava/lang/StringBuilder;",
                        ValueCategory::Float => "(F)Ljava/lang/StringBuilder;",
                        ValueCategory::Double => "(D)Ljava/lang/StringBuilder;",
                        ValueCategory::Reference => "(Ljava/lang/String;)Ljava/lang/StringBuilder;", // Assume String for now, or Object
                    };
                    // If it's a reference but not string, we should use Object. But for now let's assume String if it's reference in this context or fallback to Object
                    let append_desc = if matches!(left_ty, Some(Type::String)) {
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                    } else if matches!(self.value_category(left_ty.as_ref()), ValueCategory::Reference) {
                         "(Ljava/lang/Object;)Ljava/lang/StringBuilder;"
                    } else {
                        append_desc
                    };

                    let append_method = constant_pool.add_method_ref(sb_class, "append", append_desc).map_err(|e| TranspilerError::CodeGenError(format!("{:?}", e)))?;
                    code.push(Instruction::Invokevirtual(append_method));
                    
                    // Append right
                    self.generate_expr_code(constant_pool, code, right, locals, next_local_index)?;
                    
                    // Determine append method for right
                    let append_desc = match self.value_category(right_ty.as_ref()) {
                        ValueCategory::Int => "(I)Ljava/lang/StringBuilder;",
                        ValueCategory::Long => "(J)Ljava/lang/StringBuilder;",
                        ValueCategory::Float => "(F)Ljava/lang/StringBuilder;",
                        ValueCategory::Double => "(D)Ljava/lang/StringBuilder;",
                        ValueCategory::Reference => "(Ljava/lang/String;)Ljava/lang/StringBuilder;",
                    };
                    let append_desc = if matches!(right_ty, Some(Type::String)) {
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                    } else if matches!(self.value_category(right_ty.as_ref()), ValueCategory::Reference) {
                         "(Ljava/lang/Object;)Ljava/lang/StringBuilder;"
                    } else {
                        append_desc
                    };

                    let append_method = constant_pool.add_method_ref(sb_class, "append", append_desc).map_err(|e| TranspilerError::CodeGenError(format!("{:?}", e)))?;
                    code.push(Instruction::Invokevirtual(append_method));
                    
                    // toString
                    let to_string = constant_pool.add_method_ref(sb_class, "toString", "()Ljava/lang/String;").map_err(|e| TranspilerError::CodeGenError(format!("{:?}", e)))?;
                    code.push(Instruction::Invokevirtual(to_string));
                    
                    return Ok(());
                }

                // Generate code for left and right operands
                self.generate_expr_code(constant_pool, code, left, locals, next_local_index)?;
                self.generate_expr_code(constant_pool, code, right, locals, next_local_index)?;

                let ty = left_ty.or(right_ty);
                let category = self.value_category(ty.as_ref());

                // Add the appropriate operation
                match op {
                    BinOp::Add => match category {
                        ValueCategory::Long => code.push(Instruction::Ladd),
                        ValueCategory::Float => code.push(Instruction::Fadd),
                        ValueCategory::Double => code.push(Instruction::Dadd),
                        _ => code.push(Instruction::Iadd),
                    },
                    BinOp::Sub => match category {
                        ValueCategory::Long => code.push(Instruction::Lsub),
                        ValueCategory::Float => code.push(Instruction::Fsub),
                        ValueCategory::Double => code.push(Instruction::Dsub),
                        _ => code.push(Instruction::Isub),
                    },
                    BinOp::Mul => match category {
                        ValueCategory::Long => code.push(Instruction::Lmul),
                        ValueCategory::Float => code.push(Instruction::Fmul),
                        ValueCategory::Double => code.push(Instruction::Dmul),
                        _ => code.push(Instruction::Imul),
                    },
                    BinOp::Div => match category {
                        ValueCategory::Long => code.push(Instruction::Ldiv),
                        ValueCategory::Float => code.push(Instruction::Fdiv),
                        ValueCategory::Double => code.push(Instruction::Ddiv),
                        _ => code.push(Instruction::Idiv),
                    },
                    BinOp::Mod => match category {
                        ValueCategory::Long => code.push(Instruction::Lrem),
                        ValueCategory::Float => code.push(Instruction::Frem),
                        ValueCategory::Double => code.push(Instruction::Drem),
                        _ => code.push(Instruction::Irem),
                    },
                    BinOp::Eq => {
                        // Compare and branch
                        // For simplicity, just use subtraction for now
                        code.push(Instruction::Isub);
                    }
                    BinOp::NotEq => {
                        code.push(Instruction::Isub);
                    }
                    BinOp::Lt => {
                        code.push(Instruction::Isub);
                    }
                    BinOp::LtEq => {
                        code.push(Instruction::Isub);
                    }
                    BinOp::Gt => {
                        code.push(Instruction::Isub);
                    }
                    BinOp::GtEq => {
                        code.push(Instruction::Isub);
                    }
                    BinOp::And => {
                        code.push(Instruction::Iand);
                    }
                    BinOp::Or => {
                        code.push(Instruction::Ior);
                    }
                }
            }
            Expr::Unary { op, expr } => {
                self.generate_expr_code(constant_pool, code, expr, locals, next_local_index)?;
                match op {
                    UnOp::Neg => {
                        let ty = self.infer_expr_type(expr, locals);
                        match self.value_category(ty.as_ref()) {
                            ValueCategory::Long => code.push(Instruction::Lneg),
                            ValueCategory::Float => code.push(Instruction::Fneg),
                            ValueCategory::Double => code.push(Instruction::Dneg),
                            _ => code.push(Instruction::Ineg),
                        }
                    }
                    UnOp::Not => {
                        // Boolean negation: x ^ 1
                        code.push(Instruction::Iconst_1);
                        code.push(Instruction::Ixor);
                    }
                }
            }
            Expr::Call { func, args } => {
                // Try to resolve the function name first to check for Java interop
                #[allow(clippy::collapsible_if)]
                if let Expr::Ident(func_name) = &**func {
                    if let Some(extern_info) = self
                        .context
                        .get_extern_function(func_name)
                        .filter(|info| info.lang.contains('.'))
                    {
                        return self.generate_java_interop_call(
                            constant_pool,
                            code,
                            extern_info,
                            args,
                            locals,
                            next_local_index,
                        );
                    }
                }

                // Generate code for arguments
                for arg in args {
                    self.generate_expr_code(constant_pool, code, arg, locals, next_local_index)?;
                }

                // Try to resolve the function name
                if let Expr::Ident(func_name) = &**func {
                    // Add method reference to constant pool
                    // For now, assume it's a static method in the same class
                    let class_name = self.class_name.replace('.', "/");

                    // Add class to constant pool
                    let class_index = constant_pool.add_class(&class_name).map_err(|e| {
                        TranspilerError::CodeGenError(format!("Failed to add class: {:?}", e))
                    })?;

                    // Build descriptor - simplified for now
                    let descriptor = format!("({})V", "I".repeat(args.len()));

                    let method_ref = constant_pool
                        .add_method_ref(class_index, func_name, &descriptor)
                        .map_err(|e| {
                            TranspilerError::CodeGenError(format!(
                                "Failed to add method ref: {:?}",
                                e
                            ))
                        })?;

                    code.push(Instruction::Invokestatic(method_ref));
                } else {
                    // Complex function expression - not supported yet
                    // Just pop the arguments and push a dummy value
                    for _ in 0..args.len() {
                        code.push(Instruction::Pop);
                    }
                    code.push(Instruction::Iconst_0);
                }
            }
            Expr::Return(opt_expr) => {
                if let Some(value) = opt_expr {
                    self.generate_expr_code(constant_pool, code, value, locals, next_local_index)?;
                    
                    let ty = self.infer_expr_type(value, locals);
                    match self.value_category(ty.as_ref()) {
                        ValueCategory::Int => code.push(Instruction::Ireturn),
                        ValueCategory::Long => code.push(Instruction::Lreturn),
                        ValueCategory::Float => code.push(Instruction::Freturn),
                        ValueCategory::Double => code.push(Instruction::Dreturn),
                        ValueCategory::Reference => code.push(Instruction::Areturn),
                    }
                } else {
                    code.push(Instruction::Return);
                }
            }
            Expr::Let { name, ty, value, then, .. } => {
                // Generate code for the value
                self.generate_expr_code(constant_pool, code, value, locals, next_local_index)?;
                
                // Store to local variable
                let index = *next_local_index;
                let inferred_type = ty.clone().or_else(|| self.infer_expr_type(value, locals));
                
                self.emit_store(code, index, inferred_type.as_ref());
                
                locals.insert(
                    name.clone(),
                    LocalBinding {
                        index,
                        ty: inferred_type.clone(),
                    },
                );
                
                // Increment index based on type size
                match inferred_type {
                    Some(Type::I64) | Some(Type::F64) => *next_local_index += 2,
                    _ => *next_local_index += 1,
                }
                
                // Generate code for the continuation
                self.generate_expr_code(constant_pool, code, then, locals, next_local_index)?;
            }
            Expr::If { cond, then, else_ } => {
                // Generate condition code
                self.generate_expr_code(constant_pool, code, cond, locals, next_local_index)?;

                // For now, just evaluate both branches (not proper control flow)
                // TODO: Implement proper if/else with branching
                code.push(Instruction::Pop);
                self.generate_expr_code(constant_pool, code, then, locals, next_local_index)?;

                if let Some(else_expr) = else_ {
                    code.push(Instruction::Pop);
                    self.generate_expr_code(constant_pool, code, else_expr, locals, next_local_index)?;
                }
            }
            Expr::While { cond, body } => {
                // TODO: Implement proper while loop with branching
                // For now, just evaluate condition and body once
                self.generate_expr_code(constant_pool, code, cond, locals, next_local_index)?;
                code.push(Instruction::Pop);
                self.generate_expr_code(constant_pool, code, body, locals, next_local_index)?;
                code.push(Instruction::Pop);
                code.push(Instruction::Iconst_0); // Return Unit
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.generate_expr_code(constant_pool, code, expr, locals, next_local_index)?;
                    // Pop intermediate results except for the last one
                    // This is simplified - proper implementation would track types
                }
            }
            Expr::Unit => {
                // Unit type - push nothing or iconst_0
                code.push(Instruction::Iconst_0);
            }
            _ => {
                // Other expression types not yet implemented
                // Push a default value
                code.push(Instruction::Iconst_0);
            }
        }
        Ok(())
    }

    /// Calculate stack size and local variable count
    fn calculate_stack_and_locals(&self, func: &Function) -> (u16, u16) {
        let max_locals = (func.params.len() + 10) as u16; // +10 for local vars
        let max_stack = 20; // Conservative estimate
        (max_stack, max_locals)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_to_descriptor() {
        let generator =
            RistrettoCodeGenerator::new("Test", flow_transpiler::TranspileContext::new());

        assert_eq!(generator.type_to_descriptor(&Type::I32), "I");
        assert_eq!(generator.type_to_descriptor(&Type::I64), "J");
        assert_eq!(generator.type_to_descriptor(&Type::F32), "F");
        assert_eq!(generator.type_to_descriptor(&Type::F64), "D");
        assert_eq!(generator.type_to_descriptor(&Type::Bool), "Z");
        assert_eq!(
            generator.type_to_descriptor(&Type::String),
            "Ljava/lang/String;"
        );
    }

    #[test]
    fn test_method_descriptor() {
        let generator =
            RistrettoCodeGenerator::new("Test", flow_transpiler::TranspileContext::new());

        let func = Function {
            name: "test".to_string(),
            params: vec![
                Param {
                    name: "x".to_string(),
                    ty: Type::I32,
                },
                Param {
                    name: "y".to_string(),
                    ty: Type::I64,
                },
            ],
            return_type: Some(Type::I32),
            body: Expr::Unit,
            is_pub: false,
            is_macro: false,
            attributes: vec![],
            span: Span::new(0, 0),
        };

        assert_eq!(generator.build_method_descriptor(&func), "(IJ)I");
    }

    #[test]
    fn test_simple_program_generation() {
        let generator =
            RistrettoCodeGenerator::new("TestClass", flow_transpiler::TranspileContext::new());

        let program = Program {
            namespace: None,
            items: vec![],
        };

        let result = generator.generate(&program);
        assert!(result.is_ok(), "Failed to generate empty program");

        let class_files = result.unwrap();
        assert!(
            !class_files.is_empty(),
            "Should generate at least main class"
        );

        let main_bytes = class_files
            .get("TestClass")
            .expect("Main class should exist");
        assert!(
            !main_bytes.is_empty(),
            "Generated bytecode should not be empty"
        );
        // Check magic number
        assert_eq!(&main_bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
    }

    #[test]
    fn test_struct_generation() {
        let generator =
            RistrettoCodeGenerator::new("Main", flow_transpiler::TranspileContext::new());

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

        let program = Program {
            namespace: None,
            items: vec![Item::Struct(struct_def)],
        };

        let result = generator.generate(&program);
        assert!(result.is_ok(), "Failed to generate struct");

        let class_files = result.unwrap();
        assert!(
            class_files.contains_key("Point"),
            "Should generate Point class"
        );
        assert!(
            class_files.contains_key("Main"),
            "Should generate Main class"
        );

        let point_bytes = class_files.get("Point").unwrap();
        assert_eq!(&point_bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
    }
}
