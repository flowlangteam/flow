//! Pure Rust JVM bytecode generator using the ristretto_classfile library
//!
//! This module provides a clean, library-based approach to generating
//! JVM bytecode without any Java dependencies.

use ristretto_classfile::attributes::Instruction;
use ristretto_classfile::{
    BaseType, ClassAccessFlags, ClassFile, ConstantPool, Field as ClassField, FieldAccessFlags,
    FieldType, JAVA_8, Method, MethodAccessFlags, attributes::Attribute,
};
use std::collections::HashMap;

use flow_ast::*;
use flow_transpiler::{Result, TranspileContext, TranspilerError};

/// Generates JVM bytecode using the ristretto_classfile library
pub struct RistrettoCodeGenerator {
    class_name: String,
    context: TranspileContext,
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
            if let Item::Impl(impl_block) = item {
                if impl_block.struct_name == struct_def.name {
                    for method in &impl_block.methods {
                        methods.push(self.create_instance_method(&mut constant_pool, method)?);
                    }
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

        // Generate code for the function body
        match &func.body {
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.generate_expr_code(constant_pool, &mut code, expr)?;
                }
            }
            _ => {
                // Single expression body
                self.generate_expr_code(constant_pool, &mut code, &func.body)?;
            }
        }

        // Add return instruction if not already present
        if func.return_type.is_some() {
            // For now, just return a default value
            match &func.return_type {
                Some(Type::I32) | Some(Type::Bool) => {
                    code.push(Instruction::Iconst_0);
                    code.push(Instruction::Ireturn);
                }
                Some(Type::I64) => {
                    code.push(Instruction::Lconst_0);
                    code.push(Instruction::Lreturn);
                }
                Some(Type::F32) => {
                    code.push(Instruction::Fconst_0);
                    code.push(Instruction::Freturn);
                }
                Some(Type::F64) => {
                    code.push(Instruction::Dconst_0);
                    code.push(Instruction::Dreturn);
                }
                _ => {
                    // Reference type - push null and return
                    code.push(Instruction::Aconst_null);
                    code.push(Instruction::Areturn);
                }
            }
        } else {
            code.push(Instruction::Return);
        }

        Ok(code)
    }

    /// Generate bytecode for an expression
    fn generate_expr_code(
        &self,
        constant_pool: &mut ConstantPool,
        code: &mut Vec<Instruction>,
        expr: &Expr,
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
                } else if val >= -128 && val <= 127 {
                    code.push(Instruction::Bipush(val as i8));
                } else if val >= -32768 && val <= 32767 {
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
            Expr::Ident(_name) => {
                // Load from local variable
                // TODO: Track actual variable indices
                code.push(Instruction::Iload_1);
            }
            Expr::Field { expr, field: _ } => {
                // Generate code to load the object
                self.generate_expr_code(constant_pool, code, expr)?;

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
                for (_field_name, field_expr) in fields {
                    self.generate_expr_code(constant_pool, code, field_expr)?;
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
                self.generate_expr_code(constant_pool, code, expr)?;

                // Load arguments
                for arg in args {
                    self.generate_expr_code(constant_pool, code, arg)?;
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
                // Generate code for left and right operands
                self.generate_expr_code(constant_pool, code, left)?;
                self.generate_expr_code(constant_pool, code, right)?;

                // Add the appropriate operation
                match op {
                    BinOp::Add => code.push(Instruction::Iadd),
                    BinOp::Sub => code.push(Instruction::Isub),
                    BinOp::Mul => code.push(Instruction::Imul),
                    BinOp::Div => code.push(Instruction::Idiv),
                    BinOp::Mod => code.push(Instruction::Irem),
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
                    _ => {
                        // Other operators not yet implemented
                        code.push(Instruction::Pop);
                        code.push(Instruction::Pop);
                        code.push(Instruction::Iconst_0);
                    }
                }
            }
            Expr::Call { func, args } => {
                // Generate code for arguments
                for arg in args {
                    self.generate_expr_code(constant_pool, code, arg)?;
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
                    self.generate_expr_code(constant_pool, code, value)?;
                    // TODO: Use appropriate return instruction based on type
                    code.push(Instruction::Ireturn);
                } else {
                    code.push(Instruction::Return);
                }
            }
            Expr::Let { value, then, .. } => {
                // Generate code for the value
                self.generate_expr_code(constant_pool, code, value)?;
                // Store to local variable (simplified)
                code.push(Instruction::Istore_1);
                // Generate code for the continuation
                self.generate_expr_code(constant_pool, code, then)?;
            }
            Expr::If { cond, then, else_ } => {
                // Generate condition code
                self.generate_expr_code(constant_pool, code, cond)?;

                // For now, just evaluate both branches (not proper control flow)
                // TODO: Implement proper if/else with branching
                code.push(Instruction::Pop);
                self.generate_expr_code(constant_pool, code, then)?;

                if let Some(else_expr) = else_ {
                    code.push(Instruction::Pop);
                    self.generate_expr_code(constant_pool, code, else_expr)?;
                }
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.generate_expr_code(constant_pool, code, expr)?;
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
