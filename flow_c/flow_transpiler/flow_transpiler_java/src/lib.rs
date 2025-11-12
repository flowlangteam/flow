use byteorder::{BigEndian, WriteBytesExt};
use flow_ast::*;
use flow_transpiler::{Result, TranspileContext, Transpiler, TranspilerError};
use std::collections::HashMap;
use std::io::Write;

mod bytecode;
mod class_writer;
mod constant_pool;

use bytecode::*;
use class_writer::ClassWriter;
use constant_pool::ConstantPool;

pub struct JavaTranspiler {
    context: TranspileContext,
    class_name: String,
}

impl JavaTranspiler {
    pub fn new(class_name: impl Into<String>) -> Self {
        Self {
            context: TranspileContext::new(),
            class_name: class_name.into(),
        }
    }

    pub fn with_context(class_name: impl Into<String>, context: TranspileContext) -> Self {
        Self {
            context,
            class_name: class_name.into(),
        }
    }
}

impl Transpiler for JavaTranspiler {
    type Output = Vec<u8>;

    fn transpile(&mut self, program: &Program) -> Result<Self::Output> {
        // First pass: collect all functions and structs
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.context.add_function(func);
                }
                Item::Struct(struct_def) => {
                    self.context.add_struct(struct_def);
                }
                Item::Impl(_) => {
                    // Methods will be added as we process them
                }
                _ => {}
            }
        }

        let mut class = ClassWriter::new(&self.class_name);

        // Add main class structure
        class.set_super_class("java/lang/Object");
        class.set_access_flags(ACC_PUBLIC | ACC_SUPER);

        // Add default constructor
        class.add_default_constructor();

        // Second pass: transpile functions
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.transpile_function(&mut class, func)?;
                }
                Item::Struct(struct_def) => {
                    self.transpile_struct(&mut class, struct_def)?;
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.transpile_function(&mut class, method)?;
                    }
                }
                _ => {}
            }
        }

        class.write_bytes()
    }

    fn target_name(&self) -> &str {
        "Java Bytecode"
    }
}

impl JavaTranspiler {
    fn transpile_function(&self, class: &mut ClassWriter, func: &Function) -> Result<()> {
        let mut method = class.add_method(&func.name);

        // Set access flags
        if func.is_pub {
            method.set_access_flags(ACC_PUBLIC | ACC_STATIC);
        } else {
            method.set_access_flags(ACC_PRIVATE | ACC_STATIC);
        }

        // Build method descriptor
        let params_types: Vec<(String, Type)> = func
            .params
            .iter()
            .map(|p| (p.name.clone(), p.ty.clone()))
            .collect();
        let descriptor = self.build_method_descriptor(&params_types, &func.return_type);
        method.set_descriptor(&descriptor);

        // Transpile function body
        let mut code = CodeBuilder::new();
        let mut locals = HashMap::new();

        // Map parameters to local variable indices
        for (i, param) in func.params.iter().enumerate() {
            locals.insert(param.name.clone(), i as u16);
        }

        self.transpile_expr(&mut code, &func.body, &mut locals)?;

        // Add return instruction based on return type
        if let Some(return_type) = &func.return_type {
            match return_type {
                Type::I32 | Type::Bool | Type::Char => {
                    code.add_instruction(Instruction::IReturn);
                }
                Type::I64 => {
                    code.add_instruction(Instruction::LReturn);
                }
                Type::F32 => {
                    code.add_instruction(Instruction::FReturn);
                }
                Type::F64 => {
                    code.add_instruction(Instruction::DReturn);
                }
                _ => {
                    code.add_instruction(Instruction::AReturn);
                }
            }
        } else {
            code.add_instruction(Instruction::Return);
        }

        method.set_code(code.build());

        Ok(())
    }

    fn transpile_struct(&self, _class: &mut ClassWriter, _struct_def: &Struct) -> Result<()> {
        // For now, structs are represented as separate inner classes
        // or we could use fields in the main class
        // This is a simplified implementation
        Ok(())
    }

    fn transpile_expr(
        &self,
        code: &mut CodeBuilder,
        expr: &Expr,
        locals: &mut HashMap<String, u16>,
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
            Expr::Unit => {
                // Unit type doesn't produce a value
            }
            Expr::Ident(name) => {
                if let Some(&index) = locals.get(name) {
                    code.add_instruction(Instruction::LLoad(index));
                } else {
                    return Err(TranspilerError::NameResolutionError(format!(
                        "Undefined variable: {}",
                        name
                    )));
                }
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
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq => {
                        // Comparisons need special handling with jumps
                        code.add_instruction(Instruction::LCmp);
                        code.add_instruction(Instruction::IConst(1));
                        return Ok(());
                    }
                    BinOp::And => Instruction::LAnd,
                    BinOp::Or => Instruction::LOr,
                };
                code.add_instruction(instr);
            }
            Expr::Unary { op, expr } => {
                self.transpile_expr(code, expr, locals)?;
                match op {
                    UnOp::Neg => code.add_instruction(Instruction::LNeg),
                    UnOp::Not => {
                        // Logical not: if value == 0 then 1 else 0
                        code.add_instruction(Instruction::IConst(0));
                        code.add_instruction(Instruction::IfICmpEq(7)); // Jump if equal
                        code.add_instruction(Instruction::IConst(0));
                        code.add_instruction(Instruction::Goto(4));
                        code.add_instruction(Instruction::IConst(1));
                    }
                }
            }
            Expr::Let {
                name, value, then, ..
            } => {
                self.transpile_expr(code, value, locals)?;

                // Find the next available local variable slot
                let index = if locals.is_empty() {
                    0
                } else {
                    locals.values().max().unwrap() + 1
                };
                code.add_instruction(Instruction::LStore(index));
                locals.insert(name.clone(), index);

                self.transpile_expr(code, then, locals)?;
            }
            Expr::If { cond, then, else_ } => {
                self.transpile_expr(code, cond, locals)?;

                // If false, jump to else block
                code.add_instruction(Instruction::IfEq(0)); // Placeholder offset
                let if_jump_index = code.instruction_count() - 1;

                self.transpile_expr(code, then, locals)?;

                if let Some(else_expr) = else_ {
                    code.add_instruction(Instruction::Goto(0)); // Placeholder
                    let goto_index = code.instruction_count() - 1;

                    let else_offset = code.current_offset();
                    code.patch_jump(if_jump_index, else_offset as i16);

                    self.transpile_expr(code, else_expr, locals)?;

                    let end_offset = code.current_offset();
                    code.patch_jump(goto_index, end_offset as i16);
                } else {
                    let end_offset = code.current_offset();
                    code.patch_jump(if_jump_index, end_offset as i16);
                }
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.transpile_expr(code, expr, locals)?;
                }
            }
            Expr::Call { func, args } => {
                if let Expr::Ident(func_name) = &**func {
                    // Load arguments
                    for arg in args {
                        self.transpile_expr(code, arg, locals)?;
                    }

                    // Invoke static method
                    let sig = self.context.functions.get(func_name).ok_or_else(|| {
                        TranspilerError::NameResolutionError(format!(
                            "Function {} not found",
                            func_name
                        ))
                    })?;

                    let descriptor = self.build_method_descriptor(&sig.params, &sig.return_type);
                    code.add_instruction(Instruction::InvokeStatic(
                        self.class_name.clone(),
                        func_name.clone(),
                        descriptor,
                    ));
                } else {
                    return Err(TranspilerError::UnsupportedFeature(
                        "Only simple function calls supported".to_string(),
                    ));
                }
            }
            Expr::Pipe { left, right } => {
                // Handle pipe operator: left |> right
                // For chained pipes like x |> f |> g, the AST is:
                //   Pipe { left: x, right: Pipe { left: f, right: g } }

                // First, evaluate the left side (puts value on stack)
                self.transpile_expr(code, left, locals)?;

                // Then apply the right side to the value on the stack
                self.apply_pipe_right(code, right, locals)?;
            }
            Expr::Return(val) => {
                if let Some(expr) = val {
                    self.transpile_expr(code, expr, locals)?;
                }
            }
            _ => {
                return Err(TranspilerError::UnsupportedFeature(format!(
                    "Expression not yet supported: {:?}",
                    expr
                )));
            }
        }

        Ok(())
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

    fn apply_pipe_right(
        &self,
        code: &mut CodeBuilder,
        right: &Expr,
        locals: &mut HashMap<String, u16>,
    ) -> Result<()> {
        match right {
            Expr::Ident(func_name) => {
                // Simple function call: value |> func
                let sig = self.context.functions.get(func_name).ok_or_else(|| {
                    TranspilerError::NameResolutionError(format!(
                        "Function {} not found",
                        func_name
                    ))
                })?;

                let descriptor = self.build_method_descriptor(&sig.params, &sig.return_type);
                code.add_instruction(Instruction::InvokeStatic(
                    self.class_name.clone(),
                    func_name.clone(),
                    descriptor,
                ));
                Ok(())
            }
            Expr::Pipe {
                left: pipe_func,
                right: pipe_right,
            } => {
                // Chained pipe: value |> func1 |> func2
                // First apply func1 to the value on the stack
                self.apply_pipe_right(code, pipe_func, locals)?;
                // Then apply func2 to the result (which is now on the stack)
                self.apply_pipe_right(code, pipe_right, locals)?;
                Ok(())
            }
            Expr::Call { func, args } => {
                // Pipe with additional arguments: value |> func(arg1, arg2)
                // The piped value is already on the stack as the first argument
                if let Expr::Ident(func_name) = &**func {
                    // Load additional arguments after the piped value
                    for arg in args {
                        self.transpile_expr(code, arg, locals)?;
                    }

                    let sig = self.context.functions.get(func_name).ok_or_else(|| {
                        TranspilerError::NameResolutionError(format!(
                            "Function {} not found",
                            func_name
                        ))
                    })?;

                    let descriptor = self.build_method_descriptor(&sig.params, &sig.return_type);
                    code.add_instruction(Instruction::InvokeStatic(
                        self.class_name.clone(),
                        func_name.clone(),
                        descriptor,
                    ));
                    Ok(())
                } else {
                    Err(TranspilerError::UnsupportedFeature(
                        "Only simple function calls supported in pipes".to_string(),
                    ))
                }
            }
            _ => Err(TranspilerError::UnsupportedFeature(format!(
                "Unsupported pipe right-hand side: {:?}",
                right
            ))),
        }
    }

    fn type_to_jvm_type(&self, ty: &Type) -> String {
        match ty {
            Type::I8 | Type::U8 => "B".to_string(),
            Type::I16 | Type::U16 => "S".to_string(),
            Type::I32 | Type::U32 => "I".to_string(),
            Type::I64 | Type::U64 => "J".to_string(),
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::Bool => "Z".to_string(),
            Type::Char => "C".to_string(),
            Type::String => "Ljava/lang/String;".to_string(),
            Type::Unit => "V".to_string(),
            Type::Named(name) => format!("L{};", name),
            Type::Pointer(_) | Type::MutPointer(_) => "Ljava/lang/Object;".to_string(),
            _ => "Ljava/lang/Object;".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_simple_function() {
        let program = Program {
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Integer(42),
                is_pub: true,
            })],
        };

        let mut transpiler = JavaTranspiler::new("Main");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty());

        // Check magic number (0xCAFEBABE)
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        // Write to file for inspection
        let _ = fs::write("target/Main.class", &bytecode);
    }

    #[test]
    fn test_arithmetic_operations() {
        let program = Program {
            items: vec![Item::Function(Function {
                name: "add".to_string(),
                params: vec![
                    Param {
                        name: "a".to_string(),
                        ty: Type::I64,
                    },
                    Param {
                        name: "b".to_string(),
                        ty: Type::I64,
                    },
                ],
                return_type: Some(Type::I64),
                body: Expr::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Ident("a".to_string())),
                    right: Box::new(Expr::Ident("b".to_string())),
                },
                is_pub: true,
            })],
        };

        let mut transpiler = JavaTranspiler::new("Arithmetic");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/Arithmetic.class", &bytecode);
    }

    #[test]
    fn test_if_expression() {
        let program = Program {
            items: vec![Item::Function(Function {
                name: "max".to_string(),
                params: vec![
                    Param {
                        name: "a".to_string(),
                        ty: Type::I64,
                    },
                    Param {
                        name: "b".to_string(),
                        ty: Type::I64,
                    },
                ],
                return_type: Some(Type::I64),
                body: Expr::If {
                    cond: Box::new(Expr::Binary {
                        op: BinOp::Gt,
                        left: Box::new(Expr::Ident("a".to_string())),
                        right: Box::new(Expr::Ident("b".to_string())),
                    }),
                    then: Box::new(Expr::Ident("a".to_string())),
                    else_: Some(Box::new(Expr::Ident("b".to_string()))),
                },
                is_pub: true,
            })],
        };

        let mut transpiler = JavaTranspiler::new("Conditional");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/Conditional.class", &bytecode);
    }

    #[test]
    fn test_pipe_operator() {
        let program = Program {
            items: vec![
                Item::Function(Function {
                    name: "double".to_string(),
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: Type::I64,
                    }],
                    return_type: Some(Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(2)),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "compute".to_string(),
                    params: vec![],
                    return_type: Some(Type::I64),
                    body: Expr::Pipe {
                        left: Box::new(Expr::Integer(21)),
                        right: Box::new(Expr::Ident("double".to_string())),
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut transpiler = JavaTranspiler::new("PipeTest");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/PipeTest.class", &bytecode);
    }

    #[test]
    fn test_let_binding() {
        let program = Program {
            items: vec![Item::Function(Function {
                name: "calc".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Let {
                    name: "x".to_string(),
                    mutable: false,
                    ty: None,
                    value: Box::new(Expr::Integer(10)),
                    then: Box::new(Expr::Binary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Ident("x".to_string())),
                        right: Box::new(Expr::Integer(5)),
                    }),
                },
                is_pub: true,
            })],
        };

        let mut transpiler = JavaTranspiler::new("LetTest");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/LetTest.class", &bytecode);
    }

    #[test]
    fn test_string_literal() {
        let program = Program {
            items: vec![Item::Function(Function {
                name: "getMessage".to_string(),
                params: vec![],
                return_type: Some(Type::String),
                body: Expr::String("Hello, Flow!".to_string()),
                is_pub: true,
            })],
        };

        let mut transpiler = JavaTranspiler::new("StringTest");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/StringTest.class", &bytecode);
    }

    #[test]
    fn test_multiple_functions() {
        let program = Program {
            items: vec![
                Item::Function(Function {
                    name: "add".to_string(),
                    params: vec![
                        Param {
                            name: "a".to_string(),
                            ty: Type::I64,
                        },
                        Param {
                            name: "b".to_string(),
                            ty: Type::I64,
                        },
                    ],
                    return_type: Some(Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Add,
                        left: Box::new(Expr::Ident("a".to_string())),
                        right: Box::new(Expr::Ident("b".to_string())),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "subtract".to_string(),
                    params: vec![
                        Param {
                            name: "a".to_string(),
                            ty: Type::I64,
                        },
                        Param {
                            name: "b".to_string(),
                            ty: Type::I64,
                        },
                    ],
                    return_type: Some(Type::I64),
                    body: Expr::Binary {
                        op: BinOp::Sub,
                        left: Box::new(Expr::Ident("a".to_string())),
                        right: Box::new(Expr::Ident("b".to_string())),
                    },
                    is_pub: false,
                }),
                Item::Function(Function {
                    name: "calculate".to_string(),
                    params: vec![],
                    return_type: Some(Type::I64),
                    body: Expr::Call {
                        func: Box::new(Expr::Ident("add".to_string())),
                        args: vec![
                            Expr::Call {
                                func: Box::new(Expr::Ident("subtract".to_string())),
                                args: vec![Expr::Integer(20), Expr::Integer(5)],
                            },
                            Expr::Integer(7),
                        ],
                    },
                    is_pub: true,
                }),
            ],
        };

        let mut transpiler = JavaTranspiler::new("MultiFunction");
        let result = transpiler.transpile(&program);
        assert!(result.is_ok());

        let bytecode = result.unwrap();
        assert_eq!(&bytecode[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);

        let _ = fs::write("target/MultiFunction.class", &bytecode);
    }
}
