use flow_ast::Program;
use flow_compiler::{CompilationTarget, CompilerConfig, CompilerFactory, FlowCompilerBuilder};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Flow Unified Compiler API Demo");

    // Example 1: Simple builder pattern usage
    simple_jit_compilation_example()?;

    // Example 2: AOT compilation
    aot_compilation_example()?;

    // Example 3: Java bytecode generation
    java_transpilation_example()?;

    // Example 4: Factory pattern usage
    factory_pattern_example()?;

    // Example 5: Module compilation and linking
    module_compilation_example()?;

    Ok(())
}

fn simple_jit_compilation_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== JIT Compilation Example ===");

    // Create a simple Flow program (this would normally be parsed from source)
    let program = create_simple_program();

    // Compile using builder pattern
    let result = FlowCompilerBuilder::new()
        .target(CompilationTarget::Jit)
        .optimization_level(2)
        .debug_info(true)
        .compile_program(&program)?;

    match result {
        flow_compiler::CompilerOutput::Jit(func_ptr) => {
            println!(
                "✓ JIT compilation successful! Function pointer: {:?}",
                func_ptr
            );
        }
        _ => println!("❌ Unexpected output type for JIT compilation"),
    }

    Ok(())
}

fn aot_compilation_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== AOT Compilation Example ===");

    let program = create_simple_program();

    let result = FlowCompilerBuilder::new()
        .target(CompilationTarget::Native)
        .optimization_level(3)
        .output_dir("/tmp/flow_output")
        .flag("pic", "true")
        .compile_program(&program)?;

    match result {
        flow_compiler::CompilerOutput::Object(object_bytes) => {
            println!(
                "✓ AOT compilation successful! Object size: {} bytes",
                object_bytes.len()
            );
        }
        _ => println!("❌ Unexpected output type for AOT compilation"),
    }

    Ok(())
}

fn java_transpilation_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== Java Transpilation Example ===");

    let program = create_simple_program();

    let result = FlowCompilerBuilder::new()
        .target(CompilationTarget::JavaBytecode)
        .debug_info(true)
        .flag("class_name", "MyFlowProgram")
        .compile_program(&program)?;

    match result {
        flow_compiler::CompilerOutput::Code(bytecode) => {
            println!(
                "✓ Java transpilation successful! Bytecode size: {} bytes",
                bytecode.len()
            );
        }
        _ => println!("❌ Unexpected output type for Java transpilation"),
    }

    Ok(())
}

fn factory_pattern_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== Factory Pattern Example ===");

    let program = create_simple_program();
    let config = CompilerConfig::new(CompilationTarget::Jit)
        .with_optimization(1)
        .with_debug_info(false);

    // Show supported targets
    println!(
        "Supported targets: {:?}",
        CompilerFactory::supported_targets()
    );

    // Create and use compiler
    let mut compiler = CompilerFactory::create_compiler(CompilationTarget::Jit)?;
    println!("Created compiler for target: {:?}", compiler.target_info());
    println!("Supported features: {:?}", compiler.supported_features());

    // Validate before compilation
    compiler.validate_program(&program)?;
    println!("✓ Program validation passed");

    let _result = compiler.compile_program(&program, &config)?;
    println!("✓ Compilation successful using factory pattern");

    Ok(())
}

fn module_compilation_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== Module Compilation Example ===");

    let library_program = create_library_program();
    let main_program = create_main_program();

    let config = CompilerConfig::new(CompilationTarget::Native);

    // Compile library module
    let mut compiler = CompilerFactory::create_compiler(CompilationTarget::Native)?;
    let library_module = compiler.compile_module(&library_program, &config)?;
    println!("✓ Library module compiled: {}", library_module.name);
    println!(
        "  Functions: {:?}",
        library_module.functions.keys().collect::<Vec<_>>()
    );
    println!("  Dependencies: {:?}", library_module.dependencies);

    // Compile main module
    let main_module = compiler.compile_module(&main_program, &config)?;
    println!("✓ Main module compiled: {}", main_module.name);

    // Link modules together
    let modules = vec![library_module, main_module];
    let linked_result = compiler.link_modules(&modules, &config)?;

    match linked_result {
        flow_compiler::CompilerOutput::Object(object_bytes) => {
            println!(
                "✓ Modules linked successfully! Final size: {} bytes",
                object_bytes.len()
            );
        }
        _ => println!("❌ Unexpected output type for linked modules"),
    }

    Ok(())
}

// Helper functions to create example programs
fn create_simple_program() -> Program {
    use flow_ast::*;

    Program {
        namespace: None,
        items: vec![Item::Function(Function {
            name: "main".to_string(),
            params: vec![],
            return_type: None,
            body: Expr::Integer(42),
            is_pub: true,
            span: Span::new(0, 0),
        })],
    }
}

fn create_library_program() -> Program {
    use flow_ast::*;

    Program {
        namespace: Some(NamespaceDecl {
            namespace: "mylib".to_string(),
            filename: "mylib.flow".to_string(),
        }),
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
            span: Span::new(0, 0),
        })],
    }
}

fn create_main_program() -> Program {
    use flow_ast::*;

    Program {
        namespace: None,
        items: vec![
            Item::Import(Import {
                path: vec!["mylib".to_string()],
                alias: None,
            }),
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: None,
                body: Expr::Call {
                    func: Box::new(Expr::Ident("add".to_string())),
                    args: vec![Expr::Integer(10), Expr::Integer(20)],
                },
                is_pub: true,
                span: Span::new(0, 0),
            }),
        ],
    }
}
