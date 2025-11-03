use flow_ast::*;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::fs;
use std::process::Command;

fn main() {
    println!("Testing Java Bytecode Generation\n");
    
    // Test 1: Simple function
    println!("1. Testing simple function...");
    let program = Program {
        items: vec![Item::Function(Function {
            name: "getValue".to_string(),
            params: vec![],
            return_type: Some(Type::I64),
            body: Expr::Integer(42),
            is_pub: true,
        })],
    };
    
    let mut transpiler = JavaTranspiler::new("SimpleTest");
    let bytecode = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("SimpleTest.class", &bytecode).expect("Failed to write class file");
    println!("   Generated SimpleTest.class ({} bytes)", bytecode.len());
    
    // Verify with javap
    let output = Command::new("javap")
        .arg("-v")
        .arg("SimpleTest.class")
        .output();
    
    if let Ok(output) = output {
        println!("   javap verification: {}", if output.status.success() { "✓ VALID" } else { "✗ INVALID" });
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Print first few lines
            for line in stdout.lines().take(15) {
                println!("     {}", line);
            }
        }
    } else {
        println!("   javap not available, skipping validation");
    }
    
    // Test 2: Arithmetic operations
    println!("\n2. Testing arithmetic operations...");
    let program = Program {
        items: vec![Item::Function(Function {
            name: "add".to_string(),
            params: vec![
                Param { name: "a".to_string(), ty: Type::I64 },
                Param { name: "b".to_string(), ty: Type::I64 },
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
    
    let mut transpiler = JavaTranspiler::new("ArithmeticTest");
    let bytecode = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("ArithmeticTest.class", &bytecode).expect("Failed to write class file");
    println!("   Generated ArithmeticTest.class ({} bytes)", bytecode.len());
    
    // Test 3: Complex example with pipes
    println!("\n3. Testing pipe operator...");
    let program = Program {
        items: vec![
            Item::Function(Function {
                name: "double".to_string(),
                params: vec![Param { name: "x".to_string(), ty: Type::I64 }],
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
    let bytecode = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("PipeTest.class", &bytecode).expect("Failed to write class file");
    println!("   Generated PipeTest.class ({} bytes)", bytecode.len());
    
    println!("\n✓ All tests completed successfully!");
    println!("\nGenerated class files:");
    println!("  - SimpleTest.class");
    println!("  - ArithmeticTest.class");
    println!("  - PipeTest.class");
    println!("\nYou can inspect them with: javap -v <classname>.class");
}
