use flow_ast::*;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::fs;
use std::process::Command;

fn main() {
    println!("=== Flow to Java Bytecode Transpiler Demo ===\n");

    // Test 1: Simple calculation
    println!("1. Compiling and running simple calculation...");
    let program = Program {
        items: vec![Item::Function(Function {
            name: "calculate".to_string(),
            params: vec![],
            return_type: Some(Type::I64),
            body: Expr::Binary {
                op: BinOp::Add,
                left: Box::new(Expr::Binary {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Integer(10)),
                    right: Box::new(Expr::Integer(5)),
                }),
                right: Box::new(Expr::Integer(7)),
            },
            is_pub: true,
        })],
    };

    let mut transpiler = JavaTranspiler::new("Calculator");
    let bytecode = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("Calculator.class", &bytecode).expect("Failed to write class file");

    // Create a Java wrapper to call the method
    let wrapper = r#"
public class RunCalculator {
    public static void main(String[] args) {
        long result = Calculator.calculate();
        System.out.println("Result: " + result);
        System.out.println("Expected: 57");
        System.out.println("Match: " + (result == 57));
    }
}
"#;
    fs::write("RunCalculator.java", wrapper).expect("Failed to write wrapper");

    // Compile and run
    let compile = Command::new("javac").args(&["RunCalculator.java"]).output();

    if let Ok(output) = compile {
        if output.status.success() {
            println!("   ✓ Compiled successfully");

            let run = Command::new("java").arg("RunCalculator").output();

            if let Ok(run_output) = run {
                if run_output.status.success() {
                    let stdout = String::from_utf8_lossy(&run_output.stdout);
                    println!("   Output:");
                    for line in stdout.lines() {
                        println!("     {}", line);
                    }
                } else {
                    let stderr = String::from_utf8_lossy(&run_output.stderr);
                    println!("   ✗ Runtime error: {}", stderr);
                }
            }
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!("   ✗ Compilation error: {}", stderr);
        }
    } else {
        println!("   ⚠ Java compiler not available");
    }

    // Test 2: Fibonacci-like calculation with pipe
    println!("\n2. Compiling and running pipe operator...");
    let program = Program {
        items: vec![
            Item::Function(Function {
                name: "triple".to_string(),
                params: vec![Param {
                    name: "x".to_string(),
                    ty: Type::I64,
                }],
                return_type: Some(Type::I64),
                body: Expr::Binary {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Ident("x".to_string())),
                    right: Box::new(Expr::Integer(3)),
                },
                is_pub: false,
            }),
            Item::Function(Function {
                name: "addTen".to_string(),
                params: vec![Param {
                    name: "x".to_string(),
                    ty: Type::I64,
                }],
                return_type: Some(Type::I64),
                body: Expr::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Ident("x".to_string())),
                    right: Box::new(Expr::Integer(10)),
                },
                is_pub: false,
            }),
            Item::Function(Function {
                name: "compute".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Pipe {
                    left: Box::new(Expr::Integer(7)),
                    right: Box::new(Expr::Pipe {
                        left: Box::new(Expr::Ident("triple".to_string())),
                        right: Box::new(Expr::Ident("addTen".to_string())),
                    }),
                },
                is_pub: true,
            }),
        ],
    };

    let mut transpiler = JavaTranspiler::new("PipeDemo");
    let bytecode = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("PipeDemo.class", &bytecode).expect("Failed to write class file");

    let wrapper = r#"
public class RunPipeDemo {
    public static void main(String[] args) {
        long result = PipeDemo.compute();
        System.out.println("Result: " + result);
        System.out.println("Expected: 31 (7 * 3 + 10)");
        System.out.println("Match: " + (result == 31));
    }
}
"#;
    fs::write("RunPipeDemo.java", wrapper).expect("Failed to write wrapper");

    let compile = Command::new("javac").args(&["RunPipeDemo.java"]).output();

    if let Ok(output) = compile {
        if output.status.success() {
            println!("   ✓ Compiled successfully");

            let run = Command::new("java").arg("RunPipeDemo").output();

            if let Ok(run_output) = run {
                if run_output.status.success() {
                    let stdout = String::from_utf8_lossy(&run_output.stdout);
                    println!("   Output:");
                    for line in stdout.lines() {
                        println!("     {}", line);
                    }
                } else {
                    let stderr = String::from_utf8_lossy(&run_output.stderr);
                    println!("   ✗ Runtime error: {}", stderr);
                }
            }
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!("   ✗ Compilation error: {}", stderr);
        }
    } else {
        println!("   ⚠ Java compiler not available");
    }

    println!("\n=== Demo Complete ===");
    println!("\nGenerated bytecode files:");
    println!("  - Calculator.class (simple arithmetic)");
    println!("  - PipeDemo.class (pipe operator with multiple functions)");
}
