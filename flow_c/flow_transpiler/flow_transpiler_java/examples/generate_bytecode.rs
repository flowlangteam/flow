#![allow(clippy::collapsible_if)]
use flow_ast::*;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::fs;
use std::io::{self, Cursor};
use std::path::Path;
use std::process::Command;

use zip::ZipArchive;

fn dummy_span() -> Span {
    Span::new(0, 0)
}

fn write_classes_from_jar(bytes: &[u8]) -> io::Result<Vec<String>> {
    let cursor = Cursor::new(bytes);
    let mut archive = ZipArchive::new(cursor)?;
    let mut written = Vec::new();

    for i in 0..archive.len() {
        let mut file = archive.by_index(i)?;
        if file.name().ends_with('/') {
            continue;
        }

        let path = Path::new(file.name());
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)?;
            }
        }

        let mut out_file = fs::File::create(path)?;
        io::copy(&mut file, &mut out_file)?;
        written.push(file.name().to_string());
    }

    Ok(written)
}

fn main() {
    println!("Testing Java Bytecode Generation\n");

    // Test 1: Simple function
    println!("1. Testing simple function...");
    let program = Program {
        namespace: None,
        items: vec![Item::Function(Function {
            name: "getValue".to_string(),
            params: vec![],
            return_type: Some(Type::I64),
            body: Expr::Integer(42),
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        })],
    };

    let mut transpiler = JavaTranspiler::new("SimpleTest");
    let jar_bytes = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("SimpleTest.jar", &jar_bytes).expect("Failed to write jar file");
    let class_files =
        write_classes_from_jar(&jar_bytes).expect("Failed to extract SimpleTest classes");
    let class_name = class_files
        .iter()
        .find(|name| name.ends_with("SimpleTest.class"))
        .or_else(|| class_files.iter().find(|name| name.ends_with(".class")))
        .expect("No class file generated")
        .to_string();
    let class_bytes = fs::read(&class_name).expect("Failed to read generated class");
    println!(
        "   Generated SimpleTest.jar ({} bytes), extracted {} ({} bytes)",
        jar_bytes.len(),
        class_name,
        class_bytes.len()
    );

    // Verify with javap
    let output = Command::new("javap").arg("-v").arg(&class_name).output();

    if let Ok(output) = output {
        println!(
            "   javap verification: {}",
            if output.status.success() {
                "✓ VALID"
            } else {
                "✗ INVALID"
            }
        );
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
        namespace: None,
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
            is_macro: false,
            attributes: vec![],
            span: dummy_span(),
        })],
    };

    let mut transpiler = JavaTranspiler::new("ArithmeticTest");
    let jar_bytes = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("ArithmeticTest.jar", &jar_bytes).expect("Failed to write jar file");
    let class_files =
        write_classes_from_jar(&jar_bytes).expect("Failed to extract ArithmeticTest classes");
    let class_name = class_files
        .iter()
        .find(|name| name.ends_with("ArithmeticTest.class"))
        .or_else(|| class_files.iter().find(|name| name.ends_with(".class")))
        .expect("No class file generated")
        .to_string();
    let class_bytes = fs::read(&class_name).expect("Failed to read generated class");
    println!(
        "   Generated ArithmeticTest.jar ({} bytes), extracted {} ({} bytes)",
        jar_bytes.len(),
        class_name,
        class_bytes.len()
    );

    // Test 3: Complex example with pipes
    println!("\n3. Testing pipe operator...");
    let program = Program {
        namespace: None,
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
                is_macro: false,
                attributes: vec![],
                span: dummy_span(),
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
                is_macro: false,
                attributes: vec![],
                span: dummy_span(),
            }),
        ],
    };

    let mut transpiler = JavaTranspiler::new("PipeTest");
    let jar_bytes = transpiler.transpile(&program).expect("Failed to transpile");
    fs::write("PipeTest.jar", &jar_bytes).expect("Failed to write jar file");
    let class_files =
        write_classes_from_jar(&jar_bytes).expect("Failed to extract PipeTest classes");
    let class_name = class_files
        .iter()
        .find(|name| name.ends_with("PipeTest.class"))
        .or_else(|| class_files.iter().find(|name| name.ends_with(".class")))
        .expect("No class file generated")
        .to_string();
    let class_bytes = fs::read(&class_name).expect("Failed to read generated class");
    println!(
        "   Generated PipeTest.jar ({} bytes), extracted {} ({} bytes)",
        jar_bytes.len(),
        class_name,
        class_bytes.len()
    );

    println!("\n✓ All tests completed successfully!");
    println!("\nGenerated class files:");
    println!("  - SimpleTest.class");
    println!("  - ArithmeticTest.class");
    println!("  - PipeTest.class");
    println!("\nYou can inspect them with: javap -v <classname>.class");
}
