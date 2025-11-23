#![allow(clippy::collapsible_if)]
use flow_ast::*;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::fs;
use std::io::{self, Cursor};
use std::path::Path;

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
    println!("Debugging Java Bytecode Generation\n");

    // Simple test
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
    let class_files = write_classes_from_jar(&jar_bytes).expect("Failed to extract classes");

    let class_name = class_files
        .iter()
        .find(|name| name.ends_with("SimpleTest.class"))
        .or_else(|| class_files.iter().find(|name| name.ends_with(".class")))
        .expect("No class file generated")
        .to_string();
    let class_bytes = fs::read(&class_name).expect("Failed to read generated class");

    println!("Generated JAR size: {} bytes", jar_bytes.len());
    println!("\nFirst 200 bytes (hex):");
    for (i, chunk) in class_bytes.chunks(16).enumerate().take(13) {
        print!("{:08x}: ", i * 16);
        for byte in chunk {
            print!("{:02x} ", byte);
        }
        println!();
    }

    // Parse the constant pool manually
    println!("\n=== Manual Constant Pool Parse ===");
    let mut pos = 8; // Skip magic + version
    let cp_count = u16::from_be_bytes([class_bytes[pos], class_bytes[pos + 1]]);
    println!(
        "Constant pool count: {} (means {} entries)",
        cp_count,
        cp_count - 1
    );
    pos += 2;

    for i in 1..cp_count {
        let tag = class_bytes[pos];
        print!("#{}: ", i);
        pos += 1;

        match tag {
            1 => {
                // UTF8
                let len = u16::from_be_bytes([class_bytes[pos], class_bytes[pos + 1]]);
                pos += 2;
                let s = String::from_utf8_lossy(&class_bytes[pos..pos + len as usize]);
                println!("UTF8: \"{}\"", s);
                pos += len as usize;
            }
            3 => {
                // Integer
                let val = i32::from_be_bytes([
                    class_bytes[pos],
                    class_bytes[pos + 1],
                    class_bytes[pos + 2],
                    class_bytes[pos + 3],
                ]);
                println!("Integer: {}", val);
                pos += 4;
            }
            5 => {
                // Long
                let val = i64::from_be_bytes([
                    class_bytes[pos],
                    class_bytes[pos + 1],
                    class_bytes[pos + 2],
                    class_bytes[pos + 3],
                    class_bytes[pos + 4],
                    class_bytes[pos + 5],
                    class_bytes[pos + 6],
                    class_bytes[pos + 7],
                ]);
                println!("Long: {}", val);
                pos += 8;
            }
            7 => {
                // Class
                let name_idx = u16::from_be_bytes([class_bytes[pos], class_bytes[pos + 1]]);
                println!("Class: name_index=#{}", name_idx);
                pos += 2;
            }
            10 => {
                // Methodref
                let class_idx = u16::from_be_bytes([class_bytes[pos], class_bytes[pos + 1]]);
                let nat_idx = u16::from_be_bytes([class_bytes[pos + 2], class_bytes[pos + 3]]);
                println!(
                    "Methodref: class_index=#{}, name_and_type_index=#{}",
                    class_idx, nat_idx
                );
                pos += 4;
            }
            12 => {
                // NameAndType
                let name_idx = u16::from_be_bytes([class_bytes[pos], class_bytes[pos + 1]]);
                let desc_idx = u16::from_be_bytes([class_bytes[pos + 2], class_bytes[pos + 3]]);
                println!(
                    "NameAndType: name_index=#{}, descriptor_index=#{}",
                    name_idx, desc_idx
                );
                pos += 4;
            }
            _ => {
                println!("Unknown tag: {}", tag);
                break;
            }
        }
    }

    println!("\nConstant pool ends at byte: 0x{:x} ({})", pos, pos);
    println!(
        "Next bytes: {:02x} {:02x} {:02x} {:02x}",
        class_bytes[pos],
        class_bytes[pos + 1],
        class_bytes[pos + 2],
        class_bytes[pos + 3]
    );
}
