use flow_ast::*;
use flow_transpiler::Transpiler;
use flow_transpiler_java::JavaTranspiler;
use std::fs;

fn main() {
    println!("Debugging Java Bytecode Generation\n");

    // Simple test
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

    println!("Generated {} bytes", bytecode.len());
    println!("\nFirst 200 bytes (hex):");
    for (i, chunk) in bytecode.chunks(16).enumerate().take(13) {
        print!("{:08x}: ", i * 16);
        for byte in chunk {
            print!("{:02x} ", byte);
        }
        println!();
    }

    // Parse the constant pool manually
    println!("\n=== Manual Constant Pool Parse ===");
    let mut pos = 8; // Skip magic + version
    let cp_count = u16::from_be_bytes([bytecode[pos], bytecode[pos + 1]]);
    println!(
        "Constant pool count: {} (means {} entries)",
        cp_count,
        cp_count - 1
    );
    pos += 2;

    for i in 1..cp_count {
        let tag = bytecode[pos];
        print!("#{}: ", i);
        pos += 1;

        match tag {
            1 => {
                // UTF8
                let len = u16::from_be_bytes([bytecode[pos], bytecode[pos + 1]]);
                pos += 2;
                let s = String::from_utf8_lossy(&bytecode[pos..pos + len as usize]);
                println!("UTF8: \"{}\"", s);
                pos += len as usize;
            }
            3 => {
                // Integer
                let val = i32::from_be_bytes([
                    bytecode[pos],
                    bytecode[pos + 1],
                    bytecode[pos + 2],
                    bytecode[pos + 3],
                ]);
                println!("Integer: {}", val);
                pos += 4;
            }
            5 => {
                // Long
                let val = i64::from_be_bytes([
                    bytecode[pos],
                    bytecode[pos + 1],
                    bytecode[pos + 2],
                    bytecode[pos + 3],
                    bytecode[pos + 4],
                    bytecode[pos + 5],
                    bytecode[pos + 6],
                    bytecode[pos + 7],
                ]);
                println!("Long: {}", val);
                pos += 8;
            }
            7 => {
                // Class
                let name_idx = u16::from_be_bytes([bytecode[pos], bytecode[pos + 1]]);
                println!("Class: name_index=#{}", name_idx);
                pos += 2;
            }
            10 => {
                // Methodref
                let class_idx = u16::from_be_bytes([bytecode[pos], bytecode[pos + 1]]);
                let nat_idx = u16::from_be_bytes([bytecode[pos + 2], bytecode[pos + 3]]);
                println!(
                    "Methodref: class_index=#{}, name_and_type_index=#{}",
                    class_idx, nat_idx
                );
                pos += 4;
            }
            12 => {
                // NameAndType
                let name_idx = u16::from_be_bytes([bytecode[pos], bytecode[pos + 1]]);
                let desc_idx = u16::from_be_bytes([bytecode[pos + 2], bytecode[pos + 3]]);
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
        bytecode[pos],
        bytecode[pos + 1],
        bytecode[pos + 2],
        bytecode[pos + 3]
    );
}
