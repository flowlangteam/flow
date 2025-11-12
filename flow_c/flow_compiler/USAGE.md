# Flow Unified Compiler API Guide

The Flow Unified Compiler API provides a consistent interface for compiling Flow programs to different targets (JIT, AOT, Java bytecode, etc.). This guide shows how to use the API effectively.

## Overview

The unified API abstracts over three main compilation strategies:
- **JIT Compilation**: Runtime compilation using Cranelift for immediate execution
- **AOT Compilation**: Ahead-of-time compilation to native object code
- **Transpilation**: Converting Flow code to other languages (Java, Python, C, etc.)

## Basic Usage

### Quick Start with Builder Pattern

```rust
use flow_compiler::{FlowCompilerBuilder, CompilationTarget};
use flow_ast::Program;

// JIT compilation for immediate execution
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::Jit)
    .optimization_level(2)
    .debug_info(true)
    .compile_program(&program)?;

// AOT compilation for distribution
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::Native)
    .optimization_level(3)
    .output_dir("/tmp/flow_output")
    .compile_program(&program)?;

// Java bytecode generation
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::JavaBytecode)
    .flag("class_name", "MyProgram")
    .compile_program(&program)?;
```

### Factory Pattern for Advanced Usage

```rust
use flow_compiler::{CompilerFactory, CompilerConfig};

// Check what targets are supported
let supported = CompilerFactory::supported_targets();
println!("Supported targets: {:?}", supported);

// Create a compiler for specific target
let mut compiler = CompilerFactory::create_compiler(CompilationTarget::Jit)?;

// Configure compilation
let config = CompilerConfig::new(CompilationTarget::Jit)
    .with_optimization(2)
    .with_debug_info(true);

// Validate before compiling
compiler.validate_program(&program)?;

// Compile
let result = compiler.compile_program(&program, &config)?;
```

## Compilation Targets

### JIT Compilation (`CompilationTarget::Jit`)
- **Best for**: Development, testing, interactive environments
- **Output**: Function pointer for immediate execution
- **Features**: Fast compilation, runtime execution
- **Limitations**: No module linking, memory overhead

```rust
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::Jit)
    .compile_program(&program)?;

match result {
    CompilerOutput::Jit(func_ptr) => {
        // Execute immediately
        println!("Function compiled and ready to execute");
    }
    _ => unreachable!(),
}
```

### AOT Compilation (`CompilationTarget::Native`)
- **Best for**: Production builds, distribution, performance
- **Output**: Native object code that can be linked
- **Features**: Full optimization, small runtime footprint
- **Limitations**: Longer compilation time

```rust
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::Native)
    .optimization_level(3)
    .compile_program(&program)?;

match result {
    CompilerOutput::Object(object_bytes) => {
        // Save to file or link with other objects
        std::fs::write("program.o", &object_bytes)?;
    }
    _ => unreachable!(),
}
```

### Java Bytecode (`CompilationTarget::JavaBytecode`)
- **Best for**: JVM integration, enterprise environments
- **Output**: Java bytecode that runs on any JVM
- **Features**: Cross-platform, JVM ecosystem integration
- **Limitations**: JVM-specific limitations

```rust
let result = FlowCompilerBuilder::new()
    .target(CompilationTarget::JavaBytecode)
    .flag("class_name", "FlowProgram")
    .compile_program(&program)?;

match result {
    CompilerOutput::Code(bytecode) => {
        // Save as .class file
        std::fs::write("FlowProgram.class", &bytecode)?;
    }
    _ => unreachable!(),
}
```

## Module Compilation and Linking

### Compiling Libraries

```rust
// Compile a library module (no main function required)
let library_module = FlowCompilerBuilder::new()
    .target(CompilationTarget::Native)
    .compile_module(&library_program)?;

println!("Library: {}", library_module.name);
println!("Exports: {:?}", library_module.functions.keys());
```

### Linking Multiple Modules

```rust
// Compile multiple modules
let lib1 = compiler.compile_module(&lib1_program, &config)?;
let lib2 = compiler.compile_module(&lib2_program, &config)?;
let main_mod = compiler.compile_module(&main_program, &config)?;

// Link them together
let modules = vec![lib1, lib2, main_mod];
let linked_result = compiler.link_modules(&modules, &config)?;

match linked_result {
    CompilerOutput::Object(final_object) => {
        // Final linked executable/library
        std::fs::write("program", &final_object)?;
    }
    _ => unreachable!(),
}
```

## Configuration Options

### Optimization Levels
- **0**: No optimization (fastest compilation)
- **1**: Basic optimization (default)
- **2**: Moderate optimization
- **3**: Aggressive optimization (slowest compilation, best performance)

```rust
let config = CompilerConfig::new(target)
    .with_optimization(3);  // Maximum optimization
```

### Debug Information
```rust
let config = CompilerConfig::new(target)
    .with_debug_info(true);  // Include debug symbols
```

### Output Directory
```rust
let config = CompilerConfig::new(target)
    .with_output_dir("/build/output");
```

### Custom Flags
```rust
let config = CompilerConfig::new(target)
    .with_flag("pic", "true")           // Position-independent code
    .with_flag("class_name", "MyApp")   // Java class name
    .with_flag("target_cpu", "native"); // CPU-specific optimizations
```

## Error Handling

The API provides comprehensive error handling:

```rust
use flow_compiler::{CompilerError, Result};

match FlowCompilerBuilder::new().compile_program(&program) {
    Ok(output) => {
        // Handle successful compilation
    }
    Err(CompilerError::JitError(msg)) => {
        eprintln!("JIT compilation failed: {}", msg);
    }
    Err(CompilerError::AotError(msg)) => {
        eprintln!("AOT compilation failed: {}", msg);
    }
    Err(CompilerError::TranspileError(msg)) => {
        eprintln!("Transpilation failed: {}", msg);
    }
    Err(CompilerError::InvalidTarget(msg)) => {
        eprintln!("Unsupported target: {}", msg);
    }
    Err(e) => {
        eprintln!("Compilation error: {}", e);
    }
}
```

## Supported Features by Target

| Feature | JIT | AOT | Java | Future Targets |
|---------|-----|-----|------|----------------|
| Functions | ✅ | ✅ | ✅ | ✅ |
| Structs | ✅ | ✅ | ✅ (as classes) | ✅ |
| Conditionals | ✅ | ✅ | ✅ | ✅ |
| Loops | ✅ | ✅ | ✅ | ✅ |
| Arithmetic | ✅ | ✅ | ✅ | ✅ |
| Memory Mgmt | Manual | Manual | GC | Varies |
| Pattern Matching | 🔄 | 🔄 | 🔄 | 🔄 |
| Lambdas | 🔄 | 🔄 | 🔄 | 🔄 |
| Generics | ❌ | ❌ | ❌ | 🔄 |
| Modules | ❌ | ✅ | ✅ | ✅ |

✅ = Fully supported, 🔄 = Partial/In progress, ❌ = Not yet implemented

## Performance Considerations

### JIT Compilation
- **Compilation Speed**: Very fast
- **Runtime Performance**: Good, with potential for runtime optimization
- **Memory Usage**: Higher due to compiler overhead
- **Best for**: Development, prototyping, interactive use

### AOT Compilation  
- **Compilation Speed**: Slower due to optimization passes
- **Runtime Performance**: Best, fully optimized machine code
- **Memory Usage**: Minimal runtime overhead
- **Best for**: Production deployment, performance-critical applications

### Transpilation
- **Compilation Speed**: Fast to moderate
- **Runtime Performance**: Depends on target platform
- **Memory Usage**: Varies by target
- **Best for**: Platform integration, leveraging existing ecosystems

## Integration Examples

### Command Line Tool
```rust
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let target = match args.get(1).map(|s| s.as_str()) {
        Some("jit") => CompilationTarget::Jit,
        Some("native") => CompilationTarget::Native,
        Some("java") => CompilationTarget::JavaBytecode,
        _ => {
            eprintln!("Usage: {} <jit|native|java> <source.flow>", args[0]);
            return Ok(());
        }
    };
    
    let source_file = args.get(2).ok_or("Missing source file")?;
    let program = parse_flow_file(source_file)?;
    
    let result = FlowCompilerBuilder::new()
        .target(target)
        .optimization_level(2)
        .compile_program(&program)?;
    
    match result {
        CompilerOutput::Object(bytes) => {
            std::fs::write("output.o", bytes)?;
            println!("Object file written to output.o");
        }
        CompilerOutput::Code(bytes) => {
            std::fs::write("Output.class", bytes)?;
            println!("Class file written to Output.class");
        }
        CompilerOutput::Jit(_) => {
            println!("Program compiled and executed");
        }
        _ => {}
    }
    
    Ok(())
}
```

### Build System Integration
```rust
pub struct FlowBuildSystem {
    compiler: Box<dyn FlowCompiler>,
    config: CompilerConfig,
}

impl FlowBuildSystem {
    pub fn new(target: CompilationTarget) -> Result<Self> {
        Ok(Self {
            compiler: CompilerFactory::create_compiler(target)?,
            config: CompilerConfig::new(target),
        })
    }
    
    pub fn build_project(&mut self, src_dir: &Path, out_dir: &Path) -> Result<()> {
        let mut modules = Vec::new();
        
        // Compile all .flow files in src_dir
        for entry in std::fs::read_dir(src_dir)? {
            let path = entry?.path();
            if path.extension().map_or(false, |ext| ext == "flow") {
                let program = parse_flow_file(&path)?;
                let module = self.compiler.compile_module(&program, &self.config)?;
                modules.push(module);
            }
        }
        
        // Link all modules
        let linked = self.compiler.link_modules(&modules, &self.config)?;
        
        // Write output
        match linked {
            CompilerOutput::Object(bytes) => {
                std::fs::write(out_dir.join("program"), bytes)?;
            }
            _ => return Err("Unexpected output type".into()),
        }
        
        Ok(())
    }
}
```

## Future Targets

The API is designed to be extensible. Future compilation targets may include:

- **Python**: Source-to-source transpilation
- **JavaScript**: Web platform support  
- **C**: Systems programming integration
- **Rust**: Memory-safe systems programming
- **WebAssembly**: High-performance web applications
- **LLVM IR**: Lower-level optimization opportunities

Each target will implement the same `FlowCompiler` trait, ensuring consistent usage patterns.

## Best Practices

1. **Use appropriate targets**: JIT for development, AOT for production
2. **Validate before compilation**: Call `validate_program()` to catch errors early
3. **Handle errors properly**: Different targets may fail in different ways
4. **Configure appropriately**: Higher optimization for production, debug info for development
5. **Leverage module system**: Compile libraries separately and link for better build times
6. **Test across targets**: Ensure your code works on all intended platforms