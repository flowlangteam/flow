# Flow Programming Language

A modern, high-performance programming language with JIT compilation and multi-target transpilation.

![Build Status](https://github.com/flowlangteam/flow/workflows/Build%20Flow/badge.svg)
![Tests](https://github.com/flowlangteam/flow/workflows/Tests/badge.svg)

## Features

- 🚀 **JIT Compilation** - Native code generation with Cranelift
- 🔄 **Multi-target Transpilation** - Compile to JVM bytecode, JavaScript (TODO), etc.
- 🔗 **Java Interop** - Bidirectional FFI with Java via JNI
- 📦 **Modern Syntax** - Structs, pattern matching, lambdas, pipe operators
- ⚡ **Performance** - Zero-cost abstractions and optimized codegen
- 🛡️ **Memory Safety** - Safe by default with explicit unsafe blocks

## Quick Start

### Installation

Download the latest release for your platform:

```bash
# Linux
wget https://github.com/flowlangteam/flow/releases/latest/download/flow-linux-x64.tar.gz
tar -xzf flow-linux-x64.tar.gz
sudo mv release/bin/flow /usr/local/bin/

# macOS
curl -L https://github.com/flowlangteam/flow/releases/latest/download/flow-macos-x64.tar.gz | tar -xz
sudo mv release/bin/flow /usr/local/bin/

# Windows
# Download flow-windows-x64.zip and extract to a folder in your PATH
```

Or build from source:

```bash
cd flow_c
cargo build --release -p flow_cli
# Binary at: target/release/flow
```

### Hello World

```flow
func main() {
    println("Hello, Flow!")
}
```

Run it:

```bash
flow run hello.flow
```

## Usage Examples

### Native Execution (JIT)

```flow
func fibonacci(n: Int) -> Int {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

func main() {
    let result = fibonacci(10)
    println(result)
}
```

```bash
flow run fibonacci.flow
```

### Transpile to Java Bytecode

```flow
func add(a: Int, b: Int) -> Int {
    a + b
}

func multiply(a: Int, b: Int) -> Int {
    a * b
}
```

```bash
flow transpile --target java math.flow
# Generates: Math.class

# Use from Java:
javap -c Math.class
```

### Pipe Operator

```flow
func double(x: Int) -> Int { x * 2 }
func square(x: Int) -> Int { x * x }
func negate(x: Int) -> Int { -x }

func main() {
    let result = 5 |> double |> square |> negate
    println(result)  # Output: -100
}
```

### Pattern Matching

```flow
func describe(value: Int) -> String {
    match value {
        0 => "zero",
        1 => "one",
        _ => "many"
    }
}
```

### Structs and Methods

```flow
struct Point {
    x: Int,
    y: Int
}

impl Point {
    func distance(self) -> Float {
        sqrt(self.x * self.x + self.y * self.y)
    }
}

func main() {
    let p = Point { x: 3, y: 4 }
    println(p.distance())  # Output: 5.0
}
```

## Project Structure

```
Flow/
├── flow_c/                    # Rust implementation
│   ├── flow_cli/              # CLI compiler/transpiler
│   ├── flow_lexer/            # Lexical analysis
│   ├── flow_parser/           # Syntax parsing
│   ├── flow_ast/              # Abstract syntax tree
│   ├── flow_analyzer/         # Semantic analysis
│   ├── flow_codegen/          # Cranelift JIT codegen
│   ├── flow_runtime/          # Runtime support
│   ├── flow_stdlib/           # Standard library
│   ├── flow_transpiler/       # Base transpiler trait
│   │   └── flow_transpiler_java/  # Java bytecode transpiler
│   ├── flow_jni/              # JNI bridge for Java interop
│   └── flow_lsp/              # Language Server Protocol implementation
├── interop/                   # Language interop
│   └── java/                  # Java integration
│       ├── src/flow/
│       │   ├── runtime/       # Module loading
│       │   ├── bridge/        # JNI bindings
│       │   └── examples/      # Usage examples
│       ├── lib/               # JAR and native libs
│       ├── build.gradle       # Gradle build
│       ├── pom.xml            # Maven build
│       └── README.md          # Java interop docs
├── examples/                  # Example Flow programs
└── .github/workflows/         # CI/CD pipelines
```

## Development

### Building

```bash
# Build all components
cd flow_c
cargo build --workspace --release

# Build specific components
cargo build -p flow_cli --release       # CLI only
cargo build -p flow_jni --release       # JNI library
cargo build -p flow_transpiler_java --release  # Java transpiler
```

### Testing

```bash
# Run all tests
cargo test --workspace

# Test specific components
cargo test -p flow_codegen
cargo test -p flow_transpiler_java
cargo test -p flow_jni

# Test with output
cargo test -- --nocapture
```

### Java Interop

Build the complete Java interop package:

```bash
cd interop/java

# Using Gradle
./build.sh

# Using Maven
./build.sh maven
```

See [interop/java/SETUP.md](interop/java/SETUP.md) for complete documentation.

## Roadmap

- [x] Lexer and Parser
- [x] AST and semantic analysis
- [x] Cranelift JIT compilation
- [x] Java bytecode transpilation
- [x] JNI bridge for Java interop
- [x] CI/CD pipelines
- [ ] Standard library expansion
- [ ] Python transpiler
- [ ] JavaScript transpiler
- [ ] Package manager
- [ ] Language server (LSP)
- [ ] Debugger integration

### Language Server

Flow includes a Language Server Protocol (LSP) implementation for editor integration:

```bash
cd flow_c
cargo build --package flow_lsp --release
```

The language server provides:
- Real-time error diagnostics
- Code completion and hover information
- Go-to-definition and find references
- Document symbols and outline view

Compatible with VS Code, Neovim, Emacs, and other LSP-enabled editors.

See [flow_c/flow_lsp/README.md](flow_c/flow_lsp/README.md) for setup instructions.

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Ensure `cargo test` passes
5. Run `cargo fmt` and `cargo clippy`
6. Submit a pull request

## License

MIT License - See LICENSE file for details

## Resources

- **Documentation**: [GitHub Wiki](https://github.com/flowlangteam/flow/wiki)
- **Examples**: [examples/](examples/)
- **Java Interop**: [interop/java/SETUP.md](interop/java/SETUP.md)
- **Issues**: [GitHub Issues](https://github.com/flowlangteam/flow/issues)

## Acknowledgments

Built with:

- [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift) - JIT compilation
- [Logos](https://github.com/maciejhirsz/logos) - Lexical analysis
- [Rust](https://www.rust-lang.org/) - Implementation language
