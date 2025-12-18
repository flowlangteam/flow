# Flow

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Publish to Crates.io](https://github.com/flowlangteam/flow/actions/workflows/publish-crates.yml/badge.svg)](https://github.com/flowlangteam/flow/actions/workflows/publish-crates.yml)
[![Build Flow](https://github.com/flowlangteam/flow/actions/workflows/build.yml/badge.svg)](https://github.com/flowlangteam/flow/actions/workflows/build.yml)
[![Rust Version](https://img.shields.io/badge/rust-stable-blue.svg)](https://www.rust-lang.org)

A programming language with JIT compilation, transpilation, and a language server.

## What is Flow?

Flow is a compiled programming language that runs your code fast. It has:

- JIT compiler for quick execution
- AOT compiler for native binaries
- Transpiler to Java and other languages
- Language server for editor support
- Standard library with common tools

## Quick Start

### Install

```bash
git clone https://github.com/flowlangteam/flow
cd flow/flow_c
cargo build --release
```

The compiler will be at `target/release/flow`.

### Run Your First Program

Create `hello.flow`:

```flow
func main() -> i64 {
    return 42;
}
```

Run it:

```bash
flow run hello.flow
```

## Usage

### Run a program

```bash
flow run myfile.flow
```

### Build to native binary

```bash
flow build myfile.flow -o myprogram
```

### Transpile to Java

```bash
flow transpile myfile.flow -t java -o MyProgram.java
```

### Check for errors

```bash
flow check myfile.flow
```

### Start REPL

```bash
flow repl
```

## Examples

### Functions

```flow
func add(x: i64, y: i64) -> i64 {
    return x + y;
}

func main() -> i64 {
    return add(5, 3);
}
```

### Loops

```flow
func main() -> i64 {
    let mut sum = 0;
    for i in 0..10 {
        sum = sum + i;
    }
    return sum;
}
```

### Structs

```flow
struct Point {
    x: i64,
    y: i64,
}

func main() -> i64 {
    let p = Point { x: 10, y: 20 };
    return p.x + p.y;
}
```

## Editor Support

The language server provides:

- Syntax highlighting
- Error checking
- Code completion
- Go to definition
- Find references
- Hover information

Build the LSP server:

```bash
cd flow_c
cargo build --package flow_lsp --release
```

The binary will be at `target/release/flow-lsp`.

## Java Interop

Flow can run on the JVM and call Java code. See [`interop/java/`](interop/java/) for details.

## Building

### Requirements

- Rust toolchain (latest stable)
- Cargo

### Build all components

```bash
cd flow_c
cargo build --release
```

### Build specific component

```bash
cargo build --package flow_cli --release
cargo build --package flow_lsp --release
```

## Testing

```bash
cd flow_c
cargo test
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Contributing

Issues and pull requests are welcome.
