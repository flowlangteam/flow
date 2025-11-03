# Contributing to Flow

Thank you for your interest in contributing to Flow! This guide will help you get started.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Making Changes](#making-changes)
- [Testing](#testing)
- [Code Style](#code-style)
- [Submitting Changes](#submitting-changes)
- [Project Structure](#project-structure)

## Code of Conduct

- Be respectful and inclusive
- Welcome newcomers and help them learn
- Focus on constructive feedback
- Assume good intentions

## Getting Started

### Prerequisites

- **Rust** 1.70+ (install via [rustup](https://rustup.rs/))
- **JDK** 11+ (for Java interop work)
- **Git**
- **Gradle** or **Maven** (optional, for Java components)

### Fork and Clone

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR_USERNAME/flow.git
cd flow
git remote add upstream https://github.com/flow-lang/flow.git
```

## Development Setup

### Build the Project

```bash
cd flow_c
cargo build --workspace
```

### Run Tests

```bash
cargo test --workspace
```

### Build Specific Components

```bash
# CLI
cargo build -p flow_cli

# JIT codegen
cargo build -p flow_codegen

# Java transpiler
cargo build -p flow_transpiler_java

# JNI bridge
cargo build -p flow_jni
```

## Making Changes

### 1. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-number
```

### 2. Make Your Changes

Focus on one feature or fix per branch. Keep commits atomic and well-documented.

### 3. Write Tests

All new features **must** include tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_your_feature() {
        // Arrange
        let input = ...;
        
        // Act
        let result = your_function(input);
        
        // Assert
        assert_eq!(result, expected);
    }
}
```

### 4. Update Documentation

- Update README.md if adding user-facing features
- Add doc comments to public APIs
- Update examples/ if relevant

## Testing

### Run All Tests

```bash
cargo test --workspace
```

### Test Specific Component

```bash
cargo test -p flow_parser
cargo test -p flow_codegen
cargo test -p flow_transpiler_java
```

### Test with Output

```bash
cargo test -- --nocapture --test-threads=1
```

### Integration Testing

Test the CLI with example files:

```bash
# Build CLI
cargo build -p flow_cli --release

# Test transpilation
./target/release/flow transpile --target java examples/functions.flow

# Verify output
javap -c Functions.class
```

## Code Style

### Rust Style

We follow standard Rust conventions:

```bash
# Format code
cargo fmt --all

# Lint code
cargo clippy --workspace -- -D warnings
```

**Guidelines:**

- Use descriptive variable names
- Prefer `match` over complex `if/else` chains
- Add `#[derive(Debug)]` to structs when possible
- Use `Result` for fallible operations
- Document public APIs with `///` comments
- Keep functions focused and small

**Example:**

```rust
/// Compiles a Flow expression to Cranelift IR
///
/// # Arguments
/// * `expr` - The expression to compile
///
/// # Returns
/// A Cranelift `Value` representing the compiled expression
///
/// # Errors
/// Returns a `String` error if compilation fails
pub fn compile_expr(&mut self, expr: &Expression) -> Result<Value, String> {
    match expr {
        Expression::Integer(n) => {
            Ok(self.builder.ins().iconst(types::I64, *n))
        }
        // ... more cases
    }
}
```

### Java Style

For Java interop code:

- Follow standard Java naming (camelCase for methods, PascalCase for classes)
- Use Javadoc for public APIs
- Keep classes focused on single responsibility

## Submitting Changes

### 1. Commit Your Changes

Write clear commit messages:

```bash
git commit -m "feat: add pipe operator support to parser"
git commit -m "fix: correct constant pool deduplication in transpiler"
git commit -m "docs: update Java interop setup guide"
```

**Commit message format:**

- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation only
- `test:` - Adding/updating tests
- `refactor:` - Code restructuring
- `perf:` - Performance improvement
- `chore:` - Build/tool changes

### 2. Push to Your Fork

```bash
git push origin feature/your-feature-name
```

### 3. Create a Pull Request

- Go to GitHub and create a PR from your fork
- Fill out the PR template with:
  - Description of changes
  - Related issue numbers (if any)
  - Testing performed
  - Breaking changes (if any)

### 4. Code Review

- Address reviewer feedback promptly
- Push new commits to your branch
- Re-request review when ready

### 5. Merge

Once approved, a maintainer will merge your PR!

## Project Structure

Understanding the codebase:

```
flow_c/
├── flow_cli/          # Command-line interface
│   └── src/main.rs    # Entry point, argument parsing
│
├── flow_lexer/        # Tokenization
│   └── src/lib.rs     # Token definitions, lexer implementation
│
├── flow_parser/       # Syntax analysis
│   └── src/lib.rs     # Recursive descent parser
│
├── flow_ast/          # Abstract syntax tree
│   └── src/lib.rs     # AST node definitions
│
├── flow_analyzer/     # Semantic analysis
│   └── src/lib.rs     # Type checking, validation
│
├── flow_codegen/      # JIT compilation
│   └── src/lib.rs     # Cranelift code generation
│
├── flow_transpiler/   # Base transpiler
│   ├── src/lib.rs     # Transpiler trait
│   └── flow_transpiler_java/
│       ├── src/lib.rs           # Java transpiler
│       ├── src/bytecode.rs      # JVM opcodes
│       ├── src/constant_pool.rs # JVM constant pool
│       └── src/class_writer.rs  # .class file writer
│
└── flow_jni/          # Java interop
    └── src/lib.rs     # JNI bindings
```

## Areas for Contribution

### 🐛 Bug Fixes

Check [issues labeled "bug"](https://github.com/flow-lang/flow/labels/bug)

### ✨ New Features

- Standard library functions
- New transpiler targets (Python, JavaScript, C, etc.)
- Language features (generics, traits, async, etc.)
- IDE integration (LSP, syntax highlighting)

### 📚 Documentation

- Improve README examples
- Add language tutorials
- Write API documentation
- Create video tutorials

### 🧪 Testing

- Add test cases for edge cases
- Improve test coverage
- Add benchmark tests
- Create integration tests

### 🎨 Examples

- Add example programs
- Create project templates
- Write best practices guide

## Getting Help

- **Questions?** Open a [discussion](https://github.com/flow-lang/flow/discussions)
- **Bug?** Open an [issue](https://github.com/flow-lang/flow/issues)
- **Chat?** Join our [Discord](https://discord.gg/flow-lang) (if available)

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Recognition

Contributors will be:

- Listed in the README
- Mentioned in release notes
- Credited in commit history
- Part of building something awesome! 🚀

Thank you for contributing to Flow!
