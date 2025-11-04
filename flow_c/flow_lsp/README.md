# Flow Language Server

A Language Server Protocol (LSP) implementation for the Flow programming language.

## Features

The Flow Language Server provides the following features:

### Core Language Support
- **Syntax highlighting** - Full tokenization and parsing support
- **Error diagnostics** - Real-time syntax and semantic error checking
- **Document synchronization** - Tracks changes to Flow source files

### Code Intelligence
- **Hover information** - Shows type information and documentation
- **Code completion** - Intelligent suggestions for:
  - Keywords (`func`, `struct`, `let`, `mut`, etc.)
  - Functions and their signatures
  - Built-in and user-defined types
  - Variables in scope
- **Go to definition** - Navigate to function and struct definitions
- **Find references** - Find all usages of symbols
- **Document symbols** - Outline view of functions and structures

### Advanced Features
- **Multi-file support** - Handle imports and cross-file references
- **Type checking** - Real-time semantic analysis
- **Memory safety warnings** - Detect potential memory leaks and unsafe operations

## Installation

### Building from Source

```bash
cd flow_c
cargo build --package flow_lsp --release
```

The binary will be available at `target/release/flow-lsp`.

### Using with VS Code

To use the Flow Language Server with Visual Studio Code:

1. Build the language server binary as shown above
2. Create a VS Code extension or configure your editor to use the binary
3. The language server communicates over stdin/stdout using JSON-RPC

Example VS Code settings:

```json
{
  "languageServerExample.serverPath": "/path/to/flow-lsp"
}
```

### Using with Neovim

For Neovim with nvim-lspconfig:

```lua
local lspconfig = require('lspconfig')

local configs = require('lspconfig.configs')
if not configs.flow_lsp then
  configs.flow_lsp = {
    default_config = {
      cmd = { '/path/to/flow-lsp' },
      filetypes = { 'flow' },
      root_dir = lspconfig.util.root_pattern('.git'),
      settings = {},
    },
  }
end

lspconfig.flow_lsp.setup{}
```

### Using with Other Editors

The Flow Language Server follows the standard LSP specification and should work with any editor that supports LSP, including:

- **Emacs** (with lsp-mode)
- **Vim** (with vim-lsp or coc.nvim)
- **Sublime Text** (with LSP package)
- **Atom** (with atom-ide-ui)

## Protocol Support

The Flow Language Server implements the following LSP methods:

### Lifecycle
- `initialize` - Server initialization
- `initialized` - Server ready notification
- `shutdown` - Clean server shutdown

### Document Synchronization
- `textDocument/didOpen` - Document opened
- `textDocument/didChange` - Document changed
- `textDocument/didClose` - Document closed

### Language Features
- `textDocument/hover` - Hover information
- `textDocument/completion` - Code completion
- `textDocument/definition` - Go to definition
- `textDocument/references` - Find references
- `textDocument/documentSymbol` - Document symbols
- `textDocument/publishDiagnostics` - Error and warning diagnostics

## Configuration

The language server can be configured through initialization options:

```json
{
  "initializationOptions": {
    "trace": "verbose",
    "logLevel": "info"
  }
}
```

## Logging

The language server uses structured logging via the `tracing` crate. Set the `RUST_LOG` environment variable to control log levels:

```bash
RUST_LOG=flow_lsp=debug flow-lsp
```

Available log levels:
- `error` - Only errors
- `warn` - Warnings and errors
- `info` - General information (default)
- `debug` - Detailed debugging information
- `trace` - Very verbose tracing

## Architecture

The Flow Language Server is built with the following components:

- **Document Manager** - Tracks open files and their contents using efficient rope data structures
- **Analyzer Bridge** - Connects the existing Flow analyzer with LSP diagnostics
- **Handlers** - Implements LSP request/response handlers
- **Server** - Main LSP server implementation using tower-lsp

The server reuses the existing Flow compiler infrastructure:
- `flow_lexer` - Tokenization
- `flow_parser` - Syntax parsing  
- `flow_ast` - Abstract syntax tree representation
- `flow_analyzer` - Semantic analysis and type checking

## Development

### Running Tests

```bash
cargo test --package flow_lsp
```

### Contributing

When contributing to the language server:

1. Follow the existing code style and patterns
2. Add tests for new functionality
3. Update documentation for new features
4. Ensure all existing tests pass

### Adding New Features

To add a new LSP feature:

1. Add the method to the `LanguageServer` trait implementation in `server.rs`
2. Implement the handler logic in `handlers.rs`
3. Add any necessary bridge code in `analyzer_bridge.rs`
4. Update this README with the new capability

## License

The Flow Language Server is part of the Flow programming language project and shares the same license.