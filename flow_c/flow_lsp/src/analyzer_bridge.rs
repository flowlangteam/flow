use flow_analyzer::{AnalysisError, Analyzer, Severity as AnalysisSeverity};
use flow_ast::{Program, Span, Warning};
use flow_parser::Parser;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};
use std::collections::HashMap;

/// Bridge between Flow analyzer and LSP diagnostics
pub struct AnalyzerBridge {
    analyzer: Analyzer,
}

impl AnalyzerBridge {
    pub fn new() -> Self {
        Self {
            analyzer: Analyzer::new(),
        }
    }

    /// Analyze a document and return LSP diagnostics
    pub fn analyze_document(&mut self, _uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Parse the document
        let mut parser = Parser::new(text);
        match parser.parse() {
            Ok(program) => {
                // Analyze the parsed program
                match self.analyzer.analyze(&program) {
                    Ok(()) => {
                        // No errors, but check for warnings
                        for warning in self.analyzer.get_warnings() {
                            if let Some(diagnostic) = self.warning_to_diagnostic(warning, text) {
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                    Err(errors) => {
                        // Convert analysis errors to diagnostics
                        for error in errors {
                            if let Some(diagnostic) = self.error_to_diagnostic(&error, text) {
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                }
            }
            Err(parse_error) => {
                // Convert parse error to diagnostic
                let diagnostic = Diagnostic {
                    range: parse_error_to_range(&parse_error.span, text),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("flow-parser".to_string()),
                    message: parse_error.message,
                    related_information: None,
                    tags: None,
                    data: None,
                };
                diagnostics.push(diagnostic);
            }
        }

        diagnostics
    }

    /// Get completion suggestions for a position in the document
    pub fn get_completions(&mut self, text: &str, _position: Position) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Parse the document to get context
        let mut parser = Parser::new(text);
        if let Ok(program) = parser.parse() {
            // Analyze to get scope information
            if self.analyzer.analyze(&program).is_ok() {
                // Add keyword completions
                completions.extend(self.get_keyword_completions());
                
                // Add function completions
                completions.extend(self.get_function_completions(&program));
                
                // Add type completions
                completions.extend(self.get_type_completions(&program));
            }
        }

        completions
    }

    /// Get hover information for a position
    pub fn get_hover_info(&mut self, text: &str, _position: Position) -> Option<String> {
        let mut parser = Parser::new(text);
        if let Ok(program) = parser.parse() {
            if self.analyzer.analyze(&program).is_ok() {
                // Try to find what's at this position and provide type information
                // This is a simplified implementation - in reality, you'd need to
                // traverse the AST to find the node at the given position
                return Some("Type information would go here".to_string());
            }
        }
        None
    }

    /// Convert analysis error to LSP diagnostic
    fn error_to_diagnostic(&self, error: &AnalysisError, text: &str) -> Option<Diagnostic> {
        let severity = match error.severity {
            AnalysisSeverity::Error => DiagnosticSeverity::ERROR,
            AnalysisSeverity::Warning => DiagnosticSeverity::WARNING,
            AnalysisSeverity::Info => DiagnosticSeverity::INFORMATION,
            AnalysisSeverity::Hint => DiagnosticSeverity::HINT,
        };

        Some(Diagnostic {
            range: span_to_range(&error.span, text),
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some("flow-analyzer".to_string()),
            message: error.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    /// Convert warning to LSP diagnostic
    fn warning_to_diagnostic(&self, warning: &Warning, text: &str) -> Option<Diagnostic> {
        let (message, span) = match warning {
            Warning::UnusedVariable { name, span } => {
                (format!("Unused variable: {}", name), span)
            }
            Warning::UnusedFunction { name, span } => {
                (format!("Unused function: {}", name), span)
            }
            Warning::DeadCode { span } => {
                ("Dead code".to_string(), span)
            }
            Warning::UnnecessaryMut { name, span } => {
                (format!("Unnecessary mutable modifier for variable: {}", name), span)
            }
            Warning::PossibleMemoryLeak { span } => {
                ("Possible memory leak".to_string(), span)
            }
            Warning::UnsafeOperation { description, span } => {
                (format!("Unsafe operation: {}", description), span)
            }
            Warning::ImplicitConversion { from, to, span } => {
                (format!("Implicit conversion from {:?} to {:?}", from, to), span)
            }
            Warning::ShadowedVariable { name, span } => {
                (format!("Variable shadows previous declaration: {}", name), span)
            }
        };

        Some(Diagnostic {
            range: span_to_range(span, text),
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: Some("flow-analyzer".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        })
    }

    /// Get keyword completions
    fn get_keyword_completions(&self) -> Vec<CompletionItem> {
        use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

        let keywords = [
            "func", "struct", "impl", "let", "mut", "if", "else", "match", "return",
            "true", "false", "pub", "extern", "import", "use", "namespace",
            "lambda", "temp", "unsafe", "alloc", "free",
        ];

        keywords
            .iter()
            .map(|&keyword| CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some(format!("Flow keyword: {}", keyword)),
                ..Default::default()
            })
            .collect()
    }

    /// Get function completions from the program
    fn get_function_completions(&self, program: &Program) -> Vec<CompletionItem> {
        use flow_ast::Item;
        use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

        let mut completions = Vec::new();

        for item in &program.items {
            if let Item::Function(func) = item {
                let params_str = func
                    .params
                    .iter()
                    .map(|p| format!("{}: {:?}", p.name, p.ty))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type_str = func
                    .return_type
                    .as_ref()
                    .map(|t| format!(" -> {:?}", t))
                    .unwrap_or_default();

                completions.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!("func {}({}){}", func.name, params_str, return_type_str)),
                    insert_text: Some(format!("{}({})", func.name, 
                        func.params.iter().enumerate()
                            .map(|(i, p)| format!("${{{i}:{}}}", p.name, i = i + 1))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                    insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }
        }

        completions
    }

    /// Get type completions from the program
    fn get_type_completions(&self, program: &Program) -> Vec<CompletionItem> {
        use flow_ast::Item;
        use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

        let mut completions = Vec::new();

        // Built-in types
        let builtin_types = [
            "Int", "I8", "I16", "I32", "I64", "I128",
            "UInt", "U8", "U16", "U32", "U64", "U128",
            "Float", "F32", "F64",
            "Bool", "Char", "String", "Unit",
        ];

        for &type_name in &builtin_types {
            completions.push(CompletionItem {
                label: type_name.to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some(format!("Built-in type: {}", type_name)),
                ..Default::default()
            });
        }

        // User-defined structs
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                completions.push(CompletionItem {
                    label: struct_def.name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("struct {}", struct_def.name)),
                    ..Default::default()
                });
            }
        }

        completions
    }
}

/// Convert a Flow Span to an LSP Range
fn span_to_range(span: &Span, text: &str) -> Range {
    let start_pos = offset_to_position(span.start, text);
    let end_pos = offset_to_position(span.end, text);
    Range {
        start: start_pos,
        end: end_pos,
    }
}

/// Convert a parse error range to an LSP Range
fn parse_error_to_range(range: &std::ops::Range<usize>, text: &str) -> Range {
    let start_pos = offset_to_position(range.start, text);
    let end_pos = offset_to_position(range.end, text);
    Range {
        start: start_pos,
        end: end_pos,
    }
}

/// Convert byte offset to LSP Position
fn offset_to_position(offset: usize, text: &str) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;
    
    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    
    Position { line, character }
}

use tower_lsp::lsp_types::CompletionItem;

impl Default for AnalyzerBridge {
    fn default() -> Self {
        Self::new()
    }
}