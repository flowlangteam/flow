use flow_ast::*;
use flow_parser::Parser;
use flow_transpiler::{SymbolCollector, TranspileContext, TypeInferencer};
use std::collections::HashMap;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, Position, Range,
};

/// Bridge between the analyzer and LSP
pub struct AnalyzerBridge {
    /// Cached parse results
    _ast_cache: HashMap<String, Program>,
    /// Symbol context cache
    _context_cache: HashMap<String, TranspileContext>,
}

impl AnalyzerBridge {
    pub fn new() -> Self {
        Self {
            _ast_cache: HashMap::new(),
            _context_cache: HashMap::new(),
        }
    }

    /// Analyze a document and return diagnostics
    pub fn analyze_document(
        &mut self,
        _uri: &tower_lsp::lsp_types::Url,
        text: &str,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Parse the document
        let mut parser = Parser::new(text);
        match parser.parse() {
            Ok(program) => {
                // Collect symbols and check for errors
                let mut collector = SymbolCollector::new();
                collector.collect(&program);
                let context = collector.into_context();

                // Check for semantic errors
                self.validate_program(&program, &context, &mut diagnostics);
            }
            Err(err) => {
                // Parser error
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(tower_lsp::lsp_types::NumberOrString::String(
                        "E0001".to_string(),
                    )),
                    message: format!("Parse error: {:?}", err),
                    source: Some("flow".to_string()),
                    ..Default::default()
                });
            }
        }

        diagnostics
    }

    /// Validate the program for semantic errors
    fn validate_program(
        &self,
        program: &Program,
        context: &TranspileContext,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        // Check for duplicate definitions
        self.check_duplicates(program, diagnostics);

        // Validate functions
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.validate_function(func, context, diagnostics);
                }
                Item::Struct(struct_def) => {
                    self.validate_struct(struct_def, diagnostics);
                }
                Item::Impl(impl_block) => {
                    self.validate_impl(impl_block, context, diagnostics);
                }
                Item::ExternBlock(block) => {
                    self.validate_extern_block(block, diagnostics);
                }
                _ => {}
            }
        }
    }

    /// Check for duplicate definitions
    fn check_duplicates(&self, program: &Program, diagnostics: &mut Vec<Diagnostic>) {
        let mut function_names = HashMap::new();
        let mut struct_names = HashMap::new();

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    if let Some(prev_span) = function_names.insert(&func.name, func.span.clone()) {
                        diagnostics.push(self.create_diagnostic(
                            DiagnosticSeverity::ERROR,
                            func.span.clone(),
                            "E0008",
                            format!("Duplicate function definition: '{}'", func.name),
                        ));
                        diagnostics.push(self.create_diagnostic(
                            DiagnosticSeverity::HINT,
                            prev_span,
                            "E0008",
                            "Previous definition here".to_string(),
                        ));
                    }
                }
                Item::Struct(struct_def) => {
                    if struct_names.contains_key(&struct_def.name) {
                        diagnostics.push(self.create_diagnostic(
                            DiagnosticSeverity::ERROR,
                            Span::new(0, 0),
                            "E0008",
                            format!("Duplicate struct definition: '{}'", struct_def.name),
                        ));
                    }
                    struct_names.insert(&struct_def.name, ());
                }
                _ => {}
            }
        }
    }

    /// Validate a function
    fn validate_function(
        &self,
        func: &Function,
        context: &TranspileContext,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        // Validate the function body
        let mut locals = HashMap::new();
        for param in &func.params {
            locals.insert(param.name.clone(), param.ty.clone());
        }

        self.validate_expr(&func.body, context, &locals, diagnostics);

        // Check return type matches
        if let Some(expected_return) = &func.return_type {
            let inferencer = TypeInferencer::new(context);
            if let Some(actual_return) = inferencer.infer_expr(&func.body, &locals)
                && expected_return != &actual_return
                && actual_return != Type::Unit
            {
                diagnostics.push(self.create_diagnostic(
                    DiagnosticSeverity::ERROR,
                    func.span.clone(),
                    "E0002",
                    format!(
                        "Function '{}' expected to return {:?}, but returns {:?}",
                        func.name, expected_return, actual_return
                    ),
                ));
            }
        }
    }

    /// Validate a struct
    fn validate_struct(&self, struct_def: &Struct, diagnostics: &mut Vec<Diagnostic>) {
        let mut field_names = HashMap::new();

        for field in &struct_def.fields {
            if field_names.contains_key(&field.name) {
                diagnostics.push(self.create_diagnostic(
                    DiagnosticSeverity::ERROR,
                    Span::new(0, 0),
                    "E0008",
                    format!(
                        "Duplicate field '{}' in struct '{}'",
                        field.name, struct_def.name
                    ),
                ));
            }
            field_names.insert(&field.name, ());
        }
    }

    /// Validate an impl block
    fn validate_impl(
        &self,
        impl_block: &Impl,
        context: &TranspileContext,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        // Check that the struct exists
        if context.get_struct(&impl_block.struct_name).is_none() {
            diagnostics.push(self.create_diagnostic(
                DiagnosticSeverity::ERROR,
                Span::new(0, 0),
                "E0005",
                format!("Struct '{}' not found", impl_block.struct_name),
            ));
            return;
        }

        // Validate each method
        for method in &impl_block.methods {
            self.validate_function(method, context, diagnostics);
        }
    }

    /// Validate an extern block
    fn validate_extern_block(&self, block: &ExternBlock, diagnostics: &mut Vec<Diagnostic>) {
        // Validate the language specifier
        let supported_langs = ["C", "Java", "Python", "JavaScript"];
        if !supported_langs.contains(&block.lang.as_str()) {
            diagnostics.push(self.create_diagnostic(
                DiagnosticSeverity::WARNING,
                Span::new(0, 0),
                "W0007",
                format!(
                    "Unsupported extern language: '{}'. Supported: {:?}",
                    block.lang, supported_langs
                ),
            ));
        }

        // Check for duplicate extern function names
        let mut extern_names = HashMap::new();
        for item in &block.items {
            if extern_names.contains_key(&item.name) {
                diagnostics.push(self.create_diagnostic(
                    DiagnosticSeverity::ERROR,
                    Span::new(0, 0),
                    "E0008",
                    format!("Duplicate extern function: '{}'", item.name),
                ));
            }
            extern_names.insert(&item.name, ());
        }
    }

    /// Validate an expression
    fn validate_expr(
        &self,
        expr: &Expr,
        context: &TranspileContext,
        locals: &HashMap<String, Type>,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match expr {
            Expr::Ident(name) => {
                // Check if variable is defined
                if !locals.contains_key(name)
                    && context.get_function(name).is_none()
                    && context.get_extern_function(name).is_none()
                {
                    diagnostics.push(self.create_diagnostic(
                        DiagnosticSeverity::ERROR,
                        Span::new(0, 0),
                        "E0003",
                        format!("Undefined variable or function: '{}'", name),
                    ));
                }
            }
            Expr::Call { func, args } => {
                // Validate function exists
                if let Expr::Ident(func_name) = func.as_ref() {
                    if let Some(signature) = context.get_function(func_name) {
                        // Check argument count
                        if args.len() != signature.params.len() {
                            diagnostics.push(self.create_diagnostic(
                                DiagnosticSeverity::ERROR,
                                Span::new(0, 0),
                                "E0006",
                                format!(
                                    "Function '{}' expects {} arguments, but {} were provided",
                                    func_name,
                                    signature.params.len(),
                                    args.len()
                                ),
                            ));
                        }
                    } else if let Some(extern_info) = context.get_extern_function(func_name) {
                        // Check argument count for extern functions
                        if args.len() != extern_info.params.len() {
                            diagnostics.push(self.create_diagnostic(
                                DiagnosticSeverity::ERROR,
                                Span::new(0, 0),
                                "E0006",
                                format!(
                                    "Extern function '{}' expects {} arguments, but {} were provided",
                                    func_name,
                                    extern_info.params.len(),
                                    args.len()
                                ),
                            ));
                        }
                    } else {
                        diagnostics.push(self.create_diagnostic(
                            DiagnosticSeverity::ERROR,
                            Span::new(0, 0),
                            "E0004",
                            format!("Function '{}' not found", func_name),
                        ));
                    }
                }

                // Validate arguments
                for arg in args {
                    self.validate_expr(arg, context, locals, diagnostics);
                }
            }
            Expr::Field { expr, field } => {
                self.validate_expr(expr, context, locals, diagnostics);

                // Check if field exists on struct
                let inferencer = TypeInferencer::new(context);
                if let Some(Type::Named(struct_name)) = inferencer.infer_expr(expr, locals)
                    && let Some(struct_info) = context.get_struct(&struct_name)
                {
                    let field_exists = struct_info.fields.iter().any(|(name, _, _)| name == field);
                    if !field_exists {
                        diagnostics.push(self.create_diagnostic(
                            DiagnosticSeverity::ERROR,
                            Span::new(0, 0),
                            "E0011",
                            format!("Field '{}' not found on struct '{}'", field, struct_name),
                        ));
                    }
                }
            }
            Expr::Method { expr, method, args } => {
                self.validate_expr(expr, context, locals, diagnostics);

                // Check if method exists on struct
                let inferencer = TypeInferencer::new(context);
                if let Some(Type::Named(struct_name)) = inferencer.infer_expr(expr, locals)
                    && context.get_method(&struct_name, method).is_none()
                {
                    diagnostics.push(self.create_diagnostic(
                        DiagnosticSeverity::ERROR,
                        Span::new(0, 0),
                        "E0004",
                        format!("Method '{}' not found on struct '{}'", method, struct_name),
                    ));
                }

                // Validate arguments
                for arg in args {
                    self.validate_expr(arg, context, locals, diagnostics);
                }
            }
            Expr::StructInit { name, fields } => {
                // Check if struct exists
                if let Some(struct_info) = context.get_struct(name) {
                    // Check that all required fields are initialized
                    for (field_name, _, _) in &struct_info.fields {
                        if !fields.contains_key(field_name) {
                            diagnostics.push(self.create_diagnostic(
                                DiagnosticSeverity::ERROR,
                                Span::new(0, 0),
                                "E0012",
                                format!(
                                    "Missing field '{}' in struct '{}' initialization",
                                    field_name, name
                                ),
                            ));
                        }
                    }

                    // Check for unknown fields
                    for field_name in fields.keys() {
                        let field_exists = struct_info
                            .fields
                            .iter()
                            .any(|(fname, _, _)| fname == field_name);
                        if !field_exists {
                            diagnostics.push(self.create_diagnostic(
                                DiagnosticSeverity::ERROR,
                                Span::new(0, 0),
                                "E0011",
                                format!("Unknown field '{}' in struct '{}'", field_name, name),
                            ));
                        }
                    }

                    // Validate field expressions
                    for expr_val in fields.values() {
                        self.validate_expr(expr_val, context, locals, diagnostics);
                    }
                } else {
                    diagnostics.push(self.create_diagnostic(
                        DiagnosticSeverity::ERROR,
                        Span::new(0, 0),
                        "E0005",
                        format!("Struct '{}' not found", name),
                    ));
                }
            }
            Expr::Let { value, then, .. } => {
                self.validate_expr(value, context, locals, diagnostics);
                self.validate_expr(then, context, locals, diagnostics);
            }
            Expr::Binary { left, right, .. } => {
                self.validate_expr(left, context, locals, diagnostics);
                self.validate_expr(right, context, locals, diagnostics);
            }
            Expr::Unary { expr: inner, .. } => {
                self.validate_expr(inner, context, locals, diagnostics);
            }
            Expr::If { cond, then, else_ } => {
                self.validate_expr(cond, context, locals, diagnostics);
                self.validate_expr(then, context, locals, diagnostics);
                if let Some(else_expr) = else_ {
                    self.validate_expr(else_expr, context, locals, diagnostics);
                }
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.validate_expr(expr, context, locals, diagnostics);
                }
            }
            Expr::Match {
                expr: match_expr,
                arms,
            } => {
                self.validate_expr(match_expr, context, locals, diagnostics);
                for arm in arms {
                    self.validate_expr(&arm.body, context, locals, diagnostics);
                }
            }
            Expr::Pipe { left, right } => {
                self.validate_expr(left, context, locals, diagnostics);
                self.validate_expr(right, context, locals, diagnostics);
            }
            Expr::Lambda { body, .. } => {
                self.validate_expr(body, context, locals, diagnostics);
            }
            Expr::Return(Some(inner_expr)) => {
                self.validate_expr(inner_expr, context, locals, diagnostics);
            }
            Expr::Return(None) => {}
            _ => {}
        }
    }

    /// Get completion items for a position
    pub fn get_completions(&self, text: &str, _position: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Parse to get context
        let mut parser = Parser::new(text);
        if let Ok(program) = parser.parse() {
            let mut collector = SymbolCollector::new();
            collector.collect(&program);
            let context = collector.into_context();

            // Add functions
            for (name, sig) in &context.functions {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!(
                        "func {}({}) -> {:?}",
                        name,
                        sig.params
                            .iter()
                            .map(|(n, t)| format!("{}: {:?}", n, t))
                            .collect::<Vec<_>>()
                            .join(", "),
                        sig.return_type.as_ref().unwrap_or(&Type::Unit)
                    )),
                    ..Default::default()
                });
            }

            // Add structs
            for name in context.structs.keys() {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("struct {}", name)),
                    ..Default::default()
                });
            }

            // Add extern functions
            for (name, info) in &context.extern_functions {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!(
                        "extern \"{}\" func {}({}) -> {:?}",
                        info.lang,
                        name,
                        info.params
                            .iter()
                            .map(|t| format!("{:?}", t))
                            .collect::<Vec<_>>()
                            .join(", "),
                        info.return_type.as_ref().unwrap_or(&Type::Unit)
                    )),
                    ..Default::default()
                });
            }

            // Add keywords
            for keyword in &[
                "func",
                "struct",
                "impl",
                "extern",
                "let",
                "mut",
                "if",
                "else",
                "match",
                "return",
                "pub",
                "use",
                "namespace",
                "unsafe",
                "true",
                "false",
            ] {
                items.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }
        }

        items
    }

    /// Get hover information for a symbol
    pub fn get_hover_info(&self, text: &str, _position: Position, word: &str) -> Option<String> {
        let mut parser = Parser::new(text);
        if let Ok(program) = parser.parse() {
            let mut collector = SymbolCollector::new();
            collector.collect(&program);
            let context = collector.into_context();

            // Check if it's a function
            if let Some(sig) = context.get_function(word) {
                return Some(format!(
                    "func {}({}) -> {:?}\n\n{}",
                    word,
                    sig.params
                        .iter()
                        .map(|(n, t)| format!("{}: {:?}", n, t))
                        .collect::<Vec<_>>()
                        .join(", "),
                    sig.return_type.as_ref().unwrap_or(&Type::Unit),
                    if sig.is_pub { "public" } else { "private" }
                ));
            }

            // Check if it's an extern function
            if let Some(info) = context.get_extern_function(word) {
                return Some(format!(
                    "extern \"{}\" func {}({}) -> {:?}",
                    info.lang,
                    word,
                    info.params
                        .iter()
                        .map(|t| format!("{:?}", t))
                        .collect::<Vec<_>>()
                        .join(", "),
                    info.return_type.as_ref().unwrap_or(&Type::Unit)
                ));
            }

            // Check if it's a struct
            if let Some(struct_info) = context.get_struct(word) {
                return Some(format!(
                    "struct {} {{\n{}\n}}",
                    word,
                    struct_info
                        .fields
                        .iter()
                        .map(|(n, t, p)| format!(
                            "    {}{}: {:?}",
                            if *p { "pub " } else { "" },
                            n,
                            t
                        ))
                        .collect::<Vec<_>>()
                        .join(",\n")
                ));
            }
        }

        None
    }

    /// Create a diagnostic
    fn create_diagnostic(
        &self,
        severity: DiagnosticSeverity,
        span: Span,
        code: &str,
        message: String,
    ) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position {
                    line: (span.start / 100) as u32,
                    character: (span.start % 100) as u32,
                },
                end: Position {
                    line: (span.end / 100) as u32,
                    character: (span.end % 100) as u32,
                },
            },
            severity: Some(severity),
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                code.to_string(),
            )),
            message,
            source: Some("flow".to_string()),
            ..Default::default()
        }
    }
}

impl Default for AnalyzerBridge {
    fn default() -> Self {
        Self::new()
    }
}
