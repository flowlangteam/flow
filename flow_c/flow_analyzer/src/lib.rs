use flow_ast::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub struct Analyzer {
    warnings: Vec<Warning>,
    errors: Vec<AnalysisError>,
    scopes: Vec<Scope>,
    modules: HashMap<String, ModuleInfo>,
    current_namespace: Option<String>,
    module_search_paths: Vec<PathBuf>,
    parsed_modules: HashMap<String, Program>,
    current_source: Option<String>,
    current_file_path: Option<String>,
}

#[derive(Debug, Clone)]
struct ModuleInfo {
    namespace: String,
    filename: String,
    public_functions: HashMap<String, FunctionInfo>,
    public_structs: HashMap<String, StructInfo>,
    #[allow(dead_code)]
    file_path: PathBuf,
}

#[derive(Debug, Clone)]
struct StructInfo {
    name: String,
    fields: Vec<Field>,
    span: Span,
    methods: HashMap<String, FunctionInfo>,
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, VariableInfo>,
    functions: HashMap<String, FunctionInfo>,
    structs: HashMap<String, StructInfo>,
    temp_allocations: Vec<String>,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    ty: Type,
    mutable: bool,
    used: bool,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub used: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnalysisError {
    pub message: String,
    pub span: Span,
    pub severity: Severity,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            warnings: Vec::new(),
            errors: Vec::new(),
            scopes: vec![Scope::new()],
            modules: HashMap::new(),
            current_namespace: None,
            module_search_paths: Vec::new(),
            parsed_modules: HashMap::new(),
            current_source: None,
            current_file_path: None,
        }
    }

    pub fn add_module_search_path(&mut self, path: PathBuf) {
        self.module_search_paths.push(path);
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<AnalysisError>> {
        if let Some(namespace_decl) = &program.namespace {
            self.current_namespace = Some(format!(
                "{}::{}",
                namespace_decl.namespace, namespace_decl.filename
            ));
        }

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func)?;
                }
                Item::Struct(s) => {
                    self.declare_struct(s)?;
                }
                Item::Use(use_decl) => {
                    self.process_use_declaration(use_decl)?;
                }
                Item::Attribute(_) => {}
                Item::Import(_) | Item::ExternBlock(_) | Item::Impl(_) => {}
            }
        }

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.analyze_function(func, None)?;
                }
                Item::Impl(impl_block) => {
                    self.analyze_impl(impl_block)?;
                }
                Item::Attribute(_) => {}
                _ => {}
            }
        }

        self.check_unused();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn get_warnings(&self) -> &[Warning] {
        &self.warnings
    }

    pub fn get_errors(&self) -> &[AnalysisError] {
        &self.errors
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_info) = scope.variables.get(name) {
                return Some(format!(
                    "{} {}",
                    if var_info.mutable { "mut" } else { "let" },
                    format_type(&var_info.ty)
                ));
            }

            if let Some(func_info) = scope.functions.get(name) {
                let params_str = func_info
                    .params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, format_type(&p.ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_str = func_info
                    .return_type
                    .as_ref()
                    .map(|t| format!(" -> {}", format_type(t)))
                    .unwrap_or_default();
                return Some(format!("func {}({}){}", name, params_str, return_str));
            }

            if let Some(struct_info) = scope.structs.get(name) {
                let fields_str = struct_info
                    .fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, format_type(&f.ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                return Some(format!("struct {} {{ {} }}", name, fields_str));
            }
        }

        None
    }

    fn declare_function(&mut self, func: &Function) -> Result<(), Vec<AnalysisError>> {
        let info = FunctionInfo {
            params: func.params.clone(),
            return_type: func.return_type.clone(),
            used: func.name == "main",
            span: Span::new(0, 0),
        };

        if let Some(scope) = self.scopes.last_mut() {
            if scope.functions.contains_key(&func.name) {
                self.errors.push(AnalysisError {
                    message: format!("Function '{}' is already defined", func.name),
                    span: Span::new(0, 0),
                    severity: Severity::Error,
                });
            } else {
                scope.functions.insert(func.name.clone(), info);
            }
        }

        Ok(())
    }

    fn declare_struct(&mut self, s: &Struct) -> Result<(), Vec<AnalysisError>> {
        let struct_info = StructInfo {
            name: s.name.clone(),
            fields: s.fields.clone(),
            span: Span::new(0, 0),
            methods: HashMap::new(),
        };

        if let Some(scope) = self.scopes.last_mut() {
            if let Some(existing) = scope.structs.get(&s.name) {
                self.errors.push(AnalysisError {
                    message: format!("Struct '{}' is already defined", s.name),
                    span: existing.span.clone(),
                    severity: Severity::Error,
                });
            } else {
                scope.structs.insert(s.name.clone(), struct_info);
            }
        }

        Ok(())
    }

    fn process_use_declaration(&mut self, use_decl: &UseDecl) -> Result<(), Vec<AnalysisError>> {
        let module_key = format!("{}::{}", use_decl.namespace, use_decl.filename);

        if self.modules.contains_key(&module_key) {
            return Ok(());
        }

        let module_file_path = self.find_module_file(&use_decl.namespace, &use_decl.filename)?;

        let source = fs::read_to_string(&module_file_path).map_err(|e| {
            vec![AnalysisError {
                message: format!(
                    "Failed to read module file {}: {}",
                    module_file_path.display(),
                    e
                ),
                span: Span::new(0, 0),
                severity: Severity::Error,
            }]
        })?;

        let mut parser = flow_parser::Parser::new(&source);
        let module_program = parser.parse().map_err(|diagnostic| {
            let mut span = diagnostic
                .labels
                .iter()
                .find(|label| matches!(label.style, LabelStyle::Primary))
                .map(|label| label.span.clone())
                .unwrap_or_else(|| Span::new(0, 0));

            if span.file.is_none() {
                span.file = Some(module_file_path.to_string_lossy().to_string());
            }

            vec![AnalysisError {
                message: format!(
                    "Failed to parse module {}: {}",
                    module_key, diagnostic.message
                ),
                span,
                severity: Severity::Error,
            }]
        })?;

        if let Some(namespace_decl) = &module_program.namespace {
            let declared_key = format!("{}::{}", namespace_decl.namespace, namespace_decl.filename);
            if declared_key != module_key {
                return Err(vec![AnalysisError {
                    message: format!(
                        "Module namespace mismatch: expected '{}', found '{}'",
                        module_key, declared_key
                    ),
                    span: Span::new(0, 0),
                    severity: Severity::Error,
                }]);
            }
        } else {
            return Err(vec![AnalysisError {
                message: format!("Module '{}' must have a namespace declaration", module_key),
                span: Span::new(0, 0),
                severity: Severity::Error,
            }]);
        }

        let mut public_functions = HashMap::new();
        let mut public_structs = HashMap::new();

        for item in &module_program.items {
            match item {
                Item::Function(func) if func.is_pub => {
                    let func_info = FunctionInfo {
                        params: func.params.clone(),
                        return_type: func.return_type.clone(),
                        used: false,
                        span: func.span.clone(),
                    };
                    public_functions.insert(func.name.clone(), func_info);
                }
                Item::Struct(struct_def) if struct_def.is_pub => {
                    let struct_info = StructInfo {
                        name: struct_def.name.clone(),
                        fields: struct_def.fields.clone(),
                        span: Span::new(0, 0),
                        methods: HashMap::new(),
                    };
                    public_structs.insert(struct_def.name.clone(), struct_info);
                }
                _ => {}
            }
        }

        let module_info = ModuleInfo {
            namespace: use_decl.namespace.clone(),
            filename: use_decl.filename.clone(),
            public_functions,
            public_structs,
            file_path: module_file_path,
        };
        self.modules.insert(module_key.clone(), module_info);
        self.parsed_modules.insert(module_key, module_program);

        Ok(())
    }

    fn find_module_file(
        &self,
        namespace: &str,
        filename: &str,
    ) -> Result<PathBuf, Vec<AnalysisError>> {
        for search_path in &self.module_search_paths {
            let namespace_dir = search_path.join(namespace);
            let candidate = namespace_dir.join(format!("{}.flow", filename));
            if candidate.exists() {
                return Ok(candidate);
            }

            let module_filename = format!("{}_{}.flow", namespace, filename);
            let candidate = search_path.join(&module_filename);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err(vec![AnalysisError {
            message: format!(
                "Module file not found for '{}::{}' (searched for {}/{{filename}}.flow and {{namespace}}_{{filename}}.flow)",
                namespace, filename, namespace
            ),
            span: Span::new(0, 0),
            severity: Severity::Error,
        }])
    }

    pub fn resolve_namespaced_function(
        &self,
        namespace_alias: &str,
        function_name: &str,
    ) -> Option<&FunctionInfo> {
        for module_info in self.modules.values() {
            let module_namespace = module_info.namespace.as_str();
            let module_filename = module_info.filename.as_str();
            let module_key = format!("{}::{}", module_namespace, module_filename);
            if namespace_alias == module_namespace
                || namespace_alias == module_filename
                || namespace_alias == module_key
            {
                return module_info.public_functions.get(function_name);
            }
        }
        None
    }

    pub fn set_source_context(&mut self, source: String, file_path: String) {
        self.current_source = Some(source);
        self.current_file_path = Some(file_path);
    }

    fn estimate_expression_span(&self, expr: &Expr) -> Span {
        if let (Some(source), Some(file_path)) = (&self.current_source, &self.current_file_path) {
            match expr {
                Expr::Call { func, .. } => {
                    if let Expr::Ident(name) = func.as_ref() {
                        return self.find_identifier_span(source, name, file_path);
                    }
                }
                Expr::Ident(name) => {
                    return self.find_identifier_span(source, name, file_path);
                }
                _ => {}
            }
        }

        Span::new(0, 0)
    }

    fn find_identifier_span(&self, source: &str, name: &str, file_path: &str) -> Span {
        let mut matches = Vec::new();
        let mut start = 0;

        while let Some(pos) = source[start..].find(name) {
            let absolute_pos = start + pos;

            let is_word_start = absolute_pos == 0
                || !source
                    .chars()
                    .nth(absolute_pos - 1)
                    .unwrap_or(' ')
                    .is_alphanumeric();

            let is_word_end = absolute_pos + name.len() >= source.len()
                || !source
                    .chars()
                    .nth(absolute_pos + name.len())
                    .unwrap_or(' ')
                    .is_alphanumeric();

            if is_word_start && is_word_end {
                matches.push(absolute_pos);
            }

            start = absolute_pos + 1;
        }

        if let Some(&pos) = matches.first() {
            return Span {
                start: pos,
                end: pos + name.len(),
                file: Some(file_path.to_string()),
            };
        }

        Span::new(0, 0)
    }

    pub fn get_parsed_modules(&self) -> &HashMap<String, Program> {
        &self.parsed_modules
    }

    fn analyze_function(
        &mut self,
        func: &Function,
        self_struct: Option<&str>,
    ) -> Result<(), Vec<AnalysisError>> {
        self.push_scope();

        for param in &func.params {
            let resolved_ty = resolve_self_type(&param.ty, self_struct);
            self.declare_variable(&param.name, &resolved_ty, false, Span::new(0, 0))?;
        }

        let body_type = self.analyze_expr(&func.body)?;

        if let Some(ref expected_type) = func.return_type {
            let resolved_expected = resolve_self_type(expected_type, self_struct);
            if !self.types_compatible(&body_type, &resolved_expected) {
                self.errors.push(AnalysisError {
                    message: format!(
                        "Function '{}' returns {:?} but expected {:?}",
                        func.name, body_type, resolved_expected
                    ),
                    span: func.span.clone(),
                    severity: Severity::Error,
                });
            }
        }

        self.pop_scope();
        Ok(())
    }

    fn analyze_impl(&mut self, impl_block: &Impl) -> Result<(), Vec<AnalysisError>> {
        let struct_name = &impl_block.struct_name;

        if self.find_struct_info(struct_name).is_none() {
            self.errors.push(AnalysisError {
                message: format!("Impl block references unknown struct '{}'", struct_name),
                span: Span::new(0, 0),
                severity: Severity::Error,
            });
            return Ok(());
        }

        for method in &impl_block.methods {
            if let Some(existing) = self.find_struct_info(struct_name)
                && existing.methods.contains_key(&method.name)
            {
                self.errors.push(AnalysisError {
                    message: format!(
                        "Method '{}' is already defined for struct '{}'",
                        method.name, struct_name
                    ),
                    span: method.span.clone(),
                    severity: Severity::Error,
                });
                continue;
            }

            let resolved_params = method
                .params
                .iter()
                .map(|param| Param {
                    name: param.name.clone(),
                    ty: resolve_self_type(&param.ty, Some(struct_name)),
                })
                .collect();

            let resolved_return = method
                .return_type
                .as_ref()
                .map(|ty| resolve_self_type(ty, Some(struct_name)));

            if let Some(struct_info) = self.find_struct_info_mut(struct_name) {
                struct_info.methods.insert(
                    method.name.clone(),
                    FunctionInfo {
                        params: resolved_params,
                        return_type: resolved_return,
                        used: false,
                        span: method.span.clone(),
                    },
                );
            }
        }

        for method in &impl_block.methods {
            self.analyze_function(method, Some(struct_name))?;
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &Expr) -> Result<Type, Vec<AnalysisError>> {
        self.analyze_expr_with_span(expr, Span::new(0, 0))
    }

    fn analyze_expr_with_span(
        &mut self,
        expr: &Expr,
        expr_span: Span,
    ) -> Result<Type, Vec<AnalysisError>> {
        let span = if expr_span.start == 0 && expr_span.end == 0 {
            self.estimate_expression_span(expr)
        } else {
            expr_span
        };

        match expr {
            Expr::Integer(_) => Ok(Type::I64),
            Expr::Float(_) => Ok(Type::F64),
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::String),
            Expr::Unit => Ok(Type::Unit),

            Expr::Ident(name) => {
                if let Some(var_info) = self.find_variable(name) {
                    self.mark_variable_used(name);
                    Ok(var_info.ty.clone())
                } else {
                    self.errors.push(AnalysisError {
                        message: format!("Undefined variable '{}'", name),
                        span: Span::new(0, 0),
                        severity: Severity::Error,
                    });
                    Ok(Type::Unit)
                }
            }

            Expr::Binary { op, left, right } => {
                let left_type = self.analyze_expr(left)?;
                let right_type = self.analyze_expr(right)?;

                if !self.types_compatible(&left_type, &right_type) {
                    self.warnings.push(Warning::ImplicitConversion {
                        from: left_type.clone(),
                        to: right_type.clone(),
                        span: Span::new(0, 0),
                    });
                }

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => Ok(left_type),
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or => Ok(Type::Bool),
                }
            }

            Expr::Unary { op: _, expr } => self.analyze_expr(expr),

            Expr::Let {
                name,
                mutable,
                ty,
                value,
                then,
            } => {
                let value_type = self.analyze_expr(value)?;

                if let Some(annotated_type) = ty
                    && !self.types_compatible(&value_type, annotated_type)
                {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Type mismatch: expected {:?}, found {:?}",
                            annotated_type, value_type
                        ),
                        span: Span::new(0, 0),
                        severity: Severity::Error,
                    });
                }

                let var_type = ty.clone().unwrap_or(value_type);
                self.declare_variable(name, &var_type, *mutable, Span::new(0, 0))?;

                self.analyze_expr(then)
            }

            Expr::If { cond, then, else_ } => {
                let cond_type = self.analyze_expr(cond)?;
                if !matches!(cond_type, Type::Bool) {
                    self.errors.push(AnalysisError {
                        message: "Condition must be of type bool".to_string(),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                }

                let then_type = self.analyze_expr(then)?;
                let else_type = if let Some(else_expr) = else_ {
                    self.analyze_expr(else_expr)?
                } else {
                    Type::Unit
                };

                if !self.types_compatible(&then_type, &else_type) {
                    self.warnings.push(Warning::ImplicitConversion {
                        from: then_type.clone(),
                        to: else_type.clone(),
                        span: Span::new(0, 0),
                    });
                }

                Ok(then_type)
            }

            Expr::Block(exprs) => {
                let mut last_type = Type::Unit;
                for expr in exprs {
                    last_type = self.analyze_expr(expr)?;
                }
                Ok(last_type)
            }

            Expr::Call { func, args } => {
                if let Expr::Ident(name) = &**func {
                    if let Some(func_info) = self.find_function(name) {
                        self.mark_function_used(name);

                        if args.len() != func_info.params.len() {
                            self.errors.push(AnalysisError {
                                message: format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    name,
                                    func_info.params.len(),
                                    args.len()
                                ),
                                span: span.clone(),
                                severity: Severity::Error,
                            });
                        }

                        for (i, (arg, param)) in
                            args.iter().zip(func_info.params.iter()).enumerate()
                        {
                            let arg_type = self.analyze_expr(arg)?;
                            if !self.types_compatible(&arg_type, &param.ty) {
                                self.errors.push(AnalysisError {
                                    message: format!(
                                        "Argument {} has wrong type: expected {:?}, found {:?}",
                                        i + 1,
                                        param.ty,
                                        arg_type
                                    ),
                                    span: span.clone(),
                                    severity: Severity::Error,
                                });
                            }
                        }

                        Ok(func_info.return_type.clone().unwrap_or(Type::Unit))
                    } else {
                        let func_type = self.analyze_expr(func)?;
                        self.analyze_function_type_call(func_type, args.as_slice(), &span)
                    }
                } else {
                    let func_type = self.analyze_expr(func)?;
                    self.analyze_function_type_call(func_type, args.as_slice(), &span)
                }
            }

            Expr::StructInit { name, fields } => {
                let struct_info = if let Some(info) = self.find_struct_info(name) {
                    info
                } else {
                    self.errors.push(AnalysisError {
                        message: format!("Unknown struct '{}'", name),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Named(name.clone()));
                };

                for field_def in &struct_info.fields {
                    if let Some(field_expr) = fields.get(&field_def.name) {
                        let field_type = self.analyze_expr(field_expr)?;
                        if !self.types_compatible(&field_type, &field_def.ty) {
                            self.errors.push(AnalysisError {
                                message: format!(
                                    "Field '{}' expects type {:?}, found {:?}",
                                    field_def.name, field_def.ty, field_type
                                ),
                                span: span.clone(),
                                severity: Severity::Error,
                            });
                        }
                    } else {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Missing field '{}' in struct initializer for '{}' (defined at {:?})",
                                field_def.name, struct_info.name, struct_info.span
                            ),
                            span: span.clone(),
                            severity: Severity::Error,
                        });
                    }
                }

                for field_name in fields.keys() {
                    if !struct_info
                        .fields
                        .iter()
                        .any(|field| field.name == *field_name)
                    {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Unknown field '{}' in struct '{}' (defined at {:?})",
                                field_name, struct_info.name, struct_info.span
                            ),
                            span: span.clone(),
                            severity: Severity::Error,
                        });
                    }
                }

                Ok(Type::Named(struct_info.name.clone()))
            }

            Expr::Field { expr, field } => {
                let base_type = self.analyze_expr(expr)?;
                let struct_name = if let Some(name) = extract_struct_name(&base_type) {
                    name
                } else {
                    self.errors.push(AnalysisError {
                        message: "Field access requires a struct value".to_string(),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                };

                let struct_info = if let Some(info) = self.find_struct_info(&struct_name) {
                    info
                } else {
                    self.errors.push(AnalysisError {
                        message: format!("Unknown struct '{}'", struct_name),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                };

                if let Some(field_def) = struct_info.fields.iter().find(|def| def.name == *field) {
                    Ok(field_def.ty.clone())
                } else {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Struct '{}' has no field named '{}'",
                            struct_info.name, field
                        ),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    Ok(Type::Unit)
                }
            }

            Expr::Method {
                expr: receiver,
                method,
                args,
            } => {
                let receiver_type = self.analyze_expr(receiver)?;
                let struct_name = if let Some(name) = extract_struct_name(&receiver_type) {
                    name
                } else {
                    self.errors.push(AnalysisError {
                        message: "Method calls require a struct receiver".to_string(),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                };

                let struct_info = if let Some(info) = self.find_struct_info(&struct_name) {
                    info
                } else {
                    self.errors.push(AnalysisError {
                        message: format!("Unknown struct '{}'", struct_name),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                };

                let method_info = if let Some(info) = struct_info.methods.get(method) {
                    info.clone()
                } else {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Struct '{}' has no method named '{}'",
                            struct_info.name, method
                        ),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                };

                if method_info.params.is_empty() {
                    self.errors.push(AnalysisError {
                        message: format!("Method '{}' is missing a 'self' parameter", method),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                    return Ok(Type::Unit);
                }

                let self_param_type = &method_info.params[0].ty;
                if !self.types_compatible(&receiver_type, self_param_type) {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Method '{}' expects receiver of type {:?}, found {:?}",
                            method, self_param_type, receiver_type
                        ),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                }

                let expected_args = method_info.params.len().saturating_sub(1);
                if args.len() != expected_args {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Method '{}' expects {} argument(s), got {}",
                            method,
                            expected_args,
                            args.len()
                        ),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                }

                for (idx, arg_expr) in args.iter().enumerate() {
                    let arg_type = self.analyze_expr(arg_expr)?;
                    if let Some(expected_param) = method_info.params.get(idx + 1)
                        && !self.types_compatible(&arg_type, &expected_param.ty)
                    {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Argument {} has type {:?}, expected {:?}",
                                idx + 1,
                                arg_type,
                                expected_param.ty
                            ),
                            span: Span::new(0, 0),
                            severity: Severity::Error,
                        });
                    }
                }

                self.mark_method_used(&struct_name, method);

                Ok(method_info.return_type.clone().unwrap_or(Type::Unit))
            }

            Expr::TempScope { body } => {
                self.push_scope();
                let result = self.analyze_expr(body)?;

                if let Some(scope) = self.scopes.last()
                    && !scope.temp_allocations.is_empty()
                {
                    self.warnings.push(Warning::PossibleMemoryLeak {
                        span: Span::new(0, 0),
                    });
                }

                self.pop_scope();
                Ok(result)
            }

            Expr::Alloc { ty, count: _ } => Ok(Type::Pointer(Box::new(ty.clone()))),

            Expr::Free { ptr } => {
                let ptr_type = self.analyze_expr(ptr)?;
                if !matches!(ptr_type, Type::Pointer(_) | Type::MutPointer(_)) {
                    self.errors.push(AnalysisError {
                        message: "free() expects a pointer type".to_string(),
                        span: Span::new(0, 0),
                        severity: Severity::Error,
                    });
                }
                Ok(Type::Unit)
            }

            Expr::Ref { expr } => {
                let inner_type = self.analyze_expr(expr)?;
                Ok(Type::Pointer(Box::new(inner_type)))
            }

            Expr::Deref { expr } => {
                let ptr_type = self.analyze_expr(expr)?;
                match ptr_type {
                    Type::Pointer(inner) | Type::MutPointer(inner) => Ok(*inner),
                    _ => {
                        self.errors.push(AnalysisError {
                            message: "Cannot dereference non-pointer type".to_string(),
                            span: Span::new(0, 0),
                            severity: Severity::Error,
                        });
                        Ok(Type::Unit)
                    }
                }
            }

            Expr::Unsafe { body } => {
                self.warnings.push(Warning::UnsafeOperation {
                    description: "Unsafe block".to_string(),
                    span: Span::new(0, 0),
                });
                self.analyze_expr(body)
            }

            _ => Ok(Type::Unit),
        }
    }

    fn analyze_function_type_call(
        &mut self,
        func_type: Type,
        args: &[Expr],
        span: &Span,
    ) -> Result<Type, Vec<AnalysisError>> {
        match func_type {
            Type::Function(param_types, ret_type) => {
                if args.len() != param_types.len() {
                    self.errors.push(AnalysisError {
                        message: format!(
                            "Function expects {} argument(s), got {}",
                            param_types.len(),
                            args.len()
                        ),
                        span: span.clone(),
                        severity: Severity::Error,
                    });
                }

                for (idx, arg_expr) in args.iter().enumerate() {
                    let arg_type = self.analyze_expr(arg_expr)?;
                    if let Some(expected_type) = param_types.get(idx) {
                        if !self.types_compatible(&arg_type, expected_type) {
                            self.errors.push(AnalysisError {
                                message: format!(
                                    "Argument {} has wrong type: expected {:?}, found {:?}",
                                    idx + 1,
                                    expected_type,
                                    arg_type
                                ),
                                span: span.clone(),
                                severity: Severity::Error,
                            });
                        }
                    } else {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Extra argument {} provided to function call",
                                idx + 1
                            ),
                            span: span.clone(),
                            severity: Severity::Error,
                        });
                    }
                }

                if param_types.len() > args.len() {
                    for (idx, param_type) in param_types.iter().enumerate().skip(args.len()) {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Missing argument {} of type {:?}",
                                idx + 1,
                                param_type
                            ),
                            span: Span::new(0, 0),
                            severity: Severity::Error,
                        });
                    }
                }

                Ok(*ret_type)
            }
            other => {
                self.errors.push(AnalysisError {
                    message: format!("Attempted to call value of non-function type {:?}", other),
                    span: span.clone(),
                    severity: Severity::Error,
                });

                for arg in args {
                    let _ = self.analyze_expr(arg)?;
                }

                Ok(Type::Unit)
            }
        }
    }

    fn declare_variable(
        &mut self,
        name: &str,
        ty: &Type,
        mutable: bool,
        span: Span,
    ) -> Result<(), Vec<AnalysisError>> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.variables.contains_key(name) {
                self.warnings.push(Warning::ShadowedVariable {
                    name: name.to_string(),
                    span: span.clone(),
                });
            }

            scope.variables.insert(
                name.to_string(),
                VariableInfo {
                    ty: ty.clone(),
                    mutable,
                    used: false,
                    span,
                },
            );
        }
        Ok(())
    }

    fn find_variable(&self, name: &str) -> Option<VariableInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.variables.get(name) {
                return Some(info.clone());
            }
        }
        None
    }

    fn mark_variable_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.variables.get_mut(name) {
                info.used = true;
                return;
            }
        }
    }

    fn find_function(&self, name: &str) -> Option<FunctionInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.functions.get(name) {
                return Some(info.clone());
            }
        }

        if name.contains("::") {
            let parts: Vec<&str> = name.split("::").collect();
            if parts.len() == 2 {
                let (namespace_or_alias, function_name) = (parts[0], parts[1]);

                for (module_key, module_info) in &self.modules {
                    let module_parts: Vec<&str> = module_key.split("::").collect();
                    if module_parts.len() == 2 {
                        let (ns, filename) = (module_parts[0], module_parts[1]);

                        if (namespace_or_alias == ns
                            || namespace_or_alias == filename
                            || namespace_or_alias == module_key)
                            && let Some(func_info) = module_info.public_functions.get(function_name)
                        {
                            return Some(func_info.clone());
                        }
                    }
                }
            }
        }

        None
    }

    fn mark_function_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.functions.get_mut(name) {
                info.used = true;
                return;
            }
        }
    }

    fn find_struct_info(&self, name: &str) -> Option<StructInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.structs.get(name) {
                return Some(info.clone());
            }
        }

        for module_info in self.modules.values() {
            if let Some(info) = module_info.public_structs.get(name) {
                return Some(info.clone());
            }
        }

        None
    }

    fn find_struct_info_mut(&mut self, name: &str) -> Option<&mut StructInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.structs.get_mut(name) {
                return Some(info);
            }
        }
        None
    }

    fn mark_method_used(&mut self, struct_name: &str, method: &str) {
        if let Some(struct_info) = self.find_struct_info_mut(struct_name)
            && let Some(method_info) = struct_info.methods.get_mut(method)
        {
            method_info.used = true;
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn types_compatible(&self, a: &Type, b: &Type) -> bool {
        if a == b {
            return true;
        }

        if is_integral_type(a) && is_integral_type(b) {
            return true;
        }

        if is_float_type(a) && is_float_type(b) {
            return true;
        }

        if (is_float_type(a) && is_integral_type(b)) || (is_integral_type(a) && is_float_type(b)) {
            return true;
        }

        match (a, b) {
            (Type::Named(left), Type::Named(right)) => left == right,
            (Type::Pointer(left), Type::Pointer(right))
            | (Type::Pointer(left), Type::MutPointer(right))
            | (Type::MutPointer(left), Type::Pointer(right))
            | (Type::MutPointer(left), Type::MutPointer(right)) => {
                self.types_compatible(left, right)
            }
            (Type::Array(left_ty, left_size), Type::Array(right_ty, right_size)) => {
                left_size == right_size && self.types_compatible(left_ty, right_ty)
            }
            (Type::Slice(left_ty), Type::Slice(right_ty))
            | (Type::Slice(left_ty), Type::Array(right_ty, _))
            | (Type::Array(left_ty, _), Type::Slice(right_ty)) => {
                self.types_compatible(left_ty, right_ty)
            }
            (Type::Function(left_params, left_ret), Type::Function(right_params, right_ret)) => {
                if left_params.len() != right_params.len() {
                    return false;
                }

                left_params
                    .iter()
                    .zip(right_params.iter())
                    .all(|(lp, rp)| self.types_compatible(lp, rp))
                    && self.types_compatible(left_ret, right_ret)
            }
            _ => false,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            for (name, info) in scope.variables {
                if !info.used && !name.starts_with('_') {
                    self.warnings.push(Warning::UnusedVariable {
                        name: name.clone(),
                        span: info.span.clone(),
                    });
                }

                if !info.used && info.mutable {
                    self.warnings.push(Warning::UnnecessaryMut {
                        name,
                        span: info.span,
                    });
                }
            }
        }
    }

    fn check_unused(&mut self) {
        if let Some(scope) = self.scopes.first() {
            for (name, info) in &scope.functions {
                if !info.used && name != "main" {
                    self.warnings.push(Warning::UnusedFunction {
                        name: name.clone(),
                        span: info.span.clone(),
                    });
                }
            }
        }
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            temp_allocations: Vec::new(),
        }
    }
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

fn resolve_self_type(ty: &Type, self_struct: Option<&str>) -> Type {
    match ty {
        Type::Named(name) => {
            if let Some(struct_name) = self_struct
                && name == "Self"
            {
                return Type::Named(struct_name.to_string());
            }
            Type::Named(name.clone())
        }
        Type::Pointer(inner) => Type::Pointer(Box::new(resolve_self_type(inner, self_struct))),
        Type::MutPointer(inner) => {
            Type::MutPointer(Box::new(resolve_self_type(inner, self_struct)))
        }
        Type::Array(inner, size) => {
            Type::Array(Box::new(resolve_self_type(inner, self_struct)), *size)
        }
        Type::Slice(inner) => Type::Slice(Box::new(resolve_self_type(inner, self_struct))),
        Type::Function(params, ret) => {
            let resolved_params: Vec<Type> = params
                .iter()
                .map(|param| resolve_self_type(param, self_struct))
                .collect();
            let resolved_return = Box::new(resolve_self_type(ret, self_struct));
            Type::Function(resolved_params, resolved_return)
        }
        _ => ty.clone(),
    }
}

fn extract_struct_name(ty: &Type) -> Option<String> {
    match ty {
        Type::Named(name) => Some(name.clone()),
        Type::Pointer(inner) | Type::MutPointer(inner) => extract_struct_name(inner),
        _ => None,
    }
}

fn is_signed_integer_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128
    )
}

fn is_unsigned_integer_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128
    )
}

fn is_integral_type(ty: &Type) -> bool {
    is_signed_integer_type(ty) || is_unsigned_integer_type(ty)
}

fn is_float_type(ty: &Type) -> bool {
    matches!(ty, Type::F32 | Type::F64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unused_variable_warning() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Let {
                    name: "x".to_string(),
                    mutable: false,
                    ty: Some(Type::I64),
                    value: Box::new(Expr::Integer(42)),
                    then: Box::new(Expr::Integer(0)),
                },
                is_pub: true,
                is_macro: false,
                attributes: vec![],
                span: Span::new(0, 0),
            })],
        };

        let mut analyzer = Analyzer::new();
        let _ = analyzer.analyze(&program);

        assert!(!analyzer.get_warnings().is_empty());
    }

    #[test]
    fn test_type_mismatch_error() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: Some(Type::I64),
                body: Expr::Bool(true),
                is_pub: true,
                is_macro: false,
                attributes: vec![],
                span: Span::new(0, 0),
            })],
        };

        let mut analyzer = Analyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_err());
    }

    #[test]
    fn test_namespace_declaration() {
        let program = Program {
            namespace: Some(NamespaceDecl {
                namespace: "std".to_string(),
                filename: "math".to_string(),
            }),
            items: vec![Item::Function(Function {
                name: "add".to_string(),
                params: vec![
                    Param {
                        name: "x".to_string(),
                        ty: Type::I64,
                    },
                    Param {
                        name: "y".to_string(),
                        ty: Type::I64,
                    },
                ],
                return_type: Some(Type::I64),
                body: Expr::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Ident("x".to_string())),
                    right: Box::new(Expr::Ident("y".to_string())),
                },
                is_pub: true,
                is_macro: false,
                attributes: vec![],
                span: Span::new(0, 0),
            })],
        };

        let mut analyzer = Analyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
        assert_eq!(analyzer.current_namespace, Some("std::math".to_string()));
    }

    #[test]
    fn test_use_declaration() {
        let program = Program {
            namespace: None,
            items: vec![
                Item::Use(UseDecl {
                    namespace: "std".to_string(),
                    filename: "math".to_string(),
                    alias: None,
                }),
                Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Some(Type::I64),
                    body: Expr::Integer(42),
                    is_pub: false,
                    is_macro: false,
                    attributes: vec![],
                    span: Span::new(0, 0),
                }),
            ],
        };
        let mut analyzer = Analyzer::new();
        analyzer.add_module_search_path(PathBuf::from("test_files"));

        let result = analyzer.analyze(&program);

        if result.is_err() {
            eprintln!("Analysis error: {:?}", result);
        }

        assert!(result.is_ok());
        assert!(analyzer.modules.contains_key("std::math"));
    }

    #[test]
    fn test_numeric_type_compatibility() {
        let analyzer = Analyzer::new();

        assert!(analyzer.types_compatible(&Type::I32, &Type::I64));
        assert!(analyzer.types_compatible(&Type::F64, &Type::I16));
        assert!(!analyzer.types_compatible(&Type::Bool, &Type::I32));
    }
}

/// Format a type for display
fn format_type(ty: &Type) -> String {
    match ty {
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::I128 => "i128".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::U128 => "u128".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "String".to_string(),
        Type::Unit => "()".to_string(),
        Type::Named(name) => name.clone(),
        Type::Pointer(inner) => format!("*{}", format_type(inner)),
        Type::MutPointer(inner) => format!("*mut {}", format_type(inner)),
        Type::Array(inner, size) => format!("[{}; {}]", format_type(inner), size),
        Type::Slice(inner) => format!("[{}]", format_type(inner)),
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({}) -> {}", params_str, format_type(ret))
        }
        Type::TypeVar(name) => format!("<{}>", name),
    }
}

#[cfg(test)]
mod tests_extra {
    use super::*;

    #[test]
    fn test_higher_order_function_call_analysis() {
        let program = Program {
            namespace: None,
            items: vec![Item::Function(Function {
                name: "apply".to_string(),
                params: vec![
                    Param {
                        name: "f".to_string(),
                        ty: Type::Function(vec![Type::I64], Box::new(Type::I64)),
                    },
                    Param {
                        name: "x".to_string(),
                        ty: Type::I64,
                    },
                ],
                return_type: Some(Type::I64),
                body: Expr::Call {
                    func: Box::new(Expr::Ident("f".to_string())),
                    args: vec![Expr::Ident("x".to_string())],
                },
                is_pub: false,
                is_macro: false,
                attributes: vec![],
                span: Span::new(0, 0),
            })],
        };

        let mut analyzer = Analyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok(), "Analyzer returned errors: {:?}", result);
    }
}
