use flow_ast::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;

pub struct Analyzer {
    warnings: Vec<Warning>,
    errors: Vec<AnalysisError>,
    scopes: Vec<Scope>,
    // Multi-file support
    modules: HashMap<String, ModuleInfo>,
    current_namespace: Option<String>,
    module_search_paths: Vec<PathBuf>,
    parsed_modules: HashMap<String, Program>,
    // Source tracking
    current_source: Option<String>,
    current_file_path: Option<String>,
}

#[derive(Debug, Clone)]
struct ModuleInfo {
    namespace: String,
    filename: String,
    public_functions: HashMap<String, FunctionInfo>,
    public_structs: HashMap<String, StructInfo>,
    file_path: PathBuf,
}

#[derive(Debug, Clone)]
struct StructInfo {
    name: String,
    fields: Vec<Field>,
    span: Span,
    is_pub: bool,
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, VariableInfo>,
    functions: HashMap<String, FunctionInfo>,
    temp_allocations: Vec<String>,
}

#[derive(Debug, Clone)]
struct VariableInfo {
    ty: Type,
    mutable: bool,
    used: bool,
    initialized: bool,
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
        // Handle namespace declaration
        if let Some(namespace_decl) = &program.namespace {
            self.current_namespace = Some(format!("{}::{}", namespace_decl.namespace, namespace_decl.filename));
        }

        // First pass: collect all top-level declarations and process imports
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
                Item::Import(_) | Item::ExternBlock(_) | Item::Impl(_) => {
                    // These are handled in the second pass or don't need first-pass processing
                }
            }
        }

        // Second pass: type check and analyze each item
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.analyze_function(func)?;
                }
                Item::Impl(impl_block) => {
                    self.analyze_impl(impl_block)?;
                }
                _ => {}
            }
        }

        // Check for unused items
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

    fn declare_function(&mut self, func: &Function) -> Result<(), Vec<AnalysisError>> {
        let info = FunctionInfo {
            params: func.params.clone(),
            return_type: func.return_type.clone(),
            used: func.name == "main", // main is always considered used
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

    fn declare_struct(&mut self, _s: &Struct) -> Result<(), Vec<AnalysisError>> {
        // @TODO: Implement struct declaration analysis and type checking
        Ok(())
    }

    fn process_use_declaration(&mut self, use_decl: &UseDecl) -> Result<(), Vec<AnalysisError>> {
        let module_key = format!("{}::{}", use_decl.namespace, use_decl.filename);
        
        // Check if module is already loaded
        if self.modules.contains_key(&module_key) {
            return Ok(());
        }
        
        // Find and load the module file
        let module_file_path = self.find_module_file(&use_decl.namespace, &use_decl.filename)?;
        
        // Read and parse the module
        let source = fs::read_to_string(&module_file_path)
            .map_err(|e| vec![AnalysisError {
                message: format!("Failed to read module file {}: {}", module_file_path.display(), e),
                span: Span::new(0, 0),
                severity: Severity::Error,
            }])?;
        
        let mut parser = flow_parser::Parser::new(&source);
        let module_program = parser.parse()
            .map_err(|e| vec![AnalysisError {
                message: format!("Failed to parse module {}: {}", module_key, e.message),
                span: Span {
                    start: e.span.start,
                    end: e.span.end,
                    file: Some(module_file_path.to_string_lossy().to_string()),
                },
                severity: Severity::Error,
            }])?;
        
        // Verify the module has the correct namespace declaration
        if let Some(namespace_decl) = &module_program.namespace {
            let declared_key = format!("{}::{}", namespace_decl.namespace, namespace_decl.filename);
            if declared_key != module_key {
                return Err(vec![AnalysisError {
                    message: format!("Module namespace mismatch: expected '{}', found '{}'", module_key, declared_key),
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
        
        // Extract public items from the module
        let mut public_functions = HashMap::new();
        let mut public_structs = HashMap::new();
        
        for item in &module_program.items {
            match item {
                Item::Function(func) if func.is_pub => {
                    let func_info = FunctionInfo {
                        params: func.params.clone(),
                        return_type: func.return_type.clone(),
                        used: false,
                        span: Span::new(0, 0),
                    };
                    public_functions.insert(func.name.clone(), func_info);
                }
                Item::Struct(struct_def) if struct_def.is_pub => {
                    let struct_info = StructInfo {
                        name: struct_def.name.clone(),
                        fields: struct_def.fields.clone(),
                        span: Span::new(0, 0),
                        is_pub: true,
                    };
                    public_structs.insert(struct_def.name.clone(), struct_info);
                }
                _ => {} // Private items or other types
            }
        }
        
        // Store the module info
        let module_info = ModuleInfo {
            namespace: use_decl.namespace.clone(),
            filename: use_decl.filename.clone(),
            public_functions,
            public_structs,
            file_path: module_file_path,
        };
        self.modules.insert(module_key.clone(), module_info);
        
        // Store the parsed program for later use in codegen
        self.parsed_modules.insert(module_key, module_program);
        
        Ok(())
    }
    
    fn find_module_file(&self, namespace: &str, filename: &str) -> Result<PathBuf, Vec<AnalysisError>> {
        for search_path in &self.module_search_paths {
            // First try namespace/filename.flow structure (preferred)
            let namespace_dir = search_path.join(namespace);
            let candidate = namespace_dir.join(format!("{}.flow", filename));
            if candidate.exists() {
                return Ok(candidate);
            }
            
            // Fallback to namespace_filename.flow format
            let module_filename = format!("{}_{}.flow", namespace, filename);
            let candidate = search_path.join(&module_filename);
            if candidate.exists() {
                return Ok(candidate);
            }
        }
        
        Err(vec![AnalysisError {
            message: format!("Module file not found for '{}::{}' (searched for {}/{{filename}}.flow and {{namespace}}_{{filename}}.flow)", namespace, filename, namespace),
            span: Span::new(0, 0),
            severity: Severity::Error,
        }])
    }
    
    pub fn resolve_namespaced_function(&self, namespace_alias: &str, function_name: &str) -> Option<&FunctionInfo> {
        // Find the module by alias or direct namespace reference
        for (module_key, module_info) in &self.modules {
            let module_parts: Vec<&str> = module_key.split("::").collect();
            if module_parts.len() == 2 {
                let (ns, filename) = (module_parts[0], module_parts[1]);
                // Check if this matches the alias or direct reference
                if namespace_alias == ns || namespace_alias == filename || namespace_alias == module_key {
                    return module_info.public_functions.get(function_name);
                }
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
        
        // Fallback to a default span
        Span::new(0, 0)
    }

    fn find_identifier_span(&self, source: &str, name: &str, file_path: &str) -> Span {
        // Find all occurrences of the identifier
        let mut matches = Vec::new();
        let mut start = 0;
        
        while let Some(pos) = source[start..].find(name) {
            let absolute_pos = start + pos;
            
            // Check if this is a whole word (not part of another identifier)
            let is_word_start = absolute_pos == 0 || 
                !source.chars().nth(absolute_pos - 1).unwrap_or(' ').is_alphanumeric();
            
            let is_word_end = absolute_pos + name.len() >= source.len() || 
                !source.chars().nth(absolute_pos + name.len()).unwrap_or(' ').is_alphanumeric();
            
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
        
        // Fallback
        Span::new(0, 0)
    }

    pub fn get_parsed_modules(&self) -> &HashMap<String, Program> {
        &self.parsed_modules
    }

    fn analyze_function(&mut self, func: &Function) -> Result<(), Vec<AnalysisError>> {
        // Create new scope for function
        self.push_scope();

        // Add parameters to scope
        for param in &func.params {
            self.declare_variable(&param.name, &param.ty, false, Span::new(0, 0))?;
        }

        // Analyze function body
        let body_type = self.analyze_expr(&func.body)?;

        // Check return type matches
        if let Some(ref expected_type) = func.return_type {
            if !self.types_compatible(&body_type, expected_type) {
                self.errors.push(AnalysisError {
                    message: format!(
                        "Function '{}' returns {:?} but expected {:?}",
                        func.name, body_type, expected_type
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
        for method in &impl_block.methods {
            self.analyze_function(method)?;
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &Expr) -> Result<Type, Vec<AnalysisError>> {
        self.analyze_expr_with_span(expr, Span::new(0, 0))
    }

    fn analyze_expr_with_span(&mut self, expr: &Expr, expr_span: Span) -> Result<Type, Vec<AnalysisError>> {
        // If we have a default span (0, 0), try to estimate a better one
        let span = if expr_span.start == 0 && expr_span.end == 0 {
            self.estimate_expression_span(expr)
        } else {
            expr_span
        };
        
        match expr {
            Expr::Integer(_) => Ok(Type::I64), // Default integer type
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
                    Ok(Type::Unit) // Return dummy type to continue analysis
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
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        Ok(left_type)
                    }
                    BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq
                    | BinOp::GtEq | BinOp::And | BinOp::Or => Ok(Type::Bool),
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

                // Check if type annotation matches inferred type
                if let Some(annotated_type) = ty {
                    if !self.types_compatible(&value_type, annotated_type) {
                        self.errors.push(AnalysisError {
                            message: format!(
                                "Type mismatch: expected {:?}, found {:?}",
                                annotated_type, value_type
                            ),
                            span: span.clone(),
                            severity: Severity::Error,
                        });
                    }
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

                        // Check argument count
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

                        // Check argument types
                        for (i, (arg, param)) in args.iter().zip(func_info.params.iter()).enumerate()
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
                        self.errors.push(AnalysisError {
                            message: format!("Undefined function '{}'", name),
                            span: span.clone(),
                            severity: Severity::Error,
                        });
                        Ok(Type::Unit)
                    }
                } else {
                    // @TODO: Implement proper type checking for higher-order function calls
                    // Higher-order function call
                    let _func_type = self.analyze_expr(func)?;
                    Ok(Type::Unit) // Simplified for now
                }
            }

            Expr::TempScope { body } => {
                self.push_scope();
                let result = self.analyze_expr(body)?;
                
                // Check for leaking allocations
                if let Some(scope) = self.scopes.last() {
                    if !scope.temp_allocations.is_empty() {
                        self.warnings.push(Warning::PossibleMemoryLeak {
                            span: Span::new(0, 0),
                        });
                    }
                }
                
                self.pop_scope();
                Ok(result)
            }

            Expr::Alloc { ty, count: _ } => {
                Ok(Type::Pointer(Box::new(ty.clone())))
            }

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

            _ => Ok(Type::Unit), // Placeholder for unimplemented expressions
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
                    initialized: true,
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
        // First check local scopes
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.functions.get(name) {
                return Some(info.clone());
            }
        }
        
        // If not found locally, check if it's a namespaced function call
        if name.contains("::") {
            let parts: Vec<&str> = name.split("::").collect();
            if parts.len() == 2 {
                let (namespace_or_alias, function_name) = (parts[0], parts[1]);
                
                // Look for a module that matches this namespace/alias
                for (module_key, module_info) in &self.modules {
                    let module_parts: Vec<&str> = module_key.split("::").collect();
                    if module_parts.len() == 2 {
                        let (ns, filename) = (module_parts[0], module_parts[1]);
                        
                        // Check if the namespace_or_alias matches the namespace, filename, or full key
                        if namespace_or_alias == ns || 
                           namespace_or_alias == filename || 
                           namespace_or_alias == module_key {
                            
                            if let Some(func_info) = module_info.public_functions.get(function_name) {
                                return Some(func_info.clone());
                            }
                        }
                    }
                }
            }
        }
        
        None
    }

    fn mark_function_used(&mut self, name: &str) {
        // First try local scopes
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.functions.get_mut(name) {
                info.used = true;
                return;
            }
        }
        
    }

    fn types_compatible(&self, a: &Type, b: &Type) -> bool {
        // @TODO: Implement comprehensive type compatibility checking including coercion rules
        // Simplified type compatibility check
        match (a, b) {
            (Type::I8, Type::I8) | (Type::I16, Type::I16) | (Type::I32, Type::I32)
            | (Type::I64, Type::I64) | (Type::I128, Type::I128) => true,
            (Type::U8, Type::U8) | (Type::U16, Type::U16) | (Type::U32, Type::U32)
            | (Type::U64, Type::U64) | (Type::U128, Type::U128) => true,
            (Type::F32, Type::F32) | (Type::F64, Type::F64) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::String, Type::String) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Named(a), Type::Named(b)) => a == b,
            (Type::Pointer(a), Type::Pointer(b)) => self.types_compatible(a, b),
            (Type::MutPointer(a), Type::MutPointer(b)) => self.types_compatible(a, b),
            _ => false,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Check for unused variables
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
            temp_allocations: Vec::new(),
        }
    }
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
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
            })],
        };

        let mut analyzer = Analyzer::new();
        let _ = analyzer.analyze(&program);
        
        assert!(analyzer.get_warnings().len() > 0);
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
                    Param { name: "x".to_string(), ty: Type::I64 },
                    Param { name: "y".to_string(), ty: Type::I64 },
                ],
                return_type: Some(Type::I64),
                body: Expr::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Ident("x".to_string())),
                    right: Box::new(Expr::Ident("y".to_string())),
                },
                is_pub: true,
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
                })
            ],
        };

        let mut analyzer = Analyzer::new();
        // Add the test files directory to search path
        analyzer.add_module_search_path(PathBuf::from("test_files"));
        
        let result = analyzer.analyze(&program);
        
        if result.is_err() {
            eprintln!("Analysis error: {:?}", result);
        }
        
        assert!(result.is_ok());
        assert!(analyzer.modules.contains_key("std::math"));
    }
}
