use flow_ast::*;
use std::collections::HashMap;

pub struct Analyzer {
    warnings: Vec<Warning>,
    errors: Vec<AnalysisError>,
    scopes: Vec<Scope>,
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
struct FunctionInfo {
    params: Vec<Param>,
    return_type: Option<Type>,
    used: bool,
    span: Span,
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
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<AnalysisError>> {
        // First pass: collect all top-level declarations
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.declare_function(func)?;
                }
                Item::Struct(s) => {
                    self.declare_struct(s)?;
                }
                _ => {}
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
        // TODO: Implement struct declaration
        Ok(())
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
                    span: Span::new(0, 0),
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
                            span: Span::new(0, 0),
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
                        span: Span::new(0, 0),
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
                                span: Span::new(0, 0),
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
                                    span: Span::new(0, 0),
                                    severity: Severity::Error,
                                });
                            }
                        }

                        Ok(func_info.return_type.clone().unwrap_or(Type::Unit))
                    } else {
                        self.errors.push(AnalysisError {
                            message: format!("Undefined function '{}'", name),
                            span: Span::new(0, 0),
                            severity: Severity::Error,
                        });
                        Ok(Type::Unit)
                    }
                } else {
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
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.functions.get(name) {
                return Some(info.clone());
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

    fn types_compatible(&self, a: &Type, b: &Type) -> bool {
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
}
