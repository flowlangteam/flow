use flow_ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TranspilerError {
    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Name resolution error: {0}")]
    NameResolutionError(String),

    #[error("Code generation error: {0}")]
    CodeGenError(String),

    #[error("Invalid extern block: {0}")]
    InvalidExternBlock(String),

    #[error("Missing field: {0}")]
    MissingField(String),

    #[error("Duplicate definition: {0}")]
    DuplicateDefinition(String),

    #[error("Invalid target: {0}")]
    InvalidTarget(String),

    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, TranspilerError>;

/// Trait for transpiling Flow AST to different target languages/platforms
pub trait Transpiler {
    /// The output type (could be bytecode, source code, etc.)
    type Output;

    /// Transpile a complete Flow program
    fn transpile(&mut self, program: &Program) -> Result<Self::Output>;

    /// Get the target language/platform name
    fn target_name(&self) -> &str;

    /// Get supported features for this target
    fn supported_features(&self) -> Vec<&str> {
        vec!["functions", "structs", "primitives"]
    }

    /// Check if a feature is supported
    fn supports_feature(&self, feature: &str) -> bool {
        self.supported_features().contains(&feature)
    }
}

/// Context shared across transpilation
#[derive(Debug, Clone)]
pub struct TranspileContext {
    pub functions: HashMap<String, FunctionSignature>,
    pub structs: HashMap<String, StructInfo>,
    pub extern_functions: HashMap<String, ExternFunctionInfo>,
    pub current_function: Option<String>,
    pub methods: HashMap<String, FunctionSignature>,
    pub namespace: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<(String, Type, bool)>, // name, type, is_pub
}

#[derive(Debug, Clone)]
pub struct ExternFunctionInfo {
    pub name: String,
    pub lang: String,
    pub params: Vec<Type>,
    pub return_type: Option<Type>,
}

impl TranspileContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            extern_functions: HashMap::new(),
            current_function: None,
            methods: HashMap::new(),
            namespace: None,
        }
    }

    pub fn add_function(&mut self, func: &Function) {
        let sig = FunctionSignature {
            name: func.name.clone(),
            params: func
                .params
                .iter()
                .map(|p| (p.name.clone(), p.ty.clone()))
                .collect(),
            return_type: func.return_type.clone(),
            is_pub: func.is_pub,
        };
        self.functions.insert(func.name.clone(), sig);
    }

    pub fn add_struct(&mut self, struct_def: &Struct) {
        let info = StructInfo {
            name: struct_def.name.clone(),
            fields: struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone(), f.is_pub))
                .collect(),
        };
        self.structs.insert(struct_def.name.clone(), info);
    }

    pub fn add_method(&mut self, struct_name: &str, func: &Function) {
        let sig = FunctionSignature {
            name: func.name.clone(),
            params: func
                .params
                .iter()
                .map(|p| (p.name.clone(), p.ty.clone()))
                .collect(),
            return_type: func.return_type.clone(),
            is_pub: func.is_pub,
        };
        let key = format!("{}::{}", struct_name, func.name);
        self.methods.insert(key, sig);
    }

    pub fn add_extern_block(&mut self, block: &ExternBlock) {
        for item in &block.items {
            let info = ExternFunctionInfo {
                name: item.name.clone(),
                lang: block.lang.clone(),
                params: item.params.clone(),
                return_type: item.return_type.clone(),
            };
            self.extern_functions.insert(item.name.clone(), info);
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionSignature> {
        self.functions.get(name)
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
        self.structs.get(name)
    }

    pub fn get_extern_function(&self, name: &str) -> Option<&ExternFunctionInfo> {
        self.extern_functions.get(name)
    }

    pub fn get_method(&self, struct_name: &str, method_name: &str) -> Option<&FunctionSignature> {
        let key = format!("{}::{}", struct_name, method_name);
        self.methods.get(&key)
    }

    pub fn set_namespace(&mut self, namespace: Option<String>) {
        self.namespace = namespace;
    }
}

impl Default for TranspileContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Visitor trait for traversing AST nodes
pub trait AstVisitor {
    type Result;

    fn visit_program(&mut self, program: &Program) -> Self::Result;
    fn visit_item(&mut self, item: &Item) -> Self::Result;
    fn visit_function(&mut self, func: &Function) -> Self::Result;
    fn visit_struct(&mut self, struct_def: &Struct) -> Self::Result;
    fn visit_impl(&mut self, impl_block: &Impl) -> Self::Result;
    fn visit_extern_block(&mut self, block: &ExternBlock) -> Self::Result;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
    fn visit_type(&mut self, ty: &Type) -> Self::Result;
}

/// Helper for collecting all symbols from a program
#[derive(Debug, Clone, Default)]
pub struct SymbolCollector {
    pub context: TranspileContext,
}

impl SymbolCollector {
    pub fn new() -> Self {
        Self {
            context: TranspileContext::new(),
        }
    }

    pub fn collect(&mut self, program: &Program) {
        // Set namespace
        if let Some(ns) = &program.namespace {
            self.context.set_namespace(Some(ns.namespace.clone()));
        }

        // First pass: collect all top-level declarations
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.context.add_function(func);
                }
                Item::Struct(struct_def) => {
                    self.context.add_struct(struct_def);
                }
                Item::ExternBlock(block) => {
                    self.context.add_extern_block(block);
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        self.context.add_method(&impl_block.struct_name, method);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn into_context(self) -> TranspileContext {
        self.context
    }
}

/// Type inference helper
pub struct TypeInferencer<'a> {
    context: &'a TranspileContext,
}

impl<'a> TypeInferencer<'a> {
    pub fn new(context: &'a TranspileContext) -> Self {
        Self { context }
    }

    pub fn infer_expr(&self, expr: &Expr, locals: &HashMap<String, Type>) -> Option<Type> {
        match expr {
            Expr::Integer(_) => Some(Type::I64),
            Expr::Float(_) => Some(Type::F64),
            Expr::Bool(_) => Some(Type::Bool),
            Expr::String(_) => Some(Type::String),
            Expr::Unit => Some(Type::Unit),

            Expr::Ident(name) => {
                // Check locals first, then context
                locals.get(name).cloned().or_else(|| {
                    self.context.get_function(name).map(|sig| {
                        Type::Function(
                            sig.params.iter().map(|(_, t)| t.clone()).collect(),
                            Box::new(sig.return_type.clone().unwrap_or(Type::Unit)),
                        )
                    })
                })
            }

            Expr::StructInit { name, .. } => Some(Type::Named(name.clone())),

            Expr::Field { expr, field } => {
                let base_ty = self.infer_expr(expr, locals)?;
                if let Type::Named(struct_name) = base_ty {
                    let struct_info = self.context.get_struct(&struct_name)?;
                    for (fname, ftype, _) in &struct_info.fields {
                        if fname == field {
                            return Some(ftype.clone());
                        }
                    }
                }
                None
            }

            Expr::Call { func, .. } => {
                if let Expr::Ident(func_name) = func.as_ref() {
                    self.context
                        .get_function(func_name)
                        .and_then(|sig| sig.return_type.clone())
                        .or_else(|| {
                            self.context
                                .get_extern_function(func_name)
                                .and_then(|info| info.return_type.clone())
                        })
                } else {
                    None
                }
            }

            Expr::Method { expr, method, .. } => {
                let receiver_ty = self.infer_expr(expr, locals)?;
                if let Type::Named(struct_name) = receiver_ty {
                    self.context
                        .get_method(&struct_name, method)
                        .and_then(|sig| sig.return_type.clone())
                } else {
                    None
                }
            }

            Expr::Binary { op, left, right } => {
                use BinOp::*;
                match op {
                    Add | Sub | Mul | Div | Mod => {
                        // Arithmetic operations preserve numeric type
                        let left_ty = self.infer_expr(left, locals)?;
                        let right_ty = self.infer_expr(right, locals)?;
                        // Simple type coercion: if either is float, result is float
                        match (left_ty, right_ty) {
                            (Type::F64, _) | (_, Type::F64) => Some(Type::F64),
                            (Type::F32, _) | (_, Type::F32) => Some(Type::F32),
                            _ => Some(Type::I64),
                        }
                    }
                    Eq | NotEq | Lt | Gt | LtEq | GtEq => Some(Type::Bool),
                    And | Or => Some(Type::Bool),
                }
            }

            Expr::Unary { op, expr } => {
                use UnOp::*;
                match op {
                    Neg => self.infer_expr(expr, locals),
                    Not => Some(Type::Bool),
                }
            }

            Expr::If { then, else_, .. } => {
                let then_ty = self.infer_expr(then, locals)?;
                if let Some(else_expr) = else_ {
                    let else_ty = self.infer_expr(else_expr, locals)?;
                    // Return type if both branches match
                    if then_ty == else_ty {
                        Some(then_ty)
                    } else {
                        None
                    }
                } else {
                    Some(Type::Unit)
                }
            }

            Expr::Block(exprs) => exprs.last().and_then(|e| self.infer_expr(e, locals)),

            Expr::Let { then, .. } => self.infer_expr(then, locals),

            Expr::Match { arms, .. } => {
                // Return type of first arm (assuming all arms have same type)
                arms.first()
                    .and_then(|arm| self.infer_expr(&arm.body, locals))
            }

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let context = TranspileContext::new();
        assert!(context.functions.is_empty());
        assert!(context.structs.is_empty());
        assert!(context.extern_functions.is_empty());
    }

    #[test]
    fn test_symbol_collector() {
        let mut collector = SymbolCollector::new();

        let func = Function {
            name: "test".to_string(),
            params: vec![],
            return_type: Some(Type::I32),
            body: Expr::Unit,
            is_pub: true,
            is_macro: false,
            attributes: vec![],
            span: Span::new(0, 0),
        };

        collector.context.add_function(&func);

        assert!(collector.context.get_function("test").is_some());
    }

    #[test]
    fn test_type_inference() {
        let context = TranspileContext::new();
        let inferencer = TypeInferencer::new(&context);
        let locals = HashMap::new();

        assert_eq!(
            inferencer.infer_expr(&Expr::Integer(42), &locals),
            Some(Type::I64)
        );
        assert_eq!(
            inferencer.infer_expr(&Expr::Bool(true), &locals),
            Some(Type::Bool)
        );
        assert_eq!(
            inferencer.infer_expr(&Expr::String("test".to_string()), &locals),
            Some(Type::String)
        );
    }

    #[test]
    fn test_binary_type_inference() {
        let context = TranspileContext::new();
        let inferencer = TypeInferencer::new(&context);
        let locals = HashMap::new();

        let add_expr = Expr::Binary {
            op: BinOp::Add,
            left: Box::new(Expr::Integer(1)),
            right: Box::new(Expr::Integer(2)),
        };

        assert_eq!(inferencer.infer_expr(&add_expr, &locals), Some(Type::I64));

        let eq_expr = Expr::Binary {
            op: BinOp::Eq,
            left: Box::new(Expr::Integer(1)),
            right: Box::new(Expr::Integer(2)),
        };

        assert_eq!(inferencer.infer_expr(&eq_expr, &locals), Some(Type::Bool));
    }
}
