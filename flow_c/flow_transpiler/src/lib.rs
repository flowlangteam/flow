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
}

/// Context shared across transpilation
#[derive(Debug, Clone)]
pub struct TranspileContext {
    pub functions: HashMap<String, FunctionSignature>,
    pub structs: HashMap<String, StructInfo>,
    pub current_function: Option<String>,
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

impl TranspileContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            current_function: None,
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
}

impl Default for TranspileContext {
    fn default() -> Self {
        Self::new()
    }
}
