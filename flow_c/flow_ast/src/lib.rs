use std::collections::HashMap;

pub mod diagnostic;
pub use diagnostic::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub namespace: Option<NamespaceDecl>,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Impl(Impl),
    ExternBlock(ExternBlock),
    Import(Import),
    Use(UseDecl),
    Attribute(Attribute),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceDecl {
    pub namespace: String,
    pub filename: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub namespace: String,
    pub filename: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Expr,
    pub is_pub: bool,
    pub is_macro: bool,
    pub attributes: Vec<AttributeApplication>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
    pub is_pub: bool,
    pub attributes: Vec<AttributeApplication>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub is_pub: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    pub struct_name: String,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternBlock {
    pub lang: String,
    pub items: Vec<ExternItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternItem {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeApplication {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    I128,

    U8,
    U16,
    U32,
    U64,
    U128,

    F32,
    F64,

    Bool,
    Char,
    String,
    Unit,

    Named(String),
    Function(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
    MutPointer(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),

    TypeVar(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedBinding {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,

    Ident(String),

    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },

    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },

    Let {
        name: String,
        mutable: bool,
        ty: Option<Type>,
        value: Box<Expr>,
        then: Box<Expr>,
    },

    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },

    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Option<Box<Expr>>,
    },

    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },

    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    Block(Vec<Expr>),

    Field {
        expr: Box<Expr>,
        field: String,
    },

    Method {
        expr: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },

    StructInit {
        name: String,
        fields: HashMap<String, Expr>,
    },

    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Return(Option<Box<Expr>>),

    Alloc {
        ty: Type,
        count: Option<Box<Expr>>,
    },
    Free {
        ptr: Box<Expr>,
    },
    Ref {
        expr: Box<Expr>,
    },
    Deref {
        expr: Box<Expr>,
    },

    TempScope {
        body: Box<Expr>,
    },

    Unsafe {
        body: Box<Expr>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Warning {
    UnusedVariable { name: String, span: Span },
    UnusedFunction { name: String, span: Span },
    DeadCode { span: Span },
    UnnecessaryMut { name: String, span: Span },
    PossibleMemoryLeak { span: Span },
    UnsafeOperation { description: String, span: Span },
    ImplicitConversion { from: Type, to: Type, span: Span },
    ShadowedVariable { name: String, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file: Option<String>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            file: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ident(String),
    Integer(i64),
    Bool(bool),
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
}
