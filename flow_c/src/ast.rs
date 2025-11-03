use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Impl(Impl),
    ExternBlock(ExternBlock),
    Import(Import),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Expr,
    pub is_pub: bool,
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
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Unit,
    Named(String),
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,

    // Variables
    Ident(String),

    // Binary operations
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Unary operations
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },

    // Function call
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    // Lambda
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },

    // Let binding
    Let {
        name: String,
        mutable: bool,
        ty: Option<Type>,
        value: Box<Expr>,
        then: Box<Expr>,
    },

    // If expression
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Option<Box<Expr>>,
    },

    // Match expression
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Block
    Block(Vec<Expr>),

    // Field access
    Field {
        expr: Box<Expr>,
        field: String,
    },

    // Method call
    Method {
        expr: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },

    // Struct instantiation
    StructInit {
        name: String,
        fields: HashMap<String, Expr>,
    },

    // Pipe operator
    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Return
    Return(Option<Box<Expr>>),
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
