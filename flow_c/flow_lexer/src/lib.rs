use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    // Keywords
    #[token("func")]
    Func,
    #[token("lambda")]
    Lambda,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("struct")]
    Struct,
    #[token("impl")]
    Impl,
    #[token("for")]
    For,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("extern")]
    Extern,
    #[token("import")]
    Import,
    #[token("from")]
    From,
    #[token("as")]
    As,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
    #[token("attr")]
    Attr,
    #[token("self")]
    SelfKw,
    #[token("Self")]
    SelfType,
    #[token("alloc")]
    Alloc,
    #[token("free")]
    Free,
    #[token("ref")]
    Ref,
    #[token("deref")]
    Deref,
    #[token("temp")]
    Temp,
    #[token("unsafe")]
    Unsafe,

    // Type keywords
    #[token("i8")]
    I8,
    #[token("i16")]
    I16,
    #[token("i32")]
    I32,
    #[token("i64")]
    I64,
    #[token("i128")]
    I128,
    #[token("u8")]
    U8,
    #[token("u16")]
    U16,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,
    #[token("u128")]
    U128,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("bool")]
    BoolType,
    #[token("char")]
    CharType,
    #[token("str")]
    StrType,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("=")]
    Eq,
    #[token("=>")]
    FatArrow,
    #[token("->")]
    ThinArrow,
    #[token("|>")]
    Pipe,
    #[token("::")]
    DoubleColon,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("##")]
    DoubleHash,
    #[token("$")]
    Dollar,
    #[token("#")]
    Hash,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("@")]
    At,
    #[token("|")]
    Bar,
    #[token("&")]
    Ampersand,

    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),

    #[regex(r"'([^'\\]|\\['\\bnfrt])'", |lex| {
        let s = lex.slice();
        s.chars().nth(1)
    })]
    Char(char),
}

pub struct Lexer<'source> {
    inner: logos::Lexer<'source, Token>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (Token, std::ops::Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;
        let span = self.inner.span();
        token.ok().map(|t| (t, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "func let mut struct";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![Token::Func, Token::Let, Token::Mut, Token::Struct]
        );
    }

    #[test]
    fn test_operators() {
        let source = "+ - * / == != |>";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::EqEq,
                Token::NotEq,
                Token::Pipe
            ]
        );
    }

    #[test]
    fn test_literals() {
        let source = r#"42 3.14 "hello""#;
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Integer(42),
                Token::Float(3.14),
                Token::String("hello".to_string())
            ]
        );
    }

    #[test]
    fn test_lambda_syntax() {
        let source = "lambda[x: Int, y: Int] { x + y }";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(tokens[0], Token::Lambda);
        assert_eq!(tokens[1], Token::LBracket);
        assert_eq!(tokens[2], Token::Ident("x".to_string()));
        assert_eq!(tokens[3], Token::Colon);
    }

    #[test]
    fn test_namespace_keywords() {
        let source = "as use pub import";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![Token::As, Token::Use, Token::Pub, Token::Import]
        );
    }

    #[test]
    fn test_namespace_declaration() {
        let source = "as std::io;";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::As,
                Token::Ident("std".to_string()),
                Token::DoubleColon,
                Token::Ident("io".to_string()),
                Token::Semi
            ]
        );
    }

    #[test]
    fn test_use_declaration() {
        let source = "use math::calc as calculator;";
        let tokens: Vec<_> = Lexer::new(source).map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Use,
                Token::Ident("math".to_string()),
                Token::DoubleColon,
                Token::Ident("calc".to_string()),
                Token::As,
                Token::Ident("calculator".to_string()),
                Token::Semi
            ]
        );
    }
}
