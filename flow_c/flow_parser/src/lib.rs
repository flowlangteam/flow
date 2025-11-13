use flow_ast::{Diagnostic, DiagnosticSeverity, Label, LabelStyle, Span, *};
use flow_lexer::{Lexer, Token};
use std::collections::HashMap;

type ParseResult<T> = Result<T, Diagnostic>;

#[derive(Clone)]
struct MacroDef {
    params: Vec<Param>,
    body: Expr,
}

pub struct Parser {
    tokens: Vec<(Token, std::ops::Range<usize>)>,
    pos: usize,
    macros: HashMap<String, MacroDef>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let tokens: Vec<_> = Lexer::new(source).collect();
        Self {
            tokens,
            pos: 0,
            macros: HashMap::new(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        // Check for namespace declaration at the beginning
        let namespace = if self.check(&Token::As) {
            Some(self.parse_namespace_declaration()?)
        } else {
            None
        };

        let mut items = Vec::new();
        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }
        self.build_program(namespace, items)
    }

    fn build_program(
        &self,
        namespace: Option<NamespaceDecl>,
        items: Vec<Item>,
    ) -> ParseResult<Program> {
        Ok(Program { namespace, items })
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        // Parse #[attribute] annotations
        let mut attributes = Vec::new();
        while self.check(&Token::Hash) {
            attributes.push(self.parse_attribute_application()?);
        }

        let is_pub = if self.check(&Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        // Check for ##macro marker on functions
        let is_macro = if self.check(&Token::DoubleHash) {
            self.advance();
            true
        } else {
            false
        };

        match self.peek() {
            Some(Token::Func) => Ok(Item::Function(self.parse_function(is_pub, is_macro, attributes)?)),
            Some(Token::Struct) => Ok(Item::Struct(self.parse_struct(is_pub, attributes)?)),
            Some(Token::Impl) => Ok(Item::Impl(self.parse_impl()?)),
            Some(Token::Extern) => Ok(Item::ExternBlock(self.parse_extern_block()?)),
            Some(Token::Import) => Ok(Item::Import(self.parse_import()?)),
            Some(Token::Use) => Ok(Item::Use(self.parse_use_declaration()?)),
            Some(Token::Attr) => self.parse_attribute_definition(is_pub).map(Item::Attribute),
            _ => {
                if is_macro {
                    Err(self.error("##macro marker can only be applied to functions"))
                } else if !attributes.is_empty() {
                    Err(self.error("Attributes can only be applied to functions and structs"))
                } else {
                    Err(self.error("Expected item (func, struct, impl, extern, import, use, or attr)"))
                }
            }
        }
    }

    fn parse_function(&mut self, is_pub: bool, is_macro: bool, attributes: Vec<AttributeApplication>) -> ParseResult<Function> {
        let start_pos = self.pos;
        self.expect(&Token::Func)?;
        let name = self.expect_ident()?;

        self.expect(&Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::ThinArrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block_or_expr()?;
        let span = self.span_from(start_pos);

        // Register macro if marked with ##macro
        if is_macro {
            self.macros.insert(name.clone(), MacroDef {
                params: params.clone(),
                body: body.clone(),
            });
        }

        Ok(Function {
            name,
            params,
            return_type,
            body,
            is_pub,
            is_macro,
            attributes,
            span,
        })
    }

    fn parse_params(&mut self) -> ParseResult<Vec<Param>> {
        let mut params = Vec::new();

        if self.check(&Token::RParen) {
            return Ok(params);
        }

        loop {
            let name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            params.push(Param { name, ty });

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance();
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        match self.peek() {
            // Primitive signed integers
            Some(Token::I8) => {
                self.advance();
                Ok(Type::I8)
            }
            Some(Token::I16) => {
                self.advance();
                Ok(Type::I16)
            }
            Some(Token::I32) => {
                self.advance();
                Ok(Type::I32)
            }
            Some(Token::I64) => {
                self.advance();
                Ok(Type::I64)
            }
            Some(Token::I128) => {
                self.advance();
                Ok(Type::I128)
            }

            // Primitive unsigned integers
            Some(Token::U8) => {
                self.advance();
                Ok(Type::U8)
            }
            Some(Token::U16) => {
                self.advance();
                Ok(Type::U16)
            }
            Some(Token::U32) => {
                self.advance();
                Ok(Type::U32)
            }
            Some(Token::U64) => {
                self.advance();
                Ok(Type::U64)
            }
            Some(Token::U128) => {
                self.advance();
                Ok(Type::U128)
            }

            // Floating point
            Some(Token::F32) => {
                self.advance();
                Ok(Type::F32)
            }
            Some(Token::F64) => {
                self.advance();
                Ok(Type::F64)
            }

            // Other primitives
            Some(Token::BoolType) => {
                self.advance();
                Ok(Type::Bool)
            }
            Some(Token::CharType) => {
                self.advance();
                Ok(Type::Char)
            }
            Some(Token::StrType) => {
                self.advance();
                Ok(Type::String)
            }
            Some(Token::SelfType) => {
                self.advance();
                Ok(Type::Named("Self".to_string()))
            }

            Some(Token::Ident(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Type::Named(s))
            }
            Some(Token::Star) => {
                // Pointer type: *T or *mut T
                self.advance();
                if self.check(&Token::Mut) {
                    self.advance();
                    let inner = self.parse_type()?;
                    Ok(Type::MutPointer(Box::new(inner)))
                } else {
                    let inner = self.parse_type()?;
                    Ok(Type::Pointer(Box::new(inner)))
                }
            }
            Some(Token::LBracket) => {
                // Array or slice type: [T; N] or [T]
                self.advance();
                let inner = self.parse_type()?;

                if self.check(&Token::Semi) {
                    self.advance();
                    if let Some(Token::Integer(n)) = self.peek() {
                        let n = *n as usize;
                        self.advance();
                        self.expect(&Token::RBracket)?;
                        Ok(Type::Array(Box::new(inner), n))
                    } else {
                        Err(self.error("Expected array size"))
                    }
                } else {
                    self.expect(&Token::RBracket)?;
                    Ok(Type::Slice(Box::new(inner)))
                }
            }
            Some(Token::LParen) => {
                self.advance();
                if self.check(&Token::RParen) {
                    self.advance();
                    Ok(Type::Unit)
                } else {
                    // Function type: (T1, T2) -> T3
                    let mut param_types = vec![self.parse_type()?];
                    while self.check(&Token::Comma) {
                        self.advance();
                        param_types.push(self.parse_type()?);
                    }
                    self.expect(&Token::RParen)?;
                    self.expect(&Token::ThinArrow)?;
                    let return_type = self.parse_type()?;
                    Ok(Type::Function(param_types, Box::new(return_type)))
                }
            }
            _ => Err(self.error("Expected type")),
        }
    }

    fn parse_struct(&mut self, is_pub: bool, attributes: Vec<AttributeApplication>) -> ParseResult<Struct> {
        self.expect(&Token::Struct)?;
        let name = self.expect_ident()?;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) {
            let field_pub = if self.check(&Token::Pub) {
                self.advance();
                true
            } else {
                false
            };

            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;

            fields.push(Field {
                name: field_name,
                ty,
                is_pub: field_pub,
            });

            if !self.check(&Token::Comma) {
                break;
            }
            self.advance();
        }

        self.expect(&Token::RBrace)?;

        Ok(Struct {
            name,
            fields,
            is_pub,
            attributes,
        })
    }

    fn parse_impl(&mut self) -> ParseResult<Impl> {
        self.expect(&Token::Impl)?;
        let struct_name = self.expect_ident()?;
        self.expect(&Token::LBrace)?;

        let mut methods = Vec::new();
        while !self.check(&Token::RBrace) {
            let is_pub = if self.check(&Token::Pub) {
                self.advance();
                true
            } else {
                false
            };
            // Impl methods don't support attributes or macro markers currently
            methods.push(self.parse_function(is_pub, false, Vec::new())?);
        }

        self.expect(&Token::RBrace)?;

        Ok(Impl {
            struct_name,
            methods,
        })
    }

    fn parse_extern_block(&mut self) -> ParseResult<ExternBlock> {
        self.expect(&Token::Extern)?;
        let lang = self.expect_string()?;
        self.expect(&Token::LBrace)?;

        let mut items = Vec::new();
        while !self.check(&Token::RBrace) {
            self.expect(&Token::Func)?;
            let name = self.expect_ident()?;
            self.expect(&Token::LParen)?;

            let mut params = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    params.push(self.parse_type()?);
                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }
            self.expect(&Token::RParen)?;

            let return_type = if self.check(&Token::ThinArrow) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            self.expect(&Token::Semi)?;

            items.push(ExternItem {
                name,
                params,
                return_type,
            });
        }

        self.expect(&Token::RBrace)?;

        Ok(ExternBlock { lang, items })
    }

    fn parse_import(&mut self) -> ParseResult<Import> {
        self.expect(&Token::Import)?;

        let mut path = vec![self.expect_ident()?];
        while self.check(&Token::DoubleColon) {
            self.advance();
            path.push(self.expect_ident()?);
        }

        let alias = if self.check(&Token::As) {
            self.advance();
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(&Token::Semi)?;

        Ok(Import { path, alias })
    }

    fn parse_namespace_declaration(&mut self) -> ParseResult<NamespaceDecl> {
        self.expect(&Token::As)?;

        let namespace = self.expect_ident()?;
        self.expect(&Token::DoubleColon)?;
        let filename = self.expect_ident()?;
        self.expect(&Token::Semi)?;

        Ok(NamespaceDecl {
            namespace,
            filename,
        })
    }

    fn parse_use_declaration(&mut self) -> ParseResult<UseDecl> {
        self.expect(&Token::Use)?;

        let namespace = self.expect_ident()?;
        self.expect(&Token::DoubleColon)?;
        let filename = self.expect_ident()?;

        let alias = if self.check(&Token::As) {
            self.advance();
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(&Token::Semi)?;

        Ok(UseDecl {
            namespace,
            filename,
            alias,
        })
    }

    fn parse_attribute_definition(&mut self, is_pub: bool) -> ParseResult<Attribute> {
        if is_pub {
            return Err(self.error("Attribute definitions cannot be declared as public"));
        }

        let start_pos = self.pos;
        self.expect(&Token::Attr)?;
        let name = self.expect_ident()?;

        let mut params = Vec::new();
        if self.check(&Token::LParen) {
            self.advance();
            if !self.check(&Token::RParen) {
                loop {
                    params.push(self.expect_ident()?);
                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }
            self.expect(&Token::RParen)?;
        }

        let body = self.parse_block_or_expr()?;
        let span = self.span_from(start_pos);

        Ok(Attribute {
            name,
            params,
            body,
            span,
        })
    }

    fn parse_attribute_application(&mut self) -> ParseResult<AttributeApplication> {
        self.expect(&Token::Hash)?;
        self.expect(&Token::LBracket)?;

        let name = self.expect_ident()?;
        let mut args = Vec::new();

        if self.check(&Token::LParen) {
            self.advance();
            if !self.check(&Token::RParen) {
                loop {
                    args.push(self.expect_ident()?);
                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
            }
            self.expect(&Token::RParen)?;
        }

        self.expect(&Token::RBracket)?;

        Ok(AttributeApplication { name, args })
    }

    fn parse_block_or_expr(&mut self) -> ParseResult<Expr> {
        if self.check(&Token::LBrace) {
            self.parse_block()
        } else {
            self.parse_expr()
        }
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::LBrace)?;
        let mut exprs = Vec::new();

        while !self.check(&Token::RBrace) {
            exprs.push(self.parse_expr()?);

            if self.check(&Token::Semi) {
                self.advance();
            } else if !self.check(&Token::RBrace) {
                break;
            }
        }

        self.expect(&Token::RBrace)?;

        if exprs.is_empty() {
            Ok(Expr::Unit)
        } else if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::Block(exprs))
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_pipe()?;

        if self.check(&Token::Eq) {
            self.advance();
            let value = Box::new(self.parse_assignment()?);

            match expr {
                Expr::Ident(_) | Expr::Field { .. } => Ok(Expr::Assign {
                    target: Box::new(expr),
                    value,
                }),
                _ => Err(self.error("Invalid assignment target")),
            }
        } else {
            Ok(expr)
        }
    }

    fn parse_pipe(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_let()?;

        while self.check(&Token::Pipe) {
            self.advance();
            let right = self.parse_let()?;
            expr = Expr::Pipe {
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_let(&mut self) -> ParseResult<Expr> {
        if self.check(&Token::Let) {
            self.advance();

            let mutable = if self.check(&Token::Mut) {
                self.advance();
                true
            } else {
                false
            };

            let name = self.expect_ident()?;

            let ty = if self.check(&Token::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };

            self.expect(&Token::Eq)?;
            let value = Box::new(self.parse_expr()?);

            self.expect(&Token::Semi)?;
            let then = Box::new(self.parse_expr()?);

            Ok(Expr::Let {
                name,
                mutable,
                ty,
                value,
                then,
            })
        } else {
            self.parse_if()
        }
    }

    fn parse_if(&mut self) -> ParseResult<Expr> {
        if self.check(&Token::If) {
            self.advance();
            let cond = Box::new(self.parse_logical_or()?);
            let then = Box::new(self.parse_block_or_expr()?);

            let else_ = if self.check(&Token::Else) {
                self.advance();
                Some(Box::new(self.parse_block_or_expr()?))
            } else {
                None
            };

            Ok(Expr::If { cond, then, else_ })
        } else if self.check(&Token::Match) {
            self.parse_match()
        } else {
            self.parse_logical_or()
        }
    }

    fn parse_match(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Match)?;
        let expr = Box::new(self.parse_expr()?);
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&Token::RBrace) {
            let pattern = self.parse_pattern()?;

            let guard = if self.check(&Token::If) {
                self.advance();
                Some(self.parse_logical_or()?)
            } else {
                None
            };

            self.expect(&Token::FatArrow)?;
            let body = self.parse_expr()?;

            arms.push(MatchArm {
                pattern,
                guard,
                body,
            });

            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Expr::Match { expr, arms })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match self.peek() {
            Some(Token::Ident(s)) if s == "_" => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Some(Token::Ident(s)) => {
                let s = s.clone();
                self.advance();

                // Could be a struct pattern
                if self.check(&Token::LBrace) {
                    self.advance();
                    let mut fields = Vec::new();

                    while !self.check(&Token::RBrace) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let pattern = self.parse_pattern()?;
                        fields.push((field_name, pattern));

                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }

                    self.expect(&Token::RBrace)?;

                    Ok(Pattern::Struct { name: s, fields })
                } else {
                    Ok(Pattern::Ident(s))
                }
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Integer(n))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Pattern::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Pattern::Bool(false))
            }
            _ => Err(self.error("Expected pattern")),
        }
    }

    fn parse_logical_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_and()?;

        while self.check(&Token::Or) {
            self.advance();
            let right = self.parse_logical_and()?;
            expr = Expr::Binary {
                op: BinOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;

        while self.check(&Token::And) {
            self.advance();
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                op: BinOp::And,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;

        while let Some(op) = self.match_tokens(&[Token::EqEq, Token::NotEq]) {
            let bin_op = match op {
                Token::EqEq => BinOp::Eq,
                Token::NotEq => BinOp::NotEq,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                op: bin_op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        while let Some(op) = self.match_tokens(&[Token::Lt, Token::Gt, Token::LtEq, Token::GtEq]) {
            let bin_op = match op {
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::LtEq => BinOp::LtEq,
                Token::GtEq => BinOp::GtEq,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            expr = Expr::Binary {
                op: bin_op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        while let Some(op) = self.match_tokens(&[Token::Plus, Token::Minus]) {
            let bin_op = match op {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                op: bin_op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        while let Some(op) = self.match_tokens(&[Token::Star, Token::Slash, Token::Percent]) {
            let bin_op = match op {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                op: bin_op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if let Some(op) = self.match_tokens(&[Token::Minus, Token::Not]) {
            let un_op = match op {
                Token::Minus => UnOp::Neg,
                Token::Not => UnOp::Not,
                _ => unreachable!(),
            };
            let expr = self.parse_unary()?;
            Ok(Expr::Unary {
                op: un_op,
                expr: Box::new(expr),
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(&Token::LParen) {
                self.advance();
                let args = self.parse_args()?;
                self.expect(&Token::RParen)?;
                expr = Expr::Call {
                    func: Box::new(expr),
                    args,
                };
            } else if self.check(&Token::Dot) {
                self.advance();
                let field = self.expect_ident()?;

                if self.check(&Token::LParen) {
                    self.advance();
                    let args = self.parse_args()?;
                    self.expect(&Token::RParen)?;
                    expr = Expr::Method {
                        expr: Box::new(expr),
                        method: field,
                        args,
                    };
                } else {
                    expr = Expr::Field {
                        expr: Box::new(expr),
                        field,
                    };
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_args(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();

        if self.check(&Token::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expr()?);
            if !self.check(&Token::Comma) {
                break;
            }
            self.advance();
        }

        Ok(args)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            Some(Token::Dollar) => {
                // $MACRO(...) invocation
                let start_pos = self.pos;
                self.advance();
                let name = self.expect_ident()?;

                let args = if self.check(&Token::LParen) {
                    self.advance();
                    let mut parsed_args = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            parsed_args.push(self.parse_expr()?);
                            if !self.check(&Token::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    self.expect(&Token::RParen)?;
                    parsed_args
                } else {
                    Vec::new()
                };

                let span = self.span_from(start_pos);
                self.expand_macro(name, args, span)
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();
                Ok(Expr::Integer(n))
            }
            Some(Token::Float(f)) => {
                let f = *f;
                self.advance();
                Ok(Expr::Float(f))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::String(s))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Some(Token::Ident(s)) => {
                let s = s.clone();
                self.advance();

                // Check for namespace resolution (::)
                if self.check(&Token::DoubleColon) {
                    self.advance();
                    let member = self.expect_ident()?;
                    let namespaced_ident = format!("{}::{}", s, member);

                    // Check for struct initialization
                    if self.check(&Token::LBrace) {
                        self.advance();
                        let mut fields = HashMap::new();

                        while !self.check(&Token::RBrace) {
                            let field_name = self.expect_ident()?;
                            self.expect(&Token::Colon)?;
                            let value = self.parse_expr()?;
                            fields.insert(field_name, value);

                            if !self.check(&Token::Comma) {
                                break;
                            }
                            self.advance();
                        }

                        self.expect(&Token::RBrace)?;

                        Ok(Expr::StructInit {
                            name: namespaced_ident,
                            fields,
                        })
                    } else {
                        Ok(Expr::Ident(namespaced_ident))
                    }
                } else {
                    // Check for struct initialization
                    if self.check(&Token::LBrace) {
                        self.advance();
                        let mut fields = HashMap::new();

                        while !self.check(&Token::RBrace) {
                            let field_name = self.expect_ident()?;
                            self.expect(&Token::Colon)?;
                            let value = self.parse_expr()?;
                            fields.insert(field_name, value);

                            if !self.check(&Token::Comma) {
                                break;
                            }
                            self.advance();
                        }

                        self.expect(&Token::RBrace)?;

                        Ok(Expr::StructInit { name: s, fields })
                    } else {
                        Ok(Expr::Ident(s))
                    }
                }
            }
            Some(Token::LParen) => {
                self.advance();
                if self.check(&Token::RParen) {
                    self.advance();
                    Ok(Expr::Unit)
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(&Token::RParen)?;
                    Ok(expr)
                }
            }
            Some(Token::Lambda) => self.parse_lambda(),
            Some(Token::Return) => {
                self.advance();
                if self.check(&Token::Semi) || self.check(&Token::RBrace) {
                    Ok(Expr::Return(None))
                } else {
                    Ok(Expr::Return(Some(Box::new(self.parse_expr()?))))
                }
            }
            Some(Token::LBrace) => self.parse_block(),
            Some(Token::Temp) => {
                self.advance();
                let body = Box::new(self.parse_block()?);
                Ok(Expr::TempScope { body })
            }
            Some(Token::Unsafe) => {
                self.advance();
                let body = Box::new(self.parse_block()?);
                Ok(Expr::Unsafe { body })
            }
            Some(Token::Alloc) => {
                self.advance();
                self.expect(&Token::Lt)?;
                let ty = self.parse_type()?;
                self.expect(&Token::Gt)?;

                let count = if self.check(&Token::LBracket) {
                    self.advance();
                    let expr = self.parse_expr()?;
                    self.expect(&Token::RBracket)?;
                    Some(Box::new(expr))
                } else {
                    None
                };

                Ok(Expr::Alloc { ty, count })
            }
            Some(Token::Free) => {
                self.advance();
                self.expect(&Token::LParen)?;
                let ptr = Box::new(self.parse_expr()?);
                self.expect(&Token::RParen)?;
                Ok(Expr::Free { ptr })
            }
            Some(Token::Ref) => {
                self.advance();
                let expr = Box::new(self.parse_unary()?);
                Ok(Expr::Ref { expr })
            }
            Some(Token::Deref) => {
                self.advance();
                let expr = Box::new(self.parse_unary()?);
                Ok(Expr::Deref { expr })
            }
            Some(Token::Ampersand) => {
                self.advance();
                let expr = Box::new(self.parse_unary()?);
                Ok(Expr::Ref { expr })
            }
            _ => Err(self.error("Expected expression")),
        }
    }

    fn parse_lambda(&mut self) -> ParseResult<Expr> {
        self.expect(&Token::Lambda)?;
        self.expect(&Token::LBracket)?;

        let mut params = Vec::new();
        if !self.check(&Token::RBracket) {
            loop {
                let name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;

                params.push(Param { name, ty });

                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.expect(&Token::RBracket)?;

        let body = Box::new(self.parse_block_or_expr()?);

        Ok(Expr::Lambda { params, body })
    }

    fn expand_macro(
        &mut self,
        name: String,
        args: Vec<Expr>,
        span: Span,
    ) -> ParseResult<Expr> {
        let definition = if let Some(def) = self.macros.get(&name) {
            def.clone()
        } else {
            let mut diag = Diagnostic::new(
                DiagnosticSeverity::Error,
                format!("Unknown macro '{}'", name),
            );
            diag = diag.with_label(
                Label::new(span.clone(), LabelStyle::Primary)
                    .with_message("macro invocation here"),
            );
            return Err(diag);
        };

        if definition.params.len() != args.len() {
            let mut diag = Diagnostic::new(
                DiagnosticSeverity::Error,
                format!(
                    "Macro '{}' expects {} argument(s), got {}",
                    name,
                    definition.params.len(),
                    args.len()
                ),
            );
            diag = diag.with_label(
                Label::new(span.clone(), LabelStyle::Primary)
                    .with_message("incorrect number of arguments"),
            );
            return Err(diag);
        }

        let mut mapping = HashMap::new();
        for (param, arg) in definition.params.iter().zip(args.into_iter()) {
            mapping.insert(param.name.clone(), arg);
        }

        Ok(self.substitute_expr(&definition.body, &mapping))
    }

    fn substitute_expr(&self, template: &Expr, mapping: &HashMap<String, Expr>) -> Expr {
        match template {
            Expr::Integer(n) => Expr::Integer(*n),
            Expr::Float(f) => Expr::Float(*f),
            Expr::String(s) => Expr::String(s.clone()),
            Expr::Bool(b) => Expr::Bool(*b),
            Expr::Unit => Expr::Unit,
            Expr::Ident(name) => mapping
                .get(name)
                .cloned()
                .unwrap_or_else(|| Expr::Ident(name.clone())),
            Expr::Binary { op, left, right } => Expr::Binary {
                op: *op,
                left: Box::new(self.substitute_expr(left, mapping)),
                right: Box::new(self.substitute_expr(right, mapping)),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op: *op,
                expr: Box::new(self.substitute_expr(expr, mapping)),
            },
            Expr::Call { func, args } => Expr::Call {
                func: Box::new(self.substitute_expr(func, mapping)),
                args: args
                    .iter()
                    .map(|arg| self.substitute_expr(arg, mapping))
                    .collect(),
            },
            Expr::Lambda { params, body } => Expr::Lambda {
                params: params.clone(),
                body: Box::new(self.substitute_expr(body, mapping)),
            },
            Expr::Let {
                name,
                mutable,
                ty,
                value,
                then,
            } => Expr::Let {
                name: name.clone(),
                mutable: *mutable,
                ty: ty.clone(),
                value: Box::new(self.substitute_expr(value, mapping)),
                then: Box::new(self.substitute_expr(then, mapping)),
            },
            Expr::Assign { target, value } => Expr::Assign {
                target: Box::new(self.substitute_expr(target, mapping)),
                value: Box::new(self.substitute_expr(value, mapping)),
            },
            Expr::If { cond, then, else_ } => Expr::If {
                cond: Box::new(self.substitute_expr(cond, mapping)),
                then: Box::new(self.substitute_expr(then, mapping)),
                else_: else_
                    .as_ref()
                    .map(|expr| Box::new(self.substitute_expr(expr, mapping))),
            },
            Expr::Match { expr, arms } => Expr::Match {
                expr: Box::new(self.substitute_expr(expr, mapping)),
                arms: arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern.clone(),
                        guard: arm.guard.as_ref().map(|g| self.substitute_expr(g, mapping)),
                        body: self.substitute_expr(&arm.body, mapping),
                    })
                    .collect(),
            },
            Expr::Block(exprs) => Expr::Block(
                exprs
                    .iter()
                    .map(|expr| self.substitute_expr(expr, mapping))
                    .collect(),
            ),
            Expr::Field { expr, field } => Expr::Field {
                expr: Box::new(self.substitute_expr(expr, mapping)),
                field: field.clone(),
            },
            Expr::Method { expr, method, args } => Expr::Method {
                expr: Box::new(self.substitute_expr(expr, mapping)),
                method: method.clone(),
                args: args
                    .iter()
                    .map(|arg| self.substitute_expr(arg, mapping))
                    .collect(),
            },
            Expr::StructInit { name, fields } => {
                let mut new_fields = HashMap::new();
                for (field_name, field_expr) in fields.iter() {
                    new_fields.insert(
                        field_name.clone(),
                        self.substitute_expr(field_expr, mapping),
                    );
                }
                Expr::StructInit {
                    name: name.clone(),
                    fields: new_fields,
                }
            }
            Expr::Pipe { left, right } => Expr::Pipe {
                left: Box::new(self.substitute_expr(left, mapping)),
                right: Box::new(self.substitute_expr(right, mapping)),
            },
            Expr::Return(value) => Expr::Return(
                value
                    .as_ref()
                    .map(|expr| Box::new(self.substitute_expr(expr, mapping))),
            ),
            Expr::Alloc { ty, count } => Expr::Alloc {
                ty: ty.clone(),
                count: count
                    .as_ref()
                    .map(|expr| Box::new(self.substitute_expr(expr, mapping))),
            },
            Expr::Free { ptr } => Expr::Free {
                ptr: Box::new(self.substitute_expr(ptr, mapping)),
            },
            Expr::Ref { expr } => Expr::Ref {
                expr: Box::new(self.substitute_expr(expr, mapping)),
            },
            Expr::Deref { expr } => Expr::Deref {
                expr: Box::new(self.substitute_expr(expr, mapping)),
            },
            Expr::TempScope { body } => Expr::TempScope {
                body: Box::new(self.substitute_expr(body, mapping)),
            },
            Expr::Unsafe { body } => Expr::Unsafe {
                body: Box::new(self.substitute_expr(body, mapping)),
            },
        }
    }

    // Helper methods
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn check(&self, token: &Token) -> bool {
        if let Some(current) = self.peek() {
            std::mem::discriminant(current) == std::mem::discriminant(token)
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if !self.is_at_end() {
            self.pos += 1;
            self.tokens.get(self.pos - 1).map(|(t, _)| t.clone())
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn expect(&mut self, token: &Token) -> ParseResult<()> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("Expected {:?}", token)))
        }
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        match self.peek() {
            Some(Token::Ident(s)) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            Some(Token::SelfKw) => {
                self.advance();
                Ok("self".to_string())
            }
            _ => Err(self.error("Expected identifier")),
        }
    }

    fn expect_string(&mut self) -> ParseResult<String> {
        match self.peek() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            _ => Err(self.error("Expected string literal")),
        }
    }

    fn match_tokens(&mut self, tokens: &[Token]) -> Option<Token> {
        for token in tokens {
            if self.check(token) {
                return self.advance();
            }
        }
        None
    }

    fn span_from(&self, start_pos: usize) -> Span {
        let start = if let Some((_, range)) = self.tokens.get(start_pos) {
            range.start
        } else {
            0
        };

        let end = if self.pos > 0 {
            if let Some((_, range)) = self.tokens.get(self.pos - 1) {
                range.end
            } else {
                start
            }
        } else {
            start
        };

        Span {
            start,
            end,
            file: None,
        }
    }

    fn error(&self, message: &str) -> Diagnostic {
        let range = if let Some((_, span)) = self.tokens.get(self.pos) {
            span.clone()
        } else if let Some((_, span)) = self.tokens.last() {
            span.end..span.end
        } else {
            0..0
        };

        let span = Span::new(range.start, range.end);

        Diagnostic::new(DiagnosticSeverity::Error, message.to_string())
            .with_label(Label::new(span, LabelStyle::Primary).with_message("unexpected token"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_function() {
        let source = "func add(x: Int, y: Int) -> Int { x + y }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_lambda() {
        let source = "func main() { let f = lambda[x: Int, y: Int] { x + y }; f(1, 2) }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_struct() {
        let source = "struct Point { pub x: Int, pub y: Int }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_pipe_operator() {
        let source = "func main() { 5 |> add(3) |> mul(2) }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_namespace_declaration() {
        let source = "as std::io; func add(x: i64, y: i64) -> i64 { x + y }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert!(program.namespace.is_some());
        let namespace = program.namespace.unwrap();
        assert_eq!(namespace.namespace, "std");
        assert_eq!(namespace.filename, "io");
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_use_declaration() {
        let source = "use std::io; func main() -> i64 { 42 }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert!(program.namespace.is_none());
        assert_eq!(program.items.len(), 2);

        match &program.items[0] {
            Item::Use(use_decl) => {
                assert_eq!(use_decl.namespace, "std");
                assert_eq!(use_decl.filename, "io");
                assert!(use_decl.alias.is_none());
            }
            _ => panic!("Expected Use item"),
        }
    }

    #[test]
    fn test_parse_use_declaration_with_alias() {
        let source = "use std::io as stdio; func main() -> i64 { 42 }";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert_eq!(program.items.len(), 2);

        match &program.items[0] {
            Item::Use(use_decl) => {
                assert_eq!(use_decl.namespace, "std");
                assert_eq!(use_decl.filename, "io");
                assert_eq!(use_decl.alias, Some("stdio".to_string()));
            }
            _ => panic!("Expected Use item"),
        }
    }

    #[test]
    fn test_parse_multiple_use_declarations() {
        let source = r#"
            use std::io;
            use math::calc as calculator;
            func main() -> i64 { 42 }
        "#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert_eq!(program.items.len(), 3);

        match &program.items[0] {
            Item::Use(use_decl) => {
                assert_eq!(use_decl.namespace, "std");
                assert_eq!(use_decl.filename, "io");
                assert!(use_decl.alias.is_none());
            }
            _ => panic!("Expected Use item"),
        }

        match &program.items[1] {
            Item::Use(use_decl) => {
                assert_eq!(use_decl.namespace, "math");
                assert_eq!(use_decl.filename, "calc");
                assert_eq!(use_decl.alias, Some("calculator".to_string()));
            }
            _ => panic!("Expected Use item"),
        }
    }

    #[test]
    fn test_attribute_macro_expansion() {
        let source = r#"
            ##func add1(x: i64) -> i64 { x + 1 }

            func main() -> i64 {
                $add1(41)
            }
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert_eq!(program.items.len(), 2);

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.name, "add1");
                assert!(func.is_macro);
            }
            _ => panic!("Expected macro Function item"),
        }

        match &program.items[1] {
            Item::Function(func) => {
                if let Expr::Binary { left, right, .. } = &func.body {
                    match left.as_ref() {
                        Expr::Integer(value) => assert_eq!(*value, 41),
                        other => {
                            panic!("Expected macro substitution into literal, got {:?}", other)
                        }
                    }
                    match right.as_ref() {
                        Expr::Integer(value) => assert_eq!(*value, 1),
                        other => panic!("Expected literal 1 on right-hand side, got {:?}", other),
                    }
                } else {
                    panic!("Expected binary expression in macro-expanded body");
                }
            }
            _ => panic!("Expected Function item"),
        }
    }

    #[test]
    fn test_parse_public_functions_in_namespace() {
        let source = r#"
            as utils::math;
            
            pub func add(x: i64, y: i64) -> i64 {
                x + y
            }
            
            func private_helper() -> i64 {
                42
            }
        "#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert!(program.namespace.is_some());
        let namespace = program.namespace.unwrap();
        assert_eq!(namespace.namespace, "utils");
        assert_eq!(namespace.filename, "math");

        assert_eq!(program.items.len(), 2);

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.name, "add");
                assert!(func.is_pub);
            }
            _ => panic!("Expected Function item"),
        }

        match &program.items[1] {
            Item::Function(func) => {
                assert_eq!(func.name, "private_helper");
                assert!(!func.is_pub);
            }
            _ => panic!("Expected Function item"),
        }
    }

    #[test]
    fn test_parse_namespaced_function_call() {
        let source = r#"
            use std::math;
            
            func main() -> i64 {
                math::add(5, 3)
            }
        "#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        assert_eq!(program.items.len(), 2);

        match &program.items[0] {
            Item::Use(use_decl) => {
                assert_eq!(use_decl.namespace, "std");
                assert_eq!(use_decl.filename, "math");
            }
            _ => panic!("Expected Use item"),
        }

        match &program.items[1] {
            Item::Function(func) => {
                assert_eq!(func.name, "main");
                match &func.body {
                    Expr::Call {
                        func: call_func,
                        args,
                    } => {
                        match call_func.as_ref() {
                            Expr::Ident(name) => {
                                assert_eq!(name, "math::add");
                            }
                            _ => panic!("Expected namespaced identifier in function call"),
                        }
                        assert_eq!(args.len(), 2);
                    }
                    _ => panic!("Expected function call in main body"),
                }
            }
            _ => panic!("Expected Function item"),
        }
    }
}
