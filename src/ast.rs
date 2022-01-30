use crate::token::{self, Literal, Token, TokenKind, Tokenizer};
use std::{error, fmt, iter::Peekable, result};

#[derive(Debug)]
pub struct ParseErrorToken {
    kind: TokenKind,
    lexeme: String,
    line: usize,
}

#[derive(Debug)]
pub struct ParseError {
    message: &'static str,
    token: ParseErrorToken,
}

pub type Result<'a> = result::Result<Expr<'a>, ParseError>;

fn report(
    f: &mut fmt::Formatter<'_>,
    line: usize,
    message: &str,
    location: Option<&str>,
) -> fmt::Result {
    match location {
        Some(location) => write!(f, "[line {}] Error{}: {}", line, location, message),
        None => write!(f, "[line {}] Error: {}", line, message),
    }
}

impl<'a> fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.token.kind {
            TokenKind::Eof => report(f, self.token.line, self.message, Some(" at end")),
            _ => report(
                f,
                self.token.line,
                self.message,
                Some(&format!(" at '{}'", self.token.lexeme)),
            ),
        }
    }
}

impl<'a> error::Error for ParseError {}

#[derive(Debug)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(token::Literal<'a>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Ternary(
        Box<Expr<'a>>,
        Token<'a>,
        Box<Expr<'a>>,
        Token<'a>,
        Box<Expr<'a>>,
    ),
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

macro_rules! define_rule {
    ($rule:ident, $next:ident, $pat:pat) => {
        fn $rule(&mut self) -> Result<'a> {
            let mut expr = self.$next()?;

            while let Some($pat) = self.tokenizer.peek().map(|token| &token.kind) {
                let operator = self.tokenizer.next().unwrap();
                let right = self.$next()?;
                expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            }

            Ok(expr)
        }
    };
}

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Parser<'a> {
        Parser {
            tokenizer: tokenizer.peekable(),
        }
    }

    #[inline]
    fn expression(&mut self) -> Result<'a> {
        self.comma()
    }

    fn unary(&mut self) -> Result<'a> {
        if let Some(TokenKind::Bang | TokenKind::Minus) =
            self.tokenizer.peek().map(|token| &token.kind)
        {
            let operator = self.tokenizer.next().unwrap();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn ternary(&mut self) -> Result<'a> {
        let mut expr = self.equality()?;

        let mut sections = Vec::new();
        while let Some(TokenKind::Question) = self.tokenizer.peek().map(|token| token.kind) {
            let operator_left = self.tokenizer.next().unwrap();
            let operand_left = self.ternary()?;
            match self.tokenizer.peek() {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    let operator_right = self.tokenizer.next().unwrap();
                    let operand_right = self.equality()?;
                    sections.push((operator_left, operator_right, operand_left, operand_right));
                }
                _ => panic!("invalid token"),
            }
        }

        if !sections.is_empty() {
            let mut sections = sections.into_iter().rev();
            let mut section = sections.next().unwrap();
            for next_section in sections {
                section = (
                    next_section.0,
                    next_section.1,
                    next_section.2,
                    Expr::Ternary(
                        Box::new(next_section.3),
                        section.0,
                        Box::new(section.2),
                        section.1,
                        Box::new(section.3),
                    ),
                );
            }

            expr = Expr::Ternary(
                Box::new(expr),
                section.0,
                Box::new(section.2),
                section.1,
                Box::new(section.3),
            );
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<'a> {
        match self.tokenizer.peek() {
            Some(token) => match token.kind {
                TokenKind::True => {
                    self.tokenizer.next();
                    Ok(Expr::Literal(Literal::Boolean(true)))
                }
                TokenKind::False => {
                    self.tokenizer.next();
                    Ok(Expr::Literal(Literal::Boolean(false)))
                }
                TokenKind::Nil => {
                    self.tokenizer.next();
                    Ok(Expr::Literal(Literal::None))
                }
                TokenKind::Number | TokenKind::String => {
                    let token = self.tokenizer.next().unwrap();
                    Ok(Expr::Literal(token.literal))
                }
                TokenKind::LeftParen => {
                    self.tokenizer.next();
                    let expr = self.expression()?;
                    match self.tokenizer.peek() {
                        Some(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }) => {
                            self.tokenizer.next();
                        }
                        Some(token) => {
                            return Err(ParseError {
                                message: "expect ')' after expression",
                                token: ParseErrorToken {
                                    kind: token.kind,
                                    lexeme: token.lexeme.to_string(),
                                    line: token.line,
                                },
                            })
                        }
                        None => panic!("unexpected eof"),
                    }

                    Ok(expr)
                }
                _ => Err(ParseError {
                    message: "expect expression",
                    token: ParseErrorToken {
                        kind: token.kind,
                        lexeme: token.lexeme.to_string(),
                        line: token.line,
                    },
                }),
            },
            None => panic!("unexpected eof"),
        }
    }

    define_rule!(comma, ternary, TokenKind::Comma);

    define_rule!(
        equality,
        comparison,
        TokenKind::BangEqual | TokenKind::EqualEqual
    );

    define_rule!(
        comparison,
        term,
        TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
    );

    define_rule!(term, factory, TokenKind::Minus | TokenKind::Plus);

    define_rule!(factory, unary, TokenKind::Slash | TokenKind::Star);

    #[inline]
    fn is_at_end(&mut self) -> bool {
        matches!(
            self.tokenizer.peek(),
            Some(Token {
                kind: TokenKind::Eof,
                ..
            })
        )
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokenizer.peek().map(|token| token.kind) {
            Some(TokenKind::Eof) => None,
            Some(_) => Some(self.expression()),
            None => None,
        }
    }
}

pub mod debug {
    use super::*;

    pub struct AstFormatter;

    impl AstFormatter {
        fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> String {
            let mut s = String::new();
            s.push('(');
            s.push_str(name);
            for expr in exprs {
                s.push(' ');
                s.push_str(&self.visit_expr(expr))
            }
            s.push(')');
            s
        }

        pub fn format(&mut self, expr: &Expr) -> String {
            self.visit_expr(expr)
        }
    }

    impl Visitor<String> for AstFormatter {
        fn visit_expr(&mut self, expr: &Expr) -> String {
            match *expr {
                Expr::Binary(ref lhs, ref tok, ref rhs) => {
                    self.parenthesize(tok.lexeme, &[lhs, rhs])
                }
                Expr::Grouping(ref expr) => self.parenthesize("group", &[expr]),
                Expr::Literal(ref tok) => match *tok {
                    Literal::String(val) => val.to_string(),
                    Literal::Number(val) => val.to_string(),
                    Literal::None => "nil".to_string(),
                    Literal::Boolean(b) => b.to_string(),
                },
                Expr::Unary(ref tok, ref rhs) => self.parenthesize(tok.lexeme, &[rhs]),
                Expr::Ternary(ref lhs, ref ltok, ref mhs, ref rtok, ref rhs) => {
                    self.parenthesize(&format!("{}{}", ltok.lexeme, rtok.lexeme), &[lhs, mhs, rhs])
                }
            }
        }
    }
}
