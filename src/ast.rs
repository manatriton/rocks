use crate::token::{self, Literal, Token, TokenKind};
use std::iter::Peekable;

pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(token::Literal<'a>),
    Unary(Token<'a>, Box<Expr<'a>>),
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

macro_rules! define_rule {
    ($rule:ident, $next:ident, $pat:pat) => {
        fn $rule(&mut self) -> Expr<'a> {
            let mut expr = self.$next();

            while let Some($pat) = self.tokenizer.peek().map(|token| &token.kind) {
                let operator = self.tokenizer.next().unwrap();
                let right = self.$next();
                expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            }

            expr
        }
    };
}

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
            Expr::Binary(ref lhs, ref tok, ref rhs) => self.parenthesize(tok.lexeme, &[lhs, rhs]),
            Expr::Grouping(ref expr) => self.parenthesize("group", &[expr]),
            Expr::Literal(ref tok) => match *tok {
                Literal::String(val) => val.to_string(),
                Literal::Number(val) => val.to_string(),
                Literal::None => "nil".to_string(),
                Literal::Boolean(b) => b.to_string(),
            },
            Expr::Unary(ref tok, ref rhs) => self.parenthesize(tok.lexeme, &[rhs]),
        }
    }
}

use crate::token::Tokenizer;

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
    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }

    fn unary(&mut self) -> Expr<'a> {
        if let Some(TokenKind::Bang | TokenKind::Minus) =
            self.tokenizer.peek().map(|token| &token.kind)
        {
            let operator = self.tokenizer.next().unwrap();
            let right = self.unary();
            Expr::Unary(operator, Box::new(right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr<'a> {
        match self.tokenizer.peek().map(|token| &token.kind) {
            Some(TokenKind::True) => {
                self.tokenizer.next();
                Expr::Literal(Literal::Boolean(true))
            }
            Some(TokenKind::False) => {
                self.tokenizer.next();
                Expr::Literal(Literal::Boolean(false))
            }
            Some(TokenKind::Nil) => {
                self.tokenizer.next();
                Expr::Literal(Literal::None)
            }
            Some(TokenKind::Number | TokenKind::String) => {
                let token = self.tokenizer.next().unwrap();
                Expr::Literal(token.literal)
            }
            Some(TokenKind::LeftParen) => {
                let expr = self.expression();
                let next = self.tokenizer.next().unwrap();
                match next.kind {
                    TokenKind::RightParen => Expr::Grouping(Box::new(expr)),
                    _ => panic!("expected ')' after expression"),
                }
            }
            _ => panic!("unexpected token"),
        }
    }

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
}

impl<'a> Iterator for Parser<'a> {
    type Item = Expr<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
