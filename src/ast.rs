use crate::token::{self, Literal, Token};

pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(token::Literal<'a>),
    Unary(Token<'a>, Box<Expr<'a>>),
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
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
            },
            Expr::Unary(ref tok, ref rhs) => self.parenthesize(tok.lexeme, &[rhs]),
        }
    }
}
