use crate::ast::{Expr, Visitor};
use crate::token::{Literal, TokenKind};
use std::{error, fmt, result};

#[derive(Debug)]
pub enum LoxValue {
    Object,
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            LoxValue::Object => todo!(),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Boolean(ref val) => write!(f, "{}", val),
            LoxValue::Number(ref val) => write!(f, "{}", val),
            LoxValue::String(ref val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug)]
pub enum InterpreterError {
    BadOperator,
    BadType,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bad operator")
    }
}

impl error::Error for InterpreterError {}

pub type Result<T> = result::Result<T, InterpreterError>;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue> {
        self.visit_expr(expr)
    }
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        Interpreter::new()
    }
}

impl Visitor<Result<LoxValue>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue> {
        match *expr {
            Expr::Binary(ref lhs, ref op, ref rhs) => todo!(),
            Expr::Grouping(ref expr) => self.visit_expr(expr),
            Expr::Literal(ref lit) => match *lit {
                Literal::String(s) => Ok(LoxValue::String(s.to_string())),
                Literal::Number(n) => Ok(LoxValue::Number(n)),
                Literal::Boolean(b) => Ok(LoxValue::Boolean(b)),
                Literal::None => Ok(LoxValue::Nil),
            },
            Expr::Unary(ref op, ref rhs) => match op.kind {
                TokenKind::Minus => match self.visit_expr(rhs)? {
                    LoxValue::Number(val) => Ok(LoxValue::Number(-val)),
                    _ => Err(InterpreterError::BadType),
                },
                TokenKind::Bang => Ok(LoxValue::Boolean(!is_truthy(&self.visit_expr(rhs)?))),
                _ => Err(InterpreterError::BadOperator),
            },
            Expr::Ternary(ref lhs, ref op1, ref mhs, ref op2, ref rhs) => todo!(),
        }
    }
}

fn is_truthy(val: &LoxValue) -> bool {
    match val {
        LoxValue::Object => todo!(),
        LoxValue::Nil => false,
        LoxValue::Boolean(val) => *val,
        LoxValue::Number(val) => val != &0.0,
        LoxValue::String(val) => !val.is_empty(),
    }
}
