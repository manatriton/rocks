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

impl<'a> From<Literal<'a>> for LoxValue {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::String(val) => LoxValue::String(val.to_string()),
            Literal::Number(val) => LoxValue::Number(val),
            Literal::Boolean(val) => LoxValue::Boolean(val),
            Literal::None => LoxValue::Nil,
        }
    }
}

impl<'a, 'b> From<&'a Literal<'b>> for LoxValue {
    fn from(lit: &'a Literal<'b>) -> Self {
        match lit {
            Literal::String(val) => LoxValue::String(val.to_string()),
            Literal::Number(val) => LoxValue::Number(*val),
            Literal::Boolean(val) => LoxValue::Boolean(*val),
            Literal::None => LoxValue::Nil,
        }
    }
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

macro_rules! number {
    ($val:ident) => {{
        match $val {
            LoxValue::Number(val) => val,
            _ => return Err(InterpreterError::BadType),
        }
    }};
}

impl Visitor<Result<LoxValue>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue> {
        match *expr {
            Expr::Binary(ref lhs, ref op, ref rhs) => {
                let lhs = self.evaluate(lhs)?;
                let rhs = self.evaluate(rhs)?;
                match op.kind {
                    TokenKind::Minus => Ok(LoxValue::Number(number!(lhs) - number!(rhs))),
                    TokenKind::Slash => Ok(LoxValue::Number(number!(lhs) / number!(rhs))),
                    TokenKind::Star => Ok(LoxValue::Number(number!(lhs) * number!(rhs))),
                    TokenKind::Plus => match (lhs, rhs) {
                        (LoxValue::Number(lhs), LoxValue::Number(rhs)) => {
                            Ok(LoxValue::Number(lhs + rhs))
                        }
                        (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                            Ok(LoxValue::String(lhs + &rhs))
                        }
                        _ => Err(InterpreterError::BadType),
                    },
                    _ => Err(InterpreterError::BadOperator),
                }
            }
            Expr::Grouping(ref expr) => self.visit_expr(expr),
            Expr::Literal(ref lit) => Ok(LoxValue::from(lit)),
            Expr::Unary(ref op, ref rhs) => match op.kind {
                TokenKind::Minus => match self.visit_expr(rhs)? {
                    LoxValue::Number(val) => Ok(LoxValue::Number(-val)),
                    _ => Err(InterpreterError::BadType),
                },
                TokenKind::Bang => Ok(LoxValue::Boolean(!is_truthy(&self.visit_expr(rhs)?))),
                _ => Err(InterpreterError::BadOperator),
            },
            Expr::Ternary(ref lhs, ref op1, ref mhs, ref op2, ref rhs) => {
                match (op1.kind, op2.kind) {
                    (TokenKind::Question, TokenKind::Colon) => {
                        let lhs = self.evaluate(lhs)?;
                        self.evaluate(if is_truthy(&lhs) { mhs } else { rhs })
                    }
                    _ => Err(InterpreterError::BadOperator),
                }
            }
        }
    }
}

#[inline]
fn is_truthy(val: &LoxValue) -> bool {
    match val {
        LoxValue::Object => todo!(),
        LoxValue::Nil => false,
        LoxValue::Boolean(val) => *val,
        LoxValue::Number(val) => val != &0.0,
        LoxValue::String(val) => !val.is_empty(),
    }
}
