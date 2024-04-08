// Interpreter

use crate::{parser::Expr, token::TokenLiteral, tokentype::TokenType};

#[derive(Debug, PartialEq)]
pub enum Object {
    String(String),
    Number(f32),
    Bool(bool),
    Nil
}

pub struct Interpreter {
    //
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter{}
    }

    pub fn interpret(&self, expr: &Expr) -> Result<Object, String> {
        self.evaluate(expr)
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Object, String>  {
        match expr {
            Expr::Literal { value } => match value {
                TokenLiteral::String { value } => Ok(Object::String(value.to_string())),
                TokenLiteral::Number { value } => Ok(Object::Number(*value)),
                TokenLiteral::Bool { value } => Ok(Object::Bool(*value)),
                TokenLiteral::Nil => Ok(Object::Nil)
            },
            Expr::Grouping { expression } => self.evaluate(expression),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right)?;

                match operator.tokentype {
                    TokenType::Minus => {
                        if let Ok(i) = self.check_number(&right) {
                            Ok(Object::Number(-i))
                        }
                        else {
                            Err("Not a number".to_string())
                        }
                    },
                    TokenType::Bang => Ok(Object::Bool(!self.is_truthy(right))),
                    _ => Err("Not a valid unary operator.".to_string())
                }
            },
            Expr::Binary { left, operator, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match operator.tokentype {
                    TokenType::Minus => {
                        if let Ok((l, r)) = self.check_numbers(&left, &right) {
                            Ok(Object::Number(l - r))
                        }
                        else {
                            Err("Not a valid unary operator.".to_string())
                        }
                    },
                    TokenType::Slash => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l / r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::Star => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l*r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::Plus => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::String(format!("{} {}", l, r))),
                            _ => Err("Plus can only be used on two numbers or two strings".to_string())
                        }
                    },
                    TokenType::Greater => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l > r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::GreaterEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l>=r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::Less => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::LessEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<=r)),
                            Err(msg) => Err(msg)
                        }
                    },
                    TokenType::EqualEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l == r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l == r)),
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l == r)),
                            _ => Err("Can't check equality between different types".to_string())
                        }
                    },
                    TokenType::BangEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l != r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l != r)),
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l != r)),
                            _ => Err("Can't check equality between different types".to_string())
                        }
                    }
                    _ => Err("Not a valid binary operation".to_string())
                }

            }
        }
    }

    // Returns the numerical value from within Object enum
    fn check_number(&self, num: &Object) -> Result<f32, String> {
        match num {
            Object::Number(i) => Ok(*i),
            _ => Err("Input must be numerical objects".to_string())
        }
    }
    fn check_numbers(&self, left: &Object, right: &Object) -> Result<(f32,f32), String> {
        match (left, right) {
            (Object::Number(l), Object::Number(r)) => Ok((*l,*r)),
            _ => Err("Inputs must be numerical objects".to_string())
        }
    }

    // Returns whether an object is considered "truthy" or not
    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Bool(x) => x,
            Object::Nil => false,
            _ => true
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::token::Token;

    use super::*;

    #[test]
    fn addition() {
        let interpreter = Interpreter::new();
        let expression = Expr::Binary { left: Box::new(Expr::Literal { value: TokenLiteral::Number { value: 10.0 } }),
            operator: Token{
                tokentype: TokenType::Plus, 
                lexeme: "+".to_string(), 
                literal: TokenLiteral::Nil,
                line: 0}, 
            right: Box::new(Expr::Literal { value: TokenLiteral::Number { value: 10.0 } }) };
        assert_eq!(interpreter.interpret(&expression).expect(""), Object::Number(20.0));
    }
}


