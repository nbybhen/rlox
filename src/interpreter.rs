// Interpreter
use crate::{App, parser::{Expr, Stmt}, token::{TokenLiteral, Token}, tokentype::TokenType};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Number(f32),
    Bool(bool),
    Nil
}

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Object>,
    enclosing: Option<Box<Environment>>
}

impl Environment {
    pub fn new(env: Option<Box<Environment>>) -> Environment {
        Environment { values: HashMap::new(), enclosing: env}
    }

    // Writes a variable into the global environment
    fn define(&mut self, name: String, value: Object) -> () {
        //println!("Value being inserted : {value:?}");
        self.values.insert(name, value);
        //println!("Current environment: {:?}", self.values);
    }

    // Gets a variable from the global environment
    fn get(&mut self, name: Token) -> Result<Object, (Token, String)> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values.get(&name.lexeme).unwrap().clone());
        }
        if self.enclosing.is_some() {
            return self.enclosing.as_mut().unwrap().get(name);
        }

        Err((name.clone(), format!("Undefined variable: {name}")))
    }

    // Reassigns a variable within the global environment if it exists 
    fn assign(&mut self, name: &Token, value: &Object) -> Result<(), String>{
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }
        if self.enclosing.is_some(){
            return self.enclosing.as_mut().expect("Enclosing was wrong type").assign(name, value);
        }
        
        Err(String::from("Undefined variable."))
    }
}

pub struct Interpreter {
    environment: Environment
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter{ environment: Environment::new(None) }
    }

    pub fn interpret(&mut self, statements: &Vec<Stmt>, app: &App) -> Result<(), (Token, &'static str)> {
        for stmt in statements {
            //println!("Stmt: {stmt:?}");
            match self.decide(stmt) {
                Ok(_) => {},
                Err((token, msg)) => {
                    app.runtime_error(token, &msg);
                    break;
                }
            }
        }
        Ok(())
    }

    fn decide(&mut self, stmt: &Stmt) -> Result<(), (Token, String)> {
        match stmt {
            Stmt::Print(e) => {
                println!("{:?}",self.evaluate(e)?);
            },
            Stmt::Expression(e) => {
                let _ = self.evaluate(e)?;
            },
            Stmt::Var(n, v) => {
                if let Some(expr) = v {
                    let value = self.evaluate(expr);
                    self.environment.define(n.lexeme.clone(), value.unwrap());
                }
                else {
                    self.environment.define(n.lexeme.clone(), Object::Nil);
                }
            },
            Stmt::Block(stmts) => {
                //println!("Pre-block Env: {:?}", self.environment);
                self.execute_block(stmts, Environment::new(Some(Box::new(self.environment.clone()))))?;
            },
            Stmt::If(condition, then, el) => {
                let res = self.evaluate(condition).expect("Couldn't read if condition.");
                if is_truthy(&res) {
                    self.decide(then)?;
                }
                else if let Some(v) = el {
                    self.decide(&v)?;
                }
            },
            Stmt::While(condition, inner) => {
                while is_truthy(&self.evaluate(condition).unwrap()) {
                    self.decide(inner)?;
                }
            }
        }
        Ok(())
    }

    fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), (Token, String)> {
        let _ = std::mem::replace(&mut self.environment, env);

        for stmt in stmts {
            self.decide(&stmt)?;
        }

        self.environment = *self.environment.enclosing.clone().unwrap();
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Object, (Token, String)>  {
        match expr {
            Expr::Literal { value } => match value {
                TokenLiteral::String ( value ) => Ok(Object::String(String::from(value))),
                TokenLiteral::Number ( value ) => Ok(Object::Number(*value)),
                TokenLiteral::Bool ( value ) => Ok(Object::Bool(*value)),
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
                            Err((operator.clone(), String::from("Not a number")))
                        }
                    },
                    TokenType::Bang => Ok(Object::Bool(!is_truthy(&right))),
                    _ => Err((operator.clone(), String::from("Not a valid unary operator.")))
                }
            },
            Expr::Binary { left, operator, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                //println!("Left: {left:?}\nRight: {right:?}");

                match operator.tokentype {
                    TokenType::Minus => {
                        if let Ok((l, r)) = self.check_numbers(&left, &right) {
                            Ok(Object::Number(l - r))
                        }
                        else {
                            Err((operator.clone(), String::from("Not a valid unary operator.")))
                        }
                    },
                    TokenType::Slash => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l / r)),
                            Err(msg) => Err((operator.clone(), msg))
                        }
                    },
                    TokenType::Star => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l*r)),
                            Err(msg) => Err((operator.clone(), msg))
                        }
                    },
                    TokenType::Plus => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::String(format!("{} {}", l, r))),
                            _ => Err((operator.clone(), String::from("\'+\' can only be used on two numbers or two strings")))
                        }
                    },
                    TokenType::Greater => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l > r)),
                            Err(msg) => Err((operator.clone(), msg))
                        }
                    },
                    TokenType::GreaterEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l>=r)),
                            Err(msg) => Err((operator.clone(), msg))
                    }
                    },
                    TokenType::Less => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<r)),
                            Err(msg) => Err((operator.clone(), msg))
                        }
                    },
                    TokenType::LessEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<=r)),
                            Err(msg) => Err((operator.clone(), msg))
                        }
                    },
                    TokenType::EqualEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l == r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l == r)),
              
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l == r)),
                            _ => Err((operator.clone(), String::from("Can't check equality between different types")))
                        }
                    },
                    TokenType::BangEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l != r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l != r)),
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l != r)),
                            _ => Err((operator.clone(), String::from("Can't check equality between different types")))
                        }
                    }
                    _ => Err((operator.clone(), String::from("Not a valid binary operation")))
                }
            },
            Expr::Variable { name } => Ok(self.environment.get(name.clone())?),
            Expr::Assign { name, expr } => {
                let value: Object = self.evaluate(expr)?;
                let _ = self.environment.assign(name, &value);
                Ok(value)
            },
            Expr::Logical { left, operator, right } => {
                let left = self.evaluate(left);

                match operator.tokentype {
                    TokenType::Or => {
                        if is_truthy(&left.clone()?) {
                            return left;
                        }
                    },
                    _ => {
                        if !is_truthy(&left.clone()?) {
                            return left;
                        }
                    }
                }

                self.evaluate(right)
            }
            Expr::Call {callee, paren: _, arguments: _} => {
                let callee: Object = self.evaluate(callee).unwrap();

                Ok(callee)
            }
        }
    }

    // Returns the numerical value from within Object enum
    fn check_number(&self, num: &Object) -> Result<f32, String> {
        match num {
            Object::Number(i) => Ok(*i),
            _ => Err(String::from("Input must be numerical objects"))
        }
    }
    fn check_numbers(&self, left: &Object, right: &Object) -> Result<(f32,f32), String> {
        match (left, right) {
            (Object::Number(l), Object::Number(r)) => Ok((*l,*r)),
            _ => Err(String::from("Inputs must be numerical objects"))
        }
    }
}

// Returns whether an object is considered "truthy" or not
    fn is_truthy(object: &Object) -> bool {
        match *object {
            Object::Bool(x) => x,
            Object::Nil => false,
            _ => true
        }
    }
  

