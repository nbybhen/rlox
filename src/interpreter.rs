use std::cell::RefCell;
use crate::{App, parser::{Expr, Stmt}, token::{TokenLiteral, Token, TokenType}, functions::{Function}};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Number(f32),
    Bool(bool),
    Callable(Function),
    Nil
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Break,
    Return(Object),
    Error(Token, String)
}


#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: RefCell<HashMap<String, Object>>,
    enclosing: Option<Rc<Environment>>
}

impl Environment {
    pub fn new(env: Option<Rc<Environment>>) -> Environment {
        Environment { values: RefCell::new(HashMap::new()), enclosing: env}
    }

    // Writes a variable into the global environment
    pub fn define(&self, name: String, value: Object) -> () {
        self.values.borrow_mut().insert(name, value);
    }

    // Gets a variable from the global environment
    pub fn get(&self, name: Token) -> Result<Object, Error> {
        if self.values.borrow().contains_key(&name.lexeme) {
            return Ok(self.values.borrow().get(&name.lexeme).unwrap().clone());
        }
        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().get(name);
        }

        Err(Error::Error(name.clone(), format!("Undefined variable: {name}")))
    }

    // Reassigns a variable within the global environment if it exists 
    pub fn assign(&self, name: &Token, value: &Object) -> Result<(), String>{
        if self.values.borrow().contains_key(&name.lexeme) {
            self.values.borrow_mut().insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }
        if self.enclosing.is_some(){
            return self.enclosing.as_ref().expect("Enclosing was wrong type").assign(name, value);
        }
        
        Err(String::from("Undefined variable."))
    }
}

pub struct Interpreter {
    environment: Rc<Environment>,
    pub globals: Rc<Environment>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut globals = Rc::new(Environment::new(None));
        globals.define(String::from("clock"), Object::Callable(Function::NativeFunc{name:String::from("clock"), arity:0, func:|_, _| {
            Ok(Object::Number(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("").as_millis() as f32))
        }}));

        Interpreter{ environment: Rc::clone(&globals),  globals}
    }

    pub fn interpret(&mut self, statements: &Vec<Stmt>, app: &App) -> Result<(), (Token, &'static str)> {
        for stmt in statements {
            match self.decide(stmt) {
                Ok(_) => {},
                Err(Error::Error(token, msg)) => {
                    app.runtime_error(token, &msg);
                    break;
                },
                _ => {}
            }
        }
        Ok(())
    }

    fn decide(&mut self, stmt: &Stmt) -> Result<(), Error> {
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
                self.execute_block(stmts, Environment::new(Some(Rc::clone(&self.environment))))?;
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
            },
            Stmt::Function(name, params, body) => {
                let func: Object = Object::Callable(Function::Declared{declaration: stmt.clone(), closure: Rc::clone(&self.environment)});
                self.environment.define(name.clone().lexeme, func);
            },
            Stmt::Return(keyword, value) => {
                let ret = match value {
                    Some(value_expr) => self.evaluate(value_expr)?,
                    None => Object::Nil
                };

                return Err(Error::Return(ret))
            }
        }
        Ok(())
    }

    pub fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), Error> {
        let previous = std::mem::replace(&mut self.environment, Rc::new(env));

        for stmt in stmts {
            match self.decide(&stmt) {
                Ok(()) => {},
                Err(err) => {
                    self.environment = previous;
                    return Err(err);
                }
            }
        }

        self.environment = previous;
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Object, Error>  {
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
                            Err(Error::Error(operator.clone(), String::from("Not a number")))
                        }
                    },
                    TokenType::Bang => Ok(Object::Bool(!is_truthy(&right))),
                    _ => Err(Error::Error(operator.clone(), String::from("Not a valid unary operator.")))
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
                            Err(Error::Error(operator.clone(), String::from("Not a valid unary operator.")))
                        }
                    },
                    TokenType::Slash => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l / r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                        }
                    },
                    TokenType::Star => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Number(l*r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                        }
                    },
                    TokenType::Plus => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::String(format!("{}{}", l, r))),
                            _ => Err(Error::Error(operator.clone(), String::from("\'+\' can only be used on two numbers or two strings")))
                        }
                    },
                    TokenType::Greater => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l > r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                        }
                    },
                    TokenType::GreaterEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l>=r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                    }
                    },
                    TokenType::Less => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                        }
                    },
                    TokenType::LessEqual => {
                        match self.check_numbers(&left, &right) {
                            Ok((l,r)) => Ok(Object::Bool(l<=r)),
                            Err(msg) => Err(Error::Error(operator.clone(), msg))
                        }
                    },
                    TokenType::EqualEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l == r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l == r)),
              
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l == r)),
                            _ => Err(Error::Error(operator.clone(), String::from("Can't check equality between different types")))
                        }
                    },
                    TokenType::BangEqual => {
                        match (left, right) {
                            (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l != r)),
                            (Object::String(l), Object::String(r)) => Ok(Object::Bool(l != r)),
                            (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l != r)),
                            _ => Err(Error::Error(operator.clone(), String::from("Can't check equality between different types")))
                        }
                    }
                    _ => Err(Error::Error(operator.clone(), String::from("Not a valid binary operation")))
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
            Expr::Call {callee, paren, arguments} => {
                let callee= self.evaluate(callee)?;
                let mut eval_args: Vec<Object> = Vec::new();

                for argument in arguments {
                    eval_args.push(self.evaluate(argument)?)
                }

                if let Object::Callable(function) = callee.clone() {
                    if arguments.len() != function.arity() {
                        return Err(Error::Error(paren.clone(), format!("Expected {} arguments but got {}.", function.arity(), arguments.len())));
                    }
                    return function.call(self, eval_args);
                }
                else {
                    return Err(Error::Error(paren.clone(), String::from("Can only call functions and classes.")));
                }
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
  

