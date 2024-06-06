use crate::interpreter::{Environment, Error, Interpreter, Object};
use crate::parser::Stmt;
use crate::token::{Token, TokenLiteral, TokenType};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq)]
pub enum FunctionType {
    None,
    Function,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    NativeFunc {
        name: String,
        arity: usize,
        func: fn(&Interpreter, &Vec<Object>) -> Result<Object, Error>,
    },
    Declared {
        declaration: Stmt,
        closure: Rc<Environment>,
    },
    Class {
        name: String,
        methods: HashMap<String, Object>,
    },
}

impl Function {
    pub fn find_method(&self, name: String) -> Option<Object> {
        match self {
            Function::Class { methods, .. } => {
                // println!("Methods: ");
                // for (key, value) in methods.into_iter() {
                //     println!("{} -> {}, ", key, value);
                // }
                //println!("Method Name: {name}");
                if methods.contains_key(&name) {
                    return methods.get(&name).cloned();
                }
                //println!("Couldn't find method: {name}");
                None
            }
            _ => None,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, Error> {
        match self {
            Function::Class { .. } => {
                // Might cause issues down the line
                let instance = Object::Instance(self.clone(), HashMap::new());
                Ok(instance)
            }
            Function::NativeFunc {
                name: _,
                arity: _,
                func,
            } => func(interpreter, &args),
            Function::Declared {
                declaration,
                closure,
            } => {
                let env = Environment::new(Some(Rc::clone(&closure)));

                match &declaration {
                    Stmt::Function(_, params, body) => {
                        for i in 0..params.len() {
                            env.define(params[i].lexeme.to_owned(), args[i].clone());
                        }

                        match interpreter.execute_block(body, env) {
                            Err(Error::Return(value)) => Ok(value),
                            _ => Ok(Object::Nil),
                        }
                    }
                    _ => Err(Error::Error(
                        Token {
                            tokentype: TokenType::Nil,
                            lexeme: String::new(),
                            literal: TokenLiteral::Nil,
                            line: 0,
                        },
                        String::from("unreachable"),
                    )),
                }
            }
        }
    }
    pub fn arity(&self) -> usize {
        match self {
            Function::NativeFunc {
                name: _,
                arity,
                func: _,
            } => *arity,
            Function::Declared {
                declaration,
                closure: _,
            } => {
                if let Stmt::Function(_, params, _) = declaration {
                    return params.len();
                }
                0
            }
            Function::Class { name: _, .. } => 0,
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::NativeFunc {
                name,
                arity: _,
                func: _,
            } => write!(f, "<native fn {name}>"),
            Function::Declared {
                declaration: _,
                closure: _,
            } => write!(f, "<fn lox>"),
            Function::Class { name, methods } => {
                write!(f, "{name}, Methods: [");

                for (key, value) in methods.into_iter() {
                    write!(f, "{} -> {}, ", key, value);
                }
                write!(f, "]");
                Ok(())
            }
        }
    }
}
