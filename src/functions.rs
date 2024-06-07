use crate::interpreter::{Environment, Error, Instance, Interpreter, Object};
use crate::parser::Stmt;
use crate::token::{Token, TokenLiteral, TokenType};
use std::cell::RefCell;
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
    pub fn bind(&self, instance: Rc<Instance>) -> Self {
        if let Function::Declared {
            declaration,
            closure,
        } = self
        {
            let env = Environment::new(Some(Rc::clone(closure)));
            env.define(String::from("this"), Object::Instance(instance));
            return Function::Declared {
                declaration: declaration.clone(),
                closure: Rc::new(env),
            };
        } else {
            unreachable!()
        }
    }
    pub fn find_method(&self, name: &String) -> Option<Object> {
        match self {
            Function::Class { methods, .. } => {
                // println!("Methods: ");
                // for (key, value) in methods.into_iter() {
                //     println!("{} -> {}, ", key, value);
                // }
                //println!("Method Name: {name}");
                if methods.contains_key(name) {
                    return methods.get(name).cloned();
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
                let instance = Object::Instance(Rc::new(Instance::new(
                    RefCell::new(HashMap::new()),
                    self.clone(),
                )));
                Ok(instance)
            }
            Function::NativeFunc { func, .. } => func(interpreter, &args),
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
            Function::NativeFunc { arity, .. } => *arity,
            Function::Declared { declaration, .. } => {
                if let Stmt::Function(_, params, _) = declaration {
                    return params.len();
                }
                0
            }
            Function::Class { .. } => 0,
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::NativeFunc { name, .. } => write!(f, "<native fn {name}>"),
            Function::Declared { declaration, .. } => write!(f, "<fn {declaration}, closure: ()>"),
            Function::Class { name, methods } => {
                let _ = write!(f, "{name}, Methods: [");

                for (key, value) in methods.into_iter() {
                    let _ = write!(f, "{} -> {}, ", key, value);
                }
                let _ = write!(f, "]");
                Ok(())
            }
        }
    }
}
