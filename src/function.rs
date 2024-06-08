use crate::environment::Environment;
use crate::interpreter::{Error, Instance, Interpreter, Object};
use crate::parser::Stmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq)]
pub enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
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
        is_init: bool,
    },
    Class {
        name: String,
        methods: HashMap<String, Object>,
    },
}

impl Function {
    pub fn bind(&self, instance: Rc<Instance>) -> Self {
        println!("Before binding: {self}\n");
        if let Function::Declared {
            declaration,
            closure,
            is_init,
        } = self
        {
            let env = Environment::new(Some(Rc::clone(closure)));
            env.define(String::from("this"), Object::Instance(instance));
            let temp = Function::Declared {
                declaration: declaration.clone(),
                closure: Rc::new(env),
                is_init: *is_init,
            };
            println!("After Binding: {temp}\n");
            return temp;
        } else {
            unreachable!()
        }
    }
    pub fn find_method(&self, name: &String) -> Option<Object> {
        match self {
            Function::Class { methods, .. } => {
                if methods.contains_key(name) {
                    return methods.get(name).cloned();
                }
                None
            }
            _ => None,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, Error> {
        match self {
            Function::Class { .. } => {
                // Might cause issues down the line
                let instance = Rc::new(Instance::new(RefCell::new(HashMap::new()), self.clone()));
                if let Some(Object::Callable(initializer)) = self.find_method(&String::from("init"))
                {
                    println!("RUNNING INIT");
                    initializer
                        .bind(Rc::clone(&instance))
                        .call(interpreter, args)?;
                }

                Ok(Object::Instance(instance))
            }
            Function::NativeFunc { func, .. } => func(interpreter, &args),
            Function::Declared {
                declaration,
                closure,
                is_init,
            } => {
                let env = Environment::new(Some(Rc::clone(&closure)));

                if let Stmt::Function(_, params, body) = declaration {
                    for i in 0..params.len() {
                        env.define(params[i].lexeme.to_owned(), args[i].clone());
                    }

                    if *is_init {
                        return Ok(closure.get_at(0, String::from("this")));
                    }

                    match interpreter.execute_block(body, env) {
                        Err(Error::Return(value)) => {
                            println!("Error value? {value}");
                            Ok(value)
                        }
                        _ => Ok(Object::Nil),
                    }
                } else {
                    unreachable!()
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
            Function::Class { .. } => {
                if let Some(Object::Callable(initializer)) = self.find_method(&String::from("init"))
                {
                    initializer.arity()
                } else {
                    0
                }
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::NativeFunc { name, .. } => write!(f, "<native fn {name}>"),
            Function::Declared {
                declaration,
                closure,
                is_init,
            } => {
                write!(
                    f,
                    "<fn {declaration}, closure: {closure}, is_init: {is_init}>\n"
                )
            }
            Function::Class { name, methods } => {
                let _ = write!(f, "{name}, Methods: [");

                for (key, value) in methods.into_iter() {
                    let _ = write!(f, "{} -> {},", key, value);
                }
                let _ = write!(f, "]");
                Ok(())
            }
        }
    }
}
