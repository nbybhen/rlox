use crate::{
    environment::Environment,
    function::Function,
    parser::{Expr, Stmt},
    token::{Token, TokenLiteral, TokenType},
    App,
};
use std::rc::Rc;
use std::time::SystemTime;
use std::{borrow::Borrow, cell::RefCell};
use std::{collections::HashMap, fmt::Debug};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Number(f32),
    Bool(bool),
    // Function class, HashMap<String, Object> fields
    Instance(Rc<Instance>),
    Callable(Rc<Function>),
    Nil,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Instance(instance) => {
                // write!(f, "instance");
                write!(f, "instance, fields: {{")?;
                for (key, value) in instance.fields.borrow().iter() {
                    write!(f, "{} -> {}, ", key, value)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Object::Callable(_) => {
                write!(f, "<callable>")
            }
            Object::Nil => write!(f, "Nil"),
            Object::String(s) => write!(f, "\"{s}\""),
            Object::Number(n) => write!(f, "{n}"),
            Object::Bool(b) => write!(f, "{b}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    fields: RefCell<HashMap<String, Object>>,
    class: Function,
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Instance {} fields: {:?}", self.class, self.fields)
    }
}

impl Instance {
    pub fn new(fields: RefCell<HashMap<String, Object>>, class: Function) -> Self {
        Self { fields, class }
    }

    pub fn set(&self, name: Token, value: Object) {
        self.fields.borrow_mut().insert(name.lexeme, value);
    }

    pub fn get(&self, name: Token, instance: Rc<Instance>) -> Result<Object, Error> {
        // Checks if a property value exists, else if a method exists with a matching name.
        if let Some(field) = self.fields.borrow().get(&name.lexeme) {
            return Ok(field.clone());
        } else if let Some(method) = self.class.find_method(&name.lexeme) {
            if let Object::Callable(func) = method {
                if let Function::Declared { .. } = Rc::borrow(&func) {
                    // Rc::new(self.clone()) vs Rc::clone(self) due to no extension function on Rc<Instance>
                    return Ok(Object::Callable(Rc::new(func.bind(instance))));
                } else {
                    unreachable!();
                }
            } else {
                unreachable!();
            }
        } else {
            unreachable!()
        }
    }
}

// trait InstanceGet {
//     fn get(&self, name: Token) -> Result<Object, Error>;
// }

// impl InstanceGet for Rc<Instance> {
//     fn get(&self, name: Token) -> Result<Object, Error> {
//         // Checks if a property value exists, else if a method exists with a matching name.
//         if let Some(field) = self.fields.borrow().get(&name.lexeme) {
//             return Ok(field.clone());
//         } else if let Some(method) = self.class.find_method(&name.lexeme) {
//             if let Object::Callable(func) = method {
//                 if let Function::Declared { .. } = Rc::borrow(&func) {
//                     // Rc::new(self.clone()) vs Rc::clone(self) due to no extension function on Rc<Instance>
//                     return Ok(Object::Callable(Rc::new(func.bind(self.clone()))));
//                 } else {
//                     unreachable!();
//                 }
//             } else {
//                 unreachable!();
//             }
//         } else {
//             unreachable!()
//         }
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    //Break,
    Return(Object),
    Error(Token, String),
}

pub struct Interpreter {
    environment: Rc<Environment>,
    pub globals: Rc<Environment>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Rc::new(Environment::new(None));
        globals.define(
            String::from("clock"),
            Object::Callable(Rc::new(Function::NativeFunc {
                name: String::from("clock"),
                arity: 0,
                func: |_, _| {
                    Ok(Object::Number(
                        SystemTime::now()
                            .duration_since(SystemTime::UNIX_EPOCH)
                            .expect("")
                            .as_millis() as f32,
                    ))
                },
            })),
        );

        Interpreter {
            environment: Rc::clone(&globals),
            globals,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(
        &mut self,
        statements: &Vec<Stmt>,
        app: &App,
    ) -> Result<(), (Token, &'static str)> {
        for stmt in statements {
            match self.decide(stmt) {
                Ok(_) => {}
                Err(Error::Error(token, msg)) => {
                    app.runtime_error(token, &msg);
                    break;
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    fn decide(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Print(e) => {
                // println!("print");
                println!("{}", self.evaluate(e)?);
            }
            Stmt::Expression(e) => {
                self.evaluate(e)?;
            }
            Stmt::Var(n, v) => {
                if let Some(expr) = v {
                    let value = self.evaluate(expr);
                    self.environment.define(n.lexeme.clone(), value.unwrap());
                } else {
                    self.environment.define(n.lexeme.clone(), Object::Nil);
                }
            }
            Stmt::Block(stmts) => {
                self.execute_block(stmts, Environment::new(Some(Rc::clone(&self.environment))))?;
            }
            Stmt::If(condition, then, el) => {
                let res = self
                    .evaluate(condition)
                    .expect("Couldn't read if condition.");
                if is_truthy(&res) {
                    self.decide(then)?;
                } else if let Some(v) = el {
                    self.decide(&v)?;
                }
            }
            Stmt::While(condition, inner) => {
                // println!("while");
                while is_truthy(&self.evaluate(condition).unwrap()) {
                    self.decide(inner)?;
                }
            }
            Stmt::Function(name, _, _) => {
                let func: Object = Object::Callable(Rc::new(Function::Declared {
                    declaration: stmt.clone(),
                    closure: Rc::clone(&self.environment),
                    is_init: false,
                }));
                self.environment.define(name.clone().lexeme, func);
            }
            Stmt::Return(_, value) => {
                let ret = match value {
                    Some(value_expr) => self.evaluate(value_expr)?,
                    None => Object::Nil,
                };

                return Err(Error::Return(ret));
            }
            Stmt::Class(name, superclass, methods) => {
                let mut eval_superclass: Option<Object> = None;

                if let Some(superclass) = superclass {
                    eval_superclass = Some(self.evaluate(superclass)?);
                    match &eval_superclass {
                        Some(Object::Callable(func)) => {
                            if let Function::Class { .. } = **func {
                            } else {
                                return Err(Error::Error(
                                    name.clone(),
                                    String::from("Superclass must be a class"),
                                ));
                            }
                        }
                        _ => {}
                    }
                }

                self.environment.define(name.lexeme.clone(), Object::Nil);

                // let prev_env = self.environment.clone();

                if superclass.is_some() {
                    self.environment =
                        Rc::new(Environment::new(Some(Rc::clone(&self.environment))));
                    self.environment
                        .define(String::from("super"), eval_superclass.clone().unwrap())
                }

                let mut hash_methods: HashMap<String, Object> = HashMap::new();

                for method in methods {
                    // This might not work as intended
                    if let Stmt::Function(name, _, _) = method {
                        let function: Object = Object::Callable(Rc::new(Function::Declared {
                            declaration: method.clone(),
                            closure: Rc::clone(&self.environment),
                            is_init: name.lexeme.eq(&String::from("init")),
                        }));
                        hash_methods.insert(name.lexeme.clone(), function);
                    }
                }

                let class: Object = Object::Callable(Rc::new(Function::Class {
                    name: name.lexeme.clone(),
                    superclass: eval_superclass,
                    methods: hash_methods,
                }));

                //
                if superclass.is_some() {
                    // self.environment = prev_env;
                    self.environment = Rc::clone(&self.environment.enclosing.clone().unwrap());
                }

                let _ = self.environment.assign(name, class);
            }
        }
        Ok(())
    }

    pub fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), Error> {
        let previous = std::mem::replace(&mut self.environment, Rc::new(env));

        for stmt in stmts {
            match self.decide(&stmt) {
                Ok(()) => {}
                Err(err) => {
                    self.environment = previous;
                    return Err(err);
                }
            }
        }

        self.environment = previous;
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Object, Error> {
        // println!("{}", expr);
        match expr {
            Expr::Literal { value } => match value {
                TokenLiteral::String(value) => Ok(Object::String(String::from(value))),
                TokenLiteral::Number(value) => Ok(Object::Number(*value)),
                TokenLiteral::Bool(value) => Ok(Object::Bool(*value)),
                TokenLiteral::Nil => Ok(Object::Nil),
            },
            Expr::Grouping { expression } => self.evaluate(expression),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right)?;

                match operator.tokentype {
                    TokenType::Minus => {
                        if let Ok(i) = self.check_number(&right) {
                            Ok(Object::Number(-i))
                        } else {
                            Err(Error::Error(operator.clone(), String::from("Not a number")))
                        }
                    }
                    TokenType::Bang => Ok(Object::Bool(!is_truthy(&right))),
                    _ => Err(Error::Error(
                        operator.clone(),
                        String::from("Not a valid unary operator."),
                    )),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match operator.tokentype {
                    TokenType::Minus => {
                        if let Ok((l, r)) = self.check_numbers(&left, &right) {
                            Ok(Object::Number(l - r))
                        } else {
                            Err(Error::Error(
                                operator.clone(),
                                String::from("Not a valid unary operator."),
                            ))
                        }
                    }
                    TokenType::Slash => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Number(l / r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::Star => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Number(l * r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::Plus => match (left, right) {
                        (Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
                        (Object::String(l), Object::String(r)) => {
                            Ok(Object::String(format!("{}{}", l, r)))
                        }
                        _ => Err(Error::Error(
                            operator.clone(),
                            String::from("\'+\' can only be used on two numbers or two strings"),
                        )),
                    },
                    TokenType::Greater => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Bool(l > r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::GreaterEqual => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Bool(l >= r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::Less => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Bool(l < r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::LessEqual => match self.check_numbers(&left, &right) {
                        Ok((l, r)) => Ok(Object::Bool(l <= r)),
                        Err(msg) => Err(Error::Error(operator.clone(), msg)),
                    },
                    TokenType::EqualEqual => match (left, right) {
                        (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l == r)),
                        (Object::String(l), Object::String(r)) => Ok(Object::Bool(l == r)),

                        (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l == r)),
                        _ => Err(Error::Error(
                            operator.clone(),
                            String::from("Can't check equality between different types"),
                        )),
                    },
                    TokenType::BangEqual => match (left, right) {
                        (Object::Number(l), Object::Number(r)) => Ok(Object::Bool(l != r)),
                        (Object::String(l), Object::String(r)) => Ok(Object::Bool(l != r)),
                        (Object::Bool(l), Object::Bool(r)) => Ok(Object::Bool(l != r)),
                        _ => Err(Error::Error(
                            operator.clone(),
                            String::from("Can't check equality between different types"),
                        )),
                    },
                    _ => Err(Error::Error(
                        operator.clone(),
                        String::from("Not a valid binary operation"),
                    )),
                }
            }
            Expr::Variable { name } => self.lookup_variable(name, expr),
            Expr::Assign { name, expr } => {
                let value: Object = self.evaluate(expr)?;

                if let Some(dist) = self.locals.get(expr) {
                    self.environment.assign_at(*dist, name, value.clone());
                } else {
                    let _ = self.environment.assign(name, value.clone());
                }

                Ok(value)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left);

                match operator.tokentype {
                    TokenType::Or => {
                        if is_truthy(&left.clone()?) {
                            return left;
                        }
                    }
                    _ => {
                        if !is_truthy(&left.clone()?) {
                            return left;
                        }
                    }
                }

                self.evaluate(right)
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate(callee)?;
                let mut eval_args: Vec<Object> = Vec::new();

                for argument in arguments {
                    let arg = self.evaluate(argument)?;
                    eval_args.push(arg);
                }

                if let Object::Callable(function) = callee.clone() {
                    if arguments.len() != function.arity() {
                        return Err(Error::Error(
                            paren.clone(),
                            format!(
                                "Expected {} argument(s) but got {}.",
                                function.arity(),
                                arguments.len()
                            ),
                        ));
                    }
                    return function.call(self, eval_args);
                } else {
                    return Err(Error::Error(
                        paren.clone(),
                        String::from("Can only call functions and classes."),
                    ));
                }
            }
            Expr::Get { object, name } => {
                let object = self.evaluate(object)?;
                if let Object::Instance(instance) = object {
                    let ret = instance.get(name.clone(), Rc::clone(&instance));
                    return ret;
                }

                Err(Error::Error(
                    name.clone(),
                    format!("Only instances have properties"),
                ))
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                let mut object = self.evaluate(object)?;

                if let Object::Instance(instance) = &mut object {
                    let value = self.evaluate(value)?;
                    instance.set(name.clone(), value.clone());

                    Ok(value)
                } else {
                    Err(Error::Error(
                        name.clone(),
                        String::from("Only instances have fields."),
                    ))
                }
            }
            Expr::This { keyword } => self.lookup_variable(keyword, expr),
            Expr::Super { method, .. } => {
                let dist = self.locals.get(expr).expect("Unable to calculate dist.");

                // LoxClass = Object::Callable?
                let superclass = self.environment.get_at(*dist, String::from("super"));

                // LoxInstance = Object::Instance(Rc<Instance>)
                let object = self.environment.get_at(dist - 1, String::from("this"));

                if let Object::Callable(class) = superclass {
                    let eval_method = class.find_method(&method.lexeme);

                    if eval_method.is_none() {
                        return Err(Error::Error(
                            method.clone(),
                            format!("Undefined property {}.", method.lexeme),
                        ));
                    }
                    if let Object::Instance(instance) = object {
                        if let Some(Object::Callable(func)) = eval_method {
                            return Ok(Object::Callable(Rc::new(func.bind(instance))));
                        }
                    }
                }
                Ok(Object::Nil)
            }
        }
    }

    // Returns the numerical value from within Object enum
    fn check_number(&self, num: &Object) -> Result<f32, String> {
        match num {
            Object::Number(i) => Ok(*i),
            _ => Err(String::from("Input must be numerical objects")),
        }
    }
    fn check_numbers(&self, left: &Object, right: &Object) -> Result<(f32, f32), String> {
        match (left, right) {
            (Object::Number(l), Object::Number(r)) => Ok((*l, *r)),
            _ => Err(String::from("Inputs must be numerical objects")),
        }
    }

    fn lookup_variable(&self, name: &Token, expr: &Expr) -> Result<Object, Error> {
        let distance = self.locals.get(expr);
        if let Some(dist) = distance {
            return Ok(self.environment.get_at(*dist, name.lexeme.clone()));
        } else {
            return self.globals.get(name.clone());
        }
    }
}

// Returns whether an object is considered "truthy" or not
fn is_truthy(object: &Object) -> bool {
    match *object {
        Object::Bool(x) => x,
        Object::Nil => false,
        _ => true,
    }
}
