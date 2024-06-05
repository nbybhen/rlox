use crate::{
    functions::Function,
    parser::{Expr, Stmt},
    token::{Token, TokenLiteral, TokenType},
    App,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Number(f32),
    Bool(bool),
    // Function class, HashMap<String, Object> fields
    Instance(Function, HashMap<String, Object>),
    Callable(Function),
    Nil,
}

impl Object {
    fn bind(&self) -> Result<Object, Error> {
        // This may not work as intended (binding clone of closure)
        if let Object::Callable(func) = self {
            match func {
                Function::Declared {
                    declaration,
                    closure,
                } => {
                    let env = Environment::new(Some(Rc::clone(closure)));
                    env.define(String::from("this"), self.clone());
                    return Ok(Object::Callable(Function::Declared {
                        declaration: declaration.clone(),
                        closure: Rc::new(env),
                    }));
                }
                _ => unreachable!("Calling binding must be on a Function::Declared"),
            }
        }
        unreachable!("Calling binding must be on an Object::Callable.");
    }

    fn get(&self, name: &Token) -> Result<Object, Error> {
        match self {
            Object::Instance(class, fields) => {
                if fields.contains_key(&name.lexeme) {
                    return fields.get(&name.lexeme.clone()).cloned().ok_or_else(|| {
                        Error::Error(
                            name.clone(),
                            format!("Undefined property: {:}", name.lexeme),
                        )
                    });
                }
                let method = class.find_method(name.lexeme.clone());
                // Unsure if this is supposed to be an Object::Instance or Object::Callable
                if method.is_some() {
                    //println!("Method found! {:?}", method);
                    return method.unwrap().bind();
                }

                unreachable!("Must be a Function::Class instance to access methods")
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn set(&mut self, name: &Token, value: &Object) {
        match self {
            Object::Instance(func, fields) => {
                if let Function::Class { name: _, .. } = func {
                    fields.insert(name.lexeme.clone(), value.clone());
                }
            }
            _ => unreachable!("Must be an Object::Instance to call set()."),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    //Break,
    Return(Object),
    Error(Token, String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: RefCell<HashMap<String, Object>>,
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(env: Option<Rc<Environment>>) -> Environment {
        Environment {
            values: RefCell::new(HashMap::new()),
            enclosing: env,
        }
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

        Err(Error::Error(
            name.clone(),
            format!("Undefined variable: {name}"),
        ))
    }

    // Reassigns a variable within the global environment if it exists
    pub fn assign(&self, name: &Token, value: &Object) -> Result<(), String> {
        if self.values.borrow().contains_key(&name.lexeme) {
            self.values
                .borrow_mut()
                .insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }
        if self.enclosing.is_some() {
            return self
                .enclosing
                .as_ref()
                .expect("Enclosing was wrong type")
                .assign(name, value);
        }

        Err(String::from("Undefined variable."))
    }

    pub fn assign_at(&self, dist: usize, name: &Token, value: &Object) {
        self.ancestor(dist)
            .values
            .borrow_mut()
            .insert(name.lexeme.clone(), value.clone());
    }

    fn ancestor(&self, dist: usize) -> Environment {
        let mut env = self;
        for _ in 0..dist {
            if let Some(inner) = env.enclosing.as_ref() {
                env = inner;
            }
        }

        env.clone()
    }

    fn get_at(&self, dist: usize, name: String) -> Object {
        self.ancestor(dist)
            .values
            .borrow()
            .get(&name)
            .unwrap()
            .clone()
    }
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
            Object::Callable(Function::NativeFunc {
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
            }),
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
                println!("{:?}", self.evaluate(e)?);
            }
            Stmt::Expression(e) => {
                let _ = self.evaluate(e)?;
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
                while is_truthy(&self.evaluate(condition).unwrap()) {
                    self.decide(inner)?;
                }
            }
            Stmt::Function(name, _, _) => {
                let func: Object = Object::Callable(Function::Declared {
                    declaration: stmt.clone(),
                    closure: Rc::clone(&self.environment),
                });
                self.environment.define(name.clone().lexeme, func);
            }
            Stmt::Return(_, value) => {
                let ret = match value {
                    Some(value_expr) => self.evaluate(value_expr)?,
                    None => Object::Nil,
                };

                return Err(Error::Return(ret));
            }
            Stmt::Class(name, methods) => {
                self.environment.define(name.lexeme.clone(), Object::Nil);

                let mut hash_methods: HashMap<String, Object> = HashMap::new();

                for method in methods {
                    // This might not work as intended
                    if let Stmt::Function(name, _, _) = method {
                        let function: Object = Object::Callable(Function::Declared {
                            declaration: method.clone(),
                            closure: Rc::clone(&self.environment),
                        });
                        hash_methods.insert(name.lexeme.clone(), function);
                    }
                }

                let class: Object = Object::Callable(Function::Class {
                    name: name.lexeme.clone(),
                    methods: hash_methods,
                });
                let _ = self.environment.assign(name, &class);
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
                    self.environment.assign_at(*dist, name, &value);
                }

                let _ = self.environment.assign(name, &value);
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
                    eval_args.push(self.evaluate(argument)?)
                }

                if let Object::Callable(function) = callee.clone() {
                    if arguments.len() != function.arity() {
                        return Err(Error::Error(
                            paren.clone(),
                            format!(
                                "Expected {} arguments but got {}.",
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
                if let Object::Instance(_, _) = object {
                    return object.get(name);
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

                if let Object::Instance(_, _) = object {
                    let value = self.evaluate(value)?;
                    object.set(name, &value);
                    Ok(value)
                } else {
                    Err(Error::Error(
                        name.clone(),
                        String::from("Only instances have fields."),
                    ))
                }
            }
            Expr::This { keyword } => self.lookup_variable(keyword, expr),
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

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Instance(Function::Class { name, .. }, _) => write!(f, "{name} instance"),
            _ => write!(f, "unformatted"),
        }
    }
}
