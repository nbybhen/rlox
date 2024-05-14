use std::rc::Rc;
use crate::interpreter::{Environment, Interpreter, Object, Error};
use crate::parser::Stmt;
use crate::token::{Token, TokenLiteral, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct StaticFunc {
    name: String,
    _arity: usize,
    func: fn(&Interpreter, &Vec<Object>) -> Result<Object, Error>
}

impl StaticFunc {
    pub fn new(name: &str, arity: usize, func: fn(&Interpreter, &Vec<Object>) -> Result<Object, Error>) -> StaticFunc {
        StaticFunc{
            name: name.to_owned(),
            _arity: arity,
            func
        }
    }
    fn arity(&self) -> usize {
        self._arity
    }
    fn call(&self, interpreter: &Interpreter, arguments: Vec<Object>) -> Result<Object, Error> {
        (self.func)(interpreter, &arguments)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    declaration: Stmt,
    closure: Rc<Environment>
}

impl LoxFunction {
    pub fn new(declaration: Stmt, closure: Rc<Environment>) -> LoxFunction {
        LoxFunction{declaration, closure}
    }
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, Error> {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));

        match &self.declaration {
            Stmt::Function(_, params, body) => {
                for i in 0..params.len() {
                    env.define(params[i].lexeme.to_owned(), args[i].clone());
                }

                match interpreter.execute_block(body, env) {
                    Err(Error::Return(value)) => Ok(value),
                    _ => Ok(Object::Nil)
                }

            }
            _ => Err(Error::Error(Token{tokentype: TokenType::Nil, lexeme: String::new(), literal: TokenLiteral::Nil, line: 0}, String::from("unreachable")))
        }
    }

    fn arity(&self) -> usize {
        if let Stmt::Function(_,params,_) = &self.declaration {
            return params.len();
        }
        0
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Function::NativeFunc(static_func) => write!(f, "<native fn {}>", static_func.name),
            Function::Declared(lox_func) => write!(f, "<fn lox>")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function{
    NativeFunc(StaticFunc),
    Declared(LoxFunction)
}

impl Function{

    pub fn call(&self, interpreter:&mut Interpreter, args: Vec<Object>) -> Result<Object, Error>{
        match self {
            Function::NativeFunc(static_func) => static_func.call(interpreter, args),
            Function::Declared(lox_func) => lox_func.call(interpreter, args)
        }
    }
    pub fn arity(&self) -> usize {
        match self {
            Function::NativeFunc(static_func) => static_func.arity(),
            Function::Declared(lox_func) => lox_func.arity(),
            _ => 0
        }
    }
}