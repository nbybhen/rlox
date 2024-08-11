use std::collections::HashMap;

use crate::{
    function::FunctionType,
    interpreter::Interpreter,
    parser::{Expr, Stmt},
    token::Token,
    App,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    app: &'a App,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter, app: &'a App) -> Resolver<'a> {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            app,
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    // Adds variable to the inner-most scope as "not-ready-yet" (false)
    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                self.app.error_token(
                    name.clone(),
                    String::from("Already a variable with this name in this scope."),
                );
            }

            scope.insert(name.lexeme.clone(), false);
        }
    }

    // Sets the variable at the inner-most scope as ready (true)
    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter
                    .resolve(expr, self.scopes.len() - 1 - index);
                return;
            }
        }
    }

    fn resolve_func(&mut self, func: &Stmt, func_type: FunctionType) {
        let enclosing_func = std::mem::replace(&mut self.current_function, func_type);

        self.begin_scope();
        if let Stmt::Function(_, params, body) = func {
            for param in params {
                self.declare(param);
                self.define(param);
            }
            self.resolve(body);
        }

        self.end_scope();
        self.current_function = enclosing_func;
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve(stmts);
                self.end_scope();
            }
            Stmt::Function(name, _, _) => {
                self.declare(name);
                self.define(name);

                self.resolve_func(stmt, FunctionType::Function);
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(expr);
            }
            Stmt::If(condition, then, elses) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then);
                if let Some(stmt) = elses {
                    self.resolve_stmt(stmt);
                }
            }
            Stmt::Var(name, initializer) => {
                self.declare(name);

                if let Some(init) = initializer {
                    self.resolve_expr(init);
                }

                self.define(name);
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr);
            }
            Stmt::Return(keyword, value) => {
                if self.current_function == FunctionType::None {
                    self.app
                        .error_token(keyword.clone(), String::from("Can't return from top-level code."));
                }
                if let Some(expr) = value {
                    if self.current_function == FunctionType::Initializer {
                        self.app.error_token(
                            keyword.clone(),
                            String::from("Can't return a value from an initializer."),
                        )
                    }
                    self.resolve_expr(expr);
                }
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
            Stmt::Class(name, superclass, methods) => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name);
                self.define(name);

                if let Some(superclass) = superclass {
                    if let Expr::Variable {
                        name: superclass_name,
                    } = superclass
                    {
                        if name.lexeme == superclass_name.lexeme {
                            self.app
                                .error_token(name.clone(), String::from("A class can't inherit itself."));
                        }
                        self.current_class = ClassType::Subclass;
                        self.resolve_expr(superclass);
                    }
                    self.begin_scope();
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(String::from("super"), true);
                }

                self.begin_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(String::from("this"), true);

                for method in methods {
                    let mut declaration = FunctionType::Method;
                    if let Stmt::Function(name, _, _) = method {
                        if name.lexeme.eq(&String::from("init")) {
                            declaration = FunctionType::Initializer;
                        }
                    }
                    self.resolve_func(method, declaration);
                }

                self.end_scope();

                if superclass.is_some() {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Super { keyword, .. } => match self.current_class {
                ClassType::None => self
                    .app
                    .error_token(keyword.clone(), String::from("Can't use 'super' outside of a class.")),
                ClassType::Class => self
                    .app
                    .error_token(keyword.clone(), String::from("Can't use 'super' outside of a subclass.")),
                ClassType::Subclass => self.resolve_local(expr, keyword),
            },
            Expr::This { keyword } => {
                if self.current_class == ClassType::None {
                    self.app
                        .error_token(keyword.clone(), String::from("Can't use 'this' outside of a class."));
                }
                self.resolve_local(expr, keyword);
            }
            Expr::Variable { name } => {
                if !self.scopes.is_empty()
                    && self.scopes.last().unwrap().get(&name.lexeme).copied() == Some(false)
                {
                    self.app.error_token(
                        name.clone(),
                        String::from("Can't read local variable in its own initializer"),
                    );
                }
                //println!("Calling Resolve_local!");

                self.resolve_local(expr, name);
            }
            Expr::Assign { name, expr } => {
                self.resolve_expr(expr);
                self.resolve_local(expr, name);
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee);

                for arg in arguments {
                    self.resolve_expr(arg);
                }
            }
            Expr::Grouping { expression } => {
                self.resolve_expr(expression);
            }
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Unary { right, .. } => {
                self.resolve_expr(right);
            }
            Expr::Literal { .. } => {}
            Expr::Get { object, name: _ } => self.resolve_expr(object),
            Expr::Set { object, value, .. } => {
                self.resolve_expr(value);
                self.resolve_expr(object);
            }
        }
    }
}
