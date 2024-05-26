use std::collections::HashMap;

use crate::{
    functions::FunctionType,
    interpreter::Interpreter,
    parser::{Expr, Stmt},
    token::Token,
    App,
};

#[allow(dead_code)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    app: &'a App,
    current_function: FunctionType,
}

#[allow(dead_code)]
impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter, app: &'a App) -> Resolver<'a> {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            app,
            current_function: FunctionType::None,
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
                    "Already a variable with this name in this scope.",
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
        //println!("Current Scopes: {:?}", self.scopes);
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            //println!("Depth: {:?}", self.scopes.len() - 1 - index);
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
                        .error_token(keyword.clone(), "Can't return from top-level code.");
                }
                if let Some(expr) = value {
                    self.resolve_expr(expr);
                }
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable { name } => {
                //println!("Self scopes: {:?}", self.scopes);
                if (!self.scopes.is_empty()
                    && self.scopes.last().unwrap().get(&name.lexeme).copied() == Some(false))
                {
                    self.app.error_token(
                        name.clone(),
                        "Can't read local variable in its own initializer",
                    );
                }
                //println!("Calling Resolve_local!");

                self.resolve_local(expr, name);
            }
            Expr::Assign { name, expr } => {
                self.resolve_expr(expr);
                self.resolve_local(expr, name);
            }
            Expr::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expr(callee);

                for arg in arguments {
                    self.resolve_expr(arg);
                }
            }
            Expr::Grouping { expression } => {
                self.resolve_expr(expression);
            }
            Expr::Logical {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Unary { operator: _, right } => {
                self.resolve_expr(right);
            }
            Expr::Literal { value: _ } => {}
        }
    }

    // fn decide(&mut self, stmt: &Stmt) {
    //     match stmt {
    //         Stmt::Block(stmts) => {
    //             self.begin_scope();
    //         }
    //         _ => {}
    //     }
    //     self.begin_scope();
    // }
}
