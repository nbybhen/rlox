use crate::token::{Token, TokenLiteral, TokenType};
use crate::App;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    // Token name, Option<Expr> initializer
    Var(Token, Option<Expr>),
    // Vec<Stmt> body
    Block(Vec<Stmt>),
    // Box<Expr> condition, Box<Stmt> then, Option<Box<Stmt>> else
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    // Box<Expr> condition, Box<Stmt> inner
    While(Box<Expr>, Box<Stmt>),
    // Token name, Vec<Token> arguments, Vec<Stmt> body
    Function(Token, Vec<Token>, Vec<Stmt>),
    // Token keyword, Expr value
    Return(Token, Option<Expr>),
    // Token name, Vec<Stmt> methods
    Class(Token, Vec<Stmt>),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "Expression: {expr}"),
            Stmt::Block(_) => write!(f, "unformatted (Stmt::Block)"),
            Stmt::Class(_, _) => write!(f, "unformatted (Stmt::Class)"),
            Stmt::Print(expr) => write!(f, "print \"{expr}\";"),
            Stmt::Return(_, _) => write!(f, "unformatted (Stmt::Return)"),
            Stmt::Function(name, args, body) => {
                write!(f, "function {name}({args:?}) {{\n")?;
                for stmt in body {
                    write!(f, "{stmt}\n")?;
                }
                write!(f, "}}")?;

                Ok(())
            }
            Stmt::If(_, _, _) => write!(f, "unformatted (Stmt::If)"),
            Stmt::While(_, _) => write!(f, "unformatted (Stmt::While)"),
            Stmt::Var(name, initializer) => {
                let _ = write!(f, "var {name} = ");
                if initializer.is_some() {
                    initializer.as_ref().inspect(|x| write!(f, "{x};").unwrap());
                } else {
                    let _ = write!(f, "null;");
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Expr {
    Literal {
        value: TokenLiteral,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
    },
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => write!(f, "{value}"),
            Expr::Grouping { expression } => write!(f, "({expression})"),
            Expr::Unary { operator, right } => write!(f, "{operator} {right}"),
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "{left} {operator} {right} "),
            Expr::Variable { name } => write!(f, "{name}"),
            Expr::Assign { name, expr } => write!(f, "{name} = {expr:?}"),
            Expr::Logical {
                left,
                operator,
                right,
            } => write!(f, "{left} {operator} {right}"),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => write!(f, "{callee} {paren} ({arguments:?})"),
            Expr::Get { object, name } => write!(f, "get {object}.{name}"),
            Expr::Set {
                object,
                name,
                value,
            } => write!(f, "set {object} {name} {value}"),
            Expr::This { keyword } => write!(f, "{keyword}"),
        }
    }
}

pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    app: &'a App,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, app: &'a App) -> Parser {
        Parser {
            tokens,
            app,
            current: 0,
        }
    }

    // program -> declaration* EOF ;
    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }
        statements
    }

    // declaration -> varDecl | statement | funDecl | classDecl
    fn declaration(&mut self) -> Option<Stmt> {
        let declaration = if self.match_one_of(&[TokenType::Var]) {
            self.var_declaration()
        } else if self.match_one_of(&[TokenType::Fun]) {
            self.func_declaration("function")
        } else if self.match_one_of(&[TokenType::Class]) {
            self.class_declaration()
        } else {
            self.statement()
        };

        if declaration.is_none() {
            self.synchronize();
        }

        declaration
    }

    // classDecl -> "class" IDENTIFIER "{" function* "}" ;
    fn class_declaration(&mut self) -> Option<Stmt> {
        let name = self
            .consume(TokenType::Identifier, String::from("Expected class name"))
            .unwrap();
        let _ = self.consume(
            TokenType::LeftBrace,
            String::from("Expected \'{\' before class body"),
        );

        let mut methods: Vec<Stmt> = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.func_declaration("function")?);
        }

        let _ = self.consume(
            TokenType::RightBrace,
            String::from("Expected \'}\' after class declaration"),
        );

        Some(Stmt::Class(name, methods))
    }

    // varDecl -> "var" IDENTIFIER ( "=" expression )? ";"
    fn var_declaration(&mut self) -> Option<Stmt> {
        let name: Token = self
            .consume(TokenType::Identifier, String::from("Expect variable name."))
            .ok()?;

        let mut initializer: Option<Expr> = None;
        if self.match_one_of(&[TokenType::Equal]) {
            initializer = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, String::from("Expected a semi-colon"))
            .ok();
        Some(Stmt::Var(name, initializer))
    }

    // funDecl -> "fun" function ;
    fn func_declaration(&mut self, kind: &str) -> Option<Stmt> {
        let name = self
            .consume(TokenType::Identifier, String::from(kind))
            .unwrap();
        let _ = self.consume(
            TokenType::LeftParen,
            String::from(format!("Expect \'(\' after {kind} name.")),
        );
        let mut params: Vec<Token> = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    self.app
                        .error_token(self.peek(), "Can't have more than 255 parameters.");
                }

                params.push(
                    self.consume(
                        TokenType::Identifier,
                        String::from("Expect parameter name."),
                    )
                    .unwrap(),
                );

                if !self.match_one_of(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        let _ = self.consume(
            TokenType::RightParen,
            String::from("Expect\')\' after parameters."),
        );

        let _ = self.consume(
            TokenType::LeftBrace,
            format!("Expect \'{{\' before {kind} body."),
        );
        let body: Vec<Stmt> = self.block_statement()?;

        Some(Stmt::Function(name, params, body))
    }

    // statement -> exprStmt | printStmt | blockStmt | ifStmt | whileStmt | forStmt | returnStmt ;
    fn statement(&mut self) -> Option<Stmt> {
        if self.match_one_of(&[TokenType::Print]) {
            return self.print_statement();
        }

        if self.match_one_of(&[TokenType::LeftBrace]) {
            return Some(Stmt::Block(self.block_statement()?));
        }

        if self.match_one_of(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_one_of(&[TokenType::While]) {
            return self.while_statement();
        }

        if self.match_one_of(&[TokenType::For]) {
            return self.for_statement();
        }

        if self.match_one_of(&[TokenType::Return]) {
            return self.return_statement();
        }

        self.expression_statement()
    }

    // returnStmt -> "return" expression? ";" ;
    fn return_statement(&mut self) -> Option<Stmt> {
        let keyword = self.previous();
        let mut value = None;
        if !self.check(TokenType::Semicolon) {
            value = self.expression();
        }
        let _ = self.consume(
            TokenType::Semicolon,
            String::from("Expected \';\' after return value."),
        );
        Some(Stmt::Return(keyword, value))
    }

    fn for_statement(&mut self) -> Option<Stmt> {
        let _ = self.consume(
            TokenType::LeftParen,
            String::from("Expected \'(\' after while."),
        );

        let initializer: Option<Stmt> = if self.match_one_of(&[TokenType::Semicolon]) {
            None
        } else if self.match_one_of(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let condition: Option<Expr> = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        let _ = self.consume(
            TokenType::Semicolon,
            String::from("Expected \';\' after for condition."),
        );

        let increment: Option<Expr> = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        let _ = self.consume(
            TokenType::RightParen,
            String::from("Expected \')\' after while condition."),
        );

        let mut inner: Stmt = self.statement()?;

        if increment.is_some() {
            inner = Stmt::Block(vec![inner, Stmt::Expression(increment.unwrap())]);
        }

        if condition.is_some() {
            inner = Stmt::While(Box::new(condition.unwrap()), Box::new(inner));
        } else {
            inner = Stmt::While(
                Box::new(Expr::Literal {
                    value: TokenLiteral::Bool(true),
                }),
                Box::new(inner),
            );
        }

        if initializer.is_some() {
            inner = Stmt::Block(vec![initializer.unwrap(), inner]);
        }

        Some(inner)
    }

    // whileStmt -> "while" "(" expression ")" statement ;
    fn while_statement(&mut self) -> Option<Stmt> {
        let _ = self.consume(
            TokenType::LeftParen,
            String::from("Expected \'(\' after while."),
        );
        let condition: Expr = self.expression()?;
        let _ = self.consume(
            TokenType::RightParen,
            String::from("Expected \')\' after while condition."),
        );

        let inner: Stmt = self.statement()?;
        Some(Stmt::While(Box::new(condition), Box::new(inner)))
    }

    // ifStmt -> "if" "(" expression ")" statement ( "else" statement)? ;
    fn if_statement(&mut self) -> Option<Stmt> {
        let _ = self.consume(
            TokenType::LeftParen,
            String::from("Expected \'(\' after if"),
        );
        let condition: Expr = self.expression()?;
        let _ = self.consume(
            TokenType::RightParen,
            String::from("Expected \')\' after if condition"),
        );

        let then_branch: Stmt = self.statement()?;
        let else_branch: Option<Box<Stmt>> = if self.match_one_of(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Some(Stmt::If(
            Box::new(condition),
            Box::new(then_branch),
            else_branch,
        ))
    }

    // block -> "{" declaration "}" ;
    fn block_statement(&mut self) -> Option<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = vec![];

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(
            TokenType::RightBrace,
            String::from("Expected \'}\' after block"),
        )
        .expect("Error");
        Some(statements)
    }

    // printStmt -> "print" expression ";"
    fn print_statement(&mut self) -> Option<Stmt> {
        let val: Expr = self.expression()?;
        let _ = self.consume(
            TokenType::Semicolon,
            String::from("Expected \';\' after statement."),
        );
        Some(Stmt::Print(val))
    }

    // exprStmt -> expression ";"
    fn expression_statement(&mut self) -> Option<Stmt> {
        let val: Expr = self.expression()?;
        let _ = self.consume(
            TokenType::Semicolon,
            String::from("Expected \';\' after statement."),
        );
        Some(Stmt::Expression(val))
    }

    // expression -> assignment ;
    fn expression(&mut self) -> Option<Expr> {
        self.assignment()
    }

    // assignment -> ( call "." )? IDENTIFIER "=" assignment | logic_or;
    fn assignment(&mut self) -> Option<Expr> {
        let expr: Expr = self.or()?;
        if self.match_one_of(&[TokenType::Equal]) {
            let equals: Token = self.previous();
            let value: Expr = self.assignment()?;

            match expr {
                Expr::Variable { name } => {
                    return Some(Expr::Assign {
                        name,
                        expr: Box::new(value),
                    });
                }
                Expr::Get { object, name } => {
                    // let get = Expr::Get { object, name };
                    return Some(Expr::Set {
                        object,
                        name,
                        value: Box::new(value),
                    });
                }
                _ => self.app.error_token(equals, "Invalid assignment target."),
            }
        }

        Some(expr)
    }

    // logic_or -> logic_and ( "or" logic_and )* ;
    fn or(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.and()?;

        while self.match_one_of(&[TokenType::Or]) {
            let operator: Token = self.previous();
            let right: Expr = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Some(expr)
    }

    // logic_and -> equality ( "and" equality )* ;
    fn and(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.equality()?;

        while self.match_one_of(&[TokenType::And]) {
            let operator: Token = self.previous();
            let right: Expr = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Some(expr)
    }

    // equality -> comparison ((!= | ==) comparison)*
    fn equality(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.comparison()?;

        while self.match_one_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Some(expr)
    }

    // comparison -> term ((> | < | >= |<=) term)*
    fn comparison(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.term()?;

        while self.match_one_of(&[
            TokenType::Greater,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::Less,
        ]) {
            let operator: Token = self.previous();
            let right: Expr = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Some(expr)
    }

    // term -> factor ((- | +) factor)*
    fn term(&mut self) -> Option<Expr> {
        let mut expr: Option<Expr> = self.factor();

        while self.match_one_of(&[TokenType::Minus, TokenType::Plus]) {
            let operator: Token = self.previous();
            let right: Expr = self.factor()?;
            expr = Some(Expr::Binary {
                left: Box::new(expr?),
                operator,
                right: Box::new(right),
            })
        }
        expr
    }

    // factor -> unary ((/ | *) unary)*
    fn factor(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.unary()?;

        while self.match_one_of(&[TokenType::Slash, TokenType::Star]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Some(expr)
    }

    // unary -> (! | -) unary | call ;
    fn unary(&mut self) -> Option<Expr> {
        if self.match_one_of(&[TokenType::Bang, TokenType::Minus]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary()?;
            let expr = Expr::Unary {
                operator,
                right: Box::new(right),
            };
            return Some(expr);
        }

        self.call()
    }
    // call -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    fn call(&mut self) -> Option<Expr> {
        let mut expr: Expr = self.primary()?;
        loop {
            if self.match_one_of(&[TokenType::LeftParen]) {
                self.peek();
                expr = self.finish_call(expr)?;
            } else if self.match_one_of(&[TokenType::Dot]) {
                let name: Token = self
                    .consume(
                        TokenType::Identifier,
                        String::from("Expect property name after \'.\'."),
                    )
                    .unwrap();
                expr = Expr::Get {
                    object: Box::new(expr),
                    name,
                }
            } else {
                break;
            }
        }

        Some(expr)
    }

    // primary -> NUMBER | STRING | true | false | nil | "(" expression ")"
    fn primary(&mut self) -> Option<Expr> {
        if self.match_one_of(&[TokenType::False]) {
            return Some(Expr::Literal {
                value: TokenLiteral::Bool(false),
            });
        }
        if self.match_one_of(&[TokenType::True]) {
            return Some(Expr::Literal {
                value: TokenLiteral::Bool(true),
            });
        }
        if self.match_one_of(&[TokenType::Nil]) {
            return Some(Expr::Literal {
                value: TokenLiteral::Nil,
            });
        }

        if self.match_one_of(&[TokenType::Number, TokenType::String]) {
            return Some(Expr::Literal {
                value: self.previous().literal,
            });
        }

        if self.match_one_of(&[TokenType::LeftParen]) {
            let expr = self.expression();
            let _ = self.consume(
                TokenType::RightParen,
                String::from("Expected ')' after expression."),
            );
            return Some(Expr::Grouping {
                expression: Box::new(expr?),
            });
        }

        if self.match_one_of(&[TokenType::Identifier]) {
            return Some(Expr::Variable {
                name: self.previous(),
            });
        }

        if self.match_one_of(&[TokenType::This]) {
            return Some(Expr::This {
                keyword: self.previous(),
            });
        }
        self.app.error_token(self.peek(), "Expected expression.");

        None
    }

    // Helper Methods
    //
    // Checks if one of the given tokens match the current token
    fn match_one_of(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    // Collects function call + arguments
    fn finish_call(&mut self, callee: Expr) -> Option<Expr> {
        let mut arguments: Vec<Expr> = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    self.app
                        .error_token(self.peek(), "Can't have more that 255 arguments");
                    return None;
                }
                arguments.push(self.expression()?);
                if !self.match_one_of(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self
            .consume(
                TokenType::RightParen,
                String::from("Expect \')\' after arguments."),
            )
            .unwrap();

        Some(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn consume(&mut self, tokentype: TokenType, message: String) -> Result<Token, String> {
        if self.check(tokentype) {
            return Ok(self.advance());
        }
        Err(message)
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().tokentype == TokenType::Semicolon {
                return;
            }
            match self.peek().tokentype {
                TokenType::Class
                | TokenType::Var
                | TokenType::If
                | TokenType::For
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    // You should know what this does by now...
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // Checks if the current token equals the given token type
    fn check(&self, tokentype: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().tokentype == tokentype
        }
    }

    // Checks if the next token is EOF (the last token in the string)
    fn is_at_end(&self) -> bool {
        self.peek().tokentype == TokenType::EOF
    }

    // Returns the current token in the sequence
    fn peek(&self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }

    // Returns the previously consumed Token in sequence
    fn previous(&self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }
}

// fn parenthesize(name: String, exprs: &[&Box<Expr>]) -> String {
//     let mut builder: String = String::from("(");
//     builder.push_str(&name);

//     for expr in exprs {
//         builder.push_str(" ");
//         builder.push_str(&print(expr));
//     }
//     builder.push_str(")");

//     builder
// }

// pub fn print(expr: &Expr) -> String {
//     match expr {
//         Expr::Literal { value } => match value {
//             TokenLiteral::Nil => String::from("Nil"),
//             _ => value.to_string(),
//         },
//         Expr::Unary { operator, right } => parenthesize(operator.lexeme.clone(), &[right]),
//         Expr::Grouping { expression } => parenthesize(String::from("group"), &[expression]),
//         Expr::Binary {
//             left,
//             operator,
//             right,
//         } => parenthesize(operator.lexeme.clone(), &[left, right]),
//         _ => String::new(),
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn expression_creation() {
//         let expression = Expr::Binary {
//             left: Box::new(Expr::Unary {
//                 operator: Token {
//                     tokentype: TokenType::Minus,
//                     lexeme: String::from("-"),
//                     literal: TokenLiteral::Nil,
//                     line: 0,
//                 },
//                 right: Box::new(Expr::Literal {
//                     value: TokenLiteral::Number(123.0),
//                 }),
//             }),
//             operator: Token {
//                 tokentype: TokenType::Star,
//                 lexeme: String::from("*"),
//                 literal: TokenLiteral::Nil,
//                 line: 0,
//             },
//             right: Box::new(Expr::Grouping {
//                 expression: Box::new(Expr::Literal {
//                     value: TokenLiteral::Number(45.67),
//                 }),
//             }),
//         };
//         assert_eq!(
//             print(&expression),
//             String::from("(* (- 123) (group 45.67))")
//         );
//     }
//}
