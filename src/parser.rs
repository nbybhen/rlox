use crate::token::{Token, TokenLiteral};
use crate::tokentype::TokenType;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: TokenLiteral
    },
    Grouping {
        expression: Box<Expr>
    },
    Unary{
        operator: Token,
        right: Box<Expr>
    },
    Binary{
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },
    Variable {
        name: Token
    },
    Assign {
        name: Token,
        expr: Box<Expr>
    },
    
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => write!(f, "{value}"),
            Expr::Grouping { expression } => write!(f, "{:}", print(expression)),
            Expr::Unary { operator, right } => write!(f, "{operator} {:}", print(right)),
            Expr::Binary { left, operator, right } => write!(f, "{operator} {:} {:} ", print(left), print(right)),
            Expr::Variable { name } => write!(f, "{name}"),
            Expr::Assign { name, expr } => write!(f, "{name} = {expr:?}"),
        }
    }
}

pub struct Parser {
    pub tokens: Vec<Token>,
    current: usize
}


impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0
        }
    }

    // program -> declaration* EOF ;
    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        statements
    }

    // declaration -> varStmt | statement 
    fn declaration(&mut self) -> Stmt {
        if self.match_one_of(&[&TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }

    // varDecl -> "var" IDENTIFIER ( "=" expression )? ";"
    fn var_declaration(&mut self) -> Stmt {
        let name: Token = self.consume(TokenType::Identifier, "Expect variable name.").expect("Whoops");

        let mut initializer: Option<Expr> = None;
        if self.match_one_of(&[&TokenType::Equal]) {
            initializer = Some(self.expression());
        }

        self.consume(TokenType::Semicolon, "Expected a semi-colon").expect("Whoops");
        Stmt::Var(name, initializer)
    }

    // statement -> exprStmt | printStmt | blockStmt | ifStmt ;
    fn statement(&mut self) -> Stmt {
        if self.match_one_of(&[&TokenType::Print]) {
            return self.print_statement();
        }

        if self.match_one_of(&[&TokenType::LeftBrace]) {
            return Stmt::Block(self.block_statement());
        }

        if self.match_one_of(&[&TokenType::If]) {
            return self.if_statement();
        }

        self.expression_statement()
    }

    // ifStmt -> "if" "(" expression ")" statement ( "else" statement)? ;
    fn if_statement(&mut self) -> Stmt{
        let _ = self.consume(TokenType::LeftParen, "Expected \'(\' after if");
        let condition: Expr = self.expression();
        let _ = self.consume(TokenType::RightParen, "Expected \')\' after if condition");

        let then_branch: Stmt = self.statement();
        let else_branch = if self.match_one_of(&[&TokenType::Else]) {Some(Box::new(self.statement()))} else {None};


        Stmt::If(Box::new(condition), Box::new(then_branch), else_branch)
    }

    // block -> "{" declaration "}" ;
    fn block_statement(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = vec!();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration());
        }

        self.consume(TokenType::RightBrace, "Expected \'}\' after block").expect("Error");
        statements
    }

    // printStmt -> "print" expression ";"
    fn print_statement(&mut self) -> Stmt {
        let val: Expr = self.expression();
        let _ = self.consume(TokenType::Semicolon, "Expected \';\' after statement.").expect("Whoops");
        Stmt::Print(val)
    }

    // exprStmt -> expression ";"
    fn expression_statement(&mut self) -> Stmt {
        let val: Expr = self.expression();
        let _ = self.consume(TokenType::Semicolon, "Expected \';\' after statement.").expect("Whoops");
        Stmt::Expression(val)
    }

    // expression -> assignment ;
    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    // assignment -> IDENTIFIER "=" assignment | logic_or;
    fn assignment(&mut self) -> Expr {
        let expr: Expr = self.or();
        if self.match_one_of(&[&TokenType::Equal]) {
            let _equals: Token = self.previous();
            let value:Expr =  self.assignment();

            match expr {
                Expr::Variable { name } => {
                    return Expr::Assign { name, expr: Box::new(value) };
                },
                _ => {panic!("Invalid assignment target.")}
            }
        }

        expr
    }

    // logic_or -> logic_and ( "or" logic_and )* ;
    fn or(&mut self) -> Expr {
        let mut expr: Expr = self.and();

        while self.match_one_of(&[&TokenType::Or]) {
            let operator: Token = self.previous();
            let right: Expr = self.and();
            expr = Expr::Logical { left: Box::new(expr), operator, right: Box::new(right)}
        }

        expr
    }

    // logic_and -> equality ( "and" equality )* ;
    fn and(&mut self) -> Expr {
        let mut expr: Expr = self.equality();

        while self.match_one_of(&[&TokenType::And]) {
            let operator: Token = self.previous();
            let right: Expr = self.equality();
            expr = Expr::Logical { left: Box::new(expr), operator, right: Box::new(right) }
        }

        expr
    }

    // equality -> comparison ((!= | ==) comparison)*
    fn equality(&mut self) -> Expr {
        let mut expr: Expr = self.comparison();

        while self.match_one_of(&[&TokenType::BangEqual, &TokenType::EqualEqual]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison();
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right)}
        }
        expr
    }

    // comparison -> term ((> | < | >= |<=) term)*
    fn comparison(&mut self) -> Expr {
        let mut expr: Expr = self.term();

        while self.match_one_of(&[&TokenType::Greater, &TokenType::LessEqual, &TokenType::GreaterEqual, &TokenType::Less]) {
            let operator: Token = self.previous();
            let right: Expr = self.term();
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) }
        }
        expr
    }

    // term -> factor ((- | +) factor)*
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_one_of(&[&TokenType::Minus, &TokenType::Plus]) {
            let operator: Token = self.previous();
            let right: Expr = self.factor();
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) }
        }
        expr
    }

    // factor -> unary ((/ | *) unary)*
    fn factor(&mut self) -> Expr {
        let mut expr: Expr = self.unary();

        while self.match_one_of(&[&TokenType::Slash, &TokenType::Star]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary();
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) }
        }
        expr
    }

    // unary -> (! | -) unary
    fn unary(&mut self) -> Expr {
        if self.match_one_of(&[&TokenType::Bang, &TokenType::Minus]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary();
            let expr = Expr::Unary { operator, right: Box::new(right) };
            return expr;
        }

        self.primary()
    }

    // primary -> NUMBER | STRING | true | false | nil | "(" expression ")"
    fn primary(&mut self) -> Expr {
        if self.match_one_of(&[&TokenType::False]) {
            return Expr::Literal { value: TokenLiteral::Bool { value: false }};
        }
        if self.match_one_of(&[&TokenType::True]) {
            return Expr::Literal { value: TokenLiteral::Bool { value: true } };
        }
        if self.match_one_of(&[&TokenType::Nil]) {
            return Expr::Literal { value: TokenLiteral::Nil }
        }

        if self.match_one_of(&[&TokenType::Number, &TokenType::String]) {
            return Expr::Literal { value: self.previous().literal }
        }

        if self.match_one_of(&[&TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, "Expected ')' after expression.").expect("Parser Error");
            return Expr::Grouping { expression: Box::new(expr) };
        }

        if self.match_one_of(&[&TokenType::Identifier]) {
            return Expr::Variable {name: self.previous() };
        }

        Expr::Literal { value: TokenLiteral::Nil }
    }



    // Helper Methods
    //
    // Checks if one of the given tokens match the current token
    fn match_one_of(&mut self, tokentypes: &[&TokenType]) -> bool{
        for tokentype in tokentypes {
            if self.check(tokentype) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, tokentype: TokenType, message: &'static str) -> Result<Token, &'static str> {
        if self.check(&tokentype) {
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
    fn check(&self, tokentype: &TokenType) -> bool {
        if self.is_at_end() {false} else {self.peek().tokentype == *tokentype} 
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



fn parenthesize(name: String, exprs: &[&Box<Expr>]) -> String {
    let mut builder: String = String::from("(");
    builder.push_str(&name);

    for expr in exprs {
        builder.push_str(" ");
        builder.push_str(&print(expr));
    }
    builder.push_str(")");

    builder
}

pub fn print(expr: &Expr) -> String {
    match expr {
        Expr::Literal{value} => {
            match value {
                TokenLiteral::Nil => "Nil".to_string(),
                _ => value.to_string()
            }
        },
        Expr::Unary{operator, right} => {
            parenthesize(operator.lexeme.clone(), &[right])
        },
        Expr::Grouping{expression} => {
            parenthesize("group".to_string(), &[expression])
        },
        Expr::Binary{left, operator, right} => {
            parenthesize(operator.lexeme.clone(), &[left, right])
        },
        _ => String::new()
    } 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_creation() {
        let expression = Expr::Binary{
                left: Box::new(Expr::Unary{
                    operator: Token { tokentype: TokenType::Minus, lexeme: String::from("-"), literal: TokenLiteral::Nil, line: 0},
                    right: Box::new(Expr::Literal{
                        value: TokenLiteral::Number{value: 123.0}
                    }
        )        }),
                operator: Token { tokentype: TokenType::Star, lexeme: String::from("*"), literal: TokenLiteral::Nil, line: 0},
                right: Box::new(Expr::Grouping { 
                    expression: Box::new(Expr::Literal { 
                        value: TokenLiteral::Number{value: 45.67} 
                    })        
            })
        };
        assert_eq!(print(&expression), String::from("(* (- 123) (group 45.67))"));
    }
}

