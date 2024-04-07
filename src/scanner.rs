use crate::token::{Token, TokenLiteral};
use crate::tokentype::TokenType;
use std::collections::HashMap;


pub struct Scanner<'a> {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
    keywords: HashMap<&'a str, TokenType>
}

impl<'a> Scanner<'a> {
    pub fn build(inp: String) -> Scanner<'a> {
        let mut keywords: HashMap<&str, TokenType> = HashMap::new();
        keywords.insert("and", TokenType::And);
        keywords.insert("class", TokenType::Class);
        keywords.insert("else", TokenType::Else);
        keywords.insert("false", TokenType::False);
        keywords.insert("for", TokenType::For);
        keywords.insert("fun", TokenType::Fun);
        keywords.insert("if", TokenType::If);
        keywords.insert("nil", TokenType::Nil);
        keywords.insert("or", TokenType::Or);
        keywords.insert("print", TokenType::Print);
        keywords.insert("return", TokenType::Return);
        keywords.insert("super", TokenType::Super);
        keywords.insert("this", TokenType::This);
        keywords.insert("true", TokenType::True);
        keywords.insert("var", TokenType::Var);
        keywords.insert("while", TokenType::While);

        Scanner {
            source: inp, 
            tokens: vec![], 
            start: 0, 
            current: 0, 
            line: 1,
            keywords
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn add_token(&mut self, ty: TokenType) {
        let text: &str = &self.source[self.start..self.current];
        self.tokens.push(Token{
            tokentype: ty, 
            lexeme: text.to_string(),
            literal: TokenLiteral::Nil,
            line: self.line
        });
    }

    // Strings and Numbers
    fn add_string_token(&mut self, ty: TokenType, text: &str) {
        self.tokens.push(Token {
            tokentype: ty,
            lexeme: text.to_string(),
            literal: TokenLiteral::String{value: text.to_string()},
            line: self.line
        });
    }

    // Numbers
    fn add_number_token(&mut self, ty: TokenType, value: f32) {
        self.tokens.push(Token {
            tokentype: ty,
            lexeme: value.to_string(),
            literal: TokenLiteral::Number{value},
            line: self.line
        });
    }

    // Consumes next charater in source and returns it
    fn advance(&mut self) -> char {
        self.current += 1;
        //println!("self.current value: {:?}", self.current);
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn scan_token(&mut self) -> () {
        let c = self.advance();
        println!("c value: {c}");

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => if self.match_char('=') {self.add_token(TokenType::BangEqual)} else {self.add_token(TokenType::Bang)},
            '=' => if self.match_char('=') {self.add_token(TokenType::EqualEqual)} else {self.add_token(TokenType::Equal)},
            '<' => if self.match_char('=') {self.add_token(TokenType::LessEqual)} else {self.add_token(TokenType::EqualEqual)},
            '>' => if self.match_char('=') {self.add_token(TokenType::GreaterEqual)} else {self.add_token(TokenType::Greater)},
            // Different due to possibility of comment (e.g. //info) OR division operation
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                else {
                    self.add_token(TokenType::Slash);
                }
            },
            ' ' | '\r' | '\t' => {},
            '\n' => self.line += 1,
            '\"' => self.string(),
            _ =>  {
                if c.is_numeric() {
                    self.number();
                }
                else if c.is_alphabetic() {
                    self.identifier();
                }
            }       
        }
    }

    // Creates Identifier / Keyword token(s)
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        match self.keywords.get(text) {
            Some(v) => self.add_token(*v),
            None => self.add_token(TokenType::Identifier)
        }
    }

    // Creates numeric token
    fn number(&mut self) {
        while self.peek().is_numeric() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while self.peek().is_numeric() {
                self.advance();
            }
        }
        self.add_number_token(TokenType::Number, self.source[self.start..self.current].to_string().parse::<f32>().unwrap());
    }

    // Peeks two characters ahead into the source
    fn peek_next(&self) -> char{
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        
        self.source.chars().nth(self.current + 1).expect("Peeked character wasn't a valid character")
    }

    // Gets the entire string and puts it into a token
    fn string(&mut self) {
        while self.peek() != '\"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            eprintln!("Unterminated string!");
            std::process::exit(1);
        }

        // Eats the closing " mark
        self.advance();

        let value: &str = &self.source[self.start + 1..self.current - 1].to_string();
        self.add_string_token(TokenType::String, value);
    }


    // Returns the NEXT character without incrementing self.current
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        } 
        self.source.chars().nth(self.current).expect("Peeked character wasn't a valid character")
    }
    
    // Checks if the NEXT character equals the expected
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }
        self.current += 1;
        true 
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token>{
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token{
            tokentype: TokenType::EOF,
            lexeme: String::new(),
            literal: TokenLiteral::Nil,
            line: self.line});

        &self.tokens
    }
}
