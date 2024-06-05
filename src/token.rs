use std::fmt;
use std::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum TokenType {
    // SC Tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // DC Tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
}

// pub struct ToFraction {
//     integral: u32,
//     fractional: u32,
// }

#[derive(Debug, Clone)]
pub enum TokenLiteral {
    String(String),
    Number(f32),
    Bool(bool),
    Nil,
}

impl Hash for TokenLiteral {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        match *self {
            TokenLiteral::Number(float) => state.write(&float.to_le_bytes()),
            _ => {}
        }
    }
}

impl PartialEq for TokenLiteral {
    fn eq(&self, other: &TokenLiteral) -> bool {
        match (self, other) {
            (TokenLiteral::String(x), TokenLiteral::String(y)) => x == y,
            (TokenLiteral::Bool(x), TokenLiteral::Bool(y)) => x == y,
            (TokenLiteral::Number(x), TokenLiteral::Number(y)) => x == y,
            (TokenLiteral::Nil, TokenLiteral::Nil) => true,
            _ => false,
        }
    }
}

impl Eq for TokenLiteral {}

impl fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenLiteral::String(value) => write!(f, "{value}"),
            TokenLiteral::Number(value) => write!(f, "{value}"),
            TokenLiteral::Bool(value) => write!(f, "{value}"),
            TokenLiteral::Nil => write!(f, "Nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Token {
    pub tokentype: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub line: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //write!(f, "Token {{Type: {:?}, Lexeme: {:?}, Literal: {:?} }}", self.tokentype, self.lexeme, self.literal)
        write!(f, "{}", self.lexeme)
    }
}
