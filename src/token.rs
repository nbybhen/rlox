use std::fmt;

use crate::tokentype::TokenType;

#[derive(Debug, Clone)]
pub enum TokenLiteral {
    String {
        value: String
    },
    Number {
        value: f32
    },
    Bool {
        value: bool
    },
    Nil
}

impl fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenLiteral::String{value} => write!(f, "{value}"),
            TokenLiteral::Number{value} => write!(f, "{value}"),
            TokenLiteral::Bool{value} => write!(f, "{value}"),
            TokenLiteral::Nil => write!(f, "Nil")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tokentype: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub line: u32
}

impl fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //write!(f, "Token {{Type: {:?}, Lexeme: {:?}, Literal: {:?} }}", self.tokentype, self.lexeme, self.literal)
        write!(f, "{}", self.lexeme)
    }
}
