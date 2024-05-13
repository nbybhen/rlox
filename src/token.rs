use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    // SC Tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // DC Tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String, Number,

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    EOF
}


#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
    String(String),
    Number(f32),
    Bool(bool),
    Nil
}

impl fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenLiteral::String(value) => write!(f, "{value}"),
            TokenLiteral::Number(value) => write!(f, "{value}"),
            TokenLiteral::Bool(value) => write!(f, "{value}"),
            TokenLiteral::Nil => write!(f, "Nil")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
