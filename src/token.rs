use crate::tokentype::TokenType;

#[derive(Debug)]
pub struct Token {
    pub tokentype: TokenType,
    pub lexeme: String,
    pub literal: Option<f32>,
    pub line: u32
}
