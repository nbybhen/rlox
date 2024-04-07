#[derive(Debug, Copy, Clone)]
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
