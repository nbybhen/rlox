use crate::token::{Token, TokenLiteral};
use crate::tokentype::TokenType;

// Parser
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
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => write!(f, "{value}"),
            Expr::Grouping { expression } => write!(f, "{:}", print(expression)),
            Expr::Unary { operator, right } => write!(f, "{operator} {:}", print(right)),
            Expr::Binary { left, operator, right } => write!(f, "{operator} {:} {:} ", print(left), print(right))
        }
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

fn print(expr: &Expr) -> String {
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
        }
    } 
}

#[test]
fn main() {
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
            } 
)        })
    };
    println!("{:?}",print(&expression));
}
