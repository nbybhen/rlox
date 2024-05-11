use std::cell::Cell;
use std::env;
use std::process;

use std::io;
use std::io::Write;
use std::fs;

mod scanner;
mod tokentype;
mod token;
mod parser;
mod interpreter;

use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::parser::{Parser, Stmt};
use crate::scanner::Scanner;
use crate::tokentype::TokenType;

pub struct App {
    had_error: Cell<bool>
}

impl App {
    fn new() -> App {
        App {had_error: Cell::new(false)}
    }

    pub fn error(&self, line: u32, message: &str) {
        self.report(line, String::new(), message);
    }

    // Handles errors where the Token is displayed within the message
    pub fn error_token(&self, token: Token, message: &str) {
        if token.tokentype == TokenType::EOF {
            self.report(token.line, String::from(" at end"), message);
        }
        else {
            self.report(token.line, format!("at '{:}'", token.lexeme), message);
        }
    }



    // Reports errors to the user without panicking
    fn report(&self, line: u32, loc: String, message: &str) {
        eprintln!("[line {line}] Error{loc}: {message}");
        //self.had_error = true;
    }

    // Reads code from file
    fn run_file(&mut self, path: &String) {
        match fs::read_to_string(path) {
            Ok(stuff) => {
                self.run(stuff);
            },
            _ => {
                eprintln!("Could not read file.");
            }
        }

        if self.had_error.get() {
            process::exit(1);
        }
    }

    // REPL
    fn run_prompt(&mut self) {
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).expect("Invalid input.");
            if input.is_empty() {
                break;
            }
            self.run(input);

        }
    }

    // Runs the code inputted by the REPL
    fn run(&self, inp: String) {
        //println!("Running: {inp}");
        let mut scanner = Scanner::build(self, inp);
        let tokens: Vec<Token> = scanner.scan_tokens();

        let mut parser: Parser = Parser::new(tokens.to_vec(), self);
        let statements: Vec<Stmt> = parser.parse();

        if self.had_error.get() {
            return;
        }

        let mut interpreter: Interpreter = Interpreter::new();
        let _ = interpreter.interpret(&statements);
    }
}




fn main() {
    let mut app: App = App::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        // jlox
        1 => app.run_prompt(),
        // jlox [script]
        2 => app.run_file(&args[1]),
        // error
        _ => {
            println!("Improper syntax: Use \"rlox [script]\"");
            process::exit(64);
        },
    }
}
