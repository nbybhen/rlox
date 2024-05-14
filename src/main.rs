use std::cell::Cell;
use std::env;
use std::process;

use std::io;
use std::io::Write;
use std::fs;

mod scanner;
mod token;
mod parser;
mod interpreter;
mod functions;

use crate::interpreter::Interpreter;
use crate::token::{Token, TokenType};
use crate::parser::{Parser, Stmt};
use crate::scanner::Scanner;

pub struct App {
    had_error: Cell<bool>,
    had_runtime_error: Cell<bool>,
}

impl App {
    fn new() -> App {

        App {
            had_error: Cell::new(false),
            had_runtime_error: Cell::new(false),
        }
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
            self.report(token.line, format!(" at '{:}'", token.lexeme), message);
        }
    }

    // Reports runtime errors
    pub fn runtime_error(&self, token: Token, message: &str) {
        eprintln!("[line {}] {}", token.line, message);
    }

    // Reports errors to the user without panicking
    fn report(&self, line: u32, loc: String, message: &str) {
        eprintln!("[line {line}] Error{loc}: {message}");
        self.had_error.set(true);
    }

    // Reads code from file
    fn run_file(&self, interpreter: &mut Interpreter, path: &String) {
        match fs::read_to_string(path) {
            Ok(data) => self.run(data, interpreter),
            _ => eprintln!("Could not read file.")
        }
    }

    // REPL
    fn run_prompt(&self, interpreter: &mut Interpreter) {
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).expect("Invalid input.");

            if input.is_empty() {
                break;
            }
            self.run(input, interpreter);
            self.had_error.set(false);
        }
    }

    // Runs the code inputted by the REPL
    fn run(&self, inp: String, interpreter: &mut Interpreter) {
        let mut scanner = Scanner::build(self, inp);
        let tokens: Vec<Token> = scanner.scan_tokens();

        if self.had_error.get() {
            return;
        }

        let mut parser: Parser = Parser::new(tokens.to_vec(), self);
        let statements: Vec<Stmt> = parser.parse();

        if self.had_error.get() {
            return;
        }

        let _ = interpreter.interpret(&statements, self);

        if self.had_runtime_error.get() {
            process::exit(1);
        }

    }
}




fn main() {
    let app: App = App::new();
    let mut interpreter = Interpreter::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        // jlox
        1 => app.run_prompt(&mut interpreter),
        // jlox [script]
        2 => app.run_file(&mut interpreter, &args[1]),
        // error
        _ => {
            println!("Improper syntax: Use \"rlox [script]\"");
            process::exit(64);
        },
    }
}
