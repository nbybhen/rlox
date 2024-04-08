use std::env;
use std::process;

use std::io;
use std::io::Write;

mod scanner;
mod tokentype;
mod token;
mod parser;
mod interpreter;

use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::parser::Parser;
use crate::parser::print;

struct App {
    had_error: bool
}

impl App {
    fn new() -> App {
        App {had_error: false}
    }

    fn error(&mut self, line: u32, message: String) {
        self.report(line, String::new() ,message);
    }

    fn report(&mut self, line: u32, loc: String, message: String) {
        eprintln!("[line {line}] Error{loc}: {message}");
        self.had_error = true;
    }

    // Reads code from file
    fn run_file(&self, path: &String) {
        if self.had_error {
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
    fn run(&mut self, inp: String) {
        println!("Running: {inp}");
        let mut scanner = scanner::Scanner::build(inp);
        let tokens: &Vec<Token> = scanner.scan_tokens();

        for token in tokens {
            println!("Token: {token:?}");
        }
        self.had_error = false;

        let mut parser: Parser = Parser::new(tokens.to_vec());
        let expr = parser.parse();
        println!("Parser Print: {:?}", print(&expr));

        let interpreter: Interpreter = Interpreter::new();
        let _ = interpreter.interpret(&expr);

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
