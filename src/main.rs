use std::env;
use std::fs;
use std::io::BufReader;

mod inter;
mod lexer;
mod parser;
mod tokens;

fn main() {
    let input = env::args().nth(1);
    let lexer = lexer::Lexer::new(BufReader::new(std::io::stdin()));
    let mut parser = parser::Parser::new(lexer).expect("Creating parser");

    let mut str = String::new();
    parser.program(&mut str).expect("Parsing program");
    println!("{}", str);
}
