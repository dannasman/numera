use std::env;
use std::fs;
use std::io::{self, BufRead, BufReader};

mod codegen;
mod inter;
mod lexer;
mod parser;
mod tac;
mod tokens;

fn main() {
    let input = env::args().nth(1);
    let reader: Box<dyn BufRead> = match input {
        None => Box::new(BufReader::new(io::stdin())),
        Some(filename) => Box::new(BufReader::new(fs::File::open(filename).unwrap())),
    };
    let lexer = lexer::Lexer::new(reader);
    let mut parser = parser::Parser::new(lexer).expect("Creating parser");

    let mut ir = tac::TACIr::new();
    parser.program(&mut ir).expect("Parsing program");
    println!("{}", ir);
    let mut codegen = codegen::CodeGenerator::new();
    let mut str = String::new();
    codegen.program(&mut ir, &mut str).expect("Generating code");
    println!("{}", str);
}
