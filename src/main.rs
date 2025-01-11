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
    let mut args: Vec<String> = env::args().collect();
    let reader: Box<dyn BufRead> = match args.pop() {
        None => Box::new(BufReader::new(io::stdin())),
        Some(filename) => Box::new(BufReader::new(fs::File::open(filename).unwrap())),
    };

    let mut print_ir = false;

    while let Some(s) = args.pop() {
        if s == "--ir" {
            print_ir = true;
        }
    }

    let lexer = lexer::Lexer::new(reader);
    let mut parser = parser::Parser::new(lexer).expect("Creating parser");

    let mut ir = tac::TACIr::new();
    parser.program(&mut ir).expect("Parsing program");

    if print_ir {
        println!("================Start of TAC IR================");
        println!("{}", ir);
        println!("================End of TAC IR================");
    }

    //println!("================asm================");

    let mut codegen = codegen::CodeGenerator::new();
    let mut str = String::new();
    codegen.program(&mut ir, &mut str).expect("Generating code");

    if !print_ir {
        println!("{}", str);
    }
}
