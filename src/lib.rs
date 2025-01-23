use std::env;
use std::fs;
use std::io::{self, BufRead, BufReader};

mod codegen;
mod inter;
mod lexer;
mod parser;
mod tac;
mod tokens;

pub fn program(source: &'static [u8]) -> Result<(), String> {
    let reader: Box<dyn BufRead> = Box::new(BufReader::new(source));

    let lexer = lexer::Lexer::new(reader);
    let mut parser = parser::Parser::new(lexer).expect("Creating parser");

    let mut ir = tac::TACIr::new();
    parser.program(&mut ir).expect("Parsing program");

    let mut codegen = codegen::CodeGenerator::new();
    let mut str = String::new();
    codegen.program(&mut ir, &mut str).expect("Generating code");

    Ok(())
}
