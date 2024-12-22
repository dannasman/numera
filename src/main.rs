use std::io::BufReader;

mod inter;
mod lexer;
mod parser;
mod tac;
mod tokens;

fn main() {
    let lexer = lexer::Lexer::new(BufReader::new(std::io::stdin()));
    let mut parser = parser::Parser::new(lexer).expect("Creating parser");

    let mut ir = Vec::<tac::TACInstruction>::new();
    parser.program(&mut ir).expect("Parsing program");
    //println!("{}", str);
}
