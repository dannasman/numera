mod inter;
mod lexer;
mod parser;
mod runtime;
mod tac;

use std::env;
use std::error::Error;
use std::fs;
use std::time::Instant;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let mut filename = String::from("");
    if args.len() > 1 {
        filename = args[1].clone();
    }

    let lexer = lexer::Lexer::new();
    let mut parser = parser::Parser::new(lexer);

    let input: String = fs::read_to_string(filename)?.parse()?;
    println!("{}", input);
    println!("----------compiling----------");

    let now = Instant::now();

    let tac_ir = tac::TACState::new();

    parser.program(&input, tac_ir.clone());

    let elapsed = now.elapsed();

    tac_ir.print();

    println!("Code compiled in {:?}", elapsed);
    Ok(())
}
