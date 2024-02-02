mod inter;
mod lexer;
mod parser;
mod runtime;

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

    parser.program(&input);

    let elapsed = now.elapsed();

    println!("Code compiled in {:?}", elapsed);
    Ok(())
}
