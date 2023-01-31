mod inter;
mod lexer;
mod parser;
mod code;
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
    let ir = parser.get_ir();

    println!("{}", ir);

    println!("Code compiled in {:?}", elapsed);

    let mut code = code::CodeGen::new(ir);
    code.gen();
    let llvm_ir = code.llvm_ir;
    println!("{}", llvm_ir);
    Ok(())
}
