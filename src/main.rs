mod inter;
mod lexer;
mod parser;

fn main() {
    let lexer = lexer::Lexer::new();
    let mut parser = parser::Parser::new(lexer);
    let input = String::from("{ while (true) { break; } }");
    parser.program(&input);
}
