use std::io::BufReader;

mod inter;
mod lexer;
mod tokens;

fn main() {
    let mut lexer = lexer::Lexer::new(BufReader::new(std::io::stdin()));
    loop {
        let token = lexer.scan();
        if let Ok(tokens::Token::Eof) = token {
            break;
        } else {
            if let Ok(t) = token {
                println!("{}", t);
            }
        }
    }
}
