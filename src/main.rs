mod lexer;

fn main() {
    let s = String::from("ok hmm1 1");
    let result = lexer::lex(&s);
    match result    {
        Ok(r) => println!("{:?}", r.len()),
        Err(_) => println!("Error getting the return value."),
    }
}
