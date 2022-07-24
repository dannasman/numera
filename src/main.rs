mod lexer;

fn main() {
    let s = String::from("1 _ != && =ok 3.4 1.0=_");
    let result = lexer::lex(&s);
    match result    {
        Ok(r) => println!("{:?}", r.len()),
        Err(_) => println!("Error getting the return value."),
    }
}
