mod inter;
mod lexer;
mod parser;

fn main() {
    let lexer = lexer::Lexer::new();
    let mut parser = parser::Parser::new(lexer);
    //let input = String::from("{\n\tx = 2;\n\t{\n\t\ty = x;\n\t}\n\t{\n\t\tz = x;\n\t}\n}");
    let input = String::from("{\n\tx = 1;\n\twhile (x < 5) {\n\t\twhile(x < 3) {\n\t\t\tx = x + 1;\n\t\t}\n\t\tx = x * 2;\n\t}\n}");
    //let input = String::from("{\n\tx = 1;\n\twhile (true) {\n\t\tif(x < 3) {\n\t\t\tx = x + 1;\n\t\t} else {\n\t\t\tbreak;\n\t\t}\n\t}\n\ty=2;\n}");
    println!("{}", input);
    println!("----------compiling----------");
    parser.program(&input);
}
