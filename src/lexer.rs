use std::collections::{HashMap, VecDeque};

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub of: Box<Token>, //TODO: try to get rid of this
    pub size: u32,
    pub width: u32,
}

impl Array {
    pub fn new(size: u32, of: Token, tp_width: u32) -> Self {
        Array {
            size,
            of: Box::new(of),
            width: size * tp_width,
        }
    }

    pub fn array_to_string(&self) -> String {
        format!("[ {} ] {}", self.size, self.of.to_owned().value_to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(u32),
    Real(f64),
    Id(String),
    True(String),
    False(String),
    If(String),
    Else(String),
    While(String),
    And(String),
    Or(String),
    Eql(String),
    Ne(String),
    Le(String),
    Ge(String),
    Lt(String),
    Gt(String),
    Asgn(String),
    Not(String),
    Add(String),
    Sub(String),
    Mul(String),
    Div(String),
    Lcb(String),
    Rcb(String),
    Lrb(String),
    Rrb(String),
    Lsb(String),
    Rsb(String),
    Scol(String),
    Int(String),
    Float(String),
    Bool(String),
    Arr(Array),
    Def(String),
    Return(String),
    Void(String),
}

impl Token {
    pub fn value_to_string(self) -> String {
        match self {
            Token::Num(i) => format!("{}", i),
            Token::Real(i) => format!("{:?}", i),
            Token::Id(s) => s,
            Token::True(s) => s,
            Token::False(s) => s,
            Token::If(s) => s,
            Token::Else(s) => s,
            Token::While(s) => s,
            Token::And(s) => s,
            Token::Or(s) => s,
            Token::Eql(s) => s,
            Token::Ne(s) => s,
            Token::Le(s) => s,
            Token::Ge(s) => s,
            Token::Lt(s) => s,
            Token::Gt(s) => s,
            Token::Asgn(s) => s,
            Token::Not(s) => s,
            Token::Add(s) => s,
            Token::Sub(s) => s,
            Token::Mul(s) => s,
            Token::Div(s) => s,
            Token::Lcb(s) => s,
            Token::Rcb(s) => s,
            Token::Lrb(s) => s,
            Token::Rrb(s) => s,
            Token::Lsb(s) => s,
            Token::Rsb(s) => s,
            Token::Scol(s) => s,
            Token::Int(s) => s,
            Token::Float(s) => s,
            Token::Bool(s) => s,
            Token::Arr(a) => a.array_to_string(),
            Token::Def(s) => s,
            Token::Return(s) => s,
            Token::Void(s) => s,
        }
    }

    pub fn get_width(&self) -> Result<u32, &'static str> {
        match self {
            Token::Int(_) => Ok(4),
            Token::Float(_) => Ok(8),
            Token::Bool(_) => Ok(1),
            Token::Arr(a) => Ok(a.width),
            _ => Err("type void does not have size"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    pub tokens: VecDeque<Token>,
    words: HashMap<String, Token>,
    current_line: u32,
    pub lines: VecDeque<u32>, //safe line of token i to lines[i-1]
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            tokens: VecDeque::new(),
            words: HashMap::from([
                (String::from("true"), Token::True(String::from("true"))),
                (String::from("false"), Token::False(String::from("false"))),
                (String::from("if"), Token::If(String::from("if"))),
                (String::from("else"), Token::Else(String::from("else"))),
                (String::from("while"), Token::While(String::from("while"))),
                (String::from("int"), Token::Int(String::from("int"))),
                (String::from("float"), Token::Float(String::from("float"))),
                (String::from("bool"), Token::Bool(String::from("bool"))),
                (String::from("def"), Token::Def(String::from("def"))),
                (
                    String::from("return"),
                    Token::Return(String::from("return")),
                ),
                (String::from("void"), Token::Void(String::from("void"))),
            ]),
            current_line: 1,
            lines: VecDeque::new(),
        }
    }
    pub fn lex(&mut self, input: &str) {
        let mut it = input.chars().peekable();

        while let Some(&c) = it.peek() {
            match c {
                ' ' | '\t' | ',' => {
                    it.next();
                }
                '\n' => {
                    self.current_line += 1;
                    it.next();
                }
                '&' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('&') = ch {
                        self.tokens.push_back(Token::And(String::from("&&")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Id(String::from("&")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '|' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('|') = ch {
                        self.tokens.push_back(Token::Or(String::from("||")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Id(String::from("|")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '=' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(Token::Eql(String::from("==")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Asgn(String::from("=")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '!' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(Token::Ne(String::from("!=")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Not(String::from("!")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '<' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(Token::Le(String::from("<=")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Lt(String::from("<")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '>' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(Token::Ge(String::from(">=")));
                        it.next();
                    } else {
                        self.tokens.push_back(Token::Gt(String::from(">")));
                    };
                    self.lines.push_back(self.current_line);
                }
                '+' => {
                    self.tokens.push_back(Token::Add(String::from("+")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '-' => {
                    self.tokens.push_back(Token::Sub(String::from("-")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '*' => {
                    self.tokens.push_back(Token::Mul(String::from("*")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '/' => {
                    self.tokens.push_back(Token::Div(String::from("/")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '{' => {
                    self.tokens.push_back(Token::Lcb(String::from("{")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '}' => {
                    self.tokens.push_back(Token::Rcb(String::from("}")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '(' => {
                    self.tokens.push_back(Token::Lrb(String::from("(")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                ')' => {
                    self.tokens.push_back(Token::Rrb(String::from(")")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '[' => {
                    self.tokens.push_back(Token::Lsb(String::from("[")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                ']' => {
                    self.tokens.push_back(Token::Rsb(String::from("]")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                ';' => {
                    self.tokens.push_back(Token::Scol(String::from(";")));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
                '0'..='9' => {
                    let mut n = c
                        .to_string()
                        .parse::<f64>()
                        .expect("Character not a digit.");

                    it.next();
                    let mut digitch = it.peek();
                    let mut is_real = false;
                    while let Some(&i) = digitch {
                        if !i.is_ascii_digit() {
                            if i == '.' {
                                is_real = true;
                                let mut d = 10.0;
                                it.next();
                                digitch = it.peek();

                                while let Some(&j) = digitch {
                                    if !j.is_ascii_digit() {
                                        digitch = None;
                                    } else {
                                        let f = j
                                            .to_string()
                                            .parse::<f64>()
                                            .expect("Character not a digit.");
                                        n += f / d;
                                        d *= 10.0;
                                        it.next();
                                        digitch = it.peek();
                                    }
                                }
                            } else {
                                digitch = None;
                            }
                        } else {
                            let digit = i
                                .to_string()
                                .parse::<f64>()
                                .expect("Character not a digit.");
                            n = n * 10.0 + digit;
                            it.next();
                            digitch = it.peek();
                        }
                    }
                    if is_real {
                        self.tokens.push_back(Token::Real(n));
                    } else {
                        let i = n as u32;
                        self.tokens.push_back(Token::Num(i));
                    }
                    self.lines.push_back(self.current_line);
                }
                'A'..='Z' | 'a'..='z' => {
                    let mut s = String::new();
                    s.push(c);

                    it.next();
                    let mut ch = it.peek();
                    while let Some(&i) = ch {
                        if !i.is_ascii_digit() && !i.is_alphabetic() {
                            ch = None;
                        } else {
                            s.push(i);
                            it.next();
                            ch = it.peek();
                        }
                    }
                    match self.words.get(&s) {
                        Some(t) => {
                            self.tokens.push_back(Token::clone(t));
                            self.lines.push_back(self.current_line);
                        }
                        None => {
                            self.tokens.push_back(Token::Id(s.clone()));
                            self.words.insert(s.clone(), Token::Id(s));
                            self.lines.push_back(self.current_line);
                        }
                    }
                }
                _ => {
                    self.tokens.push_back(Token::Id(String::from(c)));
                    self.lines.push_back(self.current_line);
                    it.next();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_amount_of_tokens() {
        let input = String::from("1 _ != && =ok 3.4 1.0=_");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        assert_eq!(10, lexer.tokens.len())
    }

    #[test]
    fn correct_token_line_count() {
        let input = String::from("1 _ != && =ok 3.4 1.0=_");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        assert_eq!(10, lexer.lines.len())
    }

    #[test]
    fn correct_token_types() {
        let input = String::from("1 _ while { != && =ok 3.4 1.0=_ true false if else true1");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let output = format!("{:?}", lexer.tokens);
        assert_eq!(
            r#"[Num(1), Id("_"), While("while"), Lcb("{"), Ne("!="), And("&&"), Asgn("="), Id("ok"), Real(3.4), Real(1.0), Asgn("="), Id("_"), True("true"), False("false"), If("if"), Else("else"), Id("true1")]"#,
            output
        )
    }

    #[test]
    fn correct_bracket_handling() {
        let input = String::from("while {([*/;])}");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let output = format!("{:?}", lexer.tokens);
        assert_eq!(
            r#"[While("while"), Lcb("{"), Lrb("("), Lsb("["), Mul("*"), Div("/"), Scol(";"), Rsb("]"), Rrb(")"), Rcb("}")]"#,
            output
        )
    }

    #[test]
    fn correct_type_handling() {
        let input = String::from("1.0 1 true");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let output = format!("{:?}", lexer.tokens);
        assert_eq!(r#"[Real(1.0), Num(1), True("true")]"#, output)
    }

    #[test]
    fn correct_function_definition_handling() {
        let input = String::from("def void f(int a, int b)\n {\nreturn;\n}");
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let output = format!("{:?}", lexer.tokens);
        assert_eq!(
            r#"[Def("def"), Void("void"), Id("f"), Lrb("("), Int("int"), Id("a"), Int("int"), Id("b"), Rrb(")"), Lcb("{"), Return("return"), Scol(";"), Rcb("}")]"#,
            output
        )
    }
}
