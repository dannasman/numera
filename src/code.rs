use std::collections::{HashMap, VecDeque};
#[derive(Debug, Clone)]
enum LlvmToken {
    If(String),
    Iff(String),
    Word(String),
    Temp(String),
    Label(String),
    Lt(String),
    Gt(String),
    Le(String),
    Ge(String),
    Eql(String),
    Neq(String),
    Add(String),
    Sub(String),
    Mul(String),
    Div(String),
    Goto(String),
    Bool(String),
    Int(String),
    Float(String),
    Asgn(String),
    Lsb(String),
    Rsb(String),
    Num(String),
    Real(String),
    True(String),
    False(String),
    Col(String),
}

#[derive(Debug, Clone)]
pub struct CodeGen {
    tokens: VecDeque<LlvmToken>,
    words: HashMap<String, LlvmToken>,
    //<-in
    ta_ir: String, // three address, higher level ir
    //out->
    pub llvm_ir: String, // llvm ir
}

impl CodeGen {
    pub fn new(ta_ir: String) -> Self {
        CodeGen {
            tokens: VecDeque::new(),
            words: HashMap::from([
                (String::from("true"), LlvmToken::True(String::from("true"))),
                (String::from("false"), LlvmToken::False(String::from("false"))),
                (String::from("if"), LlvmToken::If(String::from("if"))),
                (String::from("iffalse"), LlvmToken::Iff(String::from("iffalse"))),
                (String::from("int"), LlvmToken::Int(String::from("int"))),
                (String::from("float"), LlvmToken::Float(String::from("float"))),
                (String::from("bool"), LlvmToken::Bool(String::from("bool"))),
            ]),
            ta_ir,
            llvm_ir: String::new()
        }
    }

    fn tokenize(&mut self) {
        let mut it = self.ta_ir.chars().peekable();
        while let Some(&c) = it.peek() {
            println!("{}", c);
            match c {
                ' ' | '\t' | '\n' => {
                    it.next();
                },
                ':' => {
                    self.tokens.push_back(LlvmToken::Col(String::from(":")));
                    it.next();
                }
                '=' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(LlvmToken::Eql(String::from("==")));
                        it.next();
                    } else {
                        self.tokens.push_back(LlvmToken::Asgn(String::from("=")));
                    }
                },
                '<' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(LlvmToken::Le(String::from("<=")));
                        it.next();
                    } else {
                        self.tokens.push_back(LlvmToken::Lt(String::from("<")));
                    };
                }
                '>' => {
                    it.next();
                    let ch = it.peek();
                    if let Some('=') = ch {
                        self.tokens.push_back(LlvmToken::Ge(String::from(">=")));
                        it.next();
                    } else {
                        self.tokens.push_back(LlvmToken::Gt(String::from(">")));
                    }
                }
                '+' => {
                    self.tokens.push_back(LlvmToken::Add(String::from("+")));
                    it.next();
                }
                '-' => {
                    self.tokens.push_back(LlvmToken::Sub(String::from("-")));
                    it.next();
                }
                '*' => {
                    self.tokens.push_back(LlvmToken::Mul(String::from("*")));
                    it.next();
                }
                '/' => {
                    self.tokens.push_back(LlvmToken::Div(String::from("/")));
                },
                '[' => {
                    self.tokens.push_back(LlvmToken::Lsb(String::from("[")));
                    it.next();
                }
                ']' => {
                    self.tokens.push_back(LlvmToken::Rsb(String::from("]")));
                    it.next();
                },
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
                        self.tokens.push_back(LlvmToken::Real(n.to_string()));
                    } else {
                        let i = n as u32;
                        self.tokens.push_back(LlvmToken::Num(i.to_string()));
                    }
                },
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
                            self.tokens.push_back(LlvmToken::clone(t));
                        }
                        None => {
                            self.tokens.push_back(LlvmToken::Word(s.clone()));
                            self.words.insert(s.clone(), LlvmToken::Word(s));
                        }
                    }
                }
                _ => {
                    it.next();
                }
            }
        }
    }

    pub fn gen(&mut self) {
        self.llvm_ir.push_str("define i32 @main()\t(\n");


        self.llvm_ir.push_str("\tret i32 0\n)");
    }
}

#[cfg(test)]
mod codegen_tests {
    use super::*;

    #[test]
    fn correct_amount_of_llvm_tokens() {
        let ta_ir = String::from("L2:\n iffalse x == 1");
        let mut code = CodeGen::new(ta_ir);
        code.tokenize();
        assert_eq!(6, code.tokens.len())
    }

    #[test]
    fn correct_llvm_token_types() {
        let ta_ir = String::from("L2:\n iffalse x == 1");
        let mut code = CodeGen::new(ta_ir);
        code.tokenize();
        let output = format!("{:?}", code.tokens);
        assert_eq!(
            r#"[Word("L2"), Col(":"), Iff("iffalse"), Word("x"), Eql("=="), Num("1")]"#,
            output
        )
    }
    
}
