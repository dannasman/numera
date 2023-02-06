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
    symbol_table: HashMap<String, String>,
    reg_count: u32,
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
                (String::from("true"), LlvmToken::True(String::from("1"))),
                (String::from("false"), LlvmToken::False(String::from("0"))),
                (String::from("if"), LlvmToken::If(String::from("if"))),
                (String::from("iffalse"), LlvmToken::Iff(String::from("iffalse"))),
                (String::from("int"), LlvmToken::Int(String::from("i32"))),
                (String::from("float"), LlvmToken::Float(String::from("float"))),
                (String::from("bool"), LlvmToken::Bool(String::from("i32"))),
            ]),
            symbol_table: HashMap::new(),
            reg_count: 0,
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
                    self.tokens.push_back(LlvmToken::Add(String::from("add")));
                    it.next();
                }
                '-' => {
                    self.tokens.push_back(LlvmToken::Sub(String::from("sub")));
                    it.next();
                }
                '*' => {
                    self.tokens.push_back(LlvmToken::Mul(String::from("mul")));
                    it.next();
                }
                '/' => {
                    self.tokens.push_back(LlvmToken::Div(String::from("sdiv")));
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
                    if c == 't' {
                        it.next();
                        let mut ch = it.peek();
                        while let Some(&i) = ch {
                            if i.is_ascii_digit() {
                                s.push(i);
                                it.next();
                                ch = it.peek();
                            }
                        }
                        self.tokens.push_back(LlvmToken::Temp(s));
                    } else {
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
                }
                _ => {
                    it.next();
                }
            }
        }
    }

    fn op_var_var(&mut self, temp: Option<String>, v1: String, v2: String, op: String) {
        let mut tp = self.symbol_table.get(&v1).unwrap();
        self.reg_count += 1;
        self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp, tp, v1));
        let reg1 = self.reg_count;

        tp = self.symbol_table.get(&v2).unwrap();
        self.reg_count += 1;
        self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp, tp, v1));
        let reg2 = self.reg_count;

        match temp {
            Some(t) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", t, op, tp, reg1, reg2));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", self.reg_count, op, tp, reg1, reg2));
            }
        }

    }

    fn op_var_temp(&mut self, temp: Option<String>, v: String, t: String, op: String) {
        let mut tp = self.symbol_table.get(&v).unwrap();
        self.reg_count += 1;
        self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp, tp, v));
        let reg = self.reg_count;

        match temp {
            Some(t_left) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", t_left, op, tp, reg, t));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", self.reg_count, op, tp, reg, t));
            }
        }
    }

    fn op_var_const(&mut self, temp: Option<String>, v: String, c: String, op: String) {
        let tp = self.symbol_table.get(&v).unwrap();
        self.reg_count += 1;
        self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp, tp, v));
        let reg = self.reg_count;

        match temp {
            Some(t) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, {}\n", t, op, tp, reg, c));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, {}\n", self.reg_count, op, tp, reg, c));
            }
        }
    }

    fn op_temp_temp(&mut self, temp: Option<String>, t1: String, t2: String, op: String) {
        let tp = self.symbol_table.get(&t1).unwrap();

        match temp {
            Some(t_left) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", t_left, op, tp, t1, t2));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", self.reg_count, op, tp, t1, t2));
            }
        }
    }

    fn op_temp_const(&mut self, temp: Option<String>, t: String, c: String, op: String) {
        let tp = self.symbol_table.get(&t).unwrap();

        match temp {
            Some(t_left) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, {}\n", t_left, op, tp, t, c));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, {}\n", self.reg_count, op, tp, t, c));
            }
        }
    }

    fn op_const_const(&mut self, temp: Option<String>, c1: String, c2: String, tp: String, op: String) {
        match temp {
            Some(t_left) => {
                self.llvm_ir.push_str(&format!("\t%{} = {} {} {}, {}\n", t_left, op, tp, c1, c2));
            }
            None => {
                self.reg_count += 1;
                self.llvm_ir.push_str(&format!("\t%{} = {} {} {}, {}\n", self.reg_count, op, tp, c1, c2));
            }
        }
    }

    fn load_var(&mut self, v: String) {
        let tp = self.symbol_table.get(&v).unwrap();
        self.reg_count += 1;
        self.llvm_ir.push_str(&format!("%{} = load {}, {} %*{}", self.reg_count, tp, tp, v));
    }

    // TODO: implementoi loputkin storet
    fn store_to_var(&mut self, v: String) {
        let tp = self.symbol_table.get(&v).unwrap();
        self.llvm_ir.push_str(&format!("\tstore {} %{}, {} *%{}\n", tp, self.reg_count, tp, v));
    }


    pub fn gen(&mut self) {
        self.llvm_ir.push_str("define i32 @main()\t(\n");
        let mut front = self.tokens.pop_front();
        while let Some(token) = front.to_owned() {
            match token {
                LlvmToken::Int(tp) | LlvmToken::Float(tp) | LlvmToken::Bool(tp) => {

                },
                LlvmToken::Word(word1) => {
                    front = self.tokens.pop_front();
                    if let Some(LlvmToken::Asgn(_)) = front.to_owned() {
                        front = self.tokens.pop_front();
                        match front.to_owned() {
                            Some(LlvmToken::Word(word2)) => {
                                front = self.tokens.pop_front();
                                if let Some(LlvmToken::Add(op)) | Some(LlvmToken::Sub(op)) | Some(LlvmToken::Mul(op)) | Some(LlvmToken::Div(op)) = front.to_owned() {
                                    front = self.tokens.pop_front();
                                    match front.to_owned() {
                                        Some(LlvmToken::Word(word3)) => {
                                            self.op_var_var(None, word2, word3, op);
                                            self.store_to_var(word1)

                                        },
                                        Some(LlvmToken::Temp(temp)) => {
                                            self.op_var_temp(None, word2, temp, op);
                                            self.store_to_var(word1)

                                        },
                                        Some(LlvmToken::Num(n2)) | Some(LlvmToken::Real(n2)) => {
                                            self.op_var_const(None, word2, n2, op);
                                            self.store_to_var(word1)
                                        },
                                        _ => panic!("codegen error: can not perform operation on given llvm token")
                                    }

                                } else {
                                    self.load_var(word2);
                                    self.store_to_var(word1);
                                }
                            },
                            _ => ()
                        }
                    } else {
                        panic!("codegen error: variable needs to be assigned")
                    }
                },
                LlvmToken::Temp(temp1) => {
                    front = self.tokens.pop_front();
                    if let Some(LlvmToken::Asgn(_)) = front.to_owned() {
                        front = self.tokens.pop_front();
                        match front.to_owned() {
                            Some(LlvmToken::Word(word1)) => {
                                front = self.tokens.pop_front();
                                if let Some(LlvmToken::Add(op)) | Some(LlvmToken::Sub(op)) | Some(LlvmToken::Mul(op)) | Some(LlvmToken::Div(op)) = front.to_owned() {
                                    front = self.tokens.pop_front();
                                    match front.to_owned() {
                                        Some(LlvmToken::Word(word2)) => {
                                            self.op_var_var(Some(temp1), word1, word2, op);
                                        },
                                        Some(LlvmToken::Temp(temp2)) => {
                                            self.op_var_temp(Some(temp1), word1, temp2, op)

                                        },
                                        Some(LlvmToken::Num(n)) | Some(LlvmToken::Real(n)) => {
                                            self.op_var_const(Some(temp1), word1, n, op)
                                        },
                                        _ => panic!("codegen error: can not perform operation on given llvm token")
                                    }

                                } else {
                                    // TODO: continue from here
                                    let tp = self.symbol_table.get(&word1).unwrap();
                                    self.llvm_ir.push_str(&format!("%{} = load {}, {} %*{}", temp1, tp, tp, word1));
                                }
                            },
                            _ => ()
                        }
                    } else {
                        panic!("codegen error: temp needs to be assigned")
                    }
                }
                _ => ()
            }
        }

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
        let ta_ir = String::from("L1:\n\tint x = 1\nL2:\n\tiffalse x == t1");
        let mut code = CodeGen::new(ta_ir);
        code.tokenize();
        let output = format!("{:?}", code.tokens);
        assert_eq!(
            r#"[Word("L1"), Col(":"), Int("i32"), Word("x"), Asgn("="), Num("1"), Word("L2"), Col(":"), Iff("iffalse"), Word("x"), Eql("=="), Temp("t1")]"#,
            output
        )
    }
    
}


/*
match self.tokens.front() {
                                            Some(LlvmToken::Word(word2)) => {
                                                let tp1 = self.symbol_table.get(word1).unwrap();
                                                self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp1, tp1, word1));
                                                let reg1 = self.reg_count;
                                                self.reg_count += 1;

                                                let tp2 = self.symbol_table.get(word2).unwrap();
                                                self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp2, tp2, word2));
                                                let reg2 = self.reg_count;
                                                self.reg_count += 1;

                                                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", self.reg_count, op, tp1, reg1, reg2));
                                                let reg3 = self.reg_count;
                                                self.reg_count += 1;

                                                let tp3 = self.symbol_table.get(word).unwrap();
                                                self.llvm_ir.push_str(&format!("\tstore {} %{}, {} *%{}\n", tp1, reg3, tp3, word));

                                            },
                                            Some(LlvmToken::Temp(temp2)) => {
                                                let tp1 = self.symbol_table.get(word1).unwrap();
                                                self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp1, tp1, word1));
                                                let reg1 = self.reg_count;
                                                self.reg_count += 1;

                                                let _tp2 = self.symbol_table.get(temp2).unwrap();

                                                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, %{}\n", self.reg_count, op, tp1, reg1, temp2));
                                                let reg3 = self.reg_count;
                                                self.reg_count += 1;

                                                let tp3 = self.symbol_table.get(word).unwrap();
                                                self.llvm_ir.push_str(&format!("\tstore {} %{}, {} *%{}\n", tp1, reg3, tp3, word));

                                            },
                                            Some(LlvmToken::Num(n2)) | Some(LlvmToken::Real(n2)) => {
                                                let tp1 = self.symbol_table.get(word1).unwrap();
                                                self.llvm_ir.push_str(&format!("\t%{} = load {}, {}* %{}\n", self.reg_count, tp1, tp1, word1));
                                                let reg1 = self.reg_count;
                                                self.reg_count += 1;

                                                self.llvm_ir.push_str(&format!("\t%{} = {} {} %{}, {}\n", self.reg_count, op,  tp1, reg1, n2));
                                                let reg3 = self.reg_count;
                                                self.reg_count += 1;

                                                let tp2 = self.symbol_table.get(word).unwrap();
                                                self.llvm_ir.push_str(&format!("\tstore {} %{}, {} *%{}\n", tp1, reg3, tp2, word));
                                            },
                                            _ => panic!("codegen error: can not perform operation on given llvm token")
                                        }
*/