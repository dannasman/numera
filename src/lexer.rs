#[derive(Debug, Clone)]
pub enum Token    {
    Num(f64),   // numbers are currently float only, maybe splitting into Num(u64) and Real(f64) later...
    Id(String),
    True(String),
    False(String),
    And(String),
    Or(String),
    Eql(String),
    Ne(String),
    Le(String),
    Ge(String),
    Lt(String),
    Gt(String),
}

pub fn lex(input: &String) -> Result<Vec<Token>, String>   {
    let mut result = Vec::new();

    let mut it = input.chars().peekable();

    let mut lineno = 1;

    while let Some(&c) = it.peek()  {

        // skip if empty character
        if c == ' ' && c == '\t' {
            it.next();
            continue;   
        }

        // skip and increase line number if newline
        if c == '\n'    {
            lineno += 1;
            it.next();
            continue;
        }

        // logical and comparison operators
        match c {
            '&' =>  {
                it.next();
                let ch = it.peek();
                if let Some('&') = ch   {
                    result.push(Token::And("&&".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Id("&".to_string()));
                    continue;
                };
            },
            '|' =>  {
                it.next();
                let ch = it.peek();
                if let Some('|') = ch   {
                    result.push(Token::Or("||".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Id("|".to_string()));
                    continue;
                };
            },
            '=' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Eql("==".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Id("=".to_string()));
                    continue;
                };
            },
            '!' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Ne("!=".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Id("!".to_string()));
                    continue;
                };
            },
            '<' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Le("<=".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Lt("<".to_string()));
                    continue;
                };
            },
            '>' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Ge(">=".to_string()));
                    it.next();
                    continue;
                } else  {
                    result.push(Token::Gt(">".to_string()));
                    continue;
                };
            },
            _ => (),
        }
        
        // digits
        if c.is_digit(10)    {
            let mut n = c.to_string().parse::<f64>().expect("Character not a digit.");

            it.next();
            let mut digitch = it.peek();

            while let Some(&i) = digitch {
                if !i.is_digit(10)   {
                    if i == '.'    {
                        let mut d = 10.0;
                        it.next();
                        digitch = it.peek();

                        while let Some(&j) = digitch    {
                            if !j.is_digit(10) {
                                digitch = None;
                            } else  {
                                let f = j.to_string().parse::<f64>().expect("Character not a digit.");
                                n = n + f / d;
                                d = d * 10.0;
                                it.next();
                                digitch = it.peek();
                            }
                        }
                    } else  {
                        digitch = None;
                    }
                } else  {
                    let digit = i.to_string().parse::<f64>().expect("Character not a digit.");
                    n = n*10.0 + digit;
                    it.next();
                    digitch = it.peek();
                }
            }
            result.push(Token::Num(n));
        }

        // lexemes
        if c.is_alphabetic()    {
            let mut s = String::new();
            s.push(c);

            it.next();
            let mut ch = it.peek();

            while let Some(&i) = ch {
                if !i.is_digit(10) && !i.is_alphabetic()  {
                    ch = None;
                } else  {
                    s.push(i);
                    it.next();
                    ch = it.peek();
                }
            }
            result.push(Token::Id(s));
        }

        // remaining characters
        if c != ' ' && c != '\t' && c != '\n'   {
            result.push(Token::Id(c.to_string()));
        }
        
        it.next();
    }
    //println!("{:?}", result);
    return Ok(result);
}
