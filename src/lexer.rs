use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Token    {
    Num(f64),       // numbers are currently float only, maybe splitting into Num(u64) and Real(f64) later...
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
}

pub fn lex(input: &String) -> Result<Vec<Token>, String>   {
    let mut result = Vec::new();

    // reserved words are saved to this hashmap 
    let mut words = HashMap::from([
                                  ("true".to_string(),  Token::True("true".to_string())),
                                  ("false".to_string(), Token::False("false".to_string())),
                                  ("if".to_string(),    Token::If("if".to_string())),
                                  ("else".to_string(),  Token::Else("else".to_string())),
                                  ("while".to_string(), Token::While("while".to_string())),
    ]);

    let mut it = input.chars().peekable();

    let mut _lineno = 1;

    while let Some(&c) = it.peek()  {

        // logical and comparison operators
        match c {
            ' ' | '\t' => {
                it.next();
            },
            '\n'  =>  {
                _lineno += 1;
                it.next();
            },
            '&' =>  {
                it.next();
                let ch = it.peek();
                if let Some('&') = ch   {
                    result.push(Token::And("&&".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Id("&".to_string()));
                };
            },
            '|' =>  {
                it.next();
                let ch = it.peek();
                if let Some('|') = ch   {
                    result.push(Token::Or("||".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Id("|".to_string()));
                };
            },
            '=' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Eql("==".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Id("=".to_string()));
                };
            },
            '!' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Ne("!=".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Id("!".to_string()));
                };
            },
            '<' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Le("<=".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Lt("<".to_string()));
                };
            },
            '>' =>  {
                it.next();
                let ch = it.peek();
                if let Some('=') = ch   {
                    result.push(Token::Ge(">=".to_string()));
                    it.next();
                } else  {
                    result.push(Token::Gt(">".to_string()));
                };
            },
            '0'..='9' =>    {
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
            'A'..='Z' | 'a'..='z' => {
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
                println!("{}", s);
                match words.get(&s)  {
                    Some(t) => result.push(Token::clone(t)),
                    None => {
                        result.push(Token::Id(s.clone()));
                        words.insert(s.clone(), Token::Id(s.clone()));
                    },
                }
            },
            _ => {
                result.push(Token::Id(c.to_string()));
                it.next();
            },
        }
    }

    return Ok(result);
}

#[cfg(test)]
mod tests    {
    use super::*;

    #[test]
    fn correct_amount_of_tokens()   {
        let input = String::from("1 _ != && =ok 3.4 1.0=_");
        let result = lex(&input);
        match result    {
            Ok(r) => assert_eq!(10, r.len()),
            Err(_) => println!("Error getting the return value."),
        }
    }

    #[test]
    fn correct_token_types()    {
        let input = String::from("1 _ while != && =ok 3.4 1.0=_ true false if else true1");
        let result = lex(&input);
        match result    {
            Ok(r) =>    {
                let output = format!("{:?}", r);
                assert_eq!(r#"[Num(1.0), Id("_"), While("while"), Ne("!="), And("&&"), Id("="), Id("ok"), Num(3.4), Num(1.0), Id("="), Id("_"), True("true"), False("false"), If("if"), Else("else"), Id("true1")]"#, output)
            },
            Err(_) => println!("Error getting the return value."),
        }
    }
}
