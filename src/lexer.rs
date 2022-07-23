#[derive(Debug, Clone)]
pub enum Token    {
    Num(f64),
    Id(String),
    True(String),
    False(String),
    And(String),
    Or(String),
    Eql(String),
    Ne(String),
    Le(String),
    Ge(String),
}

pub fn lex(input: &String) -> Result<Vec<Token>, String>   {
    let mut result = Vec::new();

    let mut it = input.chars().peekable();

    while let Some(&c) = it.peek()  {
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
        
        it.next();
    }

    return Ok(result);
}
