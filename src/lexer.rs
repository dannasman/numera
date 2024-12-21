use std::collections::HashMap;
use std::io::{BufReader, ErrorKind, Read, Result};

use super::tokens::{Tag, Token};

pub struct Lexer<T: std::io::Read> {
    pub line: u32,
    peek: u8,
    words: HashMap<String, Token>,
    reader: BufReader<T>,
}

impl<T: std::io::Read> Lexer<T> {
    pub fn new(source: BufReader<T>) -> Lexer<T> {
        let lexer = Lexer {
            line: 1,
            peek: b' ',
            words: HashMap::from([
                (String::from("true"), Token::ttrue()),
                (String::from("false"), Token::tfalse()),
                (String::from("int"), Token::int()),
                (String::from("float"), Token::float()),
                (String::from("char"), Token::char()),
                (String::from("bool"), Token::bool()),
                (String::from("void"), Token::void()),
                (String::from("if"), Token::Word(String::from("if"), Tag::IF)),
                (
                    String::from("else"),
                    Token::Word(String::from("else"), Tag::ELSE),
                ),
                (
                    String::from("while"),
                    Token::Word(String::from("while"), Tag::WHILE),
                ),
                (String::from("do"), Token::Word(String::from("do"), Tag::DO)),
                (
                    String::from("define"),
                    Token::Word(String::from("define"), Tag::DEFINE),
                ),
                (
                    String::from("break"),
                    Token::Word(String::from("break"), Tag::BREAK),
                ),
                (
                    String::from("return"),
                    Token::Word(String::from("return"), Tag::RETURN),
                ),
                (String::from("eof"), Token::Eof),
            ]),
            reader: source,
        };
        lexer
    }

    fn read_ch(&mut self, c: u8) -> Result<bool> {
        self.read()?;
        if self.peek != c {
            return Ok(false);
        }
        self.peek = b' ';
        Ok(true)
    }

    fn read(&mut self) -> Result<()> {
        let mut buf = [0; 1];
        if let Err(err) = self.reader.read_exact(&mut buf) {
            if let ErrorKind::UnexpectedEof = err.kind() {
                return Ok(());
            } else {
                return Err(err);
            }
        }

        self.peek = buf[0];
        Ok(())
    }

    pub fn scan(&mut self) -> Result<Token> {
        if self.peek == b'\0' {
            return Ok(Token::Eof);
        }

        loop {
            if self.peek == b' ' || self.peek == b'\t' || self.peek == b'\r' {
                self.read()?;
                continue;
            }
            if self.peek == b'\n' {
                self.line += 1;
                self.read()?;
                continue;
            }
            break;
        }
        match self.peek {
            b'&' => {
                return match self.read_ch(b'&') {
                    Ok(true) => Ok(Token::and()),
                    Ok(false) => Ok(Token::Token(b'&')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'&')),
                        _ => Err(err),
                    },
                }
            }
            b'|' => {
                return match self.read_ch(b'|') {
                    Ok(true) => Ok(Token::or()),
                    Ok(false) => Ok(Token::Token(b'|')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'&')),
                        _ => Err(err),
                    },
                }
            }
            b'=' => {
                return match self.read_ch(b'=') {
                    Ok(true) => Ok(Token::eq()),
                    Ok(false) => Ok(Token::Token(b'=')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'=')),
                        _ => Err(err),
                    },
                }
            }
            b'!' => {
                return match self.read_ch(b'=') {
                    Ok(true) => Ok(Token::ne()),
                    Ok(false) => Ok(Token::Token(b'!')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'!')),
                        _ => Err(err),
                    },
                }
            }
            b'<' => {
                return match self.read_ch(b'=') {
                    Ok(true) => Ok(Token::le()),
                    Ok(false) => Ok(Token::Token(b'<')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'<')),
                        _ => Err(err),
                    },
                }
            }
            b'>' => {
                return match self.read_ch(b'=') {
                    Ok(true) => Ok(Token::ge()),
                    Ok(false) => Ok(Token::Token(b'>')),
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => Ok(Token::Token(b'>')),
                        _ => Err(err),
                    },
                }
            }
            _ => (),
        }

        if self.peek.is_ascii_digit() {
            let mut v: i64 = 0;

            loop {
                v = 10 * v + ((self.peek - b'0') as i64);

                if let Err(err) = self.read() {
                    if let ErrorKind::UnexpectedEof = err.kind() {
                        break;
                    } else {
                        return Err(err);
                    }
                }

                if !self.peek.is_ascii_digit() {
                    break;
                }
            }

            if self.peek != b'.' {
                return Ok(Token::Num(v));
            }

            let mut x: f64 = v as f64;
            let mut d: f64 = 10.0;

            loop {
                if let Err(err) = self.read() {
                    if let ErrorKind::UnexpectedEof = err.kind() {
                        break;
                    } else {
                        return Err(err);
                    }
                }

                if !self.peek.is_ascii_digit() {
                    break;
                }

                x += (self.peek - b'0') as f64 / d;

                x = (x * d).round() / d;

                d *= 10.0;
            }

            return Ok(Token::Real(x));
        }

        if self.peek.is_ascii_alphabetic() {
            let mut b = String::new();

            loop {
                b.push(self.peek as char);
                if let Err(err) = self.read() {
                    if let ErrorKind::UnexpectedEof = err.kind() {
                        break;
                    } else {
                        return Err(err);
                    }
                }

                if !self.peek.is_ascii_alphanumeric() {
                    break;
                }
            }

            if let Some(token) = self.words.get(&b) {
                return Ok(token.to_owned());
            } else {
                return Ok(Token::Word(b, Tag::ID));
            }
        }

        let token = Token::Token(self.peek);
        self.peek = b' ';
        Ok(token)
    }
}
