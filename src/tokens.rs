use std::fmt;

#[derive(Copy, Clone, Debug, Eq)]
pub enum Tag {
    AND = 256,
    BASIC,
    BREAK,
    DO,
    ELSE,
    EQ,
    FALSE,
    GE,
    ID,
    IF,
    INDEX,
    LE,
    MINUS,
    NE,
    NUM,
    OR,
    REAL,
    TEMP,
    TRUE,
    WHILE,
    EOF = std::u32::MAX as isize,
}

impl Into<u32> for Tag {
    fn into(self) -> u32 {
        self as u32
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        (*self as u32) == (*other as u32)
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    Token(u8),
    Num(i64),
    Word(String, Tag),
    Real(f64),
    BasicType(String, Tag, u8),
    Array(Box<Token>, u32),
    Eof,
}

impl Token {
    pub fn and() -> Token {
        Token::Word(String::from("&&"), Tag::AND)
    }

    pub fn or() -> Token {
        Token::Word(String::from("||"), Tag::OR)
    }

    pub fn eq() -> Token {
        Token::Word(String::from("=="), Tag::EQ)
    }

    pub fn ne() -> Token {
        Token::Word(String::from("!="), Tag::NE)
    }

    pub fn le() -> Token {
        Token::Word(String::from("<="), Tag::LE)
    }

    pub fn ge() -> Token {
        Token::Word(String::from(">="), Tag::GE)
    }

    pub fn minus() -> Token {
        Token::Word(String::from("-"), Tag::MINUS)
    }

    pub fn ttrue() -> Token {
        Token::Word(String::from("true"), Tag::TRUE)
    }

    pub fn tfalse() -> Token {
        Token::Word(String::from("false"), Tag::FALSE)
    }

    pub fn temp() -> Token {
        Token::Word(String::from("temp"), Tag::TEMP)
    }

    pub fn int() -> Token {
        Token::BasicType(String::from("int"), Tag::BASIC, 4u8)
    }

    pub fn char() -> Token {
        Token::BasicType(String::from("char"), Tag::BASIC, 1u8)
    }

    pub fn bool() -> Token {
        Token::BasicType(String::from("bool"), Tag::BASIC, 1u8)
    }

    pub fn float() -> Token {
        Token::BasicType(String::from("float"), Tag::BASIC, 8u8)
    }

    pub fn access() -> Token {
        Token::Word(String::from("[]"), Tag::INDEX)
    }

    pub fn tag(&self) -> u32 {
        match self {
            Token::Token(tag) => *tag as u32,
            Token::Num(_) => Tag::NUM as u32,
            Token::Word(_, tag) => *tag as u32,
            Token::Real(_) => Tag::REAL as u32,
            Token::BasicType(_, tag, _) => *tag as u32,
            Token::Array(_, _) => Tag::INDEX as u32,
            Token::Eof => Tag::EOF as u32,
        }
    }

    pub fn match_tag<T: Into<u32>>(&self, o: T) -> bool {
        self.tag() == o.into()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Token(tag) => write!(f, "{}", *tag as char),
            Token::Num(n) => write!(f, "{}", n),
            Token::Word(lexeme, _) => write!(f, "{}", lexeme),
            Token::Real(r) => write!(f, "{}", r),
            Token::BasicType(lexeme, _, _) => write!(f, "{}", lexeme),
            Token::Array(tp, len) => write!(f, "[{}]{}", len, *tp),
            Token::Eof => write!(f, "\0"),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Token::Num(n1) => match other {
                Token::Num(n2) => n1 == n2,
                _ => false,
            },
            Token::Word(lexeme1, tag1) => match other {
                Token::Word(lexeme2, tag2) => lexeme1 == lexeme2 && tag1 == tag2,
                _ => false,
            },
            Token::Real(_) => match other {
                Token::Real(__) => self.to_string() == other.to_string(),
                _ => false,
            },
            Token::BasicType(lexeme1, tag1, w1) => match other {
                Token::BasicType(lexeme2, tag2, w2) => {
                    lexeme1 == lexeme2 && tag1 == tag2 && w1 == w2
                }
                _ => false,
            },
            Token::Array(tp1, len1) => match other {
                Token::Array(tp2, len2) => tp1 == tp2 && len1 == len2,
                _ => false,
            },
            _ => self.tag() == other.tag(),
        }
    }
}

impl Eq for Token {}
