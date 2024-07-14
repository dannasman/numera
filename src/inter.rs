use std::cell::RefCell;
use std::fmt;

use super::tokens::{Tag, Token};

thread_local! {
    static LABEL_COUNTER: RefCell<i64> = RefCell::new(1);
    static TEMP_COUNTER: RefCell<i32> = RefCell::new(1);
}

pub fn new_label() -> i64 {
    let mut res = 0;
    LABEL_COUNTER.with(|counter| {
        res = *counter.borrow();
        *counter.borrow_mut() = res + 1;
    });
    res
}

pub fn reset_labels() {
    LABEL_COUNTER.with(|counter| {
        *counter.borrow_mut() = 1;
    });
}

pub fn emit_label(s: &mut String, i: i64) {
    s.push_str(&format!("L{}:", i));
}

pub fn emit(s: &mut String, st: &str) {
    s.push_str(&format!("\t{}\n", st));
}

pub fn emit_jumps(s: &mut String, test: &str, t: i64, f: i64) {
    if t != 0 && f != 0 {
        emit(s, &format!("if {} goto L{}", test, t));
        emit(s, &format!("goto L{}", f));
    } else if t != 0 {
        emit(s, &format!("if {} goto L{}", test, t));
    } else if f != 0 {
        emit(s, &format!("iffalse {} goto L{}", test, f));
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Basic {
        lexeme: String,
        tag: Tag,
        width: u8,
    },
    Array {
        of: Box<Type>,
        tag: Tag,
        length: u32,
    },
}

impl Type {
    pub fn new(token: Token) -> Result<Type, String> {
        if let Token::BasicType(lexeme, tag, width) = token {
            Ok(Type::Basic { lexeme, tag, width })
        } else if let Token::Array(of, tag, length) = token {
            let o = Type::new(*of)?;
            Ok(Type::Array {
                of: Box::new(o),
                tag,
                length,
            })
        } else {
            return Err(format!("Can not convert token {} to Type", token));
        }
    }

    pub fn array(of: Type, tag: Tag, length: u32) -> Type {
        Type::Array {
            of: Box::new(of),
            tag,
            length,
        }
    }

    pub fn int() -> Type {
        Type::new(Token::int()).unwrap()
    }

    pub fn float() -> Type {
        Type::new(Token::float()).unwrap()
    }

    pub fn char() -> Type {
        Type::new(Token::char()).unwrap()
    }

    pub fn bool() -> Type {
        Type::new(Token::bool()).unwrap()
    }

    pub fn token(&self) -> Token {
        match &self {
            Type::Basic { lexeme, tag, width } => Token::BasicType(lexeme.to_owned(), *tag, *width),
            Type::Array { of, tag, length } => Token::Array(Box::new(of.token()), *tag, *length),
        }
    }

    pub fn tag(&self) -> Tag {
        match &self {
            Type::Basic {
                lexeme: _,
                tag,
                width: _,
            } => *tag,
            Type::Array {
                of: _,
                tag,
                length: _,
            } => *tag,
        }
    }

    pub fn width(&self) -> u32 {
        match self {
            Type::Basic {
                lexeme: _,
                tag: _,
                width,
            } => *width as u32,
            Type::Array { of, tag: _, length } => of.width() * length,
        }
    }

    pub fn is_numeric(&self) -> bool {
        if let Type::Basic {
            lexeme,
            tag: _,
            width: _,
        } = self
        {
            match lexeme.as_str() {
                "int" | "float" | "char" => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn max_type(left: &Type, right: &Type) -> Option<Type> {
        if !left.is_numeric() || !right.is_numeric() {
            None
        } else if *left == Type::float() || *right == Type::float() {
            Some(Type::float())
        } else if *left == Type::int() || *right == Type::int() {
            Some(Type::int())
        } else {
            Some(Type::char())
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Basic {
                lexeme: lexeme1,
                tag: tag1,
                width: width1,
            } => match other {
                Type::Basic {
                    lexeme: lexeme2,
                    tag: tag2,
                    width: width2,
                } => lexeme1 == lexeme2 && tag1 == tag2 && width1 == width2,
                _ => false,
            },
            Type::Array {
                of: of1,
                tag: tag1,
                length: length1,
            } => match other {
                Type::Array {
                    of: of2,
                    tag: tag2,
                    length: length2,
                } => of1 == of2 && tag1 == tag2 && length1 == length2,
                _ => false,
            },
        }
    }
}

impl Eq for Type {}

pub enum ExprNode {
    Constant(Token, Type),
    Id(Token, Type, i32),
    Temp(Token, Type, i32),
    Arith(Token, Type, Box<ExprNode>, Box<ExprNode>),
    Unary(Token, Type, Box<ExprNode>),
    Access(Token, Box<ExprNode>, Box<ExprNode>, Type),
    Rel(Token, Type, Box<ExprNode>, Box<ExprNode>),
    Not(Token, Type, Box<ExprNode>),
    Or(Token, Type, Box<ExprNode>, Box<ExprNode>),
    And(Token, Type, Box<ExprNode>, Box<ExprNode>),
}

impl ExprNode {
    pub fn op(&self) -> &Token {
        match self {
            ExprNode::Constant(op, _) => op,
            ExprNode::Id(op, _, _) => op,
            ExprNode::Temp(op, _, _) => op,
            ExprNode::Arith(op, _, _, _) => op,
            ExprNode::Unary(op, _, _) => op,
            ExprNode::Access(op, _, _, _) => op,
            ExprNode::Rel(op, _, _, _) => op,
            ExprNode::Not(op, _, _) => op,
            ExprNode::Or(op, _, _, _) => op,
            ExprNode::And(op, _, _, _) => op,
        }
    }

    pub fn tp(&self) -> &Type {
        match self {
            ExprNode::Constant(_, tp) => tp,
            ExprNode::Id(_, tp, _) => tp,
            ExprNode::Temp(_, tp, _) => tp,
            ExprNode::Arith(_, tp, _, _) => tp,
            ExprNode::Unary(_, tp, _) => tp,
            ExprNode::Access(_, _, _, tp) => tp,
            ExprNode::Rel(_, tp, _, _) => tp,
            ExprNode::Not(_, tp, _) => tp,
            ExprNode::Or(_, tp, _, _) => tp,
            ExprNode::And(_, tp, _, _) => tp,
        }
    }

    pub fn new_id(id: Token, tp: &Type, offset: i32) -> ExprNode {
        ExprNode::Id(id, tp.to_owned(), offset)
    }

    pub fn box_id(id: Token, tp: &Type, offset: i32) -> Box<ExprNode> {
        Box::new(ExprNode::new_id(id, tp, offset))
    }

    pub fn new_temp(tp: &Type) -> ExprNode {
        let mut num: i32 = 0;
        TEMP_COUNTER.with(|c| {
            num = *c.borrow();
            *c.borrow_mut() = num + 1;
        });
        ExprNode::Temp(Token::temp(), tp.to_owned(), num)
    }

    pub fn box_temp(tp: &Type) -> Box<ExprNode> {
        Box::new(ExprNode::new_temp(tp))
    }

    pub fn new_arith(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let tp = match Type::max_type(left.tp(), right.tp()) {
            Some(t) => t,
            None => return Err(String::from("Type error")),
        };

        Ok(ExprNode::Arith(token, tp, left, right))
    }

    pub fn box_arith(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let arith = ExprNode::new_arith(token, left, right)?;
        Ok(Box::new(arith))
    }

    pub fn new_unary(token: Token, expr: Box<ExprNode>) -> Result<ExprNode, String> {
        let tp = match Type::max_type(&Type::int(), expr.tp()) {
            Some(t) => t,
            None => return Err(String::from("Type error")),
        };

        Ok(ExprNode::Unary(token, tp, expr))
    }

    pub fn box_unary(token: Token, expr: Box<ExprNode>) -> Result<Box<ExprNode>, String> {
        let unary = ExprNode::new_unary(token, expr)?;
        Ok(Box::new(unary))
    }

    pub fn new_rel(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        if left.tp() != right.tp() {
            Err(String::from("Type error"))
        } else if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = left.tp()
        {
            Err(String::from("type error"))
        } else if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = right.tp()
        {
            Err(String::from("type error"))
        } else {
            Ok(ExprNode::Rel(token, Type::bool(), left, right))
        }
    }

    pub fn box_rel(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let rel = ExprNode::new_rel(token, left, right)?;
        Ok(Box::new(rel))
    }

    pub fn new_not(token: Token, expr: Box<ExprNode>) -> Result<ExprNode, String> {
        if expr.tp() != &Type::bool() {
            return Err(String::from("Type error"));
        }
        if token.tag() != Token::Token('!' as u8).tag() {
            return Err(String::from("Lexer error"));
        }
        Ok(ExprNode::Not(token, Type::bool(), expr))
    }

    pub fn box_not(token: Token, expr: Box<ExprNode>) -> Result<Box<ExprNode>, String> {
        let not = ExprNode::new_not(token, expr)?;
        Ok(Box::new(not))
    }

    pub fn new_or(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let bt = Type::bool();
        if left.tp() != &bt || right.tp() != &bt {
            return Err(String::from("Type Error"));
        }
        Ok(ExprNode::Or(token, Type::bool(), left, right))
    }

    pub fn box_or(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let or = ExprNode::new_or(token, left, right)?;
        Ok(Box::new(or))
    }

    pub fn new_and(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let bt = Type::bool();
        if left.tp() != &bt || right.tp() != &bt {
            return Err(String::from("Type Error"));
        }
        Ok(ExprNode::And(token, Type::bool(), left, right))
    }

    pub fn box_and(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let and = ExprNode::new_and(token, left, right)?;
        Ok(Box::new(and))
    }

    pub fn jumping(&self, b: &mut String, t: i64, f: i64) -> Result<(), String> {
        match self {
            ExprNode::Rel(op, _, left, right) => {
                let lr = left.reduce(b)?;
                let rr = right.reduce(b)?;
                emit_jumps(b, format!("{} {} {}", lr, op, rr).as_str(), t, f);
            }
            ExprNode::Not(_, _, expr) => {
                expr.jumping(b, f, t)?;
            }
            ExprNode::Or(_, _, left, right) => {
                let mut label = t;
                if t == 0 {
                    label = new_label();
                }
                left.jumping(b, label, 0)?;
                right.jumping(b, t, f)?;
                if t == 0 {
                    emit_label(b, label);
                }
            }
            ExprNode::And(_, _, left, right) => {
                let mut label = f;
                if f == 0 {
                    label = new_label();
                }
                left.jumping(b, 0, label)?;
                right.jumping(b, t, f)?;

                if f == 0 {
                    emit_label(b, label);
                }
            }
            _ => emit_jumps(b, format!("{}", self).as_str(), t, f),
        }
        Ok(())
    }

    pub fn gen(&self, b: &mut String) -> Result<Box<ExprNode>, String> {
        match self {
            ExprNode::Arith(op, _, left, right) => {
                let lr = left.reduce(b)?;
                let rr = right.reduce(b)?;
                match ExprNode::new_arith(op.to_owned(), lr, rr) {
                    Ok(arith) => Ok(Box::new(arith)),
                    Err(s) => Err(s),
                }
            }
            ExprNode::Unary(op, _, expr) => {
                let rexpr = expr.reduce(b)?;
                let unary = ExprNode::new_unary(op.to_owned(), rexpr)?;
                Ok(Box::new(unary))
            }
            ExprNode::Access(op, array, index, tp) => {
                let i = index.reduce(b)?;
                Ok(Box::new(ExprNode::Access(
                    op.to_owned(),
                    array.to_owned(),
                    i,
                    tp.to_owned(),
                )))
            }
            ExprNode::Rel(_, tp, _, _)
            | ExprNode::Not(_, tp, _)
            | ExprNode::Or(_, tp, _, _)
            | ExprNode::And(_, tp, _, _) => {
                let f = new_label();
                let a = new_label();
                let tmp = ExprNode::new_temp(tp);
                self.jumping(b, 0, f)?;
                emit(b, format!("{} = true", tmp).as_str());
                emit(b, format!("goto L{}", a).as_str());
                emit_label(b, f);
                emit(b, format!("{} = false", tmp).as_str());
                emit_label(b, a);
                Ok(Box::new(tmp))
            }
            _ => Ok(self.box_clone()),
        }
    }

    pub fn reduce(&self, b: &mut String) -> Result<Box<ExprNode>, String> {
        match self {
            ExprNode::Arith(_, tp, _, _)
            | ExprNode::Unary(_, tp, _)
            | ExprNode::Access(_, _, _, tp) => {
                let x = self.gen(b)?;
                let tmp = ExprNode::new_temp(tp);
                emit(b, format!("{} = {}", tmp, x).as_str());
                Ok(Box::new(tmp))
            }
            _ => Ok(self.box_clone()),
        }
    }

    pub fn box_clone(&self) -> Box<ExprNode> {
        Box::new(self.clone())
    }
}

impl Clone for ExprNode {
    fn clone(&self) -> Self {
        match self {
            ExprNode::Constant(op, tp) => ExprNode::Constant(op.clone(), tp.clone()),
            ExprNode::Id(id, tp, offset) => ExprNode::Id(id.clone(), tp.clone(), offset.clone()),
            ExprNode::Temp(op, tp, num) => ExprNode::Temp(op.clone(), tp.clone(), num.clone()),
            ExprNode::Arith(op, tp, left, right) => {
                ExprNode::Arith(op.clone(), tp.clone(), left.box_clone(), right.box_clone())
            }
            ExprNode::Unary(op, tp, expr) => {
                ExprNode::Unary(op.clone(), tp.clone(), expr.box_clone())
            }
            ExprNode::Access(op, array, index, tp) => {
                ExprNode::Access(op.clone(), array.box_clone(), index.box_clone(), tp.clone())
            }
            ExprNode::Rel(op, _, left, right) => ExprNode::Rel(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
            ExprNode::Not(op, _, expr) => ExprNode::Not(op.clone(), Type::bool(), expr.box_clone()),
            ExprNode::Or(op, _, left, right) => ExprNode::Or(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
            ExprNode::And(op, _, left, right) => ExprNode::And(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
        }
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprNode::Constant(token, _) => write!(f, "{}", token),
            ExprNode::Id(id, _, _) => write!(f, "{}", id),
            ExprNode::Temp(_, _, num) => write!(f, "t{}", num),
            ExprNode::Arith(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::Unary(op, _, expr) => write!(f, "{} {}", op, expr),
            ExprNode::Access(_, array, index, _) => write!(f, "{} [ {} ]", array, index),
            ExprNode::Rel(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::Not(op, _, expr) => write!(f, "{} {}", op, expr),
            ExprNode::Or(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::And(op, _, left, right) => write!(f, "{}, {}, {}", left, op, right),
        }
    }
}
