use std::collections::VecDeque;
use std::fmt;
use std::ops::{Deref, DerefMut};

use crate::inter::{ExprNode, StmtNode, Type};

#[derive(Debug, Clone)]
pub enum TACOperator {
    Add,
    Sub,
    Div,
    Mul,
    Call(i64),
    Assign,
    Gt,
    Lt,
    Ge,
    Le,
    Ret,
    Param,
    Begin(i32),
    End(i32),
    Goto,
    If,
    Iff,
    Null,
    Eql,
    Ne,
    Not,
    Or,
    And,
}

impl fmt::Display for TACOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //Token::Token(tag) => write!(f, "{}", *tag as char),
        match self {
            TACOperator::Add => write!(f, "+"),
            TACOperator::Sub => write!(f, "-"),
            TACOperator::Div => write!(f, "/"),
            TACOperator::Mul => write!(f, "*"),
            TACOperator::Call(_) => write!(f, "call"),
            TACOperator::Assign => write!(f, "="),
            TACOperator::Gt => write!(f, ">"),
            TACOperator::Lt => write!(f, "<"),
            TACOperator::Ge => write!(f, ">="),
            TACOperator::Le => write!(f, "<="),
            TACOperator::Ret => write!(f, "ret"),
            TACOperator::Param => write!(f, "param"),
            TACOperator::Begin(n) => write!(f, "begin {}", n),
            TACOperator::End(_) => write!(f, "end"),
            TACOperator::Goto => write!(f, "goto"),
            TACOperator::If => write!(f, "if"),
            TACOperator::Iff => write!(f, "iffalse"),
            TACOperator::Null => write!(f, ""),
            TACOperator::Eql => write!(f, "=="),
            TACOperator::Ne => write!(f, "!="),
            TACOperator::Not => write!(f, "!"),
            TACOperator::Or => write!(f, "||"),
            TACOperator::And => write!(f, "&&"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TACOperand {
    Var(String, Type, i32),
    Array(String, String, Type, i32),
    Temp(String, Type),
    Label(i64),
    Function(String),
    Const(String, Type),
    True,
    False,
    Null,
}

impl fmt::Display for TACOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //Token::Token(tag) => write!(f, "{}", *tag as char),
        match self {
            TACOperand::Var(s, _, _) => write!(f, "{}", s),
            TACOperand::Array(s1, s2, _, _) => write!(f, "{} [{}]", s1, s2),
            TACOperand::Temp(s, _) => write!(f, "{}", s),
            TACOperand::Label(n) => write!(f, "L{}", n),
            TACOperand::Function(s) => write!(f, "{}", s),
            TACOperand::Const(s, _) => write!(f, "{}", s),
            TACOperand::True => write!(f, "true"),
            TACOperand::False => write!(f, "false"),
            TACOperand::Null => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TACInstruction {
    pub op: TACOperator,
    pub arg1: TACOperand,
    pub arg2: TACOperand,
    pub res: TACOperand,
}

impl TACInstruction {
    pub fn new(op: TACOperator, arg1: TACOperand, arg2: TACOperand, res: TACOperand) -> Self {
        TACInstruction {
            op,
            arg1,
            arg2,
            res,
        }
    }
}

impl fmt::Display for TACInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //Token::Token(tag) => write!(f, "{}", *tag as char),
        let op = &self.op;
        let arg1 = &self.arg1;
        let arg2 = &self.arg2;
        let res = &self.res;
        match op {
            TACOperator::Add
            | TACOperator::Sub
            | TACOperator::Div
            | TACOperator::Mul
            | TACOperator::Gt
            | TACOperator::Lt
            | TACOperator::Ge
            | TACOperator::Le
            | TACOperator::Eql
            | TACOperator::Ne
            | TACOperator::Or
            | TACOperator::And => write!(f, "\t{} = {} {} {}", res, arg1, op, arg2),
            TACOperator::Call(n) => match res {
                TACOperand::Null => write!(f, "\t{} {} {}", op, arg1, n),
                _ => write!(f, "\t{} = {} {} {}", res, op, arg1, n),
            },
            TACOperator::Assign => write!(f, "\t{} {} {}", res, op, arg1),
            TACOperator::Ret => match res {
                TACOperand::Null => write!(f, "\t{}", op),
                _ => write!(f, "\t{} {}", op, res),
            },
            TACOperator::Param => write!(f, "\t{} {}", op, arg1),
            TACOperator::Begin(_) => write!(f, "\t{}", op),
            TACOperator::End(_) => write!(f, "\t{}", op),
            TACOperator::Goto => write!(f, "\t{} {}", op, res),
            TACOperator::If | TACOperator::Iff => {
                write!(f, "\t{} {} {} {}", op, arg1, TACOperator::Goto, res)
            }
            TACOperator::Not => write!(f, "\t{} = {} {}", res, op, arg1),
            TACOperator::Null => write!(f, "{}:", res),
        }
    }
}

pub struct TACIr(VecDeque<TACInstruction>);

impl TACIr {
    pub fn new() -> Self {
        TACIr(VecDeque::<TACInstruction>::new())
    }

    pub fn push(&mut self, tac: TACInstruction) {
        self.0.push_back(tac);
    }

    pub fn pop(&mut self) -> Option<TACInstruction> {
        self.0.pop_front()
    }
}

impl Deref for TACIr {
    type Target = VecDeque<TACInstruction>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TACIr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for TACIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for tac in self.iter() {
            writeln!(f, "{}", tac)?;
        }
        Ok(())
    }
}
