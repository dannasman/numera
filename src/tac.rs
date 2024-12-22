use crate::inter::{ExprNode, StmtNode, Type};

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
    End,
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

pub struct TACInstruction {
    pub op: TACOperator,
    pub arg1: TACOperand,
    pub arg2: TACOperand,
    pub res: TACOperand,
}
