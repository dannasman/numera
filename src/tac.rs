/*
 * TAC implementation here.
 * Goals:
 * 1. Easy to form information for activation records
 * 2. As uniform as possible for all different instruction types
 * add more if necessary
 */

pub enum TACOperator {
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    NEQ,
    GT,
    GE,
    LT,
    LE,
    ASGN,
}

impl TACOperator {
    pub fn to_string(&self) -> String {
        match self {
            TACOperator::ADD => String::from("+"),
            TACOperator::SUB => String::from("-"),
            TACOperator::MUL => String::from("+"),
            TACOperator::DIV => String::from("/"),
            TACOperator::EQ => String::from("=="),
            TACOperator::NEQ => String::from("!="),
            TACOperator::GT => String::from(">"),
            TACOperator::GE => String::from(">="),
            TACOperator::LT => String::from("<"),
            TACOperator::LE => String::from("<="),
            TACOperator::ASGN => String::from("="),
        }
    }
}

pub enum TACOperand {
    VAR(String),
    TEMP(String),
    INT(String),
    FLOAT(String),
    BOOL(String),
    LABEL(String),
}

impl TACOperand {
    pub fn to_string(&self) -> String {
        match self {
            TACOperand::VAR(s) => s.to_owned(),
            TACOperand::TEMP(s) => s.to_owned(),
            TACOperand::INT(s) => s.to_owned(),
            TACOperand::FLOAT(s) => s.to_owned(),
            TACOperand::BOOL(s) => s.to_owned(),
            TACOperand::LABEL(s) => s.to_owned(),
        }
    }
}

pub struct TACInstruction {
    op: TACOperator,
    arg1: TACOperand,
    arg2: TACOperand,
    result: TACOperand,
}

impl TACInstruction {
    pub fn new(op: TACOperator, arg1: TACOperand, arg2: TACOperand, result: TACOperand) -> Self {
        TACInstruction {
            op,
            arg1,
            arg2,
            result,
        }
    }
}
