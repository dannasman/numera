/*
 * TAC implementation here.
 * Goals:
 * 1. Easy to form information for activation records
 * 2. As uniform as possible for all different instruction types
 * add more if necessary
 * TODO:
 * re-evaluate the way conditional jumps are recorded
 */

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
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
    AND,
    OR,
    PUSH,
    POP,
    GOTO,
    CALL,
    RET,
    NONE,
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
            TACOperator::AND => String::from("&&"),
            TACOperator::OR => String::from("||"),
            TACOperator::PUSH => String::from("push"),
            TACOperator::POP => String::from("pop"),
            TACOperator::GOTO => String::from("goto"),
            TACOperator::CALL => String::from("call"),
            TACOperator::RET => String::from("ret"),
            TACOperator::NONE => String::from(""),
        }
    }
}

#[derive(Clone)]
pub enum TACOperand {
    VAR_INT(String),
    VAR_FLOAT(String),
    VAR_BOOL(String),
    TEMP_INT(String),
    TEMP_FLOAT(String),
    TEMP_BOOL(String),
    CONST_INT(String),
    CONST_FLOAT(String),
    CONST_BOOL(String),
    LABEL(String),
    ACCESS(Box<TACOperand>, Box<TACOperand>), // a bit wonky, revisit some time
    IF,
    IFFALSE,
    NULL,
}

impl TACOperand {
    pub fn to_string(&self) -> String {
        match self {
            TACOperand::VAR_INT(s) => s.to_owned(),
            TACOperand::VAR_FLOAT(s) => s.to_owned(),
            TACOperand::VAR_BOOL(s) => s.to_owned(),
            TACOperand::TEMP_INT(s) => s.to_owned(),
            TACOperand::TEMP_FLOAT(s) => s.to_owned(),
            TACOperand::TEMP_BOOL(s) => s.to_owned(),
            TACOperand::CONST_INT(s) => s.to_owned(),
            TACOperand::CONST_FLOAT(s) => s.to_owned(),
            TACOperand::CONST_BOOL(s) => s.to_owned(),
            TACOperand::LABEL(s) => s.to_owned(),
            TACOperand::ACCESS(operand1, operand2) => format!("{} [ {} ]", operand1.to_string(), operand2.to_string()),
            TACOperand::IF => String::from("if"),
            TACOperand::IFFALSE => String::from("iffalse"),
            TACOperand::NULL => String::from(""),
        }
    }
}

#[derive(Clone)]
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

pub enum TACExpr {
    SINGLE(TACOperand),
    DOUBLE(TACOperator, TACOperand, TACOperand),
}

// TODO: implement methods for TACState (clone, new etc.)
pub struct TACState(Rc<RefCell<Vec<TACInstruction>>>);

impl TACState {
    pub fn new() -> Self {
        TACState(Rc::new(RefCell::new(Vec::<TACInstruction>::new())))
    }

    pub fn push(&self, instruction: TACInstruction) {
        let mut state = self.0.borrow_mut();
        state.push(instruction);
        drop(state);
    }

    pub fn len(&self) -> usize {
        let size = self.0.borrow().len();
        size
    }

    pub fn print(&self) {
        let tac_ir = self.0.borrow().to_vec();
        tac_ir.into_iter().for_each(|instruction| {
            println!("{} {} {} {}", instruction.op.to_string(), instruction.arg1.to_string(), instruction.arg2.to_string(), instruction.result.to_string());
        });
    }
}

impl Clone for TACState {
    fn clone(&self) -> TACState {
        TACState(Rc::clone(&self.0))
    }
}

/*

        let mut c = temp_count.borrow_mut();
        *c += 1;
        let number = *c;
        drop(c);
        Temp { tp, number }
*/
