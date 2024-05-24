/*
 * TAC implementation here.
 * Goals:
 * 1. Easy to form information for activation records
 * 2. As uniform as possible for all different instruction types
 * add more if necessary
 * TODO:
 * re-evaluate the way conditional jumps are recorded
 * add size to TACOperand variables and temporaries (maybe constants also)
 */
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[allow(non_snake_case)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug, PartialEq)]
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
    AND,
    OR,
    PUSH,
    POP,
    GOTO,
    CALL,
    RET,
    NONE,
}

impl fmt::Display for TACOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TACOperator::ADD => write!(f, "ADD"),
            TACOperator::SUB => write!(f, "SUB"),
            TACOperator::MUL => write!(f, "MUL"),
            TACOperator::DIV => write!(f, "DIV"),
            TACOperator::EQ => write!(f, "EQ"),
            TACOperator::NEQ => write!(f, "NEQ"),
            TACOperator::GT => write!(f, "GT"),
            TACOperator::GE => write!(f, "GE"),
            TACOperator::LT => write!(f, "LT"),
            TACOperator::LE => write!(f, "LE"),
            TACOperator::AND => write!(f, "AND"),
            TACOperator::OR => write!(f, "OR"),
            TACOperator::PUSH => write!(f, "PUSH"),
            TACOperator::POP => write!(f, "POP"),
            TACOperator::GOTO => write!(f, "GOTO"),
            TACOperator::CALL => write!(f, "CALL"),
            TACOperator::RET => write!(f, "RET"),
            TACOperator::NONE => write!(f, ""),
        }
    }
}

#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug, PartialEq)]
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

impl fmt::Display for TACOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TACOperand::VAR_INT(s) => write!(f, "{}", s),
            TACOperand::VAR_FLOAT(s) => write!(f, "{}", s),
            TACOperand::VAR_BOOL(s) => write!(f, "{}", s),
            TACOperand::TEMP_INT(s) => write!(f, "{}", s),
            TACOperand::TEMP_FLOAT(s) => write!(f, "{}", s),
            TACOperand::TEMP_BOOL(s) => write!(f, "{}", s),
            TACOperand::CONST_INT(s) => write!(f, "{}", s),
            TACOperand::CONST_FLOAT(s) => write!(f, "{}", s),
            TACOperand::CONST_BOOL(s) => write!(f, "{}", s),
            TACOperand::LABEL(s) => write!(f, "{}", s),
            TACOperand::ACCESS(operand1, operand2) => {
                write!(f, "{} [ {} ]", operand1, operand2)
            }
            TACOperand::IF => write!(f, "if"),
            TACOperand::IFFALSE => write!(f, "iffalse"),
            TACOperand::NULL => write!(f, ""),
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

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        let size = self.0.borrow().len();
        size
    }

    pub fn print(&self) {
        let tac_ir = self.0.borrow().to_vec();
        tac_ir.into_iter().for_each(|instruction| {
            let s = format!(
                "{} {} {} {}",
                instruction.op, instruction.result, instruction.arg1, instruction.arg2
            );
            println!("{}", s.split_whitespace().collect::<Vec<_>>().join(" "));
        });
    }
}

impl Clone for TACState {
    fn clone(&self) -> TACState {
        TACState(Rc::clone(&self.0))
    }
}
