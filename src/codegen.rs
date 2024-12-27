use std::collections::HashMap;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::sync::Mutex;

use crate::tac::*;

#[derive(Debug, Clone)]
enum Register {
    RAX, // for return values
    RBP, // for local variables
    RSP, // stack pointer
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

static GENERAL_REGISTERS: &'static [Register] = &[
    Register::RBX,
    Register::RCX,
    Register::RDX,
    Register::RSI,
    Register::RDI,
    Register::R8,
    Register::R9,
    Register::R10,
    Register::R11,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

#[derive(Debug, Clone)]
pub struct RegisterDescriptor {
    rax: Option<String>,
    rbp: Option<String>,
    rsp: Option<String>,
    rbx: Option<String>,
    rcx: Option<String>,
    rdx: Option<String>,
    rsi: Option<String>,
    rdi: Option<String>,
    r8: Option<String>,
    r9: Option<String>,
    r10: Option<String>,
    r11: Option<String>,
    r12: Option<String>,
    r13: Option<String>,
    r14: Option<String>,
    r15: Option<String>,
}

impl RegisterDescriptor {
    pub fn new() -> Self {
        RegisterDescriptor {
            rax: None,
            rbp: None,
            rsp: None,
            rbx: None,
            rcx: None,
            rdx: None,
            rsi: None,
            rdi: None,
            r8: None,
            r9: None,
            r10: None,
            r11: None,
            r12: None,
            r13: None,
            r14: None,
            r15: None,
        }
    }
}

impl Index<&Register> for RegisterDescriptor {
    type Output = Option<String>;

    fn index(&self, register: &Register) -> &Self::Output {
        match register {
            Register::RAX => &self.rax,
            Register::RBP => &self.rbp,
            Register::RSP => &self.rsp,
            Register::RBX => &self.rbx,
            Register::RCX => &self.rcx,
            Register::RDX => &self.rdx,
            Register::RSI => &self.rsi,
            Register::RDI => &self.rdi,
            Register::R8 => &self.r8,
            Register::R9 => &self.r9,
            Register::R10 => &self.r10,
            Register::R11 => &self.r11,
            Register::R12 => &self.r12,
            Register::R13 => &self.r13,
            Register::R14 => &self.r14,
            Register::R15 => &self.r15,
        }
    }
}

impl IndexMut<&Register> for RegisterDescriptor {
    fn index_mut(&mut self, register: &Register) -> &mut Self::Output {
        match register {
            Register::RAX => &mut self.rax,
            Register::RBP => &mut self.rbp,
            Register::RSP => &mut self.rsp,
            Register::RBX => &mut self.rbx,
            Register::RCX => &mut self.rcx,
            Register::RDX => &mut self.rdx,
            Register::RSI => &mut self.rsi,
            Register::RDI => &mut self.rdi,
            Register::R8 => &mut self.r8,
            Register::R9 => &mut self.r9,
            Register::R10 => &mut self.r10,
            Register::R11 => &mut self.r11,
            Register::R12 => &mut self.r12,
            Register::R13 => &mut self.r13,
            Register::R14 => &mut self.r14,
            Register::R15 => &mut self.r15,
        }
    }
}

/*
pub fn get_reg(val: TACOperand) -> Result<Register, &'static str> {
    let mut registers = unsafe { REGISTER_DESCRIPTOR.lock().unwrap() };
    let begin = 3;
    let end = registers.len();

    let mut i = 3;
    let mut j = registers.len();
    while i < registers.len() {
        if let Some(id) = &registers[i].1 {
            if id.to_string() == val.to_string() {
                return Ok(registers[i].0.to_owned());
            }
        } else {
            j = i;
        }
    }

    if j < registers.len() {
        registers[j].1 = Some(val);
        Ok(registers[j].0.to_owned())
    } else {
        Err("Failed to get register")
    }
}*/

pub struct CodeGenerator {
    address_descriptor: HashMap<String, Register>,
    register_descriptor: RegisterDescriptor,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            address_descriptor: HashMap::new(),
            register_descriptor: RegisterDescriptor::new(),
        }
    }

    pub fn get_reg(&mut self, val: TACOperand) -> Result<Register, &'static str> {
        let mut empty_reg: Option<&Register> = None;
        for reg in GENERAL_REGISTERS {
            if let Some(s) = &self.register_descriptor[reg] {
                if *s == val.to_string() {
                    return Ok(reg.to_owned());
                } else {
                    empty_reg = Some(reg);
                }
            }
        }

        if let Some(reg) = empty_reg {
            self.register_descriptor[reg] = Some(val.to_string());
            Ok(reg.to_owned())
        } else {
            Err("Failed to get register")
        }
    }

    pub fn program(&mut self, ir: &mut TACIr, b: &mut String) -> Result<(), &'static str> {
         while let Some(tac) = ir.pop() {
            match tac.op {
                TACOperator::Null => {
                    b.push_str(format!("{}:\n", tac.res).as_str());
                },
                TACOperator::Begin(n) => {
                    if n > 0 {
                    b.push_str("\tpush %rbp\n");
                    b.push_str("\tmov %rsp, %rbp\n");
                    b.push_str(format!("\tsub ${}, %rsp\n", n).as_str());
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }
}

/*
 *
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
            TACOperator::Param => write!(f, "\t{} {}", op, res),
            TACOperator::Begin(_) => write!(f, "\t{}", op),
            TACOperator::End => write!(f, "\t{}", op),
            TACOperator::Goto => write!(f, "\t{} {}", op, res),
            TACOperator::If | TACOperator::Iff => {
                write!(f, "\t{} {} {} {}", op, arg1, TACOperator::Goto, res)
            }
            TACOperator::Not => write!(f, "\t{} = {} {}", res, op, arg1),
            TACOperator::Null => write!(f, "{}:", res),
        }
    }
}
 */
