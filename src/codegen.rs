use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::sync::Mutex;

use crate::inter::Type;
use crate::tac::*;
use crate::tokens::Token;

thread_local! {
    static FLOAT_COUNTER: RefCell<i64> = RefCell::new(0);
}

#[derive(Debug, Clone)]
enum Register {
    RAX, // for return values
    RBP, // for local variables
    RSP, // stack pointer
    RBX, // has to be preserved
    RCX, // reserved
    RDX, // reserved
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,  // has to be preserved
    R14,  // has to be preserved
    R15,  // has to be preserved,
    XMM0, // for return values
    XMM1, // reserved
    XMM2, // reserved
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::RAX => write!(f, "%rax"),
            Register::RBP => write!(f, "%rbp"),
            Register::RSP => write!(f, "%rsp"),
            Register::RBX => write!(f, "%rbx"),
            Register::RCX => write!(f, "%rcx"),
            Register::RDX => write!(f, "%rdx"),
            Register::RSI => write!(f, "%rsi"),
            Register::RDI => write!(f, "%rdi"),
            Register::R8 => write!(f, "%r8"),
            Register::R9 => write!(f, "%r9"),
            Register::R10 => write!(f, "%r10"),
            Register::R11 => write!(f, "%r11"),
            Register::R12 => write!(f, "%r12"),
            Register::R13 => write!(f, "%r13"),
            Register::R14 => write!(f, "%r14"),
            Register::R15 => write!(f, "%r15"),
            Register::XMM0 => write!(f, "%xmm0"),
            Register::XMM1 => write!(f, "%xmm1"),
            Register::XMM2 => write!(f, "%xmm2"),
            Register::XMM3 => write!(f, "%xmm3"),
            Register::XMM4 => write!(f, "%xmm4"),
            Register::XMM5 => write!(f, "%xmm5"),
            Register::XMM6 => write!(f, "%xmm6"),
            Register::XMM7 => write!(f, "%xmm7"),
            Register::XMM8 => write!(f, "%xmm8"),
            Register::XMM9 => write!(f, "%xmm9"),
            Register::XMM10 => write!(f, "%xmm10"),
            Register::XMM11 => write!(f, "%xmm11"),
            Register::XMM12 => write!(f, "%xmm12"),
            Register::XMM13 => write!(f, "%xmm13"),
            Register::XMM14 => write!(f, "%xmm14"),
            Register::XMM15 => write!(f, "%xmm15"),
        }
    }
}

static GENERAL_REGISTERS: &'static [Register] = &[
    //Register::RBX,
    //Register::RCX,
    //Register::RDX,
    Register::RSI,
    Register::RDI,
    Register::R8,
    Register::R9,
    Register::R10,
    Register::R11,
    Register::R12,
    //Register::R13,
    //Register::R14,
    //Register::R15,
];

static FLOAT_REGISTERS: &'static [Register] = &[
    Register::XMM3,
    Register::XMM4,
    Register::XMM5,
    Register::XMM6,
    Register::XMM7,
    Register::XMM8,
    Register::XMM9,
    Register::XMM10,
    Register::XMM11,
    Register::XMM12,
    Register::XMM13,
    Register::XMM14,
    Register::XMM15,
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
    xmm0: Option<String>,
    xmm1: Option<String>,
    xmm2: Option<String>,
    xmm3: Option<String>,
    xmm4: Option<String>,
    xmm5: Option<String>,
    xmm6: Option<String>,
    xmm7: Option<String>,
    xmm8: Option<String>,
    xmm9: Option<String>,
    xmm10: Option<String>,
    xmm11: Option<String>,
    xmm12: Option<String>,
    xmm13: Option<String>,
    xmm14: Option<String>,
    xmm15: Option<String>,
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
            xmm0: None,
            xmm1: None,
            xmm2: None,
            xmm3: None,
            xmm4: None,
            xmm5: None,
            xmm6: None,
            xmm7: None,
            xmm8: None,
            xmm9: None,
            xmm10: None,
            xmm11: None,
            xmm12: None,
            xmm13: None,
            xmm14: None,
            xmm15: None,
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
            Register::XMM0 => &self.xmm0,
            Register::XMM1 => &self.xmm1,
            Register::XMM2 => &self.xmm2,
            Register::XMM3 => &self.xmm3,
            Register::XMM4 => &self.xmm4,
            Register::XMM5 => &self.xmm5,
            Register::XMM6 => &self.xmm6,
            Register::XMM7 => &self.xmm7,
            Register::XMM8 => &self.xmm8,
            Register::XMM9 => &self.xmm9,
            Register::XMM10 => &self.xmm10,
            Register::XMM11 => &self.xmm11,
            Register::XMM12 => &self.xmm12,
            Register::XMM13 => &self.xmm13,
            Register::XMM14 => &self.xmm14,
            Register::XMM15 => &self.xmm15,
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
            Register::XMM0 => &mut self.xmm0,
            Register::XMM1 => &mut self.xmm1,
            Register::XMM2 => &mut self.xmm2,
            Register::XMM3 => &mut self.xmm3,
            Register::XMM4 => &mut self.xmm4,
            Register::XMM5 => &mut self.xmm5,
            Register::XMM6 => &mut self.xmm6,
            Register::XMM7 => &mut self.xmm7,
            Register::XMM8 => &mut self.xmm8,
            Register::XMM9 => &mut self.xmm9,
            Register::XMM10 => &mut self.xmm10,
            Register::XMM11 => &mut self.xmm11,
            Register::XMM12 => &mut self.xmm12,
            Register::XMM13 => &mut self.xmm13,
            Register::XMM14 => &mut self.xmm14,
            Register::XMM15 => &mut self.xmm15,
        }
    }
}

pub fn align_stack(n: i32) -> Result<i32, String> {
    if n > 0 {
        let aligned = (n + 15) & !15;
        Ok(aligned)
    } else {
        Err(String::from("Alignment of non positive integer no allowed"))
    }
}

pub fn to_bits_f64(s: String) -> u64 {
    s.parse::<f64>().unwrap().to_bits()
}

pub fn arg_tp(arg: &TACOperand) -> Result<&Type, String> {
    match arg {
        TACOperand::Var(_, tp, _) => Ok(tp),
        TACOperand::Temp(_, tp) => Ok(tp),
        TACOperand::Array(_, _, tp, _) => Ok(tp),
        TACOperand::Const(_, tp) => Ok(tp),
        TACOperand::Function(_, tp) => Ok(tp),
        _ => Err(format!("Can get type of {}", arg)),
    }
}

//Type::Array { of, tag: _, length } => Token::Array(Box::new(of.token()), *length),
pub fn mov(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("movsd"))
            } else {
                Ok(String::from("movq"))
            }
        }
        _ => Err(format!("Failed to form mov instruction for {}", tp.token())),
    }
}

pub fn add(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("addsd"))
            } else {
                Ok(String::from("addq"))
            }
        }
        _ => Err(String::from("Failed to form add instruction")),
    }
}

pub fn sub(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("subsd"))
            } else {
                Ok(String::from("subq"))
            }
        }
        _ => Err(String::from("Failed to form sub instruction")),
    }
}

pub fn mul(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("mulsd"))
            } else {
                Ok(String::from("imulq"))
            }
        }
        _ => Err(String::from("Failed to form mul instruction")),
    }
}

pub fn div(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("divsd"))
            } else {
                Ok(String::from("idivq"))
            }
        }
        _ => Err(String::from("Failed to form div instruction")),
    }
}

pub fn cmp(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("comisd"))
            } else {
                Ok(String::from("cmpq"))
            }
        }
        _ => Err(String::from("Failed to form cmp instruction")),
    }
}

pub fn scratch_ret(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("%xmm0"))
            } else {
                Ok(String::from("%rax"))
            }
        }
        _ => Err(String::from("Failed to form return scratch register")),
    }
}

pub fn scratch_left(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("%xmm1"))
            } else {
                Ok(String::from("%rcx"))
            }
        }
        _ => Err(String::from("Failed to form left scratch register")),
    }
}

pub fn scratch_right(tp: &Type) -> Result<String, String> {
    match tp.element_tp().token() {
        Token::BasicType(lexeme, _, _) => {
            if lexeme == "float" {
                Ok(String::from("%xmm2"))
            } else {
                Ok(String::from("%rdx"))
            }
        }
        _ => Err(String::from("Failed to form right scratch register")),
    }
}

pub struct Address {
    offset: i32,
    reg: Option<Register>,
}

impl Address {
    pub fn new(offset: i32) -> Self {
        Address { offset, reg: None }
    }
}

pub struct CodeGenerator {
    address_descriptor: HashMap<String, Address>,
    register_descriptor: RegisterDescriptor,
    constants: String,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            address_descriptor: HashMap::new(),
            register_descriptor: RegisterDescriptor::new(),
            constants: String::new(),
        }
    }

    pub fn const_float(&mut self, constant: String) -> i64 {
        let mut res = 0;
        FLOAT_COUNTER.with(|counter| {
            res = *counter.borrow();
            *counter.borrow_mut() = res + 1;
        });
        let bits = to_bits_f64(constant);
        self.constants.push_str(format!(".LC{}:\n", res).as_str());
        self.constants
            .push_str(format!("\t.long {}\n", bits & 0x7fffffff).as_str());
        self.constants
            .push_str(format!("\t.long {}\n", bits >> 32).as_str());
        res
    }

    pub fn allocate_temps(&mut self, ir: &mut TACIr) {
        let len = ir.len();
        let mut i = 0;
        let mut j = 0;
        while i < len {
            if let TACOperator::Begin(n) = ir[i].op {
                let mut size = n;
                j = i + 1;
                while j < len {
                    if let TACOperator::End(_) = &ir[j].op {
                        ir[j].op = TACOperator::End(size);
                        break;
                    }
                    if let TACOperand::Temp(s, tp) = &ir[j].res {
                        size += tp.width() as i32;
                        ir[j].res = TACOperand::Var(s.to_owned(), tp.to_owned(), size);
                    }
                    j += 1
                }
                ir[i].op = TACOperator::Begin(size);
            }
            i += 1;
        }
    }

    pub fn clear_regs(&mut self) {
        for reg in GENERAL_REGISTERS {
            self.register_descriptor[reg] = None;
        }
    }

    pub fn get_reg(&mut self, val: &String, tp: &Type) -> Option<Register> {
        let mut empty_reg: Option<&Register> = None;
        if tp == &Type::float() {
            for reg in FLOAT_REGISTERS {
                if let Some(s) = &self.register_descriptor[reg] {
                    if s == val {
                        return Some(reg.to_owned());
                    }
                } else {
                    empty_reg = Some(reg);
                }
            }
        } else {
            for reg in GENERAL_REGISTERS {
                if let Some(s) = &self.register_descriptor[reg] {
                    if s == val {
                        return Some(reg.to_owned());
                    }
                } else {
                    empty_reg = Some(reg);
                }
            }
        }

        if let Some(reg) = empty_reg {
            self.register_descriptor[reg] = Some(val.to_owned());
            if let Some(address) = self.address_descriptor.get_mut(val) {
                address.reg = Some(reg.to_owned());
            }
            Some(reg.to_owned())
        } else {
            None
        }
    }

    pub fn arg_code(&mut self, arg: &TACOperand, b: &mut String) -> Result<String, String> {
        match arg {
            TACOperand::Var(s, tp, offset) => {
                if let Some(address) = self.address_descriptor.get(s) {
                    Ok(format!("{}(%rbp)", -address.offset))
                } else {
                    let address = Address::new(*offset);
                    self.address_descriptor.insert(s.to_owned(), address);
                    Ok(format!("{}(%rbp)", -offset))
                }
            }
            TACOperand::Temp(s, tp) => {
                let mov_code = mov(tp)?;
                if let Some(address) = self.address_descriptor.get(s) {
                    let offset = address.offset;
                    match &address.reg {
                        None => {
                            if let Some(reg) = self.get_reg(s, tp) {
                                b.push_str(
                                    format!("\t{} {}(%rbp), {}\n", mov_code, -offset, reg).as_str(),
                                );
                                Ok(reg.to_string())
                            } else {
                                Ok(format!("{}(%rbp)", -offset))
                            }
                        }
                        Some(reg) => Ok(reg.to_string()),
                    }
                } else {
                    Err(format!("Failed to allocate register for {}", s))
                }
            }
            TACOperand::Array(s, shift, _, offset) => match self.address_descriptor.get(shift) {
                Some(address) => {
                    let address_offset = address.offset;
                    let tp = Type::int();
                    let mov_code = mov(&tp)?;
                    match &address.reg {
                        None => {
                            if let Some(reg) = self.get_reg(shift, &tp) {
                                b.push_str(
                                    format!("\t{} {}(%rbp), {}\n", mov_code, -address_offset, reg)
                                        .as_str(),
                                );
                                if *offset > 0 {
                                    Ok(format!("{}(%rbp, {})", -offset, reg.to_string()))
                                } else {
                                    Ok(format!("(%rbp, {})", reg.to_string()))
                                }
                            } else {
                                let scratch_ret = scratch_ret(&tp)?;
                                b.push_str(
                                    format!(
                                        "\t{} {}(%rbp), {}\n",
                                        mov_code, -address_offset, scratch_ret
                                    )
                                    .as_str(),
                                );
                                Ok(format!("{}(%rbp, {})", -offset, scratch_ret))
                            }
                        }
                        Some(reg) => {
                            if *offset > 0 {
                                Ok(format!("{}(%rbp, {})", -offset, reg.to_string()))
                            } else {
                                Ok(format!("(%rbp, {})", reg.to_string()))
                            }
                        }
                    }
                }
                None => Err(format!("Failed to allocate register for {}", shift)),
            },
            TACOperand::Const(s, tp) => {
                if tp == &Type::float() {
                    if let Some(address) = self.address_descriptor.get(s) {
                        Ok(format!(".LC{}(%rip)", address.offset))
                    } else {
                        let l = self.const_float(s.to_owned());
                        let address = Address::new(l.try_into().unwrap());
                        self.address_descriptor.insert(s.to_owned(), address);
                        Ok(format!(".LC{}(%rip)", l))
                    }
                } else {
                    Ok(format!("${}", s))
                }
            }
            _ => Err(format!("Can generate arg from {}", arg)),
        }
    }

    pub fn program(&mut self, ir: &mut TACIr, b: &mut String) -> Result<(), String> {
        let mut float_defs = String::new();
        self.allocate_temps(ir);

        b.push_str(".global main\n");

        while let Some(tac) = ir.pop() {
            match tac.op {
                TACOperator::Null => {
                    b.push_str(format!("{}:\n", tac.res).as_str());
                }
                TACOperator::Begin(n) => {
                    if n > 0 {
                        let aligned_offset = align_stack(n)?;
                        b.push_str("\tpushq %rbp\n");
                        b.push_str("\tmovq %rsp, %rbp\n");
                        b.push_str(format!("\tsubq ${}, %rsp\n", aligned_offset).as_str());
                    }
                }
                TACOperator::End(n) => {
                    if n > 0 {
                        let aligned_offset = align_stack(n)?;
                        b.push_str(format!("\taddq ${}, %rsp\n", aligned_offset).as_str());
                        b.push_str("\tpopq %rbp\n");
                    }
                    b.push_str("\tret\n");
                    self.clear_regs();
                    self.address_descriptor.clear();
                }
                TACOperator::Assign => {
                    let res_tp = arg_tp(&tac.res)?;
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let mov_code = mov(res_tp)?;
                    let scratch_left = scratch_left(res_tp)?;

                    let mut arg1_code = self.arg_code(&tac.arg1, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if res_tp == &Type::float() && arg1_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_left).as_str(),
                        );
                    }
                    b.push_str(format!("\t{} {}, {}\n", mov_code, scratch_left, res_code).as_str());
                }
                TACOperator::Add => {
                    let res_tp = arg_tp(&tac.res)?;
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;

                    let mov_code = mov(res_tp)?;
                    let add_code = add(res_tp)?;
                    let scratch_left = scratch_left(res_tp)?;
                    let scratch_right = scratch_right(res_tp)?;

                    let mut arg1_code = self.arg_code(&tac.arg1, b)?;
                    let mut arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if res_tp == &Type::float() && arg1_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_left).as_str(),
                        );
                    }

                    if res_tp == &Type::float() && arg2_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_right).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", add_code, scratch_right, scratch_left).as_str(),
                    );
                    b.push_str(format!("\t{} {}, {}\n", mov_code, scratch_left, res_code).as_str());
                }
                TACOperator::Sub => {
                    let res_tp = arg_tp(&tac.res)?;
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;

                    let mov_code = mov(res_tp)?;
                    let sub_code = sub(res_tp)?;
                    let scratch_left = scratch_left(res_tp)?;
                    let scratch_right = scratch_right(res_tp)?;

                    let arg1_code = match tac.arg1 {
                        TACOperand::Null => {
                            if arg1_tp == &Type::float() {
                                let l = self.const_float(String::from("0.0"));
                                format!(".LC{}(%rip)", l)
                            } else {
                                String::from("$0")
                            }
                        }
                        _ => self.arg_code(&tac.arg1, b)?,
                    };
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if res_tp == &Type::float() && arg1_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_left).as_str(),
                        );
                    }

                    if res_tp == &Type::float() && arg2_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_right).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", sub_code, scratch_right, scratch_left).as_str(),
                    );
                    b.push_str(format!("\t{} {}, {}\n", mov_code, scratch_left, res_code).as_str());
                }
                TACOperator::Div => {
                    let res_tp = arg_tp(&tac.res)?;
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;

                    let mov_code = mov(res_tp)?;
                    let div_code = div(res_tp)?;
                    let scratch_ret = scratch_ret(res_tp)?;
                    let scratch_left = scratch_left(res_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if res_tp != &Type::float() {
                        b.push_str("\tmovq $0, %rdx\n");
                    }

                    if res_tp == &Type::float() && arg1_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_ret).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_ret).as_str(),
                        );
                    }

                    if res_tp != &Type::float() {
                        b.push_str("\tcqo\n");
                    }
                    if res_tp == &Type::float() && arg2_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                        b.push_str(format!("\t{} {}\n", div_code, scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                        b.push_str(format!("\t{} {}\n", div_code, scratch_left).as_str());
                    }
                    b.push_str(format!("\t{} {}, {}\n", mov_code, scratch_ret, res_code).as_str());
                }
                TACOperator::Mul => {
                    let res_tp = arg_tp(&tac.res)?;
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;

                    let mov_code = mov(res_tp)?;
                    let mul_code = mul(res_tp)?;
                    let scratch_left = scratch_left(res_tp)?;
                    let scratch_right = scratch_right(res_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if res_tp == &Type::float() && arg1_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_left).as_str(),
                        );
                    }

                    if res_tp == &Type::float() && arg2_tp != res_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_right).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", mul_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str(
                        format!("\t{} {}, {}\n", mov_code, scratch_right, res_code).as_str(),
                    );
                }
                TACOperator::Gt => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_left).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_right).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsetg %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Lt => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_right).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsetl %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Ge => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_right).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsetge %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Le => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_right).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsetle %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Eql => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_right).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsete %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Ne => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let arg2_tp = arg_tp(&tac.arg2)?;
                    let cmp_tp = match Type::max_type(arg1_tp, arg2_tp) {
                        Some(tp) => tp,
                        None => arg1_tp.to_owned(),
                    };
                    let mov_code = mov(&cmp_tp)?;
                    let cmp_code = cmp(&cmp_tp)?;
                    let scratch_left = scratch_left(&cmp_tp)?;
                    let scratch_right = scratch_right(&cmp_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;

                    if &cmp_tp == &Type::float() && arg1_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_right).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_right).as_str(),
                        );
                    }

                    if &cmp_tp == &Type::float() && arg2_tp != &cmp_tp {
                        b.push_str(format!("\tmovq {}, %rax\n", arg2_code).as_str());
                        b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_left).as_str());
                    } else {
                        b.push_str(
                            format!("\t{} {}, {}\n", mov_code, arg2_code, scratch_left).as_str(),
                        );
                    }

                    b.push_str(
                        format!("\t{} {}, {}\n", cmp_code, scratch_left, scratch_right).as_str(),
                    );
                    b.push_str("\tsetne %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmovq %rcx, {}\n", res_code).as_str());
                }
                TACOperator::And => {
                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;
                    b.push_str(format!("\tmovq {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\tand {}, {}\n", arg2_code, res_code).as_str());
                }
                TACOperator::Or => {
                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let arg2_code = self.arg_code(&tac.arg2, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;
                    b.push_str(format!("\tmovq {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\tor {}, {}\n", arg2_code, res_code).as_str());
                }
                TACOperator::Call(n) => {
                    let res_tp = arg_tp(&tac.res)?;
                    let mov_code = mov(res_tp)?;
                    let res_ret = scratch_ret(res_tp)?;

                    b.push_str(format!("\tcall {}\n", tac.arg1).as_str());
                    match tac.res {
                        TACOperand::Null => (),
                        _ => {
                            let arg1_tp = arg_tp(&tac.arg1)?;
                            let arg1_ret = scratch_ret(arg1_tp)?;
                            let res_code = self.arg_code(&tac.res, b)?;
                            if res_tp == &Type::float() && arg1_tp != res_tp {
                                b.push_str(
                                    format!("\tcvtsi2sdq {}, {}\n", arg1_ret, res_code).as_str(),
                                );
                            } else {
                                b.push_str(
                                    format!("\t{} {}, {}\n", mov_code, res_ret, res_code).as_str(),
                                );
                            }
                        }
                    }
                    b.push_str(format!("\taddq ${}, %rsp\n", 8 * n).as_str());
                    self.clear_regs();
                }
                TACOperator::Ret(ret_tp) => match tac.res {
                    TACOperand::Null => (),
                    _ => {
                        let res_tp = arg_tp(&tac.res)?;
                        let mov_code = mov(&ret_tp)?;
                        let scratch_ret = scratch_ret(&ret_tp)?;

                        let res_code = self.arg_code(&tac.res, b)?;

                        if &ret_tp == &Type::float() && res_tp != &ret_tp {
                            b.push_str(format!("\tmovq {}, %rax\n", res_code).as_str());
                            b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_ret).as_str());
                        } else {
                            b.push_str(
                                format!("\t{} {}, {}\n", mov_code, res_code, scratch_ret).as_str(),
                            );
                        }
                    }
                },
                TACOperator::Param(param_tp) => {
                    let arg1_tp = arg_tp(&tac.arg1)?;
                    let mov_code = mov(&param_tp)?;
                    let scratch_ret = scratch_ret(&param_tp)?;

                    let arg1_code = self.arg_code(&tac.arg1, b)?;

                    if &param_tp == &Type::float() {
                        if arg1_tp != &param_tp {
                            b.push_str(format!("\tmovq {}, %rax\n", arg1_code).as_str());
                            b.push_str(format!("\tcvtsi2sdq %rax, {}\n", scratch_ret).as_str());
                        } else {
                            b.push_str(
                                format!("\t{} {}, {}\n", mov_code, arg1_code, scratch_ret).as_str(),
                            );
                        }
                        b.push_str("\tleaq -8(%rsp), %rsp\n");
                        b.push_str(format!("\t{} {}, (%rsp)\n", mov_code, scratch_ret).as_str());
                    } else {
                        b.push_str(format!("\tpushq {}\n", arg1_code).as_str());
                    }
                }
                TACOperator::Goto => {
                    b.push_str(format!("\tjmp {}\n", tac.res).as_str());
                }
                TACOperator::If => {
                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let res_code = tac.res.to_string();
                    b.push_str(format!("\tmovq {}, %rcx\n", arg1_code).as_str());
                    b.push_str("\ttest %rcx, %rcx\n");
                    b.push_str(format!("\tjne {}\n", res_code).as_str());
                }
                TACOperator::Iff => {
                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let res_code = tac.res.to_string();
                    b.push_str(format!("\tmovq {}, %rcx\n", arg1_code).as_str());
                    b.push_str("\ttest %rcx, %rcx\n");
                    b.push_str(format!("\tje {}\n", res_code).as_str());
                }
                TACOperator::Not => {
                    let arg1_code = self.arg_code(&tac.arg1, b)?;
                    let res_code = self.arg_code(&tac.res, b)?;
                    b.push_str(format!("\tmovq {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\txor $1, {}\n", res_code).as_str());
                }
            }
        }
        b.push_str(self.constants.as_str());
        Ok(())
    }
}
