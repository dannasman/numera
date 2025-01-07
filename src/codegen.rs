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
    R13, // has to be preserved
    R14, // has to be preserved
    R15, // has to be preserved
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

pub fn align_stack(n: i32) -> Result<i32, String> {
    if n > 0 {
        let aligned = (n + 15) & !15;
        Ok(aligned)
    } else {
        Err(String::from("Alignment of non positive integer no allowed"))
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
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            address_descriptor: HashMap::new(),
            register_descriptor: RegisterDescriptor::new(),
        }
    }

    pub fn find_reg(&mut self, val: &String) -> bool {
        for reg in GENERAL_REGISTERS {
            if let Some(s) = &self.register_descriptor[reg] {
                if s == val {
                    return true;
                }
            }
        }
        false
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

    pub fn get_reg(&mut self, val: &String) -> Option<Register> {
        let mut empty_reg: Option<&Register> = None;
        for reg in GENERAL_REGISTERS {
            if let Some(s) = &self.register_descriptor[reg] {
                if s == val {
                    return Some(reg.to_owned());
                }
            } else {
                empty_reg = Some(reg);
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

    pub fn arg_code(&mut self, arg: TACOperand, b: &mut String) -> Result<String, String> {
        match arg {
            TACOperand::Var(s, tp, offset) => {
                if let Some(address) = self.address_descriptor.get(&s) {
                    Ok(format!("{}(%rbp)", -address.offset))
                } else {
                    let address = Address::new(offset);
                    self.address_descriptor.insert(s.to_owned(), address);
                    Ok(format!("{}(%rbp)", -offset))
                }
            }
            TACOperand::Temp(s, tp) => {
                if let Some(address) = self.address_descriptor.get(&s) {
                    let offset = address.offset;
                    match &address.reg {
                        None => {
                            if let Some(reg) = self.get_reg(&s) {
                                b.push_str(format!("\tmov {}(%rbp), {}\n", -offset, reg).as_str());
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
            TACOperand::Array(s, shift, tp, offset) => match self.address_descriptor.get(&shift) {
                Some(address) => {
                    let address_offset = address.offset;
                    match &address.reg {
                        None => {
                            if let Some(reg) = self.get_reg(&shift) {
                                b.push_str(
                                    format!("\tmov {}(%rbp), {}\n", -address_offset, reg).as_str(),
                                );
                                if offset > 0 {
                                    Ok(format!("{}(%rbp, {})", -offset, reg.to_string()))
                                } else {
                                    Ok(format!("(%rbp, {})", reg.to_string()))
                                }
                            } else {
                                b.push_str(format!("\tmov {}(%rbp), %rax\n", -address_offset).as_str());
                                Ok(format!("{}(%rbp, %rax)", -offset))
                            }
                        }
                        Some(reg) => {
                            if offset > 0 {
                                Ok(format!("{}(%rbp, {})", -offset, reg.to_string()))
                            } else {
                                Ok(format!("(%rbp, {})", reg.to_string()))
                            }
                        }
                    }
                }
                None => Err(format!("Failed to allocate register for {}", shift)),
            },
            TACOperand::Const(s, tp) => Ok(format!("${}", s)),
            _ => Err(format!("Can generate arg from {}", arg)),
        }
    }

    // TODO: korjaa funktioihin liittyvä -4(%rbp) kikkailu
    pub fn program(&mut self, ir: &mut TACIr, b: &mut String) -> Result<(), String> {
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
                        b.push_str("\tpush %rbp\n");
                        b.push_str("\tmov %rsp, %rbp\n");
                        b.push_str(format!("\tsub ${}, %rsp\n", aligned_offset).as_str());
                    }
                }
                TACOperator::End(n) => {
                    if n > 0 {
                        let aligned_offset = align_stack(n)?;
                        b.push_str(format!("\tadd ${}, %rsp\n", aligned_offset).as_str());
                        b.push_str("\tpop %rbp\n");
                    }
                    b.push_str("\tret\n");
                    self.clear_regs();
                    self.address_descriptor.clear();
                }
                TACOperator::Assign => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Add => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tadd %rcx, %rdx\n");
                    b.push_str(format!("\tmov %rdx, {}\n", res_code).as_str());
                }
                TACOperator::Sub => {
                    let arg1_code = match tac.arg1 {
                        TACOperand::Null => String::from("$0"),
                        _ => self.arg_code(tac.arg1, b)?,
                    };
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                    b.push_str(format!("\tsub %rdx, {}\n", res_code).as_str());
                }
                TACOperator::Div => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str("\tmov $0, %rdx\n");
                    b.push_str(format!("\tmov {}, %rax\n", arg1_code).as_str());
                    b.push_str("\tcqo\n");
                    b.push_str(format!("\tidivq {}\n", arg2_code).as_str());
                    b.push_str(format!("\tmov %rax, {}\n", res_code).as_str());
                }
                TACOperator::Mul => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\timul %rcx, %rdx\n");
                    b.push_str(format!("\tmov %rdx, {}\n", res_code).as_str());
                }
                TACOperator::Gt => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsetg %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Lt => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsetl %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Ge => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsetge %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Le => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsetle %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Eql => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsete %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::Ne => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str(format!("\tmov {}, %rdx\n", arg2_code).as_str());
                    b.push_str("\tcmpq %rdx, %rcx\n");
                    b.push_str("\tsetne %al\n");
                    b.push_str("\tmovzx %al, %rcx\n");
                    b.push_str(format!("\tmov %rcx, {}\n", res_code).as_str());
                }
                TACOperator::And => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\tand {}, {}\n", arg2_code, res_code).as_str());
                }
                TACOperator::Or => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let arg2_code = self.arg_code(tac.arg2, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\tor {}, {}\n", arg2_code, res_code).as_str());
                }
                TACOperator::Call(n) => {
                    b.push_str(format!("\tcall {}\n", tac.arg1).as_str());
                    match tac.res {
                        TACOperand::Null => (),
                        _ => {
                            let res_code = self.arg_code(tac.res, b)?;
                            b.push_str(format!("\tmov %rax, {}\n", res_code).as_str());
                        }
                    }
                    b.push_str(format!("\tadd ${}, %rsp\n", 8 * n).as_str());
                    self.clear_regs();
                }
                TACOperator::Ret => match tac.res {
                    TACOperand::Null => (),
                    _ => {
                        let res_code = self.arg_code(tac.res, b)?;
                        b.push_str(format!("\tmov {}, %rax\n", res_code).as_str());
                    }
                },
                TACOperator::Param => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    b.push_str(format!("\tpush {}\n", arg1_code).as_str());
                }
                TACOperator::Goto => {
                    b.push_str(format!("\tjmp {}\n", tac.res).as_str());
                }
                TACOperator::If => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let res_code = tac.res.to_string();
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str("\ttest %rcx, %rcx\n");
                    b.push_str(format!("\tjne {}\n", res_code).as_str());
                }
                TACOperator::Iff => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let res_code = tac.res.to_string();
                    b.push_str(format!("\tmov {}, %rcx\n", arg1_code).as_str());
                    b.push_str("\ttest %rcx, %rcx\n");
                    b.push_str(format!("\tje {}\n", res_code).as_str());
                }
                TACOperator::Not => {
                    let arg1_code = self.arg_code(tac.arg1, b)?;
                    let res_code = self.arg_code(tac.res, b)?;
                    b.push_str(format!("\tmov {}, {}\n", arg1_code, res_code).as_str());
                    b.push_str(format!("\txor $1, {}\n", res_code).as_str());
                }
            }
        }
        Ok(())
    }
}
