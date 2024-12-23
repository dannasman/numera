use std::fmt;
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

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::RAX => write!(f, "rax"),
            Register::RBP => write!(f, "rbp"),
            Register::RSP => write!(f, "rsp"),
            Register::RBX => write!(f, "rbx"),
            Register::RCX => write!(f, "rcx"),
            Register::RDX => write!(f, "rdx"),
            Register::RSI => write!(f, "rsi"),
            Register::RDI => write!(f, "rdi"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
        }
    }
}

static mut REGISTER_DESCRIPTOR: Mutex<[(Register, Option<TACOperand>); 16]> = Mutex::new([
    (Register::RAX, None),
    (Register::RBX, None),
    (Register::RCX, None),
    (Register::RDX, None),
    (Register::RBP, None),
    (Register::RSP, None),
    (Register::RSI, None),
    (Register::RDI, None),
    (Register::R8, None),
    (Register::R9, None),
    (Register::R10, None),
    (Register::R11, None),
    (Register::R12, None),
    (Register::R13, None),
    (Register::R14, None),
    (Register::R15, None),
]);

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
}

struct AddressDescriptor {
    table: Vec<(String, Register)>,
}

impl AddressDescriptor {
    pub const fn new() -> Self {
        AddressDescriptor { table: Vec::new() }
    }

    pub fn put(&mut self, s: String, r: Register) {
        for (st, rt) in self.table.iter_mut() {
            if *st == s {
                *rt = r;
                return;
            }
        }
        self.table.push((s, r));
    }

    pub fn get(&mut self, s: String) -> Option<Register> {
        for (st, rt) in self.table.iter() {
            if *st == s {
                return Some(rt.to_owned());
            }
        }
        None
    }

    pub fn clear(&mut self) {
        self.table.clear()
    }
}

static mut ADDRESS_DESCRIPTOR: Mutex<AddressDescriptor> = Mutex::new(AddressDescriptor::new());

pub fn put_value(s: String, r: Register) {
    unsafe { ADDRESS_DESCRIPTOR.lock().unwrap().put(s, r) }
}

pub fn get_value(s: String) -> Option<Register> {
    unsafe { ADDRESS_DESCRIPTOR.lock().unwrap().get(s) }
}

pub trait CodeGen {
    fn codegen(&self, s: &mut String) {}
}
