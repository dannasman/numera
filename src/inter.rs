/*
 * TODO:
 * Add use of TACState (NOTE: Unary next)
 * Try to only use gen_tac and reduce_tac!!!
 */

use super::lexer::Token;
use super::tac::*;
use std::cell::RefCell;
use std::rc::Rc;

pub trait ExprNode {
    fn emit_label(&self, tac_ir: TACState, i: u32) {
        let instruction = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}:", i)),
        );
        tac_ir.push(instruction);
    }

    fn emit_jumps(&self, tac_ir: TACState, test: TACOperand, t: u32, f: u32) {
        if t != 0 && f != 0 {
            let instruction1 = TACInstruction::new(
                TACOperator::GOTO,
                TACOperand::IF,
                test,
                TACOperand::LABEL(format!("L{}", t)),
            );
            tac_ir.push(instruction1);
            let instruction2 = TACInstruction::new(
                TACOperator::GOTO,
                TACOperand::NULL,
                TACOperand::NULL,
                TACOperand::LABEL(format!("{}", f)),
            );
            tac_ir.push(instruction2);
        } else if t != 0 {
            let instruction = TACInstruction::new(
                TACOperator::GOTO,
                TACOperand::IF,
                test,
                TACOperand::LABEL(format!("L{}", t)),
            );
            tac_ir.push(instruction);
        } else if f != 0 {
            let instruction = TACInstruction::new(
                TACOperator::GOTO,
                TACOperand::IFFALSE,
                test,
                TACOperand::LABEL(format!("L{}", f)),
            );
            tac_ir.push(instruction);
        }
    }

    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        self.emit_jumps(tac_ir.clone(), self.reduce_tac(tac_ir.clone()), t, f);
    }

    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand);

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand;
}

pub trait StmtNode {
    fn emit_label(&self, tac_ir: TACState, i: u32) {
        let instruction = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}:", i)),
        );
        tac_ir.push(instruction);
    }

    fn gen_tac(&self, tac_ir: TACState, b: u32, a: u32);
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ExprUnion {
    Id(Rc<Id>),
    Arith(Rc<Arith>),
    Temp(Rc<Temp>),
    Unary(Rc<Unary>),
    Constant(Rc<Constant>),
    Or(Rc<Or>),
    And(Rc<And>),
    Not(Rc<Not>),
    Rel(Rc<Rel>),
    Access(Rc<Access>),
    Call(Rc<Call>),
}

impl ExprUnion {
    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        match self {
            ExprUnion::Id(id) => id.gen_tac(tac_ir),
            ExprUnion::Arith(arith) => arith.gen_tac(tac_ir),
            ExprUnion::Temp(temp) => temp.gen_tac(tac_ir),
            ExprUnion::Unary(unary) => unary.gen_tac(tac_ir),
            ExprUnion::Constant(constant) => constant.gen_tac(tac_ir),
            ExprUnion::Or(or) => or.gen_tac(tac_ir),
            ExprUnion::And(and) => and.gen_tac(tac_ir),
            ExprUnion::Not(not) => not.gen_tac(tac_ir),
            ExprUnion::Rel(rel) => rel.gen_tac(tac_ir),
            ExprUnion::Access(acc) => acc.gen_tac(tac_ir),
            ExprUnion::Call(call) => call.gen_tac(tac_ir),
        }
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        match self {
            ExprUnion::Id(id) => id.reduce_tac(tac_ir),
            ExprUnion::Arith(arith) => arith.reduce_tac(tac_ir),
            ExprUnion::Temp(temp) => temp.reduce_tac(tac_ir),
            ExprUnion::Unary(unary) => unary.reduce_tac(tac_ir),
            ExprUnion::Constant(constant) => constant.reduce_tac(tac_ir),
            ExprUnion::Or(or) => or.reduce_tac(tac_ir),
            ExprUnion::And(and) => and.reduce_tac(tac_ir),
            ExprUnion::Not(not) => not.reduce_tac(tac_ir),
            ExprUnion::Rel(rel) => rel.reduce_tac(tac_ir),
            ExprUnion::Access(acc) => acc.reduce_tac(tac_ir),
            ExprUnion::Call(call) => call.reduce_tac(tac_ir),
        }
    }

    pub fn get_type(&self) -> Token {
        match self {
            ExprUnion::Id(id) => id.tp.clone(),
            ExprUnion::Arith(arith) => arith.tp.clone(),
            ExprUnion::Temp(temp) => temp.tp.clone(),
            ExprUnion::Unary(unary) => unary.tp.clone(),
            ExprUnion::Constant(constant) => constant.tp.clone(),
            ExprUnion::Or(or) => or.tp.clone(),
            ExprUnion::And(and) => and.tp.clone(),
            ExprUnion::Not(not) => not.tp.clone(),
            ExprUnion::Rel(rel) => rel.tp.clone(),
            ExprUnion::Access(acc) => acc.tp.clone(),
            ExprUnion::Call(call) => call.id.tp.clone(),
        }
    }

    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        match self {
            ExprUnion::Id(id) => id.jumping(tac_ir, t, f),
            ExprUnion::Arith(arith) => arith.jumping(tac_ir, t, f),
            ExprUnion::Temp(temp) => temp.jumping(tac_ir, t, f),
            ExprUnion::Unary(unary) => unary.jumping(tac_ir, t, f),
            ExprUnion::Constant(constant) => constant.jumping(tac_ir, t, f),
            ExprUnion::Or(or) => or.jumping(tac_ir, t, f),
            ExprUnion::And(and) => and.jumping(tac_ir, t, f),
            ExprUnion::Not(not) => not.jumping(tac_ir, t, f),
            ExprUnion::Rel(rel) => rel.jumping(tac_ir, t, f),
            ExprUnion::Access(acc) => acc.jumping(tac_ir, t, f),
            ExprUnion::Call(call) => call.jumping(tac_ir, t, f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtUnion {
    If(Rc<If>),
    Else(Rc<Else>),
    While(Rc<RefCell<While>>),
    Set(Rc<Set>),
    SetElem(Rc<SetElem>),
    Seq(Rc<Seq>),
    Break(Rc<Break>),
    Function(Rc<Function>),
    FunctionCall(Rc<FunctionCall>), //this is a wrapper struct implemented to handle void function calls
    Return(Rc<Return>),
}

impl StmtUnion {
    fn after(&self) -> u32 {
        match self {
            StmtUnion::While(while_stmt) => {
                let while_borrow = while_stmt.borrow_mut();
                let after_borrow = while_borrow.after.borrow_mut();
                let after = *after_borrow;
                drop(after_borrow);
                drop(while_borrow);
                after
            }
            _ => 0,
        }
    }
    pub fn emit_label(&self, tac_ir: TACState, i: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.emit_label(tac_ir, i),
            StmtUnion::Else(else_stmt) => else_stmt.emit_label(tac_ir, i),
            StmtUnion::While(while_stmt) => while_stmt.borrow().emit_label(tac_ir, i),
            StmtUnion::Set(set_stmt) => set_stmt.emit_label(tac_ir, i),
            StmtUnion::SetElem(set_stmt) => set_stmt.emit_label(tac_ir, i),
            StmtUnion::Seq(seq_stmt) => seq_stmt.emit_label(tac_ir, i),
            StmtUnion::Break(break_stmt) => break_stmt.emit_label(tac_ir, i),
            StmtUnion::Function(function_stmt) => function_stmt.emit_label(tac_ir, i),
            StmtUnion::FunctionCall(call_stmt) => call_stmt.emit_label(tac_ir, i),
            StmtUnion::Return(return_stmt) => return_stmt.emit_label(tac_ir, i),
        }
    }
    pub fn gen_tac(&self, tac_ir: TACState, b: u32, a: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::Else(else_stmt) => else_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::While(while_stmt) => while_stmt.borrow().gen_tac(tac_ir, b, a),
            StmtUnion::Set(set_stmt) => set_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::SetElem(set_stmt) => set_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::Seq(seq_stmt) => seq_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::Break(break_stmt) => break_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::Function(function_stmt) => function_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::FunctionCall(call_stmt) => call_stmt.gen_tac(tac_ir, b, a),
            StmtUnion::Return(return_stmt) => return_stmt.gen_tac(tac_ir, b, a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Id {
    pub tp: Token, //type
    pub token: Token,
    _offset: u32,
}

impl Id {
    pub fn new(tp: Token, token: Token, b: u32) -> Self {
        Id {
            tp,
            token,
            _offset: b,
        }
    }
}

impl ExprNode for Id {
    fn gen_tac(&self, _tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        match &self.tp {
            Token::Int(_) => (
                TACOperator::NONE,
                TACOperand::VAR_INT(self.token.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            Token::Float(_) => (
                TACOperator::NONE,
                TACOperand::VAR_FLOAT(self.token.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            Token::Bool(_) => (
                TACOperator::NONE,
                TACOperand::VAR_BOOL(self.token.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            Token::Arr(arr) => match *arr.of {
                Token::Int(_) => (
                    TACOperator::NONE,
                    TACOperand::VAR_INT(self.token.to_owned().value_to_string()),
                    TACOperand::NULL,
                ),
                Token::Float(_) => (
                    TACOperator::NONE,
                    TACOperand::VAR_FLOAT(self.token.to_owned().value_to_string()),
                    TACOperand::NULL,
                ),
                Token::Bool(_) => (
                    TACOperator::NONE,
                    TACOperand::VAR_BOOL(self.token.to_owned().value_to_string()),
                    TACOperand::NULL,
                ),
                _ => (TACOperator::NONE, TACOperand::NULL, TACOperand::NULL),
            },
            _ => (TACOperator::NONE, TACOperand::NULL, TACOperand::NULL),
        }
    }

    fn reduce_tac(&self, _tac_ir: TACState) -> TACOperand {
        match &self.tp {
            Token::Int(_) => TACOperand::VAR_INT(self.token.to_owned().value_to_string()),
            Token::Float(_) => TACOperand::VAR_FLOAT(self.token.to_owned().value_to_string()),
            Token::Bool(_) => TACOperand::VAR_BOOL(self.token.to_owned().value_to_string()),
            Token::Arr(arr) => match *arr.of {
                Token::Int(_) => TACOperand::VAR_INT(self.token.to_owned().value_to_string()),
                Token::Float(_) => TACOperand::VAR_FLOAT(self.token.to_owned().value_to_string()),
                Token::Bool(_) => TACOperand::VAR_BOOL(self.token.to_owned().value_to_string()),
                _ => TACOperand::NULL,
            },
            _ => TACOperand::NULL,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    id: Id,
    params: Vec<ExprUnion>,
    temp_count: Rc<RefCell<u32>>,
}

impl Call {
    pub fn new(id: Id, params: Vec<ExprUnion>, temp_count: Rc<RefCell<u32>>) -> Self {
        Self {
            id,
            params,
            temp_count,
        }
    }
}

impl ExprNode for Call {
    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        self.params.iter().for_each(|e| {
            let arg1 = e.reduce_tac(tac_ir.clone());
            let instruction =
                TACInstruction::new(TACOperator::PUSH, arg1, TACOperand::NULL, TACOperand::NULL);
            tac_ir.push(instruction);
        });
        (
            TACOperator::CALL,
            self.id.reduce_tac(tac_ir.clone()),
            TACOperand::NULL,
        )
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        self.params.iter().for_each(|e| {
            let arg1 = e.reduce_tac(tac_ir.clone());
            let instruction =
                TACInstruction::new(TACOperator::PUSH, arg1, TACOperand::NULL, TACOperand::NULL);
            tac_ir.push(instruction);
        });
        let temp = Temp::new(self.id.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());
        let instruction = TACInstruction::new(
            TACOperator::CALL,
            self.id.reduce_tac(tac_ir.clone()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction);
        result
    }
}

#[derive(Debug, Clone)]
pub struct Arith {
    tp: Token, //type
    op: Token,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl Arith {
    pub fn new(
        op: Token,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        match expr1.get_type() {
            Token::Int(s1) => match expr2.get_type() {
                Token::Int(_) => Ok(Arith {
                    tp: Token::Int(s1),
                    op,
                    temp_count,
                    expr1,
                    expr2,
                }),
                Token::Float(s2) => Ok(Arith {
                    tp: Token::Float(s2),
                    op,
                    temp_count,
                    expr1,
                    expr2,
                }),
                _ => Err("wrong expression type"),
            },
            Token::Float(s1) => match expr2.get_type() {
                Token::Int(_) => Ok(Arith {
                    tp: Token::Float(s1),
                    op,
                    temp_count,
                    expr1,
                    expr2,
                }),
                Token::Float(s2) => Ok(Arith {
                    tp: Token::Float(s2),
                    op,
                    temp_count,
                    expr1,
                    expr2,
                }),
                _ => Err("wrong expression type"),
            },
            _ => Err("wrong expression type"),
        }
    }
}

impl ExprNode for Arith {
    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let e1 = self.expr1.reduce_tac(tac_ir.clone());
        let e2 = self.expr2.reduce_tac(tac_ir.clone());

        match self.op {
            Token::Add(_) => (TACOperator::ADD, e1, e2),
            Token::Sub(_) => (TACOperator::SUB, e1, e2),
            Token::Mul(_) => (TACOperator::MUL, e1, e2),
            Token::Div(_) => (TACOperator::DIV, e1, e2),
            _ => (TACOperator::NONE, TACOperand::NULL, TACOperand::NULL),
        }
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());
        let (op, arg1, arg2) = self.gen_tac(tac_ir.clone());
        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);
        result
    }
}

#[derive(Debug, Clone)]
pub struct Temp {
    tp: Token, //type
    number: u32,
}

impl Temp {
    pub fn new(tp: Token, temp_count: Rc<RefCell<u32>>) -> Self {
        let mut c = temp_count.borrow_mut();
        *c += 1;
        let number = *c;
        drop(c);
        Temp { tp, number }
    }
}

impl ExprNode for Temp {
    fn gen_tac(&self, _tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        match self.tp {
            Token::Int(_) => (
                TACOperator::NONE,
                TACOperand::TEMP_INT(format!("t{}", self.number)),
                TACOperand::NULL,
            ),
            Token::Float(_) => (
                TACOperator::NONE,
                TACOperand::TEMP_FLOAT(format!("t{}", self.number)),
                TACOperand::NULL,
            ),
            Token::Bool(_) => (
                TACOperator::NONE,
                TACOperand::TEMP_BOOL(format!("t{}", self.number)),
                TACOperand::NULL,
            ),
            _ => (TACOperator::NONE, TACOperand::NULL, TACOperand::NULL),
        }
    }

    fn reduce_tac(&self, _tac_ir: TACState) -> TACOperand {
        match self.tp {
            Token::Int(_) => TACOperand::TEMP_INT(format!("t{}", self.number)),
            Token::Float(_) => TACOperand::TEMP_FLOAT(format!("t{}", self.number)),
            Token::Bool(_) => TACOperand::TEMP_BOOL(format!("t{}", self.number)),
            _ => TACOperand::NULL,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    tp: Token, //type
    op: Token,
    temp_count: Rc<RefCell<u32>>,
    expr: ExprUnion,
}

impl Unary {
    pub fn new(op: Token, temp_count: Rc<RefCell<u32>>, expr: ExprUnion) -> Self {
        Unary {
            tp: expr.get_type(),
            op,
            temp_count,
            expr,
        }
    }
}

impl ExprNode for Unary {
    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let arg1 = self.expr.reduce_tac(tac_ir.clone());
        let op = match self.op {
            Token::Sub(_) => TACOperator::SUB,
            _ => TACOperator::NONE,
        };
        (op, arg1, TACOperand::NULL)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());
        let (op, arg1, arg2) = self.gen_tac(tac_ir.clone());
        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);
        result
    }
}

#[derive(Debug, Clone)]
pub struct Constant {
    tp: Token,
    constant: Token,
}

impl Constant {
    pub fn new(tp: Token, constant: Token) -> Self {
        Constant { tp, constant }
    }
}

impl ExprNode for Constant {
    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        match self.constant {
            Token::True(_) => {
                if t != 0 {
                    let instruction = TACInstruction::new(
                        TACOperator::GOTO,
                        TACOperand::NULL,
                        TACOperand::NULL,
                        TACOperand::LABEL(format!("{}", t)),
                    );
                    tac_ir.push(instruction);
                }
            }
            Token::False(_) => {
                if f != 0 {
                    let instruction = TACInstruction::new(
                        TACOperator::GOTO,
                        TACOperand::NULL,
                        TACOperand::NULL,
                        TACOperand::LABEL(format!("{}", f)),
                    );
                    tac_ir.push(instruction);
                }
            }
            _ => (),
        }
    }

    fn gen_tac(&self, _tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        match self.tp {
            Token::Int(_) => (
                TACOperator::NONE,
                TACOperand::CONST_INT(self.constant.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            Token::Float(_) => (
                TACOperator::NONE,
                TACOperand::CONST_FLOAT(self.constant.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            Token::Bool(_) => (
                TACOperator::NONE,
                TACOperand::CONST_BOOL(self.constant.to_owned().value_to_string()),
                TACOperand::NULL,
            ),
            _ => (TACOperator::NONE, TACOperand::NULL, TACOperand::NULL),
        }
    }

    fn reduce_tac(&self, _tac_ir: TACState) -> TACOperand {
        match self.tp {
            Token::Int(_) => TACOperand::CONST_INT(self.constant.to_owned().value_to_string()),
            Token::Float(_) => TACOperand::CONST_FLOAT(self.constant.to_owned().value_to_string()),
            Token::Bool(_) => TACOperand::CONST_BOOL(self.constant.to_owned().value_to_string()),
            _ => TACOperand::NULL,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Or {
    tp: Token, //type
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl Or {
    pub fn new(
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s1) = expr1.get_type() {
            if let Token::Bool(_) = expr2.get_type() {
                return Ok(Or {
                    tp: Token::Bool(s1),
                    label,
                    temp_count,
                    expr1,
                    expr2,
                });
            }
        }
        Err("wrong expression type")
    }
}

impl ExprNode for Or {
    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        let mut new_label = t;
        if t == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
            drop(l);
        }
        self.expr1.jumping(tac_ir.clone(), new_label, 0);
        self.expr2.jumping(tac_ir.clone(), t, f);
        if t == 0 {
            self.emit_label(tac_ir.clone(), new_label);
        }
    }

    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let op = TACOperator::OR;
        let arg1 = self.expr1.reduce_tac(tac_ir.clone());
        let arg2 = self.expr2.reduce_tac(tac_ir.clone());
        (op, arg1, arg2)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());

        self.jumping(tac_ir.clone(), 0, f);

        let instruction1 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("true".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction1);

        let instruction2 = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", a)),
        );
        tac_ir.push(instruction2);

        self.emit_label(tac_ir.clone(), f);

        let instruction3 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("false".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction3);

        self.emit_label(tac_ir.clone(), a);

        result
    }
}

#[derive(Debug, Clone)]
pub struct And {
    tp: Token, //type
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl And {
    pub fn new(
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s1) = expr1.get_type() {
            if let Token::Bool(_) = expr2.get_type() {
                return Ok(And {
                    tp: Token::Bool(s1),
                    label,
                    temp_count,
                    expr1,
                    expr2,
                });
            }
        }
        Err("wrong expression type")
    }
}

impl ExprNode for And {
    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        let mut new_label = f;
        if f == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
            drop(l);
        }
        self.expr1.jumping(tac_ir.clone(), 0, new_label);
        self.expr2.jumping(tac_ir.clone(), t, f);
        if f == 0 {
            self.emit_label(tac_ir.clone(), new_label);
        }
    }

    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let op = TACOperator::AND;
        let arg1 = self.expr1.reduce_tac(tac_ir.clone());
        let arg2 = self.expr2.reduce_tac(tac_ir.clone());
        (op, arg1, arg2)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());

        self.jumping(tac_ir.clone(), 0, f);

        let instruction1 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("true".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction1);

        let instruction2 = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", a)),
        );
        tac_ir.push(instruction2);

        self.emit_label(tac_ir.clone(), f);

        let instruction3 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("false".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction3);

        self.emit_label(tac_ir.clone(), a);

        result
    }
}

#[derive(Debug, Clone)]
pub struct Not {
    tp: Token,
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr: ExprUnion,
}

impl Not {
    pub fn new(
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s) = expr.get_type() {
            return Ok(Not {
                tp: Token::Bool(s),
                label,
                temp_count,
                expr,
            });
        }
        Err("wrong expression type")
    }
}

impl ExprNode for Not {
    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        self.expr.jumping(tac_ir.clone(), f, t);
    }

    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let arg1 = self.reduce_tac(tac_ir.clone());
        (TACOperator::NONE, arg1, TACOperand::NULL)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());

        self.jumping(tac_ir.clone(), 0, f);

        let instruction1 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("true".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction1);

        let instruction2 = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", a)),
        );
        tac_ir.push(instruction2);

        self.emit_label(tac_ir.clone(), f);

        let instruction3 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("false".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction3);

        self.emit_label(tac_ir.clone(), a);

        result
    }
}

#[derive(Debug, Clone)]
pub struct Rel {
    tp: Token,
    op: Token,
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl Rel {
    pub fn new(
        op: Token,
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        if expr1.get_type() == expr2.get_type() {
            Ok(Rel {
                tp: Token::Bool(String::from("bool")),
                op,
                label,
                temp_count,
                expr1,
                expr2,
            })
        } else {
            Err("wrong expression type")
        }
    }
}

impl ExprNode for Rel {
    fn jumping(&self, tac_ir: TACState, t: u32, f: u32) {
        let op = match self.op {
            Token::Eql(_) => TACOperator::EQ,
            Token::Ne(_) => TACOperator::NEQ,
            Token::Lt(_) => TACOperator::LT,
            Token::Le(_) => TACOperator::LE,
            Token::Gt(_) => TACOperator::GT,
            Token::Ge(_) => TACOperator::GE,
            _ => TACOperator::NONE,
        };
        let arg1 = self.expr1.reduce_tac(tac_ir.clone());
        let arg2 = self.expr2.reduce_tac(tac_ir.clone());

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());

        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);

        self.emit_jumps(tac_ir.clone(), result, t, f);
    }

    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let op = match self.op {
            Token::Eql(_) => TACOperator::EQ,
            Token::Ne(_) => TACOperator::NEQ,
            Token::Lt(_) => TACOperator::LT,
            Token::Le(_) => TACOperator::LE,
            Token::Gt(_) => TACOperator::GT,
            Token::Ge(_) => TACOperator::GE,
            _ => TACOperator::NONE,
        };
        let arg1 = self.expr1.reduce_tac(tac_ir.clone());
        let arg2 = self.expr2.reduce_tac(tac_ir.clone());
        (op, arg1, arg2)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let result = temp.reduce_tac(tac_ir.clone());

        self.jumping(tac_ir.clone(), 0, f);

        let instruction1 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("true".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction1);

        let instruction2 = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", a)),
        );
        tac_ir.push(instruction2);

        self.emit_label(tac_ir.clone(), f);

        let instruction3 = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::CONST_BOOL("false".to_string()),
            TACOperand::NULL,
            result.to_owned(),
        );
        tac_ir.push(instruction3);

        self.emit_label(tac_ir.clone(), a);

        result
    }
}

#[derive(Debug, Clone)]
pub struct Access {
    tp: Token, //type
    _token: Token,
    temp_count: Rc<RefCell<u32>>,
    array: Id,
    index: ExprUnion,
}

impl Access {
    pub fn new(tp: Token, temp_count: Rc<RefCell<u32>>, array: Id, index: ExprUnion) -> Self {
        Access {
            tp,
            _token: Token::Id(String::from("[]")),
            temp_count,
            array,
            index,
        }
    }
}

impl ExprNode for Access {
    fn gen_tac(&self, tac_ir: TACState) -> (TACOperator, TACOperand, TACOperand) {
        let operand1 = self.array.reduce_tac(tac_ir.clone());
        let operand2 = self.index.reduce_tac(tac_ir.clone());
        let arg1 = TACOperand::ACCESS(Box::new(operand1), Box::new(operand2));
        (TACOperator::NONE, arg1, TACOperand::NULL)
    }

    fn reduce_tac(&self, tac_ir: TACState) -> TACOperand {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        let (op, arg1, arg2) = self.gen_tac(tac_ir.clone());
        let result = temp.reduce_tac(tac_ir.clone());
        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);
        result
    }
}

#[derive(Debug, Clone)]
pub struct If {
    label: Rc<RefCell<u32>>,
    expr: ExprUnion,
    stmt: StmtUnion,
}

impl If {
    pub fn new(
        label: Rc<RefCell<u32>>,
        expr: ExprUnion,
        stmt: StmtUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(_) = expr.get_type() {
            Ok(If { label, expr, stmt })
        } else {
            Err("wrong expression type")
        }
    }
}

impl StmtNode for If {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label = *l;
        drop(l);
        self.expr.jumping(tac_ir.clone(), 0, a);
        self.emit_label(tac_ir.clone(), new_label);
        self.stmt.gen_tac(tac_ir.clone(), new_label, a);
    }
}

#[derive(Debug, Clone)]
pub struct Else {
    label: Rc<RefCell<u32>>,
    expr: ExprUnion,
    stmt1: StmtUnion,
    stmt2: StmtUnion,
}

impl Else {
    pub fn new(
        label: Rc<RefCell<u32>>,
        expr: ExprUnion,
        stmt1: StmtUnion,
        stmt2: StmtUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(_) = expr.get_type() {
            Ok(Else {
                label,
                expr,
                stmt1,
                stmt2,
            })
        } else {
            Err("wrong expression type")
        }
    }
}

impl StmtNode for Else {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label1 = *l;
        *l += 1;
        let new_label2 = *l;
        drop(l);

        self.expr.jumping(tac_ir.clone(), 0, new_label2);
        self.emit_label(tac_ir.clone(), new_label1);
        self.stmt1.gen_tac(tac_ir.clone(), new_label1, a);
        let instruction = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", a)),
        );
        tac_ir.push(instruction);

        self.emit_label(tac_ir.clone(), new_label2);
        self.stmt2.gen_tac(tac_ir.clone(), new_label2, a);
    }
}

#[derive(Debug, Clone)]
pub struct While {
    label: Rc<RefCell<u32>>,
    after: Rc<RefCell<u32>>,
    expr: Option<ExprUnion>,
    stmt: Option<StmtUnion>,
}

impl While {
    pub fn new(label: Rc<RefCell<u32>>, after: Rc<RefCell<u32>>) -> Self {
        While {
            label,
            after,
            expr: None,
            stmt: None,
        }
    }
    pub fn init(
        &mut self,
        expr: Option<ExprUnion>,
        stmt: Option<StmtUnion>,
    ) -> Result<(), &'static str> {
        match expr {
            Some(x) => {
                if let Token::Bool(_) = x.get_type() {
                    self.expr = Some(x);
                    self.stmt = stmt;
                    Ok(())
                } else {
                    Err("wrong expression type")
                }
            }
            None => Err("expression missing"),
        }
    }
}

impl StmtNode for While {
    fn gen_tac(&self, tac_ir: TACState, b: u32, a: u32) {
        if let Some(e) = &self.expr {
            if let Some(s) = &self.stmt {
                let mut after_lock = self.after.borrow_mut();
                *after_lock = a;
                drop(after_lock);

                e.jumping(tac_ir.clone(), 0, a);

                let mut l = self.label.borrow_mut();
                *l += 1;
                let new_label = *l;
                drop(l);
                self.emit_label(tac_ir.clone(), new_label);
                s.gen_tac(tac_ir.clone(), new_label, b);
                let instruction = TACInstruction::new(
                    TACOperator::GOTO,
                    TACOperand::NULL,
                    TACOperand::NULL,
                    TACOperand::LABEL(format!("L{}", b)),
                );
                tac_ir.push(instruction);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Set {
    id: Id,
    expr: ExprUnion,
}

impl Set {
    pub fn new(id: Id, expr: ExprUnion) -> Result<Self, &'static str> {
        if id.tp == expr.get_type() {
            Ok(Set { id, expr })
        } else {
            Err("variable and value types do not match")
        }
    }
}

impl StmtNode for Set {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, _a: u32) {
        let result = self.id.reduce_tac(tac_ir.clone());
        let (op, arg1, arg2) = self.expr.gen_tac(tac_ir.clone());
        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct SetElem {
    array: Id,
    index: ExprUnion,
    expr: ExprUnion,
}

impl SetElem {
    pub fn new(x: Access, y: ExprUnion) -> Result<Self, &'static str> {
        if let Token::Arr(_) = x.tp {
            return Err("can not assign to instance of type array");
        }
        if let Token::Arr(_) = y.get_type() {
            return Err("can not assign a value of type array");
        }
        if x.tp == y.get_type() {
            Ok(SetElem {
                array: x.array,
                index: x.index,
                expr: y,
            })
        } else {
            Err("variable and value types do not match")
        }
    }
}

impl StmtNode for SetElem {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, _a: u32) {
        let operand1 = self.array.reduce_tac(tac_ir.clone());
        let operand2 = self.index.reduce_tac(tac_ir.clone());
        let result = TACOperand::ACCESS(Box::new(operand1), Box::new(operand2));
        let (op, arg1, arg2) = self.expr.gen_tac(tac_ir.clone());
        let instruction = TACInstruction::new(op, arg1, arg2, result.to_owned());
        tac_ir.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct Seq {
    label: Rc<RefCell<u32>>,
    stmt1: Option<StmtUnion>,
    stmt2: Option<StmtUnion>,
}

impl Seq {
    pub fn new(
        label: Rc<RefCell<u32>>,
        stmt1: Option<StmtUnion>,
        stmt2: Option<StmtUnion>,
    ) -> Self {
        Seq {
            label,
            stmt1,
            stmt2,
        }
    }
}

impl StmtNode for Seq {
    fn gen_tac(&self, tac_ir: TACState, b: u32, a: u32) {
        match &self.stmt1 {
            Some(s1) => match &self.stmt2 {
                Some(s2) => {
                    let mut l = self.label.borrow_mut();
                    *l += 1;
                    let new_label = *l;
                    drop(l);
                    s1.gen_tac(tac_ir.clone(), b, new_label);
                    self.emit_label(tac_ir.clone(), new_label);
                    s2.gen_tac(tac_ir.clone(), new_label, a);
                }
                None => {
                    s1.gen_tac(tac_ir.clone(), b, a);
                }
            },
            None => match &self.stmt2 {
                Some(s2) => {
                    s2.gen_tac(tac_ir.clone(), b, a);
                }
                None => (),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Break {
    stmt: StmtUnion,
}

impl Break {
    pub fn new(stmt: Option<StmtUnion>) -> Result<Self, &'static str> {
        match stmt {
            Some(s) => Ok(Break { stmt: s }),
            None => Err("unenclosed break"),
        }
    }
}

impl StmtNode for Break {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, _a: u32) {
        let after = self.stmt.after();
        let instruction = TACInstruction::new(
            TACOperator::GOTO,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("L{}", after)),
        );
        tac_ir.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    params: Vec<Id>,
    stmt: Option<StmtUnion>,
}

impl Function {
    pub fn new(name: String, params: Vec<Id>, stmt: Option<StmtUnion>) -> Self {
        Self { name, params, stmt }
    }
}

impl StmtNode for Function {
    fn gen_tac(&self, tac_ir: TACState, b: u32, a: u32) {
        let instruction = TACInstruction::new(
            TACOperator::NONE,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::LABEL(format!("{}:", self.name)),
        );
        tac_ir.push(instruction);

        self.params.iter().for_each(|id| {
            let result = id.reduce_tac(tac_ir.clone());
            let instruction = TACInstruction::new(
                TACOperator::POP,
                TACOperand::NULL,
                TACOperand::NULL,
                result.to_owned(),
            );
            tac_ir.push(instruction);
        });

        if let Some(s) = &self.stmt {
            s.gen_tac(tac_ir.clone(), b, a);
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    call: Call,
}

impl FunctionCall {
    pub fn new(call: Call) -> Result<Self, &'static str> {
        if let Token::Void(_) = call.id.tp {
            Ok(FunctionCall { call })
        } else {
            Err("Function called without assignment must be of type void")
        }
    }
}

impl StmtNode for FunctionCall {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, _a: u32) {
        self.call.params.iter().for_each(|e| {
            let arg1 = e.reduce_tac(tac_ir.clone());
            let instruction =
                TACInstruction::new(TACOperator::PUSH, arg1, TACOperand::NULL, TACOperand::NULL);
            tac_ir.push(instruction);
        });

        let result = self.call.id.reduce_tac(tac_ir.clone());
        let instruction = TACInstruction::new(
            TACOperator::CALL,
            result,
            TACOperand::NULL,
            TACOperand::NULL,
        );
        tac_ir.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    expr: Option<ExprUnion>,
}

impl Return {
    pub fn new(expr: Option<ExprUnion>) -> Self {
        Self { expr }
    }
}

impl StmtNode for Return {
    fn gen_tac(&self, tac_ir: TACState, _b: u32, _a: u32) {
        if let Some(e) = &self.expr {
            let arg1 = e.reduce_tac(tac_ir.clone());
            let instruction =
                TACInstruction::new(TACOperator::PUSH, arg1, TACOperand::NULL, TACOperand::NULL);
            tac_ir.push(instruction);
        }

        let instruction = TACInstruction::new(
            TACOperator::RET,
            TACOperand::NULL,
            TACOperand::NULL,
            TACOperand::NULL,
        );
        tac_ir.push(instruction);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Array;

    #[test]
    fn test_id() {
        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = id.gen_tac(tac_ir.clone());
        let reduced = id.reduce_tac(tac_ir.clone());
        assert_eq!(tac_ir.len(), 0);
        assert_eq!(reduced, TACOperand::VAR_INT(String::from("x")));
        assert_eq!(op, TACOperator::NONE);
        assert_eq!(arg1, TACOperand::VAR_INT(String::from("x")));
        assert_eq!(arg2, TACOperand::NULL);
    }

    #[test]
    fn test_arith() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let arith = Arith::new(
            Token::Add(String::from("+")),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = arith.gen_tac(tac_ir.clone());
        let reduced = arith.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 1);
        assert_eq!(reduced, TACOperand::TEMP_INT(String::from("t1")));
        assert_eq!(op, TACOperator::ADD);
        assert_eq!(arg1, TACOperand::CONST_INT(String::from("1")));
        assert_eq!(arg2, TACOperand::CONST_INT(String::from("2")));
        Ok(())
    }

    #[test]
    fn test_temp() {
        let temp = Temp::new(Token::Int(String::from("int")), Rc::new(RefCell::new(0)));

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = temp.gen_tac(tac_ir.clone());
        let reduced = temp.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 0);
        assert_eq!(reduced, TACOperand::TEMP_INT(String::from("t1")));
        assert_eq!(op, TACOperator::NONE);
        assert_eq!(arg1, TACOperand::TEMP_INT(String::from("t1")));
        assert_eq!(arg2, TACOperand::NULL);
    }

    #[test]
    fn test_unary() {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let unary = Unary::new(
            Token::Sub(String::from("-")),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
        );

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = unary.gen_tac(tac_ir.clone());
        let reduced = unary.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 1);
        assert_eq!(reduced, TACOperand::TEMP_INT(String::from("t1")));
        assert_eq!(op, TACOperator::SUB);
        assert_eq!(arg1, TACOperand::CONST_INT(String::from("1")));
        assert_eq!(arg2, TACOperand::NULL);
    }

    #[test]
    fn test_constant() {
        let constant = Constant::new(Token::Int(String::from("int")), Token::Num(1));

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = constant.gen_tac(tac_ir.clone());
        let reduced = constant.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 0);
        assert_eq!(reduced, TACOperand::CONST_INT(String::from("1")));
        assert_eq!(op, TACOperator::NONE);
        assert_eq!(arg1, TACOperand::CONST_INT(String::from("1")));
        assert_eq!(arg2, TACOperand::NULL);
    }

    #[test]
    fn test_or() -> Result<(), &'static str> {
        let x = Constant::new(
            Token::Bool(String::from("bool")),
            Token::True(String::from("true")),
        );
        let y = Constant::new(
            Token::Bool(String::from("bool")),
            Token::True(String::from("false")),
        );
        let or = Or::new(
            Token::Or(String::from("||")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = or.gen_tac(tac_ir.clone());
        let reduced = or.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 7);
        assert_eq!(reduced, TACOperand::TEMP_BOOL(String::from("t1")));
        assert_eq!(op, TACOperator::OR);
        assert_eq!(arg1, TACOperand::CONST_BOOL(String::from("true")));
        assert_eq!(arg2, TACOperand::CONST_BOOL(String::from("false")));

        Ok(())
    }

    #[test]
    fn test_and() -> Result<(), &'static str> {
        let x = Constant::new(
            Token::Bool(String::from("bool")),
            Token::True(String::from("true")),
        );
        let y = Constant::new(
            Token::Bool(String::from("bool")),
            Token::True(String::from("false")),
        );
        let and = And::new(
            Token::And(String::from("&&")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = and.gen_tac(tac_ir.clone());
        let reduced = and.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 5);
        assert_eq!(reduced, TACOperand::TEMP_BOOL(String::from("t1")));
        assert_eq!(op, TACOperator::AND);
        assert_eq!(arg1, TACOperand::CONST_BOOL(String::from("true")));
        assert_eq!(arg2, TACOperand::CONST_BOOL(String::from("false")));

        Ok(())
    }

    #[test]
    fn test_not() -> Result<(), &'static str> {
        let x = Constant::new(
            Token::Bool(String::from("bool")),
            Token::True(String::from("true")),
        );
        let not = Not::new(
            Token::Not(String::from("!")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
        )?;

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = not.gen_tac(tac_ir.clone());
        let reduced = not.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 12);
        assert_eq!(reduced, TACOperand::TEMP_BOOL(String::from("t2")));
        assert_eq!(op, TACOperator::NONE);
        assert_eq!(arg1, TACOperand::TEMP_BOOL(String::from("t1")));
        assert_eq!(arg2, TACOperand::NULL);

        Ok(())
    }

    #[test]
    fn test_rel() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let rel = Rel::new(
            Token::Le(String::from("<=")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = rel.gen_tac(tac_ir.clone());
        let reduced = rel.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 7);
        assert_eq!(reduced, TACOperand::TEMP_BOOL(String::from("t1")));
        assert_eq!(op, TACOperator::LE);
        assert_eq!(arg1, TACOperand::CONST_INT(String::from("1")));
        assert_eq!(arg2, TACOperand::CONST_INT(String::from("2")));

        Ok(())
    }

    #[test]
    fn test_access() {
        let size = 5;
        let of = Token::Int(String::from("int"));
        let w = 4;
        let tp = Token::Arr(Array::new(size, of, w));

        let array = Id::new(tp, Token::Id(String::from("x")), 0);
        let index = Constant::new(Token::Int(String::from("int")), Token::Num(0));
        let access = Access::new(
            Token::Int(String::from("int")),
            Rc::new(RefCell::new(0)),
            array,
            ExprUnion::Constant(Rc::from(index)),
        );

        let tac_ir = TACState::new();

        let (op, arg1, arg2) = access.gen_tac(tac_ir.clone());
        let reduced = access.reduce_tac(tac_ir.clone());

        assert_eq!(tac_ir.len(), 1);
        assert_eq!(reduced, TACOperand::TEMP_INT(String::from("t1")));
        assert_eq!(op, TACOperator::NONE);
        assert_eq!(
            arg1,
            TACOperand::ACCESS(
                Box::new(TACOperand::VAR_INT(String::from("x"))),
                Box::new(TACOperand::CONST_INT(String::from("0")))
            )
        );
        assert_eq!(arg2, TACOperand::NULL);
    }

    #[test]
    fn test_if() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let rel = Rel::new(
            Token::Le(String::from("<=")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let z = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let set = Set::new(id, ExprUnion::Constant(Rc::new(z)))?;

        let if_stmt = If::new(
            Rc::new(RefCell::new(0)),
            ExprUnion::Rel(Rc::new(rel)),
            StmtUnion::Set(Rc::new(set)),
        )?;

        let tac_ir = TACState::new();

        if_stmt.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 4);

        Ok(())
    }

    #[test]
    fn test_else() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let rel = Rel::new(
            Token::Le(String::from("<=")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let z1 = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let z2 = Constant::new(Token::Int(String::from("int")), Token::Num(4));
        let id1 = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let id2 = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("y")),
            0,
        );
        let set1 = Set::new(id1, ExprUnion::Constant(Rc::new(z1)))?;
        let set2 = Set::new(id2, ExprUnion::Constant(Rc::new(z2)))?;

        let else_stmt = Else::new(
            Rc::new(RefCell::new(0)),
            ExprUnion::Rel(Rc::new(rel)),
            StmtUnion::Set(Rc::new(set1)),
            StmtUnion::Set(Rc::new(set2)),
        )?;

        let tac_ir = TACState::new();

        else_stmt.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 7);

        Ok(())
    }

    #[test]
    fn test_while() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let rel = Rel::new(
            Token::Le(String::from("<=")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let z = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let set = Set::new(id, ExprUnion::Constant(Rc::new(z)))?;

        let mut while_stmt = While::new(Rc::new(RefCell::new(0)), Rc::new(RefCell::new(0)));
        while_stmt.init(
            Some(ExprUnion::Rel(Rc::new(rel))),
            Some(StmtUnion::Set(Rc::new(set))),
        )?;

        let tac_ir = TACState::new();

        while_stmt.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 5);

        Ok(())
    }

    #[test]
    fn test_set() -> Result<(), &'static str> {
        let z = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let set = Set::new(id, ExprUnion::Constant(Rc::new(z)))?;

        let tac_ir = TACState::new();

        set.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 1);

        Ok(())
    }

    #[test]
    fn test_set_elem() -> Result<(), &'static str> {
        let size = 5;
        let of = Token::Int(String::from("int"));
        let w = 4;
        let tp = Token::Arr(Array::new(size, of, w));

        let array = Id::new(tp, Token::Id(String::from("x")), 0);
        let index = Constant::new(Token::Int(String::from("int")), Token::Num(0));
        let x = Access::new(
            Token::Int(String::from("int")),
            Rc::new(RefCell::new(0)),
            array,
            ExprUnion::Constant(Rc::from(index)),
        );
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(3));

        let set_elem = SetElem::new(x, ExprUnion::Constant(Rc::new(y)))?;

        let tac_ir = TACState::new();

        set_elem.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 1);

        Ok(())
    }

    #[test]
    fn test_seq() -> Result<(), &'static str> {
        let z1 = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let z2 = Constant::new(Token::Int(String::from("int")), Token::Num(4));
        let id1 = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let id2 = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("y")),
            0,
        );
        let set1 = Set::new(id1, ExprUnion::Constant(Rc::new(z1)))?;
        let set2 = Set::new(id2, ExprUnion::Constant(Rc::new(z2)))?;

        let seq = Seq::new(
            Rc::new(RefCell::new(0)),
            Some(StmtUnion::Set(Rc::new(set1))),
            Some(StmtUnion::Set(Rc::new(set2))),
        );

        let tac_ir = TACState::new();

        seq.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 3);

        Ok(())
    }

    #[test]
    fn test_break() -> Result<(), &'static str> {
        let while_stmt = While::new(Rc::new(RefCell::new(0)), Rc::new(RefCell::new(0)));
        let break_stmt = Break::new(Some(StmtUnion::While(Rc::new(RefCell::new(while_stmt)))))?;

        let tac_ir = TACState::new();

        break_stmt.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 1);

        Ok(())
    }

    #[test]
    fn test_function() -> Result<(), &'static str> {
        let label = Rc::new(RefCell::new(1 as u32));
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let rel = Rel::new(
            Token::Le(String::from("<=")),
            Rc::new(RefCell::new(0)),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;

        let z = Constant::new(Token::Int(String::from("int")), Token::Num(3));
        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let set = Set::new(id.clone(), ExprUnion::Constant(Rc::new(z)))?;

        let mut while_stmt = While::new(Rc::clone(&label), Rc::new(RefCell::new(1)));
        while_stmt.init(
            Some(ExprUnion::Rel(Rc::new(rel))),
            Some(StmtUnion::Set(Rc::new(set))),
        )?;

        let function_stmt = Function::new(
            String::from("f"),
            vec![id],
            Some(StmtUnion::While(Rc::new(RefCell::new(while_stmt)))),
        );

        let tac_ir = TACState::new();

        function_stmt.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 7);

        Ok(())
    }

    #[test]
    fn test_function_call() -> Result<(), &'static str> {
        let temp_count = Rc::new(RefCell::new(0));
        // create calls both with and without FunctionCall wrapper
        let id_void = Id::new(
            Token::Void(String::from("void")),
            Token::Id(String::from("f")),
            0,
        );
        let id_int = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("f")),
            0,
        );

        let call_void = Call::new(id_void, vec![], Rc::clone(&temp_count));
        let call_int = Call::new(id_int, vec![], Rc::clone(&temp_count));

        let function_call_void = FunctionCall::new(call_void)?;

        let id = Id::new(
            Token::Int(String::from("int")),
            Token::Id(String::from("x")),
            0,
        );
        let set = Set::new(id, ExprUnion::Call(Rc::new(call_int)))?;

        let tac_ir = TACState::new();

        function_call_void.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 1);

        set.gen_tac(tac_ir.clone(), 1, 2);

        assert_eq!(tac_ir.len(), 2);

        Ok(())
    }
}
