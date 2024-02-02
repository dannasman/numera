use super::lexer::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub trait ExprNode {
    fn emit_label(&self, i: u32) {
        println!("L{}:", i);
    }

    fn emit(&self, s: String) {
        println!("\t{}", s);
    }

    fn emit_jumps(&self, test: String, t: u32, f: u32) {
        if t != 0 && f != 0 {
            self.emit(format!("if {} goto L{}", test, t));
            self.emit(format!("goto L{}", f));
        } else if t != 0 {
            self.emit(format!("if {} goto L{}", test, t));
        } else if f != 0 {
            self.emit(format!("iffalse {} goto L{}", test, f));
        }
    }

    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.to_string(), t, f);
    }

    fn to_string(&self) -> String;
}

pub trait StmtNode {
    fn emit_label(&self, i: u32) {
        println!("L{}:", i);
    }

    fn emit(&self, s: String) {
        println!("\t{}", s);
    }

    fn gen(&self, b: u32, a: u32);
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
    fn gen_expr_string(&self) -> String {
        match self {
            ExprUnion::Id(id) => id.to_string(),
            ExprUnion::Arith(arith) => arith.gen().to_string(),
            ExprUnion::Temp(temp) => temp.to_string(),
            ExprUnion::Unary(unary) => unary.gen().to_string(),
            ExprUnion::Constant(constant) => constant.to_string(),
            ExprUnion::Or(or) => or.gen().to_string(),
            ExprUnion::And(and) => and.gen().to_string(),
            ExprUnion::Not(not) => not.gen().to_string(),
            ExprUnion::Rel(rel) => rel.gen().to_string(),
            ExprUnion::Access(acc) => acc.to_string(),
            ExprUnion::Call(call) => call.reduce().to_string(),
        }
    }

    fn gen_reduce_string(&self) -> String {
        match self {
            ExprUnion::Id(id) => id.to_string(),
            ExprUnion::Arith(arith) => arith.reduce().to_string(),
            ExprUnion::Temp(temp) => temp.to_string(),
            ExprUnion::Unary(unary) => unary.reduce().to_string(),
            ExprUnion::Constant(constant) => constant.to_string(),
            ExprUnion::Or(or) => or.gen().to_string(),
            ExprUnion::And(and) => and.gen().to_string(),
            ExprUnion::Not(not) => not.gen().to_string(),
            ExprUnion::Rel(rel) => rel.gen().to_string(),
            ExprUnion::Access(acc) => acc.reduce().to_string(),
            ExprUnion::Call(call) => call.reduce().to_string(),
        }
    }

    fn reduce(&self) -> ExprUnion {
        match self {
            ExprUnion::Id(id) => ExprUnion::Id(id.to_owned()),
            ExprUnion::Arith(arith) => ExprUnion::Temp(Rc::new(arith.reduce())),
            ExprUnion::Temp(temp) => ExprUnion::Temp(temp.to_owned()),
            ExprUnion::Unary(unary) => ExprUnion::Temp(Rc::new(unary.reduce())),
            ExprUnion::Constant(constant) => ExprUnion::Constant(constant.to_owned()),
            ExprUnion::Or(or) => ExprUnion::Temp(Rc::new(or.gen())),
            ExprUnion::And(and) => ExprUnion::Temp(Rc::new(and.gen())),
            ExprUnion::Not(not) => ExprUnion::Temp(Rc::new(not.gen())),
            ExprUnion::Rel(rel) => ExprUnion::Temp(Rc::new(rel.gen())),
            ExprUnion::Access(acc) => ExprUnion::Temp(Rc::new(acc.reduce())),
            ExprUnion::Call(call) => ExprUnion::Temp(Rc::new(call.reduce())),
        }
    }

    fn get_type(&self) -> Token {
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

    fn jumping(&self, t: u32, f: u32) {
        match self {
            ExprUnion::Id(id) => id.jumping(t, f),
            ExprUnion::Arith(arith) => arith.jumping(t, f),
            ExprUnion::Temp(temp) => temp.jumping(t, f),
            ExprUnion::Unary(unary) => unary.jumping(t, f),
            ExprUnion::Constant(constant) => constant.jumping(t, f),
            ExprUnion::Or(or) => or.jumping(t, f),
            ExprUnion::And(and) => and.jumping(t, f),
            ExprUnion::Not(not) => not.jumping(t, f),
            ExprUnion::Rel(rel) => rel.jumping(t, f),
            ExprUnion::Access(acc) => acc.jumping(t, f),
            ExprUnion::Call(call) => call.jumping(t, f),
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
    pub fn emit_label(&self, i: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.emit_label(i),
            StmtUnion::Else(else_stmt) => else_stmt.emit_label(i),
            StmtUnion::While(while_stmt) => while_stmt.borrow().emit_label(i),
            StmtUnion::Set(set_stmt) => set_stmt.emit_label(i),
            StmtUnion::SetElem(set_stmt) => set_stmt.emit_label(i),
            StmtUnion::Seq(seq_stmt) => seq_stmt.emit_label(i),
            StmtUnion::Break(break_stmt) => break_stmt.emit_label(i),
            StmtUnion::Function(function_stmt) => function_stmt.emit_label(i),
            StmtUnion::FunctionCall(call_stmt) => call_stmt.emit_label(i),
            StmtUnion::Return(return_stmt) => return_stmt.emit_label(i),
        }
    }
    pub fn gen(&self, b: u32, a: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.gen(b, a),
            StmtUnion::Else(else_stmt) => else_stmt.gen(b, a),
            StmtUnion::While(while_stmt) => while_stmt.borrow().gen(b, a),
            StmtUnion::Set(set_stmt) => set_stmt.gen(b, a),
            StmtUnion::SetElem(set_stmt) => set_stmt.gen(b, a),
            StmtUnion::Seq(seq_stmt) => seq_stmt.gen(b, a),
            StmtUnion::Break(break_stmt) => break_stmt.gen(b, a),
            StmtUnion::Function(function_stmt) => function_stmt.gen(b, a),
            StmtUnion::FunctionCall(call_stmt) => call_stmt.gen(b, a),
            StmtUnion::Return(return_stmt) => return_stmt.gen(b, a),
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
    fn to_string(&self) -> String {
        self.token.clone().value_to_string()
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

    fn reduce(&self) -> Temp {
        self.params.iter().for_each(|e| {
            let reduced = e.reduce();
            let w = reduced.get_type().get_width().unwrap_or_else(|_| {
                panic!("failed to read width of {}", reduced.gen_expr_string())
            });
            self.emit(format!("push {} {}(%sp)", reduced.gen_expr_string(), w));
        });
        let temp = Temp::new(self.id.tp.clone(), Rc::clone(&self.temp_count));
        self.emit(format!(
            "{} = call {}",
            temp.to_string(),
            self.id.to_string()
        ));
        temp
    }
}

impl ExprNode for Call {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.reduce().to_string(), t, f);
    }
    fn to_string(&self) -> String {
        self.reduce().to_string()
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
    pub fn gen(&self) -> Self {
        let mut e1 = self.expr1.clone();
        let mut e2 = self.expr2.clone();

        match e1 {
            ExprUnion::Arith(arith) => {
                e1 = ExprUnion::Temp(Rc::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e1 = ExprUnion::Temp(Rc::new(unary.reduce()));
            }
            ExprUnion::Access(acc) => {
                e1 = ExprUnion::Temp(Rc::new(acc.reduce()));
            }
            _ => (),
        }

        match e2 {
            ExprUnion::Arith(arith) => {
                e2 = ExprUnion::Temp(Rc::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e2 = ExprUnion::Temp(Rc::new(unary.reduce()));
            }
            ExprUnion::Access(acc) => {
                e2 = ExprUnion::Temp(Rc::new(acc.reduce()));
            }
            _ => (),
        }

        Arith {
            tp: self.tp.clone(),
            op: self.op.clone(),
            temp_count: Rc::clone(&self.temp_count),
            expr1: e1,
            expr2: e2,
        }
    }

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

    fn reduce(&self) -> Temp {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.emit(format!("{} = {}", temp.to_string(), self.gen().to_string()));
        temp
    }
}

impl ExprNode for Arith {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.to_string(), t, f);
    }
    fn to_string(&self) -> String {
        let e1 = self.expr1.gen_expr_string();
        let e2 = self.expr2.gen_expr_string();
        format!("{} {} {}", e1, self.op.clone().value_to_string(), e2)
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
    fn to_string(&self) -> String {
        format!("t{}", self.number)
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
    pub fn gen(&self) -> Self {
        let mut e = self.expr.clone();
        match e {
            ExprUnion::Arith(arith) => {
                e = ExprUnion::Temp(Rc::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e = ExprUnion::Temp(Rc::new(unary.reduce()));
            }
            ExprUnion::Access(acc) => {
                e = ExprUnion::Temp(Rc::new(acc.reduce()));
            }

            _ => (),
        }
        Unary::new(self.op.clone(), Rc::clone(&self.temp_count), e)
    }
    pub fn new(op: Token, temp_count: Rc<RefCell<u32>>, expr: ExprUnion) -> Self {
        Unary {
            tp: expr.get_type(),
            op,
            temp_count,
            expr,
        }
    }

    fn reduce(&self) -> Temp {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.emit(format!("{} = {}", temp.to_string(), self.gen().to_string()));
        temp
    }
}

impl ExprNode for Unary {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.to_string(), t, f);
    }

    fn to_string(&self) -> String {
        let e = self.expr.gen_expr_string();
        format!("{} {}", self.op.clone().value_to_string(), e)
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
    fn jumping(&self, t: u32, f: u32) {
        match self.constant {
            Token::True(_) => {
                if t != 0 {
                    self.emit(format!("goto L{}", t));
                }
            }
            Token::False(_) => {
                if f != 0 {
                    self.emit(format!("goto L{}", f));
                }
            }
            _ => (),
        }
    }

    fn to_string(&self) -> String {
        self.constant.clone().value_to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Or {
    tp: Token, //type
    op: Token,
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl Or {
    //used by logical expressions
    fn gen(&self) -> Temp {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
    pub fn new(
        op: Token,
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s1) = expr1.get_type() {
            if let Token::Bool(_) = expr2.get_type() {
                return Ok(Or {
                    tp: Token::Bool(s1),
                    op,
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
    fn jumping(&self, t: u32, f: u32) {
        let mut new_label = t;
        if t == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
            drop(l);
        }
        self.expr1.jumping(new_label, 0);
        self.expr2.jumping(t, f);
        if t == 0 {
            self.emit_label(new_label);
        }
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.gen_expr_string();
        let e2 = self.expr2.gen_expr_string();
        format!("{} {} {}", e1, self.op.clone().value_to_string(), e2)
    }
}

#[derive(Debug, Clone)]
pub struct And {
    tp: Token, //type
    op: Token,
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl And {
    //used by logical expressions
    fn gen(&self) -> Temp {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
    pub fn new(
        op: Token,
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s1) = expr1.get_type() {
            if let Token::Bool(_) = expr2.get_type() {
                return Ok(And {
                    tp: Token::Bool(s1),
                    op,
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
    fn jumping(&self, t: u32, f: u32) {
        let mut new_label = f;
        if f == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
            drop(l);
        }
        self.expr1.jumping(0, new_label);
        self.expr2.jumping(t, f);
        if f == 0 {
            self.emit_label(new_label);
        }
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.gen_expr_string();
        let e2 = self.expr2.gen_expr_string();
        format!("{} {} {}", e1, self.op.clone().value_to_string(), e2)
    }
}

#[derive(Debug, Clone)]
pub struct Not {
    tp: Token,
    op: Token,
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    expr: ExprUnion,
}

impl Not {
    fn gen(&self) -> Temp {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
    pub fn new(
        op: Token,
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        expr: ExprUnion,
    ) -> Result<Self, &'static str> {
        if let Token::Bool(s) = expr.get_type() {
            return Ok(Not {
                tp: Token::Bool(s),
                op,
                label,
                temp_count,
                expr,
            });
        }
        Err("wrong expression type")
    }
}

impl ExprNode for Not {
    fn jumping(&self, t: u32, f: u32) {
        self.expr.jumping(f, t);
    }

    fn to_string(&self) -> String {
        let e = self.expr.gen_expr_string();
        format!("{} {}", self.op.clone().value_to_string(), e)
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
    fn gen(&self) -> Temp {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let f = *l;
        *l += 1;
        let a = *l;
        drop(l);

        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
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
    fn jumping(&self, t: u32, f: u32) {
        let test = format!(
            "{} {} {}",
            self.expr1.gen_reduce_string(),
            self.op.clone().value_to_string(),
            self.expr2.gen_reduce_string(),
        );
        self.emit_jumps(test, t, f);
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.gen_expr_string();
        let e2 = self.expr2.gen_expr_string();
        format!("{} {} {}", e1, self.op.clone().value_to_string(), e2)
    }
}

#[derive(Debug, Clone)]
pub struct Access {
    tp: Token, //type
    token: Token,
    temp_count: Rc<RefCell<u32>>,
    array: Id,
    index: ExprUnion,
}

impl Access {
    pub fn gen(&self) -> Self {
        match &self.index {
            ExprUnion::Arith(arith) => Access {
                tp: self.tp.to_owned(),
                token: self.token.to_owned(),
                temp_count: Rc::clone(&self.temp_count),
                array: self.array.to_owned(),
                index: ExprUnion::Temp(Rc::new(arith.reduce())),
            },
            ExprUnion::Unary(unary) => Access {
                tp: self.tp.to_owned(),
                token: self.token.to_owned(),
                temp_count: Rc::clone(&self.temp_count),
                array: self.array.to_owned(),
                index: ExprUnion::Temp(Rc::new(unary.reduce())),
            },
            _ => Access {
                tp: self.tp.to_owned(),
                token: self.token.to_owned(),
                temp_count: Rc::clone(&self.temp_count),
                array: self.array.to_owned(),
                index: self.index.to_owned(),
            },
        }
    }
    pub fn new(tp: Token, temp_count: Rc<RefCell<u32>>, array: Id, index: ExprUnion) -> Self {
        Access {
            tp,
            token: Token::Id(String::from("[]")),
            temp_count,
            array,
            index,
        }
    }
    pub fn reduce(&self) -> Temp {
        let temp = Temp::new(self.tp.clone(), Rc::clone(&self.temp_count));
        self.emit(format!("{} = {}", temp.to_string(), self.gen().to_string()));
        temp
    }
}

impl ExprNode for Access {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.reduce().to_string(), t, f)
    }

    fn to_string(&self) -> String {
        format!(
            "{} [ {} ]",
            self.array.to_string(),
            self.index.gen_expr_string()
        )
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
    fn gen(&self, _b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label = *l;
        drop(l);
        self.expr.jumping(0, a);
        self.emit_label(new_label);
        self.stmt.gen(new_label, a);
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
    fn gen(&self, _b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label1 = *l;
        *l += 1;
        let new_label2 = *l;
        drop(l);

        self.expr.jumping(0, new_label2);
        self.emit_label(new_label1);
        self.stmt1.gen(new_label1, a);
        self.emit(format!("goto L{}", a));

        self.emit_label(new_label2);
        self.stmt2.gen(new_label2, a);
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
    fn gen(&self, b: u32, a: u32) {
        if let Some(e) = &self.expr {
            if let Some(s) = &self.stmt {
                let mut after_lock = self.after.borrow_mut();
                *after_lock = a;
                drop(after_lock);

                e.jumping(0, a);

                let mut l = self.label.borrow_mut();
                *l += 1;
                let new_label = *l;
                drop(l);
                self.emit_label(new_label);
                s.gen(new_label, b);
                self.emit(format!("goto L{}", b));
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
    fn gen(&self, _b: u32, _a: u32) {
        let e = self.expr.gen_expr_string();
        self.emit(format!("{} = {}", self.id.to_string(), e));
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
    fn gen(&self, _b: u32, _a: u32) {
        self.emit(format!(
            "{} [ {} ] = {}",
            self.array.to_string(),
            self.index.gen_reduce_string(),
            self.expr.gen_reduce_string()
        ))
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
    fn gen(&self, b: u32, a: u32) {
        match &self.stmt1 {
            Some(s1) => match &self.stmt2 {
                Some(s2) => {
                    let mut l = self.label.borrow_mut();
                    *l += 1;
                    let new_label = *l;
                    drop(l);
                    s1.gen(b, new_label);
                    self.emit_label(new_label);
                    s2.gen(new_label, a);
                }
                None => {
                    s1.gen(b, a);
                }
            },
            None => match &self.stmt2 {
                Some(s2) => {
                    s2.gen(b, a);
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
    fn gen(&self, _b: u32, _a: u32) {
        let after = self.stmt.after();
        self.emit(format!("goto L{}", after));
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    label: Rc<RefCell<u32>>,
    name: String,
    params: Vec<Id>,
    stmt: Option<StmtUnion>,
}

impl Function {
    pub fn new(
        label: Rc<RefCell<u32>>,
        name: String,
        params: Vec<Id>,
        stmt: Option<StmtUnion>,
    ) -> Self {
        Self {
            label,
            name,
            params,
            stmt,
        }
    }
}

impl StmtNode for Function {
    fn gen(&self, _b: u32, _a: u32) {
        println!("{}:", self.name);

        self.params.iter().for_each(|id| {
            let w = id
                .tp
                .get_width()
                .unwrap_or_else(|_| panic!("failed to read width of {}", id.to_string()));
            self.emit(format!("pop {} {}(%sp)", id.to_string(), w));
        });

        let mut l = self.label.borrow_mut();
        *l += 1;
        let begin = *l;
        *l += 1;
        let after = *l;
        drop(l);

        self.emit_label(begin);
        if let Some(s) = &self.stmt {
            s.gen(begin, after)
        }
        self.emit_label(after);

        //self.emit(String::from("ret"));
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
    fn gen(&self, _b: u32, _a: u32) {
        self.call.params.iter().for_each(|e| {
            let reduced = e.reduce();
            let w = reduced.get_type().get_width().unwrap_or_else(|_| {
                panic!("failed to read width of {}", reduced.gen_expr_string())
            });
            self.emit(format!("push {} {}(%sp)", reduced.gen_expr_string(), w));
        });
        self.emit(format!("call {}", self.call.id.to_string()));
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
    fn gen(&self, _b: u32, _a: u32) {
        if let Some(e) = &self.expr {
            let reduced = e.reduce();
            let w = reduced.get_type().get_width().unwrap_or_else(|_| {
                panic!("failed to read width of {}", reduced.gen_expr_string())
            });
            self.emit(format!("push {} {}(%sp)", reduced.gen_expr_string(), w));
        }
        self.emit(String::from("ret"));
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
        assert_eq!(id.to_string(), "x");
    }

    #[test]
    fn test_arith() -> Result<(), &'static str> {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(2));
        let artih = Arith::new(
            Token::Add(String::from("+")),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
            ExprUnion::Constant(Rc::new(y)),
        )?;
        assert_eq!(artih.to_string(), "1 + 2");
        assert_eq!(artih.reduce().to_string(), "t1"); // currently reduce() also prints t1 = 1 + 2;
        Ok(())
    }

    #[test]
    fn test_temp() {
        let temp = Temp::new(Token::Id(String::from("t1")), Rc::new(RefCell::new(0)));
        assert_eq!(temp.to_string(), "t1");
    }

    #[test]
    fn test_unary() {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        let unary = Unary::new(
            Token::Sub(String::from("-")),
            Rc::new(RefCell::new(0)),
            ExprUnion::Constant(Rc::new(x)),
        );

        assert_eq!(unary.to_string(), "- 1");
        assert_eq!(unary.reduce().to_string(), "t1");
    }

    #[test]
    fn test_constant() {
        let x = Constant::new(Token::Int(String::from("int")), Token::Num(1));
        assert_eq!(x.to_string(), "1");
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

        assert_eq!(or.to_string(), "true || false");
        assert_eq!(or.gen().to_string(), "t1");
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

        assert_eq!(and.to_string(), "true && false");
        assert_eq!(and.gen().to_string(), "t1");
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

        assert_eq!(not.to_string(), "! true");
        assert_eq!(not.gen().to_string(), "t1");
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

        assert_eq!(rel.to_string(), "1 <= 2");
        assert_eq!(rel.gen().to_string(), "t1");
        Ok(())
    }

    #[test]
    fn test_access() {
        let size = 5;
        let of = Token::Int(String::from("int"));
        let w = 4;
        let tp = Token::Arr(Array::new(size, of, w));

        let array = Id::new(tp, Token::Id(String::from("x")), 0);
        let index = Constant::new(Token::Int(String::from("bool")), Token::Num(0));
        let access = Access::new(
            Token::Int(String::from("int")),
            Rc::new(RefCell::new(0)),
            array,
            ExprUnion::Constant(Rc::from(index)),
        );

        assert_eq!(access.to_string(), "x [ 0 ]");
        assert_eq!(access.gen().to_string(), "x [ 0 ]");
        assert_eq!(access.reduce().to_string(), "t1");
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

        let _if_stmt = If::new(
            Rc::new(RefCell::new(0)),
            ExprUnion::Rel(Rc::new(rel)),
            StmtUnion::Set(Rc::new(set)),
        )?;
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

        let _else_stmt = Else::new(
            Rc::new(RefCell::new(0)),
            ExprUnion::Rel(Rc::new(rel)),
            StmtUnion::Set(Rc::new(set1)),
            StmtUnion::Set(Rc::new(set2)),
        )?;
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
        let _set = Set::new(id, ExprUnion::Constant(Rc::new(z)))?;
        Ok(())
    }

    #[test]
    fn test_set_elem() -> Result<(), &'static str> {
        let size = 5;
        let of = Token::Int(String::from("int"));
        let w = 4;
        let tp = Token::Arr(Array::new(size, of, w));

        let array = Id::new(tp, Token::Id(String::from("x")), 0);
        let index = Constant::new(Token::Int(String::from("bool")), Token::Num(0));
        let x = Access::new(
            Token::Int(String::from("int")),
            Rc::new(RefCell::new(0)),
            array,
            ExprUnion::Constant(Rc::from(index)),
        );
        let y = Constant::new(Token::Int(String::from("int")), Token::Num(3));

        let _set_elem = SetElem::new(x, ExprUnion::Constant(Rc::new(y)))?;

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

        let _seq = Seq::new(
            Rc::new(RefCell::new(0)),
            Some(StmtUnion::Set(Rc::new(set1))),
            Some(StmtUnion::Set(Rc::new(set2))),
        );

        Ok(())
    }

    #[test]
    fn test_break() -> Result<(), &'static str> {
        let while_stmt = While::new(Rc::new(RefCell::new(0)), Rc::new(RefCell::new(0)));
        let _break_stmt = Break::new(Some(StmtUnion::While(Rc::new(RefCell::new(while_stmt)))))?;
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
            Rc::clone(&label),
            String::from("f"),
            vec![id],
            Some(StmtUnion::While(Rc::new(RefCell::new(while_stmt)))),
        );

        let mut l = label.borrow_mut();
        *l += 1;
        let begin = *l;
        *l += 1;
        let after = *l;
        drop(l);

        function_stmt.emit_label(begin);
        function_stmt.gen(begin, after);
        function_stmt.emit_label(after);

        Ok(())
    }
}
