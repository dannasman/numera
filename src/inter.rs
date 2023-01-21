use super::lexer::Token;
use std::rc::Rc;
use std::cell::RefCell;

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
    Id(Box<Id>),
    Arith(Box<Arith>),
    Temp(Box<Temp>),
    Unary(Box<Unary>),
    Constant(Box<Constant>),
    Or(Box<Or>),
    And(Box<And>),
    Not(Box<Not>),
    Rel(Box<Rel>),
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtUnion {
    If(Box<If>),
    Else(Box<Else>),
    While(Box<While>),
    Set(Box<Set>),
    Seq(Box<Seq>),
    Break(Box<Break>),
}

impl StmtUnion {
    fn after(&self) -> u32 {
        match self {
            StmtUnion::While(while_stmt) => {
                let after_borrow= while_stmt.after.borrow_mut();
                let after = *after_borrow;
                drop(after_borrow);
                after
            }
            _ => 0,
        }
    }
    pub fn emit_label(&self, i: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.emit_label(i),
            StmtUnion::Else(else_stmt) => else_stmt.emit_label(i),
            StmtUnion::While(while_stmt) => while_stmt.emit_label(i),
            StmtUnion::Set(set_stmt) => set_stmt.emit_label(i),
            StmtUnion::Seq(seq_stmt) => seq_stmt.emit_label(i),
            StmtUnion::Break(break_stmt) => break_stmt.emit_label(i),
        }
    }
    pub fn gen(&self, b: u32, a: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.gen(b, a),
            StmtUnion::Else(else_stmt) => else_stmt.gen(b, a),
            StmtUnion::While(while_stmt) => while_stmt.gen(b, a),
            StmtUnion::Set(set_stmt) => set_stmt.gen(b, a),
            StmtUnion::Seq(seq_stmt) => seq_stmt.gen(b, a),
            StmtUnion::Break(break_stmt) => break_stmt.gen(b, a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Id {
    token: Token,
}

impl Id {
    pub fn new(token: Token) -> Self {
        Id { token }
    }
}

impl ExprNode for Id {
    fn to_string(&self) -> String {
        self.token.clone().value_to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Arith {
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
                e1 = ExprUnion::Temp(Box::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e1 = ExprUnion::Temp(Box::new(unary.reduce()));
            }
            _ => (),
        }

        match e2 {
            ExprUnion::Arith(arith) => {
                e2 = ExprUnion::Temp(Box::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e2 = ExprUnion::Temp(Box::new(unary.reduce()));
            }
            _ => (),
        }

        Arith::new(self.op.clone(), Rc::clone(&self.temp_count), e1, e2)
    }

    pub fn new(op: Token, temp_count: Rc<RefCell<u32>>, expr1: ExprUnion, expr2: ExprUnion) -> Self {
        Arith {
            op,
            temp_count,
            expr1,
            expr2,
        }
    }

    fn reduce(&self) -> Temp {
        let temp = Temp::new(Rc::clone(&self.temp_count));
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
    number: u32,
}

impl Temp {
    pub fn new(temp_count: Rc<RefCell<u32>>) -> Self {
        let mut c = temp_count.borrow_mut();
        *c += 1;
        let number = *c;
        drop(c);
        Temp { number }
    }
}

impl ExprNode for Temp {
    fn to_string(&self) -> String {
        format!("t{}", self.number)
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    op: Token,
    temp_count: Rc<RefCell<u32>>,
    expr: ExprUnion,
}

impl Unary {
    pub fn gen(&self) -> Self {
        let mut e = self.expr.clone();
        match e {
            ExprUnion::Arith(arith) => {
                e = ExprUnion::Temp(Box::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e = ExprUnion::Temp(Box::new(unary.reduce()));
            }
            _ => (),
        }
        Unary::new(self.op.clone(), Rc::clone(&self.temp_count), e)
    }
    pub fn new(op: Token, temp_count: Rc<RefCell<u32>>, expr: ExprUnion) -> Self {
        Unary {
            op,
            temp_count,
            expr,
        }
    }

    fn reduce(&self) -> Temp {
        let temp = Temp::new(Rc::clone(&self.temp_count));
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
    constant: Token,
}

impl Constant {
    pub fn new(constant: Token) -> Self {
        Constant { constant }
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
            _ => (), //TODO: palaa tähån kun funktioiden palautus on Result-tyyppiä
        }
    }

    fn to_string(&self) -> String {
        self.constant.clone().value_to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Or {
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    op: Token,
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

        let temp = Temp::new(Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
    pub fn new(
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        op: Token,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Self {
        Or {
            label,
            temp_count,
            op,
            expr1,
            expr2,
        }
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
    label: Rc<RefCell<u32>>,
    temp_count: Rc<RefCell<u32>>,
    op: Token,
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

        let temp = Temp::new(Rc::clone(&self.temp_count));
        self.jumping(0, f);
        self.emit(format!("{} = true", temp.to_string()));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("{} = false", temp.to_string()));
        self.emit_label(a);
        temp
    }
    pub fn new(
        label: Rc<RefCell<u32>>,
        temp_count: Rc<RefCell<u32>>,
        op: Token,
        expr1: ExprUnion,
        expr2: ExprUnion,
    ) -> Self {
        And {
            label,
            temp_count,
            op,
            expr1,
            expr2,
        }
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

        let temp = Temp::new(Rc::clone(&self.temp_count));
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
    ) -> Self {
        Not {
            op,
            label,
            temp_count,
            expr,
        }
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

        let temp = Temp::new(Rc::clone(&self.temp_count));
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
    ) -> Self {
        Rel {
            op,
            label,
            temp_count,
            expr1,
            expr2,
        }
    }
}

impl ExprNode for Rel {
    fn jumping(&self, t: u32, f: u32) {
        let mut e1 = self.expr1.clone();
        let mut e2 = self.expr2.clone();

        match e1 {
            ExprUnion::Arith(arith) => {
                e1 = ExprUnion::Temp(Box::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e1 = ExprUnion::Temp(Box::new(unary.reduce()));
            }
            _ => (),
        }

        match e2 {
            ExprUnion::Arith(arith) => {
                e2 = ExprUnion::Temp(Box::new(arith.reduce()));
            }
            ExprUnion::Unary(unary) => {
                e2 = ExprUnion::Temp(Box::new(unary.reduce()));
            }
            _ => (),
        }

        let test = format!(
            "{} {} {}",
            e1.gen_expr_string(),
            self.op.clone().value_to_string(),
            e2.gen_expr_string()
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
pub struct If {
    label: Rc<RefCell<u32>>,
    expr: ExprUnion,
    stmt: StmtUnion,
}

impl If {
    pub fn new(label: Rc<RefCell<u32>>, expr: ExprUnion, stmt: StmtUnion) -> Self {
        If { label, expr, stmt }
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
    ) -> Self {
        Else {
            label,
            expr,
            stmt1,
            stmt2,
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
    pub fn init(&mut self, expr: Option<ExprUnion>, stmt: Option<StmtUnion>) {
        self.expr = expr;
        self.stmt = stmt;
    }
}

impl StmtNode for While {
    fn gen(&self, b: u32, a: u32) {
        match &self.expr {
            Some(e) => match &self.stmt {
                Some(s) => {
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
                None => {
                    println!("expression missing");
                }
            },
            None => {
                println!("expression missing");
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
    pub fn new(id: Id, expr: ExprUnion) -> Self {
        Set { id, expr }
    }
}

impl StmtNode for Set {
    fn gen(&self, _b: u32, _a: u32) {
        let e = self.expr.gen_expr_string();
        self.emit(format!("{} = {}", self.id.to_string(), e));
    }
}

#[derive(Debug, Clone)]
pub struct Seq {
    label: Rc<RefCell<u32>>,
    stmt1: Option<StmtUnion>,
    stmt2: Option<StmtUnion>,
}

impl Seq {
    pub fn new(label: Rc<RefCell<u32>>, stmt1: Option<StmtUnion>, stmt2: Option<StmtUnion>) -> Self {
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
    stmt: Option<StmtUnion>,
}

impl Break {
    pub fn new(stmt: Option<StmtUnion>) -> Self {
        Break { stmt }
    }
}

impl StmtNode for Break {
    fn gen(&self, _b: u32, _a: u32) {
        match &self.stmt {
            Some(s) => {
                let after = s.after();
                self.emit(format!("goto L{}", after));
            }
            None => {
                println!("unenclosed break");
            }
        }
    }
}
