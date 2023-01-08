use super::lexer::{Lexer, Token};
use std::cell::RefCell;
use std::rc::Rc;

//TODO:
//Expression-union jonka tyyppejä ovat Unary(Op), Arith(Op), Constant(Op),
//Or(Logical), And(Logical), Not(Logical), Rel(Logical).
//Statement-union jonka tyyppejä ovat If, Else, While, Set, Seq.

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

    //used by logical expressions
    fn gen(&self, f: u32, a: u32, temp_number: u32) -> u32 {
        self.jumping(0, f);
        self.emit(format!("t{} = true", temp_number));
        self.emit(format!("goto L{}", a));
        self.emit_label(f);
        self.emit(format!("t{} = false", temp_number));
        self.emit_label(a);
        return temp_number;
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

#[derive(Debug, Clone)]
enum ExprUnion {
    Arith(Box<Arith>),
    Unary(Box<Unary>),
    Constant(Box<Constant>),
    Or(Box<Or>),
    And(Box<And>),
    Not(Box<Not>),
    Rel(Box<Rel>),
}

impl ExprUnion {
    fn match_expr(&self) -> String {
        match self {
            ExprUnion::Arith(arith) => return arith.to_string(),
            ExprUnion::Unary(unary) => return unary.to_string(),
            ExprUnion::Constant(constant) => return constant.to_string(),
            ExprUnion::Or(or) => return or.to_string(),
            ExprUnion::And(and) => return and.to_string(),
            ExprUnion::Not(not) => return not.to_string(),
            ExprUnion::Rel(rel) => return rel.to_string(),
            _ => return "".to_string(),
        }
    }

    fn jumping(&self, t: u32, f: u32) {
        match self {
            ExprUnion::Arith(arith) => arith.jumping(t, f),
            ExprUnion::Unary(unary) => unary.jumping(t, f),
            ExprUnion::Constant(constant) => constant.jumping(t, f),
            ExprUnion::Or(or) => or.jumping(t, f),
            ExprUnion::And(and) => and.jumping(t, f),
            ExprUnion::Not(not) => not.jumping(t, f),
            ExprUnion::Rel(rel) => rel.jumping(t, f),
            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
enum StmtUnion {
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
                let after = while_stmt.after.clone().into_inner();
                return after;
            }
            other => return 0,
        }
    }
    fn gen(&self, b: u32, a: u32) {
        match self {
            StmtUnion::If(if_stmt) => if_stmt.gen(b, a),
            StmtUnion::Else(else_stmt) => else_stmt.gen(b, a),
            StmtUnion::While(while_stmt) => while_stmt.gen(b, a),
            StmtUnion::Set(set_stmt) => set_stmt.gen(b, a),
            StmtUnion::Seq(seq_stmt) => seq_stmt.gen(b, a),
            StmtUnion::Break(break_stmt) => break_stmt.gen(b, a),
            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
struct Arith {
    op: Token,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl ExprNode for Arith {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.to_string(), t, f);
    }
    //TODO: muuta match-muotoon tai keksi parempi ratkaisu
    fn to_string(&self) -> String {
        let e1 = self.expr1.match_expr();
        let e2 = self.expr2.match_expr();
        return format!("{} {} {}", e1, self.op.clone().value_to_string(), e2);
    }
}

#[derive(Debug, Clone)]
struct Unary {
    op: Token,
    expr: ExprUnion,
}

impl ExprNode for Unary {
    fn jumping(&self, t: u32, f: u32) {
        self.emit_jumps(self.to_string(), t, f);
    }

    fn to_string(&self) -> String {
        let e = self.expr.match_expr();
        return format!("{} {}", self.op.clone().value_to_string(), e);
    }
}

#[derive(Debug, Clone)]
struct Constant {
    constant: Token,
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
            _ => return, //TODO: palaa tähån kun funktioiden palautus on Result-tyyppiä
        }
    }

    fn to_string(&self) -> String {
        return self.constant.clone().value_to_string();
    }
}

#[derive(Debug, Clone)]
struct Or {
    label: Rc<RefCell<u32>>,
    op: Token,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl ExprNode for Or {
    fn jumping(&self, t: u32, f: u32) {
        let mut new_label = t;
        if t == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
        }
        self.expr1.jumping(new_label, 0);
        self.expr2.jumping(t, f);
        if t == 0 {
            self.emit_label(new_label);
        }
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.match_expr();
        let e2 = self.expr2.match_expr();
        return format!("{} {} {}", e1, self.op.clone().value_to_string(), e2);
    }
}

#[derive(Debug, Clone)]
struct And {
    label: Rc<RefCell<u32>>,
    op: Token,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl ExprNode for And {
    fn jumping(&self, t: u32, f: u32) {
        let mut new_label = f;
        if f == 0 {
            let mut l = self.label.borrow_mut();
            *l += 1;
            new_label = *l;
        }
        self.expr1.jumping(0, new_label);
        self.expr2.jumping(t, f);
        if f == 0 {
            self.emit_label(new_label);
        }
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.match_expr();
        let e2 = self.expr2.match_expr();
        return format!("{} {} {}", e1, self.op.clone().value_to_string(), e2);
    }
}

#[derive(Debug, Clone)]
struct Not {
    op: Token,
    expr: ExprUnion,
}

impl ExprNode for Not {
    fn jumping(&self, t: u32, f: u32) {
        self.expr.jumping(f, t);
    }

    fn to_string(&self) -> String {
        let e = self.expr.match_expr();
        return format!("{} {}", self.op.clone().value_to_string(), e);
    }
}

#[derive(Debug, Clone)]
struct Rel {
    op: Token,
    expr1: ExprUnion,
    expr2: ExprUnion,
}

impl ExprNode for Rel {
    fn jumping(&self, t: u32, f: u32) {
        let e1 = self.expr1.match_expr();
        let e2 = self.expr2.match_expr();
        let test = self.to_string();
        self.emit_jumps(test, t, f);
    }

    fn to_string(&self) -> String {
        let e1 = self.expr1.match_expr();
        let e2 = self.expr2.match_expr();
        return format!("{} {} {}", e1, self.op.clone().value_to_string(), e2);
    }
}

#[derive(Debug, Clone)]
struct If {
    label: Rc<RefCell<u32>>,
    expr: ExprUnion,
    stmt: StmtUnion,
}

impl StmtNode for If {
    fn gen(&self, b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label = *l;
        self.expr.jumping(0, a);
        self.emit_label(new_label);
        self.stmt.gen(new_label, a);
    }
}

#[derive(Debug, Clone)]
struct Else {
    label: Rc<RefCell<u32>>,
    expr: ExprUnion,
    stmt1: StmtUnion,
    stmt2: StmtUnion,
}

impl StmtNode for Else {
    fn gen(&self, b: u32, a: u32) {
        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label1 = *l;
        *l += 1;
        let new_label2 = *l;

        self.expr.jumping(0, new_label2);
        self.emit_label(new_label1);
        self.stmt1.gen(new_label1, a);
        self.emit(format!("goto L{}", a));

        self.emit_label(new_label2);
        self.stmt2.gen(new_label2, a);
    }
}

#[derive(Debug, Clone)]
struct While {
    label: Rc<RefCell<u32>>,
    after: RefCell<u32>,
    expr: ExprUnion,
    stmt: StmtUnion,
}

impl StmtNode for While {
    fn gen(&self, b: u32, a: u32) {
        let mut after_borrow = self.after.borrow_mut();
        *after_borrow = a;
        self.expr.jumping(0, a);

        let mut l = self.label.borrow_mut();
        *l += 1;
        let new_label = *l;
        self.emit_label(new_label);
        self.stmt.gen(new_label, b);
        self.emit(format!("goto L{}", b));
    }
}

#[derive(Debug, Clone)]
struct Set {
    id: Token,
    expr: ExprUnion,
}

impl StmtNode for Set {
    fn gen(&self, b: u32, a: u32) {
        let e = self.expr.match_expr();
        self.emit(format!("{} = {}", self.id.clone().value_to_string(), e));
    }
}

#[derive(Debug, Clone)]
struct Seq {
    label: Rc<RefCell<u32>>,
    stmt1: Option<StmtUnion>,
    stmt2: Option<StmtUnion>,
}

impl StmtNode for Seq {
    fn gen(&self, b: u32, a: u32) {
        match &self.stmt1 {
            Some(s1) => match &self.stmt2 {
                Some(s2) => {
                    let mut l = self.label.borrow_mut();
                    *l += 1;
                    let new_label = *l;
                    s1.gen(b, new_label);
                    self.emit_label(new_label);
                    s2.gen(new_label, a);
                }
                None => (),
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
struct Break {
    stmt: StmtUnion,
}

impl StmtNode for Break {
    fn gen(&self, b: u32, a: u32) {
        let after = self.stmt.after();
        self.emit(format!("goto L{}", after));
    }
}
