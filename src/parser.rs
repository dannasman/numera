use super::inter::{
    And, Arith, Break, Constant, Else, ExprUnion, Id, If, Not, Or, Rel, Seq, Set, StmtUnion, Unary,
    While,
};
use super::lexer::{Lexer, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Parser {
    enclosing_stmt: Rc<RefCell<Option<StmtUnion>>>,
    label: Rc<RefCell<u32>>,
    lexer: Lexer,
    symbol_table: HashMap<String, Id>,
    temp_count: Rc<RefCell<u32>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            enclosing_stmt: Rc::new(RefCell::new(None)),
            label: Rc::new(RefCell::new(0)),
            lexer,
            symbol_table: HashMap::new(),
            temp_count: Rc::new(RefCell::new(0)),
        }
    }

    pub fn get_line(&mut self) -> u32 {
        if let Some(line) = self.lexer.lines.pop_front() {
            line
        } else {
            0
        }
    }

    pub fn program(&mut self, input: &str) {
        self.lexer.lex(input);

        let stmt = self.block();
        if let Some(s) = stmt {
            let mut l = self.label.borrow_mut();
            *l += 1;
            let begin = *l;
            *l += 1;
            let after = *l;
            drop(l);

            s.emit_label(begin);
            s.gen(begin, after);
            s.emit_label(after);
        }
    }

    fn block(&mut self) -> Option<StmtUnion> {
        let mut t = self.lexer.tokens.pop_front();
        let mut line = self.get_line();
        if let Some(Token::Lcb(_)) = t {
        } else {
            panic!("Error at line {}: Token did not match {{", line);
        }

        //save symbol table related to current scope
        let saved_symbol_table = self.symbol_table.clone();

        let stmt = self.stmts();
        t = self.lexer.tokens.pop_front();
        line = self.get_line();
        if let Some(Token::Rcb(_)) = t {
        } else {
            panic!("Error at line {}: token did not match }}", line);
        }

        //set saved symbol table
        self.symbol_table = saved_symbol_table;
        stmt
    }

    fn stmts(&mut self) -> Option<StmtUnion> {
        let t = self.lexer.tokens.front();
        match t {
            Some(token) => match token {
                Token::Rcb(_) => None,
                _ => {
                    let stmt1 = self.stmt();
                    let stmt2 = self.stmts();
                    let seq =
                        StmtUnion::Seq(Box::new(Seq::new(Rc::clone(&self.label), stmt1, stmt2)));
                    Some(seq)
                }
            },
            None => None,
        }
    }

    fn stmt(&mut self) -> Option<StmtUnion> {
        let t = self.lexer.tokens.front();
        match t {
            Some(token) => match token {
                Token::Scol(_) => {
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    None
                }
                Token::Lcb(_) => self.block(),
                Token::Id(s) => {
                    if s == "break" {
                        self.lexer.tokens.pop_front();
                        let line = self.get_line();

                        let next_t = self.lexer.tokens.pop_front();
                        self.get_line();
                        if let Some(Token::Scol(_)) = next_t {
                            let enclosing = self.enclosing_stmt.borrow().to_owned();
                            match Break::new(enclosing) {
                                Ok(new_break) => {
                                    let break_stmt = StmtUnion::Break(Box::new(new_break));
                                    Some(break_stmt)
                                }
                                Err(s) => {
                                    panic!("Error at line {}: {}", line, s);
                                }
                            }
                        } else {
                            panic!("Error at line {}: token did not match ;", line);
                        }
                    } else {
                        self.assign()
                    }
                }
                Token::If(_) => {
                    self.lexer.tokens.pop_front();
                    let line = self.get_line();
                    let mut next_t = self.lexer.tokens.pop_front();
                    self.get_line();
                    if let Some(Token::Lrb(_)) = next_t {
                    } else {
                        panic!("Error at line {}: token did not match (", line);
                    }

                    let expr = self.boolean();

                    next_t = self.lexer.tokens.pop_front();
                    self.get_line();
                    if let Some(Token::Rrb(_)) = next_t {
                    } else {
                        panic!("Error at line {}: token did not match )", line);
                    }

                    let stmt1 = self.stmt();

                    let peek = self.lexer.tokens.front();
                    match peek {
                        Some(next) => {
                            if let Token::Else(_) = next {
                                self.lexer.tokens.pop_front();
                                self.get_line();
                                let stmt2 = self.stmt();
                                match stmt1 {
                                    Some(s1) => match stmt2 {
                                        Some(s2) => match expr {
                                            Some(x) => {
                                                let else_stmt = StmtUnion::Else(Box::new(
                                                    Else::new(Rc::clone(&self.label), x, s1, s2),
                                                ));
                                                Some(else_stmt)
                                            }
                                            None => {
                                                panic!(
                                                    "Error at line {}: missing expression",
                                                    line
                                                );
                                            }
                                        },
                                        None => None,
                                    },
                                    None => None,
                                }
                            } else {
                                match stmt1 {
                                    Some(s1) => match expr {
                                        Some(x) => {
                                            let if_stmt = StmtUnion::If(Box::new(If::new(
                                                Rc::clone(&self.label),
                                                x,
                                                s1,
                                            )));
                                            Some(if_stmt)
                                        }
                                        None => {
                                            panic!("Error at line {}: missing expression", line);
                                        }
                                    },
                                    None => None,
                                }
                            }
                        }
                        None => None,
                    }
                }
                Token::While(_) => {
                    self.lexer.tokens.pop_front();
                    let while_line = self.get_line();
                    let while_cell = Rc::new(RefCell::new(0));
                    let while_stmt = StmtUnion::While(Box::new(While::new(
                        Rc::clone(&self.label),
                        Rc::clone(&while_cell),
                    )));

                    let enclosing_read = self.enclosing_stmt.borrow().to_owned();
                    let mut enclosing_write = self.enclosing_stmt.borrow_mut();
                    *enclosing_write = Some(StmtUnion::While(Box::new(While::new(
                        Rc::clone(&self.label),
                        Rc::clone(&while_cell),
                    ))));
                    drop(enclosing_write);

                    let mut next_t = self.lexer.tokens.pop_front();
                    self.get_line();
                    if let Some(Token::Lrb(_)) = next_t {
                    } else {
                        panic!("Error at line {}: token did not match (", while_line);
                    }
                    let expr = self.boolean();
                    next_t = self.lexer.tokens.pop_front();
                    self.get_line();
                    if let Some(Token::Rrb(_)) = next_t {
                    } else {
                        panic!("Error at line {}: token did not match )", while_line);
                    }

                    let stmt = self.stmt();

                    match while_stmt {
                        StmtUnion::While(mut ws) => {
                            ws.init(expr, stmt);
                            let mut enclosing_write = self.enclosing_stmt.borrow_mut();
                            *enclosing_write = enclosing_read;
                            drop(enclosing_write);
                            Some(StmtUnion::While(ws))
                        }
                        _ => {
                            panic!(
                                "Error at line {}: failed to initialize while statement",
                                while_line
                            );
                        }
                    }
                }
                _ => self.assign(),
            },
            None => None,
        }
    }

    fn assign(&mut self) -> Option<StmtUnion> {
        let mut t = self.lexer.tokens.pop_front();
        let line = self.get_line();
        match t {
            Some(Token::Id(s)) => {
                let symbol = self.symbol_table.get_mut(&s);
                match symbol {
                    Some(sym) => {
                        let id = sym.to_owned();
                        t = self.lexer.tokens.pop_front();
                        self.get_line();
                        match t {
                            Some(token) => {
                                if let Token::Asgn(_) = token {
                                } else {
                                    panic!("Error at line {}: token did not match =", line);
                                }
                                let expr = self.boolean();
                                match expr {
                                    Some(x) => {
                                        let stmt = StmtUnion::Set(Box::new(Set::new(id, x)));
                                        let next_t = self.lexer.tokens.pop_front();
                                        self.get_line();
                                        if let Some(Token::Scol(_)) = next_t {
                                        } else {
                                            panic!("Error at line {}: token did not match ;", line);
                                        }
                                        Some(stmt)
                                    }
                                    None => None,
                                }
                            }
                            None => None,
                        }
                    }
                    None => {
                        let mut next_t = self.lexer.tokens.pop_front();
                        self.get_line();
                        if let Some(Token::Asgn(_)) = next_t {
                        } else {
                            panic!("Error at line {}: {} undeclared", line, s);
                        }
                        let expr = self.boolean();
                        let id = Id::new(Token::Id(s.clone()));
                        self.symbol_table.insert(s, id.clone());
                        match expr {
                            Some(x) => {
                                let stmt = StmtUnion::Set(Box::new(Set::new(id, x)));
                                next_t = self.lexer.tokens.pop_front();
                                self.get_line();
                                if let Some(Token::Scol(_)) = next_t {
                                } else {
                                    panic!("Error at line {}: token did not match ;", line);
                                }
                                Some(stmt)
                            }
                            None => None,
                        }
                    }
                }
            }
            _ => {
                panic!("Error at line {}: token did not match Id", line);
            }
        }
    }

    fn boolean(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.join();
        while let Some(Token::Or(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.join();
                    match expr2 {
                        Some(x2) => {
                            let or = ExprUnion::Or(Box::new(Or::new(
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                Token::Or(token_string),
                                x1,
                                x2,
                            )));
                            expr1 = Some(or);
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => {
                    expr1 = None;
                }
            }
        }
        expr1
    }

    fn join(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.equality();
        while let Some(Token::And(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.equality();
                    match expr2 {
                        Some(x2) => {
                            let and = ExprUnion::And(Box::new(And::new(
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                Token::And(token_string),
                                x1,
                                x2,
                            )));
                            expr1 = Some(and);
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => {
                    expr1 = None;
                }
            }
        }
        expr1
    }

    fn equality(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.rel();
        while let Some(Token::Eql(s)) | Some(Token::Ne(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.rel();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"==" {
                                let eql = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Eql(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(eql);
                            } else if token_string == *"!=" {
                                let ne = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Ne(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(ne);
                            } else {
                                expr1 = None;
                            }
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => {
                    expr1 = None;
                }
            }
        }
        expr1
    }

    fn rel(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.expr();
        if let Some(Token::Lt(s)) | Some(Token::Gt(s)) | Some(Token::Le(s)) | Some(Token::Ge(s)) =
            self.lexer.tokens.front()
        {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.expr();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"<" {
                                let lt = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Lt(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(lt);
                            } else if token_string == *">" {
                                let gt = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Gt(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(gt);
                            } else if token_string == *"<=" {
                                let le = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Le(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(le);
                            } else if token_string == *">=" {
                                let ge = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Ge(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(ge);
                            } else {
                                expr1 = None;
                            }
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => expr1 = None,
            }
        }
        expr1
    }

    fn expr(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.term();
        while let Some(Token::Add(s)) | Some(Token::Sub(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.term();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"+" {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Add(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if token_string == *"-" {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Sub(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else {
                                expr1 = None;
                            }
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => {
                    expr1 = None;
                }
            }
        }
        expr1
    }

    fn term(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.unary();
        while let Some(Token::Mul(s)) | Some(Token::Div(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.unary();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"*" {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Mul(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if token_string == *"/" {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Div(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else {
                                expr1 = None;
                            }
                        }
                        None => {
                            expr1 = None;
                        }
                    }
                }
                None => {
                    expr1 = None;
                }
            }
        }
        expr1
    }

    fn unary(&mut self) -> Option<ExprUnion> {
        let peek = self.lexer.tokens.front();
        match peek {
            Some(t) => {
                if let Token::Sub(s) = t.clone() {
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    let expr = self.unary();
                    match expr {
                        Some(x) => {
                            let unary = ExprUnion::Unary(Box::new(Unary::new(
                                Token::Sub(s),
                                Rc::clone(&self.temp_count),
                                x,
                            )));
                            Some(unary)
                        }
                        None => None,
                    }
                } else if let Token::Not(s) = t.clone() {
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    let expr = self.unary();
                    match expr {
                        Some(x) => {
                            let unary = ExprUnion::Not(Box::new(Not::new(
                                Token::Not(s),
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                x,
                            )));
                            Some(unary)
                        }
                        None => None,
                    }
                } else {
                    self.factor()
                }
            }
            None => None,
        }
    }

    fn factor(&mut self) -> Option<ExprUnion> {
        let peek = self.lexer.tokens.front();
        match peek {
            Some(t) => match t.clone() {
                Token::Num(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::Num(s))));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::True(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::True(s))));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::False(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::False(s))));
                    self.lexer.tokens.pop_front();
                    Some(constant)
                }
                Token::Lrb(_) => {
                    self.lexer.tokens.pop_front();
                    let expr = self.boolean();
                    let next = self.lexer.tokens.pop_front();
                    let line = self.get_line();
                    if let Some(Token::Rrb(_)) = next {
                        expr
                    } else {
                        panic!("Error at line {}: token did not match )", line);
                    }
                }
                Token::Id(s) => {
                    self.lexer.tokens.pop_front();
                    let line = self.get_line();
                    let symbol = self.symbol_table.get(&s);
                    match symbol {
                        Some(sym) => {
                            let id = ExprUnion::Id(Box::new(sym.clone()));
                            Some(id)
                        }
                        None => {
                            panic!("Error at line: {}: {} undeclared", line, s);
                        }
                    }
                }
                _ => None,
            },
            None => None,
        }
    }
}
