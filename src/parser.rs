use super::inter::{
    Access, And, Arith, Break, Call, Constant, Else, ExprUnion, Function, Id, If, Not, Or, Rel,
    Return, Seq, Set, SetElem, StmtUnion, Unary, While,
};
use super::lexer::{Array, Lexer, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Parser {
    enclosing_stmt: Rc<RefCell<Option<StmtUnion>>>,
    declarations: Vec<StmtUnion>,
    label: Rc<RefCell<u32>>,
    lexer: Lexer,
    symbol_table: HashMap<String, Id>,
    temp_count: Rc<RefCell<u32>>,
    used: u32,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            enclosing_stmt: Rc::new(RefCell::new(None)),
            declarations: Vec::new(),
            label: Rc::new(RefCell::new(0)),
            lexer,
            symbol_table: HashMap::new(),
            temp_count: Rc::new(RefCell::new(0)),
            used: 0,
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

            self.declarations.iter().for_each(|s| {
                s.gen(begin, after);
            });

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
                        StmtUnion::Seq(Rc::new(Seq::new(Rc::clone(&self.label), stmt1, stmt2)));
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
                                    let break_stmt = StmtUnion::Break(Rc::new(new_break));
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
                                                let else_stmt =
                                                    Else::new(Rc::clone(&self.label), x, s1, s2);
                                                match else_stmt {
                                                    Ok(s) => Some(StmtUnion::Else(Rc::new(s))),
                                                    Err(e) => {
                                                        panic!("Error at line {}: {}", line, e);
                                                    }
                                                }
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
                                            let if_stmt = If::new(Rc::clone(&self.label), x, s1);
                                            match if_stmt {
                                                Ok(s) => Some(StmtUnion::If(Rc::new(s))),
                                                Err(e) => {
                                                    panic!("Error at line {}: {}", line, e);
                                                }
                                            }
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
                    let while_stmt = StmtUnion::While(Rc::new(RefCell::new(While::new(
                        Rc::clone(&self.label),
                        Rc::clone(&while_cell),
                    ))));

                    let enclosing_read = self.enclosing_stmt.borrow().to_owned();
                    let mut enclosing_write = self.enclosing_stmt.borrow_mut();
                    *enclosing_write = Some(StmtUnion::While(Rc::new(RefCell::new(While::new(
                        Rc::clone(&self.label),
                        Rc::clone(&while_cell),
                    )))));
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
                        StmtUnion::While(ws) => {
                            if let Err(e) = ws.borrow_mut().init(expr, stmt) {
                                panic!("Error at line {}: {}", while_line, e);
                            }
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
                Token::Def(_) => {
                    self.lexer.tokens.pop_front();
                    let function_line = self.get_line();
                    if let Some(tp @ Token::Int(_))
                    | Some(tp @ Token::Float(_))
                    | Some(tp @ Token::Bool(_)) = self.lexer.tokens.pop_front()
                    {
                        self.get_line();
                        if let Some(Token::Id(id_s)) = self.lexer.tokens.pop_front() {
                            self.get_line();
                            if self.symbol_table.get_mut(&id_s).is_some() {
                                panic!(
                                    "Error at line {}: function {} already declared",
                                    function_line, id_s
                                );
                            } else {
                                let id =
                                    Id::new(tp.to_owned(), Token::Id(id_s.to_owned()), self.used);
                                match tp.get_width() {
                                    Ok(w) => self.used += w,
                                    Err(e) => panic!("Error at line {}: {}", function_line, e),
                                }
                                self.symbol_table.insert(id_s.to_owned(), id.clone());
                                self.get_line();
                                if let Some(Token::Lrb(_)) = self.lexer.tokens.pop_front() {
                                } else {
                                    panic!(
                                        "Error at line {}: token did not match (",
                                        function_line
                                    );
                                }
                                let params = self.params();
                                self.get_line();
                                if let Some(Token::Rrb(_)) = self.lexer.tokens.pop_front() {
                                } else {
                                    panic!(
                                        "Error at line {}: token did not match )",
                                        function_line
                                    );
                                }

                                let saved_symbol_table = self.symbol_table.clone();

                                params.iter().for_each(|p| {
                                    self.symbol_table
                                        .insert(p.token.to_owned().value_to_string(), p.clone());
                                });

                                self.get_line();
                                if let Some(Token::Lcb(_)) = self.lexer.tokens.pop_front() {
                                } else {
                                    panic!(
                                        "Error at line {}: token did not match {{",
                                        function_line
                                    );
                                }

                                let stmt = self.stmts();

                                self.get_line();
                                if let Some(Token::Rcb(_)) = self.lexer.tokens.pop_front() {
                                } else {
                                    panic!(
                                        "Error at line {}: token did not match }}",
                                        function_line
                                    );
                                }

                                self.symbol_table = saved_symbol_table;

                                let function_stmt = StmtUnion::Function(Rc::new(Function::new(
                                    Rc::clone(&self.label),
                                    id_s,
                                    params,
                                    stmt,
                                )));
                                self.declarations.push(function_stmt);
                                None
                            }
                        } else {
                            panic!("Error at line {}: failed to define function due to missing function name", function_line)
                        }
                    } else {
                        panic!("Error at line {}: failed to define function due to missing function type", function_line);
                    }
                }
                Token::Return(_) => {
                    self.lexer.tokens.pop_front();
                    let return_line = self.get_line();

                    if let Some(Token::Scol(_)) = self.lexer.tokens.front() {
                        let return_stmt = StmtUnion::Return(Rc::new(Return::new(None)));
                        Some(return_stmt)
                    } else {
                        let expr = self.expr();

                        self.get_line();
                        if let Some(Token::Scol(_)) = self.lexer.tokens.pop_front() {
                        } else {
                            panic!("Error at line {}: token did not match ;", return_line);
                        }

                        self.get_line();
                        if let Some(Token::Rcb(_)) = self.lexer.tokens.front() {
                        } else {
                            panic!(
                                "Error at line {}: body should end after return",
                                return_line
                            );
                        }

                        let return_stmt = StmtUnion::Return(Rc::new(Return::new(expr)));
                        Some(return_stmt)
                    }
                }
                _ => self.assign(),
            },
            None => None,
        }
    }

    fn params(&mut self) -> Vec<Id> {
        let params_line = self.get_line();
        let mut params_vec: Vec<Id> = Vec::new();
        while let Some(tp @ Token::Int(_))
        | Some(tp @ Token::Float(_))
        | Some(tp @ Token::Bool(_)) = self.lexer.tokens.front()
        {
            let param_tp = tp.to_owned();
            self.lexer.tokens.pop_front();
            self.get_line();
            if let Some(Token::Id(id_s)) = self.lexer.tokens.front() {
                let id_string = id_s.to_owned();
                self.lexer.tokens.pop_front();
                self.get_line();
                if self.symbol_table.get_mut(&id_string).is_some() {
                    panic!(
                        "Error at line {}: parameter {} already declared",
                        params_line, id_string
                    );
                } else {
                    let id = Id::new(param_tp, Token::Id(id_string), self.used);
                    params_vec.push(id);
                }
            } else {
                panic!("Error at line {}: missing parameter name", params_line);
            }
        }
        params_vec
    }

    fn dims(&mut self, tp: Token) -> Token {
        let size;
        let mut of = tp.to_owned();

        let mut t = self.lexer.tokens.pop_front();
        let line = self.get_line();
        if let Some(Token::Lsb(_)) = t {
        } else {
            panic!("Error at line {}: token did not match [", line);
        }

        t = self.lexer.tokens.pop_front();
        self.get_line();
        if let Some(Token::Num(i)) = t {
            size = i
        } else {
            panic!("Error at line {}: index must be integer", line);
        }

        t = self.lexer.tokens.pop_front();
        self.get_line();
        if let Some(Token::Rsb(_)) = t {
        } else {
            panic!("Error at line {}: token did not match ]", line);
        }

        if let Some(Token::Lsb(_)) = self.lexer.tokens.front() {
            of = self.dims(tp);
        }
        match of.get_width() {
            Ok(w) => Token::Arr(Array::new(size, of, w)),
            Err(e) => {
                panic!("Error at line {}: {}", line, e)
            }
        }
    }
    fn assign(&mut self) -> Option<StmtUnion> {
        let mut t = self.lexer.tokens.pop_front();
        let mut line = self.get_line();
        match t {
            Some(tp @ Token::Int(_)) | Some(tp @ Token::Float(_)) | Some(tp @ Token::Bool(_)) => {
                t = self.lexer.tokens.pop_front();
                self.get_line();
                if let Some(Token::Id(id_s)) = t {
                    if let Some(Token::Asgn(_)) = self.lexer.tokens.front() {
                        self.lexer.tokens.pop_front();
                        line = self.get_line();
                        if self.symbol_table.get_mut(&id_s).is_some() {
                            panic!("Error at line {}: variable {} already declared", line, id_s);
                        }

                        let id = Id::new(tp.to_owned(), Token::Id(id_s.to_owned()), self.used);
                        match tp.get_width() {
                            Ok(w) => self.used += w,
                            Err(e) => panic!("Error at line {}: {}", line, e),
                        }

                        let expr = self.boolean();
                        self.symbol_table.insert(id_s, id.clone());
                        match expr {
                            Some(x) => {
                                let set_stmt = Set::new(id, x);
                                match set_stmt {
                                    Ok(set) => {
                                        t = self.lexer.tokens.pop_front();
                                        self.get_line();
                                        if let Some(Token::Scol(_)) = t {
                                        } else {
                                            panic!("Error at line {}: token did not match ;", line);
                                        }
                                        Some(StmtUnion::Set(Rc::new(set)))
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            }
                            None => {
                                panic!("Error at line {}: expression missing", line);
                            }
                        }
                    } else {
                        let array_tp = self.dims(tp);
                        let id =
                            Id::new(array_tp.to_owned(), Token::Id(id_s.to_owned()), self.used);
                        match array_tp.get_width() {
                            Ok(w) => self.used += w,
                            Err(e) => panic!("Error at line {}: {}", line, e),
                        }
                        self.symbol_table.insert(id_s, id);

                        t = self.lexer.tokens.pop_front();
                        self.get_line();
                        if let Some(Token::Scol(_)) = t {
                        } else {
                            panic!("Error at line {}: token did not match ;", line);
                        }
                        None
                    }
                } else {
                    panic!("Error at line {}: variable name missing", line);
                }
            }
            Some(Token::Id(id_s)) => {
                let symbol = self.symbol_table.get_mut(&id_s);
                match symbol {
                    Some(sym) => {
                        let id = sym.to_owned();
                        match self.lexer.tokens.front() {
                            Some(token) => {
                                if let Token::Asgn(_) = token {
                                    self.lexer.tokens.pop_front();
                                    self.get_line();
                                    let expr = self.boolean();
                                    match expr {
                                        Some(x) => {
                                            let set_stmt = Set::new(id, x);
                                            match set_stmt {
                                                Ok(set) => {
                                                    t = self.lexer.tokens.pop_front();
                                                    self.get_line();
                                                    if let Some(Token::Scol(_)) = t {
                                                    } else {
                                                        panic!(
                                                            "Error at line {}: token did not match ;",
                                                            line
                                                        );
                                                    }
                                                    Some(StmtUnion::Set(Rc::new(set)))
                                                }
                                                Err(e) => {
                                                    panic!("Error at line {}: {}", line, e);
                                                }
                                            }
                                        }
                                        None => {
                                            panic!("Error at line {}: expression missing", line);
                                        }
                                    }
                                } else {
                                    if let Token::Lsb(_) = token {
                                    } else {
                                        panic!("Error at line {}: invalid assign statement", line);
                                    }
                                    let access = self.offset(id);
                                    t = self.lexer.tokens.pop_front();
                                    self.get_line();
                                    if let Some(Token::Asgn(_)) = t {
                                        let expr = self.boolean();
                                        match expr {
                                            Some(x) => {
                                                let set_stmt = SetElem::new(access, x);
                                                match set_stmt {
                                                    Ok(set) => {
                                                        t = self.lexer.tokens.pop_front();
                                                        self.get_line();
                                                        if let Some(Token::Scol(_)) = t {
                                                        } else {
                                                            panic!(
                                                                "Error at line {}: token did not match ;",
                                                                line
                                                            );
                                                        }
                                                        Some(StmtUnion::SetElem(Rc::new(set)))
                                                    }
                                                    Err(e) => {
                                                        panic!("Error at line {}: {}", line, e);
                                                    }
                                                }
                                            }
                                            None => {
                                                panic!(
                                                    "Error at line {}: expression missing",
                                                    line
                                                );
                                            }
                                        }
                                    } else {
                                        panic!("Error at line {}: token did not match =", line);
                                    }
                                }
                            }
                            None => None,
                        }
                    }
                    None => {
                        panic!("Error at line {}: type of {} missing", line, id_s);
                    }
                }
            }
            _ => {
                panic!("Error at line {}: assign failed", line);
            }
        }
    }

    fn boolean(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.join();
        while let Some(Token::Or(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.join();
                    match expr2 {
                        Some(x2) => {
                            let or = Or::new(
                                Token::Or(token_string),
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                x1,
                                x2,
                            );
                            match or {
                                Ok(o) => {
                                    expr1 = Some(ExprUnion::Or(Rc::new(o)));
                                }
                                Err(e) => {
                                    panic!("Error at line {}: {}", line, e);
                                }
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

    fn join(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.equality();
        while let Some(Token::And(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.equality();
                    match expr2 {
                        Some(x2) => {
                            let and = And::new(
                                Token::And(token_string),
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                x1,
                                x2,
                            );
                            match and {
                                Ok(a) => {
                                    expr1 = Some(ExprUnion::And(Rc::new(a)));
                                }
                                Err(e) => {
                                    panic!("Error at line {}: {}", line, e);
                                }
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

    fn equality(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.rel();
        while let Some(Token::Eql(s)) | Some(Token::Ne(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.rel();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"==" {
                                let eql = Rel::new(
                                    Token::Eql(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match eql {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *"!=" {
                                let ne = Rel::new(
                                    Token::Ne(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match ne {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
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
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.expr();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"<" {
                                let lt = Rel::new(
                                    Token::Lt(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match lt {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *">" {
                                let gt = Rel::new(
                                    Token::Gt(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match gt {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *"<=" {
                                let le = Rel::new(
                                    Token::Le(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match le {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *">=" {
                                let ge = Rel::new(
                                    Token::Ge(token_string),
                                    Rc::clone(&self.label),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match ge {
                                    Ok(rel) => {
                                        expr1 = Some(ExprUnion::Rel(Rc::new(rel)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
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
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.term();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"+" {
                                let arith = Arith::new(
                                    Token::Add(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match arith {
                                    Ok(a) => {
                                        expr1 = Some(ExprUnion::Arith(Rc::new(a)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *"-" {
                                let arith = Arith::new(
                                    Token::Sub(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match arith {
                                    Ok(a) => {
                                        expr1 = Some(ExprUnion::Arith(Rc::new(a)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
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
            let line = self.get_line();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.unary();
                    match expr2 {
                        Some(x2) => {
                            if token_string == *"*" {
                                let arith = Arith::new(
                                    Token::Mul(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match arith {
                                    Ok(a) => {
                                        expr1 = Some(ExprUnion::Arith(Rc::new(a)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
                            } else if token_string == *"/" {
                                let arith = Arith::new(
                                    Token::Div(token_string),
                                    Rc::clone(&self.temp_count),
                                    x1,
                                    x2,
                                );
                                match arith {
                                    Ok(a) => {
                                        expr1 = Some(ExprUnion::Arith(Rc::new(a)));
                                    }
                                    Err(e) => {
                                        panic!("Error at line {}: {}", line, e);
                                    }
                                }
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
                            let unary = ExprUnion::Unary(Rc::new(Unary::new(
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
                    let line = self.get_line();
                    let expr = self.unary();
                    match expr {
                        Some(x) => {
                            let unary = Not::new(
                                Token::Not(s),
                                Rc::clone(&self.label),
                                Rc::clone(&self.temp_count),
                                x,
                            );
                            match unary {
                                Ok(u) => Some(ExprUnion::Not(Rc::new(u))),
                                Err(e) => {
                                    panic!("Error at line {}: {}", line, e);
                                }
                            }
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
                    let constant = ExprUnion::Constant(Rc::new(Constant::new(
                        Token::Int(String::from("int")),
                        Token::Num(s),
                    )));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::Real(s) => {
                    let constant = ExprUnion::Constant(Rc::new(Constant::new(
                        Token::Float(String::from("float")),
                        Token::Real(s),
                    )));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::True(s) => {
                    let constant = ExprUnion::Constant(Rc::new(Constant::new(
                        Token::Bool(String::from("bool")),
                        Token::True(s),
                    )));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::False(s) => {
                    let constant = ExprUnion::Constant(Rc::new(Constant::new(
                        Token::Bool(String::from("bool")),
                        Token::False(s),
                    )));
                    self.lexer.tokens.pop_front();
                    self.get_line();
                    Some(constant)
                }
                Token::Lrb(_) => {
                    self.lexer.tokens.pop_front();
                    self.get_line();
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
                            let id = ExprUnion::Id(Rc::new(sym.to_owned()));
                            match self.lexer.tokens.front() {
                                Some(Token::Lsb(_)) => {
                                    let access = self.offset(sym.to_owned());
                                    Some(ExprUnion::Access(Rc::new(access)))
                                }
                                Some(Token::Lrb(_)) => {
                                    let func_id = sym.to_owned();
                                    let args: Vec<ExprUnion> = self.args();
                                    let call = ExprUnion::Call(Rc::new(Call::new(
                                        func_id,
                                        args,
                                        Rc::clone(&self.temp_count),
                                    )));
                                    Some(call)
                                }
                                _ => Some(id),
                            }
                        }
                        None => {
                            panic!("Error at line {}: {} undeclared", line, s);
                        }
                    }
                }
                _ => {
                    let line = self.get_line();
                    panic!("Error at line {}: missing expression", line)
                }
            },
            None => None,
        }
    }

    fn args(&mut self) -> Vec<ExprUnion> {
        let mut args_vec: Vec<ExprUnion> = Vec::new();

        let args_line = self.get_line();

        if let Some(Token::Lrb(_)) = self.lexer.tokens.pop_front() {
        } else {
            panic!("Error at line {}: missing (", args_line);
        }

        while let Some(Token::Id(_))
        | Some(Token::True(_))
        | Some(Token::False(_))
        | Some(Token::Num(_))
        | Some(Token::Lrb(_)) = self.lexer.tokens.front()
        {
            if let Some(expr) = self.boolean() {
                args_vec.push(expr);
            } else {
                panic!("Error at line {}: missing expression", args_line);
            }
        }

        if let Some(Token::Rrb(_)) = self.lexer.tokens.pop_front() {
        } else {
            panic!("Error at line {}: missing )", args_line);
        }

        args_vec.reverse();
        args_vec
    }

    fn offset(&mut self, a: Id) -> Access {
        let mut tp = a.tp.to_owned();

        let mut t = self.lexer.tokens.pop_front();
        let line = self.get_line();
        if let Some(Token::Lsb(_)) = t {
        } else {
            panic!("Error at line {}: token did not macth [", line);
        }

        let mut idx = self.boolean();

        t = self.lexer.tokens.pop_front();
        self.get_line();
        if let Some(Token::Rsb(_)) = t {
        } else {
            panic!("Error at line {}: token did not macth ]", line);
        }

        if let Token::Arr(array) = tp {
            tp = *array.of;
        }

        match tp.get_width() {
            Ok(width) => {
                let mut w = Constant::new(Token::Int(String::from("int")), Token::Num(width));
                match idx {
                    Some(i) => {
                        let mut temp1 = Arith::new(
                            Token::Mul(String::from("*")),
                            Rc::clone(&self.temp_count),
                            i,
                            ExprUnion::Constant(Rc::new(w.to_owned())),
                        );
                        match temp1 {
                            Ok(t1) => {
                                let mut loc = t1;

                                while let Some(Token::Lsb(_)) = self.lexer.tokens.front() {
                                    self.lexer.tokens.pop_front();
                                    self.get_line();

                                    idx = self.boolean();
                                    match idx {
                                        Some(i_inner) => {
                                            t = self.lexer.tokens.pop_front();
                                            self.get_line();
                                            if let Some(Token::Rsb(_)) = t {
                                            } else {
                                                panic!(
                                                    "Error at line {}: token did not match ]",
                                                    line
                                                );
                                            }

                                            if let Token::Arr(array) = tp {
                                                tp = *array.of;
                                            }

                                            match tp.get_width() {
                                                Ok(width_inner) => {
                                                    w = Constant::new(
                                                        Token::Int(String::from("int")),
                                                        Token::Num(width_inner),
                                                    );

                                                    temp1 = Arith::new(
                                                        Token::Mul(String::from("*")),
                                                        Rc::clone(&self.temp_count),
                                                        i_inner.to_owned(),
                                                        ExprUnion::Constant(Rc::new(w.to_owned())),
                                                    );
                                                    match temp1 {
                                                        Ok(t1_inner) => {
                                                            let temp2 = Arith::new(
                                                                Token::Add(String::from("+")),
                                                                Rc::clone(&self.temp_count),
                                                                ExprUnion::Arith(Rc::new(
                                                                    loc.to_owned(),
                                                                )),
                                                                ExprUnion::Arith(Rc::new(
                                                                    t1_inner.to_owned(),
                                                                )),
                                                            );
                                                            match temp2 {
                                                                Ok(t2) => {
                                                                    loc = t2;
                                                                }
                                                                Err(e) => {
                                                                    panic!(
                                                                        "Error at line {}: {}",
                                                                        line, e
                                                                    );
                                                                }
                                                            }
                                                        }
                                                        Err(e) => {
                                                            panic!("Error at line {}: {}", line, e)
                                                        }
                                                    }
                                                }
                                                Err(e) => panic!("Error at line {}: {}", line, e),
                                            }
                                        }
                                        None => {
                                            panic!("Error at line {}: index missing", line);
                                        }
                                    }
                                }
                                Access::new(
                                    tp.to_owned(),
                                    Rc::clone(&self.temp_count),
                                    a,
                                    ExprUnion::Arith(Rc::new(loc)),
                                )
                            }
                            Err(e) => {
                                panic!("Error at line {}: {}", line, e);
                            }
                        }
                    }
                    None => {
                        panic!("Error at line {}: index missing", line);
                    }
                }
            }
            Err(e) => panic!("Error at line {}: {}", line, e),
        }
    }
}
