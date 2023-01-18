use super::inter::{
    And, Arith, Break, Constant, Else, ExprUnion, Id, If, Not, Or, Rel, Seq, Set, StmtUnion, Unary,
    While,
};
use super::lexer::{Lexer, Token};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug, Clone)]
struct Symbol {
    scope: u32,
    id: Id,
}

impl Symbol {
    pub fn new(scope: u32, id: Id) -> Self {
        return Symbol { scope, id };
    }
}

pub struct Parser {
    current_scope: u32,
    enclosing_stmt: Arc<RwLock<Option<StmtUnion>>>,
    label: Arc<Mutex<u32>>,
    lexer: Lexer,
    symbol_table: HashMap<String, Symbol>,
    temp_count: Arc<Mutex<u32>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        return Parser {
            current_scope: 0,
            enclosing_stmt: Arc::new(RwLock::new(None)),
            label: Arc::new(Mutex::new(0)),
            lexer: lexer,
            symbol_table: HashMap::new(),
            temp_count: Arc::new(Mutex::new(0)),
        };
    }

    fn increment_scope(&mut self) {
        self.current_scope = self.current_scope + 1;
    }

    fn decrement_scope(&mut self) {
        self.current_scope = self.current_scope - 1;
    }

    fn match_token(t: Token, s: String) -> bool {
        if t.value_to_string() == s {
            return true;
        }
        return false;
    }

    fn match_token_ref(t: &Token, s: String) -> bool {
        if t.to_owned().value_to_string() == s {
            return true;
        }
        return false;
    }

    fn match_option_token(ot: Option<Token>, s: String) -> bool {
        match ot {
            Some(token) => return Self::match_token(token, s),
            None => return false,
        }
    }

    fn match_option_token_ref(ot: Option<&Token>, s: String) -> bool {
        match ot {
            Some(token) => return Self::match_token_ref(token, s),
            None => return false,
        }
    }

    pub fn program(&mut self, input: &String) {
        self.lexer.lex(input);
        let stmt = self.block();
        match stmt {
            Some(s) => {
                let mut l = self.label.lock().unwrap();
                *l += 1;
                let begin = *l;
                *l += 1;
                let after = *l;
                drop(l);
                s.emit_label(begin);
                s.gen(begin, after);
                s.emit_label(after);
            }
            None => (),
        }
    }

    fn block(&mut self) -> Option<StmtUnion> {
        let mut t = self.lexer.tokens.pop_front();
        match t {
            Some(token) => {
                if !Self::match_token(token, "{".to_string()) {
                    println!("token did not match {{");
                    return None;
                }
            }
            None => {
                return None;
            }
        }
        let saved_symbol_table = self.symbol_table.clone();
        self.increment_scope();
        let stmt = self.stmts();
        t = self.lexer.tokens.pop_front();
        match t {
            Some(token) => {
                if !Self::match_token(token, "}".to_string()) {
                    println!("token did not match }}");
                    return None;
                }
            }
            None => {
                println!("token did not match }}");
                return None;
            }
        }

        self.decrement_scope();
        self.symbol_table = saved_symbol_table;
        return stmt;
    }

    fn stmts(&mut self) -> Option<StmtUnion> {
        let t = self.lexer.tokens.front();
        match t {
            Some(token) => {
                if Self::match_token(token.clone(), "}".to_string()) {
                    return None;
                }
                let stmt1 = self.stmt();
                let stmt2 = self.stmts();
                let seq = StmtUnion::Seq(Box::new(Seq::new(Arc::clone(&self.label), stmt1, stmt2)));
                return Some(seq);
            }
            None => return None,
        }
    }

    fn stmt(&mut self) -> Option<StmtUnion> {
        let t = self.lexer.tokens.front();
        match t {
            Some(token) => {
                match token {
                    Token::Id(s) => {
                        if s == ";" {
                            self.lexer.tokens.pop_front();
                            return None;
                        } else if s == "{" {
                            let stmt = self.block();
                            return stmt;
                        } else if s == "break" {
                            // TODO: palaa tähän, keksi Breakille parempi ratkaisu
                            self.lexer.tokens.pop_front();
                            let next_t = self.lexer.tokens.pop_front();
                            if !Self::match_option_token(next_t, ";".to_string()) {
                                println!("token did not match ;");
                                return None;
                            }
                            let enclosing = self.enclosing_stmt.read().unwrap().clone();
                            let break_stmt = StmtUnion::Break(Box::new(Break::new(enclosing)));
                            return Some(break_stmt);
                        } else {
                            let stmt = self.assign();
                            return stmt;
                        }
                    }
                    Token::If(_) => {
                        self.lexer.tokens.pop_front();
                        let mut next_t = self.lexer.tokens.pop_front();
                        if !Self::match_option_token(next_t, "(".to_string()) {
                            println!("token did not match (");
                            return None;
                        }
                        let expr = self.boolean();
                        next_t = self.lexer.tokens.pop_front();
                        if !Self::match_option_token(next_t, ")".to_string()) {
                            println!("token did not match )");
                            return None;
                        }
                        let stmt1 = self.stmt();
                        let peek = self.lexer.tokens.front();
                        match peek {
                            Some(next) => {
                                if !Self::match_token(next.clone(), "else".to_string()) {
                                    match stmt1 {
                                        Some(s1) => match expr {
                                            Some(x) => {
                                                let if_stmt = StmtUnion::If(Box::new(If::new(
                                                    Arc::clone(&self.label),
                                                    x,
                                                    s1,
                                                )));
                                                return Some(if_stmt);
                                            }
                                            None => {
                                                return None;
                                            }
                                        },
                                        None => return None,
                                    }
                                }
                                self.lexer.tokens.pop_front();
                                let stmt2 = self.stmt();
                                match stmt1 {
                                    Some(s1) => match stmt2 {
                                        Some(s2) => match expr {
                                            Some(x) => {
                                                let else_stmt = StmtUnion::Else(Box::new(
                                                    Else::new(Arc::clone(&self.label), x, s1, s2),
                                                ));
                                                return Some(else_stmt);
                                            }
                                            None => {
                                                return None;
                                            }
                                        },
                                        None => {
                                            return None;
                                        }
                                    },
                                    None => {
                                        return None;
                                    }
                                }
                            }
                            None => return None,
                        }
                    }
                    Token::While(_) => {
                        self.lexer.tokens.pop_front();
                        let while_mutex = Arc::new(Mutex::new(0));
                        let while_stmt = StmtUnion::While(Box::new(While::new(
                            Arc::clone(&self.label),
                            Arc::clone(&while_mutex)
                        )));

                        let enclosing_read = self.enclosing_stmt.read().unwrap().clone();
                        let mut enclosing_write = self.enclosing_stmt.write().unwrap();
                        *enclosing_write = Some(StmtUnion::While(Box::new(While::new(Arc::clone(&self.label), Arc::clone(&while_mutex)))));
                        drop(enclosing_write);

                        let mut next_t = self.lexer.tokens.pop_front();
                        if !Self::match_option_token(next_t, "(".to_string()) {
                            println!("token did not match (");
                            return None;
                        }
                        let expr = self.boolean();
                        next_t = self.lexer.tokens.pop_front();
                        if !Self::match_option_token(next_t, ")".to_string()) {
                            println!("token did not match )");
                            return None;
                        }
                        
                        let stmt = self.stmt();

                        match while_stmt {
                            StmtUnion::While(mut ws) => {
                                ws.init(expr, stmt);
                                let mut enclosing_write = self.enclosing_stmt.write().unwrap();
                                *enclosing_write = enclosing_read;
                                drop(enclosing_write);
                                return Some(StmtUnion::While(ws));
                            }
                            _ => {
                                println!("failed to initialize while statement");
                                return None;
                            }
                        }
                    }
                    _ => {
                        let stmt = self.assign();
                        return stmt;
                    }
                }
            }
            None => return None,
        }
    }

    fn assign(&mut self) -> Option<StmtUnion> {
        let t = self.lexer.tokens.pop_front();
        match t {
            Some(token) => match token {
                Token::Id(s) => {
                    let symbol = self.symbol_table.get_mut(&s.to_string());
                    match &symbol {
                        Some(sym) => {
                            if sym.scope > self.current_scope {
                                println!("{} out of scope", s.to_string());
                                return None;
                            }
                            let peek = self.lexer.tokens.front();
                            match peek {
                                Some(ptoken) => {
                                    if !Self::match_token(ptoken.clone(), "=".to_string()) {
                                        println!("token did not match =");
                                        return None;
                                    }
                                    self.lexer.tokens.pop_front();
                                    let id = sym.id.clone();
                                    let expr = self.boolean();
                                    match expr {
                                        Some(x) => {
                                            let stmt = StmtUnion::Set(Box::new(Set::new(id, x, Arc::clone(&self.temp_count))));
                                            let next_t = self.lexer.tokens.pop_front();
                                            if !Self::match_option_token(next_t, ";".to_string()) {
                                                println!("token did not match ;");
                                                return None;
                                            }
                                            return Some(stmt);
                                        }
                                        None => {
                                            return None;
                                        }
                                    }
                                }
                                None => {
                                    return None;
                                }
                            }
                        }
                        None => {
                            let mut next_t = self.lexer.tokens.pop_front();
                            if !Self::match_option_token(next_t, "=".to_string()) {
                                println!("{} undeclared", s.to_string());
                                return None;
                            }
                            let expr = self.boolean();
                            let id = Id::new(Token::Id(s.to_string()));
                            let new_symbol = Symbol::new(self.current_scope, id.clone());
                            self.symbol_table.insert(s.to_string(), new_symbol);
                            match expr {
                                Some(x) => {
                                    let stmt = StmtUnion::Set(Box::new(Set::new(id, x, Arc::clone(&self.temp_count))));
                                    next_t = self.lexer.tokens.pop_front();
                                    if !Self::match_option_token(next_t, ";".to_string()) {
                                        println!("token did not match ;");
                                        return None;
                                    }
                                    return Some(stmt);
                                }
                                None => {
                                    return None;
                                }
                            }
                        }
                    }
                }
                _ => {
                    println!("token did not match Id");
                    return None;
                }
            },
            None => {
                println!("token did not match Id");
                return None;
            }
        }
    }

    fn boolean(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.join();
        while let Some(Token::Or(s)) = self.lexer.tokens.front() {
            match expr1 {
                Some(x1) => {
                    let token_string = s.clone();
                    let expr2 = self.join();
                    match expr2 {
                        Some(x2) => {
                            let or = ExprUnion::Or(Box::new(Or::new(
                                Arc::clone(&self.label),
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
            self.lexer.tokens.pop_front();
        }
        return expr1;
    }

    fn join(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.equality();
        while let Some(Token::And(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.equality();
                    match expr2 {
                        Some(x2) => {
                            let and = ExprUnion::And(Box::new(And::new(
                                Arc::clone(&self.label),
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
            self.lexer.tokens.pop_front();
        }
        return expr1;
    }

    fn equality(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.rel();
        while let Some(Token::Eql(s)) | Some(Token::Ne(s)) = self.lexer.tokens.front() {
            let token_string = s.clone();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.rel();
                    match expr2 {
                        Some(x2) => {
                            if token_string == "==".to_string() {
                                let eql = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Eql(token_string),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(eql);
                            } else if token_string == "!=".to_string() {
                                let ne = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Ne(token_string),
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
            self.lexer.tokens.pop_front();
        }
        return expr1;
    }

    fn rel(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.expr();
        if let Some(Token::Lt(s)) | Some(Token::Gt(s)) | Some(Token::Le(s)) | Some(Token::Ge(s)) =
            self.lexer.tokens.front()
        {
            let token_string = s.clone();
            self.lexer.tokens.pop_front();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.expr();
                    match expr2 {
                        Some(x2) => {
                            if token_string == "<".to_string() {
                                let lt = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Lt(token_string),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(lt);
                            } else if token_string == ">".to_string() {
                                let gt = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Gt(token_string),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(gt);
                            } else if token_string == "<=".to_string() {
                                let le = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Le(token_string),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(le);
                            } else if token_string == ">=".to_string() {
                                let ge = ExprUnion::Rel(Box::new(Rel::new(
                                    Token::Ge(token_string),
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
        return expr1;
    }

    fn expr(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.term();
        while Self::match_option_token_ref(self.lexer.tokens.front(), "+".to_string())
            || Self::match_option_token_ref(self.lexer.tokens.front(), "-".to_string())
        {
            let t = self.lexer.tokens.pop_front();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.term();
                    match expr2 {
                        Some(x2) => {
                            if Self::match_option_token(
                                t.clone(),
                                "+".to_string(),
                            ) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("+".to_string()),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if Self::match_option_token(
                                t.clone(),
                                "-".to_string(),
                            ) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("-".to_string()),
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
        return expr1;
    }

    fn term(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.unary();
        while Self::match_option_token_ref(self.lexer.tokens.front(), "*".to_string())
            || Self::match_option_token_ref(self.lexer.tokens.front(), "/".to_string())
        {
            let t = self.lexer.tokens.pop_front();
            match expr1 {
                Some(x1) => {
                    let expr2 = self.unary();
                    match expr2 {
                        Some(x2) => {
                            if Self::match_option_token(
                                t.clone(),
                                "*".to_string(),
                            ) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("*".to_string()),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if Self::match_option_token(
                                t.clone(),
                                "/".to_string(),
                            ) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("/".to_string()),
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
        return expr1;
    }

    fn unary(&mut self) -> Option<ExprUnion> {
        let peek = self.lexer.tokens.front();
        match peek {
            Some(t) => {
                if Self::match_token(t.clone(), "-".to_string()) {
                    self.lexer.tokens.pop_front();
                    let expr = self.unary();
                    match expr {
                        Some(x) => {
                            let unary = ExprUnion::Unary(Box::new(Unary::new(
                                Token::Id("-".to_string()),
                                x,
                            )));
                            return Some(unary);
                        }
                        None => {
                            return None;
                        }
                    }
                } else if Self::match_token(t.clone(), "!".to_string()) {
                    self.lexer.tokens.pop_front();
                    let expr = self.unary();
                    match expr {
                        Some(x) => {
                            let unary =
                                ExprUnion::Not(Box::new(Not::new(Token::Id("!".to_string()), x)));
                            return Some(unary);
                        }
                        None => {
                            return None;
                        }
                    }
                } else {
                    let expr = self.factor();
                    return expr;
                }
            }
            None => {
                return None;
            }
        }
    }

    fn factor(&mut self) -> Option<ExprUnion> {
        let peek = self.lexer.tokens.front();
        match peek {
            Some(t) => match t.clone() {
                Token::Num(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::Num(s))));
                    self.lexer.tokens.pop_front();
                    return Some(constant);
                }
                Token::True(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::True(s))));
                    self.lexer.tokens.pop_front();
                    return Some(constant);
                }
                Token::False(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::False(s))));
                    self.lexer.tokens.pop_front();
                    return Some(constant);
                }
                Token::Id(s) => {
                    self.lexer.tokens.pop_front();
                    if s == "(".to_string() {
                        let expr = self.boolean();
                        let next = self.lexer.tokens.pop_front();
                        if !Self::match_option_token(next, ")".to_string()) {
                            println!("token did not match )");
                            return None;
                        }
                        return expr;
                    } else {
                        let symbol = self.symbol_table.get(&s.to_string());
                        match symbol {
                            Some(sym) => {
                                if sym.scope > self.current_scope {
                                    println!("{} out of scope", s.to_string());
                                    return None;
                                }
                                let id = ExprUnion::Id(Box::new(sym.id.clone()));
                                return Some(id);
                            }
                            None => {
                                println!("{} undeclared", s.to_string());
                                return None;
                            }
                        }
                    }
                }
                _ => {
                    return None;
                }
            },
            None => {
                return None;
            }
        }
    }
}
