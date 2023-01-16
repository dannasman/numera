use super::inter::{And, Arith, Constant, ExprUnion, Id, Not, Or, Rel, Unary};
use super::lexer::{Lexer, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

enum SymVal {
    Val(f64),
    Str(String),
}

struct Symbol {
    scope: u32,
    symbol: SymVal,
}

struct Parser {
    current_scope: u32,
    label: Rc<RefCell<u32>>,
    lexer: Lexer,
    symbol_table: HashMap<String, Symbol>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        return Parser {
            lexer: lexer,
            label: Rc::new(RefCell::new(0)),
            symbol_table: HashMap::new(),
            current_scope: 0,
        };
    }

    fn increment_scope(&mut self) {
        self.current_scope = self.current_scope + 1;
    }

    fn match_token(t: Token, s: String) -> bool {
        if t.value_to_string() == s {
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

    fn program(&mut self, input: &String) {
        self.lexer.lex(input);
        self.block();
    }

    fn block(&mut self) {
        let mut t = self.lexer.tokens.pop_back();
        match t {
            Some(token) => {
                if !Self::match_token(token, "{".to_string()) {
                    println!("token did not match {{");
                    return;
                }
            }
            None => {
                println!("token did not match {{");
                return;
            }
        }

        self.increment_scope();
        self.stmts();

        t = self.lexer.tokens.pop_back();
        match t {
            Some(token) => {
                if !Self::match_token(token, "}".to_string()) {
                    println!("token did not match }}");
                    return;
                }
            }
            None => {
                println!("token did not match }}");
                return;
            }
        }
    }

    fn stmts(&mut self) {
        let t = self.lexer.tokens.back();
        match t {
            Some(token) => {
                if Self::match_token(token.clone(), "}".to_string()) {
                    return;
                }
            }
            None => return,
        }
    }

    fn stmt(&mut self) {
        let t = self.lexer.tokens.back();
        match t {
            Some(token) => {
                match token {
                    Token::Id(s) => {
                        if s == ";" {
                            //Stmt.null
                            println!("implementation missing (Stmt.null)")
                        } else if s == "{" {
                            self.block();
                        } else {
                            println!("implementation missing (self.assign())")
                        }
                        return;
                    }
                    Token::If(_) => {
                        self.lexer.tokens.pop_back();
                        let mut next_t = self.lexer.tokens.pop_back();
                        if !Self::match_option_token(next_t, "(".to_string()) {
                            println!("token did not match (");
                            return;
                        }
                        //self.bool();
                        next_t = self.lexer.tokens.pop_back();
                        if !Self::match_option_token(next_t, ")".to_string()) {
                            println!("token did not match )");
                            return;
                        }
                        self.stmt();
                        let peek = self.lexer.tokens.back();
                        match peek {
                            Some(next) => {
                                if !Self::match_token(next.clone(), "else".to_string()) {
                                    //save If to AST
                                    return;
                                }
                                let _else = self.lexer.tokens.pop_back();
                                self.stmt();
                                return;
                            }
                            None => return,
                        }
                    }
                    Token::While(_) => {
                        self.lexer.tokens.pop_back();
                        //new while node
                        let mut next_t = self.lexer.tokens.pop_back();
                        if !Self::match_option_token(next_t, "(".to_string()) {
                            println!("token did not match (");
                            return;
                        }
                        //self.bool();
                        next_t = self.lexer.tokens.pop_back();
                        if !Self::match_option_token(next_t, ")".to_string()) {
                            println!("token did not match )");
                        }
                        self.stmt();
                        return;
                    }
                    _ => println!("implementation missing (self.assign())"),
                }
            }
            None => return,
        }
    }

    fn assign(&mut self) {
        let mut t = self.lexer.tokens.pop_back();
        match t {
            Some(token) => {
                match token {
                    Token::Id(s) => {
                        let id = self.symbol_table.get(&s.to_string());
                        match id {
                            Some(symbol) => {
                                if symbol.scope > self.current_scope {
                                    println!("{} out of scope", s.to_string());
                                    return;
                                }
                                let peek = self.lexer.tokens.back();
                                match peek {
                                    Some(ptoken) => {
                                        if !Self::match_token(ptoken.clone(), "=".to_string()) {
                                            println!("token did not match =");
                                            return;
                                        }
                                        self.lexer.tokens.pop_back();
                                        //self.bool();
                                    }
                                    None => return,
                                }
                            }
                            None => {
                                println!("{} undeclared", s.to_string());
                                return;
                            }
                        }
                    }
                    _ => {
                        println!("token did not match Id");
                        return;
                    }
                }
            }
            None => {
                println!("token did not match Id");
                return;
            }
        }
        t = self.lexer.tokens.pop_back();
        match t {
            Some(token) => {
                if !Self::match_token(token, ";".to_string()) {
                    println!("token did not match ;");
                    return;
                }
            }
            None => {
                println!("token did not match ;");
                return;
            }
        }
    }

    fn boolean(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.join(); //TODO: self.join();
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::Or(s)) = t {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.join(); //TODO: self.join();
                    match expr2 {
                        Some(x2) => {
                            let or = ExprUnion::Or(Box::new(Or::new(
                                Rc::clone(&self.label),
                                Token::Or(s),
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
            t = self.lexer.tokens.pop_back();
        }
        return expr1;
    }

    fn join(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.equality();
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::And(s)) = t {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.equality();
                    match expr2 {
                        Some(x2) => {
                            let and = ExprUnion::And(Box::new(And::new(
                                Rc::clone(&self.label),
                                Token::And(s),
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
            t = self.lexer.tokens.pop_back();
        }
        return expr1;
    }

    fn equality(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.rel();
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::Eql(s)) | Some(Token::Ne(s)) = t {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.rel();
                    match expr2 {
                        Some(x2) => {
                            if s == "==".to_string() {
                                let eql = ExprUnion::Rel(Box::new(Rel::new(Token::Eql(s), x1, x2)));
                                expr1 = Some(eql);
                            } else if s == "!=".to_string() {
                                let ne = ExprUnion::Rel(Box::new(Rel::new(Token::Ne(s), x1, x2)));
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
            t = self.lexer.tokens.pop_back();
        }
        return expr1;
    }

    fn rel(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.expr();
        let t = self.lexer.tokens.pop_back();
        if let Some(Token::Lt(s)) | Some(Token::Gt(s)) | Some(Token::Le(s)) | Some(Token::Ge(s)) = t
        {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.expr();
                    match expr2 {
                        Some(x2) => {
                            if s == "<".to_string() {
                                let lt = ExprUnion::Rel(Box::new(Rel::new(Token::Lt(s), x1, x2)));
                                expr1 = Some(lt);
                            } else if s == ">".to_string() {
                                let gt = ExprUnion::Rel(Box::new(Rel::new(Token::Gt(s), x1, x2)));
                                expr1 = Some(gt);
                            } else if s == "<=".to_string() {
                                let le = ExprUnion::Rel(Box::new(Rel::new(Token::Le(s), x1, x2)));
                                expr1 = Some(le);
                            } else if s == ">=".to_string() {
                                let ge = ExprUnion::Rel(Box::new(Rel::new(Token::Ge(s), x1, x2)));
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
        let mut expr1 = self.factor(); //TODO: self.term();
        let mut t = self.lexer.tokens.pop_back();
        while Self::match_option_token(t.clone(), "+".to_string())
            || Self::match_option_token(t.clone(), "-".to_string())
        {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.factor(); //TODO: self.term();
                    match expr2 {
                        Some(x2) => {
                            if Self::match_option_token(t.clone(), "+".to_string()) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("+".to_string()),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if Self::match_option_token(t.clone(), "-".to_string()) {
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
            t = self.lexer.tokens.pop_back();
        }
        return expr1;
    }

    fn term(&mut self) -> Option<ExprUnion> {
        let mut expr1 = self.unary();
        let mut t = self.lexer.tokens.pop_back();
        while Self::match_option_token(t.clone(), "*".to_string())
            || Self::match_option_token(t.clone(), "/".to_string())
        {
            match expr1 {
                Some(x1) => {
                    let expr2 = self.unary();
                    match expr2 {
                        Some(x2) => {
                            if Self::match_option_token(t.clone(), "*".to_string()) {
                                let arith = ExprUnion::Arith(Box::new(Arith::new(
                                    Token::Id("*".to_string()),
                                    x1,
                                    x2,
                                )));
                                expr1 = Some(arith);
                            } else if Self::match_option_token(t.clone(), "/".to_string()) {
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
            t = self.lexer.tokens.pop_back();
        }
        return expr1;
    }

    fn unary(&mut self) -> Option<ExprUnion> {
        let peek = self.lexer.tokens.back();
        match peek {
            Some(t) => {
                if Self::match_token(t.clone(), "-".to_string()) {
                    self.lexer.tokens.pop_back();
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
                    self.lexer.tokens.pop_back();
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
        let peek = self.lexer.tokens.back();
        match peek {
            Some(t) => match t.clone() {
                Token::Num(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::Num(s))));
                    self.lexer.tokens.pop_back();
                    return Some(constant);
                }
                Token::True(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::True(s))));
                    self.lexer.tokens.pop_back();
                    return Some(constant);
                }
                Token::False(s) => {
                    let constant = ExprUnion::Constant(Box::new(Constant::new(Token::False(s))));
                    self.lexer.tokens.pop_back();
                    return Some(constant);
                }
                Token::Id(s) => {
                    self.lexer.tokens.pop_back();
                    if s == "(".to_string() {
                        let expr = self.boolean();
                        let next = self.lexer.tokens.pop_back();
                        if !Self::match_option_token(next, ")".to_string()) {
                            println!("token did not match )");
                            return None;
                        }
                        return expr;
                    } else {
                        let id = self.symbol_table.get(&s.to_string());
                        match id {
                            Some(symbol) => {
                                if symbol.scope > self.current_scope {
                                    println!("{} out of scope", s.to_string());
                                    return None;
                                }
                                let id = ExprUnion::Id(Box::new(Id::new(Token::Id(s))));
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
