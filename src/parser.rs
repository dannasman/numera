use std::collections::HashMap;
use std::convert::Into;
use std::mem::swap;

use super::inter::*;
use super::lexer::Lexer;
use super::tokens::{Tag, Token};

const OPEN_BR: u32 = b'{' as u32;
const SEMICOLON: u32 = b';' as u32;
const IF: u32 = Tag::IF as u32;
const WHILE: u32 = Tag::WHILE as u32;
const DO: u32 = Tag::DO as u32;
const BREAK: u32 = Tag::BREAK as u32;
const LT: u32 = b'<' as u32;
const GT: u32 = b'>' as u32;
const LE: u32 = Tag::LE as u32;
const GE: u32 = Tag::GE as u32;
const MINUS: u32 = b'-' as u32;
const EXCL: u32 = b'!' as u32;
const OPAREN: u32 = b'(' as u32;
const NUM: u32 = Tag::NUM as u32;
const REAL: u32 = Tag::REAL as u32;
const TRUE: u32 = Tag::TRUE as u32;
const FALSE: u32 = Tag::FALSE as u32;
const ID: u32 = Tag::ID as u32;
const DEFINE: u32 = Tag::DEFINE as u32;
const RETURN: u32 = Tag::RETURN as u32;
const VOID: u32 = Tag::VOID as u32;
const BASIC: u32 = Tag::BASIC as u32;

pub struct Env {
    table: HashMap<String, ExprNode>,
    prev: Box<Option<Env>>,
}

impl Env {
    fn empty() -> Box<Env> {
        Box::new(Env {
            table: HashMap::new(),
            prev: Box::new(None),
        })
    }

    fn new(prev: Box<Env>) -> Box<Env> {
        Box::new(Env {
            table: HashMap::new(),
            prev: Box::new(Some(*prev)),
        })
    }

    fn pop(&mut self) -> Result<Box<Env>, String> {
        let mut res = Box::new(None);
        swap(&mut self.prev, &mut res);
        match *res {
            Some(env) => Ok(Box::new(env)),
            None => Err(String::from("Popping empty environment")),
        }
    }

    fn put(&mut self, key: &str, value: ExprNode) -> Result<(), String> {
        if value.is_id() {
            self.table.insert(String::from(key), value);
            Ok(())
        } else {
            Err(String::from(
                "Only id expressions can be added to symbol table",
            ))
        }
    }

    fn get(&self, key: &str) -> Result<ExprNode, String> {
        if let Some(value) = self.table.get(key) {
            Ok(value.to_owned())
        } else if let Some(env) = self.prev.as_ref() {
            env.get(key)
        } else {
            Err(format!("Undeclared id {}", key))
        }
    }
}

pub struct Parser<T: std::io::Read> {
    lex: Lexer<T>,
    look: Token,
    top: Box<Env>,
    functions: Vec<StmtNode>,
    used: i64,
}

impl<T: std::io::Read> Parser<T> {
    pub fn new(lex: Lexer<T>) -> Result<Parser<T>, String> {
        let mut res = Parser {
            lex,
            look: Token::Eof,
            top: Env::empty(),
            functions: Vec::new(),
            used: 0,
        };
        res.next()?;
        Ok(res)
    }

    pub fn program(&mut self, s: &mut String) -> Result<(), String> {
        self.function()?;
        for function in &self.functions {
            let begin = new_label();
            let after = new_label();
            function.gen(s, begin, after)?;
        }
        /*let stmt = self.block()?;
        let begin = new_label();
        let after = new_label();
        emit_label(s, begin);
        stmt.gen(s, begin, after)?;
        emit_label(s, after);*/
        Ok(())
    }

    fn next(&mut self) -> Result<(), String> {
        self.look = match self.lex.scan() {
            Ok(token) => token,
            Err(err) => return Err(format!("{} near line {}", err, self.lex.line)),
        };
        Ok(())
    }

    fn match_token<U: Into<u32>>(&mut self, tag: U) -> Result<(), String> {
        if !self.look.match_tag(tag) {
            return Err(format!(
                "Syntax error near line {}, ({})",
                self.lex.line, self.look
            ));
        }
        self.next()
    }

    fn function(&mut self) -> Result<(), String> {
        // TODO: palaa loopin käsittelyyn
        while self.look.match_tag(DEFINE) {
            self.match_token(DEFINE)?;
            let tp = match self.look.tag() {
                BASIC => self.tp()?,
                VOID => {
                    let tp = Type::new(self.look.to_owned())?;
                    self.next()?;
                    tp
                }
                _ => return Err(format!("Syntax error near line {}", self.lex.line)),
            };
            let id_token = self.look.to_owned();
            self.match_token(Tag::ID)?;
            self.match_token(b'(')?;
            let mut params = Vec::<ExprNode>::new();
            let mut param_tps = Vec::<Type>::new();
            while self.look.match_tag(Tag::BASIC) {
                let tp = self.tp()?;
                param_tps.push(tp.to_owned());
                let param_token = self.look.to_owned();
                self.match_token(Tag::ID)?;
                let id = ExprNode::new_id(param_token, &tp, self.used as i32);
                params.push(id.to_owned());
                self.top.put(&id.to_string(), id)?;
                self.used += tp.width() as i64;
                if self.look.match_tag(b',') {
                    self.next()?;
                }
            }
            self.match_token(b')')?;
            let func_tp = Type::function(Box::new(tp), param_tps);
            let func_id = ExprNode::new_id(id_token, &func_tp, self.used as i32);
            self.top.put(&func_id.to_string(), func_id.to_owned())?;
            let stmt = self.block()?;
            let func_stmt = StmtNode::FuncDef(Box::new(func_id), stmt, self.used as i32);
            self.functions.push(func_stmt);
            self.used = 0;
        }
        Ok(())
    }

    fn block(&mut self) -> Result<Box<StmtNode>, String> {
        self.match_token(b'{')?;

        let mut empty = Env::empty();
        swap(&mut self.top, &mut empty);
        self.top = Env::new(empty);

        self.decls()?;
        let stmts = self.stmts()?;
        self.match_token(b'}')?;

        self.top = self.top.pop()?;
        Ok(stmts)
    }

    fn decls(&mut self) -> Result<(), String> {
        while self.look.match_tag(Tag::BASIC) {
            let tp = self.tp()?;
            let token = self.look.to_owned();
            self.match_token(Tag::ID)?;
            self.match_token(b';')?;
            let id = ExprNode::new_id(token, &tp, self.used as i32);
            self.top.put(&id.to_string(), id)?;
            self.used += tp.width() as i64;
        }
        Ok(())
    }

    fn tp(&mut self) -> Result<Type, String> {
        let tp = Type::new(self.look.to_owned())?;
        self.match_token(Tag::BASIC)?;
        if !self.look.match_tag(b'[') {
            return Ok(tp);
        }
        self.dims(tp)
    }

    fn dims(&mut self, tp: Type) -> Result<Type, String> {
        self.match_token(b'[')?;
        let token = self.look.to_owned();
        self.match_token(Tag::NUM)?;
        let size = match token {
            Token::Num(val) => val,
            _ => return Err(format!("Syntax error near line {}", self.lex.line)),
        };
        self.match_token(b']')?;

        let mut of = tp.to_owned();
        if self.look.match_tag(b'[') {
            of = self.dims(tp)?;
        }
        Ok(Type::array(of, size as u32))
    }

    fn stmts(&mut self) -> Result<Box<StmtNode>, String> {
        if self.look.match_tag(b'}') {
            return Ok(StmtNode::box_null());
        }
        let head = self.stmt()?;
        let tail = self.stmts()?;
        Ok(StmtNode::box_seq(head, tail))
    }

    fn stmt(&mut self) -> Result<Box<StmtNode>, String> {
        match self.look.tag() {
            SEMICOLON => {
                self.next()?;
                Ok(StmtNode::box_null())
            }
            IF => {
                self.match_token(IF)?;
                self.match_token(b'(')?;
                let expr = self.boolean()?;
                self.match_token(b')')?;
                let body = self.stmt()?;
                if !self.look.match_tag(Tag::ELSE) {
                    let if_stmt = StmtNode::box_if(expr, body)?;
                    return Ok(if_stmt);
                }
                self.match_token(Tag::ELSE)?;
                let false_stmt = self.stmt()?;
                let else_stmt = StmtNode::box_else(expr, body, false_stmt)?;
                Ok(else_stmt)
            }
            WHILE => {
                self.match_token(WHILE)?;
                self.match_token(b'(')?;

                let expr = self.boolean()?;
                if *expr.tp() != Type::bool() {
                    return Err(String::from("Boolean required in while"));
                }

                self.match_token(b')')?;
                let body = self.stmt()?;
                let while_stmt = StmtNode::box_while(expr, body)?;
                Ok(while_stmt)
            }
            DO => {
                self.match_token(DO)?;
                let body = self.stmt()?;

                self.match_token(WHILE)?;
                self.match_token(b'(')?;
                let expr = self.boolean()?;
                if *expr.tp() != Type::bool() {
                    return Err(String::from("Boolean required in do"));
                }
                self.match_token(b')')?;
                self.match_token(b';')?;
                let do_stmt = StmtNode::box_do(expr, body)?;
                Ok(do_stmt)
            }
            BREAK => {
                self.match_token(BREAK)?;
                self.match_token(b';')?;
                let break_stmt = StmtNode::box_break();
                Ok(break_stmt)
            }
            RETURN => {
                self.match_token(RETURN)?;
                let reuturn_stmt = match self.look.tag() {
                    SEMICOLON => StmtNode::box_return(None),
                    _ => {
                        let expr = self.boolean()?;
                        StmtNode::box_return(Some(expr))
                    }
                };
                self.match_token(b';')?;
                Ok(reuturn_stmt)
            }
            OPEN_BR => self.block(),
            _ => self.assign(),
        }
    }

    /*

                   let tok = id.op().to_owned();
                   let tp = id.tp().to_owned();
                   let funccall = ExprNode::box_funccall(tok, &tp, self.args()?)?;
                   return Ok(funccall)
    * */
    fn assign(&mut self) -> Result<Box<StmtNode>, String> {
        let token = self.look.to_owned();
        self.match_token(Tag::ID)?;

        let id = self.top.get(&token.to_string())?;

        if self.look.match_tag(b'=') {
            self.next()?;
            let expr = self.boolean()?;
            let stmt = StmtNode::box_set(Box::new(id), expr)?;
            self.match_token(b';')?;
            return Ok(stmt);
        } else if self.look.match_tag(b'(') {
            let tok = id.op().to_owned();
            let tp = id.tp().to_owned();
            let funccall = ExprNode::box_funccall(tok, &tp, self.args()?)?;
            let stmt = StmtNode::box_funccall(funccall)?;
            self.match_token(b';')?;
            return Ok(stmt);
        }

        let access = self.offset(id)?;
        self.match_token(b'=')?;
        let expr = self.boolean()?;
        let stmt = StmtNode::box_setelem(access, expr)?;
        self.match_token(b';')?;
        Ok(stmt)
    }

    fn boolean(&mut self) -> Result<Box<ExprNode>, String> {
        let mut expr = self.join()?;
        while self.look.match_tag(Tag::OR) {
            self.next()?;
            let right = self.join()?;
            expr = ExprNode::box_or(Token::or(), expr, right)?;
        }
        Ok(expr)
    }

    fn join(&mut self) -> Result<Box<ExprNode>, String> {
        let mut expr = self.equality()?;
        while self.look.match_tag(Tag::AND) {
            self.next()?;
            let right = self.equality()?;
            expr = ExprNode::box_and(Token::and(), expr, right)?;
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Box<ExprNode>, String> {
        let mut expr = self.relation()?;
        while self.look.match_tag(Tag::EQ) || self.look.match_tag(Tag::NE) {
            let token = self.look.to_owned();
            self.next()?;
            let right = self.relation()?;
            expr = ExprNode::box_rel(token, expr, right)?;
        }
        Ok(expr)
    }

    fn relation(&mut self) -> Result<Box<ExprNode>, String> {
        let expr = self.expr()?;
        let token = self.look.to_owned();

        match token.tag() {
            LT | GT | LE | GE => {
                self.next()?;
                let right = self.expr()?;
                let rel = ExprNode::box_rel(token, expr, right)?;
                Ok(rel)
            }
            _ => Ok(expr),
        }
    }

    fn expr(&mut self) -> Result<Box<ExprNode>, String> {
        let mut expr = self.term()?;

        while self.look.match_tag(b'+') || self.look.match_tag(b'-') {
            let token = self.look.to_owned();
            self.next()?;
            let right = self.term()?;
            expr = ExprNode::box_arith(token, expr, right)?;
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Box<ExprNode>, String> {
        let mut expr = self.unary()?;
        while self.look.match_tag(b'*') || self.look.match_tag(b'/') {
            let token = self.look.to_owned();
            self.next()?;
            let right = self.unary()?;
            expr = ExprNode::box_arith(token, expr, right)?;
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Box<ExprNode>, String> {
        match self.look.tag() {
            MINUS => {
                self.next()?;
                let mut expr = self.unary()?;
                expr = ExprNode::box_unary(Token::minus(), expr)?;
                Ok(expr)
            }
            EXCL => {
                let token = self.look.to_owned();
                self.next()?;
                let mut expr = self.unary()?;
                expr = ExprNode::box_not(token, expr)?;
                Ok(expr)
            }
            _ => self.factor(),
        }
    }

    fn factor(&mut self) -> Result<Box<ExprNode>, String> {
        match self.look.tag() {
            OPAREN => {
                self.next()?;
                let expr = self.boolean()?;
                self.match_token(b')')?;
                Ok(expr)
            }
            NUM | REAL => {
                let expr = ExprNode::box_constant(self.look.to_owned())?;
                self.next()?;
                Ok(expr)
            }
            TRUE => {
                let expr = ExprNode::box_true();
                self.next()?;
                Ok(expr)
            }
            FALSE => {
                let expr = ExprNode::box_false();
                self.next()?;
                Ok(expr)
            }
            ID => {
                let id = self.top.get(&format!("{}", self.look))?;
                self.next()?;
                if self.look.match_tag(b'[') {
                    let expr = self.offset(id)?;
                    return Ok(expr);
                } else if self.look.match_tag(b'(') {
                    let tok = id.op().to_owned();
                    let tp = id.tp().to_owned();
                    let funccall = ExprNode::box_funccall(tok, &tp, self.args()?)?;
                    return Ok(funccall);
                }
                Ok(Box::new(id))
            }
            _ => Err(format!("Syntax Error near line {}", self.lex.line)),
        }
    }

    fn args(&mut self) -> Result<Vec<Box<ExprNode>>, String> {
        let mut params = Vec::<Box<ExprNode>>::new();
        self.match_token(b'(')?;
        while !self.look.match_tag(b')') {
            let expr = self.boolean()?;
            params.push(expr);
            if self.look.match_tag(b',') {
                self.match_token(b',')?;
            }
        }
        self.match_token(b')')?;
        Ok(params)
    }

    fn offset(&mut self, id: ExprNode) -> Result<Box<ExprNode>, String> {
        let mut tp = id.tp().to_owned();

        self.match_token(b'[')?;
        let index = self.boolean()?;
        self.match_token(b']')?;

        match tp {
            Type::Array {
                of,
                tag: _,
                length: _,
            } => tp = *of,
            _ => return Err(String::from("String error")),
        };

        let width = ExprNode::box_constant(Token::Num(tp.width() as i64))?;
        let t1 = ExprNode::box_arith(Token::Token(b'*'), index, width)?;

        let mut loc = t1;
        while self.look.match_tag(b'[') {
            self.match_token(b'[')?;
            let index = self.boolean()?;
            self.match_token(b']')?;

            match tp {
                Type::Array {
                    of,
                    tag: _,
                    length: _,
                } => tp = *of,
                _ => return Err(String::from("String error")),
            };
            let width = ExprNode::box_constant(Token::Num(tp.width() as i64))?;
            let t1 = ExprNode::box_arith(Token::Token(b'*'), index, width)?;

            let t2 = ExprNode::box_arith(Token::Token(b'*'), loc, t1)?;
            loc = t2;
        }

        ExprNode::box_access(Box::new(id), loc, tp)
    }
}
