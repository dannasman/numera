use super::lexer::{Lexer, Token};
use std::collections::HashMap;

enum SymVal {
    Val(f64),
    Str(String),
}

struct Symbol {
    scope: u32,
    symbol: SymVal,
}

struct Parser {
    lexer: Lexer,
    symbol_table: HashMap<String, Symbol>,
    current_scope: u32,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        return Parser {
            lexer: lexer,
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

    fn bool(&mut self) {
        //self.join;
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::Or(_)) = t {
            //katso myöhemmin sisältö tähän, jotain join-funktioon liittyvää
            t = self.lexer.tokens.pop_back();
        }
    }

    fn join(&mut self) {
        //self.equality;
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::And(_)) = t {
            //katso myöhemmin sisältö tähän, jotain join-funktioon liittyvää
            t = self.lexer.tokens.pop_back();
        }
    }

    fn equality(&mut self) {
        //self.rel;
        let mut t = self.lexer.tokens.pop_back();
        while let Some(Token::Eql(_)) | Some(Token::Ne(_)) = t {
            //katso myöhemmin sisältö tähän, jotain rel-funktioon liittyvää
            t = self.lexer.tokens.pop_back();
        }
    }

    fn rel(&mut self) {
        //self.expr();
        let t = self.lexer.tokens.pop_back();
        if let Some(Token::Lt(_)) | Some(Token::Gt(_)) | Some(Token::Le(_)) | Some(Token::Ge(_)) = t
        {
            //luo uusi Rel-instanssi ja palauta
            return;
        } else {
            return;
        }
    }

    fn expr(&mut self) {
        //self.term();
        let mut t = self.lexer.tokens.pop_back();
        while Self::match_option_token(t.clone(), "+".to_string())
            || Self::match_option_token(t.clone(), "-".to_string())
        {
            //luo uusi Arith-instanssi ja kutsu self.term()
            t = self.lexer.tokens.pop_back();
        }
        return;
    }

    fn term(&mut self) {
        //self.unary();
        let mut t = self.lexer.tokens.pop_back();
        while Self::match_option_token(t.clone(), "*".to_string())
            || Self::match_option_token(t.clone(), "/".to_string())
        {
            //luo uusi Arith-instanssi ja kutsu unary
            t = self.lexer.tokens.pop_back();
        }
        return;
    }

    fn unary(&mut self) {
        let peek = self.lexer.tokens.back();
        match peek {
            Some(t) => {
                if Self::match_token(t.clone(), "-".to_string()) {
                    self.lexer.tokens.pop_back();
                    //luo uusi Unary-instanssi ja kutsu unary
                } else if Self::match_token(t.clone(), "!".to_string()) {
                    self.lexer.tokens.pop_back();
                    //luo uusi Not-instanssi ja kutsu unary
                } else {
                    //kutsu factor
                }
            }
            None => {
                return;
            }
        }
        return;
    }

    fn factor(&mut self) {
        let peek = self.lexer.tokens.back();
        match peek {
            Some(t) => {
                match t.clone() {
                    Token::Num(_) => {
                        //uusi Constant-instanssi
                        self.lexer.tokens.pop_back();
                    }
                    Token::True(_) => {
                        //true Constant-instanssi
                        self.lexer.tokens.pop_back();
                    }
                    Token::False(_) => {
                        //false Constant-instanssi
                        self.lexer.tokens.pop_back();
                    }
                    Token::Id(s) => {
                        self.lexer.tokens.pop_back();
                        if s == "(".to_string() {
                            //self.bool
                            let next = self.lexer.tokens.pop_back();
                            if !Self::match_option_token(next, ")".to_string()) {
                                println!("token did not match )");
                            }
                        } else {
                            let id = self.symbol_table.get(&s.to_string());
                            match id {
                                Some(symbol) => {
                                    if symbol.scope > self.current_scope {
                                        println!("{} out of scope", s.to_string());
                                        return;
                                    }
                                    //palauta id
                                    return;
                                }
                                None => {
                                    println!("{} undeclared", s.to_string());
                                    return;
                                }
                            }
                        }
                    }
                    _ => {
                        return;
                    }
                }
            }
            None => {
                return;
            }
        }
    }
}
