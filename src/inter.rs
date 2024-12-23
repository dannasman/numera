use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use super::tac::*;
use super::tokens::{Tag, Token};

thread_local! {
    static LABEL_COUNTER: RefCell<i64> = RefCell::new(1);
    static TEMP_COUNTER: RefCell<i32> = RefCell::new(1);
}

pub fn new_label() -> i64 {
    let mut res = 0;
    LABEL_COUNTER.with(|counter| {
        res = *counter.borrow();
        *counter.borrow_mut() = res + 1;
    });
    res
}

pub fn reset_labels() {
    LABEL_COUNTER.with(|counter| {
        *counter.borrow_mut() = 1;
    });
}

pub fn emit_label(ir: &mut TACIr, i: i64) {
    let tac = TACInstruction {
        op: TACOperator::Null,
        arg1: TACOperand::Null,
        arg2: TACOperand::Null,
        res: TACOperand::Label(i),
    };
    ir.push(tac)
}

pub fn emit_function(ir: &mut TACIr, id: String) {
    let tac = TACInstruction {
        op: TACOperator::Null,
        arg1: TACOperand::Null,
        arg2: TACOperand::Null,
        res: TACOperand::Function(id),
    };
    ir.push(tac)
}

pub fn emit(ir: &mut TACIr, tac: TACInstruction) {
    ir.push(tac);
}

pub fn emit_jumps(ir: &mut TACIr, test: TACOperand, t: i64, f: i64) {
    let tac_t = TACInstruction {
        op: TACOperator::If,
        arg1: test.to_owned(),
        arg2: TACOperand::Null,
        res: TACOperand::Label(t),
    };
    if t != 0 && f != 0 {
        emit(ir, tac_t);
        let tac_f = TACInstruction {
            op: TACOperator::Goto,
            arg1: TACOperand::Null,
            arg2: TACOperand::Null,
            res: TACOperand::Label(f),
        };
        emit(ir, tac_f);
    } else if t != 0 {
        emit(ir, tac_t);
    } else if f != 0 {
        let tac_f = TACInstruction {
            op: TACOperator::Iff,
            arg1: test,
            arg2: TACOperand::Null,
            res: TACOperand::Label(f),
        };
        emit(ir, tac_f);
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Basic {
        lexeme: String,
        tag: Tag,
        width: u8,
    },
    Array {
        of: Box<Type>,
        tag: Tag,
        length: u32,
    },
    Function {
        return_tp: Box<Type>,
        param_tps: Vec<Type>,
    },
    Void,
}

impl Type {
    pub fn new(token: Token) -> Result<Type, String> {
        match token {
            Token::BasicType(lexeme, tag, width) => Ok(Type::Basic { lexeme, tag, width }),
            Token::Array(of, length) => {
                let o = Type::new(*of)?;
                Ok(Type::Array {
                    of: Box::new(o),
                    tag: Tag::INDEX,
                    length,
                })
            }
            Token::Function(return_tp, param_tps) => {
                let r = Type::new(*return_tp)?;
                let mut p = Vec::new();
                for tok in param_tps {
                    let tp = Type::new(tok)?;
                    p.push(tp);
                }
                Ok(Type::Function {
                    return_tp: Box::new(r),
                    param_tps: p,
                })
            }
            Token::Void => Ok(Type::Void),
            _ => Err(format!("Can not convert token {} to Type", token)),
        }
    }

    pub fn function(return_tp: Box<Type>, param_tps: Vec<Type>) -> Type {
        Type::Function {
            return_tp,
            param_tps,
        }
    }

    pub fn array(of: Type, length: u32) -> Type {
        Type::Array {
            of: Box::new(of),
            tag: Tag::INDEX,
            length,
        }
    }

    pub fn int() -> Type {
        Type::new(Token::int()).unwrap()
    }

    pub fn float() -> Type {
        Type::new(Token::float()).unwrap()
    }

    pub fn char() -> Type {
        Type::new(Token::char()).unwrap()
    }

    pub fn bool() -> Type {
        Type::new(Token::bool()).unwrap()
    }

    pub fn void() -> Type {
        Type::new(Token::void()).unwrap()
    }

    pub fn token(&self) -> Token {
        match &self {
            Type::Basic { lexeme, tag, width } => Token::BasicType(lexeme.to_owned(), *tag, *width),
            Type::Array { of, tag: _, length } => Token::Array(Box::new(of.token()), *length),
            Type::Function {
                return_tp,
                param_tps,
            } => Token::Function(
                Box::new(return_tp.token()),
                param_tps.into_iter().map(|tp| tp.token()).collect(),
            ),
            Type::Void => Token::Void,
        }
    }

    pub fn tag(&self) -> Tag {
        match &self {
            Type::Basic {
                lexeme: _,
                tag,
                width: _,
            } => *tag,
            Type::Array {
                of: _,
                tag,
                length: _,
            } => *tag,
            Type::Function {
                return_tp: _,
                param_tps: _,
            } => Tag::FUNCTION,
            Type::Void => Tag::VOID,
        }
    }

    pub fn width(&self) -> u32 {
        match self {
            Type::Basic {
                lexeme: _,
                tag: _,
                width,
            } => *width as u32,
            Type::Array { of, tag: _, length } => of.width() * length,
            Type::Function {
                return_tp: _,
                param_tps,
            } => {
                /*let mut width = return_tp.width();
                width += param_tps
                    .into_iter()
                    .map(|tp| tp.width())
                    .fold(0, |w, e| w + e);
                width*/
                param_tps
                    .into_iter()
                    .map(|tp| tp.width())
                    .fold(0, |w, e| w + e)
            }
            Type::Void => 0,
        }
    }

    pub fn is_numeric(&self) -> bool {
        if let Type::Basic {
            lexeme,
            tag: _,
            width: _,
        } = self
        {
            match lexeme.as_str() {
                "int" | "float" | "char" => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn is_function(&self) -> bool {
        if let Type::Function {
            return_tp: _,
            param_tps: _,
        } = self
        {
            true
        } else {
            false
        }
    }

    pub fn max_type(left: &Type, right: &Type) -> Option<Type> {
        if !left.is_numeric() || !right.is_numeric() {
            None
        } else if *left == Type::float() || *right == Type::float() {
            Some(Type::float())
        } else if *left == Type::int() || *right == Type::int() {
            Some(Type::int())
        } else {
            Some(Type::char())
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Basic {
                lexeme: lexeme1,
                tag: tag1,
                width: width1,
            } => match other {
                Type::Basic {
                    lexeme: lexeme2,
                    tag: tag2,
                    width: width2,
                } => lexeme1 == lexeme2 && tag1 == tag2 && width1 == width2,
                _ => false,
            },
            Type::Array {
                of: of1,
                tag: tag1,
                length: length1,
            } => match other {
                Type::Array {
                    of: of2,
                    tag: tag2,
                    length: length2,
                } => of1 == of2 && tag1 == tag2 && length1 == length2,
                _ => false,
            },
            Type::Function {
                return_tp: rtp1,
                param_tps: ptps1,
            } => match other {
                Type::Function {
                    return_tp: rtp2,
                    param_tps: ptps2,
                } => {
                    rtp1 == rtp2
                        && ptps1.len() == ptps2.len()
                        && ptps1
                            .into_iter()
                            .zip(ptps2.into_iter())
                            .map(|(tp1, tp2)| tp1 == tp2)
                            .fold(true, |b, e| b && e)
                }
                _ => false,
            },
            Type::Void => match other {
                Type::Void => true,
                _ => false,
            },
        }
    }
}

impl Eq for Type {}

pub enum ExprNode {
    Constant(Token, Type),
    Id(Token, Type, i32),
    Temp(Token, Type, i32),
    Arith(Token, Type, Box<ExprNode>, Box<ExprNode>),
    Unary(Token, Type, Box<ExprNode>),
    Access(Token, Box<ExprNode>, Box<ExprNode>, Type),
    Rel(Token, Type, Box<ExprNode>, Box<ExprNode>),
    Not(Token, Type, Box<ExprNode>),
    Or(Token, Type, Box<ExprNode>, Box<ExprNode>),
    And(Token, Type, Box<ExprNode>, Box<ExprNode>),
    FuncCall(Token, Type, Vec<Box<ExprNode>>),
}

impl ExprNode {
    pub fn op(&self) -> &Token {
        match self {
            ExprNode::Constant(op, _) => op,
            ExprNode::Id(op, _, _) => op,
            ExprNode::Temp(op, _, _) => op,
            ExprNode::Arith(op, _, _, _) => op,
            ExprNode::Unary(op, _, _) => op,
            ExprNode::Access(op, _, _, _) => op,
            ExprNode::Rel(op, _, _, _) => op,
            ExprNode::Not(op, _, _) => op,
            ExprNode::Or(op, _, _, _) => op,
            ExprNode::And(op, _, _, _) => op,
            ExprNode::FuncCall(op, _, _) => op,
        }
    }

    pub fn tp(&self) -> &Type {
        match self {
            ExprNode::Constant(_, tp) => tp,
            ExprNode::Id(_, tp, _) => tp,
            ExprNode::Temp(_, tp, _) => tp,
            ExprNode::Arith(_, tp, _, _) => tp,
            ExprNode::Unary(_, tp, _) => tp,
            ExprNode::Access(_, _, _, tp) => tp,
            ExprNode::Rel(_, tp, _, _) => tp,
            ExprNode::Not(_, tp, _) => tp,
            ExprNode::Or(_, tp, _, _) => tp,
            ExprNode::And(_, tp, _, _) => tp,
            ExprNode::FuncCall(_, tp, _) => tp,
        }
    }

    pub fn return_tp(&self) -> Result<Type, String> {
        match self {
            ExprNode::Id(_, tp, _) => match tp {
                Type::Function {
                    return_tp,
                    param_tps: _,
                } => Ok(*return_tp.to_owned()),
                _ => return Err(String::from("Type of id not Function")),
            },
            _ => return Err(String::from("Not an Id expression")),
        }
    }

    pub fn is_id(&self) -> bool {
        if let ExprNode::Id(_, _, _) = self {
            true
        } else {
            false
        }
    }

    pub fn return_offset(&self) -> Result<i32, String> {
        match self {
            ExprNode::Id(_, _, offset) => Ok(*offset),
            _ => Err(String::from("Failed to return offset")),
        }
    }

    pub fn expr_to_tac(&self) -> Result<TACOperand, String> {
        match self {
            ExprNode::Constant(op, tp) => Ok(TACOperand::Const(op.to_string(), tp.to_owned())),
            ExprNode::Id(op, tp, offset) => {
                Ok(TACOperand::Var(op.to_string(), tp.to_owned(), *offset))
            }
            ExprNode::Temp(_, tp, num) => Ok(TACOperand::Temp(format!("t{}", num), tp.to_owned())),
            ExprNode::Access(_, array, index, tp) => Ok(TACOperand::Array(
                array.to_string(),
                index.to_string(),
                tp.to_owned(),
                array.return_offset()?,
            )),
            _ => Err(format!(
                "Failed to generate TAC from given expression {}",
                self
            )),
        }
    }

    pub fn operator_to_tac(&self) -> Result<TACOperator, String> {
        match self {
            ExprNode::Arith(op, _, _, _) => {
                if op.tag() == Token::Token('+' as u8).tag() {
                    Ok(TACOperator::Add)
                } else if op.tag() == Token::Token('-' as u8).tag() {
                    Ok(TACOperator::Sub)
                } else if op.tag() == Token::Token('*' as u8).tag() {
                    Ok(TACOperator::Mul)
                } else if op.tag() == Token::Token('/' as u8).tag() {
                    Ok(TACOperator::Div)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            ExprNode::Unary(op, _, _) => {
                if op.tag() == Tag::MINUS.into() {
                    Ok(TACOperator::Sub)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            ExprNode::Rel(op, _, _, _) => {
                if op.tag() == Token::Token('<' as u8).tag() {
                    Ok(TACOperator::Lt)
                } else if op.tag() == Token::Token('>' as u8).tag() {
                    Ok(TACOperator::Gt)
                } else if op.tag() == Tag::LE.into() {
                    Ok(TACOperator::Le)
                } else if op.tag() == Tag::GE.into() {
                    Ok(TACOperator::Ge)
                } else if op.tag() == Tag::EQ.into() {
                    Ok(TACOperator::Eql)
                } else if op.tag() == Tag::NE.into() {
                    Ok(TACOperator::Ne)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            ExprNode::Not(op, _, _) => {
                if op.tag() == Token::Token('-' as u8).tag() {
                    Ok(TACOperator::Not)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            ExprNode::Or(op, _, _, _) => {
                if op.tag() == Tag::OR.into() {
                    Ok(TACOperator::Or)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            ExprNode::And(op, _, _, _) => {
                if op.tag() == Tag::AND.into() {
                    Ok(TACOperator::And)
                } else {
                    Err(String::from("Failed to generate TAC from given operator"))
                }
            }
            _ => Err(String::from("Failed to generate TAC from given operator")),
        }
    }

    pub fn new_true() -> ExprNode {
        ExprNode::Constant(Token::ttrue(), Type::bool())
    }

    pub fn box_true() -> Box<ExprNode> {
        Box::new(ExprNode::new_true())
    }

    pub fn new_false() -> ExprNode {
        ExprNode::Constant(Token::tfalse(), Type::bool())
    }

    pub fn box_false() -> Box<ExprNode> {
        Box::new(ExprNode::new_false())
    }

    pub fn new_constant(token: Token) -> Result<ExprNode, String> {
        match token {
            Token::Num(_) => Ok(ExprNode::Constant(token, Type::int())),
            Token::Real(_) => Ok(ExprNode::Constant(token, Type::float())),
            t => Err(format!("Invalid parameter: {}", t)),
        }
    }

    pub fn box_constant(token: Token) -> Result<Box<ExprNode>, String> {
        let constant = ExprNode::new_constant(token)?;
        Ok(Box::new(constant))
    }

    pub fn new_id(id: Token, tp: &Type, offset: i32) -> ExprNode {
        ExprNode::Id(id, tp.to_owned(), offset)
    }

    pub fn box_id(id: Token, tp: &Type, offset: i32) -> Box<ExprNode> {
        Box::new(ExprNode::new_id(id, tp, offset))
    }

    pub fn new_temp(tp: &Type) -> ExprNode {
        let mut num: i32 = 0;
        TEMP_COUNTER.with(|c| {
            num = *c.borrow();
            *c.borrow_mut() = num + 1;
        });
        ExprNode::Temp(Token::temp(), tp.to_owned(), num)
    }

    pub fn box_temp(tp: &Type) -> Box<ExprNode> {
        Box::new(ExprNode::new_temp(tp))
    }

    pub fn new_arith(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let tp = match Type::max_type(left.tp(), right.tp()) {
            Some(t) => t,
            None => return Err(String::from("Type error")),
        };

        Ok(ExprNode::Arith(token, tp, left, right))
    }

    pub fn box_arith(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let arith = ExprNode::new_arith(token, left, right)?;
        Ok(Box::new(arith))
    }

    pub fn new_unary(token: Token, expr: Box<ExprNode>) -> Result<ExprNode, String> {
        let tp = match Type::max_type(&Type::int(), expr.tp()) {
            Some(t) => t,
            None => return Err(String::from("Type error")),
        };

        Ok(ExprNode::Unary(token, tp, expr))
    }

    pub fn box_unary(token: Token, expr: Box<ExprNode>) -> Result<Box<ExprNode>, String> {
        let unary = ExprNode::new_unary(token, expr)?;
        Ok(Box::new(unary))
    }

    pub fn new_access(
        array: Box<ExprNode>,
        index: Box<ExprNode>,
        tp: Type,
    ) -> Result<ExprNode, String> {
        if !array.is_id() {
            Err(String::from("Type error"))
        } else {
            Ok(ExprNode::Access(Token::access(), array, index, tp))
        }
    }

    pub fn box_access(
        array: Box<ExprNode>,
        index: Box<ExprNode>,
        tp: Type,
    ) -> Result<Box<ExprNode>, String> {
        let access = ExprNode::new_access(array, index, tp)?;
        Ok(Box::new(access))
    }

    pub fn new_rel(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        if left.tp() != right.tp() {
            Err(String::from("Type error"))
        } else if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = left.tp()
        {
            Err(String::from("type error"))
        } else if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = right.tp()
        {
            Err(String::from("type error"))
        } else {
            Ok(ExprNode::Rel(token, Type::bool(), left, right))
        }
    }

    pub fn box_rel(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let rel = ExprNode::new_rel(token, left, right)?;
        Ok(Box::new(rel))
    }

    pub fn new_not(token: Token, expr: Box<ExprNode>) -> Result<ExprNode, String> {
        if expr.tp() != &Type::bool() {
            return Err(String::from("Type error"));
        }
        if token.tag() != Token::Token('!' as u8).tag() {
            return Err(String::from("Lexer error"));
        }
        Ok(ExprNode::Not(token, Type::bool(), expr))
    }

    pub fn box_not(token: Token, expr: Box<ExprNode>) -> Result<Box<ExprNode>, String> {
        let not = ExprNode::new_not(token, expr)?;
        Ok(Box::new(not))
    }

    pub fn new_or(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let bt = Type::bool();
        if left.tp() != &bt || right.tp() != &bt {
            return Err(String::from("Type Error"));
        }
        Ok(ExprNode::Or(token, Type::bool(), left, right))
    }

    pub fn box_or(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let or = ExprNode::new_or(token, left, right)?;
        Ok(Box::new(or))
    }

    pub fn new_and(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<ExprNode, String> {
        let bt = Type::bool();
        if left.tp() != &bt || right.tp() != &bt {
            return Err(String::from("Type Error"));
        }
        Ok(ExprNode::And(token, Type::bool(), left, right))
    }

    pub fn box_and(
        token: Token,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    ) -> Result<Box<ExprNode>, String> {
        let and = ExprNode::new_and(token, left, right)?;
        Ok(Box::new(and))
    }

    pub fn new_funccall(
        id: Token,
        tp: &Type,
        params: Vec<Box<ExprNode>>,
    ) -> Result<ExprNode, String> {
        match tp {
            Type::Function {
                return_tp,
                param_tps,
            } => {
                let matching_tps = params
                    .to_owned()
                    .into_iter()
                    .map(|p| p.tp().to_owned())
                    .zip(param_tps.into_iter())
                    .map(|(tp1, tp2)| match Type::max_type(&tp1, tp2) {
                        Some(tp) => tp == *tp2,
                        None => false,
                    })
                    .fold(true, |b, e| b && e);
                if !matching_tps || param_tps.len() != params.len() {
                    return Err(String::from("Type Error"));
                }
                let funccall_tp = *return_tp.to_owned();
                Ok(ExprNode::FuncCall(id, funccall_tp, params))
            }
            _ => Err(String::from("Type Error")),
        }
    }

    pub fn box_funccall(
        id: Token,
        tp: &Type,
        params: Vec<Box<ExprNode>>,
    ) -> Result<Box<ExprNode>, String> {
        let funccall = ExprNode::new_funccall(id, tp, params)?;
        Ok(Box::new(funccall))
    }

    pub fn void_funccall(&self, ir: &mut TACIr) -> Result<(), String> {
        if let ExprNode::FuncCall(id, tp, params) = self {
            if *tp != Type::Void {
                return Err(String::from("Type Error"));
            }
            let mut count = 0;
            let mut params_reduced = Vec::<Box<ExprNode>>::new();
            for param in params.into_iter().rev() {
                let expr = param.reduce(ir)?;
                params_reduced.push(expr);
                count += 1;
            }
            for param in params_reduced {
                let tac_param = TACInstruction {
                    op: TACOperator::Param,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: param.expr_to_tac()?,
                };
                emit(ir, tac_param);
            }
            let tac_call = TACInstruction {
                op: TACOperator::Call(count),
                arg1: TACOperand::Null,
                arg2: TACOperand::Null,
                res: TACOperand::Function(id.to_string()),
            };
            emit(ir, tac_call);
            Ok(())
        } else {
            Err(String::from("Expression is not a function call"))
        }
    }

    pub fn jumping(&self, ir: &mut TACIr, t: i64, f: i64) -> Result<(), String> {
        match self {
            ExprNode::Rel(_, tp, left, right) => {
                let lr = left.reduce(ir)?;
                let rr = right.reduce(ir)?;
                let tmp = ExprNode::new_temp(tp);
                let tac = TACInstruction {
                    op: self.operator_to_tac()?,
                    arg1: lr.expr_to_tac()?,
                    arg2: rr.expr_to_tac()?,
                    res: tmp.expr_to_tac()?,
                };
                emit(ir, tac);
                emit_jumps(ir, tmp.expr_to_tac()?, t, f);
            }
            ExprNode::Not(_, _, expr) => {
                expr.jumping(ir, f, t)?;
            }
            ExprNode::Or(_, _, left, right) => {
                let mut label = t;
                if t == 0 {
                    label = new_label();
                }
                left.jumping(ir, label, 0)?;
                right.jumping(ir, t, f)?;
                if t == 0 {
                    emit_label(ir, label);
                }
            }
            ExprNode::And(_, _, left, right) => {
                let mut label = f;
                if f == 0 {
                    label = new_label();
                }
                left.jumping(ir, 0, label)?;
                right.jumping(ir, t, f)?;

                if f == 0 {
                    emit_label(ir, label);
                }
            }
            _ => emit_jumps(ir, self.expr_to_tac()?, t, f),
        }
        Ok(())
    }

    pub fn gen(&self, ir: &mut TACIr) -> Result<Box<ExprNode>, String> {
        match self {
            ExprNode::Arith(op, _, left, right) => {
                let lr = left.reduce(ir)?;
                let rr = right.reduce(ir)?;
                match ExprNode::new_arith(op.to_owned(), lr, rr) {
                    Ok(arith) => Ok(Box::new(arith)),
                    Err(s) => Err(s),
                }
            }
            ExprNode::Unary(op, _, expr) => {
                let rexpr = expr.reduce(ir)?;
                let unary = ExprNode::new_unary(op.to_owned(), rexpr)?;
                Ok(Box::new(unary))
            }
            ExprNode::Access(_, array, index, tp) => {
                let i = index.reduce(ir)?;
                Ok(Box::new(ExprNode::Access(
                    Token::access(),
                    array.to_owned(),
                    i,
                    tp.to_owned(),
                )))
            }
            ExprNode::Rel(_, tp, _, _)
            | ExprNode::Not(_, tp, _)
            | ExprNode::Or(_, tp, _, _)
            | ExprNode::And(_, tp, _, _) => {
                let f = new_label();
                let a = new_label();
                let tmp = ExprNode::new_temp(tp);
                self.jumping(ir, 0, f)?;
                let tac_true = TACInstruction {
                    op: TACOperator::Assign,
                    arg1: TACOperand::True,
                    arg2: TACOperand::Null,
                    res: tmp.expr_to_tac()?,
                };
                let tac_goto = TACInstruction {
                    op: TACOperator::Goto,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Label(a),
                };
                let tac_false = TACInstruction {
                    op: TACOperator::Assign,
                    arg1: TACOperand::False,
                    arg2: TACOperand::Null,
                    res: tmp.expr_to_tac()?,
                };
                emit(ir, tac_true);
                emit(ir, tac_goto);
                emit_label(ir, f);
                emit(ir, tac_false);
                emit_label(ir, a);
                Ok(Box::new(tmp))
            }
            ExprNode::FuncCall(_, _, _) => self.reduce(ir),
            _ => Ok(self.box_clone()),
        }
    }

    pub fn reduce(&self, ir: &mut TACIr) -> Result<Box<ExprNode>, String> {
        match self {
            ExprNode::Arith(_, tp, _, _) | ExprNode::Unary(_, tp, _) => {
                let x = self.gen(ir)?;
                let tmp = ExprNode::new_temp(tp);
                let op = x.operator_to_tac()?;
                let tac = match *x {
                    ExprNode::Arith(_, _, left, right) => TACInstruction {
                        op,
                        arg1: left.expr_to_tac()?,
                        arg2: right.expr_to_tac()?,
                        res: tmp.expr_to_tac()?,
                    },
                    ExprNode::Unary(_, _, expr) => TACInstruction {
                        op,
                        arg1: expr.expr_to_tac()?,
                        arg2: TACOperand::Null,
                        res: tmp.expr_to_tac()?,
                    },
                    _ => return Err(format!("Failed to generate TAC from {}", x)),
                };
                emit(ir, tac);
                Ok(Box::new(tmp))
            }
            ExprNode::Access(_, _, _, tp) => {
                let x = self.gen(ir)?;
                let tmp = ExprNode::new_temp(tp);
                let tac = TACInstruction {
                    op: TACOperator::Assign,
                    arg1: x.expr_to_tac()?,
                    arg2: TACOperand::Null,
                    res: tmp.expr_to_tac()?,
                };
                emit(ir, tac);
                Ok(Box::new(tmp))
            }
            ExprNode::FuncCall(id, tp, params) => {
                let mut count = 0;
                let mut params_reduced = Vec::<Box<ExprNode>>::new();
                for param in params.into_iter().rev() {
                    let expr = param.reduce(ir)?;
                    params_reduced.push(expr);
                    count += 1;
                }
                for param in params_reduced {
                    let tac_param = TACInstruction {
                        op: TACOperator::Param,
                        arg1: TACOperand::Null,
                        arg2: TACOperand::Null,
                        res: param.expr_to_tac()?,
                    };
                    emit(ir, tac_param);
                }
                let tmp = ExprNode::new_temp(tp);
                let tac_call = TACInstruction {
                    op: TACOperator::Call(count),
                    arg1: TACOperand::Function(id.to_string()),
                    arg2: TACOperand::Null,
                    res: tmp.expr_to_tac()?,
                };
                emit(ir, tac_call);
                Ok(Box::new(tmp))
            }
            _ => Ok(self.box_clone()),
        }
    }

    pub fn box_clone(&self) -> Box<ExprNode> {
        Box::new(self.clone())
    }
}

impl Clone for ExprNode {
    fn clone(&self) -> Self {
        match self {
            ExprNode::Constant(op, tp) => ExprNode::Constant(op.clone(), tp.clone()),
            ExprNode::Id(id, tp, offset) => ExprNode::Id(id.clone(), tp.clone(), offset.clone()),
            ExprNode::Temp(op, tp, num) => ExprNode::Temp(op.clone(), tp.clone(), num.clone()),
            ExprNode::Arith(op, tp, left, right) => {
                ExprNode::Arith(op.clone(), tp.clone(), left.box_clone(), right.box_clone())
            }
            ExprNode::Unary(op, tp, expr) => {
                ExprNode::Unary(op.clone(), tp.clone(), expr.box_clone())
            }
            ExprNode::Access(op, array, index, tp) => {
                ExprNode::Access(op.clone(), array.box_clone(), index.box_clone(), tp.clone())
            }
            ExprNode::Rel(op, _, left, right) => ExprNode::Rel(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
            ExprNode::Not(op, _, expr) => ExprNode::Not(op.clone(), Type::bool(), expr.box_clone()),
            ExprNode::Or(op, _, left, right) => ExprNode::Or(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
            ExprNode::And(op, _, left, right) => ExprNode::And(
                op.clone(),
                Type::bool(),
                left.box_clone(),
                right.box_clone(),
            ),
            ExprNode::FuncCall(id, tp, params) => {
                ExprNode::FuncCall(id.clone(), tp.clone(), params.clone())
            }
        }
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprNode::Constant(token, _) => write!(f, "{}", token),
            ExprNode::Id(id, _, _) => write!(f, "{}", id),
            ExprNode::Temp(_, _, num) => write!(f, "t{}", num),
            ExprNode::Arith(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::Unary(op, _, expr) => write!(f, "{} {}", op, expr),
            ExprNode::Access(_, array, index, _) => write!(f, "{} [ {} ]", array, index),
            ExprNode::Rel(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::Not(op, _, expr) => write!(f, "{} {}", op, expr),
            ExprNode::Or(op, _, left, right) => write!(f, "{} {} {}", left, op, right),
            ExprNode::And(op, _, left, right) => write!(f, "{}, {}, {}", left, op, right),
            _ => write!(f, ""),
        }
    }
}

pub enum StmtNode {
    Null,
    Set(Box<ExprNode>, Box<ExprNode>),
    SetElem(Box<ExprNode>, Box<ExprNode>, Box<ExprNode>),
    Seq(Box<StmtNode>, Box<StmtNode>),
    If(Box<ExprNode>, Box<StmtNode>),
    Else(Box<ExprNode>, Box<StmtNode>, Box<StmtNode>),
    While(Box<ExprNode>, Box<StmtNode>),
    Do(Box<ExprNode>, Box<StmtNode>),
    Break(Rc<RefCell<i64>>),
    FuncDef(Box<ExprNode>, Box<StmtNode>, i32),
    Return(Option<Box<ExprNode>>),
    FuncCall(Box<ExprNode>),
}

impl StmtNode {
    pub fn new_null() -> StmtNode {
        StmtNode::Null
    }

    pub fn box_null() -> Box<StmtNode> {
        Box::new(StmtNode::new_null())
    }

    pub fn new_set(id: Box<ExprNode>, expr: Box<ExprNode>) -> Result<StmtNode, String> {
        if !id.is_id() {
            return Err(String::from("Type Error"));
        }

        let id_tp = id.tp();
        let expr_tp = expr.tp();
        if id_tp.is_numeric() && !expr_tp.is_numeric()
            || !id_tp.is_numeric() && expr_tp.is_numeric()
        {
            return Err(String::from("Type Error"));
        }
        let bt = Type::bool();
        if id_tp == &bt && expr_tp != &bt || id_tp != &bt && expr_tp == &bt {
            return Err(String::from("Type Error"));
        }
        Ok(StmtNode::Set(id, expr))
    }

    pub fn box_set(id: Box<ExprNode>, expr: Box<ExprNode>) -> Result<Box<StmtNode>, String> {
        let set = StmtNode::new_set(id, expr)?;
        Ok(Box::new(set))
    }

    pub fn new_setelem(access: Box<ExprNode>, expr: Box<ExprNode>) -> Result<StmtNode, String> {
        let access_tp = access.tp();
        let expr_tp = expr.tp();

        if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = access_tp
        {
            return Err(String::from("Type Error"));
        }

        if let Type::Array {
            of: _,
            tag: _,
            length: _,
        } = expr_tp
        {
            return Err(String::from("Type Error"));
        }

        if access_tp == expr_tp || (access_tp.is_numeric() && expr_tp.is_numeric()) {
            if let ExprNode::Access(_, array, index, _) = *access {
                return Ok(StmtNode::SetElem(array, index, expr));
            }
        }

        Err(String::from("Type error"))
    }

    pub fn box_setelem(id: Box<ExprNode>, expr: Box<ExprNode>) -> Result<Box<StmtNode>, String> {
        let setelem = StmtNode::new_setelem(id, expr)?;
        Ok(Box::new(setelem))
    }

    pub fn new_seq(head: Box<StmtNode>, tail: Box<StmtNode>) -> StmtNode {
        StmtNode::Seq(head, tail)
    }

    pub fn box_seq(head: Box<StmtNode>, tail: Box<StmtNode>) -> Box<StmtNode> {
        Box::new(StmtNode::new_seq(head, tail))
    }

    pub fn new_if(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<StmtNode, String> {
        if *cond.tp() != Type::bool() {
            return Err(String::from("Boolean required in if"));
        }
        Ok(StmtNode::If(cond, body))
    }

    pub fn box_if(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<Box<StmtNode>, String> {
        let is = StmtNode::new_if(cond, body)?;
        Ok(Box::new(is))
    }

    pub fn new_else(
        cond: Box<ExprNode>,
        true_stmt: Box<StmtNode>,
        false_stmt: Box<StmtNode>,
    ) -> Result<StmtNode, String> {
        if *cond.tp() != Type::bool() {
            return Err(String::from("Boolean required in if"));
        }
        Ok(StmtNode::Else(cond, true_stmt, false_stmt))
    }

    pub fn box_else(
        cond: Box<ExprNode>,
        true_stmt: Box<StmtNode>,
        false_stmt: Box<StmtNode>,
    ) -> Result<Box<StmtNode>, String> {
        let es = StmtNode::new_else(cond, true_stmt, false_stmt)?;
        Ok(Box::new(es))
    }

    pub fn new_while(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<StmtNode, String> {
        if *cond.tp() != Type::bool() {
            return Err(String::from("Boolean required in while"));
        }
        Ok(StmtNode::While(cond, body))
    }

    pub fn box_while(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<Box<StmtNode>, String> {
        let ws = StmtNode::new_while(cond, body)?;
        Ok(Box::new(ws))
    }

    pub fn new_do(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<StmtNode, String> {
        if *cond.tp() != Type::bool() {
            return Err(String::from("Boolean required in do"));
        }
        Ok(StmtNode::Do(cond, body))
    }

    pub fn box_do(cond: Box<ExprNode>, body: Box<StmtNode>) -> Result<Box<StmtNode>, String> {
        let ds = StmtNode::new_do(cond, body)?;
        Ok(Box::new(ds))
    }

    pub fn new_break() -> StmtNode {
        StmtNode::Break(Rc::new(RefCell::new(0)))
    }

    pub fn box_break() -> Box<StmtNode> {
        Box::new(StmtNode::new_break())
    }

    pub fn new_funcdef(
        id: Box<ExprNode>,
        body: Box<StmtNode>,
        used: i32,
    ) -> Result<StmtNode, String> {
        if !id.is_id() && !id.tp().is_function() {
            return Err(String::from("Type Error"));
        }
        Ok(StmtNode::FuncDef(id, body, used))
    }

    pub fn box_funcdef(
        id: Box<ExprNode>,
        body: Box<StmtNode>,
        used: i32,
    ) -> Result<Box<StmtNode>, String> {
        let fd = StmtNode::new_funcdef(id, body, used)?;
        Ok(Box::new(fd))
    }

    pub fn new_return(expr: Option<Box<ExprNode>>) -> StmtNode {
        StmtNode::Return(expr)
    }

    pub fn box_return(expr: Option<Box<ExprNode>>) -> Box<StmtNode> {
        Box::new(StmtNode::new_return(expr))
    }

    pub fn new_funccall(expr: Box<ExprNode>) -> Result<StmtNode, String> {
        match *expr {
            ExprNode::FuncCall(_, _, _) => Ok(StmtNode::FuncCall(expr)),
            _ => Err(String::from("Type Error")),
        }
    }

    pub fn box_funccall(expr: Box<ExprNode>) -> Result<Box<StmtNode>, String> {
        let funccall = StmtNode::new_funccall(expr)?;
        Ok(Box::new(funccall))
    }

    pub fn gen(&self, ir: &mut TACIr, begin: i64, after: i64) -> Result<(), String> {
        match self {
            StmtNode::Set(id, expr) => {
                let e = expr.gen(ir)?;
                let tac = match *e {
                    ExprNode::Arith(_, _, left, right) => TACInstruction {
                        op: expr.operator_to_tac()?,
                        arg1: left.expr_to_tac()?,
                        arg2: right.expr_to_tac()?,
                        res: id.expr_to_tac()?,
                    },
                    ExprNode::Unary(_, _, unary) => TACInstruction {
                        op: expr.operator_to_tac()?,
                        arg1: TACOperand::Null,
                        arg2: unary.expr_to_tac()?,
                        res: id.expr_to_tac()?,
                    },
                    _ => TACInstruction {
                        op: TACOperator::Assign,
                        arg1: e.expr_to_tac()?,
                        arg2: TACOperand::Null,
                        res: id.expr_to_tac()?,
                    },
                };
                emit(ir, tac);
            }
            StmtNode::SetElem(array, index, expr) => {
                let i = index.reduce(ir)?;
                let e = expr.reduce(ir)?;
                let tac = TACInstruction {
                    op: TACOperator::Assign,
                    arg1: e.expr_to_tac()?,
                    arg2: TACOperand::Null,
                    res: TACOperand::Array(
                        array.to_string(),
                        i.to_string(),
                        array.tp().to_owned(),
                        array.return_offset()?,
                    ),
                };
                emit(ir, tac);
            }
            StmtNode::Seq(head, tail) => {
                if head.is_null() {
                    tail.gen(ir, begin, after)?;
                } else if tail.is_null() {
                    head.gen(ir, begin, after)?;
                } else {
                    let label = new_label();
                    head.gen(ir, begin, label)?;
                    emit_label(ir, label);
                    tail.gen(ir, label, after)?;
                }
            }
            StmtNode::If(cond, body) => {
                let label = new_label();
                cond.jumping(ir, 0, after)?;
                emit_label(ir, label);
                body.gen(ir, label, after)?;
            }
            StmtNode::Else(cond, true_stmt, false_stmt) => {
                let label_if = new_label();
                let label_else = new_label();
                cond.jumping(ir, 0, label_else)?;
                emit_label(ir, label_if);
                true_stmt.gen(ir, label_if, after)?;
                let tac = TACInstruction {
                    op: TACOperator::Goto,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Label(after),
                };
                emit(ir, tac);
                emit_label(ir, label_else);
                false_stmt.gen(ir, label_else, after)?;
            }
            StmtNode::While(cond, body) => {
                self.after(after);
                cond.jumping(ir, 0, after)?;
                let label = new_label();
                emit_label(ir, label);
                body.gen(ir, label, begin)?;
                let tac = TACInstruction {
                    op: TACOperator::Goto,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Label(begin),
                };
                emit(ir, tac);
            }
            StmtNode::Do(cond, body) => {
                self.after(after);
                let label = new_label();
                body.gen(ir, begin, label)?;
                emit_label(ir, label);
                cond.jumping(ir, begin, 0)?;
            }
            StmtNode::Break(after) => {
                let a = after.borrow();
                if *a == 0 {
                    return Err(String::from("Unenclosed break"));
                }
                let tac = TACInstruction {
                    op: TACOperator::Goto,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Label(*a),
                };
                emit(ir, tac);
            }
            StmtNode::FuncDef(id, body, used) => {
                let ret_tp = id.return_tp()?;
                self.ret(&ret_tp)?;
                emit_function(ir, id.to_string());
                let tac_begin = TACInstruction {
                    op: TACOperator::Begin(*used),
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Null,
                };
                let tac_end = TACInstruction {
                    op: TACOperator::End,
                    arg1: TACOperand::Null,
                    arg2: TACOperand::Null,
                    res: TACOperand::Null,
                };
                emit(ir, tac_begin);
                emit_label(ir, begin);
                body.gen(ir, begin, after)?;
                emit_label(ir, after);
                emit(ir, tac_end);
            }
            StmtNode::Return(expr) => match expr {
                Some(e) => {
                    let return_value = e.reduce(ir)?;
                    let tac = TACInstruction {
                        op: TACOperator::Ret,
                        arg1: TACOperand::Null,
                        arg2: TACOperand::Null,
                        res: return_value.expr_to_tac()?,
                    };
                    emit(ir, tac);
                }
                None => {
                    let tac = TACInstruction {
                        op: TACOperator::Ret,
                        arg1: TACOperand::Null,
                        arg2: TACOperand::Null,
                        res: TACOperand::Null,
                    };
                    emit(ir, tac);
                }
            },
            StmtNode::FuncCall(expr) => {
                expr.void_funccall(ir)?;
            }
            _ => (),
        }
        Ok(())
    }

    fn after(&self, label: i64) {
        match self {
            StmtNode::Seq(s1, s2) | StmtNode::Else(_, s1, s2) => {
                s1.after(label);
                s2.after(label);
            }
            StmtNode::If(_, body) | StmtNode::While(_, body) | StmtNode::Do(_, body) => {
                body.after(label);
            }
            StmtNode::Break(after) => {
                let mut a = after.borrow_mut();
                *a = label;
            }
            _ => (),
        }
    }

    fn ret(&self, tp: &Type) -> Result<(), String> {
        match self {
            StmtNode::Seq(s1, s2) | StmtNode::Else(_, s1, s2) => {
                s1.ret(tp)?;
                s2.ret(tp)?;
            }
            StmtNode::FuncDef(_, body, _)
            | StmtNode::If(_, body)
            | StmtNode::While(_, body)
            | StmtNode::Do(_, body) => body.ret(tp)?,
            StmtNode::Return(expr) => match expr {
                Some(e) => match Type::max_type(tp, e.tp()) {
                    Some(max_tp) => {
                        if max_tp != *tp {
                            return Err(String::from("Type error"));
                        }
                    }
                    None => return Err(String::from("Type error")),
                },
                None => {
                    if *tp != Type::void() {
                        return Err(String::from("Type error"));
                    }
                }
            },
            _ => (),
        }
        Ok(())
    }

    fn is_null(&self) -> bool {
        if let StmtNode::Null = self {
            true
        } else {
            false
        }
    }
}
