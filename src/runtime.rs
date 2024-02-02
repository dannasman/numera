use super::lexer::Token;
use crate::inter::Id;

#[derive(Debug, Clone)]
pub struct ActivationRecord {
    pub tp: Token,
    pub params: Vec<Id>,
}

impl ActivationRecord {
    pub fn new(tp: Token, params: Vec<Id>) -> Self {
        ActivationRecord { tp, params }
    }
}
