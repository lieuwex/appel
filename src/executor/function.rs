use crate::ast::*;

#[derive(Clone, Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub expr: Expr,
}
