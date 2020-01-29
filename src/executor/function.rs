use std::fmt;

use crate::ast::*;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub expr: Expr,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}", self.name)?;
        for param in &self.params {
            write!(f, " {}", param)?;
        }

        write!(f, " = {}", self.expr)
    }
}
