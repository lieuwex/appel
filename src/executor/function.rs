use std::fmt;

use crate::ast::*;

#[derive(Clone, Debug)]
pub enum Function {
    Named {
        name: String,
        params: Vec<String>,
        expr: Expr,
    },
    Lambda {
        params: Vec<String>,
        expr: Expr,
    },
}

impl Function {
    pub fn params(&self) -> &[String] {
        match self {
            Function::Named { params, .. } => params,
            Function::Lambda { params, .. } => params,
        }
    }

    pub fn expr(&self) -> &Expr {
        match self {
            Function::Named { expr, .. } => expr,
            Function::Lambda { expr, .. } => expr,
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Named { name, params, expr } => {
                write!(f, "fn {}", name)?;
                for param in params {
                    write!(f, " {}", param)?;
                }

                write!(f, " = {}", expr)
            }
            Function::Lambda { params, expr } => {
                write!(f, "\\")?;
                for param in params {
                    write!(f, "{} ", param)?;
                }

                write!(f, "-> {}", expr)
            }
        }
    }
}
