use std::fmt;

use super::function::Function;
use super::matrix::Matrix;

#[derive(Clone, Debug)]
pub enum Value {
    Matrix(Matrix),
    Function(Function),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Matrix(m) => write!(f, "{}", m),
            Value::Function(fun) => {
                write!(f, "fn")?;
                for param in &fun.params {
                    write!(f, " {}", param)?;
                }

                write!(f, " = {}", fun.expr)
            }
        }
    }
}

impl From<Matrix> for Value {
    fn from(m: Matrix) -> Self {
        Value::Matrix(m)
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Value::Function(f)
    }
}
