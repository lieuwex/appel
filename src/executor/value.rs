use crate::ast::Ratio;

use super::function::Function;
use super::matrix::Matrix;

#[derive(Clone, Debug)]
pub enum Value {
    Matrix(Matrix),
    Scalar(Ratio),
    Function(Function),
}

impl From<Matrix> for Value {
    fn from(m: Matrix) -> Self {
        Value::Matrix(m)
    }
}

impl From<Ratio> for Value {
    fn from(r: Ratio) -> Self {
        Value::Scalar(r)
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Value::Function(f)
    }
}
