use super::function::Function;
use super::matrix::Matrix;

#[derive(Clone, Debug)]
pub enum Value {
    Matrix(Matrix),
    Function(Function),
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
