use crate::ast::Ratio;
use crate::units::Unit;

use super::function::Function;
use super::matrix::Matrix;

#[derive(Clone, Debug)]
pub enum Value {
    Matrix(Matrix),
    Scalar(Ratio),
    Function(Function),
}

impl Value {
    /// A single number in the given unit.
    pub fn quantity(r: Ratio, unit: &Unit) -> Self {
        if unit.is_none() {
            Value::Scalar(r)
        } else {
            Value::Matrix(Matrix::with_unit(vec![r], unit.clone()))
        }
    }
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
