use super::chain::Error;
use super::result::ExecutorResult;
use super::value::Value;
use crate::ast::*;

use std::convert::TryFrom;

#[derive(Clone, Copy)]
pub enum Formatter {
    Float(Option<usize>), // precision
    Ratio,
}

impl Formatter {
    pub fn apply(self, rat: &Ratio) -> String {
        if rat.is_integer() {
            return format!("{}", rat);
        }

        match self {
            Formatter::Float(None) => {
                let mut buf = dtoa::Buffer::new();
                buf.format(rat.to_f64()).to_string()
            }
            Formatter::Float(Some(precision)) => format!("{1:.0$}", precision, rat.to_f64()),
            Formatter::Ratio => format!("{}", rat),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Matrix(Vec<Ratio>);

impl Matrix {
    pub fn make_scalar(value: Ratio) -> Self {
        Self(vec![value])
    }

    pub fn make_vector(values: Vec<Ratio>) -> Self {
        Self(values)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_scalar(&self) -> bool {
        self.len() == 1
    }

    pub fn scalar(&self) -> Option<&Ratio> {
        self.is_scalar().then(|| &self.0[0])
    }

    pub fn into_scalar(self) -> Option<Ratio> {
        self.is_scalar().then(|| self.0.into_iter().nth(0).unwrap())
    }

    pub fn get_at(&self, i: usize) -> Option<&Ratio> {
        self.0.get(i)
    }
    pub fn take_at(self, i: usize) -> Option<Ratio> {
        self.0.into_iter().nth(i)
    }

    pub fn get_multiple(&self, indices: &[usize]) -> Option<Vec<Ratio>> {
        indices.iter().map(|i| self.0.get(*i).cloned()).collect()
    }

    pub fn iter(&self) -> std::slice::Iter<Ratio> {
        self.0.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<Ratio> {
        self.0.iter_mut()
    }
}

impl IntoIterator for Matrix {
    type Item = Ratio;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl TryFrom<Value> for Matrix {
    type Error = Error;

    fn try_from(res: Value) -> Result<Self, Self::Error> {
        match res {
            Value::Function(_) => Err(Error::from("expected matrix, got a function")),
            Value::Matrix(m) => Ok(m),
        }
    }
}
impl TryFrom<ExecutorResult> for Matrix {
    type Error = Error;

    fn try_from(res: ExecutorResult) -> Result<Self, Self::Error> {
        match res {
            ExecutorResult::None => Err(Error::from("expected value")),
            ExecutorResult::Info(_) => Err(Error::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(Error::from("expected value, got a setting")),
            ExecutorResult::Chain(c) => Matrix::try_from(Value::try_from(c)?),
        }
    }
}
