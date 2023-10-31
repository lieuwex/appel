use super::result::ExecutorResult;
use super::value::Value;
use crate::ast::*;

use std::convert::TryFrom;
use std::io::Write;

use tabwriter::{Alignment, TabWriter};

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
                let mut buf: Vec<u8> = vec![];
                dtoa::write(&mut buf, rat.to_f64()).unwrap();
                String::from_utf8(buf).unwrap()
            }
            Formatter::Float(Some(precision)) => format!("{1:.0$}", precision, rat.to_f64()),
            Formatter::Ratio => format!("{}", rat),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Matrix {
    pub values: Vec<Ratio>,
    pub len: usize,
}

impl Matrix {
    pub fn format(&self, fmt: Formatter) -> String {
        self.values
            .iter()
            .enumerate()
            .map(|(i, val)| {
                let val = fmt.apply(val);
                if i > 0 {
                    format!(" {}", val)
                } else {
                    val
                }
            })
            .collect::<String>()
    }
}

impl Matrix {
    pub fn make_scalar(value: Ratio) -> Self {
        Self {
            values: vec![value],
            len: 1,
        }
    }

    pub fn make_vector(values: Vec<Ratio>) -> Self {
        Self {
            len: values.len(),
            values,
        }
    }

    pub fn is_scalar(&self) -> bool {
        self.values.len() == 1
    }

    pub fn scalar(&self) -> Option<&Ratio> {
        self.is_scalar().then(|| &self.values[0])
    }

    pub fn into_scalar(self) -> Option<Ratio> {
        self.is_scalar()
            .then(|| self.values.into_iter().nth(0).unwrap())
    }

    pub fn get_at(&self, i: usize) -> Option<&Ratio> {
        self.values.get(i)
    }
    pub fn take_at(self, i: usize) -> Option<Ratio> {
        self.values.into_iter().nth(i)
    }

    pub fn take_multiple(self, indices: &[usize]) -> Option<Vec<Ratio>> {
        if indices.iter().any(|i| *i >= self.len) {
            return None;
        }

        let res = indices
            .into_iter()
            .map(|i| self.values[*i].clone())
            .collect();
        Some(res)
    }
}

impl TryFrom<Value> for Matrix {
    type Error = String;

    fn try_from(res: Value) -> Result<Self, Self::Error> {
        match res {
            Value::Function(_) => Err(String::from("expected matrix, got a function")),
            Value::Matrix(m) => Ok(m),
        }
    }
}
impl TryFrom<ExecutorResult> for Matrix {
    type Error = String;

    fn try_from(res: ExecutorResult) -> Result<Self, Self::Error> {
        match res {
            ExecutorResult::None => Err(String::from("expected value")),
            ExecutorResult::Info(_) => Err(String::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(String::from("expected value, got a setting")),
            ExecutorResult::Chain(c) => Matrix::try_from(Value::try_from(c)?),
            ExecutorResult::Value(val) => Matrix::try_from(val),
        }
    }
}
