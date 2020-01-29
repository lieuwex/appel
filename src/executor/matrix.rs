use super::result::ExecutorResult;
use super::value::Value;
use crate::ast::*;

use std::convert::TryFrom;
use std::fmt;
use std::io::Write;

use tabwriter::{Alignment, TabWriter};

#[derive(Clone, Debug)]
pub struct Matrix {
    pub values: Vec<Ratio>,
    pub shape: Vec<usize>,
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: actually format

        let n_dimensions = self.shape.len();
        match n_dimensions {
            2 => {
                let mut writer = TabWriter::new(vec![])
                    .padding(1)
                    .minwidth(0)
                    .alignment(Alignment::Right);

                for i in 0..self.shape[0] {
                    for j in 0..self.shape[1] {
                        let val = self.get_at(vec![i, j]).unwrap();
                        write!(&mut writer, "{}\t", val).or(Err(fmt::Error {}))?;
                    }
                    writeln!(writer).or(Err(fmt::Error {}))?;
                }

                writer.flush().or(Err(fmt::Error {}))?;
                let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
                write!(f, "{}", s)?;
            }

            _ => {
                for (i, val) in self.values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", val)?;
                }
            }
        }

        Ok(())
    }
}

impl Matrix {
    pub fn make_vector(values: Vec<Ratio>) -> Self {
        let shape = vec![values.len()];
        Self { values, shape }
    }

    pub fn is_scalar(&self) -> bool {
        self.values.len() == 1
    }

    pub fn scalar(&self) -> Option<&Ratio> {
        if self.is_scalar() {
            Some(&self.values[0])
        } else {
            None
        }
    }

    pub fn get_at(&self, indices: Vec<usize>) -> Option<&Ratio> {
        if indices.len() != self.shape.len() {
            return None;
        }

        let mut i = 0;
        let mut mult = 1;
        for (dim, index) in indices.iter().rev().enumerate() {
            i += index * mult;
            mult *= self.shape[dim];
        }

        self.values.get(i)
    }
}

impl From<Ratio> for Matrix {
    fn from(rat: Ratio) -> Self {
        Self {
            values: vec![rat],
            shape: vec![],
        }
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
            ExecutorResult::Value(val) => Matrix::try_from(val),
        }
    }
}
