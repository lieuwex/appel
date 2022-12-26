use super::executor::to_f64;
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
    fn apply(self, rat: &Ratio) -> String {
        if rat.is_integer() {
            return format!("{}", rat);
        }

        match self {
            Formatter::Float(None) => match to_f64(rat) {
                None => format!("{}", rat),
                Some(f) => {
                    let mut buf: Vec<u8> = vec![];
                    dtoa::write(&mut buf, f).unwrap();
                    String::from_utf8(buf).unwrap()
                }
            },
            Formatter::Float(Some(precision)) => match to_f64(rat) {
                None => format!("{}", rat),
                Some(f) => format!("{1:.0$}", precision, f),
            },
            Formatter::Ratio => format!("{}", rat),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Matrix {
    pub values: Vec<Ratio>,
    pub shape: Vec<usize>,
}

impl Matrix {
    pub fn format(&self, fmt: Formatter) -> String {
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
                        write!(&mut writer, "{}\t", fmt.apply(val)).unwrap();
                    }
                    writeln!(writer).unwrap();
                }

                writer.flush().unwrap();
                String::from_utf8(writer.into_inner().unwrap()).unwrap()
            }

            _ => self
                .values
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
                .collect::<String>(),
        }
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

    pub fn into_scalar(self) -> Option<Ratio> {
        if self.is_scalar() {
            Some(self.values.into_iter().nth(0).unwrap())
        } else {
            None
        }
    }

    // REVIEW: this seems broken
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
            shape: vec![1],
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
            ExecutorResult::Setting(_, _) => Err(String::from("expected value, got a setting")),
            ExecutorResult::Chain(c) => Matrix::try_from(Value::try_from(c)?),
            ExecutorResult::Value(val) => Matrix::try_from(val),
        }
    }
}
