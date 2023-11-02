use std::{backtrace::Backtrace, convert::TryFrom};

use crate::ast::Ratio;

use super::{
    matrix::{Formatter, Matrix},
    ExecutorResult, Value,
};

use dyn_clonable::{dyn_clone::clone_box, *};

#[clonable]
pub trait ValueIter: Iterator<Item = Result<Ratio, String>> + Clone {}
impl<T> ValueIter for T where T: Iterator<Item = Result<Ratio, String>> + Clone {}

pub struct IterShape {
    pub iterator: Box<dyn ValueIter>,
    pub len: usize,
}

impl IterShape {
    pub fn is_scalar(&self) -> bool {
        self.len == 1
    }

    pub fn scalar(&self) -> Option<Ratio> {
        self.is_scalar().then(|| {
            // TODO: optimize
            let mut i = self.iterator.clone();
            i.next().unwrap().unwrap()
        })
    }

    pub fn into_scalar(mut self) -> Option<Ratio> {
        self.is_scalar().then(|| {
            // TODO
            self.iterator.next().unwrap().unwrap()
        })
    }
}

impl TryFrom<IterShape> for Matrix {
    type Error = String;

    fn try_from(value: IterShape) -> Result<Self, Self::Error> {
        if cfg!(debug_assertions) {
            let bt = Backtrace::force_capture();
            println!("collect point:\n{}", bt);
        }

        let values: Result<Vec<Ratio>, String> = value.iterator.collect();
        let values = values?;

        Ok(Matrix { values })
    }
}

impl From<IterShape> for Chain {
    fn from(value: IterShape) -> Self {
        Chain::Iterator(value)
    }
}

impl From<IterShape> for ExecutorResult {
    fn from(value: IterShape) -> Self {
        ExecutorResult::Chain(value.into())
    }
}

pub enum Chain {
    Value(Value),
    Iterator(IterShape),
}

impl Chain {
    pub fn make_scalar(r: Ratio) -> Self {
        Chain::Value(Value::Matrix(Matrix::make_scalar(r)))
    }

    pub fn make_vector(iterator: Box<dyn ValueIter>, len: usize) -> Self {
        Self::Iterator(IterShape { iterator, len })
    }

    pub fn into_iter_shape(self) -> Result<IterShape, String> {
        match self {
            Chain::Value(Value::Function(_)) => Err(String::from("expected value, got a function")),

            Chain::Iterator(iter_shape) => Ok(iter_shape),
            Chain::Value(Value::Matrix(m)) => Ok(IterShape {
                len: m.len(),
                iterator: Box::new(m.values.into_iter().map(Ok)),
            }),
        }
    }

    pub fn into_result(self) -> ExecutorResult {
        ExecutorResult::Chain(self)
    }

    pub fn format(self, fmt: Formatter) -> Result<String, String> {
        let res = match self {
            Chain::Value(Value::Function(f)) => format!("{}", f),
            Chain::Value(Value::Matrix(m)) => m.format(fmt),
            Chain::Iterator(IterShape { iterator, .. }) => iterator
                .enumerate()
                .map(|(i, val)| {
                    let val = fmt.apply(&val?);
                    if i > 0 {
                        Ok(format!(" {}", val))
                    } else {
                        Ok(val)
                    }
                })
                .collect::<Result<String, String>>()?,
        };
        Ok(res)
    }
}

impl TryFrom<Chain> for Value {
    type Error = String;

    fn try_from(value: Chain) -> Result<Self, Self::Error> {
        match value {
            Chain::Value(v) => Ok(v),
            Chain::Iterator(iter_shape) => Matrix::try_from(iter_shape).map(Value::Matrix),
        }
    }
}

impl TryFrom<Chain> for Matrix {
    type Error = String;

    fn try_from(chain: Chain) -> Result<Self, Self::Error> {
        let chain = Value::try_from(chain)?;
        match chain {
            Value::Function(_) => Err(String::from("expected matrix, got a function")),
            Value::Matrix(m) => Ok(m),
        }
    }
}

impl Clone for Chain {
    fn clone(&self) -> Self {
        match self {
            Chain::Value(v) => Chain::Value(v.clone()),
            Chain::Iterator(IterShape { iterator, len }) => Chain::Iterator(IterShape {
                iterator: clone_box(iterator),
                len: *len,
            }),
        }
    }
}
