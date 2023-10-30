use std::{backtrace::Backtrace, convert::TryFrom};

use crate::ast::Ratio;

use super::{
    function::Function,
    matrix::{Formatter, Matrix},
    ExecutorResult, Value,
};

use dyn_clonable::{dyn_clone::clone_box, *};
use smallvec::{smallvec, SmallVec};

#[clonable]
pub trait ValueIter: Iterator<Item = Result<Ratio, String>> + Clone {}
impl<T> ValueIter for T where T: Iterator<Item = Result<Ratio, String>> + Clone {}
#[clonable]
pub trait FunctionIter: Iterator<Item = Function> + Clone {}
impl<T> FunctionIter for T where T: Iterator<Item = Function> + Clone {}

pub struct IterShape {
    pub iterator: Box<dyn ValueIter>,
    pub shape: SmallVec<[usize; 4]>,
}

impl IterShape {
    pub fn len(&self) -> usize {
        self.shape.iter().sum()
    }

    pub fn vector(&self) -> Option<Ratio> {
        self.is_vector().then(|| {
            // TODO: optimize
            let mut i = self.iterator.clone();
            i.next().unwrap().unwrap()
        })
    }

    pub fn into_vector(mut self) -> Option<Ratio> {
        self.is_vector().then(|| {
            // TODO
            self.iterator.next().unwrap().unwrap()
        })
    }

    pub fn is_vector(&self) -> bool {
        matches!(self.shape.len(), 0 | 1)
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
        Ok(Matrix {
            values: values?,
            shape: value.shape,
        })
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
        Self::Iterator(IterShape {
            iterator,
            shape: smallvec![len],
        })
    }

    pub fn into_iter_shape(self) -> Result<IterShape, String> {
        match self {
            Chain::Value(Value::Function(_)) => Err(String::from("expected value, got a function")),

            Chain::Iterator(iter_shape) => Ok(iter_shape),
            Chain::Value(Value::Matrix(m)) => Ok(IterShape {
                iterator: Box::new(m.values.into_iter().map(Ok)),
                shape: m.shape,
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
            Chain::Iterator(IterShape { iterator, shape }) => {
                let n_dimensions = shape.len();
                match n_dimensions {
                    2 => Matrix::try_from(Chain::Iterator(IterShape { iterator, shape }))?
                        .format(fmt),
                    _ => iterator
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
                }
            }
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
            Chain::Iterator(IterShape { iterator, shape }) => Chain::Iterator(IterShape {
                iterator: clone_box(iterator),
                shape: shape.clone(),
            }),
        }
    }
}
