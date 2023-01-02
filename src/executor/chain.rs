use std::{backtrace::Backtrace, borrow::Borrow, convert::TryFrom};

use crate::ast::Ratio;

use super::{function::Function, matrix::Matrix, ExecutorResult, Value};

use dyn_clonable::{
    dyn_clone::{clone, clone_box},
    *,
};
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

    pub fn is_scalar(&self) -> bool {
        self.len() == 1
    }

    pub fn scalar(&self) -> Option<Ratio> {
        if self.is_scalar() {
            let mut i = self.iterator.clone();
            // TODO
            Some(i.next().unwrap().unwrap())
        } else {
            None
        }
    }

    pub fn into_scalar(mut self) -> Option<Ratio> {
        if self.is_scalar() {
            // TODO
            Some(self.iterator.next().unwrap().unwrap())
        } else {
            None
        }
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
        Chain::MatrixIterator {
            iterator: value.iterator,
            shape: value.shape,
        }
    }
}

impl From<IterShape> for ExecutorResult {
    fn from(value: IterShape) -> Self {
        ExecutorResult::Chain(value.into())
    }
}

pub enum Chain {
    Value(Value),
    MatrixIterator {
        iterator: Box<dyn ValueIter>,
        shape: SmallVec<[usize; 4]>,
    },
}

impl Chain {
    pub fn make_scalar(r: Ratio) -> Self {
        Chain::Value(Value::Matrix(Matrix::from(r)))
    }

    pub fn make_vector(iterator: Box<dyn ValueIter>, len: usize) -> Self {
        Self::MatrixIterator {
            iterator,
            shape: smallvec![len],
        }
    }

    pub fn into_iter_shape(self) -> Result<IterShape, String> {
        match self {
            Chain::Value(Value::Function(_)) => Err(String::from("expected value, got a function")),

            Chain::MatrixIterator { iterator, shape } => Ok(IterShape { iterator, shape }),
            Chain::Value(Value::Matrix(m)) => Ok(IterShape {
                iterator: Box::new(m.values.into_iter().map(Ok)),
                shape: m.shape,
            }),
        }
    }

    pub fn into_result(self) -> ExecutorResult {
        ExecutorResult::Chain(self)
    }
}

impl TryFrom<Chain> for Value {
    type Error = String;

    fn try_from(value: Chain) -> Result<Self, Self::Error> {
        match value {
            Chain::Value(v) => Ok(v),
            Chain::MatrixIterator { iterator, shape } => {
                Matrix::try_from(IterShape { iterator, shape }).map(Value::Matrix)
            }
        }
    }
}

impl TryFrom<Chain> for Matrix {
    type Error = String;

    fn try_from(chain: Chain) -> Result<Self, Self::Error> {
        let chain = Value::try_from(chain)?;
        match chain {
            Value::Function(f) => Err(String::from("expected matrix, got a function")),
            Value::Matrix(m) => Ok(m),
        }
    }
}

impl Clone for Chain {
    fn clone(&self) -> Self {
        match self {
            Chain::Value(v) => Chain::Value(v.clone()),
            Chain::MatrixIterator { iterator, shape } => Chain::MatrixIterator {
                iterator: clone_box(iterator),
                shape: shape.clone(),
            },
        }
    }
}
