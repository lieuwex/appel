use crate::ast::Ratio;

use super::{
    chain::{Chain, ValueIter},
    value::Value,
};

pub struct IterShape {
    pub iterator: Box<dyn ValueIter>,
    pub shape: Vec<usize>,
}

impl IterShape {
    pub fn is_scalar(&self) -> bool {
        let sum: usize = self.shape.iter().sum();
        sum == 1
    }

    pub fn scalar(mut self) -> Option<Ratio> {
        if self.is_scalar() {
            // TODO
            Some(self.iterator.next().unwrap().unwrap())
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub enum ExecutorResult {
    None,
    Chain(Chain),
    Value(Value),
    Info(String),
    Setting(String, String),
}

impl ExecutorResult {
    /// Unwrap into Value
    pub fn unwrap_value(self) -> Value {
        match self {
            ExecutorResult::Value(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_value_iter(self) -> Result<IterShape, String> {
        match self {
            ExecutorResult::None => Err(String::from("expected value")),
            ExecutorResult::Info(_) => Err(String::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(String::from("expected value, got a setting")),
            ExecutorResult::Chain(Chain::MatrixIterator { iterator, shape }) => {
                Ok(IterShape { iterator, shape })
            }
            ExecutorResult::Value(Value::Matrix(m)) => Ok(IterShape {
                iterator: Box::new(m.values.into_iter().map(Ok)),
                shape: m.shape,
            }),
            _ => todo!(),
        }
    }
}

impl<T> From<T> for ExecutorResult
where
    T: Into<Value>,
{
    fn from(v: T) -> Self {
        ExecutorResult::Value(v.into())
    }
}
