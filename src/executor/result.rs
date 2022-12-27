use std::convert::TryFrom;

use crate::ast::Ratio;

use super::{
    chain::{Chain, IterShape, ValueIter},
    matrix::Matrix,
    value::Value,
};

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

    pub fn into_chain(self) -> Result<Chain, String> {
        match self {
            ExecutorResult::None => Err(String::from("expected value")),
            ExecutorResult::Info(_) => Err(String::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(String::from("expected value, got a setting")),

            ExecutorResult::Chain(c) => Ok(c),
            ExecutorResult::Value(v) => Ok(Chain::Value(v)),
        }
    }

    pub fn into_iter_shape(self) -> Result<IterShape, String> {
        self.into_chain()?.into_iter_shape()
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
