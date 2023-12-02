use std::convert::{TryFrom, TryInto};

use super::{
    chain::{Chain, Error, IterShape},
    value::Value,
};

#[derive(Clone)]
pub enum ExecutorResult {
    None,
    Chain(Chain),
    Info(String),
    Setting(String, String),
}

impl ExecutorResult {
    pub fn into_iter_shape(self) -> Result<IterShape, Error> {
        Chain::try_from(self)?.into_iter_shape()
    }
}

impl<T> From<T> for ExecutorResult
where
    T: Into<Value>,
{
    fn from(v: T) -> Self {
        ExecutorResult::Chain(Chain::Value(v.into()))
    }
}

impl TryFrom<ExecutorResult> for Value {
    type Error = Error;

    fn try_from(value: ExecutorResult) -> Result<Self, Self::Error> {
        match value {
            ExecutorResult::Chain(c) => c.try_into(),
            _ => Err(Error::from("Result is not a value or chain")),
        }
    }
}

impl TryFrom<ExecutorResult> for Chain {
    type Error = Error;

    fn try_from(value: ExecutorResult) -> Result<Self, Self::Error> {
        match value {
            ExecutorResult::None => Err(Error::from("expected value")),
            ExecutorResult::Info(_) => Err(Error::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(Error::from("expected value, got a setting")),

            ExecutorResult::Chain(c) => Ok(c),
        }
    }
}
