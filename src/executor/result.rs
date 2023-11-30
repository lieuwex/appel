use std::convert::{TryFrom, TryInto};

use super::{
    chain::{Chain, IterShape},
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
    pub fn into_iter_shape(self) -> Result<IterShape, String> {
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
    type Error = String;

    fn try_from(value: ExecutorResult) -> Result<Self, Self::Error> {
        match value {
            ExecutorResult::Chain(c) => c.try_into(),
            _ => Err(String::from("Result is not a value or chain")),
        }
    }
}

impl TryFrom<ExecutorResult> for Chain {
    type Error = String;

    fn try_from(value: ExecutorResult) -> Result<Self, Self::Error> {
        match value {
            ExecutorResult::None => Err(String::from("expected value")),
            ExecutorResult::Info(_) => Err(String::from("expected value, got an info string")),
            ExecutorResult::Setting(_, _) => Err(String::from("expected value, got a setting")),

            ExecutorResult::Chain(c) => Ok(c),
        }
    }
}
