use super::value::Value;

#[derive(Clone, Debug)]
pub enum ExecutorResult {
    None,
    Value(Value),
    Info(String),
}

impl ExecutorResult {
    /// Unwrap into Value
    pub fn unwrap_value(self) -> Value {
        match self {
            ExecutorResult::Value(v) => v,
            _ => panic!(),
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
