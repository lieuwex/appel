use super::value::Value;

#[derive(Clone, Debug)]
pub enum ExecutorResult {
    None,
    Value(Value),
    Info(String),
}

impl ExecutorResult {
    /// Unwrap into Value
    pub fn unwrap(self) -> Value {
        match self {
            ExecutorResult::Value(v) => v,
            _ => panic!(),
        }
    }
}
