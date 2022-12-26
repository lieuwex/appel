pub mod chain;
#[allow(clippy::module_inception)]
pub mod executor;
pub mod function;
pub mod matrix;
pub mod result;
pub mod value;

pub use executor::Executor;
pub use result::ExecutorResult;
pub use value::Value;
