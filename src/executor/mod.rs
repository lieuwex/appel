#[allow(clippy::module_inception)]
mod executor;
mod function;
mod matrix;
mod result;
mod value;

pub use executor::Executor;
pub use result::ExecutorResult;
