use std::convert::TryFrom;

use crate::ast::Ratio;
use crate::collect_point;
use crate::time;
use crate::units::Unit;

use super::{
    matrix::{Formatter, Matrix},
    ExecutorResult, Value,
};

use dyn_clonable::*;

use itertools::Itertools;

pub type Error = std::borrow::Cow<'static, str>;

#[clonable]
pub trait ValueIter: Iterator<Item = Result<Ratio, Error>> + Clone {}
impl<T> ValueIter for T where T: Iterator<Item = Result<Ratio, Error>> + Clone {}

#[derive(Clone)]
pub struct IterShape {
    pub iterator: Box<dyn ValueIter>,
    pub len: usize,
    pub unit: Unit,
}

impl IterShape {
    pub fn expect_no_unit(&self) -> Result<(), Error> {
        self.unit.expect_none()
    }

    pub fn is_scalar(&self) -> bool {
        self.len == 1
    }

    pub fn scalar(&self) -> Option<Ratio> {
        self.is_scalar().then(|| {
            // TODO: optimize
            let mut i = self.iterator.clone();
            i.next().unwrap().unwrap()
        })
    }

    pub fn into_scalar(mut self) -> Result<Ratio, Self> {
        if self.is_scalar() {
            Ok(self.iterator.next().unwrap().unwrap())
        } else {
            Err(self)
        }
    }
}

impl std::fmt::Debug for IterShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IterShape")
            .field("len", &self.len)
            .field("unit", &self.unit)
            .finish()
    }
}

impl TryFrom<IterShape> for Matrix {
    type Error = Error;

    fn try_from(value: IterShape) -> Result<Self, Self::Error> {
        collect_point!();

        let values: Result<Vec<Ratio>, Error> = value.iterator.collect();
        let values = values?;

        Ok(Matrix::with_unit(values, value.unit))
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

#[derive(Clone, Debug)]
pub enum Chain {
    Value(Value),
    Iterator(IterShape),
}

impl Chain {
    pub fn make_scalar(r: Ratio) -> Self {
        Chain::Value(Value::Scalar(r))
    }

    pub fn make_vector(iterator: Box<dyn ValueIter>, len: usize) -> Self {
        Self::Iterator(IterShape {
            iterator,
            len,
            unit: Unit::none(),
        })
    }

    pub fn into_iter_shape(self) -> Result<IterShape, Error> {
        match self {
            Chain::Value(Value::Function(_)) => Err(Error::from("expected value, got a function")),

            Chain::Iterator(iter_shape) => Ok(iter_shape),
            Chain::Value(Value::Scalar(r)) => Ok(IterShape {
                iterator: Box::new(std::iter::once(Ok(r))),
                len: 1,
                unit: Unit::none(),
            }),
            Chain::Value(Value::Matrix(m)) => Ok(IterShape {
                len: m.len(),
                unit: m.unit.clone(),
                iterator: Box::new(m.into_iter().map(Ok)),
            }),
        }
    }

    pub fn format(self, fmt: Formatter) -> Result<String, Error> {
        fn format_iter(
            fmt: Formatter,
            iterator: impl Iterator<Item = Result<Ratio, Error>>,
            unit: Unit,
        ) -> Result<String, Error> {
            if let Some(clock) = unit.clock_format() {
                return iterator
                    .map(|val| Ok(time::format_clock(clock, &val?, fmt)))
                    .intersperse(Ok(" ".to_string()))
                    .collect();
            }

            let factor = unit.display.as_ref().map(|named| &named.factor);
            let mut count = 0;
            let values: String = iterator
                .inspect(|_| count += 1)
                .map(|val| {
                    let val = val?;
                    Ok(match factor {
                        Some(factor) => fmt.apply(&Ratio::from(&val / factor)),
                        None => fmt.apply(&val),
                    })
                })
                .intersperse(Ok(" ".to_string()))
                .collect::<Result<_, Error>>()?;

            Ok(unit.attach(&values, count))
        }

        match self {
            Chain::Value(Value::Function(f)) => Ok(format!("{}", f)),
            Chain::Value(Value::Scalar(r)) => Ok(format!("{}", r)),
            Chain::Value(Value::Matrix(m)) => {
                let unit = m.unit.clone();
                format_iter(fmt, m.into_iter().map(Result::<_, Error>::Ok), unit)
            }
            Chain::Iterator(IterShape { iterator, unit, .. }) => format_iter(fmt, iterator, unit),
        }
    }
}

impl TryFrom<Chain> for Value {
    type Error = Error;

    fn try_from(value: Chain) -> Result<Self, Self::Error> {
        match value {
            Chain::Value(v) => Ok(v),
            Chain::Iterator(iter_shape) if iter_shape.unit.is_none() => {
                match iter_shape.into_scalar() {
                    Ok(s) => Ok(Value::Scalar(s)),
                    Err(iter_shape) => Matrix::try_from(iter_shape).map(Value::Matrix),
                }
            }
            Chain::Iterator(iter_shape) => Matrix::try_from(iter_shape).map(Value::Matrix),
        }
    }
}

impl TryFrom<Chain> for Matrix {
    type Error = Error;

    fn try_from(chain: Chain) -> Result<Self, Self::Error> {
        let val = Value::try_from(chain)?;
        Self::try_from(val)
    }
}

impl From<Chain> for ExecutorResult {
    fn from(c: Chain) -> Self {
        ExecutorResult::Chain(c)
    }
}

impl From<Value> for Chain {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}
