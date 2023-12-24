use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::iter;
use std::ops::Neg;
use std::time::Instant;

use crate::executor::chain::{Chain, Error, IterShape, ValueIter};
use crate::parser;
use crate::{ast::*, collect_point};

use itertools::Itertools;
use num_bigint::{BigInt, Sign};
use num_traits::*;
use rug::{Assign, Float, Integer, Rational};

use rand::prelude::*;

use super::function::Function;
use super::matrix::Matrix;
use super::result::ExecutorResult;
use super::value::Value;

const FLOAT_PRECISION: u32 = 100;

fn to_float<T>(val: T) -> Float
where
    Float: Assign<T>,
{
    Float::with_val(FLOAT_PRECISION, val)
}

fn pow(a: Ratio, b: Ratio) -> Result<Ratio, Error> {
    let pair = b.is_integer().then(|| (b.to_u32(), b.to_i32()));

    match pair {
        Some((Some(b), _)) => Ok(a.pow(b)),
        Some((_, Some(b))) => Ok(a.pow(b)),
        _ => {
            let a = to_float(a);
            let b = to_float(b);

            a.pow(b)
                .to_rational()
                .ok_or_else(|| Error::from("result is not finite"))
        }
    }
}
fn log(base: Ratio, n: Ratio) -> Result<Ratio, Error> {
    let base = to_float(base);
    let n = to_float(n);

    let res = n.log2() / base.log2();
    res.to_rational()
        .ok_or_else(|| Error::from("result is not finite"))
}

pub struct Executor<'a> {
    previous: Option<&'a Executor<'a>>,
    variables: HashMap<String, Value>,
}

fn expect_scalar(v: ExecutorResult) -> Result<Ratio, Error> {
    match v.into_iter_shape()?.into_scalar() {
        Err(_) => Err(Error::from("expected scalar")),
        Ok(s) => Ok(s),
    }
}

fn to_usize_error(rat: &Ratio) -> Result<usize, Error> {
    let int = rat
        .to_integer()
        .ok_or_else(|| Error::from("value is not an integer"))?;
    int.to_usize()
        .ok_or_else(|| Error::from("value negative or too large for usize"))
}

fn into_integer_error(iter_shape: IterShape) -> Result<Integer, Error> {
    let scalar = iter_shape
        .into_scalar()
        .map_err(|_| Error::from("expected scalar, got vector"))?;
    let n = scalar
        .into_integer()
        .ok_or_else(|| Error::from("value must be an integer"))?;
    Ok(n)
}

fn expect_function_expr(exec: &Executor, expr: &Expr) -> Result<Function, Error> {
    match Value::try_from(exec.execute_expr(&expr)?) {
        Ok(Value::Function(f)) => Ok(f),
        _ => Err(Error::from("expected left hand to be a function")),
    }
}

fn get_comp_op_fn(op: CompOp) -> fn(Ratio, Ratio) -> Ratio {
    macro_rules! op {
        ($op:tt) => {
            |a, b| Ratio::from(a $op b)
        }
    }

    match op {
        CompOp::Eq => op!(==),
        CompOp::Neq => op!(!=),
        CompOp::Lt => op!(<),
        CompOp::Le => op!(<=),
        CompOp::Gt => op!(>),
        CompOp::Ge => op!(>=),
    }
}

fn call_binary(op: BinOp, a: Chain, b: Chain) -> Result<ExecutorResult, Error> {
    let a = a.into_iter_shape()?;
    let b = b.into_iter_shape()?;

    fn apply(
        a: IterShape,
        b: IterShape,
        f: impl Fn(Rational, Rational) -> Result<Rational, Error> + Clone + 'static,
    ) -> Result<ExecutorResult, Error> {
        if a.len == b.len {
            let values = (a.iterator).zip(b.iterator).map(move |(a, b)| f(a?, b?));

            let matrix = Chain::Iterator(IterShape {
                iterator: Box::new(values),
                len: a.len,
            });
            return Ok(matrix.into());
        }

        let (scalar, non_scalar, scalar_is_left) = if a.is_scalar() {
            (a.into_scalar().unwrap(), b, true)
        } else if b.is_scalar() {
            (b.into_scalar().unwrap(), a, false)
        } else {
            return Err(Error::from("length mismatch"));
        };

        let matrix = Chain::Iterator(IterShape {
            iterator: Box::new(non_scalar.iterator.map(move |v| {
                let scalar = scalar.clone();
                if scalar_is_left {
                    f(scalar, v?)
                } else {
                    f(v?, scalar)
                }
            })),
            len: non_scalar.len,
        });

        Ok(matrix.into())
    }

    macro_rules! safe_div {
        ($a:expr, $b:expr) => {{
            if $b.is_zero() {
                Err(Error::from("division by zero"))
            } else {
                Ok($a / $b)
            }
        }};
    }

    macro_rules! apply {
        ($f:expr) => {
            apply(a, b, $f)
        };
    }
    macro_rules! apply_ok {
        ($f:expr) => {
            apply!(move |a, b| Ok($f(a, b)))
        };
    }
    match op {
        BinOp::Add => apply_ok!(|a, b| a + b),
        BinOp::Sub => apply_ok!(|a, b| a - b),
        BinOp::Mul => apply_ok!(|a, b| a * b),
        BinOp::Div => apply!(|a, b| safe_div!(a, b)),
        BinOp::Mod => apply!(|a: Ratio, b: Ratio| safe_div!(a, &b).map(|v| v.rem_trunc() * b)),
        BinOp::Pow => apply!(pow),
        BinOp::Log => apply!(log),
        BinOp::CompOp(x) => apply_ok!(get_comp_op_fn(x)),

        BinOp::Concat => {
            let values = a.iterator.chain(b.iterator);
            Ok(Chain::Iterator(IterShape {
                iterator: Box::new(values),
                len: a.len + b.len,
            })
            .into())
        }

        BinOp::Drop => {
            let n = into_integer_error(a)?;

            let (n, at_start) = if (&n).signum() == -1 {
                (n.neg(), false)
            } else {
                (n, true)
            };
            let n = n
                .to_usize()
                .ok_or_else(|| Error::from("value too large for usize"))?;

            let (iterator, len): (Box<dyn ValueIter>, usize) = if at_start {
                (Box::new(b.iterator.skip(n)), b.len - n)
            } else {
                let amount = b.len - n;
                (Box::new(b.iterator.take(amount)), amount)
            };

            Ok(Chain::Iterator(IterShape { iterator, len }).into())
        }

        BinOp::Rho => {
            let len = a
                .into_scalar()
                .map_err(|_| Error::from("expected scalar"))?;
            let len = to_usize_error(&len)?;

            let values = std::iter::repeat(b.iterator).flatten().take(len);

            Ok(Chain::Iterator(IterShape {
                iterator: Box::new(values),
                len,
            })
            .into())
        }

        BinOp::Unpack => {
            let a = into_integer_error(a)?;

            let b = into_integer_error(b)?;
            let b = b.to_string();
            let b = BigInt::parse_bytes(b.as_bytes(), 10).unwrap();

            let radix = a
                .to_u32()
                .filter(|r| (2..=256).contains(r))
                .ok_or(Error::from("radix must be greater than or equal to 2"))?;

            let (sign, bits) = b.to_radix_be(radix);

            let len = bits.len();
            let values = bits
                .into_iter()
                .map(|b| Ratio::from(b))
                .map(move |b| {
                    if sign == num_bigint::Sign::Minus {
                        b.neg()
                    } else {
                        b
                    }
                })
                .map(Result::Ok);

            Ok(Chain::make_vector(Box::new(values), len).into())
        }
        BinOp::Pack => {
            let a = into_integer_error(a)?;

            let b: Result<_, _> = b
                .iterator
                .map(|v| {
                    v.map(|v| {
                        let (numer, denom) = v.into_numer_denom();

                        let numer = numer.to_string();
                        let numer = BigInt::parse_bytes(numer.as_bytes(), 10).unwrap();

                        let denom = denom.to_string();
                        let denom = BigInt::parse_bytes(denom.as_bytes(), 10).unwrap();

                        num_rational::BigRational::new_raw(numer, denom)
                    })
                })
                .collect();
            let b: Vec<_> = b?;

            let radix = a
                .to_u32()
                .filter(|r| (2..=256).contains(r))
                .ok_or(Error::from("radix must be greater than or equal to 2"))?;

            let sign = if b.iter().any(|b| b.to_integer().sign() == Sign::Minus) {
                Sign::Minus
            } else {
                Sign::Plus
            };

            let bits: Option<Vec<u8>> = b.iter().map(|b| b.to_integer().to_u8()).collect();
            let bits = bits.ok_or_else(|| Error::from("value too large"))?;

            let packed = BigInt::from_radix_be(sign, &bits, radix);
            let int = packed.ok_or(Error::from("couldn't convert bits to int"))?;

            let int = int.to_string();
            let int = Integer::from_str_radix(&int, 10).unwrap();

            Ok(Chain::make_scalar(Ratio::from(int)).into())
        }

        BinOp::In => {
            let base_set: Result<HashSet<Ratio>, Error> = b.iterator.collect();
            let base_set = base_set?;
            let values = a.iterator.map(move |x| {
                x.map(|x| {
                    if base_set.contains(&x) {
                        Ratio::one()
                    } else {
                        Ratio::zero()
                    }
                })
            });

            Ok(Chain::Iterator(IterShape {
                iterator: Box::new(values),
                len: a.len,
            })
            .into())
        }

        BinOp::Union => {
            let a: Result<Vec<_>, Error> = a.iterator.collect();
            let a = a?;

            let new_values: Vec<_> = b
                .iterator
                .filter(|v| v.as_ref().map_or(true, |v| !a.contains(v)))
                .collect();

            Ok(Chain::Iterator(IterShape {
                len: a.len() + new_values.len(),
                iterator: Box::new(a.into_iter().map(Result::Ok).chain(new_values)),
            })
            .into())
        }

        BinOp::Mask => {
            // TODO: only LHS has to be collected
            let values: Result<Vec<_>, Error> = (a.iterator)
                .zip(b.iterator)
                .map(|(a, b)| Ok((a?, b?)))
                .filter_map_ok(|(a, b)| (!a.is_zero()).then_some(b))
                .collect();

            Ok(Matrix::make_vector(values?).into())
        }

        BinOp::Max => apply_ok!(|a: Ratio, b: Ratio| a.max(b)),
        BinOp::Min => apply_ok!(|a: Ratio, b: Ratio| a.min(b)),

        BinOp::Pad => {
            let Some((new_len, value)) = a.iterator.collect_tuple() else {
                return Err(format!("expected 2 arguments on the left, got {}", a.len).into());
            };

            let new_len = new_len?;

            let (new_len, at_start) = if new_len < 0 {
                (new_len.neg(), false)
            } else {
                (new_len, true)
            };
            let new_len = to_usize_error(&new_len)?;

            let amount = new_len.checked_sub(b.len).ok_or_else(|| {
                Error::from("vector on the right is longer than the wanted padded length")
            })?;

            let repeated = iter::repeat(value).take(amount);
            let values: Box<dyn ValueIter> = if at_start {
                Box::new(repeated.chain(b.iterator))
            } else {
                Box::new(b.iterator.chain(repeated))
            };

            Ok(Chain::make_vector(values, new_len).into())
        }
    }
}

impl<'a> Executor<'a> {
    fn get_variable(&self, name: &str) -> Option<&Value> {
        match (self.variables.get(name), &self.previous) {
            (Some(v), _) => Some(v),
            (None, Some(prev)) => prev.get_variable(name),
            (None, None) => None,
        }
    }

    pub fn new() -> Self {
        let mut res = Self {
            previous: None,
            variables: HashMap::new(),
        };

        macro_rules! add_float {
            ($name:expr, $f:expr) => {
                res.variables.insert(
                    String::from($name),
                    Value::Scalar({
                        let mut r = Ratio::new();
                        r.assign_f64($f).unwrap();
                        r
                    }),
                );
            };
        }

        add_float!("pi", f64::PI());
        add_float!("e", f64::E());

        /*
        macro_rules! add_frac {
            ($name:expr, $a:expr, $b:expr) => {
                res.variables.insert(
                    String::from($name),
                    Value::Matrix(Matrix::from(Ratio::from((
                        Integer::from_usize($a).unwrap(),
                        Integer::from_usize($b).unwrap(),
                    )))),
                );
            };
        }
        */

        res
    }

    fn call_function(
        &self,
        f: &Function,
        args: impl Iterator<Item = Value> + ExactSizeIterator,
    ) -> Result<ExecutorResult, Error> {
        if args.len() != f.params().len() {
            return Err(
                format!("expected {} arguments got {}", f.params().len(), args.len()).into(),
            );
        }

        let ctx = Executor {
            previous: Some(self),
            variables: f
                .params()
                .iter()
                .zip(args)
                .map(|(param, value)| (param.to_string(), value))
                .collect(),
        };

        ctx.execute_expr(f.expr())
    }

    fn execute_unary(&self, op: UnOp, expr: &Expr) -> Result<ExecutorResult, Error> {
        let res = self.execute_expr(expr)?;

        fn for_all(
            res: ExecutorResult,
            f: impl Fn(Rational) -> Result<Rational, Error> + Clone + 'static,
        ) -> Result<ExecutorResult, Error> {
            let IterShape { iterator, len } = res.into_iter_shape()?;

            let values = iterator.map(move |v| f(v?));

            let new = Chain::Iterator(IterShape {
                iterator: Box::new(values),
                len,
            });
            Ok(new.into())
        }

        macro_rules! for_all {
            ($f:expr) => {
                for_all(res, $f)
            };
        }
        macro_rules! for_all_ok {
            ($f:expr) => {
                for_all!(|x: Ratio| Ok($f(x)))
            };
        }

        match op {
            UnOp::Id => Ok(res),
            UnOp::Neg => for_all_ok!(|x: Ratio| x.neg()),
            UnOp::Not => for_all_ok!(|x: Ratio| {
                if x.is_zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),

            UnOp::RollInt => for_all!(|x: Ratio| {
                if x <= Ratio::zero() {
                    return Err(Error::from("value must be greater than 0"));
                }

                let mut rng = rand::thread_rng();
                let upper: u128 = x
                    .into_integer()
                    .ok_or_else(|| Error::from("value must be an integer"))?
                    .to_u128()
                    .ok_or_else(|| Error::from("value too large to be rolled"))?;
                let val: u128 = rng.gen_range(0..=upper);
                Ratio::from_u128(val).ok_or_else(|| Error::from("couldn't convert u64 to ratio"))
            }),
            UnOp::RollFloat => for_all!(|x: Ratio| {
                let mut rng = rand::thread_rng();
                let val: f64 = rng.gen_range(0.0..=1.0);
                Ratio::from_f64(val)
                    .map(|val| x * val)
                    .ok_or_else(|| Error::from("couldn't convert f64 to ratio"))
            }),

            UnOp::Floor => for_all_ok!(|x: Ratio| x.floor()),
            UnOp::Ceil => for_all_ok!(|x: Ratio| x.ceil()),
            UnOp::Round => for_all_ok!(|x: Ratio| x.round()),
            UnOp::Abs => for_all_ok!(|x: Ratio| x.abs()),
            UnOp::Sin | UnOp::Cos | UnOp::Tan => for_all!(move |x: Ratio| {
                let f = to_float(x);
                let res = match op {
                    UnOp::Sin => f.sin(),
                    UnOp::Cos => f.cos(),
                    UnOp::Tan => f.tan(),
                    _ => unreachable!(),
                };
                res.to_rational()
                    .ok_or_else(|| Error::from("invalid result"))
            }),
            UnOp::Sign => for_all_ok!(|x: Ratio| x.signum()),

            UnOp::Iota => {
                let s = expect_scalar(res)?;
                let upper = to_usize_error(&s)?;

                let it = (1..=upper).map(|v| Ok(Ratio::from(v)));
                let len = upper;

                Ok(Chain::make_vector(Box::new(it), len).into())
            }

            UnOp::Rho => {
                let res = res.into_iter_shape()?;
                Ok(Chain::make_scalar(Rational::from(res.len)).into())
            }

            UnOp::Rev => {
                let m = Matrix::try_from(res)?;
                let len = m.len();
                let values = m.into_iter().rev().map(Result::Ok);
                Ok(Chain::Iterator(IterShape {
                    len,
                    iterator: Box::new(values),
                })
                .into())
            }

            UnOp::Up | UnOp::Down => {
                let IterShape { iterator, len } = res.into_iter_shape()?;
                let values: Result<Vec<(usize, Ratio)>, Error> = iterator
                    .enumerate()
                    .map(|(i, x)| x.map(|x| (i, x)))
                    .collect();
                let mut values = values?;

                if op == UnOp::Up {
                    values.sort_by(|a, b| a.1.cmp(&b.1))
                } else {
                    values.sort_by(|a, b| b.1.cmp(&a.1))
                }

                let values = values.into_iter().map(|(i, _)| Ok(Ratio::from(i)));

                Ok(Chain::Iterator(IterShape {
                    iterator: Box::new(values),
                    len,
                })
                .into())
            }
        }
    }

    fn execute_binary(&self, op: BinOp, a: &Expr, b: &Expr) -> Result<ExecutorResult, Error> {
        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        let a = self.execute_expr(a)?.try_into()?;
        let b = self.execute_expr(b)?.try_into()?;
        call_binary(op, a, b)
    }

    fn execute_fold_scan(
        &self,
        op: FoldOp,
        expr: &Expr,
        is_fold: bool,
    ) -> Result<ExecutorResult, Error> {
        let iter_shape = self.execute_expr(expr)?.into_iter_shape()?;
        if iter_shape.len < 1 {
            return Err(Error::from("matrix must have at least 1 value"));
        }

        fn apply(
            iter_shape: IterShape,
            is_fold: bool,
            f: impl Fn(Value, Value) -> Result<ExecutorResult, Error>,
        ) -> Result<ExecutorResult, Error> {
            if is_fold {
                let mut it = iter_shape.iterator;
                let first = it.next().unwrap();

                // fold
                it.try_fold(
                    ExecutorResult::Chain(Chain::make_scalar(first?)),
                    |acc, item| f(acc.try_into()?, item?.into()),
                )
            } else {
                let mut it = iter_shape.iterator;
                let mut accum = Vec::with_capacity(iter_shape.len);
                accum.push(it.next().unwrap().unwrap());

                loop {
                    let item = match it.next() {
                        None => break Ok(Matrix::make_vector(accum).into()),
                        Some(i) => i?,
                    };

                    let prev = Value::from(accum.last().unwrap().clone());
                    let curr = Value::from(item);

                    let res = f(prev, curr)?;
                    accum.push(expect_scalar(res)?)
                }
            }
        }
        macro_rules! apply {
            ($f:expr) => {
                apply(iter_shape, is_fold, $f)
            };
        }

        match op {
            FoldOp::BinOp(op) => apply!(|acc, item| call_binary(op, acc.into(), item.into())),

            FoldOp::Expr(expr) => {
                let f = expect_function_expr(self, &expr)?;
                apply!(|acc, item| {
                    let args = [acc, item];
                    let res = self.call_function(&f, args.into_iter())?;
                    Ok(res)
                })
            }
        }
    }

    fn execute_chunker(&self, op: FoldOp, expr: &Expr) -> Result<ExecutorResult, Error> {
        let iter_shape = self.execute_expr(expr)?.into_iter_shape()?;
        if iter_shape.len < 1 {
            return Ok(iter_shape.into());
        }

        fn apply(
            iter_shape: IterShape,
            chunk_size: usize,
            f: impl Fn(Vec<Value>) -> Result<ExecutorResult, Error>,
        ) -> Result<ExecutorResult, Error> {
            if iter_shape.len % chunk_size != 0 {
                return Err(Error::from(format!(
                    "left hand sides expects {} arguments, but the right hand side length ({}) is not divisible by that",
                    chunk_size,
                    iter_shape.len,
                )));
            }

            let chunks: Result<Vec<_>, Error> = iter_shape
                .iterator
                .chunks(chunk_size)
                .into_iter()
                .map(|args| -> Result<_, Error> {
                    let args: Result<Vec<Value>, _> =
                        args.into_iter().map(|v| v.map(Value::Scalar)).collect();

                    let args = args?;
                    assert_eq!(args.len(), chunk_size);

                    let res = f(args)?;
                    Ok(Chain::try_from(res)?.into_iter_shape()?.iterator)
                })
                .flatten_ok()
                .flatten_ok()
                .collect();
            collect_point!();

            let m = Matrix::make_vector(chunks?);
            Ok(m.into())
        }
        macro_rules! apply {
            ($chunk_size:expr, $f:expr) => {
                apply(iter_shape, $chunk_size, $f)
            };
        }

        match op {
            FoldOp::BinOp(op) => apply!(2, |args| {
                let (a, b) = args.into_iter().collect_tuple().unwrap();
                call_binary(op, a.into(), b.into())
            }),

            FoldOp::Expr(expr) => {
                let f = expect_function_expr(self, &expr)?;
                apply!(f.params().len(), |args| self
                    .call_function(&f, args.into_iter()))
            }
        }
    }

    fn execute_expr(&self, node: &Expr) -> Result<ExecutorResult, Error> {
        match node {
            Expr::Atom(Atom::Rat(v)) => Ok(Chain::make_scalar(v.clone()).into()),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.get_variable(s) {
                    None => return Err(format!("variable {} not found", s).into()),
                    Some(var) => var.clone(),
                };

                Ok(var.into())
            }

            Expr::Vector(v) => {
                let expressions: Vec<_> = v
                    .iter()
                    .map(|e| Value::try_from(self.execute_expr(e)?))
                    .collect::<Result<_, Error>>()?;
                collect_point!();

                match &expressions[0] {
                    Value::Function(_) => {
                        let mut e = expressions.into_iter();
                        let Value::Function(f) = e.next().unwrap() else {
                            unreachable!()
                        };
                        self.call_function(&f, e)
                    }

                    Value::Scalar(_) | Value::Matrix(_) => {
                        let len = expressions.len();
                        let mut expressions = expressions.into_iter();

                        if len == 1 {
                            return Ok(expressions.next().unwrap().into());
                        }

                        let values = expressions.map(|v| match v {
                            Value::Matrix(m) => m
                                .into_scalar()
                                .ok_or_else(|| Error::from("nested matrices aren't allowed")),
                            Value::Scalar(r) => Ok(r),
                            Value::Function(_) => Err(Error::from("expected scalar, got function")),
                        });

                        Ok(Chain::make_vector(Box::new(values), len).into())
                    }
                }
            }

            Expr::Unary(op, expr) => self.execute_unary(*op, expr),
            Expr::Binary(a, op, b) => self.execute_binary(*op, a, b),
            Expr::Fold(op, expr) => self.execute_fold_scan(op.clone(), expr, true),
            Expr::Scan(op, expr) => self.execute_fold_scan(op.clone(), expr, false),
            Expr::Map(op, expr) => self.execute_chunker(op.clone(), expr),

            Expr::Index(m, indices) => {
                let indices = self.execute_expr(indices)?.into_iter_shape()?;

                let m = Matrix::try_from(self.execute_expr(m)?)?;

                let indices: Vec<usize> = indices
                    .iterator
                    .map(|v| to_usize_error(&v?))
                    .collect::<Result<_, Error>>()?;

                match m.get_multiple(&indices) {
                    None => Err(Error::from("out of bounds")),
                    Some(i) => Ok(Matrix::make_vector(i).into()),
                }
            }

            Expr::Let(name, expr, body) => {
                let expr: Value = self.execute_expr(expr)?.try_into()?;
                self.call_function(
                    &Function::Lambda {
                        params: vec![name.clone()],
                        expr: *body.clone(),
                    },
                    iter::once(expr),
                )
            }

            Expr::Lambda(params, body) => Ok(Value::Function(Function::Lambda {
                params: params.clone(),
                expr: (**body).clone(),
            })
            .into()),
        }
    }

    pub fn execute(&mut self, node: Statement, remember: bool) -> Result<ExecutorResult, Error> {
        let is_conflict = |old: Option<&Value>, new_is_fun: bool| match old {
            None => false,
            Some(Value::Matrix(_)) => new_is_fun,
            Some(Value::Scalar(_)) => new_is_fun,
            Some(Value::Function(_)) => !new_is_fun,
        };

        macro_rules! err_var_exists {
            ($name:expr, $is_fun:expr) => {
                let old = self.get_variable(&$name);
                if is_conflict(old, $is_fun) {
                    return Err(
                        format!("variable {} already exists with different type", $name).into(),
                    );
                }
            };
        }

        let res = match node {
            Statement::Expr(e) => self.execute_expr(&e),

            Statement::Assign(var, val) => {
                err_var_exists!(var, matches!(val, Expr::Lambda(_, _)));
                let res = self.execute_expr(&val)?;
                let val: Value = res.clone().try_into()?;
                self.variables.insert(var, val);
                Ok(res)
            }

            Statement::FunDeclare(name, params, expr) => {
                err_var_exists!(name, true);
                let f = Function::Named {
                    name: name.clone(),
                    params,
                    expr,
                };
                self.variables.insert(name, Value::Function(f.clone()));
                Ok(f.into())
            }

            Statement::InternalCommand(command, body) => match command.as_str() {
                "n" | "number" => {
                    let parsed = parser::parse(&body)
                        .map_err(|_| Error::from("couldn't parse expression"))?;
                    let Some(parsed) = parsed else {
                        return Ok(ExecutorResult::None);
                    };
                    let res = self.execute(parsed, false)?;

                    let s = Matrix::try_from(res)?
                        .iter()
                        .map(|x| x.to_f64())
                        .enumerate()
                        .map(|(i, val)| {
                            if i == 0 {
                                format!("{}", val)
                            } else {
                                format!(" {}", val)
                            }
                        })
                        .collect::<Vec<String>>()
                        .concat();

                    Ok(ExecutorResult::Info(s))
                }

                "s" | "set" => {
                    let mut it: VecDeque<String> =
                        body.splitn(2, ' ').map(|x| x.to_owned()).collect();
                    if it.len() != 2 {
                        return Err(format!(
                            "expected key and value, got {} items instead",
                            it.len()
                        )
                        .into());
                    }
                    let key = it.pop_front().unwrap();
                    let value = it.pop_front().unwrap();
                    Ok(ExecutorResult::Setting(key, value))
                }

                "t" | "time" => {
                    let start = Instant::now();
                    let parsed =
                        parser::parse(&body).map_err(|_| Error::from("couldn't parse expression"));
                    eprintln!("parsing took {:?}", start.elapsed());
                    let Some(parsed) = parsed? else {
                        return Ok(ExecutorResult::None);
                    };
                    eprintln!("parsed as: {:?}", parsed);

                    let start = Instant::now();
                    let res = self.execute(parsed, false)?;
                    eprintln!("executing took {:?}", start.elapsed());

                    let start = Instant::now();
                    let m = Matrix::try_from(res)?;
                    eprintln!("collecting took {:?}", start.elapsed());

                    Ok(m.into())
                }

                cmd => Err(format!("unknown command {}", cmd).into()),
            },
        };

        if remember {
            let val = res.clone().and_then(Value::try_from);
            if let Ok(val) = val {
                self.variables.insert(String::from("_"), val);
            }
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! eval {
        ($exec:expr, $line:expr) => {{
            let s = parser::parse($line).unwrap().unwrap();
            $exec.execute(s, false).unwrap()
        }};
    }

    #[test]
    fn test_parse_prelude() {
        let lines = r#"
fn avg xs = (+//xs)/(rho xs)
fn sqrt x = x ** (1/2)
fn norm xs = sqrt (+//(xs ** 2))
fn head n xs = n rho xs
fn tail n xs = n drop xs
fn time x = (floor x) ((x-floor x) * 60)
fn fact n = *//iota n
fn binom n k = (fact n)/((fact k) * (fact (n-k)))
fn normalize v = v/(dist v)
fn dot v1 v2 = +//(v1 * v2)
fn frombase b digs = +//(digs * (b ** ((rev iota rho digs) - 1)))
fn timesecs xs = frombase 60 xs
fn identity size = size size rho ((iota (size+1)) == 1)
fn bitmask n = rev (2 ** ((iota n)-1))
        "#;

        let mut exec = Executor::new();

        for l in lines.lines() {
            let Some(p) = parser::parse(l).unwrap() else {
                continue;
            };
            let res = exec.execute(p, true).unwrap();
            assert!(matches!(res, ExecutorResult::Chain(_)));
        }
    }

    #[test]
    fn test_prelude_avg() {
        let mut exec = Executor::new();

        eval!(exec, "fn avg xs = (+//xs)/(rho xs)");

        let res = eval!(exec, "avg (5 5)");
        assert!(res.into_iter_shape().unwrap().into_scalar().unwrap() == 5);
    }

    #[test]
    fn test_map() {
        let mut exec = Executor::new();

        eval!(exec, "fn f a b = a + b");

        let res = eval!(exec, "f . (1 1 2 2)");
        let res: Result<Vec<_>, _> = res.into_iter_shape().unwrap().iterator.collect();
        let res = res.unwrap();

        assert_eq!(res, vec![2, 4]);
    }

    #[test]
    fn test_lambda_call() {
        let mut exec = Executor::new();

        let res = eval!(exec, r#"(\a -> a + 1) 1"#);
        let res = res.into_iter_shape().unwrap().into_scalar().unwrap();

        assert_eq!(res, 2);
    }
}
