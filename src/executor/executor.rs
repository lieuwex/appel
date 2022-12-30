use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Deref, DerefMut, Neg};

use crate::ast::*;
use crate::executor::chain::{Chain, IterShape, ValueIter};
use crate::parser;

use num_traits::*;
use replace_with::replace_with_or_default_and_return;
use rug::{float, Float, Integer, Rational};

use rand::prelude::*;

use super::function::Function;
use super::matrix::Matrix;
use super::result::ExecutorResult;
use super::value::Value;

const FLOAT_PRECISION: u32 = 100;

pub fn to_f64(val: &Ratio) -> Option<f64> {
    Some(val.to_f64())
}

fn pow(a: Ratio, b: Ratio) -> Option<Ratio> {
    let b = b.to_i32()?;
    let res = a.pow(b);
    return Some(Ratio::from(res));
}
fn log(base: Ratio, n: Ratio) -> Option<Ratio> {
    let base = Float::new(FLOAT_PRECISION).add(base);
    let n = Float::new(FLOAT_PRECISION).add(n);

    let res = n.log2() / base.log2();
    res.to_rational()
}

#[derive(Clone)]
pub struct Executor {
    variables: HashMap<String, Value>,
}

fn expect_vector(v: ExecutorResult) -> Result<Vec<Ratio>, String> {
    let m = Matrix::try_from(v)?;
    match m.shape.len() {
        0 | 1 => Ok(m.values),
        _ => Err(String::from("expected vector, got matrix")),
    }
}
fn expect_scalar(v: ExecutorResult) -> Result<Ratio, String> {
    match Matrix::try_from(v)?.into_scalar() {
        None => Err(String::from("expected scalar")),
        Some(s) => Ok(s),
    }
}

fn to_usize_error(rat: &Ratio) -> Result<usize, String> {
    let int = rat
        .to_integer()
        .ok_or_else(|| "value is not an integer".to_owned())?;
    int.to_usize()
        .ok_or_else(|| "value too large for usize".to_owned())
}

fn get_comp_op_fn(op: CompOp) -> impl Fn(Ratio, Ratio) -> Ratio {
    let fun: &dyn Fn(Ratio, Ratio) -> bool = match op {
        CompOp::Eq => &|a, b| a == b,
        CompOp::Neq => &|a, b| a != b,
        CompOp::Lt => &|a, b| a < b,
        CompOp::Le => &|a, b| a <= b,
        CompOp::Gt => &|a, b| a > b,
        CompOp::Ge => &|a, b| a >= b,
    };

    move |a: Ratio, b: Ratio| -> Ratio { Ratio::from_u8(fun(a, b) as u8).unwrap() }
}

fn call_binary(op: BinOp, a: Chain, b: Chain) -> Result<ExecutorResult, String> {
    let a = a.into_iter_shape()?;
    let b = b.into_iter_shape()?;

    let get_int = |m: Matrix| -> Result<Integer, String> {
        let s = m
            .into_scalar()
            .ok_or_else(|| String::from("expected scalar"))?;
        s.into_integer()
            .ok_or_else(|| String::from("expected integer"))
    };

    let matrix_to_res = |m: Matrix| -> ExecutorResult { ExecutorResult::Value(Value::Matrix(m)) };

    macro_rules! apply {
        ($f:expr) => {{
            if a.shape == b.shape {
                let values = a
                    .iterator
                    .zip(b.iterator)
                    .map(|(a, b)| (a.unwrap(), b.unwrap()))
                    .map(move |(a, b): (Rational, Rational)| $f(a, b).map(Ratio::from));

                let matrix = Chain::MatrixIterator {
                    iterator: Box::new(values),
                    shape: a.shape,
                };
                return Ok(matrix.into_result());
            }

            let (scalar, non_scalar, scalar_is_left) = if let Some(s) = a.scalar() {
                (s, b, true)
            } else if let Some(s) = b.scalar() {
                (s, a, false)
            } else {
                return Err(String::from("rank mismatch"));
            };

            let matrix = Chain::MatrixIterator {
                iterator: Box::new(non_scalar.iterator.map(Result::unwrap).map(
                    move |v: Rational| {
                        if scalar_is_left {
                            $f(scalar.clone(), v).map(Ratio::from)
                        } else {
                            $f(v, scalar.clone()).map(Ratio::from)
                        }
                    },
                )),
                shape: non_scalar.shape,
            };

            Ok(matrix.into_result())
        }};
    }
    macro_rules! apply_ok {
        ($f:expr) => {
            apply!(|a, b| Ok($f(a, b)))
        };
    }

    match op {
        BinOp::Add => apply_ok!(|a, b| a + b),
        BinOp::Sub => apply_ok!(|a, b| a - b),
        BinOp::Mul => apply_ok!(|a, b| a * b),
        BinOp::Div => apply_ok!(|a, b| a / b),
        BinOp::Mod => apply_ok!(|a: Ratio, b: Ratio| (a / b).rem_trunc()),
        BinOp::Pow => {
            apply!(|a, b| pow(a, b).ok_or_else(|| "error while converting to i32".to_owned()))
        }
        BinOp::Log => apply!(|a, b| log(a, b).ok_or_else(|| "result is not finite".to_owned())),
        BinOp::CompOp(x) => apply_ok!(get_comp_op_fn(x)),

        BinOp::Concat => {
            if a.shape.len() != b.shape.len() {
                return Err(String::from("rank mismatch"));
            }

            let values = a.iterator.chain(b.iterator);
            Ok(Chain::MatrixIterator {
                iterator: Box::new(values),
                shape: a.shape,
            }
            .into_result())
        }

        BinOp::Drop => {
            let scalar = a
                .into_scalar()
                .ok_or_else(|| String::from("expected vector, got matrix"))?;
            let n = scalar
                .into_integer()
                .ok_or_else(|| "value must be an integer".to_owned())?;

            let (n, at_start) = if (&n).signum() == -1 {
                (n.neg().into(), false)
            } else {
                (n, true)
            };
            let n = n
                .to_usize()
                .ok_or_else(|| "value too large for usize".to_owned())?;

            let values: Box<dyn ValueIter> = if at_start {
                Box::new(b.iterator.skip(n))
            } else {
                let amount = b.len() - n;
                Box::new(b.iterator.take(amount))
            };

            Ok(Chain::MatrixIterator {
                iterator: values,
                shape: b.shape,
            }
            .into_result())
        }

        BinOp::Rho => {
            let shape: Vec<usize> = a
                .iterator
                .map(Result::unwrap)
                .map(|v| to_usize_error(&v))
                .collect::<Result<_, String>>()?;

            let values = std::iter::repeat(b.iterator)
                .flatten()
                .take(shape.iter().product());

            Ok(Chain::MatrixIterator {
                iterator: Box::new(values),
                shape,
            }
            .into_result())
        }

        BinOp::Unpack => {
            todo!()

            /*
            let a = get_int(a)?;
            let b = get_int(b)?;

            let radix = a
                .to_i32()
                .and_then(|r| if 2 <= r && r <= 256 { Some(r) } else { None })
                .ok_or(String::from("radix must be greater than or equal to 2"))?;

            let sign = if b.is_negative() { -1 } else { 1 };
            let bytes: Vec<u8> = {
                let mut v = Vec::with_capacity(b.significant_digits::<u8>());
                b.write_digits(&mut v, rug::integer::Order::Msf);
                v
            };

            let values: Vec<Ratio> = bytes
                .into_iter()
                .map(|b| Ratio::from_u8(b).unwrap())
                .map(|b| if sign == -1 { b.neg() } else { b })
                .collect();

            Ok(Matrix::make_vector(values).into())
            */
        }
        BinOp::Pack => {
            todo!()

            /*
            let a = get_int(a)?;
            let b = {
                let b = expect_vector(matrix_to_res(b))?;
                let b: Option<Vec<Integer>> = b.into_iter().map(|b| b.into_integer()).collect();
                b.ok_or_else(|| "all unpacked values should be integers".to_owned())?
            };

            let radix = a
                .to_i32()
                // check variant a
                .and_then(|r| if 2 <= r && r <= 256 { Some(r) } else { None })
                .ok_or(String::from("radix must be greater than or equal to 2"))?;

            let is_negative = b.iter().any(|b| b.is_negative());
            let bytes: Vec<u8> = b.into_iter().map(|b| b.to_u8().unwrap()).collect();

            // check variant b
            if let Some((idx, val)) = bytes
                .iter()
                .enumerate()
                .find(|(_, &x)| !(0 < x && (x as i32) < radix))
            {
                return Err(format!(
                    "Value at index {idx} is out-of-bounds, value is {val}, radix is {radix}"
                ));
            }

            let packed = {
                let mut res = Integer::new();

                // SAFETY: assign_bytes_radix_unchecked assumes two things:
                // (a) 2 <= radix <= 256
                // (b) all bytes are 0 <= x < radix
                // We check both.
                unsafe {
                    res.assign_bytes_radix_unchecked(&bytes, radix, is_negative);
                }

                if is_negative {
                    res.neg()
                } else {
                    res
                }
            };

            Ok(Matrix::from(Ratio::from(packed)).into())
            */
        }

        BinOp::In => {
            let base_set: Result<HashSet<Ratio>, String> = b.iterator.collect();
            let base_set = base_set?;
            let values = a
                .iterator
                .map(Result::unwrap)
                .map(move |x| {
                    if base_set.contains(&x) {
                        Ratio::one()
                    } else {
                        Ratio::zero()
                    }
                })
                .map(Result::Ok);

            Ok(Chain::MatrixIterator {
                iterator: Box::new(values),
                shape: a.shape,
            }
            .into_result())
        }

        BinOp::Max => apply_ok!(|a: Ratio, b: Ratio| if b > a { b } else { a }),
        BinOp::Min => apply_ok!(|a: Ratio, b: Ratio| if b < a { b } else { a }),

        BinOp::Pad => {
            let a = Matrix::try_from(a)?;
            let a = expect_vector(ExecutorResult::Value(Value::Matrix(a)))?;
            if a.len() != 2 {
                return Err(format!("expected 2 arguments on the left, got {}", a.len()));
            }

            let mut it = a.into_iter();
            let amount = it
                .next()
                .unwrap()
                .into_integer()
                .expect("amount must be an integer");
            let number = it.next().unwrap();

            let (amount, at_start) = if (&amount).signum() == -1 {
                (amount.neg(), false)
            } else {
                (amount, true)
            };

            let values: Result<Vec<_>, String> = b.iterator.collect();
            let mut values: Vec<_> = values?;

            let mut to_add = vec![number; amount.to_usize().unwrap()];
            if at_start {
                to_add.append(&mut values);
                values = to_add;
            } else {
                values.append(&mut to_add);
            }

            Ok(Matrix {
                values,
                shape: b.shape,
            }
            .into())
        }

        BinOp::Map => unreachable!(),
    }
}

impl Executor {
    pub fn new() -> Self {
        let mut res = Self {
            variables: HashMap::new(),
        };

        macro_rules! add_float {
            ($name:expr, $f:expr) => {
                res.variables.insert(
                    String::from($name),
                    Value::Matrix(Matrix::from({
                        let mut r = Ratio::new();
                        r.assign_f64($f).unwrap();
                        r
                    })),
                );
            };
        }

        add_float!("pi", f64::PI());
        add_float!("e", f64::E());

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

        res
    }

    fn call_function(&self, f: &Function, args: Vec<Chain>) -> Result<ExecutorResult, String> {
        let args: Result<Vec<Matrix>, _> = args.into_iter().map(Matrix::try_from).collect();
        let args = args?;

        if args.len() != f.params.len() {
            return Err(format!(
                "expected {} arguments got {}",
                f.params.len(),
                args.len()
            ));
        }

        let mut ctx = self.clone();
        for (param, matrix) in f.params.iter().zip(args) {
            ctx.variables
                .insert(param.to_string(), Value::Matrix(matrix));
        }

        ctx.execute_expr(&f.expr)
    }

    fn execute_unary(&mut self, op: UnOp, expr: &Expr) -> Result<ExecutorResult, String> {
        let res = self.execute_expr(expr)?;

        macro_rules! for_all {
            ($f:expr) => {{
                let IterShape { iterator, shape } = res.into_iter_shape()?;

                // TODO: remove unwrap
                let values = iterator.map(|x| x.unwrap()).map($f);

                let new = Chain::MatrixIterator {
                    iterator: Box::new(values),
                    shape,
                };
                Ok(new.into_result())
            }};
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
                if x == Ratio::zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),

            UnOp::RollInt => for_all!(|x: Ratio| {
                if x <= Ratio::zero() {
                    return Err("value must be greater than 0".to_owned());
                }

                let mut rng = rand::thread_rng();
                let upper: u64 = x
                    .into_integer()
                    .ok_or_else(|| "value must be an integer".to_owned())?
                    .to_u64()
                    .ok_or_else(|| "value too large to be rolled".to_owned())?;
                let val: u64 = rng.gen_range(0..=upper);
                Ratio::from_u64(val).ok_or_else(|| "couldn't convert u64 to ratio".to_owned())
            }),
            UnOp::RollFloat => for_all!(|x: Ratio| {
                if x <= Ratio::zero() {
                    return Err("value must be greater than 0".to_owned());
                }

                let mut rng = rand::thread_rng();
                let val: f64 = rng.gen_range(0.0..=1.0);
                Ratio::from_f64(val)
                    .map(|val| x * val)
                    .ok_or_else(|| "couldn't convert f64 to ratio".to_owned())
            }),

            UnOp::Floor => for_all_ok!(|x: Ratio| x.floor()),
            UnOp::Ceil => for_all_ok!(|x: Ratio| x.ceil()),
            UnOp::Abs => for_all_ok!(|x: Ratio| x.abs()),
            UnOp::Sin | UnOp::Cos | UnOp::Tan => for_all!(move |x: Ratio| {
                let f = to_f64(&x).ok_or_else(|| "couldn't convert fo f64".to_owned())?;
                let res = match op {
                    UnOp::Sin => f.sin(),
                    UnOp::Cos => f.cos(),
                    UnOp::Tan => f.tan(),
                    _ => unreachable!(),
                };
                Ratio::from_f64(res).ok_or_else(|| "invalid result".to_owned())
            }),
            UnOp::Sign => for_all_ok!(|x: Ratio| x.signum()),

            UnOp::Iota => {
                let s = expect_scalar(res)?;
                let upper = to_usize_error(&s)?;

                let it = (1..=upper).map(|v| Ratio::from_usize(v).ok_or_else(|| String::new()));
                let len = upper;

                Ok(Chain::make_vector(Box::new(it), len).into_result())
            }

            UnOp::Rho => {
                let res = res.into_iter_shape()?;

                let new_shape: Vec<usize> = vec![res.len()];
                let iterator = res
                    .shape
                    .into_iter()
                    .map(|s| Ratio::from_usize(s).unwrap())
                    .map(Ok);

                Ok(Chain::MatrixIterator {
                    iterator: Box::new(iterator),
                    shape: new_shape,
                }
                .into_result())
            }

            UnOp::Rev => {
                let m = Matrix::try_from(res)?;
                let values = m.values.into_iter().rev().map(Result::Ok);
                Ok(Chain::MatrixIterator {
                    iterator: Box::new(values),
                    shape: m.shape,
                }
                .into_result())
            }

            UnOp::Up | UnOp::Down => {
                let IterShape { iterator, shape } = res.into_iter_shape()?;
                let values: Result<Vec<(usize, Ratio)>, String> = iterator
                    .enumerate()
                    .map(|(i, x)| x.map(|x| (i, x)))
                    .collect();
                let mut values = values?;

                if op == UnOp::Up {
                    values.sort_by(|a, b| a.1.cmp(&b.1))
                } else {
                    values.sort_by(|a, b| b.1.cmp(&a.1))
                }

                let values = values
                    .into_iter()
                    .map(move |v| Ratio::from_usize(v.0).unwrap())
                    .map(Result::Ok);

                Ok(Chain::MatrixIterator {
                    iterator: Box::new(values),
                    shape,
                }
                .into_result())
            }

            UnOp::Ravel => {
                let IterShape { iterator, shape } = res.into_iter_shape()?;
                let len: usize = shape.into_iter().sum();
                Ok(Chain::make_vector(Box::new(iterator), len).into_result())
            }
        }
    }

    fn execute_binary(&mut self, op: BinOp, a: &Expr, b: &Expr) -> Result<ExecutorResult, String> {
        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        let a = self.execute_expr(a)?.into_chain()?;
        let b = self.execute_expr(b)?.into_chain()?;
        call_binary(op, a, b)
    }

    fn execute_fold_scan(
        &mut self,
        op: FoldOp,
        expr: &Expr,
        is_fold: bool,
    ) -> Result<ExecutorResult, String> {
        let iter_shape = self.execute_expr(expr)?.into_iter_shape()?;
        if iter_shape.len() < 1 {
            return Err(String::from("matrix must have at least 1 value"));
        }

        macro_rules! apply {
            ($f:expr) => {{
                if is_fold {
                    let mut it = iter_shape.iterator;
                    let first = it.next().unwrap();

                    // fold
                    it.try_fold(
                        ExecutorResult::Chain(Chain::make_scalar(first?)),
                        |acc, item| -> Result<ExecutorResult, String> {
                            let acc = acc.into_chain()?;
                            let item = Chain::make_scalar(item?);
                            $f(acc, item)
                        },
                    )
                } else {
                    let items: Vec<Ratio> = iter_shape.iterator.map(Result::unwrap).collect();
                    let mut accum = vec![items[0].clone()];

                    let mut i = 1;
                    loop {
                        if i >= items.len() {
                            break Ok(Matrix {
                                values: accum,
                                shape: iter_shape.shape,
                            }
                            .into());
                        }

                        let prev = Chain::make_scalar(accum[i - 1].clone());
                        let curr = Chain::make_scalar(items[i].clone());

                        match $f(prev, curr) {
                            e @ Err(_) => break e,
                            Ok(res) => accum.push(expect_scalar(res)?),
                        };

                        i += 1;
                    }
                }
            }};
        }

        match op {
            FoldOp::BinOp(op) => apply!(|acc, item| call_binary(op, acc, item)),

            FoldOp::FunctionRef(f) => match self.variables.get(&f) {
                None => Err(format!("variable {} not found", f)),
                Some(v) => match v {
                    Value::Matrix(_) => Err(String::from("variable is a matrix")),
                    Value::Function(f) => {
                        if f.params.len() != 2 {
                            return Err(String::from("function does not take 2 params"));
                        }

                        apply!(|acc, item| {
                            let args = vec![acc, item];
                            let res = self.call_function(f, args)?;
                            Ok(res)
                        })
                    }
                },
            },
        }
    }

    fn execute_map(&mut self, a: &Expr, b: &Expr) -> Result<ExecutorResult, String> {
        let f = match self.execute_expr(a)?.unwrap_value() {
            Value::Function(f) => f,
            _ => return Err(String::from("expected left hand to be a function")),
        };
        let mut x = Matrix::try_from(self.execute_expr(b)?)?;

        for value in &mut x.values {
            replace_with_or_default_and_return(value, |value| {
                let res: Result<Rational, String> = (|| {
                    let res = self.call_function(&f, vec![Chain::make_scalar(value)])?;
                    res.into_iter_shape()?
                        .into_scalar()
                        .ok_or(format!("Function returned non-scalar in map"))
                })();

                match res {
                    Ok(r) => (Ok(()), r),
                    Err(e) => (Err(e), Rational::default()),
                }
            })?;
        }

        Ok(ExecutorResult::from(x))
    }

    fn execute_expr(&mut self, node: &Expr) -> Result<ExecutorResult, String> {
        match node {
            Expr::Atom(Atom::Rat(v)) => Ok(Matrix::from(v.clone()).into()),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.variables.get(s) {
                    None => return Err(format!("variable {} not found", s)),
                    Some(var) => var.clone(),
                };

                match var {
                    Value::Matrix(m) => Ok(m.into()),
                    Value::Function(f) => Ok(ExecutorResult::Value(Value::Function(f))),
                }
            }

            Expr::Vector(v) => {
                let mut expressions = Vec::with_capacity(v.len());
                for e in v.into_iter() {
                    expressions.push(self.execute_expr(e)?.unwrap_value());
                }

                match expressions[0].clone() {
                    Value::Function(f) => {
                        let mut args = Vec::with_capacity(expressions.len() - 1);
                        for e in expressions.into_iter().skip(1) {
                            args.push(Chain::Value(e));
                        }

                        self.call_function(&f, args)
                    }

                    Value::Matrix(first) => {
                        if expressions.len() == 1 {
                            return Ok(first.into());
                        }

                        let values: Result<Vec<_>, String> = expressions
                            .into_iter()
                            .map(|e| match Matrix::try_from(e)?.into_scalar() {
                                None => Err(String::from("nested matrices aren't allowed")),
                                Some(s) => Ok(s),
                            })
                            .collect();

                        Ok(Matrix::make_vector(values?).into())
                    }
                }
            }

            Expr::Unary(op, expr) => self.execute_unary(*op, expr),
            Expr::Binary(a, BinOp::Map, b) => self.execute_map(a, b),
            Expr::Binary(a, op, b) => self.execute_binary(*op, a, b),
            Expr::Fold(op, expr) => self.execute_fold_scan(op.clone(), expr, true),
            Expr::Scan(op, expr) => self.execute_fold_scan(op.clone(), expr, false),

            Expr::Index(m, indices) => {
                let indices = expect_vector(self.execute_expr(indices)?)?;

                if indices.iter().any(|i| !i.is_integer()) {
                    return Err(String::from("expected integers"));
                }

                let m = Matrix::try_from(self.execute_expr(m)?)?;

                if indices.len() != m.shape.len() {
                    return Err(String::from("rank mismatch"));
                }

                let indices: Vec<usize> = indices
                    .into_iter()
                    .map(|v| to_usize_error(&v))
                    .collect::<Result<_, String>>()?;

                match m.get_at(indices) {
                    None => Err(String::from("out of bounds")),
                    Some(i) => Ok(Matrix::from(i.clone()).into()),
                }
            }
        }
    }

    pub fn execute(&mut self, node: Statement, remember: bool) -> Result<ExecutorResult, String> {
        let is_conflict = |old: Option<&Value>, new_is_fun: bool| match old {
            None => false,
            Some(old) => match old {
                Value::Matrix(_) => new_is_fun,
                Value::Function(_) => !new_is_fun,
            },
        };

        macro_rules! err_var_exists {
            ($name:expr, $is_fun:expr) => {
                let old = self.variables.get(&$name);
                if is_conflict(old, $is_fun) {
                    return Err(format!(
                        "variable {} already exists with different type",
                        $name
                    ));
                }
            };
        }

        let res = match node {
            Statement::Expr(e) => self.execute_expr(&e),

            Statement::Assign(var, val) => {
                err_var_exists!(var, false);
                let res = self.execute_expr(&val)?;
                let val: Value = res.clone().into_chain()?.try_into()?;
                self.variables.insert(var, val);
                Ok(res)
            }

            Statement::FunDeclare(name, params, expr) => {
                err_var_exists!(name, true);
                let f = Function {
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
                        .or_else(|_| Err("couldn't parse expression".to_owned()))?;
                    if parsed.is_none() {
                        return Ok(ExecutorResult::None);
                    }
                    let res = self.execute(parsed.unwrap(), false)?;

                    let s = Matrix::try_from(res)?
                        .values
                        .iter()
                        .map(|x| (to_f64(x), x))
                        .enumerate()
                        .map(|(i, (val, orig))| {
                            let s = match val {
                                None => format!("{}", orig),
                                Some(f) => format!("{}", f),
                            };

                            if i == 0 {
                                s
                            } else {
                                format!(" {}", s)
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
                        ));
                    }
                    let key = it.pop_front().unwrap();
                    let value = it.pop_front().unwrap();
                    Ok(ExecutorResult::Setting(key, value))
                }

                cmd => Err(format!("unknown command {}", cmd)),
            },
        };

        if remember {
            let val = res
                .clone()
                .and_then(|r| r.into_chain())
                .and_then(Value::try_from);
            if let Ok(Value::Matrix(m)) = val {
                self.variables.insert(String::from("_"), Value::Matrix(m));
            }
        }

        res
    }
}
