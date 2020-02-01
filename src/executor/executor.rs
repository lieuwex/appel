use std::collections::HashMap;
use std::convert::TryFrom;
use std::ops::Neg;

use crate::ast::*;
use crate::parser;

use num_bigint::{BigInt, Sign};
use num_traits::cast::{FromPrimitive, ToPrimitive};
use num_traits::identities::{One, Zero};
use num_traits::pow::Pow;
use num_traits::sign::Signed;

use rand::prelude::*;

use super::function::Function;
use super::matrix::Matrix;
use super::result::ExecutorResult;
use super::value::Value;

pub fn to_f64(val: &Ratio) -> Option<f64> {
    let (num, den) = (val.numer(), val.denom());
    let fnum = num.to_f64()?;
    let fden = den.to_f64()?;
    Some(fnum / fden)
}
fn pow(a: &Ratio, b: &Ratio) -> Option<Ratio> {
    if a.is_integer() && b.is_integer() {
        let a = a.to_integer();
        let b = b.to_integer();

        let (b, is_neg) = match b.to_biguint() {
            Some(b) => (Some(b), false),
            None => (b.neg().to_biguint(), true),
        };

        if let Some(b) = b {
            let res = if is_neg { 1 / a.pow(b) } else { a.pow(b) };
            return Some(Ratio::from(res));
        }
    }

    let a = to_f64(a)?;
    let b = to_f64(b)?;
    let res = a.powf(b);
    Ratio::from_f64(res)
}
fn log(base: &Ratio, n: &Ratio) -> Option<Ratio> {
    let base = to_f64(base)?;
    let n = to_f64(n)?;
    let res = n.log(base);
    Ratio::from_f64(res)
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
    match Matrix::try_from(v)?.scalar() {
        None => Err(String::from("expected scalar")),
        Some(s) => Ok(s.clone()),
    }
}

fn to_usize_error(rat: &Ratio) -> Result<usize, String> {
    if !rat.is_integer() {
        Err("value is not an integer".to_owned())
    } else {
        let int = rat.to_integer();
        int.to_usize().ok_or("value too large for usize".to_owned())
    }
}

fn get_comp_op_fn(op: CompOp) -> impl Fn(&Ratio, &Ratio) -> Ratio {
    let fun: &dyn Fn(&Ratio, &Ratio) -> bool = match op {
        CompOp::Eq => &|a, b| a == b,
        CompOp::Neq => &|a, b| a != b,
        CompOp::Lt => &|a, b| a < b,
        CompOp::Le => &|a, b| a <= b,
        CompOp::Gt => &|a, b| a > b,
        CompOp::Ge => &|a, b| a >= b,
    };

    move |a: &Ratio, b: &Ratio| -> Ratio { Ratio::from_u8(fun(a, b) as u8).unwrap() }
}

fn call_binary(op: BinOp, a: Matrix, b: Matrix) -> Result<ExecutorResult, String> {
    let get_int = |m: Matrix| -> Result<BigInt, String> {
        let s = expect_scalar(ExecutorResult::Value(Value::Matrix(m)))?;
        if s.is_integer() {
            Ok(s.to_integer())
        } else {
            Err(String::from("expected integer"))
        }
    };

    let matrix_to_res = |m: Matrix| -> ExecutorResult { ExecutorResult::Value(Value::Matrix(m)) };

    macro_rules! apply {
        ($f:expr) => {{
            if a.shape == b.shape {
                let values = a
                    .values
                    .iter()
                    .zip(b.values)
                    .map(|(a, b)| $f(a, &b))
                    .collect::<Result<_, String>>()?;

                let matrix = Matrix {
                    values,
                    shape: a.shape,
                };
                return Ok(matrix.into());
            }

            if !a.is_scalar() && !b.is_scalar() {
                return Err(String::from("rank mismatch"));
            }

            // Since:
            //   !(!scalar(a) && !scalar(b)) => (scalar(a) || scalar(b))
            //
            // We can assume that the unwrap never fails.
            let (scalar, non_scalar, scalar_is_left) = if a.is_scalar() {
                (a.scalar().unwrap(), b, true)
            } else {
                (b.scalar().unwrap(), a, false)
            };

            let matrix = Matrix {
                values: non_scalar
                    .values
                    .iter()
                    .map(|v| {
                        if scalar_is_left {
                            $f(scalar, v)
                        } else {
                            $f(v, scalar)
                        }
                    })
                    .collect::<Result<_, String>>()?,
                shape: non_scalar.shape,
            };

            Ok(matrix.into())
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
        BinOp::Mod => apply_ok!(|a, b| a % b),
        BinOp::Pow => apply!(|a, b| pow(a, b).ok_or("error while converting to float".to_owned())),
        BinOp::Log => apply!(|a, b| log(a, b).ok_or("error while converting to float".to_owned())),
        BinOp::CompOp(x) => apply_ok!(get_comp_op_fn(x)),

        BinOp::Concat => {
            if a.shape != b.shape {
                return Err(String::from("rank mismatch"));
            }

            let values: Vec<Ratio> = a.values.into_iter().chain(b.values).collect();
            Ok(Matrix {
                values,
                shape: a.shape,
            }
            .into())
        }

        BinOp::Skip => {
            let scalar = expect_scalar(ExecutorResult::Value(Value::Matrix(a)))?;
            let n = to_usize_error(&scalar)?;

            Ok(Matrix {
                values: b.values.into_iter().skip(n).collect(),
                shape: b.shape,
            }
            .into())
        }

        BinOp::Rho => {
            let shape: Vec<usize> = a
                .values
                .iter()
                .map(|v| to_usize_error(v))
                .collect::<Result<_, String>>()?;

            let values: Vec<Ratio> = std::iter::repeat(b.values)
                .flatten()
                .take(shape.iter().product())
                .collect();

            Ok(Matrix { values, shape }.into())
        }

        BinOp::Unpack => {
            let a = get_int(a)?;
            let b = get_int(b)?;

            let radix = a.to_u32().unwrap_or(std::u32::MAX);
            if radix < 2 {
                return Err(String::from("radix must be greater than or equal to 2"));
            }

            let (sign, bits) = b.to_radix_be(radix);

            let values: Vec<Ratio> = bits
                .into_iter()
                .map(|b| Ratio::from_u8(b).unwrap())
                .map(|b| {
                    if sign == num_bigint::Sign::Minus {
                        b.neg()
                    } else {
                        b
                    }
                })
                .collect();

            Ok(Matrix::make_vector(values).into())
        }
        BinOp::Pack => {
            let a = get_int(a)?;
            let b = expect_vector(matrix_to_res(b))?;

            let sign = if b.iter().any(|b| b.to_integer().sign() == Sign::Minus) {
                Sign::Minus
            } else {
                Sign::Plus
            };

            let bits: Vec<u8> = b.iter().map(|b| b.to_integer().to_u8().unwrap()).collect();

            let packed = BigInt::from_radix_be(sign, &bits, a.to_u32().unwrap_or(std::u32::MAX));
            let int = match packed {
                None => return Err(String::from("couldn't convert bits to int")),
                Some(i) => i,
            };
            Ok(Matrix::from(Ratio::from_integer(int)).into())
        }
    }
}

impl Executor {
    pub fn new() -> Self {
        let mut res = Self {
            variables: HashMap::new(),
        };

        macro_rules! add_frac {
            ($name:expr, $a:expr, $b:expr) => {
                res.variables.insert(
                    String::from($name),
                    Value::Matrix(Matrix::from(Ratio::new_raw(
                        BigInt::from_usize($a).unwrap(),
                        BigInt::from_usize($b).unwrap(),
                    ))),
                );
            };
        }

        add_frac!("pi", 2646693125139304345, 842468587426513207);
        add_frac!("e", 28875761731, 10622799089);

        res
    }

    fn call_function(
        &self,
        f: Function,
        args: impl IntoIterator<Item = Matrix>,
    ) -> Result<ExecutorResult, String> {
        let mut ctx = self.clone();
        for (param, matrix) in f.params.iter().zip(args) {
            ctx.variables
                .insert(param.to_string(), Value::Matrix(matrix));
        }
        ctx.execute_expr(f.expr)
    }

    fn execute_unary(&mut self, op: UnOp, expr: Expr) -> Result<ExecutorResult, String> {
        let res = self.execute_expr(expr)?;

        macro_rules! for_all {
            ($f:expr) => {{
                let val = Matrix::try_from(res)?;

                let values: Result<Vec<Ratio>, String> = val.values.into_iter().map($f).collect();
                let values = match values {
                    Err(e) => return Err(e),
                    Ok(s) => s,
                };

                let new = Matrix {
                    values,
                    shape: val.shape,
                };
                Ok(new.into())
            }};
        }
        macro_rules! for_all_ok {
            ($f:expr) => {
                for_all!(&|x: Ratio| Ok($f(x)))
            };
        }

        match op {
            UnOp::Id => Ok(res),
            UnOp::Neg => for_all_ok!(&|x: Ratio| x.neg()),
            UnOp::Not => for_all_ok!(&|x: Ratio| {
                if x == Ratio::zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),

            UnOp::RollInt => for_all!(&|x: Ratio| {
                if x <= Ratio::zero() {
                    return Err("value must be greater than 0".to_owned());
                }

                let mut rng = rand::thread_rng();
                let upper: u64 = x
                    .to_integer()
                    .to_u64()
                    .ok_or("value too large to be rolled".to_owned())?;
                let val: u64 = rng.gen_range(0, upper);
                Ratio::from_u64(val).ok_or("couldn't convert u64 to ratio".to_owned())
            }),
            UnOp::RollFloat => for_all!(&|x: Ratio| {
                if x <= Ratio::zero() {
                    return Err("value must be greater than 0".to_owned());
                }

                let mut rng = rand::thread_rng();
                let val: f64 = rng.gen_range(0.0, 1.0);
                Ratio::from_f64(val)
                    .map(|val| x * val)
                    .ok_or("couldn't convert f64 to ratio".to_owned())
            }),

            UnOp::Floor => for_all_ok!(&|x: Ratio| x.floor()),
            UnOp::Ceil => for_all_ok!(&|x: Ratio| x.ceil()),
            UnOp::Abs => for_all_ok!(&|x: Ratio| x.abs()),
            UnOp::Sin | UnOp::Cos | UnOp::Tan => for_all!(&|x: Ratio| {
                let f = to_f64(&x).ok_or("couldn't convert fo f64".to_owned())?;
                let res = match op {
                    UnOp::Sin => f.sin(),
                    UnOp::Cos => f.cos(),
                    UnOp::Tan => f.tan(),
                    _ => unreachable!(),
                };
                Ratio::from_f64(res).ok_or("invalid result".to_owned())
            }),
            UnOp::Sign => for_all_ok!(&|x: Ratio| x.signum()),

            UnOp::Iota => {
                let s = expect_scalar(res)?;
                let upper = to_usize_error(&s)?;
                let values: Vec<_> = (0..upper).filter_map(Ratio::from_usize).collect();
                Ok(Matrix::make_vector(values).into())
            }

            UnOp::Rho => {
                let m = Matrix::try_from(res)?;
                let values: Vec<Ratio> = m
                    .shape
                    .iter()
                    .map(|s| Ratio::from_usize(*s).unwrap())
                    .collect();
                Ok(Matrix::make_vector(values).into())
            }

            UnOp::Rev => {
                let m = Matrix::try_from(res)?;
                let values: Vec<Ratio> = m.values.into_iter().rev().collect();
                Ok(Matrix {
                    values,
                    shape: m.shape,
                }
                .into())
            }

            UnOp::Up | UnOp::Down => {
                let matrix = Matrix::try_from(res)?;
                let mut values: Vec<(usize, Ratio)> =
                    matrix.values.into_iter().enumerate().collect();

                values.sort_by(|a, b| a.1.cmp(&b.1));

                let values: Vec<Ratio> = (if op == UnOp::Up {
                    values
                } else {
                    values.into_iter().rev().collect()
                })
                .into_iter()
                .map(|v| Ratio::from_usize(v.0).unwrap())
                .collect();

                Ok(Matrix {
                    values,
                    shape: matrix.shape,
                }
                .into())
            }
        }
    }

    fn execute_binary(&mut self, op: BinOp, a: Expr, b: Expr) -> Result<ExecutorResult, String> {
        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        let a = Matrix::try_from(self.execute_expr(a)?)?;
        let b = Matrix::try_from(self.execute_expr(b)?)?;
        call_binary(op, a, b)
    }

    fn execute_fold(&mut self, op: FoldOp, expr: Expr) -> Result<ExecutorResult, String> {
        let matrix = Matrix::try_from(self.execute_expr(expr)?)?;
        if matrix.values.len() < 2 {
            return Err(String::from("matrix has to have at least 2 values"));
        }

        match op {
            FoldOp::BinOp(op) => {
                let mut it = matrix.values.into_iter();
                let first = it.next().unwrap();
                it.try_fold(
                    ExecutorResult::Value(Value::Matrix(Matrix::from(first))),
                    |acc, item| -> Result<ExecutorResult, String> {
                        let acc = Matrix::try_from(acc)?;
                        call_binary(op, acc, Matrix::from(item))
                    },
                )
            }

            FoldOp::FunctionRef(f) => match self.variables.get(&f) {
                None => Err(format!("variable {} not found", f)),
                Some(v) => match v {
                    Value::Matrix(_) => Err(String::from("variable is a matrix")),
                    Value::Function(f) => {
                        if f.params.len() != 2 {
                            return Err(String::from("function does not take 2 params"));
                        }

                        let mut it = matrix.values.into_iter();
                        let first = it.next().unwrap();
                        it.try_fold(Matrix::from(first), |acc, item| -> Result<Matrix, String> {
                            let args = vec![acc, Matrix::from(item)];
                            let m = Matrix::try_from(self.call_function(f.clone(), args)?)?;
                            Ok(m)
                        })
                        .map(|m| ExecutorResult::Value(Value::Matrix(m)))
                    }
                },
            },
        }
    }

    fn execute_expr(&mut self, node: Expr) -> Result<ExecutorResult, String> {
        match node {
            Expr::Atom(Atom::Rat(v)) => Ok(Matrix::from(v).into()),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.variables.get(&s) {
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
                            args.push(Matrix::try_from(ExecutorResult::Value(e))?);
                        }

                        self.call_function(f, args)
                    }

                    Value::Matrix(first) => {
                        if expressions.len() == 1 {
                            return Ok(first.into());
                        }

                        let mut values = Vec::with_capacity(expressions.len());
                        for e in expressions.into_iter() {
                            let scalar = match Matrix::try_from(e)?.scalar() {
                                None => return Err(String::from("nested matrices aren't allowed")),
                                Some(s) => s.clone(),
                            };
                            values.push(scalar);
                        }

                        Ok(Matrix::make_vector(values).into())
                    }
                }
            }

            Expr::Unary(op, expr) => self.execute_unary(op, *expr),
            Expr::Binary(a, op, b) => self.execute_binary(op, *a, *b),
            Expr::Fold(op, expr) => self.execute_fold(op, *expr),

            Expr::Index(m, indices) => {
                let indices = expect_vector(self.execute_expr(*indices)?)?;

                if indices.iter().any(|i| !i.is_integer()) {
                    return Err(String::from("expected integers"));
                }

                let m = Matrix::try_from(self.execute_expr(*m)?)?;

                if indices.len() != m.shape.len() {
                    return Err(String::from("rank mismatch"));
                }

                let indices: Vec<usize> = indices
                    .iter()
                    .map(|v| to_usize_error(v))
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
            Statement::Expr(e) => self.execute_expr(e),

            Statement::Assign(var, val) => {
                err_var_exists!(var, false);
                let res = self.execute_expr(val)?;
                if let ExecutorResult::Value(val) = &res {
                    self.variables.insert(var, val.clone());
                }
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
                    let parsed =
                        parser::parse(&body).or(Err("couldn't parse expression".to_owned()))?;
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
                                format!("{}", s)
                            } else {
                                format!(" {}", s)
                            }
                        })
                        .collect::<Vec<String>>()
                        .concat();

                    Ok(ExecutorResult::Info(s))
                }

                "s" | "set" => {
                    let mut it = body.splitn(2, " ").map(|x| x.to_owned());
                    let key = it.next().unwrap();
                    let value = it.next().unwrap();
                    Ok(ExecutorResult::Setting(key, value))
                }

                cmd => Err(format!("unknown command {}", cmd)),
            },
        };

        if remember {
            if let Ok(ExecutorResult::Value(Value::Matrix(m))) = &res {
                self.variables
                    .insert(String::from("_"), Value::Matrix(m.clone()));
            }
        }

        res
    }
}
