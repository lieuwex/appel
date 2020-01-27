use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::ops::Neg;

use crate::ast::*;

use num_bigint::{BigInt, Sign};
use num_traits::cast::{FromPrimitive, ToPrimitive};
use num_traits::identities::{One, Zero};
use num_traits::sign::Signed;

fn to_f64(val: &Ratio) -> f64 {
    let (num, den) = val.clone().into();
    let fnum = num.to_f64().unwrap_or(std::f64::MAX);
    let fden = den.to_f64().unwrap_or(std::f64::MAX);
    fnum / fden
}
fn pow(a: &Ratio, b: &Ratio) -> Ratio {
    // TODO: optimize this when using an integer for the exponent.

    let a = to_f64(a);
    let b = to_f64(b);
    let res = a.powf(b);
    Ratio::from_f64(res).unwrap()
}
fn log(base: &Ratio, n: &Ratio) -> Ratio {
    let base = to_f64(base);
    let n = to_f64(n);
    let res = n.log(base);
    Ratio::from_f64(res).unwrap()
}

#[derive(Clone, Debug)]
pub struct Function {
    params: Vec<String>,
    expr: Expr,
}

#[derive(Clone, Debug)]
pub enum Value {
    Matrix(Matrix),
    Function(Function),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Matrix(m) => write!(f, "{}", m),
            Value::Function(fun) => {
                write!(f, "fun")?;
                for param in &fun.params {
                    write!(f, " {}", param)?;
                }

                write!(f, "{}", fun.expr)
            }
        }
    }
}

#[derive(Clone)]
pub struct Executor {
    variables: HashMap<String, Value>,
}

#[derive(Clone, Debug)]
pub enum ExecutorResult {
    None,
    Value(Value),
    Info(String),
}

impl ExecutorResult {
    /// Unwrap into Value
    fn unwrap(self) -> Value {
        match self {
            ExecutorResult::Value(v) => v,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Matrix {
    values: Vec<Ratio>,
    shape: Vec<usize>,
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: actually format

        let n_dimensions = self.shape.len();
        match n_dimensions {
            2 => {
                for i in 0..self.shape[0] {
                    for j in 0..self.shape[1] {
                        let val = self.get_at(vec![i, j]).unwrap();
                        if j > 0 {
                            write!(f, " ")?;
                        }

                        write!(f, "{}", val)?;
                    }
                    write!(f, "\n")?;
                }
            }

            _ => {
                for (i, val) in self.values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", val)?;
                }
            }
        }

        Ok(())
    }
}

impl Matrix {
    pub fn make_vector(values: Vec<Ratio>) -> Self {
        let shape = vec![values.len()];
        Self { values, shape }
    }

    pub fn is_scalar(&self) -> bool {
        return self.values.len() == 1;
    }

    pub fn scalar(&self) -> Option<&Ratio> {
        if self.is_scalar() {
            Some(&self.values[0])
        } else {
            None
        }
    }

    pub fn get_at(&self, indices: Vec<usize>) -> Option<&Ratio> {
        if indices.len() != self.shape.len() {
            return None;
        }

        let mut i = 0;
        let mut mult = 1;
        for (dim, index) in indices.iter().enumerate() {
            i += index * mult;
            mult *= self.shape[dim];
        }

        self.values.get(i)
    }
}

impl From<Ratio> for Matrix {
    fn from(rat: Ratio) -> Self {
        Self {
            values: vec![rat],
            shape: vec![],
        }
    }
}

impl TryFrom<Value> for Matrix {
    type Error = String;

    fn try_from(res: Value) -> Result<Self, Self::Error> {
        match res {
            Value::Function(_) => Err(String::from("expected matrix, got a function")),
            Value::Matrix(m) => Ok(m),
        }
    }
}
impl TryFrom<ExecutorResult> for Matrix {
    type Error = String;

    fn try_from(res: ExecutorResult) -> Result<Self, Self::Error> {
        match res {
            ExecutorResult::None => Err(String::from("expected value")),
            ExecutorResult::Info(_) => Err(String::from("expected value, got an info string")),
            ExecutorResult::Value(val) => Matrix::try_from(val),
        }
    }
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
macro_rules! ok_matrix {
    ($var:expr) => {
        Ok(ExecutorResult::Value(Value::Matrix(Matrix::from($var))))
    };
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

fn call_binary(op: BinOp, a: Matrix, mut b: Matrix) -> Result<ExecutorResult, String> {
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
                    .collect();

                let matrix = Matrix {
                    values,
                    shape: a.shape,
                };
                return ok_matrix!(matrix);
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
                    .collect(),
                shape: non_scalar.shape,
            };

            ok_matrix!(matrix)
        }};
    }

    match op {
        BinOp::Add => apply!(|a, b| a + b),
        BinOp::Sub => apply!(|a, b| a - b),
        BinOp::Mul => apply!(|a, b| a * b),
        BinOp::Div => apply!(|a, b| a / b),
        BinOp::Mod => apply!(|a, b| a % b),
        BinOp::Pow => apply!(pow),
        BinOp::Log => apply!(log),
        BinOp::CompOp(x) => apply!(get_comp_op_fn(x)),

        BinOp::Skip => {
            let scalar = expect_scalar(ExecutorResult::Value(Value::Matrix(a)))?;
            let n = scalar.to_integer().to_usize().unwrap_or(std::usize::MAX);

            ok_matrix!(Matrix {
                values: b.values.drain(..).skip(n).collect(),
                shape: b.shape,
            })
        }

        BinOp::Rho => {
            let shape: Vec<usize> = a
                .values
                .iter()
                .map(|v| v.to_integer().to_usize().unwrap_or(std::usize::MAX))
                .collect();

            let values: Vec<Ratio> = std::iter::repeat(b.values)
                .flatten()
                .take(shape.iter().product())
                .collect();

            ok_matrix!(Matrix { values, shape })
        }

        BinOp::Unpack => {
            let a = get_int(a)?;
            let b = get_int(b)?;

            let radix = a.to_u32().unwrap_or(std::u32::MAX);
            if radix < 2 {
                return Err(String::from("radix must be greater than or equal to 2"));
            }

            let (sign, mut bits) = b.to_radix_be(radix);

            let values: Vec<Ratio> = bits
                .drain(..)
                .map(|b| Ratio::from_u8(b).unwrap())
                .map(|b| {
                    if sign == num_bigint::Sign::Minus {
                        b.neg()
                    } else {
                        b
                    }
                })
                .collect();

            ok_matrix!(Matrix::make_vector(values))
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
            ok_matrix!(Matrix::from(Ratio::from_integer(int)))
        }
    }
}

impl Executor {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
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
                let mut val = Matrix::try_from(res)?;

                let values = val.values.drain(..).map($f).collect();
                let new = Matrix {
                    values: values,
                    shape: val.shape,
                };
                ok_matrix!(new)
            }};
        }

        match op {
            UnOp::Id => Ok(res),
            UnOp::Neg => for_all!(&|x: Ratio| x.neg()),
            UnOp::Not => for_all!(&|x: Ratio| {
                if x == Ratio::zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),
            UnOp::Abs => for_all!(&|x: Ratio| x.abs()),

            UnOp::Iota => {
                let s = expect_scalar(res)?;
                let upper = s.to_integer().to_usize().unwrap_or(std::usize::MAX);
                let values: Vec<_> = (0..upper).filter_map(Ratio::from_usize).collect();
                ok_matrix!(Matrix::make_vector(values))
            }

            UnOp::Rho => {
                let m = Matrix::try_from(res)?;
                let values: Vec<Ratio> = m
                    .shape
                    .iter()
                    .map(|s| Ratio::from_usize(*s).unwrap())
                    .collect();
                ok_matrix!(Matrix::make_vector(values))
            }

            UnOp::Rev => {
                let mut m = Matrix::try_from(res)?;
                let values: Vec<Ratio> = m.values.drain(..).rev().collect();
                ok_matrix!(Matrix {
                    values,
                    shape: m.shape,
                })
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
        let mut matrix = Matrix::try_from(self.execute_expr(expr)?)?;
        if matrix.values.len() < 2 {
            return Err(String::from("matrix has to have at least 2 values"));
        }

        match op {
            FoldOp::BinOp(op) => {
                let mut it = matrix.values.drain(..);
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
                None => return Err(String::from("variable not found")),
                Some(v) => match v {
                    Value::Matrix(_) => return Err(String::from("variable is a matrix")),
                    Value::Function(f) => {
                        if f.params.len() != 2 {
                            return Err(String::from("function does not take 2 params"));
                        }

                        let mut it = matrix.values.drain(..);
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
            Expr::Atom(Atom::Rat(v)) => ok_matrix!(v),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.variables.get(&s) {
                    None => return Err(String::from("variable not found")),
                    Some(var) => var.clone(),
                };

                match var {
                    Value::Matrix(m) => ok_matrix!(m),
                    Value::Function(f) => Ok(ExecutorResult::Value(Value::Function(f))),
                }
            }

            Expr::Vector(mut v) => {
                let mut expressions = Vec::with_capacity(v.len());
                for e in v.drain(..) {
                    expressions.push(self.execute_expr(e)?.unwrap());
                }

                match expressions[0].clone() {
                    Value::Function(f) => {
                        let mut args = Vec::with_capacity(expressions.len() - 1);
                        for e in expressions.drain(..).skip(1) {
                            args.push(Matrix::try_from(ExecutorResult::Value(e))?);
                        }

                        self.call_function(f, args)
                    }

                    Value::Matrix(first) => {
                        if expressions.len() == 1 {
                            return ok_matrix!(first);
                        }

                        let mut values = Vec::with_capacity(expressions.len());
                        for e in expressions.drain(..) {
                            let scalar = match Matrix::try_from(e)?.scalar() {
                                None => return Err(String::from("nested matrices aren't allowed")),
                                Some(s) => s.clone(),
                            };
                            values.push(scalar);
                        }

                        ok_matrix!(Matrix::make_vector(values))
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

                let item = m.get_at(
                    indices
                        .iter()
                        .map(|i| i.to_integer().to_usize().unwrap_or(std::usize::MAX))
                        .collect(),
                );

                match item {
                    None => Err(String::from("out of bounds")),
                    Some(i) => ok_matrix!(Matrix::from(i.clone())),
                }
            }
        }
    }

    pub fn execute(&mut self, node: Statement) -> Result<ExecutorResult, String> {
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
                let f = Function { params, expr };
                self.variables.insert(name, Value::Function(f));
                Ok(ExecutorResult::None)
            }

            Statement::InternalCommand(command, body) => {
                /*
                macro_rules! expect_nargs {
                    (== $count:expr) => {
                        if args.len() != $count {
                            return Err(format!("expected {} arguments", $count));
                        }
                    };
                    (< $count:expr) => {
                        if args.len() >= $count {
                            return Err(format!("expected less than {} arguments", $count));
                        }
                    };
                    (> $count:expr) => {
                        if args.len() <= $count {
                            return Err(format!("expected more than {} arguments", $count));
                        }
                    };
                }
                */

                match command.as_str() {
                    "n" | "number" => {
                        //expect_nargs!(== 1);
                        match self.execute_expr(body)? {
                            ExecutorResult::None => {
                                Err(String::from("can't format nothing to number"))
                            }
                            ExecutorResult::Info(_) => {
                                Err(String::from("can't format info string to number"))
                            }
                            ExecutorResult::Value(Value::Function(_)) => {
                                Err(String::from("can't format function to number"))
                            }
                            ExecutorResult::Value(Value::Matrix(m)) => {
                                let s = m
                                    .values
                                    .iter()
                                    .map(to_f64)
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
                        }
                    }

                    cmd => Err(format!("unknown command {}", cmd)),
                }
            }
        };

        if let Ok(ExecutorResult::Value(Value::Matrix(m))) = &res {
            self.variables
                .insert(String::from("_"), Value::Matrix(m.clone()));
        }

        res
    }
}
