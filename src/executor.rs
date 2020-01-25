use std::collections::HashMap;
use std::fmt;
use std::ops::Neg;

use crate::ast::*;

use num_traits::cast::{FromPrimitive, ToPrimitive};
use num_traits::identities::{One, Zero};

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

        for (i, val) in self.values.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }

            write!(f, "{}", val)?;
        }

        Ok(())
    }
}

impl Matrix {
    pub fn is_scalar(&self) -> bool {
        return self.values.len() == 1;
    }

    pub fn scalar(&self) -> Option<Ratio> {
        if self.is_scalar() {
            Some(self.values[0].clone())
        } else {
            None
        }
    }

    pub fn make_vector(values: Vec<Ratio>) -> Self {
        let shape = vec![values.len()];
        Self { values, shape }
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

macro_rules! expect_matrix {
    ($v:expr) => {
        match $v {
            ExecutorResult::None => return Err(String::from("expected value")),
            ExecutorResult::Value(Value::Function(_)) => {
                return Err(String::from("expected matrix, got a function"))
            }
            ExecutorResult::Info(_) => {
                return Err(String::from("expected value, got an info string"))
            }
            ExecutorResult::Value(Value::Matrix(m)) => m,
        }
    };
}
macro_rules! expect_scalar {
    ($v:expr) => {
        match expect_matrix!($v).scalar() {
            None => return Err(String::from("expected scalar")),
            Some(s) => s,
        }
    };
}
macro_rules! ok_matrix {
    ($var:expr) => {
        Ok(ExecutorResult::Value(Value::Matrix(Matrix::from($var))))
    };
}

impl Executor {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn call_function(
        &mut self,
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

        if op == UnOp::Id {
            return Ok(res);
        }

        macro_rules! for_all {
            ($f:expr) => {{
                let mut val = expect_matrix!(res);

                let values = val.values.drain(..).map($f).collect();
                let new = Matrix {
                    values: values,
                    shape: val.shape,
                };
                ok_matrix!(new)
            }};
        }

        match op {
            UnOp::Id => unreachable!(),
            UnOp::Neg => for_all!(&|x: Ratio| x.neg()),
            UnOp::Not => for_all!(&|x: Ratio| {
                if x == Ratio::zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),
            UnOp::Iota => match expect_matrix!(res).scalar() {
                None => return Err(String::from("expected scalar")),
                Some(s) => {
                    let upper = s.to_integer().to_usize().unwrap_or(std::usize::MAX);
                    let values: Vec<_> = (0..upper).filter_map(Ratio::from_usize).collect();
                    ok_matrix!(Matrix::make_vector(values))
                }
            },
        }
    }

    fn execute_binary(&mut self, op: BinOp, a: Expr, b: Expr) -> Result<ExecutorResult, String> {
        macro_rules! apply {
            ($f:expr) => {{
                let a_res = expect_matrix!(self.execute_expr(a)?);
                let b_res = expect_matrix!(self.execute_expr(b)?);

                if a_res.shape == b_res.shape {
                    let values = a_res
                        .values
                        .iter()
                        .zip(b_res.values)
                        .map(|(a, b)| $f(a, b))
                        .collect();

                    let matrix = Matrix {
                        values,
                        shape: a_res.shape,
                    };
                    return ok_matrix!(matrix);
                }

                if !a_res.is_scalar() && !b_res.is_scalar() {
                    return Err(String::from("rank mismatch"));
                }

                // Since:
                //   !(!scalar(a) && !scalar(b)) => (scalar(a) || scalar(b))
                //
                // We can assume that the unwrap never fails.
                let (scalar, mut non_scalar, scalar_is_left) = if a_res.is_scalar() {
                    (a_res.scalar().unwrap(), b_res, true)
                } else {
                    (b_res.scalar().unwrap(), a_res, false)
                };

                let matrix = Matrix {
                    values: non_scalar
                        .values
                        .drain(..)
                        .map(|v| {
                            if scalar_is_left {
                                $f(&scalar, v)
                            } else {
                                $f(v, &scalar)
                            }
                        })
                        .collect(),
                    shape: non_scalar.shape,
                };

                ok_matrix!(matrix)
            }};
        }

        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        match op {
            BinOp::Add => apply!(|a, b| a + b),
            BinOp::Sub => apply!(|a, b| a - b),
            BinOp::Mul => apply!(|a, b| a * b),
            BinOp::Div => apply!(|a, b| a / b),
            BinOp::Mod => apply!(|a, b| a % b),
            BinOp::Pow => apply!(|_, _| todo!()),

            BinOp::Skip => {
                let scalar = expect_scalar!(self.execute_expr(a)?);
                let mut b_res = expect_matrix!(self.execute_expr(b)?);

                let n = scalar.to_integer().to_usize().unwrap_or(std::usize::MAX);

                ok_matrix!(Matrix {
                    values: b_res.values.drain(..).skip(n).collect(),
                    shape: b_res.shape,
                })
            }
        }
    }

    fn execute_fold(&mut self, op: FoldOp, expr: Expr) -> Result<ExecutorResult, String> {
        let mut matrix = expect_matrix!(self.execute_expr(expr)?);

        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let mut curr = matrix.values[0].clone();
            for val in matrix.values.iter().skip(1) {
                curr = f(&curr, &val);
            }
            ok_matrix!(curr)
        };

        match op {
            FoldOp::BinOp(BinOp::Add) => apply(&|a, b| a + b),
            FoldOp::BinOp(BinOp::Sub) => apply(&|a, b| a - b),
            FoldOp::BinOp(BinOp::Mul) => apply(&|a, b| a * b),
            FoldOp::BinOp(BinOp::Div) => apply(&|a, b| a / b),
            FoldOp::BinOp(BinOp::Mod) => apply(&|a, b| a % b),
            FoldOp::BinOp(BinOp::Pow) => apply(&|_, _| todo!()),
            FoldOp::BinOp(BinOp::Skip) => apply(&|_, _| todo!()),

            FoldOp::FunctionRef(f) => match self.variables.get(&f) {
                None => return Err(String::from("variable not found")),
                Some(v) => match v {
                    Value::Matrix(_) => return Err(String::from("variable is a matrix")),
                    Value::Function(_f) => {
                        let f = _f.clone();

                        if f.params.len() != 2 {
                            return Err(String::from("function does not take 2 params"));
                        }

                        let args = matrix.values.drain(..).map(|v| Matrix::from(v));
                        self.call_function(f, args)
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
                    Some(var) => var,
                };

                match var {
                    Value::Matrix(m) => ok_matrix!(m.clone()),
                    Value::Function(f) => Ok(ExecutorResult::Value(Value::Function(f.clone()))),
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
                            args.push(expect_matrix!(ExecutorResult::Value(e)));
                        }

                        self.call_function(f, args)
                    }

                    Value::Matrix(first) => {
                        if expressions.len() == 1 {
                            return ok_matrix!(first);
                        }

                        let mut values = Vec::with_capacity(expressions.len());
                        for e in expressions.drain(..) {
                            let scalar = match expect_matrix!(ExecutorResult::Value(e)).scalar() {
                                None => return Err(String::from("nested matrices aren't allowed")),
                                Some(s) => s,
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
                let res = self.execute_expr(*val)?;
                if let ExecutorResult::Value(val) = res.clone() {
                    self.variables.insert(var, val);
                }
                Ok(res)
            }

            Statement::FunDeclare(name, params, expr) => {
                err_var_exists!(name, true);
                let f = Function {
                    params,
                    expr: *expr,
                };
                self.variables.insert(name, Value::Function(f));
                Ok(ExecutorResult::None)
            }

            Statement::InternalCommand(command, mut args) => {
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

                match command.as_str() {
                    "n" | "number" => {
                        expect_nargs!(== 1);
                        match self.execute_expr(args.drain(..).next().unwrap())? {
                            ExecutorResult::None => {
                                Err(String::from("can't format nothing to number"))
                            }
                            ExecutorResult::Info(_) => {
                                Err(String::from("can't format info string to number"))
                            }
                            ExecutorResult::Value(Value::Function(_)) => {
                                Err(String::from("can't format function to number"))
                            }
                            ExecutorResult::Value(Value::Matrix(mut m)) => {
                                let s = m
                                    .values
                                    .drain(..)
                                    .map(|val| {
                                        let (num, den) = val.clone().into();
                                        let fnum = num.to_f64().unwrap_or(std::f64::MAX);
                                        let fden = den.to_f64().unwrap_or(std::f64::MAX);
                                        fnum / fden
                                    })
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

        if let Ok(ExecutorResult::Value(Value::Matrix(m))) = res.clone() {
            self.variables.insert(String::from("_"), Value::Matrix(m));
        }

        res
    }
}
