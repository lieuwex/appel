use std::collections::HashMap;
use std::fmt;
use std::ops::Neg;

use crate::ast::*;

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
}

impl From<Ratio> for Matrix {
    fn from(rat: Ratio) -> Self {
        Self {
            values: vec![rat],
            shape: vec![],
        }
    }
}

fn expect_matrix(val: Value) -> Matrix {
    match val {
        Value::Matrix(m) => m,
        Value::Function(_) => panic!(),
    }
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
        args: impl IntoIterator<Item=Matrix>,
    ) -> Result<Option<Value>, String> {
        let mut ctx = self.clone();
        for (param, matrix) in f.params.iter().zip(args) {
            ctx.variables
                .insert(param.to_string(), Value::Matrix(matrix));
        }
        ctx.execute_expr(f.expr)
    }

    fn execute_unary(&mut self, op: UnOp, expr: Expr) -> Result<Option<Value>, String> {
        if op == UnOp::Id {
            return self.execute_expr(expr);
        }

        let for_all = |f: &dyn Fn(Ratio) -> Ratio| {
            let res = self.execute_expr(expr)?;
            let mut val = match res {
                None => return Ok(None),
                Some(x) => expect_matrix(x),
            };

            let values = val.values.drain(..).map(f).collect();
            let new = Matrix {
                values: values,
                shape: val.shape,
            };
            Ok(Some(Value::Matrix(new)))
        };

        match op {
            UnOp::Id => unreachable!(),
            UnOp::Neg => for_all(&|x: Ratio| x.neg()),
            UnOp::Not => for_all(&|x: Ratio| {
                if x == Ratio::zero() {
                    Ratio::one()
                } else {
                    Ratio::zero()
                }
            }),
        }
    }

    fn execute_binary(&mut self, op: BinOp, a: Expr, b: Expr) -> Result<Option<Value>, String> {
        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let a_res = expect_matrix(self.execute_expr(a)?.unwrap());
            let b_res = expect_matrix(self.execute_expr(b)?.unwrap());

            if a_res.shape == b_res.shape {
                let values = a_res
                    .values
                    .iter()
                    .zip(b_res.values)
                    .map(|(a, b)| f(&a, &b))
                    .collect();

                let matrix = Matrix {
                    values,
                    shape: a_res.shape,
                };
                return Ok(Some(Value::Matrix(matrix)));
            }

            if !a_res.is_scalar() && !b_res.is_scalar() {
                return Err(String::from("rank mismatch"));
            }

            let scalar = if a_res.is_scalar() {
                a_res.clone()
            } else {
                b_res.clone()
            }
            .scalar()
            .unwrap();
            let mut non_scalar = if a_res.is_scalar() { b_res } else { a_res };

            let matrix = Matrix {
                values: non_scalar
                    .values
                    .drain(..)
                    .map(|v| f(&v, &scalar))
                    .collect(),
                shape: non_scalar.shape,
            };

            Ok(Some(Value::Matrix(matrix)))
        };

        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        match op {
            BinOp::Add => apply(&|a, b| a + b),
            BinOp::Sub => apply(&|a, b| a - b),
            BinOp::Mul => apply(&|a, b| a * b),
            BinOp::Div => apply(&|a, b| a / b),
            BinOp::Mod => apply(&|a, b| a % b),
            BinOp::Pow => apply(&|_, _| todo!()),
        }
    }

    fn execute_fold(&mut self, op: FoldOp, expr: Expr) -> Result<Option<Value>, String> {
        let mut matrix = expect_matrix(self.execute_expr(expr)?.unwrap());

        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let mut curr = matrix.values[0].clone();
            for val in matrix.values.iter().skip(1) {
                curr = f(&curr, &val);
            }
            Ok(Some(Value::Matrix(Matrix::from(curr))))
        };

        match op {
            FoldOp::BinOp(BinOp::Add) => apply(&|a, b| a + b),
            FoldOp::BinOp(BinOp::Sub) => apply(&|a, b| a - b),
            FoldOp::BinOp(BinOp::Mul) => apply(&|a, b| a * b),
            FoldOp::BinOp(BinOp::Div) => apply(&|a, b| a / b),
            FoldOp::BinOp(BinOp::Mod) => apply(&|a, b| a % b),
            FoldOp::BinOp(BinOp::Pow) => apply(&|_, _| todo!()),

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

    pub fn execute_expr(&mut self, node: Expr) -> Result<Option<Value>, String> {
        macro_rules! err_var_exists {
            ($var:expr) => {
                if self.variables.contains_key(&$var) {
                    return Err(format!("variable {} already exists", $var));
                }
            };
        }
        macro_rules! ok_matrix {
            ($var:expr) => {
                Ok(Some(Value::Matrix(Matrix::from($var))))
            };
        }

        return match node {
            Expr::Atom(Atom::Rat(v)) => ok_matrix!(v),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.variables.get(&s) {
                    None => return Err(String::from("variable not found")),
                    Some(var) => var,
                };

                match var {
                    Value::Matrix(m) => ok_matrix!(m.clone()),
                    Value::Function(f) => Ok(Some(Value::Function(f.clone()))),
                }
            }
            Expr::Vector(mut v) => {
                let mut expressions: Vec<_> = v
                    .drain(..)
                    .map(|e| self.execute_expr(e).unwrap().unwrap())
                    .collect();

                match expressions[0].clone() {
                    Value::Function(f) => {
                        let args = expressions
                            .drain(..)
                            .skip(1)
                            .map(|e| expect_matrix(e));

                        self.call_function(f, args)
                    }

                    Value::Matrix(_) => {
                        let values: Vec<_> = expressions
                            .drain(..)
                            .map(|e| expect_matrix(e).scalar().unwrap())
                            .collect();
                        let shape = vec![values.len()];

                        ok_matrix!(Matrix { values, shape })
                    }
                }
            }

            Expr::Unary(op, expr) => self.execute_unary(op, *expr),
            Expr::Binary(a, op, b) => self.execute_binary(op, *a, *b),
            Expr::Fold(op, expr) => self.execute_fold(op, *expr),

            Expr::Assign(var, val) => {
                err_var_exists!(var);
                let res = self.execute_expr(*val)?;
                self.variables.insert(var, res.clone().unwrap());
                Ok(res)
            }

            Expr::FunDeclare(name, params, expr) => {
                err_var_exists!(name);
                let f = Function {
                    params,
                    expr: *expr,
                };
                self.variables.insert(name, Value::Function(f));
                Ok(None)
            }
        };
    }
}
