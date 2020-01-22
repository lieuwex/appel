use std::collections::HashMap;
use std::ops::Neg;

use crate::ast::*;

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

#[derive(Clone)]
pub struct Executor {
    variables: HashMap<String, Value>,
}

#[derive(Clone, Debug)]
pub struct Matrix {
    values: Vec<Ratio>,
    shape: Vec<usize>,
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

    fn execute_unary(&mut self, op: UnOp, expr: Expr) -> Result<Option<Value>, String> {
        match op {
            UnOp::Id => self.execute_expr(expr),
            UnOp::Neg => {
                let res = self.execute_expr(expr)?;
                let val = match res {
                    None => return Ok(None),
                    Some(x) => expect_matrix(x),
                };

                let values = val.values.iter().map(|x| x.neg()).collect();
                let new = Matrix {
                    values: values,
                    shape: val.shape,
                };
                Ok(Some(Value::Matrix(new)))
            }
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

            if !a_res.is_scalar() && b_res.is_scalar() {
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

    fn execute_fold(&mut self, op: BinOp, expr: Expr) -> Result<Option<Value>, String> {
        let matrix = expect_matrix(self.execute_expr(expr)?.unwrap());

        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let mut curr = matrix.values[0].clone();
            for val in matrix.values.iter().skip(1) {
                curr = f(&curr, &val);
            }
            Ok(Some(Value::Matrix(Matrix::from(curr))))
        };

        match op {
            BinOp::Add => apply(&|a, b| a + b),
            BinOp::Sub => apply(&|a, b| a - b),
            BinOp::Mul => apply(&|a, b| a * b),
            BinOp::Div => apply(&|a, b| a / b),
            BinOp::Mod => apply(&|a, b| a % b),
            BinOp::Pow => apply(&|_, _| todo!()),
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
                        let mut ctx = self.clone();
                        for (param, expr) in f.params.iter().zip(expressions.drain(..).skip(1)) {
                            ctx.variables.insert(param.to_string(), expr);
                        }
                        ctx.execute_expr(f.expr)
                    }

                    Value::Matrix(_) => {
                        let values: Vec<_> = expressions
                            .drain(..)
                            .map(|e| expect_matrix(e).scalar().unwrap())
                            .collect();
                        let shape = vec![values.len()];

                        let m = Matrix { values, shape };
                        Ok(Some(Value::Matrix(m)))
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
