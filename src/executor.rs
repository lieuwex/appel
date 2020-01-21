use std::collections::HashMap;
use std::ops::Neg;

use crate::ast::*;

pub struct Function {
    params: Vec<String>,
    expr: Expr,
}

pub enum Variable {
    Matrix(Matrix),
    Function(Function),
}

pub struct Executor {
    variables: HashMap<String, Variable>,
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

impl Executor {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn execute_unary(&mut self, op: UnOp, expr: Expr) -> Result<Option<Matrix>, String> {
        match op {
            UnOp::Id => self.execute_expr(expr),
            UnOp::Neg => {
                let res = self.execute_expr(expr)?;
                let val = match res {
                    None => return Ok(None),
                    Some(x) => x,
                };

                let values = val.values.iter().map(|x| x.neg()).collect();
                let new = Matrix {
                    values: values,
                    shape: val.shape,
                };
                Ok(Some(new))
            }
        }
    }

    fn execute_binary(&mut self, op: BinOp, a: Expr, b: Expr) -> Result<Option<Matrix>, String> {
        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let a_res = self.execute_expr(a)?.unwrap();
            let b_res = self.execute_expr(b)?.unwrap();

            if a_res.shape == b_res.shape {
                let values = a_res
                    .values
                    .iter()
                    .zip(b_res.values)
                    .map(|(a, b)| f(&a, &b))
                    .collect();

                return Ok(Some(Matrix {
                    values,
                    shape: a_res.shape,
                }));
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

            Ok(Some(matrix))
        };

        // (a/b)^(c/d) = (\sqrt d {a^c}) / (\sqrt d {b ^c})

        match op {
            BinOp::Add => apply(&|a, b| a + b),
            BinOp::Sub => apply(&|a, b| a - b),
            BinOp::Mul => apply(&|a, b| a * b),
            BinOp::Div => apply(&|a, b| a / b),
            BinOp::Mod => apply(&|a, b| a % b),
            BinOp::Pow => apply(&|_, _| todo!())
        }
    }

    fn execute_fold(&mut self, op: BinOp, expr: Expr) -> Result<Option<Matrix>, String> {
        let matrix = self.execute_expr(expr)?.unwrap();

        let apply = |f: &dyn Fn(&Ratio, &Ratio) -> Ratio| {
            let mut curr = matrix.values[0].clone();
            for val in matrix.values.iter().skip(1) {
                curr = f(&curr, &val);
            }
            Ok(Some(Matrix::from(curr)))
        };

        match op {
            BinOp::Add => apply(&|a, b| a + b),
            BinOp::Sub => apply(&|a, b| a - b),
            BinOp::Mul => apply(&|a, b| a * b),
            BinOp::Div => apply(&|a, b| a / b),
            BinOp::Mod => apply(&|a, b| a % b),
            BinOp::Pow => apply(&|_, _| todo!())
        }
    }

    // TODO: call functions

    pub fn execute_expr(&mut self, node: Expr) -> Result<Option<Matrix>, String> {
        macro_rules! check_var_exists {
            ($var:expr) => {
                if self.variables.contains_key(&$var) {
                    return Err(format!("variable {} already exists", $var));
                }
            };
        }

        return match node {
            Expr::Atom(Atom::Rat(v)) => Ok(Some(Matrix::from(v))),
            Expr::Atom(Atom::Ref(s)) => {
                let var = match self.variables.get(&s) {
                    None => return Err(String::from("variable not found")),
                    Some(var) => var,
                };

                match var {
                    Variable::Matrix(m) => Ok(Some(m.clone())),
                    Variable::Function(_) => todo!(),
                }
            },
            Expr::Vector(mut v) => {
                // TODO
                let values: Vec<_> = v
                    .drain(..)
                    .map(|e| self.execute_expr(e).unwrap().unwrap().scalar().unwrap())
                    .collect();
                let shape = vec![values.len()];

                let m = Matrix { values, shape };
                Ok(Some(m))
            }

            Expr::Unary(op, expr) => self.execute_unary(op, *expr),
            Expr::Binary(a, op, b) => self.execute_binary(op, *a, *b),
            Expr::Fold(op, expr) => self.execute_fold(op, *expr),

            Expr::Assign(var, val) => {
                check_var_exists!(var);
                let res = self.execute_expr(*val)?;
                self.variables
                    .insert(var, Variable::Matrix(res.clone().unwrap()));
                Ok(res)
            }

            Expr::FunDeclare(name, params, expr) => {
                check_var_exists!(name);
                let f = Function {
                    params,
                    expr: *expr,
                };
                self.variables.insert(name, Variable::Function(f));
                Ok(None)
            }
        };
    }
}
