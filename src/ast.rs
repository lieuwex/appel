use num_bigint::BigInt;
use std::fmt;

pub type Ratio = num_rational::Ratio<BigInt>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Id,
    Neg,
    Not,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Id => write!(f, "+"),
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Pow => write!(f, "**"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FoldOp {
    BinOp(BinOp),
    FunctionRef(String),
}

impl fmt::Display for FoldOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FoldOp::BinOp(op) => write!(f, "{}", op),
            FoldOp::FunctionRef(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Atom {
    Rat(Ratio),
    Ref(String),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Atom(Atom),
    Vector(Vec<Expr>),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Fold(FoldOp, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Atom(Atom::Rat(v)) => write!(f, "{}", v),
            Expr::Atom(Atom::Ref(v)) => write!(f, "{}", v),

            Expr::Vector(exprs) => {
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", expr)?;
                }
                Ok(())
            }

            Expr::Unary(op, expr) => write!(f, "{}{}", op, expr),
            Expr::Binary(a, op, b) => write!(f, "{} {} {}", a, op, b),
            Expr::Fold(op, expr) => write!(f, "{}//{}", op, expr),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expr),
    Assign(String, Box<Expr>),
    // name, parameters, tree
    FunDeclare(String, Vec<String>, Box<Expr>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expr(e) => write!(f, "{}", e),

            Statement::Assign(name, expr) => write!(f, "{} = {}", name, expr),

            Statement::FunDeclare(name, args, expr) => {
                write!(f, "{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }

                write!(f, " {}", expr)
            }
        }
    }
}
