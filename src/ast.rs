use std::fmt;

pub type Ratio = num_rational::BigRational;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Id,
    Neg,
    Not,
    RollInt,
    RollFloat,
    Iota,
    Abs,
    Rho,
    Rev,
    Sin,
    Cos,
    Tan,
    Up,
    Down,
    Floor,
    Ceil,
    Sign,
}

// TODO: precendence for operators.

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Id => write!(f, "+"),
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
            UnOp::RollInt => write!(f, "?i"),
            UnOp::RollFloat => write!(f, "?f"),
            UnOp::Iota => write!(f, "iota "),
            UnOp::Abs => write!(f, "abs "),
            UnOp::Rho => write!(f, "rho "),
            UnOp::Rev => write!(f, "rev "),
            UnOp::Sin => write!(f, "sin "),
            UnOp::Cos => write!(f, "cos "),
            UnOp::Tan => write!(f, "tan "),
            UnOp::Up => write!(f, "up "),
            UnOp::Down => write!(f, "down "),
            UnOp::Floor => write!(f, "floor "),
            UnOp::Ceil => write!(f, "ceil "),
            UnOp::Sign => write!(f, "sgn "),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompOp {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for CompOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompOp::Eq => write!(f, "=="),
            CompOp::Neq => write!(f, "!="),
            CompOp::Lt => write!(f, "<"),
            CompOp::Le => write!(f, "<="),
            CompOp::Gt => write!(f, ">"),
            CompOp::Ge => write!(f, ">="),
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
    Concat,

    CompOp(CompOp),

    Log,
    Skip,
    Rho,
    Unpack,
    Pack,
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
            BinOp::Concat => write!(f, ","),

            BinOp::CompOp(x) => write!(f, "{}", x),

            BinOp::Log => write!(f, "log"),
            BinOp::Skip => write!(f, "skip"),
            BinOp::Rho => write!(f, "rho"),
            BinOp::Unpack => write!(f, "unpack"),
            BinOp::Pack => write!(f, "pack"),
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
    Index(Box<Expr>, Box<Expr>), // vector, indices
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (s, paren) = match self {
            Expr::Atom(Atom::Rat(v)) => (format!("{}", v), false),
            Expr::Atom(Atom::Ref(v)) => (format!("{}", v), false),

            Expr::Vector(exprs) => (
                exprs
                    .iter()
                    .enumerate()
                    .map(|(i, expr)| {
                        if i > 0 {
                            format!(" {}", expr)
                        } else {
                            format!("{}", expr)
                        }
                    })
                    .collect::<String>(),
                true,
            ),

            Expr::Unary(op, expr) => (format!("{}{}", op, expr), false),
            Expr::Binary(a, op, b) => (format!("{} {} {}", a, op, b), true),
            Expr::Fold(op, expr) => (format!("{}//{}", op, expr), true),
            Expr::Index(vec, indices) => (format!("{}[{}]", vec, indices), false),
        };

        if paren {
            write!(f, "({})", s)
        } else {
            write!(f, "{}", s)
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expr),
    Assign(String, Expr),
    // name, parameters, tree
    FunDeclare(String, Vec<String>, Expr),
    InternalCommand(String, String),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expr(e) => write!(f, "{}", e),

            Statement::Assign(name, expr) => write!(f, "{} = {}", name, expr),

            Statement::FunDeclare(name, args, expr) => {
                write!(f, "fn {}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }

                write!(f, " = {}", expr)
            }

            Statement::InternalCommand(name, body) => write!(f, ")n {} {}", name, body),
        }
    }
}
