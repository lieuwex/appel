use num_bigint::BigInt;

pub type Ratio = num_rational::Ratio<BigInt>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Id,
    Neg,
    Not,
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
    Fold(BinOp, Box<Expr>),
    Assign(String, Box<Expr>),
    // name, parameters, tree
    FunDeclare(String, Vec<String>, Box<Expr>),
}
