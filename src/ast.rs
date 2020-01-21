use num_bigint::BigInt;

pub enum UnOp {
    Id,
    Neg,
    Inv,
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub enum Atom {
    Rat(BigInt, BigInt),
    Float(f64),
    Var(String),
}

pub enum Expr {
    Atom(Atom),
    Fold(BinOp, Vec<Expr>),
    Map(UnOp, Vec<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnOp, Box<Expr>),
}
