use num_bigint::BigInt;

pub type Ratio = num_rational::Ratio<BigInt>;

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
    Rat(Ratio),
    Ref(String),
}

pub enum Expr {
    Atom(Atom),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Fold(BinOp, Vec<Expr>),
}
