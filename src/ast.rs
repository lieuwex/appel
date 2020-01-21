use num_bigint::BigInt;

pub type Ratio = num_rational::Ratio<BigInt>;

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Id,
    Neg,
}

#[derive(Clone, Copy, Debug)]
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
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Fold(BinOp, Box<Expr>),
    Vector(Vec<Expr>),
    Assign(String, Box<Expr>),
    // name, parameters, tree
    FunDeclare(String, Vec<String>, Box<Expr>),
}
