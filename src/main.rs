mod ast;
mod parser;
mod executor;

use num_traits::cast::FromPrimitive;

use crate::executor::*;
use crate::ast::*;

fn main() {
    println!("{:?}", parser::parse("1.2").unwrap());

    let mut exec = Executor::new();

    let vec = Expr::Vector(vec![ Expr::Atom(parser::parse("1.2").unwrap()), Expr::Atom(parser::parse("5.4").unwrap()) ]);
    let res = exec.execute_expr(Expr::Fold(BinOp::Add, Box::new(vec)));

    println!("{:?}", res);
}
