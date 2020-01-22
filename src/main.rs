mod ast;
mod executor;
mod parser;

use num_traits::cast::FromPrimitive;

use crate::ast::*;
use crate::executor::*;

fn main() {
    println!("{:?}", parser::parse("1.2").unwrap());

    let mut exec = Executor::new();

    // add some numbers
    let res = exec.execute_expr(Expr::Fold(
        BinOp::Add,
        Box::new(Expr::Vector(vec![
            parser::parse("1.5").unwrap(),
            parser::parse("3.5").unwrap(),
            parser::parse("5").unwrap(),
        ])),
    ));
    println!("{:?}", res);

    // make function
    let res = exec.execute_expr(Expr::FunDeclare(
        String::from("perc"),
        vec![String::from("a")],
        Box::new(Expr::Binary(
            Box::new(Expr::Atom(Atom::Ref(String::from("a")))),
            BinOp::Div,
            Box::new(Expr::Atom(Atom::Rat(Ratio::from_u64(100).unwrap()))),
        )),
    ));
    println!("{:?}", res);

    // call function
    let res = exec.execute_expr(Expr::Vector(vec![
        Expr::Atom(Atom::Ref(String::from("perc"))),
        Expr::Atom(Atom::Rat(Ratio::from_f64(12.5).unwrap())),
    ]));
    println!("{:?}", res);
}
