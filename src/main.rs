mod ast;
mod executor;
mod parser;

use num_traits::cast::FromPrimitive;
use std::env;

use crate::ast::*;
use crate::executor::*;

macro_rules! fmt_res {
    ($res:expr) => {
        match $res {
            Err(e) => format!("Err({})", e),
            Ok(r) => match r {
                Some(v) => format!("Ok({})", v),
                None => format!("Ok(None)"),
            },
        }
    };
}

fn main() {
    if let Some(input) = env::args().nth(1) {
        let parsed = parser::parse(&input).unwrap();
        let mut exec = Executor::new();
        let res = exec.execute_expr(parsed);
        println!("arg output: {}\n", fmt_res!(res));
    }

    println!("{}", parser::parse("2 / 5").unwrap());
    println!("{}", parser::parse("1.2").unwrap());

    let mut exec = Executor::new();

    // add some numbers
    let res = exec.execute_expr(Expr::Fold(
        FoldOp::BinOp(BinOp::Add),
        Box::new(Expr::Vector(vec![
            parser::parse("1.5").unwrap(),
            parser::parse("3.5").unwrap(),
            parser::parse("5").unwrap(),
        ])),
    ));
    println!("{}", fmt_res!(res));

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
    println!("{}", fmt_res!(res));

    // call function
    let res = exec.execute_expr(Expr::Vector(vec![
        Expr::Atom(Atom::Ref(String::from("perc"))),
        Expr::Atom(Atom::Rat(Ratio::from_f64(12.5).unwrap())),
    ]));
    println!("{}", fmt_res!(res));
}
