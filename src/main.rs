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
        println!("parsed as: {:?}\n", parsed);
        let mut exec = Executor::new();
        let res = exec.execute(parsed);
        println!("arg output: {}\n", fmt_res!(res));
    }

    println!("{}", parser::parse("2 / 5").unwrap());
    println!("{}", parser::parse("1.2").unwrap());
}
