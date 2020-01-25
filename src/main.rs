mod ast;
mod executor;
mod parser;

use std::io::BufRead;

use crate::executor::{Executor, ExecutorResult};

fn exec_line(exec: &mut Executor, line: std::io::Result<String>) -> Result<String, String> {
    let line = match line {
        Err(e) => {
            eprintln!("error reading from stdin: {}", e);
            std::process::exit(1);
        }
        Ok(l) => l,
    };

    let parsed = match parser::parse(&line) {
        Err(e) => return Err(format!("error while parsing: {}", e)),
        Ok(s) => s,
    };

    println!("parsed as: {:?}", parsed);

    match exec.execute(parsed) {
        Err(e) => Err(format!("error while executing: {}", e)),
        Ok(ExecutorResult::None) => Ok(String::new()),
        Ok(ExecutorResult::Value(res)) => Ok(format!("{}", res)),
        Ok(ExecutorResult::Info(s)) => Ok(s),
    }
}

fn main() {
    let mut exec = Executor::new();

    for line in std::io::stdin().lock().lines() {
        match exec_line(&mut exec, line) {
            Err(e) => eprintln!("{}\n", e),
            Ok(r) => println!("{}\n", r),
        }
    }
}
