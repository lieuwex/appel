mod ast;
mod executor;
mod parser;

use std::io::BufRead;

use crate::executor::matrix::Formatter;
use crate::executor::{Executor, ExecutorResult, Value};

fn format_res(res: Value, fmt: Formatter) -> String {
    match res {
        Value::Function(f) => format!("{}", f),
        Value::Matrix(m) => m.format(fmt),
    }
}

struct State {
    formatter: Formatter,
    exec: Executor,
}

impl State {
    fn exec_line(&mut self, line: std::io::Result<String>) -> Result<String, String> {
        let line = match line {
            Err(e) => {
                eprintln!("error reading from stdin: {}", e);
                std::process::exit(1);
            }
            Ok(l) => l,
        };

        let (line, silent) = if line.starts_with('#') {
            (&line[1..], true)
        } else {
            (line.as_str(), false)
        };

        let parsed = match parser::parse(line) {
            Err(e) => return Err(format!("error while parsing: {}", e)),
            Ok(None) => return Ok(String::new()),
            Ok(Some(s)) => s,
        };

        println!("parsed as: {:?}", parsed);

        let res = match self.exec.execute(parsed, true) {
            Err(e) => Err(format!("error while executing: {}", e)),
            Ok(ExecutorResult::None) => Ok(String::new()),
            Ok(ExecutorResult::Value(res)) => Ok(format_res(res, self.formatter)),
            Ok(ExecutorResult::Info(s)) => Ok(s),
            Ok(ExecutorResult::Setting(key, val)) => {
                match key.to_ascii_lowercase().as_str() {
                    "f" | "format" => {
                        self.formatter = match val.to_ascii_lowercase().as_str() {
                            "rat" | "ratio" => Formatter::Ratio,
                            n => {
                                let precision = n
                                    .trim()
                                    .parse::<usize>()
                                    .or_else(|_| Err("conversion error".to_owned()))?;
                                Formatter::Float(precision)
                            }
                        };
                    }
                    setting => return Err(format!("unknown setting {}", setting)),
                };
                Ok(String::new())
            }
        };
        res.map(|x| if silent { String::new() } else { x })
    }
}

fn main() {
    let mut state = State {
        exec: Executor::new(),
        formatter: Formatter::Ratio,
    };

    for line in std::io::stdin().lock().lines() {
        match state.exec_line(line) {
            Err(e) => eprintln!("{}\n", e),
            Ok(r) => {
                if r.is_empty() {
                    println!()
                } else {
                    println!("{}\n", r)
                }
            }
        }
    }
}
