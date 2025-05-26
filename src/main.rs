#![allow(clippy::redundant_closure_call)]

mod ast;
mod executor;
mod parser;

use std::fs;

use clap::{Arg, Command};
use executor::chain::Error;

use crate::executor::matrix::Formatter;
use crate::executor::{Executor, ExecutorResult};

struct State {
    formatter: Formatter,
    exec: Executor<'static>,
}

impl State {
    fn exec_line(&mut self, line: &str) -> Result<String, Error> {
        let (line, silent) = line
            .strip_prefix('#')
            .map(|line| (line, true))
            .unwrap_or((line, false));

        let parsed = match parser::parse(line) {
            Err(e) => return Err(format!("error while parsing: {}", e).into()),
            Ok(None) => return Ok(String::new()),
            Ok(Some(s)) => s,
        };

        if cfg!(debug_assertions) {
            println!("parsed as: {:?}", parsed);
        }

        let res = match self.exec.execute(parsed, true) {
            Err(e) => Err(format!("error while executing: {}", e).into()),
            Ok(ExecutorResult::None) => Ok(String::new()),
            Ok(ExecutorResult::Chain(c)) => c.format(self.formatter),
            Ok(ExecutorResult::Info(s)) => Ok(s),
            Ok(ExecutorResult::Setting(key, val)) => {
                match key.to_ascii_lowercase().as_str() {
                    "f" | "format" => {
                        self.formatter = match val.to_ascii_lowercase().as_str() {
                            "rat" | "ratio" => Formatter::Ratio,
                            "f" | "float" => Formatter::Float(None),
                            n => {
                                let precision = n
                                    .trim()
                                    .parse::<usize>()
                                    .map_err(|_| Error::from("conversion error"))?;
                                Formatter::Float(Some(precision))
                            }
                        };
                    }
                    setting => return Err(format!("unknown setting {}", setting).into()),
                };
                Ok(String::new())
            }
        };
        res.map(|x| if silent { String::new() } else { x })
    }
}

fn main() -> Result<(), String> {
    let matches = Command::new("Appel")
        .version("alpha-1")
        .author("Lieuwe Rooijakkers <lieuwerooijakkers@gmail.com>")
        .about("A sane arbitrary precision rational calculator inspired by APL")
        .arg(
            Arg::new("format")
                .short('f')
                .long("format")
                .value_name("FORMAT")
                .help("Set the number output format"),
        )
        .arg(
            Arg::new("pre-exec")
                .long("pre")
                .value_name("PRE_EXEC")
                .help("A script to execute before showing prompt, all output except for errors are ignored. Useful for a custom prelude."),
        )
        .get_matches();

    let format = matches.get_one::<String>("format");
    let formatter = match format.map(String::as_str) {
        None | Some("rat" | "ratio") => Ok(Formatter::Ratio),
        Some("f" | "float") => Ok(Formatter::Float(None)),
        Some(s) => s.parse::<usize>().map(|n| Formatter::Float(Some(n))),
    }
    .map_err(|e| e.to_string())?;

    let scripts: Vec<(&String, String)> = matches
        .get_many::<String>("pre-exec")
        .unwrap_or_default()
        .map(|fname| fs::read_to_string(fname).map(|s| (fname, s)))
        .collect::<Result<_, _>>()
        .map_err(|e| e.to_string())?;

    let mut state = State {
        exec: Executor::new(),
        formatter,
    };

    for (fname, script) in scripts.into_iter() {
        for (i, line) in script.lines().enumerate() {
            if let Err(e) = state.exec_line(line) {
                eprintln!(
                    "error while executing script {} line {}: {}\n",
                    fname,
                    i + 1,
                    e
                )
            }
        }
    }

    let lines = std::io::stdin().lines();
    for line in lines {
        let line = match line {
            Err(e) => return Err(format!("error reading from stdin: {}", e)),
            Ok(l) => l,
        };

        match state.exec_line(&line) {
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

    Ok(())
}
