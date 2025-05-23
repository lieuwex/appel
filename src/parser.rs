use crate::ast::*;

use pom::{self, parser::*};

use num_traits::*;
use rug::{integer::ParseIntegerError, ops::Pow, Integer, Rational};

type Parser<'a, T> = pom::parser::Parser<'a, char, T>;

fn left_recurse<'a, E: 'a, O: 'a>(
    atom_l_p: impl Fn() -> Parser<'a, E>,
    op_p: Parser<'a, O>,
    atom_r_p: impl Fn() -> Parser<'a, E> + 'a,
    name: &'a str,
    combine: impl Fn(E, O, E) -> E + 'a,
) -> Parser<'a, E> {
    (atom_l_p() + (op_p + call(atom_r_p)).repeat(0..))
        .map(move |(expr, v)| {
            v.into_iter()
                .fold(expr, |acc, (op, expr2)| combine(acc, op, expr2))
        })
        .name(name)
}
fn right_recurse<'a, O: 'a>(
    atom_l_p: impl Fn() -> Parser<'a, Expr>,
    op_p: Parser<'a, O>,
    atom_r_p: impl Fn() -> Parser<'a, Expr> + 'a,
    name: &'a str,
    combine: impl Fn(Expr, O, Expr) -> Expr + 'a,
) -> Parser<'a, Expr> {
    (atom_l_p() + (op_p + call(atom_r_p)).opt())
        .map(move |(a, b)| match b {
            None => a,
            Some((op, b)) => combine(a, op, b),
        })
        .name(name)
}

fn comp_op<'a>() -> Parser<'a, CompOp> {
    operator("==").map(|_| CompOp::Eq)
        | operator("!=").map(|_| CompOp::Neq)
        | operator("<").map(|_| CompOp::Lt)
        | operator("<=").map(|_| CompOp::Le)
        | operator(">").map(|_| CompOp::Gt)
        | operator(">=").map(|_| CompOp::Ge)
}

fn binary_op<'a>() -> Parser<'a, BinOp> {
    symbol_both(operator("drop")).map(|_| BinOp::Drop)
        | symbol_both(operator("rho")).map(|_| BinOp::Rho)
        | symbol_both(operator("unpack")).map(|_| BinOp::Unpack)
        | symbol_both(operator("pack")).map(|_| BinOp::Pack)
        | symbol_both(operator("log")).map(|_| BinOp::Log)
        | symbol_both(operator(",")).map(|_| BinOp::Concat)
        | symbol_both(operator("in")).map(|_| BinOp::In)
        | symbol_both(operator("union")).map(|_| BinOp::Union)
        | symbol_both(operator("mask")).map(|_| BinOp::Mask)
        | symbol_both(operator("max")).map(|_| BinOp::Max)
        | symbol_both(operator("min")).map(|_| BinOp::Min)
        | symbol_both(operator("pad")).map(|_| BinOp::Pad)
        | operator("**").map(|_| BinOp::Pow)
        | operator("*").map(|_| BinOp::Mul)
        | operator("/").map(|_| BinOp::Div)
        | operator("%").map(|_| BinOp::Mod)
        | operator("+").map(|_| BinOp::Add)
        | operator("-").map(|_| BinOp::Sub)
        | comp_op().map(BinOp::CompOp)
}

fn check_reserved(s: String) -> Result<String, String> {
    let reserved = [
        "drop", "rho", "unpack", "pack", "log", "iota", "abs", "rev", "in", "union", "mask", "max",
        "min", "pad", "let", "fn",
    ];

    if reserved.contains(&s.as_str()) {
        Err(format!("{} is a reserved keyword", s))
    } else {
        Ok(s)
    }
}

fn whitespace<'a>(min: usize) -> Parser<'a, ()> {
    is_a(|c: char| c.is_ascii_whitespace())
        .repeat(min..)
        .discard()
        .name("whitespace")
}

fn symbol_left<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, T> {
    (whitespace(0) * p).name("symbol (whitespace left)")
}
fn symbol_right<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, T> {
    (p - whitespace(0)).name("symbol (whitespace right)")
}
fn symbol_both<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, T> {
    (whitespace(0) * p - whitespace(0)).name("symbol (whitespace both)")
}

fn operator<'a>(text: &'static str) -> Parser<'a, ()> {
    tag(text).discard().name("op")
}

fn integer_decimal(s: &[char]) -> Result<Integer, ParseIntegerError> {
    if s.is_empty() {
        Ok(Integer::ZERO)
    } else {
        s.iter().collect::<String>().parse::<Integer>()
    }
}

fn integer_hex(s: &[char]) -> Result<Integer, &str> {
    if s.is_empty() {
        return Err("Empty string cannot be a hexadecimal number");
    }

    let from_hex_digit = |c: char| {
        let c = c.to_ascii_lowercase();
        if c.is_ascii_digit() {
            Ok((c as u8) - b'0')
        } else if ('a'..='f').contains(&c) {
            Ok((c as u8) - b'a' + 10)
        } else {
            Err("Invalid argument to from_hex_digit")
        }
    };

    let num_bytes = (s.len() + 1) / 2;
    let num_full_bytes = s.len() / 2;

    let mut bytes: Vec<u8> = Vec::with_capacity(num_bytes);
    let s = if s.len() % 2 == 1 {
        bytes.push(from_hex_digit(s[0])?);
        &s[1..]
    } else {
        s
    };

    for i in 0..num_full_bytes {
        let c1 = s[2 * i];
        let c2 = s[2 * i + 1];
        bytes.push(16 * from_hex_digit(c1)? + from_hex_digit(c2)?);
    }

    let mut res = Integer::new();
    res.assign_digits(&bytes, rug::integer::Order::Msf);
    Ok(res)
}

fn p_hexdigit<'a>() -> Parser<'a, char> {
    is_a(|c: char| {
        let c = c.to_ascii_lowercase();
        c.is_ascii_digit() || ('a'..='f').contains(&c)
    })
    .name("hex")
}

fn p_digit<'a>() -> Parser<'a, char> {
    is_a(|c: char| c.is_ascii_digit()).name("digit")
}

fn p_posexp<'a>() -> Parser<'a, Integer> {
    (sym('e') * p_digit().repeat(1..).collect().convert(integer_decimal))
        | empty().map(|_| Integer::ZERO)
}

fn negative<T>(p: Parser<T>) -> Parser<T>
where
    T: std::ops::Neg<Output = T> + 'static,
{
    let p_sign = sym('-').opt().map(|o| o.is_some());
    (p_sign + p)
        .map(|(s, v)| if s { -v } else { v })
        .name("neg")
}

fn p_exp<'a>() -> Parser<'a, Integer> {
    (sym('e')
        * negative(
            p_digit()
                .repeat(1..)
                .collect()
                .convert(integer_decimal)
                .map(Integer::from),
        ))
        | empty().map(|_| Integer::ZERO)
}

fn p_int<'a>() -> Parser<'a, Atom> {
    let p = (tag("0x") * p_hexdigit().repeat(1..))
        .collect()
        .convert(integer_hex)
        | (p_digit().repeat(1..).collect().convert(integer_decimal) + p_posexp())
            .map(|(b, e)| b * Integer::from(10u32).pow(e.to_u32().unwrap()));
    negative(p.map(Integer::from))
        .map(Ratio::from)
        .map(Atom::Rat)
        .name("int")
}

fn p_float<'a>() -> Parser<'a, Atom> {
    let p = (p_digit().repeat(1..) - sym('.') + p_digit().repeat(0..) + p_exp())
        | (p_digit().repeat(0..) - sym('.') + p_digit().repeat(1..) + p_exp());

    let p_ratio = p.convert(|((pre, post), exp)| {
        let ten = Integer::from(10u32);
        let comma_exp = ten.clone().pow(post.len() as u32);
        let base_num = integer_decimal(&pre)? * &comma_exp + integer_decimal(&post)?;
        let base_rat = Ratio::from((base_num, comma_exp));
        let res = if (&exp).signum() == -1 {
            let exp = (-exp).to_u32().unwrap();
            let exp_pow = ten.pow(exp);
            let exp_rat = Rational::from((Integer::one(), exp_pow));
            base_rat * exp_rat
        } else {
            let exp = exp.to_u32().unwrap();
            base_rat * Ratio::from(ten.pow(exp))
        };
        Ok::<_, ParseIntegerError>(res)
    });

    negative(p_ratio).map(Atom::Rat).name("float")
}

fn p_varname<'a>() -> Parser<'a, String> {
    let name = is_a(|c: char| c.is_alphabetic() || c == '_')
        + is_a(|c: char| c.is_alphanumeric() || c == '_').repeat(0..);

    let op = name
        .collect()
        .map(|s| s.iter().collect::<String>())
        .name("variable name");

    op.convert(check_reserved)
}

fn p_var<'a>() -> Parser<'a, Atom> {
    p_varname().map(Atom::Ref).name("variable")
}

fn p_atom<'a>() -> Parser<'a, Atom> {
    (p_float() | p_int() | p_var()).name("atom")
}

/// Parenthesised expression, or plain atom
fn p_expr_0<'a>() -> Parser<'a, Expr> {
    let atom = call(p_atom).map(Expr::Atom);

    (symbol_right(operator("(")) * call(p_expr) - symbol_left(operator(")"))).name("paren") | atom
}

/// Indexing
fn p_expr_1<'a>() -> Parser<'a, Expr> {
    let index = symbol_right(sym('[')) * whitespace(0) * call(p_expr)
        - whitespace(0)
        - symbol_left(sym(']'));

    (call(p_expr_0) + index)
        .name("index")
        .map(|(a, b)| Expr::Index(Box::new(a), Box::new(b)))
        | call(p_expr_0)
}

fn p_expr_2<'a>() -> Parser<'a, Expr> {
    let unary = operator("+").map(|_| UnOp::Id)
        | operator("-").map(|_| UnOp::Neg)
        | operator("!").map(|_| UnOp::Not)
        | operator("?i").map(|_| UnOp::RollInt)
        | operator("?f").map(|_| UnOp::RollFloat)
        | symbol_both(operator("sin")).map(|_| UnOp::Sin)
        | symbol_both(operator("cos")).map(|_| UnOp::Cos)
        | symbol_both(operator("tan")).map(|_| UnOp::Tan)
        | symbol_both(operator("floor")).map(|_| UnOp::Floor)
        | symbol_both(operator("ceil")).map(|_| UnOp::Ceil)
        | symbol_both(operator("round")).map(|_| UnOp::Round);

    (unary.repeat(0..) + call(p_expr_1))
        .name("unary")
        .map(|(ops, ex)| {
            ops.into_iter()
                .rev()
                .fold(ex, |acc, op| Expr::Unary(op, Box::new(acc)))
        })
}

/// Binary comparison operators
fn p_expr_3<'a>() -> Parser<'a, Expr> {
    left_recurse(
        p_expr_2,
        symbol_both(comp_op()),
        p_expr_2,
        "binary comparison",
        |e1, op, e2| Expr::Binary(Box::new(e1), BinOp::CompOp(op), Box::new(e2)),
    )
}

/// Power (**) of unary'd vector
fn p_expr_4<'a>() -> Parser<'a, Expr> {
    let op_p = symbol_both(operator("**")).map(|_| BinOp::Pow);
    right_recurse(p_expr_3, op_p, p_expr_4, "power", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

/// Product (*, /, %) of powers
fn p_expr_5<'a>() -> Parser<'a, Expr> {
    let op_p = symbol_both(operator("*")).map(|_| BinOp::Mul)
        | symbol_both(operator("/")).map(|_| BinOp::Div)
        | symbol_both(operator("%")).map(|_| BinOp::Mod);
    left_recurse(p_expr_4, op_p, p_expr_4, "product", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

/// Vector of atom-like things
fn p_expr_6<'a>() -> Parser<'a, Expr> {
    list(call(p_expr_5), whitespace(1))
        .convert(|v| match v.len() {
            0 => Err(String::from("expected non-empty vector")),
            1 => Ok(v.into_iter().next().unwrap()),
            _ => Ok(Expr::Vector(v)),
        })
        .name("vector")
}

fn p_expr_7<'a>() -> Parser<'a, Expr> {
    let op_un = symbol_both(operator("iota")).map(|_| UnOp::Iota)
        | symbol_both(operator("abs")).map(|_| UnOp::Abs)
        | symbol_both(operator("rho")).map(|_| UnOp::Rho)
        | symbol_both(operator("rev")).map(|_| UnOp::Rev)
        | symbol_both(operator("up")).map(|_| UnOp::Up)
        | symbol_both(operator("down")).map(|_| UnOp::Down)
        | symbol_both(operator("sgn")).map(|_| UnOp::Sign);

    (op_un.repeat(0..) + call(p_expr_6))
        .map(|(ops, ex)| {
            ops.into_iter()
                .rev()
                .fold(ex, |acc, op| Expr::Unary(op, Box::new(acc)))
        })
        .name("special unary")
}

/// Sum (+, -) of products
fn p_expr_8<'a>() -> Parser<'a, Expr> {
    let op_p =
        symbol_both(sym('+')).map(|_| BinOp::Add) | symbol_both(sym('-')).map(|_| BinOp::Sub);

    left_recurse(p_expr_7, op_p, p_expr_7, "sum", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

fn p_expr_9<'a>() -> Parser<'a, Expr> {
    let op_bin = symbol_both(operator("drop")).map(|_| BinOp::Drop)
        | symbol_both(operator("rho")).map(|_| BinOp::Rho)
        | symbol_both(operator("unpack")).map(|_| BinOp::Unpack)
        | symbol_both(operator("pack")).map(|_| BinOp::Pack)
        | symbol_both(operator("log")).map(|_| BinOp::Log)
        | symbol_both(operator(",")).map(|_| BinOp::Concat)
        | symbol_both(operator("in")).map(|_| BinOp::In)
        | symbol_both(operator("union")).map(|_| BinOp::Union)
        | symbol_both(operator("mask")).map(|_| BinOp::Mask)
        | symbol_both(operator("max")).map(|_| BinOp::Max)
        | symbol_both(operator("min")).map(|_| BinOp::Min)
        | symbol_both(operator("pad")).map(|_| BinOp::Pad);

    right_recurse(
        p_expr_8,
        op_bin,
        p_expr_8,
        "special binary",
        |e1, op, e2| Expr::Binary(Box::new(e1), op, Box::new(e2)),
    )
}

fn p_foldlike<'a>(ops: &'static str) -> Parser<'a, (FoldOp, Box<Expr>)> {
    let op_p = binary_op().map(FoldOp::BinOp);
    let op =
        (op_p - symbol_both(operator(ops)) + call(p_expr)).map(|(op, expr)| (op, Box::new(expr)));

    let expr = (call(p_expr_0) - symbol_both(operator(ops)) + call(p_expr))
        .map(|(op, expr)| (FoldOp::Expr(Box::new(op)), Box::new(expr)));

    op | expr
}

/// Scan
fn p_expr_10<'a>() -> Parser<'a, Expr> {
    (p_foldlike(r"\\")
        .map(|(op, expr)| Expr::Scan(op, expr))
        .name("scan"))
        | p_expr_9()
}

/// Fold
fn p_expr_11<'a>() -> Parser<'a, Expr> {
    (p_foldlike("//")
        .map(|(op, expr)| Expr::Fold(op, expr))
        .name("fold"))
        | p_expr_10()
}

/// Map
fn p_expr_12<'a>() -> Parser<'a, Expr> {
    (p_foldlike(". ")
        .map(|(op, expr)| Expr::Map(op, expr))
        .name("map"))
        | p_expr_11()
}

/// Let binding
fn p_expr_13<'a>() -> Parser<'a, Expr> {
    let let_binding = ((symbol_right(operator("let")) * p_varname())
        + (symbol_both(operator("=")) * call(p_expr_0))
        + (symbol_both(operator("in")) * call(p_expr)))
    .map(|((name, expr), body)| Expr::Let(name, Box::new(expr), Box::new(body)));

    let_binding.name("let_binding") | p_expr_12()
}

/// Lambda function
fn p_expr_14<'a>() -> Parser<'a, Expr> {
    let lambda = ((symbol_both(operator("\\")) * (p_varname() - whitespace(1)).repeat(1..))
        + (symbol_both(operator("->")) * call(p_expr)))
    .map(|(variables, body)| Expr::Lambda(variables, Box::new(body)));

    lambda.name("lambda") | p_expr_13()
}

fn p_expr<'a>() -> Parser<'a, Expr> {
    p_expr_14()
}

/// Function declare
fn p_fun<'a>() -> Parser<'a, Statement> {
    (symbol_right(operator("fn")) * p_varname() + (whitespace(1) * p_varname()).repeat(1..)
        - symbol_both(operator("="))
        + call(p_expr))
    .map(|((fnname, args), body)| Statement::FunDeclare(fnname, args, body))
    .name("function")
}

/// Variable assignment
fn p_assign<'a>() -> Parser<'a, Statement> {
    (p_varname() - symbol_both(operator("=")) + call(p_expr))
        .map(|(name, body)| Statement::Assign(name, body))
        .name("assign")
}

/// Internal command
fn p_command<'a>() -> Parser<'a, Statement> {
    let not_whitespace_word = || {
        is_a(|c: char| !c.is_ascii_whitespace())
            .repeat(1..)
            .map(|x| x.iter().collect::<String>())
    };

    (symbol_right(operator(")")) * not_whitespace_word() - whitespace(0)
        + is_a(|_| true)
            .repeat(0..)
            .collect()
            .map(|s| s.iter().collect::<String>()))
    .map(|(name, body)| Statement::InternalCommand(name, body))
    .name("assign")
}

fn p_statement<'a>() -> Parser<'a, Statement> {
    p_fun() | p_assign() | p_command() | p_expr().map(Statement::Expr)
}

pub fn parse(source: &str) -> Result<Option<Statement>, pom::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let parser = whitespace(0) * p_statement().opt() - whitespace(0) - end();
    parser.parse(&chars)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! is_ok_some {
        ($v:expr) => {
            match $v {
                Err(_) => false,
                Ok(None) => false,
                Ok(Some(_)) => true,
            }
        };
    }

    #[test]
    fn test_num() {
        assert!(is_ok_some!(parse("1")));
        assert!(is_ok_some!(parse("1.5")));
        assert!(is_ok_some!(parse(".5")));
        assert!(is_ok_some!(parse("1.")));
        assert!(is_ok_some!(parse("1.5e1")));
        assert!(is_ok_some!(parse("1.5e-1")));
        assert!(is_ok_some!(parse(".5e9")));
        assert!(is_ok_some!(parse("-.5e9")));
    }

    #[test]
    fn test_var() {
        assert!(!is_ok_some!(parse("1kaas")));
        assert!(is_ok_some!(parse("kaas1")));

        assert!(is_ok_some!(parse("kaas1 = a")));
    }

    #[test]
    fn test_vec() {
        assert!(is_ok_some!(parse("1 2 3")));
        assert!(is_ok_some!(parse("a b c")));
        assert!(is_ok_some!(parse("a 2 c")));
        assert!(is_ok_some!(parse("a (a b) c")));

        assert_eq!(
            parse(".1 .2"),
            Ok(Some(Statement::Expr(Expr::Vector(vec![
                Expr::Atom(Atom::Rat(Ratio::from((1, 10)))),
                Expr::Atom(Atom::Rat(Ratio::from((1, 5))))
            ]))))
        );
    }

    #[test]
    fn test_fn() {
        assert!(is_ok_some!(parse("fn test a = a")));
        assert!(is_ok_some!(parse("fn test a = a + a")));
        assert!(is_ok_some!(parse("fn test a = +//a")));
        assert!(is_ok_some!(parse("fn test a = +//a")));
        assert!(!is_ok_some!(parse("fn a = +//a")));
    }

    #[test]
    fn test_binop() {
        assert!(is_ok_some!(parse("a+ b")));
        assert!(is_ok_some!(parse("a+b")));
        assert!(is_ok_some!(parse("a + b")));
        assert!(is_ok_some!(parse("a ** b + c")));
    }

    #[test]
    fn test_paren() {
        assert!(is_ok_some!(parse("( a + b )")));
        assert!(is_ok_some!(parse("( a + (b) )")));
        assert!(is_ok_some!(parse("( a + (b * c) )")));
        assert!(is_ok_some!(parse("1 2 (3 4)")));
        assert!(is_ok_some!(parse("(3 4) 2 (3 4)")));
        assert!(is_ok_some!(parse("(1+ 2 )*3")));
    }

    #[test]
    fn test_let() {
        assert!(is_ok_some!(parse("let a = 1 in a")));
        assert!(!is_ok_some!(parse("let a in 5")));
        assert!(is_ok_some!(parse("let a = (let b = 6 in b) in a")));
    }

    #[test]
    fn test_lambda() {
        assert!(is_ok_some!(parse(r"f = (\x -> x*2)")));
        assert!(is_ok_some!(parse(r"f = \x -> x*2")));
        assert!(is_ok_some!(parse(r"f = \a b -> a + b")));

        assert!(is_ok_some!(parse(r"(\x -> x*2) . 1 2 3")));
        assert!(!is_ok_some!(parse(r"(\-> x*2) . (1 2 3)")));
        assert!(!is_ok_some!(parse(r"(\x x*2) . (1 2 3)")));
        assert!(!is_ok_some!(parse(r"(x -> x*2) . (1 2 3)")));
    }

    #[test]
    fn test_map() {
        assert!(is_ok_some!(parse(r"f . 1 2 3")));
    }
}
