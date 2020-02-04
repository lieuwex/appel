use crate::ast::*;
use num_bigint::{BigInt, BigUint};
use num_traits::{One, Pow, Zero};
use pom::{self, parser::*};

type Parser<'a, T> = pom::parser::Parser<'a, char, T>;

fn left_recurse<'a, E: 'a, O: 'a>(
    atom_p: impl Fn() -> Parser<'a, E>,
    op_p: Parser<'a, O>,
    name: &'a str,
    combine: impl Fn(E, O, E) -> E + 'a,
) -> Parser<'a, E> {
    (atom_p() + (op_p + atom_p()).repeat(0..))
        .map(move |(expr, v)| {
            v.into_iter()
                .fold(expr, |acc, (op, expr2)| combine(acc, op, expr2))
        })
        .name(name)
}
fn right_recurse<'a, O: 'a>(
    atom_p: impl Fn() -> Parser<'a, Expr>,
    op_p: Parser<'a, O>,
    name: &'a str,
    combine: impl Fn(Expr, O, Expr) -> Expr + 'a,
) -> Parser<'a, Expr> {
    (atom_p() + (op_p + call(p_expr)).opt())
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
    symbol_both(operator("skip")).map(|_| BinOp::Skip)
        | symbol_both(operator("rho")).map(|_| BinOp::Rho)
        | symbol_both(operator("unpack")).map(|_| BinOp::Unpack)
        | symbol_both(operator("pack")).map(|_| BinOp::Pack)
        | symbol_both(operator("log")).map(|_| BinOp::Log)
        | symbol_both(operator(",")).map(|_| BinOp::Concat)
        | symbol_both(operator("in")).map(|_| BinOp::In)
        | symbol_both(operator("max")).map(|_| BinOp::Max)
        | symbol_both(operator("min")).map(|_| BinOp::Min)
        | operator("**").map(|_| BinOp::Pow)
        | operator("*").map(|_| BinOp::Mul)
        | operator("/").map(|_| BinOp::Div)
        | operator("%").map(|_| BinOp::Mod)
        | operator("+").map(|_| BinOp::Add)
        | operator("-").map(|_| BinOp::Sub)
        | comp_op().map(BinOp::CompOp)
}

fn check_reserved(s: String) -> Result<String, String> {
    let reserved = vec![
        "skip", "rho", "unpack", "pack", "log", "iota", "abs", "rev", "in", "max", "min",
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

fn integer_decimal(s: &[char]) -> Result<BigUint, num_bigint::ParseBigIntError> {
    if s.is_empty() {
        Ok(BigUint::zero())
    } else {
        s.iter().collect::<String>().parse::<BigUint>()
    }
}

fn integer_hex(s: &[char]) -> Result<BigUint, &str> {
    if s.is_empty() {
        return Err("Empty string cannot be a hexadecimal number");
    }

    let from_hex_digit = |c: char| {
        let c = c.to_ascii_lowercase();
        if '0' <= c && c <= '9' {
            Ok((c as u8) - b'0')
        } else if 'a' <= c && c <= 'f' {
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

    Ok(BigUint::from_bytes_be(&bytes))
}

fn p_hexdigit<'a>() -> Parser<'a, char> {
    is_a(|c: char| {
        let c = c.to_ascii_lowercase();
        ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
    })
    .name("hex")
}

fn p_digit<'a>() -> Parser<'a, char> {
    is_a(|c| '0' <= c && c <= '9').name("digit")
}

fn p_posexp<'a>() -> Parser<'a, BigUint> {
    (sym('e') * p_digit().repeat(1..).collect().convert(integer_decimal))
        | empty().map(|_| Zero::zero())
}

fn negative<T: 'static>(p: Parser<T>) -> Parser<T>
where
    T: std::ops::Neg<Output = T>,
{
    let p_sign = sym('-').opt().map(|o| o.is_some());
    (p_sign + p)
        .map(|(s, v)| if s { -v } else { v })
        .name("neg")
}

fn p_exp<'a>() -> Parser<'a, BigInt> {
    (sym('e')
        * negative(
            p_digit()
                .repeat(1..)
                .collect()
                .convert(integer_decimal)
                .map(BigInt::from),
        ))
        | empty().map(|_| Zero::zero())
}

fn p_int<'a>() -> Parser<'a, Atom> {
    let p = (tag("0x") * p_hexdigit().repeat(1..))
        .collect()
        .convert(integer_hex)
        | (p_digit().repeat(1..).collect().convert(integer_decimal) + p_posexp())
            .map(|(b, e)| b * BigUint::from(10u32).pow(e));
    negative(p.map(BigInt::from))
        .map(Ratio::from)
        .map(Atom::Rat)
        .name("int")
}

fn p_float<'a>() -> Parser<'a, Atom> {
    let p = (p_digit().repeat(1..) - sym('.') + p_digit().repeat(0..) + p_exp())
        | (p_digit().repeat(0..) - sym('.') + p_digit().repeat(1..) + p_exp());

    let p_ratio = p.convert(|((pre, post), exp)| {
        let ten = BigUint::from(10u32);
        let comma_exp = ten.pow(post.len());
        let base_num = integer_decimal(&pre)? * &comma_exp + integer_decimal(&post)?;
        let base_rat = Ratio::from((BigInt::from(base_num), BigInt::from(comma_exp)));
        Ok(match exp.to_biguint() {
            Some(pos_exp) => base_rat * Ratio::from(BigInt::from(ten.pow(pos_exp))),

            None => {
                let exp_pow = BigInt::from(ten.pow((-exp).to_biguint().unwrap()));
                let exp_rat = Ratio::from((One::one(), exp_pow));
                base_rat * exp_rat
            }
        }) as Result<Ratio, num_bigint::ParseBigIntError>
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
        | operator(",").map(|_| UnOp::Ravel)
        | symbol_both(operator("sin")).map(|_| UnOp::Sin)
        | symbol_both(operator("cos")).map(|_| UnOp::Cos)
        | symbol_both(operator("tan")).map(|_| UnOp::Tan)
        | symbol_both(operator("floor")).map(|_| UnOp::Floor)
        | symbol_both(operator("ceil")).map(|_| UnOp::Ceil);

    (unary.repeat(0..) + call(p_expr_1))
        .name("unary")
        .map(|(ops, ex)| {
            ops.into_iter()
                .rev()
                .fold(ex, |acc, op| Expr::Unary(op, Box::new(acc)))
        })
}

/// Vector of atom-like things
fn p_expr_3<'a>() -> Parser<'a, Expr> {
    list(call(p_expr_2), whitespace(1))
        .convert(|v| match v.len() {
            0 => Err(String::from("expected non-empty vector")),
            1 => Ok(v.into_iter().next().unwrap()),
            _ => Ok(Expr::Vector(v)),
        })
        .name("vector")
}

/// Binary comparison operators
fn p_expr_4<'a>() -> Parser<'a, Expr> {
    left_recurse(
        p_expr_3,
        symbol_both(comp_op()),
        "binary comparison",
        |e1, op, e2| Expr::Binary(Box::new(e1), BinOp::CompOp(op), Box::new(e2)),
    )
}

/// Power (**) of unary'd vector
fn p_expr_5<'a>() -> Parser<'a, Expr> {
    let op_p = symbol_both(operator("**")).map(|_| BinOp::Pow);
    right_recurse(p_expr_4, op_p, "power", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

/// Product (*, /, %) of powers
fn p_expr_6<'a>() -> Parser<'a, Expr> {
    let op_p = symbol_both(operator("*")).map(|_| BinOp::Mul)
        | symbol_both(operator("/")).map(|_| BinOp::Div)
        | symbol_both(operator("%")).map(|_| BinOp::Mod);
    left_recurse(p_expr_5, op_p, "product", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

/// Sum (+, -) of products
fn p_expr_7<'a>() -> Parser<'a, Expr> {
    let op_p =
        symbol_both(sym('+')).map(|_| BinOp::Add) | symbol_both(sym('-')).map(|_| BinOp::Sub);

    left_recurse(p_expr_6, op_p, "sum", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

fn p_expr_8<'a>() -> Parser<'a, Expr> {
    let op_un = symbol_both(operator("iota")).map(|_| UnOp::Iota)
        | symbol_both(operator("abs")).map(|_| UnOp::Abs)
        | symbol_both(operator("rho")).map(|_| UnOp::Rho)
        | symbol_both(operator("rev")).map(|_| UnOp::Rev)
        | symbol_both(operator("up")).map(|_| UnOp::Up)
        | symbol_both(operator("down")).map(|_| UnOp::Down)
        | symbol_both(operator("sgn")).map(|_| UnOp::Sign);

    (op_un.repeat(0..) + call(p_expr_7))
        .map(|(ops, ex)| {
            ops.into_iter()
                .rev()
                .fold(ex, |acc, op| Expr::Unary(op, Box::new(acc)))
        })
        .name("special unary")
}

fn p_expr_9<'a>() -> Parser<'a, Expr> {
    let op_bin = symbol_both(operator("skip")).map(|_| BinOp::Skip)
        | symbol_both(operator("rho")).map(|_| BinOp::Rho)
        | symbol_both(operator("unpack")).map(|_| BinOp::Unpack)
        | symbol_both(operator("pack")).map(|_| BinOp::Pack)
        | symbol_both(operator("log")).map(|_| BinOp::Log)
        | symbol_both(operator(",")).map(|_| BinOp::Concat)
        | symbol_both(operator("in")).map(|_| BinOp::In)
        | symbol_both(operator("max")).map(|_| BinOp::Max)
        | symbol_both(operator("min")).map(|_| BinOp::Min);

    right_recurse(p_expr_8, op_bin, "special binary", |e1, op, e2| {
        Expr::Binary(Box::new(e1), op, Box::new(e2))
    })
}

/// Scan
fn p_expr_10<'a>() -> Parser<'a, Expr> {
    let op_p = binary_op().map(FoldOp::BinOp) | p_varname().map(FoldOp::FunctionRef);
    let scan = (op_p - symbol_both(operator(r"\\")) + call(p_expr))
        .map(|(op, expr)| Expr::Scan(op, Box::new(expr)));

    scan.name("scan") | p_expr_9()
}

/// Fold
fn p_expr_11<'a>() -> Parser<'a, Expr> {
    let op_p = binary_op().map(FoldOp::BinOp) | p_varname().map(FoldOp::FunctionRef);
    let fold = (op_p - symbol_both(operator("//")) + call(p_expr))
        .map(|(op, expr)| Expr::Fold(op, Box::new(expr)));

    fold.name("fold") | p_expr_10()
}

fn p_expr<'a>() -> Parser<'a, Expr> {
    p_expr_11()
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
}
