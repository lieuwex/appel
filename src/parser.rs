use num_bigint::{BigInt, BigUint};
use num_traits::{Pow, One, Zero};
use pom::{self, parser::*};
use crate::ast::*;

type Parser<'a, T> = pom::parser::Parser<'a, char, T>;

fn left_recurse<'a, E: 'a, O: 'a>(
    atom_p: impl Fn() -> Parser<'a, E>,
    op_p: impl Fn() -> Parser<'a, O>,
    name: &'a str,
    combine: impl Fn(E, O, E) -> E + 'a
) -> Parser<'a, E> {
    (atom_p() + (op_p() + atom_p()).repeat(0..)).map(move |(mut expr, v)| {
        for (op, expr2) in v {
            expr = combine(expr, op, expr2);
        }
        expr
    }).name(name)
}

fn binary_op<'a>() -> Parser<'a, BinOp> {
    symbol(operator("**")).map(|_| BinOp::Pow)
    | symbol(operator("*")).map(|_| BinOp::Mul)
    | symbol(operator("/")).map(|_| BinOp::Div)
    | symbol(operator("%")).map(|_| BinOp::Mod)
    | symbol(operator("+")).map(|_| BinOp::Add)
    | symbol(operator("-")).map(|_| BinOp::Sub)
}

fn whitespace<'a>() -> Parser<'a, ()> {
    is_a(|c: char| c.is_ascii_whitespace()).repeat(0..).discard().name("whitespace")
}

fn symbol<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, T> {
    (whitespace() * p).name("symbol")
}

fn operator<'a>(text: &'static str) -> Parser<'a, ()> {
    tag(text).discard().name("op")
}

fn integer_decimal(s: &[char]) -> Result<BigUint, num_bigint::ParseBigIntError> {
    s.iter().collect::<String>().parse::<BigUint>()
}

fn integer_hex(s: &[char]) -> Result<BigUint, &str> {
    if s.len() == 0 {
        return Err("Empty string cannot be a hexadecimal number");
    }

    let from_hex_digit = |c: char| {
        let c = c.to_ascii_lowercase();
        if '0' <= c && c <= '9' { Ok((c as u8) - ('0' as u8)) }
        else if 'a' <= c && c <= 'f' { Ok((c as u8) - ('a' as u8) + 10) }
        else { Err("Invalid argument to from_hex_digit") }
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
        let c1 = s[2*i];
        let c2 = s[2*i+1];
        bytes.push(16 * from_hex_digit(c1)? + from_hex_digit(c2)?);
    }

    Ok(BigUint::from_bytes_be(&bytes))
}

fn p_hexdigit<'a>() -> Parser<'a, char> {
    is_a(|c: char| {
        let c = c.to_ascii_lowercase();
        ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
    }).name("hex")
}

fn p_digit<'a>() -> Parser<'a, char> {
    is_a(|c| '0' <= c && c <= '9').name("digit")
}

fn p_posexp<'a>() -> Parser<'a, BigUint> {
    sym('e') * p_digit().repeat(1..).collect().convert(integer_decimal)
        | empty().map(|_| Zero::zero())
}

fn negative<'a, T: 'static>(p: Parser<'a, T>) -> Parser<'a, T>
        where T: std::ops::Neg<Output = T> {
    let p_sign = sym('-').opt().map(|o| o.is_some());
    (p_sign + p).map(|(s, v)| if s { -v } else { v }).name("neg")
}

fn p_exp<'a>() -> Parser<'a, BigInt> {
    sym('e') * negative(p_digit().repeat(1..).collect().convert(integer_decimal).map(BigInt::from))
        | empty().map(|_| Zero::zero())
}

fn p_int<'a>() -> Parser<'a, Atom> {
    let p = (tag("0x") * p_hexdigit().repeat(1..))
                .collect().convert(integer_hex)
            | (p_digit().repeat(1..).collect().convert(integer_decimal) + p_posexp())
                .map(|(b, e)| b * BigUint::from(10u32).pow(e));
    negative(p.map(BigInt::from)).map(Ratio::from).map(Atom::Rat).name("int")
}

fn p_float<'a>() -> Parser<'a, Atom> {
    let p = (p_digit().repeat(1..) + sym('.') + p_digit().repeat(0..) + p_exp())
            | (p_digit().repeat(0..) + sym('.') + p_digit().repeat(1..) + p_exp());

    let p_ratio = p.convert(|(((pre, _), post), exp)| {
        let ten = BigUint::from(10u32);
        let comma_exp = ten.pow(post.len());
        let base_num = integer_decimal(&pre)? * &comma_exp + integer_decimal(&post)?;
        let base_rat = Ratio::from((BigInt::from(base_num), BigInt::from(comma_exp)));
        Ok(match exp.to_biguint() {
            Some(pos_exp) => {
                base_rat * Ratio::from(BigInt::from(ten.pow(pos_exp)))
            }

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
    (
        is_a(|c: char| c.is_alphabetic() || c == '_')
        + is_a(|c: char| c.is_alphanumeric() || c == '_').repeat(0..)
    ).collect().map(|s| s.iter().collect::<String>()).name("variable name")
}

fn p_var<'a>() -> Parser<'a, Atom> {
    p_varname().map(Atom::Ref).name("variable")
}

fn p_atom<'a>() -> Parser<'a, Atom> {
    symbol(p_float() | p_int() | p_var()).name("atom")
}

/// Parenthesised expression or plain atom
fn p_expr_0<'a>() -> Parser<'a, Expr> {
    (symbol(operator("(")) * call(p_expr) - symbol(operator(")"))).name("paren") | p_atom().map(Expr::Atom)
}

/// Vector of atom-like things
fn p_expr_1<'a>() -> Parser<'a, Expr> {
    p_expr_0().repeat(1..).map(|v| Expr::Vector(v.to_vec())).name("vector")
}

/// Unary operators on a vector
fn p_expr_2<'a>() -> Parser<'a, Expr> {
    fn p_unary<'a>() -> Parser<'a, UnOp> {
        symbol(
            operator("+").map(|_| UnOp::Id)
            | operator("-").map(|_| UnOp::Neg)
            | operator("!").map(|_| UnOp::Not)
            | operator("iota").map(|_| UnOp::Iota)
        ).name("unary")
    }

    (p_unary().repeat(0..) + p_expr_1()).map(|(ops, mut ex)| {
        for i in (0..ops.len()).rev() {
            ex = Expr::Unary(ops[i], Box::new(ex));
        }
        ex
    })
}

/// Power (**) of unary'd vector
fn p_expr_3<'a>() -> Parser<'a, Expr> {
    let op_p = || symbol(operator("**")).map(|_| BinOp::Pow);
    left_recurse(p_expr_2, op_p, "power", |e1, op, e2| Expr::Binary(Box::new(e1), op, Box::new(e2)))
}

/// Product (*, /, %) of powers
fn p_expr_4<'a>() -> Parser<'a, Expr> {
    let op_p = || symbol(operator("*")).map(|_| BinOp::Mul)
                    | symbol(operator("/")).map(|_| BinOp::Div)
                    | symbol(operator("%")).map(|_| BinOp::Mod);
    left_recurse(p_expr_3, op_p, "product", |e1, op, e2| Expr::Binary(Box::new(e1), op, Box::new(e2)))
}

/// Sum (+, -) of products
fn p_expr_5<'a>() -> Parser<'a, Expr> {
    let op_p = || symbol(operator("+")).map(|_| BinOp::Add)
                    | symbol(operator("-")).map(|_| BinOp::Sub);
    left_recurse(p_expr_4, op_p, "sum", |e1, op, e2| Expr::Binary(Box::new(e1), op, Box::new(e2)))
}

/// Fold
fn p_expr_6<'a>() -> Parser<'a, Expr> {
    let op_p = || binary_op().map(FoldOp::BinOp) | p_varname().map(FoldOp::FunctionRef);
    let fold = (op_p() - symbol(operator("//")) + call(p_expr)).map(|(op, expr)| Expr::Fold(op, Box::new(expr)));

    (fold).name("fold") | p_expr_5()
}

fn p_expr<'a>() -> Parser<'a, Expr> {
    p_expr_6()
}

/// Function declare
fn p_fun<'a>() -> Parser<'a, Statement> {
    (
        operator("fn") * symbol(p_varname())
        + symbol(p_varname()).repeat(1..) - symbol(operator("="))
        + call(p_expr)
    ).map(|((fnname, args), body)| Statement::FunDeclare(fnname, args, Box::new(body))).name("function")
}

/// Variable assignment
fn p_assign<'a>() -> Parser<'a, Statement> {
    (
        symbol(p_varname())
        - symbol(operator("="))
        + call(p_expr)
    ).map(|(name, body)| Statement::Assign(name, Box::new(body))).name("assign")
}

/// Internal command
fn p_command<'a>() -> Parser<'a, Statement> {
    let not_whitespace_word = || is_a(|c: char| !c.is_ascii_whitespace()).repeat(1..).map(|x| x.iter().collect::<String>());

    (
        symbol(operator(")"))
        * not_whitespace_word()
        - whitespace()
        + list(not_whitespace_word(), whitespace())
    ).map(|(name, vars)| Statement::InternalCommand(name, vars)).name("assign")
}

fn p_statement<'a>() -> Parser<'a, Statement> {
    p_fun() | p_assign() | p_command() | p_expr().map(Statement::Expr)
}

pub fn parse(source: &str) -> Result<Statement, pom::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let res = (p_statement() - whitespace() - end()).parse(&chars);
    res
}
