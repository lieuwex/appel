use num_bigint::{BigInt, BigUint};
use num_traits::Pow;
use pom::{self, parser::*};
use crate::ast::*;

type Parser<T> = pom::Parser<char, T>;

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

fn p_hexdigit() -> Parser<char> {
    is_a(|c: char| {
        let c = c.to_ascii_lowercase();
        ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
    })
}

fn p_digit() -> Parser<char> {
    is_a(|c| '0' <= c && c <= '9')
}

fn p_posexp() -> Parser<BigUint> {
    sym('e') * p_digit().repeat(1..).collect().convert(integer_decimal)
}

fn negative<T: 'static>(p: Parser<T>) -> Parser<T>
        where T: std::ops::Neg + std::ops::Mul<Output = T> + From<i32> {
    let p_sign = sym('-').opt().map(|o| if o.is_some() { -1 } else { 1 });
    (p_sign + p).map(|(s, v)| T::from(s) * v)
}

fn p_exp() -> Parser<BigInt> {
    sym('e') * negative(p_digit().repeat(1..).collect().convert(integer_decimal).map(BigInt::from))
}

fn p_int() -> Parser<Atom> {
    let p = (tag("0x") * p_hexdigit().repeat(1..))
                .collect().convert(integer_hex)
            | (p_digit().repeat(1..).collect().convert(integer_decimal) + p_posexp())
                .map(|(b, e)| b * BigUint::from(10u32).pow(e));
    negative(p.map(BigInt::from)).map(Atom::Int)
}

fn p_float() -> Parser<Atom> {
    let p = (p_digit().repeat(1..) + sym('.') + p_digit().repeat(0..) + p_exp()).collect()
            | (p_digit().repeat(0..) + sym('.') + p_digit().repeat(1..) + p_exp()).collect();

    (sym('-').repeat(0..2) + p).collect()
        .convert(|s| s.iter().collect::<String>().parse::<f64>())
        .map(Atom::Float)
}

// fn p_atom() -> Parser<Atom> {
//     p_int() | p_float() | p_var()
// }

// fn p_expression() -> Parser<Expr> {

// }
