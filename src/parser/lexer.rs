use std::fmt;

use rug::{Integer, ops::Pow};

use super::ParseError;
use crate::ast::Ratio;

/// Every word that has a meaning of its own, and therefore can't be used as a variable name.
pub const KEYWORDS: &[&str] = &[
    "let", "in", "fn", "drop", "rho", "unpack", "pack", "log", "union", "mask", "max", "min",
    "pad", "iota", "abs", "rev", "up", "down", "sgn", "sin", "cos", "tan", "floor", "ceil",
    "round",
];

/// Symbolic operators, longest first so that e.g. `**` is not lexed as two `*`.
const SYMBOLS: &[&str] = &[
    "**", "//", r"\\", "->", "==", "!=", "<=", ">=", "<<", ">>", "?i", "?f", "(", ")", "[", "]",
    "+", "-", "*", "/", r"\", "%", ",", ".", "=", "<", ">", "!",
];

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Number(Ratio),
    Ident(String),
    Keyword(&'static str),
    Symbol(&'static str),
    End,
}

impl TokenKind {
    /// The keyword or symbol text of this token, if it is one.
    pub fn text(&self) -> Option<&'static str> {
        match self {
            TokenKind::Keyword(s) | TokenKind::Symbol(s) => Some(s),
            _ => None,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Number(n) => write!(f, "number `{}`", n),
            TokenKind::Ident(s) => write!(f, "`{}`", s),
            TokenKind::Keyword(s) | TokenKind::Symbol(s) => write!(f, "`{}`", s),
            TokenKind::End => write!(f, "end of input"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    /// Character offset in the source.
    pub pos: usize,
    /// Whether this token is preceded by whitespace. The grammar is whitespace sensitive in a
    /// few places: `1 -2` is a vector, `1 - 2` a subtraction, and `a [1]` is not an index.
    pub space_before: bool,
}

/// Split `source` into tokens. The result always ends with a `TokenKind::End` token.
pub fn tokenize(source: &str) -> Result<Vec<Token>, ParseError> {
    let chars: Vec<char> = source.chars().collect();
    let mut tokens = Vec::new();
    let mut i = 0;

    loop {
        let start = i;
        while i < chars.len() && chars[i].is_ascii_whitespace() {
            i += 1;
        }
        let space_before = i > start;
        let pos = i;

        let Some(&c) = chars.get(i) else {
            tokens.push(Token {
                kind: TokenKind::End,
                pos,
                space_before,
            });
            return Ok(tokens);
        };

        let next_is_digit = chars.get(i + 1).is_some_and(|c| c.is_ascii_digit());
        let kind = if c.is_ascii_digit() || (c == '.' && next_is_digit) {
            let (n, len) = lex_number(&chars[i..]).map_err(|msg| ParseError::new(pos, msg))?;
            i += len;
            TokenKind::Number(n)
        } else if c.is_alphabetic() || c == '_' {
            let len = chars[i..]
                .iter()
                .take_while(|c| c.is_alphanumeric() || **c == '_')
                .count();
            let word: String = chars[i..i + len].iter().collect();
            i += len;
            match KEYWORDS.iter().find(|k| **k == word) {
                Some(k) => TokenKind::Keyword(k),
                None => TokenKind::Ident(word),
            }
        } else {
            let sym = SYMBOLS
                .iter()
                .find(|s| chars[i..].iter().copied().take(s.len()).eq(s.chars()))
                .ok_or_else(|| ParseError::new(pos, format!("unexpected character `{}`", c)))?;
            i += sym.len();
            TokenKind::Symbol(sym)
        };

        tokens.push(Token {
            kind,
            pos,
            space_before,
        });
    }
}

fn count_digits(s: &[char], is_digit: impl Fn(&char) -> bool) -> usize {
    s.iter().take_while(|c| is_digit(c)).count()
}

/// Lex a number at the start of `s`, returning its value and its length in characters.
///
/// Accepted forms are hexadecimal integers (`0xff`) and decimals with an optional fractional
/// part and exponent (`12`, `1.5`, `.5`, `1.`, `1e3`, `2.5e-3`).
fn lex_number(s: &[char]) -> Result<(Ratio, usize), String> {
    if s.starts_with(&['0', 'x']) {
        let len = count_digits(&s[2..], char::is_ascii_hexdigit);
        if len > 0 {
            let digits: String = s[2..2 + len].iter().collect();
            let n = Integer::from_str_radix(&digits, 16).map_err(|e| e.to_string())?;
            return Ok((Ratio::from(n), 2 + len));
        }
    }

    let int_len = count_digits(s, char::is_ascii_digit);
    let mut len = int_len;
    let mut frac: &[char] = &[];
    if s.get(len) == Some(&'.') {
        frac = &s[len + 1..len + 1 + count_digits(&s[len + 1..], char::is_ascii_digit)];
        len += 1 + frac.len();
    }

    // `e` only starts an exponent when digits follow, so `2e` stays a number followed by `e`.
    let mut exp: i64 = 0;
    if s.get(len) == Some(&'e') {
        let neg = s.get(len + 1) == Some(&'-');
        let digits_start = len + 1 + neg as usize;
        let exp_len = count_digits(&s[digits_start.min(s.len())..], char::is_ascii_digit);
        if exp_len > 0 {
            let digits: String = s[digits_start..digits_start + exp_len].iter().collect();
            exp = digits
                .parse::<u32>()
                .map_err(|_| String::from("exponent too large"))?
                .into();
            if neg {
                exp = -exp;
            }
            len = digits_start + exp_len;
        }
    }

    let mantissa: String = s[..int_len].iter().chain(frac).collect();
    let numer = mantissa.parse::<Integer>().map_err(|e| e.to_string())?;

    // The value is mantissa * 10^(exp - frac.len())
    let exp = exp - frac.len() as i64;
    let scale = Integer::from(10u32).pow(exp.unsigned_abs() as u32);
    let value = if exp >= 0 {
        Ratio::from(numer * scale)
    } else {
        Ratio::from((numer, scale))
    };
    Ok((value, len))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(s: &str) -> Vec<TokenKind> {
        tokenize(s).unwrap().into_iter().map(|t| t.kind).collect()
    }

    fn num(s: &str) -> Ratio {
        match kinds(s).as_slice() {
            [TokenKind::Number(n), TokenKind::End] => n.clone(),
            k => panic!("{} lexed as {:?}", s, k),
        }
    }

    #[test]
    fn test_numbers() {
        assert_eq!(num("1"), 1);
        assert_eq!(num("1.5"), Ratio::from((3, 2)));
        assert_eq!(num(".5"), Ratio::from((1, 2)));
        assert_eq!(num("1."), 1);
        assert_eq!(num("1.5e1"), 15);
        assert_eq!(num("1.5e-1"), Ratio::from((3, 20)));
        assert_eq!(num("1e3"), 1000);
        assert_eq!(num("1e-3"), Ratio::from((1, 1000)));
        assert_eq!(num("0xff"), 255);
        assert_eq!(num("0xABC"), 0xabc);
        assert_eq!(num("0"), 0);
        assert!(tokenize("1e99999999999").is_err());
    }

    #[test]
    fn test_keywords_need_word_boundary() {
        assert_eq!(kinds("single")[0], TokenKind::Ident(String::from("single")));
        assert_eq!(kinds("iotax")[0], TokenKind::Ident(String::from("iotax")));
        assert_eq!(kinds("iota")[0], TokenKind::Keyword("iota"));
    }

    #[test]
    fn test_symbols() {
        assert_eq!(
            kinds(r"** * // / \\ \ <= < << -> -"),
            [
                "**", "*", "//", "/", r"\\", r"\", "<=", "<", "<<", "->", "-"
            ]
            .iter()
            .map(|s| TokenKind::Symbol(s))
            .chain([TokenKind::End])
            .collect::<Vec<_>>()
        );
        assert!(tokenize("a & b").is_err());
    }
}
