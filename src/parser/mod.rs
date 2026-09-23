//! Parser for appel statements.
//!
//! The source is first split into tokens (see [`lexer`]), after which a recursive descent parser
//! builds the AST. Expressions are parsed by the functions below, from loosest to tightest
//! binding:
//!
//! | construct                                        | associativity | function           |
//! |--------------------------------------------------|---------------|--------------------|
//! | `\x -> e`, `let x = a in e`, `f // e`, `f \\ e`, `f . e` | prefix | [`Parser::open_ended`] |
//! | `drop rho unpack pack log , in union mask max min pad` | right   | [`Parser::special_binary`] |
//! | `== != < <= > >=`                                | left          | [`Parser::comparison`] |
//! | `+ - << >>`                                      | left          | [`Parser::sum`]    |
//! | `iota abs rho rev up down sgn`                   | prefix        | [`Parser::word_unary`] |
//! | vector (`1 2 3`, function application `f x`)     |               | [`Parser::vector`] |
//! | `* / %`                                          | left          | [`Parser::product`] |
//! | `**`                                             | right         | [`Parser::power`]  |
//! | `+ - ! ?i ?f sin cos tan floor ceil round`       | prefix        | [`Parser::prefix`] |
//! | indexing `a[i]`                                  | postfix       | [`Parser::postfix`] |
//! | number, variable, `(e)`                          |               | [`Parser::primary`] |
//!
//! A prefix construct may start any operand, even when it binds looser than the operator before
//! it, and then extends as far to the right as its own precedence allows. So `2 ** iota 5 + 1`
//! is `(2 ** (iota 5)) + 1`, and `1 + let a = 2 in a * a` is `1 + (let a = 2 in (a * a))`.
//!
//! Vector elements are separated by whitespace, and a vector continues only with an element that
//! starts with a tight operand: `1 -2` is a vector of `1` and `-2`, while `1 - 2` and `1-2` are
//! subtractions and `1 iota 2` is an error.

mod lexer;

use std::fmt;

use crate::ast::*;
use lexer::{Token, TokenKind, tokenize};

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    /// Character offset in the source.
    pub pos: usize,
    pub message: String,
}

impl ParseError {
    fn new(pos: usize, message: impl Into<String>) -> Self {
        Self {
            pos,
            message: message.into(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (at position {})", self.message, self.pos)
    }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

const TIGHT_UNARY: &[(&str, UnOp)] = &[
    ("+", UnOp::Id),
    ("-", UnOp::Neg),
    ("!", UnOp::Not),
    ("?i", UnOp::RollInt),
    ("?f", UnOp::RollFloat),
    ("sin", UnOp::Sin),
    ("cos", UnOp::Cos),
    ("tan", UnOp::Tan),
    ("floor", UnOp::Floor),
    ("ceil", UnOp::Ceil),
    ("round", UnOp::Round),
];

const WORD_UNARY: &[(&str, UnOp)] = &[
    ("iota", UnOp::Iota),
    ("abs", UnOp::Abs),
    ("rho", UnOp::Rho),
    ("rev", UnOp::Rev),
    ("up", UnOp::Up),
    ("down", UnOp::Down),
    ("sgn", UnOp::Sign),
];

const POWER: &[(&str, BinOp)] = &[("**", BinOp::Pow)];

const PRODUCT: &[(&str, BinOp)] = &[("*", BinOp::Mul), ("/", BinOp::Div), ("%", BinOp::Mod)];

const SUM: &[(&str, BinOp)] = &[
    ("+", BinOp::Add),
    ("-", BinOp::Sub),
    ("<<", BinOp::LeftShift),
    (">>", BinOp::RightShift),
];

const COMPARISON: &[(&str, BinOp)] = &[
    ("==", BinOp::CompOp(CompOp::Eq)),
    ("!=", BinOp::CompOp(CompOp::Neq)),
    ("<", BinOp::CompOp(CompOp::Lt)),
    ("<=", BinOp::CompOp(CompOp::Le)),
    (">", BinOp::CompOp(CompOp::Gt)),
    (">=", BinOp::CompOp(CompOp::Ge)),
];

const SPECIAL_BINARY: &[(&str, BinOp)] = &[
    ("drop", BinOp::Drop),
    ("rho", BinOp::Rho),
    ("unpack", BinOp::Unpack),
    ("pack", BinOp::Pack),
    ("log", BinOp::Log),
    (",", BinOp::Concat),
    ("in", BinOp::In),
    ("union", BinOp::Union),
    ("mask", BinOp::Mask),
    ("max", BinOp::Max),
    ("min", BinOp::Min),
    ("pad", BinOp::Pad),
];

/// Every binary operator, usable as the operator of a fold, scan or map (`+//xs`).
const ALL_BINARY: &[&[(&str, BinOp)]] = &[POWER, PRODUCT, SUM, COMPARISON, SPECIAL_BINARY];

fn lookup<T: Copy>(table: &[(&str, T)], token: &Token) -> Option<T> {
    let text = token.kind.text()?;
    table.iter().find(|(s, _)| *s == text).map(|(_, v)| *v)
}

fn lookup_binary(token: &Token) -> Option<BinOp> {
    ALL_BINARY.iter().find_map(|table| lookup(table, token))
}

fn fold_kind(token: &Token) -> Option<fn(FoldOp, Box<Expr>) -> Expr> {
    match token.kind {
        TokenKind::Symbol("//") => Some(Expr::Fold),
        TokenKind::Symbol(r"\\") => Some(Expr::Scan),
        TokenKind::Symbol(".") => Some(Expr::Map),
        _ => None,
    }
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Token {
        self.peek_at(0)
    }

    fn peek_at(&self, offset: usize) -> &Token {
        let last = self.tokens.len() - 1; // always the End token
        &self.tokens[(self.pos + offset).min(last)]
    }

    fn advance(&mut self) -> Token {
        let token = self.peek().clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }

    fn is(&self, text: &str) -> bool {
        self.peek().kind.text() == Some(text)
    }

    fn eat(&mut self, text: &str) -> bool {
        let found = self.is(text);
        if found {
            self.advance();
        }
        found
    }

    fn expect(&mut self, text: &str) -> Result<()> {
        if self.eat(text) {
            Ok(())
        } else {
            Err(self.unexpected(&format!("`{}`", text)))
        }
    }

    fn unexpected(&self, expected: &str) -> ParseError {
        let token = self.peek();
        ParseError::new(
            token.pos,
            format!("expected {}, found {}", expected, token.kind),
        )
    }

    /// Consume the next token if it is an operator in `table`.
    fn eat_op<T: Copy>(&mut self, table: &[(&str, T)]) -> Option<T> {
        let op = lookup(table, self.peek())?;
        self.advance();
        Some(op)
    }

    fn ident(&mut self) -> Result<String> {
        match &self.peek().kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => Err(self.unexpected("a name")),
        }
    }

    /// Parse a left associative chain of `operand (op operand)*`.
    fn left_assoc(
        &mut self,
        ops: &[(&str, BinOp)],
        operand: fn(&mut Self) -> Result<Expr>,
    ) -> Result<Expr> {
        let mut lhs = operand(self)?;
        while let Some(op) = self.eat_op(ops) {
            let rhs = operand(self)?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    /// Parse a right associative chain of `operand (op operand)*`.
    fn right_assoc(
        &mut self,
        ops: &[(&str, BinOp)],
        operand: fn(&mut Self) -> Result<Expr>,
    ) -> Result<Expr> {
        let lhs = operand(self)?;
        match self.eat_op(ops) {
            Some(op) => {
                let rhs = self.right_assoc(ops, operand)?;
                Ok(Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
            }
            None => Ok(lhs),
        }
    }

    /// Is the parser at the start of a fold, scan or map? Their operator is a binary operator
    /// or a primary expression, and is followed by `//`, `\\` or `.`.
    fn at_fold(&self) -> bool {
        if lookup_binary(self.peek()).is_some() && fold_kind(self.peek_at(1)).is_some() {
            return true;
        }

        let after_primary = match self.peek().kind {
            TokenKind::Number(_) | TokenKind::Ident(_) => 1,
            TokenKind::Symbol("(") => {
                let mut depth = 0;
                let mut offset = 0;
                loop {
                    match self.peek_at(offset).kind {
                        TokenKind::Symbol("(") => depth += 1,
                        TokenKind::Symbol(")") => depth -= 1,
                        TokenKind::End => return false,
                        _ => {}
                    }
                    offset += 1;
                    if depth == 0 {
                        break offset;
                    }
                }
            }
            _ => return false,
        };
        fold_kind(self.peek_at(after_primary)).is_some()
    }

    fn at_open_ended(&self) -> bool {
        self.is(r"\") || self.is("let") || self.at_fold()
    }

    fn expr(&mut self) -> Result<Expr> {
        if self.at_open_ended() {
            self.open_ended()
        } else {
            self.special_binary()
        }
    }

    /// Lambda, let binding, fold, scan or map. Their body extends as far to the right as
    /// possible.
    fn open_ended(&mut self) -> Result<Expr> {
        if self.eat(r"\") {
            let mut params = vec![self.ident()?];
            while !self.eat("->") {
                params.push(
                    self.ident()
                        .map_err(|_| self.unexpected("a name or `->`"))?,
                );
            }
            let body = self.expr()?;
            return Ok(Expr::Lambda(params, Box::new(body)));
        }

        if self.eat("let") {
            let name = self.ident()?;
            self.expect("=")?;
            // `in` is also a binary operator, so the value can't contain one unparenthesised.
            let value = self.comparison()?;
            self.expect("in")?;
            let body = self.expr()?;
            return Ok(Expr::Let(name, Box::new(value), Box::new(body)));
        }

        let op = match lookup_binary(self.peek()) {
            Some(op) if fold_kind(self.peek_at(1)).is_some() => {
                self.advance();
                FoldOp::BinOp(op)
            }
            _ => FoldOp::Expr(Box::new(self.primary()?)),
        };
        let make = fold_kind(self.peek()).ok_or_else(|| self.unexpected("`//`, `\\\\` or `.`"))?;
        self.advance();
        let body = self.expr()?;
        Ok(make(op, Box::new(body)))
    }

    fn special_binary(&mut self) -> Result<Expr> {
        self.right_assoc(SPECIAL_BINARY, Self::comparison)
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.left_assoc(COMPARISON, Self::sum)
    }

    fn sum(&mut self) -> Result<Expr> {
        self.left_assoc(SUM, Self::word_unary)
    }

    fn word_unary(&mut self) -> Result<Expr> {
        match self.eat_op(WORD_UNARY) {
            Some(op) => Ok(Expr::Unary(op, Box::new(self.word_unary()?))),
            None => self.vector(),
        }
    }

    /// Can the next token start another element of the vector that is being parsed?
    fn continues_vector(&self) -> bool {
        let token = self.peek();
        if !token.space_before || self.at_open_ended() {
            return false;
        }

        match token.kind {
            TokenKind::Number(_) | TokenKind::Ident(_) | TokenKind::Symbol("(") => true,
            // `1 -2` is a vector, `1 - 2` is not
            TokenKind::Symbol("+" | "-") => !self.peek_at(1).space_before,
            _ => lookup(TIGHT_UNARY, token).is_some(),
        }
    }

    fn vector(&mut self) -> Result<Expr> {
        let mut elements = vec![self.product()?];
        while self.continues_vector() {
            elements.push(self.product()?);
        }

        Ok(if elements.len() == 1 {
            elements.pop().unwrap()
        } else {
            Expr::Vector(elements)
        })
    }

    fn product(&mut self) -> Result<Expr> {
        self.left_assoc(PRODUCT, Self::power)
    }

    fn power(&mut self) -> Result<Expr> {
        self.right_assoc(POWER, Self::prefix)
    }

    fn prefix(&mut self) -> Result<Expr> {
        if self.at_open_ended() {
            return self.open_ended();
        }
        if lookup(WORD_UNARY, self.peek()).is_some() {
            return self.word_unary();
        }
        match self.eat_op(TIGHT_UNARY) {
            Some(op) => Ok(Expr::Unary(op, Box::new(self.prefix()?))),
            None => self.postfix(),
        }
    }

    fn postfix(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        while self.is("[") && !self.peek().space_before {
            self.advance();
            let index = self.expr()?;
            self.expect("]")?;
            expr = Expr::Index(Box::new(expr), Box::new(index));
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        match &self.peek().kind {
            TokenKind::Number(n) => {
                let n = n.clone();
                self.advance();
                Ok(Expr::Atom(Atom::Rat(n)))
            }
            TokenKind::Ident(_) => Ok(Expr::Atom(Atom::Ref(self.ident()?))),
            TokenKind::Symbol("(") => {
                self.advance();
                let expr = self.expr()?;
                self.expect(")")?;
                Ok(expr)
            }
            _ => Err(self.unexpected("an expression")),
        }
    }

    fn statement(&mut self) -> Result<Statement> {
        if self.eat("fn") {
            let name = self.ident()?;
            let mut params = vec![self.ident()?];
            while !self.eat("=") {
                params.push(self.ident().map_err(|_| self.unexpected("a name or `=`"))?);
            }
            let body = self.expr()?;
            return Ok(Statement::FunDeclare(name, params, body));
        }

        if let TokenKind::Ident(name) = &self.peek().kind
            && self.peek_at(1).kind == TokenKind::Symbol("=")
        {
            let name = name.clone();
            self.pos += 2;
            return Ok(Statement::Assign(name, self.expr()?));
        }

        Ok(Statement::Expr(self.expr()?))
    }
}

/// Parse an internal command: `)name rest of the line`.
fn parse_command(source: &str) -> Option<Statement> {
    let rest = source.trim_start().strip_prefix(')')?.trim_start();
    let (name, body) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
    Some(Statement::InternalCommand(
        name.to_string(),
        body.trim().to_string(),
    ))
}

/// Parse a single statement. Returns `Ok(None)` when `source` is empty or only whitespace.
pub fn parse(source: &str) -> Result<Option<Statement>> {
    if let Some(command) = parse_command(source) {
        return Ok(Some(command));
    }

    let tokens = tokenize(source)?;
    if tokens[0].kind == TokenKind::End {
        return Ok(None);
    }

    let mut parser = Parser { tokens, pos: 0 };
    let statement = parser.statement()?;
    if parser.peek().kind != TokenKind::End {
        return Err(parser.unexpected("end of input"));
    }
    Ok(Some(statement))
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

    /// Parse an expression and show it with all implicit parentheses made explicit.
    fn show(s: &str) -> String {
        match parse(s) {
            Ok(Some(Statement::Expr(e))) => e.to_string(),
            r => panic!("{:?} parsed as {:?}", s, r),
        }
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
        assert!(is_ok_some!(parse("0xff")));
    }

    #[test]
    fn test_var() {
        assert!(!is_ok_some!(parse("1kaas")));
        assert!(is_ok_some!(parse("kaas1")));

        assert!(is_ok_some!(parse("kaas1 = a")));
        assert!(!is_ok_some!(parse("iota = a")));
        assert_eq!(show("single"), "single");
        assert_eq!(show("iotax"), "iotax");
    }

    #[test]
    fn test_vec() {
        assert!(is_ok_some!(parse("1 2 3")));
        assert!(is_ok_some!(parse("a b c")));
        assert!(is_ok_some!(parse("a 2 c")));
        assert!(is_ok_some!(parse("a (a b) c")));
        assert!(!is_ok_some!(parse("(1)(2)")));

        assert_eq!(
            parse(".1 .2"),
            Ok(Some(Statement::Expr(Expr::Vector(vec![
                Expr::Atom(Atom::Rat(Ratio::from((1, 10)))),
                Expr::Atom(Atom::Rat(Ratio::from((1, 5))))
            ]))))
        );

        assert_eq!(show("1/2 2/4"), "((1 / 2) (2 / 4))");
        assert_eq!(show("1 -2"), "(1 -2)");
        assert_eq!(show("1 - 2"), "(1 - 2)");
        assert_eq!(show("1-2"), "(1 - 2)");
        assert_eq!(show("1 - -2"), "(1 - -2)");
        assert_eq!(show("1 sin 2"), "(1 sin 2)");
    }

    #[test]
    fn test_fn() {
        assert!(is_ok_some!(parse("fn test a = a")));
        assert!(is_ok_some!(parse("fn test a = a + a")));
        assert!(is_ok_some!(parse("fn test a = +//a")));
        assert!(is_ok_some!(parse("fn test a b = a + b")));
        assert!(!is_ok_some!(parse("fn a = +//a")));
    }

    #[test]
    fn test_binop() {
        assert!(is_ok_some!(parse("a+ b")));
        assert!(is_ok_some!(parse("a+b")));
        assert!(is_ok_some!(parse("a + b")));
        assert!(is_ok_some!(parse("a ** b + c")));

        assert!(is_ok_some!(parse("a << b")));
        assert!(is_ok_some!(parse("a >> b")));

        assert_eq!(show("1 + 2 * 3"), "(1 + (2 * 3))");
        assert_eq!(show("1 - 2 - 3"), "((1 - 2) - 3)");
        assert_eq!(show("2 ** 3 ** 2"), "(2 ** (3 ** 2))");
        assert_eq!(show("-2 ** 2"), "(-2 ** 2)");
        assert_eq!(show("2 ** -1"), "(2 ** -1)");
    }

    #[test]
    fn test_comparison() {
        assert_eq!(show("a <= b"), "(a <= b)");
        assert_eq!(show("a >= b"), "(a >= b)");
        assert_eq!(show("a < b"), "(a < b)");
        assert_eq!(show("a + 1 == b"), "((a + 1) == b)");
        assert_eq!(show("1 2 == 1 3"), "((1 2) == (1 3))");
        assert_eq!(show("n != 0 rho n"), "((n != 0) rho n)");
    }

    #[test]
    fn test_unary() {
        assert_eq!(show("?i 6"), "?i6");
        assert_eq!(show("- a"), "-a");
        assert_eq!(show("iota 5 6"), "iota (5 6)");
        assert_eq!(show("rev iota 5"), "rev iota 5");
        assert_eq!(show("iota 5 + 1"), "(iota 5 + 1)");
        assert_eq!(show("floor x - 1"), "(floor x - 1)");
        assert_eq!(show("rho rho 1 2"), "rho rho (1 2)");
        assert_eq!(show("3 rho 1 2"), "(3 rho (1 2))");
    }

    #[test]
    fn test_issue_5() {
        // unary operators with lower precedence are allowed after a binary operator
        assert_eq!(show("2 ** iota 5"), "(2 ** iota 5)");
        assert_eq!(show("2 * iota 5"), "(2 * iota 5)");
        assert_eq!(show("2 ** iota 5 + 1"), "((2 ** iota 5) + 1)");
        assert_eq!(show("2 ** rho x"), "(2 ** rho x)");
    }

    #[test]
    fn test_issue_6() {
        // special binary operators chain (right associatively)
        assert_eq!(show("1 , (1 2) , 5"), "(1 , ((1 2) , 5))");
        assert_eq!(show("1 , 2 , 3"), "(1 , (2 , 3))");
        assert_eq!(show("a max b min c"), "(a max (b min c))");
    }

    #[test]
    fn test_paren() {
        assert!(is_ok_some!(parse("( a + b )")));
        assert!(is_ok_some!(parse("( a + (b) )")));
        assert!(is_ok_some!(parse("( a + (b * c) )")));
        assert!(is_ok_some!(parse("1 2 (3 4)")));
        assert!(is_ok_some!(parse("(3 4) 2 (3 4)")));
        assert!(is_ok_some!(parse("(1+ 2 )*3")));
        assert!(!is_ok_some!(parse("(1 + 2")));
        assert!(!is_ok_some!(parse("1 + 2)")));

        let deep = format!("{}5{}", "(".repeat(200), ")".repeat(200));
        assert_eq!(show(&deep), "5");
    }

    #[test]
    fn test_index() {
        assert_eq!(show("a[1]"), "a[1]");
        assert_eq!(show("a[ 1 2 ]"), "a[(1 2)]");
        assert_eq!(show("(1 2 3)[2]"), "(1 2 3)[2]");
        assert_eq!(show("a[1][2]"), "a[1][2]");
        assert!(!is_ok_some!(parse("a [1]")));
    }

    #[test]
    fn test_fold() {
        assert_eq!(show("+//a"), "(+//a)");
        assert_eq!(show("+ // a"), "(+//a)");
        assert_eq!(show(r"+\\a"), r"(+\\a)");
        assert_eq!(show("f//a"), "(f//a)");
        assert_eq!(show("*//iota 5"), "(*//iota 5)");
        assert_eq!(show(",//a"), "(,//a)");
        assert_eq!(show("max//a"), "(max//a)");
        assert_eq!(show("1 + +//a"), "(1 + (+//a))");
        assert!(!is_ok_some!(parse("a b . x")));
    }

    #[test]
    fn test_map() {
        assert_eq!(show("f . 1 2 3"), "(f . (1 2 3))");
        assert_eq!(show("f . (1 1 2 2)"), "(f . (1 1 2 2))");
        assert_eq!(show("(f 1) . 1 2"), "((f 1) . (1 2))");
        assert_eq!(show("f .5"), "(f 1/2)");
    }

    #[test]
    fn test_let() {
        assert!(is_ok_some!(parse("let a = 1 in a")));
        assert!(!is_ok_some!(parse("let a in 5")));
        assert!(is_ok_some!(parse("let a = (let b = 6 in b) in a")));
        assert_eq!(show("let a = 1 + 2 in a"), "(let a = (1 + 2) in a)");
        assert_eq!(
            show("let a = 5 in let b = 10 in a + b"),
            "(let a = 5 in (let b = 10 in (a + b)))"
        );
        assert_eq!(show("1 + let a = 2 in a"), "(1 + (let a = 2 in a))");
    }

    #[test]
    fn test_lambda() {
        assert!(is_ok_some!(parse(r"f = (\x -> x*2)")));
        assert!(is_ok_some!(parse(r"f = \x -> x*2")));
        assert!(is_ok_some!(parse(r"f = \a b -> a + b")));
        assert!(is_ok_some!(parse(r"f = \x->x")));

        assert!(is_ok_some!(parse(r"(\x -> x*2) . 1 2 3")));
        assert!(!is_ok_some!(parse(r"(\-> x*2) . (1 2 3)")));
        assert!(!is_ok_some!(parse(r"(\x x*2) . (1 2 3)")));
        assert!(!is_ok_some!(parse(r"(x -> x*2) . (1 2 3)")));
    }

    #[test]
    fn test_statement() {
        assert_eq!(parse(""), Ok(None));
        assert_eq!(parse("   "), Ok(None));
        assert_eq!(
            parse(")f float "),
            Ok(Some(Statement::InternalCommand("f".into(), "float".into())))
        );
        assert_eq!(
            parse(" ) time 1 + 2"),
            Ok(Some(Statement::InternalCommand(
                "time".into(),
                "1 + 2".into()
            )))
        );
        assert_eq!(
            parse(")help"),
            Ok(Some(Statement::InternalCommand("help".into(), "".into())))
        );
        assert!(matches!(
            parse("x = 5"),
            Ok(Some(Statement::Assign(name, _))) if name == "x"
        ));
    }

    #[test]
    fn test_error() {
        assert_eq!(
            parse("1 + )"),
            Err(ParseError::new(4, "expected an expression, found `)`"))
        );
        assert_eq!(
            parse("1 2 3 iota 4"),
            Err(ParseError::new(6, "expected end of input, found `iota`"))
        );
    }
}
