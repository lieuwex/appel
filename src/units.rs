//! Physical units.
//!
//! Every value carries a [`Unit`]: its dimension (the exponents of the SI base units) and,
//! optionally, the named unit it should be shown in. Values themselves are always stored in SI
//! base units, so `5km` is stored as `5000` with dimension `m` and display unit `km`.
//!
//! Unit expressions are unit names joined by `.` (multiply) and `/` (divide), each optionally
//! raised to an integer power with `^`: `km/h`, `kg.m^2/s^2`, `s^-1`.

use std::fmt;
use std::rc::Rc;

use rug::{Integer, ops::Pow};

use crate::ast::Ratio;

/// Symbols of the SI base units, in the order of the exponents in [`Dim`].
const BASE_UNITS: [&str; 7] = ["m", "kg", "s", "A", "K", "mol", "cd"];

/// Exponents of the SI base units.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Dim([i8; 7]);

impl Dim {
    pub fn is_dimensionless(self) -> bool {
        self == Dim::default()
    }

    fn zip(self, other: Dim, f: impl Fn(i8, i8) -> Option<i8>) -> Option<Dim> {
        let mut res = Dim::default();
        for i in 0..7 {
            res.0[i] = f(self.0[i], other.0[i])?;
        }
        Some(res)
    }

    fn mul(self, other: Dim) -> Option<Dim> {
        self.zip(other, i8::checked_add)
    }

    fn div(self, other: Dim) -> Option<Dim> {
        self.zip(other, i8::checked_sub)
    }

    fn pow(self, n: i32) -> Option<Dim> {
        let n = i8::try_from(n).ok()?;
        self.zip(Dim::default(), |e, _| e.checked_mul(n))
    }
}

/// One named unit raised to a power, like the `h^-1` in `km/h`.
#[derive(Clone, Debug, PartialEq)]
struct Part {
    name: String,
    /// The size of one of this unit in SI base units.
    size: Ratio,
    exp: i32,
}

/// A unit a value can be shown in: named units with exponents, so `km/h` is `km^1 h^-1`.
#[derive(Clone, Debug, PartialEq)]
pub struct NamedUnit {
    parts: Vec<Part>,
    /// The size of one of this unit in SI base units.
    pub factor: Ratio,
}

impl NamedUnit {
    fn new(parts: Vec<Part>) -> Self {
        let parts: Vec<Part> = parts.into_iter().filter(|p| p.exp != 0).collect();
        let factor = parts
            .iter()
            .map(|p| Ratio::from((&p.size).pow(p.exp)))
            .product();
        Self { parts, factor }
    }

    /// The SI base units of `dim`, e.g. `m/s^2`.
    fn base(dim: Dim) -> Self {
        let parts = BASE_UNITS
            .iter()
            .zip(dim.0)
            .map(|(name, exp)| Part {
                name: name.to_string(),
                size: Ratio::from(1),
                exp: exp.into(),
            })
            .collect();
        Self::new(parts)
    }

    /// Whether this only consists of SI base units.
    fn is_base(&self) -> bool {
        self.parts
            .iter()
            .all(|p| BASE_UNITS.contains(&p.name.as_str()))
    }

    /// Multiply by `other` raised to `sign`, which is 1 or -1. Parts with the same name are
    /// merged, so `km/h * h` is `km`.
    fn mul(&self, other: &NamedUnit, sign: i32) -> Option<NamedUnit> {
        let mut parts = self.parts.clone();
        for part in &other.parts {
            let exp = part.exp.checked_mul(sign)?;
            match parts.iter_mut().find(|p| p.name == part.name) {
                Some(p) => p.exp = p.exp.checked_add(exp)?,
                None => parts.push(Part {
                    exp,
                    ..part.clone()
                }),
            }
        }
        Some(Self::new(parts))
    }

    /// Raise to the power `numer / denom`, if every exponent stays an integer.
    fn pow(&self, numer: i32, denom: i32) -> Option<NamedUnit> {
        let parts = self
            .parts
            .iter()
            .map(|p| {
                let exp = p.exp.checked_mul(numer)?;
                (exp % denom == 0).then(|| Part {
                    exp: exp / denom,
                    ..p.clone()
                })
            })
            .collect::<Option<_>>()?;
        Some(Self::new(parts))
    }
}

impl fmt::Display for NamedUnit {
    /// Format as a unit expression that can be lexed again, e.g. `kg.m^2/s^2`. Every unit with a
    /// negative exponent gets its own `/`, as `/` only divides by the unit directly after it.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_part(f: &mut fmt::Formatter<'_>, name: &str, exp: i32) -> fmt::Result {
            if exp == 1 {
                write!(f, "{}", name)
            } else {
                write!(f, "{}^{}", name, exp)
            }
        }

        let has_positive = self.parts.iter().any(|p| p.exp > 0);

        let mut first = true;
        for p in self.parts.iter().filter(|p| p.exp > 0) {
            if !first {
                write!(f, ".")?;
            }
            first = false;
            write_part(f, &p.name, p.exp)?;
        }

        for p in self.parts.iter().filter(|p| p.exp < 0) {
            if has_positive {
                write!(f, "/")?;
                write_part(f, &p.name, -p.exp)?;
            } else {
                if !first {
                    write!(f, ".")?;
                }
                first = false;
                write_part(f, &p.name, p.exp)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Unit {
    pub dim: Dim,
    /// The unit to show values in, if not SI base units. Always `None` for dimensionless values.
    pub display: Option<Rc<NamedUnit>>,
}

type Error = std::borrow::Cow<'static, str>;

fn too_large() -> Error {
    Error::from("unit exponent too large")
}

impl Unit {
    /// The unit of a plain number.
    pub fn none() -> Self {
        Self::default()
    }

    pub fn is_none(&self) -> bool {
        self.dim.is_dimensionless()
    }

    #[cfg(test)]
    fn from_dim(dim: Dim) -> Self {
        Self { dim, display: None }
    }

    /// The unit of a computed value. A display unit consisting of only SI base units is dropped,
    /// so that in a sum the other side's display unit is used.
    fn computed(dim: Dim, display: Option<NamedUnit>) -> Self {
        let display = display
            .filter(|d| !dim.is_dimensionless() && !d.is_base())
            .map(Rc::new);
        Self { dim, display }
    }

    /// The unit values are shown in, SI base units if there is no display unit.
    fn display_or_base(&self) -> NamedUnit {
        match &self.display {
            Some(named) => (**named).clone(),
            None => NamedUnit::base(self.dim),
        }
    }

    /// The size of one display unit in SI base units.
    pub fn factor(&self) -> Ratio {
        match &self.display {
            Some(named) => named.factor.clone(),
            None => Ratio::from(1),
        }
    }

    /// The unit for use in error messages.
    pub fn describe(&self) -> String {
        if self.is_none() {
            String::from("a plain number")
        } else {
            self.to_string()
        }
    }

    /// Attach this unit to `count` formatted values separated by spaces, such that the result
    /// can be parsed again: `5km`, `(3/2)km` or `(1 2 3)km`.
    pub fn attach(&self, values: &str, count: usize) -> String {
        if self.is_none() {
            values.to_string()
        } else if count == 1 && !values.contains('/') {
            format!("{}{}", values, self)
        } else {
            format!("({}){}", values, self)
        }
    }

    pub fn expect_none(&self) -> Result<(), Error> {
        if self.is_none() {
            Ok(())
        } else {
            Err(format!("expected a plain number, got a value in {}", self).into())
        }
    }

    /// The unit of the result of adding values in `self` and `other`, which must have the same
    /// dimension.
    pub fn combine(&self, other: &Unit) -> Result<Unit, Error> {
        if self.dim != other.dim {
            return Err(format!("unit mismatch: {} and {}", self.describe(), other.describe()).into());
        }

        Ok(Unit {
            dim: self.dim,
            display: self.display.clone().or_else(|| other.display.clone()),
        })
    }

    pub fn mul(&self, other: &Unit) -> Result<Unit, Error> {
        if other.is_none() {
            return Ok(self.clone());
        }
        if self.is_none() {
            return Ok(other.clone());
        }
        let dim = self.dim.mul(other.dim).ok_or_else(too_large)?;
        let display = self.display_or_base().mul(&other.display_or_base(), 1);
        Ok(Unit::computed(dim, display))
    }

    pub fn div(&self, other: &Unit) -> Result<Unit, Error> {
        if other.is_none() {
            return Ok(self.clone());
        }
        let dim = self.dim.div(other.dim).ok_or_else(too_large)?;
        let display = self.display_or_base().mul(&other.display_or_base(), -1);
        Ok(Unit::computed(dim, display))
    }

    /// The unit of a value in this unit raised to the power `exp`. Fractional powers are allowed
    /// as long as the result has integer exponents, like the square root of an area. The
    /// display unit is kept if its exponents stay integers too, so `(2km) ** 2` is in `km^2`.
    pub fn pow(&self, exp: &Ratio) -> Result<Unit, Error> {
        if self.is_none() {
            return Ok(Unit::none());
        }

        let numer = exp.numer().to_i32().ok_or_else(too_large)?;
        let denom = exp.denom().to_i32().ok_or_else(too_large)?;

        let mut dim = self.dim.pow(numer).ok_or_else(too_large)?;
        for e in dim.0.iter_mut() {
            if i32::from(*e) % denom != 0 {
                return Err(format!("can't raise a value in {} to the power {}", self, exp).into());
            }
            *e = (i32::from(*e) / denom) as i8;
        }

        let display = self.display_or_base().pow(numer, denom);
        Ok(Unit::computed(dim, display))
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.display {
            Some(named) => write!(f, "{}", named),
            None => write!(f, "{}", NamedUnit::base(self.dim)),
        }
    }
}

const fn dim(m: i8, kg: i8, s: i8, a: i8, k: i8) -> Dim {
    Dim([m, kg, s, a, k, 0, 0])
}

const LENGTH: Dim = dim(1, 0, 0, 0, 0);
const MASS: Dim = dim(0, 1, 0, 0, 0);
const TIME: Dim = dim(0, 0, 1, 0, 0);
const ENERGY: Dim = dim(2, 1, -2, 0, 0);
const PRESSURE: Dim = dim(-1, 1, -2, 0, 0);
const VOLUME: Dim = dim(3, 0, 0, 0, 0);

/// Name, dimension, size in SI base units as numerator and denominator, and whether SI prefixes
/// may be used.
const UNITS: &[(&str, Dim, i128, i128, bool)] = &[
    // SI base units, with the gram instead of the kilogram so that prefixes work
    ("m", LENGTH, 1, 1, true),
    ("g", MASS, 1, 1000, true),
    ("s", TIME, 1, 1, true),
    ("A", dim(0, 0, 0, 1, 0), 1, 1, true),
    ("K", dim(0, 0, 0, 0, 1), 1, 1, true),
    ("mol", Dim([0, 0, 0, 0, 0, 1, 0]), 1, 1, true),
    ("cd", Dim([0, 0, 0, 0, 0, 0, 1]), 1, 1, true),
    // derived SI units
    ("Hz", dim(0, 0, -1, 0, 0), 1, 1, true),
    ("N", dim(1, 1, -2, 0, 0), 1, 1, true),
    ("Pa", PRESSURE, 1, 1, true),
    ("J", ENERGY, 1, 1, true),
    ("W", dim(2, 1, -3, 0, 0), 1, 1, true),
    ("C", dim(0, 0, 1, 1, 0), 1, 1, true),
    ("V", dim(2, 1, -3, -1, 0), 1, 1, true),
    ("Ohm", dim(2, 1, -3, -2, 0), 1, 1, true),
    // time
    ("min", TIME, 60, 1, false),
    ("h", TIME, 3600, 1, false),
    ("d", TIME, 86400, 1, false),
    ("wk", TIME, 604800, 1, false),
    ("yr", TIME, 31557600, 1, false), // Julian year
    // other metric units
    ("L", VOLUME, 1, 1000, true),
    ("l", VOLUME, 1, 1000, true),
    ("t", MASS, 1000, 1, false),
    ("bar", PRESSURE, 100000, 1, true),
    ("Wh", ENERGY, 3600, 1, true),
    ("cal", ENERGY, 4184, 1000, true),
    ("eV", ENERGY, 1602176634, 10000000000000000000000000000, true),
    ("atm", PRESSURE, 101325, 1, false),
    // imperial units
    ("in", LENGTH, 254, 10000, false),
    ("ft", LENGTH, 3048, 10000, false),
    ("yd", LENGTH, 9144, 10000, false),
    ("mi", LENGTH, 1609344, 1000, false),
    ("lb", MASS, 45359237, 100000000, false),
    ("oz", MASS, 45359237, 1600000000, false),
];

/// SI prefixes and their power of ten. Hecto and deca are left out because `h` and `d` are
/// hours and days.
const PREFIXES: &[(&str, i32)] = &[
    ("p", -12),
    ("n", -9),
    ("u", -6),
    ("µ", -6),
    ("μ", -6),
    ("m", -3),
    ("c", -2),
    ("k", 3),
    ("M", 6),
    ("G", 9),
    ("T", 12),
];

/// Look up a single unit name, possibly with an SI prefix.
fn lookup(word: &str) -> Option<(Dim, Ratio)> {
    let size = |numer: i128, denom: i128| Ratio::from((Integer::from(numer), Integer::from(denom)));

    if let Some(&(_, dim, numer, denom, _)) = UNITS.iter().find(|u| u.0 == word) {
        return Some((dim, size(numer, denom)));
    }

    PREFIXES.iter().find_map(|&(prefix, power)| {
        let rest = word.strip_prefix(prefix)?;
        let &(_, dim, numer, denom, _) = UNITS.iter().find(|u| u.0 == rest && u.4)?;
        let scale = Ratio::from(10).pow(power);
        Some((dim, size(numer, denom) * scale))
    })
}

fn word_len(s: &[char]) -> usize {
    s.iter().take_while(|c| c.is_alphabetic()).count()
}

/// Lex a unit expression at the start of `s`, returning it and its length in characters.
///
/// Returns `None` if `s` doesn't start with a known unit. The expression only continues after a
/// `.` or `/` if a known unit follows, so in `5m/x` only `m` is part of the unit.
pub fn lex_unit(s: &[char]) -> Option<(Unit, usize)> {
    let mut dim = Dim::default();
    let mut parts: Vec<Part> = Vec::new();
    let mut divide = false;
    let mut i = 0;

    loop {
        let len = word_len(&s[i..]);
        let word: String = s[i..i + len].iter().collect();
        let (mut d, size) = lookup(&word)?;
        let mut exp = 1;
        i += len;

        if s.get(i) == Some(&'^') {
            let neg = s.get(i + 1) == Some(&'-');
            let digits_start = (i + 1 + neg as usize).min(s.len());
            let digits = s[digits_start..]
                .iter()
                .take_while(|c| c.is_ascii_digit())
                .count();
            let exp_digits: Option<i32> = s[digits_start..digits_start + digits]
                .iter()
                .collect::<String>()
                .parse()
                .ok();

            // without digits, `^` isn't part of the unit
            if let Some(e) = exp_digits {
                exp = if neg { -e } else { e };
                d = d.pow(exp)?;
                i = digits_start + digits;
            }
        }

        if divide {
            dim = dim.div(d)?;
            exp = -exp;
        } else {
            dim = dim.mul(d)?;
        }

        match parts.iter_mut().find(|p| p.name == word) {
            Some(p) => p.exp = p.exp.checked_add(exp)?,
            None => parts.push(Part {
                name: word,
                size,
                exp,
            }),
        }

        let continues = matches!(s.get(i), Some('.' | '/')) && {
            let len = word_len(&s[i + 1..]);
            let word: String = s[i + 1..i + 1 + len].iter().collect();
            lookup(&word).is_some()
        };
        if !continues {
            break;
        }

        divide = s[i] == '/';
        i += 1;
    }

    let display = (!dim.is_dimensionless()).then(|| Rc::new(NamedUnit::new(parts)));
    Some((Unit { dim, display }, i))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(s: &str) -> Unit {
        let chars: Vec<char> = s.chars().collect();
        let (unit, len) = lex_unit(&chars).unwrap();
        assert_eq!(len, chars.len(), "{} not fully lexed", s);
        unit
    }

    #[test]
    fn test_lookup() {
        assert_eq!(unit("km").factor(), 1000);
        assert_eq!(unit("g").factor(), Ratio::from((1, 1000)));
        assert_eq!(unit("kg").factor(), 1);
        assert_eq!(unit("min").factor(), 60);
        assert_eq!(unit("mm").factor(), Ratio::from((1, 1000)));
        assert_eq!(unit("kWh").factor(), 3600000);
        assert_eq!(unit("cd").dim, Dim([0, 0, 0, 0, 0, 0, 1]));
        assert!(lookup("kmin").is_none());
        assert!(lookup("x").is_none());
    }

    #[test]
    fn test_expressions() {
        assert_eq!(unit("km/h").factor(), Ratio::from((5, 18)));
        assert_eq!(unit("km/h").dim, dim(1, 0, -1, 0, 0));
        assert_eq!(unit("kg.m^2/s^2").dim, ENERGY);
        assert_eq!(unit("s^-1").dim, dim(0, 0, -1, 0, 0));
        assert_eq!(unit("cm^3").factor(), Ratio::from((1, 1000000)));
        assert!(unit("m/m").display.is_none());

        let chars: Vec<char> = "m/x".chars().collect();
        assert_eq!(lex_unit(&chars).unwrap().1, 1);
        let chars: Vec<char> = "m^".chars().collect();
        assert_eq!(lex_unit(&chars).unwrap().1, 1);
    }

    #[test]
    fn test_display() {
        assert_eq!(Unit::from_dim(ENERGY).to_string(), "m^2.kg/s^2");
        assert_eq!(Unit::from_dim(dim(0, 0, -1, 0, 0)).to_string(), "s^-1");
        assert_eq!(Unit::from_dim(LENGTH).to_string(), "m");
        assert_eq!(unit("km/h").to_string(), "km/h");
        assert_eq!(unit("m.m").to_string(), "m^2");
        assert_eq!(unit("m/s/A").to_string(), "m/s/A");
        assert_eq!(unit("s^-1.A^-2").to_string(), "s^-1.A^-2");
        assert_eq!(unit("km.m/km").to_string(), "m");
    }

    #[test]
    fn test_pow() {
        let area = Unit::from_dim(dim(2, 0, 0, 0, 0));
        assert_eq!(area.pow(&Ratio::from((1, 2))).unwrap().dim, LENGTH);
        assert!(Unit::from_dim(LENGTH).pow(&Ratio::from((1, 2))).is_err());
    }
}
