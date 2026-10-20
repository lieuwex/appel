//! Time literals.
//!
//! For now these are only durations written as clock times: `6:05` is 6 minutes and 5 seconds
//! and `1:02:03` is 1 hour, 2 minutes and 3 seconds. They are lexed as a single token and
//! evaluate to a number of seconds, so `6:05 to min` works like any other unit conversion.
//! Dates and times of day (with time zones) are meant to live here too later.

use rug::{Integer, ops::Pow};

use crate::ast::Ratio;

/// Whether `s` starts with a duration literal: digits directly followed by `:` and a digit.
pub fn starts_duration(s: &[char]) -> bool {
    let digits = s.iter().take_while(|c| c.is_ascii_digit()).count();
    digits > 0 && s.get(digits) == Some(&':') && s.get(digits + 1).is_some_and(char::is_ascii_digit)
}

/// Lex a duration literal `m:ss` or `h:mm:ss` at the start of `s`, returning its length in
/// seconds and its length in characters. The first field may have any number of digits, the
/// others need exactly two and must be below 60. The seconds may have a fractional part, as in
/// `1:02.5`.
pub fn lex_duration(s: &[char]) -> Result<(Ratio, usize), String> {
    let mut fields: Vec<&[char]> = Vec::new();
    let mut i = 0;
    loop {
        let len = s[i..].iter().take_while(|c| c.is_ascii_digit()).count();
        if len == 0 {
            return Err(String::from("expected digits in duration"));
        }
        fields.push(&s[i..i + len]);
        i += len;
        if s.get(i) != Some(&':') {
            break;
        }
        i += 1;
    }

    if fields.len() > 3 {
        return Err(String::from("a duration has at most three fields, as in `h:mm:ss`"));
    }

    let mut frac: &[char] = &[];
    if s.get(i) == Some(&'.') {
        let len = s[i + 1..].iter().take_while(|c| c.is_ascii_digit()).count();
        frac = &s[i + 1..i + 1 + len];
        i += 1 + len;
    }

    if s.get(i).is_some_and(|c| c.is_alphanumeric() || *c == '_' || *c == '.') {
        return Err(format!("unexpected `{}` after duration", s[i]));
    }

    let mut seconds = Ratio::new();
    for (n, field) in fields.iter().enumerate() {
        let value: u64 = field
            .iter()
            .collect::<String>()
            .parse()
            .map_err(|_| String::from("duration too large"))?;
        if n > 0 && (field.len() != 2 || value >= 60) {
            return Err(String::from(
                "minutes and seconds in a duration need two digits and must be below 60",
            ));
        }
        seconds = seconds * 60 + value;
    }

    if !frac.is_empty() {
        let digits: String = frac.iter().collect();
        let numer: Integer = digits.parse().unwrap();
        let denom = Integer::from(10).pow(frac.len() as u32);
        seconds += Ratio::from((numer, denom));
    }

    Ok((seconds, i))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn duration(s: &str) -> Result<Ratio, String> {
        let chars: Vec<char> = s.chars().collect();
        assert!(starts_duration(&chars), "{}", s);
        let (n, len) = lex_duration(&chars)?;
        assert_eq!(len, chars.len(), "{} not fully lexed", s);
        Ok(n)
    }

    #[test]
    fn test_duration() {
        assert_eq!(duration("06:05").unwrap(), 6 * 60 + 5);
        assert_eq!(duration("6:05").unwrap(), 365);
        assert_eq!(duration("0:00").unwrap(), 0);
        assert_eq!(duration("90:00").unwrap(), 5400);
        assert_eq!(duration("1:02:03").unwrap(), 3723);
        assert_eq!(duration("100:00:00").unwrap(), 360000);
        assert_eq!(duration("1:02.5").unwrap(), Ratio::from((125, 2)));
        assert_eq!(duration("1:00:00.25").unwrap(), Ratio::from((14401, 4)));

        assert!(duration("1:2").is_err());
        assert!(duration("1:60").is_err());
        assert!(duration("1:000").is_err());
        assert!(duration("1:60:00").is_err());
        assert!(duration("1:00:00:00").is_err());
        assert!(duration("1:00x").is_err());
        assert!(duration("1:00.5.5").is_err());
        assert!(duration("99999999999999999999:00").is_err());
    }

    #[test]
    fn test_starts_duration() {
        let starts = |s: &str| starts_duration(&s.chars().collect::<Vec<_>>());
        assert!(starts("1:00"));
        assert!(!starts("1"));
        assert!(!starts("1:"));
        assert!(!starts("1.5:00"));
        assert!(!starts(":00"));
    }
}
