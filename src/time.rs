//! Time literals.
//!
//! For now these are only durations written as clock times: `6:05` is 6 minutes and 5 seconds
//! and `1:02:03` is 1 hour, 2 minutes and 3 seconds. They are lexed as a single token and
//! evaluate to a number of seconds, so `6:05 to min` works like any other unit conversion.
//! Dates and times of day (with time zones) are meant to live here too later.
//!
//! A [`Clock`] is a way to show a value in seconds as a clock time. It is part of the display
//! unit, so a duration literal is shown as a clock time again, and `x to m:ss` converts to one.

use std::fmt;

use rug::{Integer, ops::Pow};

use crate::ast::Ratio;
use crate::executor::matrix::Formatter;
use crate::units::Unit;

/// How to show a duration as a clock time.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Clock {
    /// `m:ss`, or `h:mm:ss` from an hour on.
    MinSec,
    /// Always `h:mm:ss`.
    HourMinSec,
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Clock::MinSec => write!(f, "m:ss"),
            Clock::HourMinSec => write!(f, "h:mm:ss"),
        }
    }
}

/// Lex a clock format after `to` at the start of `s`: `m:ss`, `mm:ss`, `h:mm:ss` or `hh:mm:ss`.
pub fn lex_clock(s: &[char]) -> Option<(Clock, usize)> {
    let formats = [
        ("hh:mm:ss", Clock::HourMinSec),
        ("h:mm:ss", Clock::HourMinSec),
        ("mm:ss", Clock::MinSec),
        ("m:ss", Clock::MinSec),
    ];
    formats.into_iter().find_map(|(text, clock)| {
        let len = text.len();
        let matches = s.iter().copied().take(len).eq(text.chars())
            && !s.get(len).is_some_and(|c| c.is_alphanumeric() || *c == '_');
        matches.then_some((clock, len))
    })
}

/// Format `seconds` as a clock time, such that it can be parsed again: `6:05`, `1:02:03` or
/// `-1:02.5`. With [`Formatter::Ratio`] values whose seconds have no exact decimal form, like
/// 62⅓ seconds, are shown in seconds instead: `(187/3)s`. The float formatters round them.
pub fn format_clock(clock: Clock, seconds: &Ratio, fmt: Formatter) -> String {
    let Some((whole, frac)) = split_seconds(&Ratio::from(seconds.abs_ref()), fmt) else {
        return Unit::seconds().attach(&fmt.apply(seconds), 1);
    };

    let (minutes, secs) = whole.div_rem(Integer::from(60));
    let (hours, minutes) = minutes.div_rem(Integer::from(60));

    let sign = if *seconds < 0 { "-" } else { "" };
    let frac = if frac.is_empty() { frac } else { format!(".{}", frac) };
    if clock == Clock::HourMinSec || hours != 0 {
        format!("{sign}{hours}:{minutes:0>2}:{secs:0>2}{frac}")
    } else {
        format!("{sign}{minutes}:{secs:0>2}{frac}")
    }
}

/// Split a non-negative number of seconds into whole seconds and the digits after the decimal
/// point, rounded as `fmt` says. `None` if the digits don't end and `fmt` doesn't round.
fn split_seconds(seconds: &Ratio, fmt: Formatter) -> Option<(Integer, String)> {
    let whole = Integer::from(seconds.floor_ref());
    let frac = Ratio::from(seconds - &whole);

    match fmt {
        _ if frac == 0 => Some((whole, String::new())),
        Formatter::Ratio => {
            // a fraction has a finite decimal form if its denominator only has factors 2 and 5
            let mut denom = frac.denom().clone();
            let twos = denom.remove_factor_mut(&Integer::from(2));
            let fives = denom.remove_factor_mut(&Integer::from(5));
            if denom != 1 {
                return None;
            }
            let digits = twos.max(fives);
            let scaled = Ratio::from(&frac * Integer::from(10).pow(digits));
            Some((whole, format!("{:0>1$}", scaled.numer(), digits as usize)))
        }
        Formatter::Float(Some(precision)) => {
            // round the whole value, so that 59.999 seconds becomes a full minute
            let scale = Integer::from(10).pow(precision as u32);
            let rounded = Integer::from(Ratio::from(seconds * &scale).round_ref());
            let (whole, frac) = rounded.div_rem(scale);
            let frac = if precision == 0 { String::new() } else { format!("{:0>1$}", frac, precision) };
            Some((whole, frac))
        }
        Formatter::Float(None) => {
            let frac = frac.to_f64();
            if frac >= 1.0 {
                return Some((whole + 1, String::new()));
            }
            let digits = frac.to_string();
            let digits = digits.strip_prefix("0.").unwrap_or("");
            Some((whole, digits.to_string()))
        }
    }
}

/// Whether `s` starts with a duration literal: digits directly followed by `:` and a digit.
pub fn starts_duration(s: &[char]) -> bool {
    let digits = s.iter().take_while(|c| c.is_ascii_digit()).count();
    digits > 0 && s.get(digits) == Some(&':') && s.get(digits + 1).is_some_and(char::is_ascii_digit)
}

/// Lex a duration literal `m:ss` or `h:mm:ss` at the start of `s`, returning its length in
/// seconds, the clock format it was written in and its length in characters. The first field may have any number of digits, the
/// others need exactly two and must be below 60. The seconds may have a fractional part, as in
/// `1:02.5`.
pub fn lex_duration(s: &[char]) -> Result<(Ratio, Clock, usize), String> {
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

    let clock = if fields.len() == 3 { Clock::HourMinSec } else { Clock::MinSec };
    Ok((seconds, clock, i))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn duration(s: &str) -> Result<Ratio, String> {
        let chars: Vec<char> = s.chars().collect();
        assert!(starts_duration(&chars), "{}", s);
        let (n, _, len) = lex_duration(&chars)?;
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
    fn test_format_clock() {
        let ratio = |clock, n: Ratio| format_clock(clock, &n, Formatter::Ratio);
        assert_eq!(ratio(Clock::MinSec, Ratio::from(365)), "6:05");
        assert_eq!(ratio(Clock::MinSec, Ratio::from(0)), "0:00");
        assert_eq!(ratio(Clock::MinSec, Ratio::from(3599)), "59:59");
        assert_eq!(ratio(Clock::MinSec, Ratio::from(3600)), "1:00:00");
        assert_eq!(ratio(Clock::MinSec, Ratio::from(360000)), "100:00:00");
        assert_eq!(ratio(Clock::HourMinSec, Ratio::from(365)), "0:06:05");
        assert_eq!(ratio(Clock::MinSec, Ratio::from(-90)), "-1:30");
        assert_eq!(ratio(Clock::MinSec, Ratio::from((125, 2))), "1:02.5");
        assert_eq!(ratio(Clock::MinSec, Ratio::from((-1, 100))), "-0:00.01");
        assert_eq!(ratio(Clock::MinSec, Ratio::from((187, 3))), "(187/3)s");

        let float = |n: Ratio, precision| format_clock(Clock::MinSec, &n, Formatter::Float(precision));
        assert_eq!(float(Ratio::from((187, 3)), Some(2)), "1:02.33");
        assert_eq!(float(Ratio::from((187, 3)), Some(0)), "1:02");
        assert_eq!(float(Ratio::from((59999, 1000)), Some(2)), "1:00.00");
        assert_eq!(float(Ratio::from((125, 2)), None), "1:02.5");
        assert!(float(Ratio::from((187, 3)), None).starts_with("1:02.333"));
    }

    #[test]
    fn test_lex_clock() {
        let lex = |s: &str| lex_clock(&s.chars().collect::<Vec<_>>());
        assert_eq!(lex("m:ss"), Some((Clock::MinSec, 4)));
        assert_eq!(lex("mm:ss"), Some((Clock::MinSec, 5)));
        assert_eq!(lex("h:mm:ss + 1"), Some((Clock::HourMinSec, 7)));
        assert_eq!(lex("m"), None);
        assert_eq!(lex("m:ssx"), None);
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
