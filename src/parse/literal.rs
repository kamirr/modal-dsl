use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{choice, just, one_of},
    text, Parser,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Unit {
    Pct,
}

impl Unit {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        just("%").to(Unit::Pct)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralValue {
    Float(f32),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
    pub value: LiteralValue,
    pub span: Range<usize>,
}

impl Literal {
    fn float_parser() -> impl Parser<char, f32, Error = Simple<char>> {
        let digits = text::digits(10);

        let frac = just('.')
            .then(digits)
            .map(|(c, digits)| format!("{c}{digits}"));

        let exp = just('e')
            .or(just('E'))
            .then(one_of("+-").or_not())
            .then(digits)
            .map(|((e, sgn), digits)| {
                if let Some(sgn) = sgn {
                    format!("{e}{sgn}{digits}")
                } else {
                    format!("{e}{digits}")
                }
            });

        let num = text::int(10)
            .then(frac.or_not())
            .then(exp.or_not())
            .boxed()
            .map(|((digits, frac), exp)| {
                let mut text = String::new();
                text.push_str(digits.as_str());
                if let Some(frac) = frac {
                    text.push_str(frac.as_str());
                }
                if let Some(exp) = exp {
                    text.push_str(exp.as_str());
                }

                text
            })
            .map(|s| s.parse::<f32>().unwrap());

        num.then(Unit::parser().or_not())
            .map(move |(v, unit)| match unit {
                Some(Unit::Pct) => v / 100.0,
                None => v,
            })
    }

    fn bool_parser() -> impl Parser<char, bool, Error = Simple<char>> {
        choice((just("true").to(true), just("false").to(false)))
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        choice((
            Self::float_parser().map(LiteralValue::Float),
            Self::bool_parser().map(LiteralValue::Bool),
        ))
        .map_with_span(|value, span| Literal { value, span })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_float() {
        let cases = [
            ("2", 2.0, 0..1),
            ("1e1", 10.0, 0..3),
            ("2.0", 2.0, 0..3),
            ("20e-1", 2.0, 0..5),
            ("200%", 2.0, 0..4),
        ];

        for (text, expected_v, expected_span) in cases {
            assert_eq!(
                Literal::parser().parse(text),
                Ok(Literal {
                    value: LiteralValue::Float(expected_v),
                    span: expected_span
                })
            )
        }
    }

    #[test]
    fn test_bool() {
        let cases = [("true", true, 0..4), ("false", false, 0..5)];

        for (text, expected_v, expected_span) in cases {
            assert_eq!(
                Literal::parser().parse(text),
                Ok(Literal {
                    value: LiteralValue::Bool(expected_v),
                    span: expected_span
                })
            )
        }
    }
}
