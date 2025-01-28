use chumsky::{
    error::Simple,
    prelude::{choice, just, one_of},
    text, Parser,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Unit {
    Pct,
    Ms,
}

impl Unit {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        choice((just("%").to(Unit::Pct), just("ms").to(Unit::Ms)))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Float(f32),
}

impl Literal {
    fn float_parser(sample_rate: f32) -> impl Parser<char, f32, Error = Simple<char>> {
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

        let num = just('-')
            .or_not()
            .then(text::int(10))
            .then(frac.or_not())
            .then(exp.or_not())
            .boxed()
            .map(|(((sgn, digits), frac), exp)| {
                let mut text = String::new();
                if let Some(sgn) = sgn {
                    text.push(sgn);
                }
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
                Some(Unit::Ms) => v * sample_rate / 1000.0,
                None => v,
            })
    }

    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Self::float_parser(sample_rate).map(Literal::Float)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal() {
        let cases = [
            ("2", 2.0),
            ("1e1", 10.0),
            ("2.0", 2.0),
            ("20e-1", 2.0),
            ("200%", 2.0),
            ("10ms", 441.0),
        ];

        for (text, expected) in cases {
            assert_eq!(
                Literal::parser(44100.0).parse(text),
                Ok(Literal::Float(expected))
            )
        }
    }
}
