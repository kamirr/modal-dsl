use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{choice, just},
    text::{digits, ident},
    Parser,
};

use super::kwords;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Range<usize>,
}

impl Ident {
    #[cfg(test)]
    pub fn new(ident: impl Into<String>, span: Range<usize>) -> Self {
        Ident {
            name: ident.into(),
            span,
        }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        ident().validate(|id: String, span: Range<usize>, emit| {
            if kwords::ALL.contains(&id.as_str()) {
                emit(Simple::custom(span.clone(), format!("{id} is a keyword")))
            }

            Ident {
                name: id.to_string(),
                span,
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Number {
    pub n: u32,
    pub span: Range<usize>,
}

impl Number {
    #[cfg(test)]
    pub fn new(n: u32, span: Range<usize>) -> Self {
        Number { n, span }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        digits(10)
            .map(|s: String| s.parse::<u32>().unwrap())
            .map_with_span(|n, span| Number { n, span })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Segment {
    Member(Ident),
    Index(Number),
}

impl Segment {
    pub fn span(&self) -> Range<usize> {
        match self {
            Segment::Member(ident) => ident.span.clone(),
            Segment::Index(number) => number.span.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path {
    pub base: Ident,
    pub tail: Vec<Segment>,
    pub span: Range<usize>,
}

impl Path {
    #[cfg(test)]
    pub fn new_single(base: Ident) -> Self {
        let span = base.span.clone();
        Path {
            base,
            tail: vec![],
            span,
        }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then(
                just(".")
                    .ignored()
                    .then(choice((
                        Ident::parser().map(Segment::Member),
                        Number::parser().map(Segment::Index),
                    )))
                    .map(|((), member)| member)
                    .repeated(),
            )
            .map_with_span(|(base, tail), span| Path { base, tail, span })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_path() {
        let cases = [
            (
                "foo",
                Path {
                    base: Ident::new("foo", 0..3),
                    tail: vec![],
                    span: 0..3,
                },
            ),
            (
                "foo.bar",
                Path {
                    base: Ident::new("foo", 0..3),
                    tail: vec![Segment::Member(Ident::new("bar", 4..7))],
                    span: 0..7,
                },
            ),
            (
                "foo.123.baz",
                Path {
                    base: Ident::new("foo", 0..3),
                    tail: vec![
                        Segment::Index(Number::new(123, 4..7)),
                        Segment::Member(Ident::new("baz", 8..11)),
                    ],
                    span: 0..11,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Path::parser().parse(text), Ok(expected));
        }
    }
}
