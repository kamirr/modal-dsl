use std::ops::Range;

use chumsky::{error::Simple, prelude::just, text::ident, Parser};

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
pub struct Path(pub Vec<Ident>);

impl Path {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then(
                just(".")
                    .ignored()
                    .then(Ident::parser())
                    .map(|((), ident)| ident)
                    .repeated(),
            )
            .map(|(first, mut tail)| {
                tail.insert(0, first);
                tail
            })
            .map(Path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_path() {
        let cases = [
            ("foo", Path(vec![Ident::new("foo", 0..3)])),
            (
                "foo.bar",
                Path(vec![Ident::new("foo", 0..3), Ident::new("bar", 4..7)]),
            ),
            (
                "foo.bar.baz",
                Path(vec![
                    Ident::new("foo", 0..3),
                    Ident::new("bar", 4..7),
                    Ident::new("baz", 8..11),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Path::parser().parse(text), Ok(expected));
        }
    }
}
