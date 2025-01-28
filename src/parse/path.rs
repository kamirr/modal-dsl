use chumsky::{error::Simple, prelude::just, text::ident, Parser};

use super::kwords;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident(pub String);

impl Ident {
    #[cfg(test)]
    pub fn new(ident: impl Into<String>) -> Self {
        Ident(ident.into())
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        ident().validate(|id: String, span, emit| {
            if kwords::ALL.contains(&id.as_str()) {
                emit(Simple::custom(span, format!("{id} is a keyword")))
            }

            Ident(id.to_string())
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

    #[test]
    fn test_path() {
        let cases = [
            ("foo", Path(vec![Ident::new("foo")])),
            ("foo.bar", Path(vec![Ident::new("foo"), Ident::new("bar")])),
            (
                "foo.bar.baz",
                Path(vec![
                    Ident::new("foo"),
                    Ident::new("bar"),
                    Ident::new("baz"),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Path::parser().parse(text), Ok(expected));
        }
    }
}
