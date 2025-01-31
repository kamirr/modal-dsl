use chumsky::{
    error::Simple,
    prelude::just,
    text::{whitespace, TextParser},
    Parser,
};

use super::{expr::Expr, path::Ident};

#[derive(Clone, Debug, PartialEq)]
pub struct StateEntry {
    pub name: Ident,
    pub init: Expr,
}

impl StateEntry {
    #[cfg(test)]
    pub fn new(name: impl Into<String>, init: Expr) -> Self {
        StateEntry {
            name: Ident::new(name),
            init,
        }
    }

    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then_ignore(just("=").padded())
            .then(Expr::parser(sample_rate))
            .map(|(name, init)| StateEntry { name, init })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct State(pub Vec<StateEntry>);

impl State {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        just("state")
            .ignored()
            .then_ignore(whitespace())
            .then_ignore(just("{"))
            .then_ignore(whitespace())
            .then(
                StateEntry::parser(sample_rate)
                    .then_ignore(whitespace())
                    .then(just(","))
                    .then_ignore(whitespace())
                    .map(|(entry, _)| entry)
                    .repeated(),
            )
            .map(|((), entries)| entries)
            .then(StateEntry::parser(sample_rate).or_not())
            .map(|(mut entries, last_entry)| {
                if let Some(entry) = last_entry {
                    entries.push(entry);
                }

                State(entries)
            })
            .then_ignore(whitespace())
            .then_ignore(just("}"))
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::literal::Literal;

    use super::*;

    #[test]
    fn test_state_entry() {
        let zero = Expr::Literal(Literal::Float(0.0));
        let cases = [("in=0.0", ("in", zero))];

        for (text, (name, init)) in cases {
            assert_eq!(
                StateEntry::parser(44100.0).parse(text),
                Ok(StateEntry {
                    name: Ident::new(name),

                    init
                })
            );
        }
    }

    #[test]
    fn test_state() {
        let zero = Expr::Literal(Literal::Float(0.0));
        let cases = [
            (
                "state{in=0.0}",
                State(vec![StateEntry::new("in", zero.clone())]),
            ),
            (
                "state{in = 0.0,}",
                State(vec![StateEntry::new("in", zero.clone())]),
            ),
            (
                "state{in = 0.0,in2=0.0}",
                State(vec![
                    StateEntry::new("in", zero.clone()),
                    StateEntry::new("in2", zero.clone()),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(State::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
