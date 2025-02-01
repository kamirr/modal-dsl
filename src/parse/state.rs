use std::ops::Range;

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
    pub span: Range<usize>,
}

impl StateEntry {
    #[cfg(test)]
    pub fn new(name: Ident, init: Expr, span: Range<usize>) -> Self {
        StateEntry { name, init, span }
    }

    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then_ignore(just("=").padded())
            .then(Expr::parser(sample_rate))
            .map_with_span(|(name, init), span| StateEntry { name, init, span })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct State {
    pub entries: Vec<StateEntry>,
    pub span: Range<usize>,
}

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
            .then_ignore(whitespace())
            .then_ignore(just("}"))
            .map_with_span(|(mut entries, last_entry), span| {
                if let Some(entry) = last_entry {
                    entries.push(entry);
                }

                State { entries, span }
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::literal::{Literal, LiteralValue};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_state_entry() {
        let zero = Expr::Literal(Literal {
            value: LiteralValue::Float(0.0),
            span: 3..6,
        });
        let cases = [("in=0.0", ("in", 0..2, zero))];

        for (text, (name, span, init)) in cases {
            assert_eq!(
                StateEntry::parser(44100.0).parse(text),
                Ok(StateEntry {
                    name: Ident::new(name, span),
                    init,
                    span: 0..6
                })
            );
        }
    }

    #[test]
    fn test_state() {
        let cases = [
            (
                "state{in=0.0}",
                State {
                    entries: vec![StateEntry::new(
                        Ident::new("in", 6..8),
                        Literal {
                            value: LiteralValue::Float(0.0),
                            span: 9..12,
                        }
                        .into(),
                        6..12,
                    )],
                    span: 0..13,
                },
            ),
            (
                "state{in = 0.0,}",
                State {
                    entries: vec![StateEntry::new(
                        Ident::new("in", 6..8),
                        Literal {
                            value: LiteralValue::Float(0.0),
                            span: 11..14,
                        }
                        .into(),
                        6..14,
                    )],
                    span: 0..16,
                },
            ),
            (
                "state{in = 0.0,in2=0.0}",
                State {
                    entries: vec![
                        StateEntry::new(
                            Ident::new("in", 6..8),
                            Literal {
                                value: LiteralValue::Float(0.0),
                                span: 11..14,
                            }
                            .into(),
                            6..14,
                        ),
                        StateEntry::new(
                            Ident::new("in2", 15..18),
                            Literal {
                                value: LiteralValue::Float(0.0),
                                span: 19..22,
                            }
                            .into(),
                            15..22,
                        ),
                    ],
                    span: 0..23,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(State::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
