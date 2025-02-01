use std::ops::Range;

use chumsky::{error::Simple, prelude::just, text::whitespace, Parser};

use super::{literal::Literal, path::Ident};

#[derive(Clone, Debug, PartialEq)]
pub struct InputEntry {
    pub name: Ident,
    pub ty: Ident,
    pub default: Option<Literal>,
    pub span: Range<usize>,
}

impl InputEntry {
    #[cfg(test)]
    pub fn new(name: Ident, ty: Ident, default: Option<Literal>, span: Range<usize>) -> Self {
        InputEntry {
            name,
            ty,
            default,
            span,
        }
    }

    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then_ignore(whitespace())
            .then_ignore(just(":"))
            .then_ignore(whitespace())
            .then(Ident::parser())
            .then(
                whitespace()
                    .ignored()
                    .then_ignore(just("="))
                    .then_ignore(whitespace())
                    .then(Literal::parser(sample_rate))
                    .map(|((), value)| value)
                    .or_not(),
            )
            .map_with_span(|((name, ty), default), span| (name, ty, default, span))
            .map(|(name, ty, default, span)| InputEntry {
                name,
                ty,
                default,
                span,
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Inputs {
    pub entries: Vec<InputEntry>,
    pub span: Range<usize>,
}

impl Inputs {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        just("inputs")
            .ignored()
            .then_ignore(whitespace())
            .then_ignore(just("{"))
            .then_ignore(whitespace())
            .then(
                InputEntry::parser(sample_rate)
                    .then_ignore(whitespace())
                    .then(just(","))
                    .then_ignore(whitespace())
                    .map(|(entry, _)| entry)
                    .repeated(),
            )
            .map(|((), entries)| entries)
            .then(InputEntry::parser(sample_rate).or_not())
            .then_ignore(whitespace())
            .then_ignore(just("}"))
            .map_with_span(|(mut entries, last_entry), span| {
                if let Some(entry) = last_entry {
                    entries.push(entry);
                }

                Inputs { entries, span }
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::literal::LiteralValue;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_input_entry() {
        let cases = [
            (
                "foo:sig",
                InputEntry::new(Ident::new("foo", 0..3), Ident::new("sig", 4..7), None, 0..7),
            ),
            (
                "foo: percentage=3%",
                InputEntry::new(
                    Ident::new("foo", 0..3),
                    Ident::new("percentage", 5..15),
                    Some(Literal {
                        value: LiteralValue::Float(0.03),
                        span: 16..18,
                    }),
                    0..18,
                ),
            ),
            (
                "delay: time = 24ms",
                InputEntry::new(
                    Ident::new("delay", 0..5),
                    Ident::new("time", 7..11),
                    Some(Literal {
                        value: LiteralValue::Float(24.0 * 44100.0 / 1000.0),
                        span: 14..18,
                    }),
                    0..18,
                ),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(InputEntry::parser(44100.0).parse(text), Ok(expected));
        }
    }

    #[test]
    fn test_inputs() {
        let cases = [
            (
                "inputs{x:sig}",
                Inputs {
                    entries: vec![InputEntry::new(
                        Ident::new("x", 7..8),
                        Ident::new("sig", 9..12),
                        None,
                        7..12,
                    )],
                    span: 0..13,
                },
            ),
            (
                "inputs{\ns:sig,\n feedback:percentage = 20% ,\ndelay : time = 20ms,}",
                Inputs {
                    entries: vec![
                        InputEntry::new(
                            Ident::new("s", 8..9),
                            Ident::new("sig", 10..13),
                            None,
                            8..13,
                        ),
                        InputEntry::new(
                            Ident::new("feedback", 16..24),
                            Ident::new("percentage", 25..35),
                            Some(Literal {
                                value: LiteralValue::Float(0.2),
                                span: 38..41,
                            }),
                            16..41,
                        ),
                        InputEntry::new(
                            Ident::new("delay", 44..49),
                            Ident::new("time", 52..56),
                            Some(Literal {
                                value: LiteralValue::Float(882.0),
                                span: 59..63,
                            }),
                            44..63,
                        ),
                    ],
                    span: 0..65,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Inputs::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
