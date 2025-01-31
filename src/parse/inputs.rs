use chumsky::{
    error::Simple,
    prelude::just,
    text::{ident, whitespace},
    Parser,
};

use super::{literal::Literal, path::Ident};

#[derive(Clone, Debug, PartialEq)]
pub struct InputEntry {
    pub name: Ident,
    pub ty: String,
    pub default: Option<Literal>,
}

impl InputEntry {
    #[cfg(test)]
    pub fn new(name: impl Into<String>, ty: String, default: Option<Literal>) -> Self {
        InputEntry {
            name: Ident::new(name),
            ty,
            default,
        }
    }

    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then_ignore(whitespace())
            .then_ignore(just(":"))
            .then_ignore(whitespace())
            .then(ident())
            .then(
                whitespace()
                    .ignored()
                    .then_ignore(just("="))
                    .then_ignore(whitespace())
                    .then(Literal::parser(sample_rate))
                    .map(|((), value)| value)
                    .or_not(),
            )
            .map(|((name, ty), default)| InputEntry { name, ty, default })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Inputs(pub Vec<InputEntry>);

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
            .map(|(mut entries, last_entry)| {
                if let Some(entry) = last_entry {
                    entries.push(entry);
                }

                Inputs(entries)
            })
            .then_ignore(whitespace())
            .then_ignore(just("}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_entry() {
        let cases = [
            ("foo:sig", InputEntry::new("foo", "sig".into(), None)),
            (
                "foo: percentage=3%",
                InputEntry::new("foo", "percentage".into(), Some(Literal::Float(0.03))),
            ),
            (
                "delay: time = 24ms",
                InputEntry::new(
                    "delay",
                    "time".into(),
                    Some(Literal::Float(24.0 * 44100.0 / 1000.0)),
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
                Inputs(vec![InputEntry::new("x", "sig".into(), None)]),
            ),
            (
                "inputs{x:sig,}",
                Inputs(vec![InputEntry::new("x", "sig".into(), None)]),
            ),
            (
                "inputs{\ns:sig,\n feedback:percentage = 20% ,\ndelay : time = 20ms,}",
                Inputs(vec![
                    InputEntry::new("s", "sig".into(), None),
                    InputEntry::new("feedback", "percentage".into(), Some(Literal::Float(0.2))),
                    InputEntry::new("delay", "time".into(), Some(Literal::Float(882.0))),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Inputs::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
