use chumsky::{
    error::Simple,
    prelude::{choice, just},
    text::whitespace,
    Parser,
};

use super::{literal::Literal, path::Ident};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputType {
    Sig,
    Percentage,
    Time,
}

impl InputType {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        choice((
            just("sig").to(InputType::Sig),
            just("percentage").to(InputType::Percentage),
            just("time").to(InputType::Time),
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputEntry {
    name: Ident,
    ty: InputType,
    default: Option<Literal>,
}

impl InputEntry {
    #[cfg(test)]
    pub fn new(name: impl Into<String>, ty: InputType, default: Option<Literal>) -> Self {
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
            .then(InputType::parser())
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
    fn test_input_type() {
        let cases = [
            ("sig", InputType::Sig),
            ("percentage", InputType::Percentage),
            ("time", InputType::Time),
        ];

        for (text, expected) in cases {
            assert_eq!(InputType::parser().parse(text), Ok(expected));
        }
    }

    #[test]
    fn test_input_entry() {
        let cases = [
            ("foo:sig", InputEntry::new("foo", InputType::Sig, None)),
            (
                "foo: percentage=3%",
                InputEntry::new("foo", InputType::Percentage, Some(Literal::Float(0.03))),
            ),
            (
                "delay: time = 24ms",
                InputEntry::new(
                    "delay",
                    InputType::Time,
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
                Inputs(vec![InputEntry::new("x", InputType::Sig, None)]),
            ),
            (
                "inputs{x:sig,}",
                Inputs(vec![InputEntry::new("x", InputType::Sig, None)]),
            ),
            (
                "inputs{\ns:sig,\n feedback:percentage = 20% ,\ndelay : time = 20ms,}",
                Inputs(vec![
                    InputEntry::new("s", InputType::Sig, None),
                    InputEntry::new("feedback", InputType::Percentage, Some(Literal::Float(0.2))),
                    InputEntry::new("delay", InputType::Time, Some(Literal::Float(882.0))),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Inputs::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
