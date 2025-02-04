use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{just, one_of},
    text::{whitespace, TextParser},
    Parser,
};

use super::path::Ident;

#[derive(Clone, Debug, PartialEq)]
pub struct InputEntry {
    pub name: Ident,
    pub ty: Ident,
    pub args: Vec<String>,
    pub span: Range<usize>,
}

impl InputEntry {
    #[cfg(test)]
    pub fn new(name: Ident, ty: Ident, args: Vec<String>, span: Range<usize>) -> Self {
        InputEntry {
            name,
            ty,
            args,
            span,
        }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let arg = one_of("(), \n\t").not().padded().repeated().at_least(1);
        Ident::parser()
            .then_ignore(just("=").padded())
            .then(Ident::parser())
            .then_ignore(just("(").padded())
            .then(
                arg.clone()
                    .then_ignore(just(",").padded())
                    .repeated()
                    .then(arg.or_not())
                    .boxed(),
            )
            .then_ignore(just(")").padded())
            .map_with_span(|((name, ty), (args, tail)), span| InputEntry {
                name,
                ty,
                args: args
                    .into_iter()
                    .chain(tail.into_iter())
                    .map(|vc| vc.into_iter().collect())
                    .collect(),
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
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        just("inputs")
            .ignored()
            .then_ignore(whitespace())
            .then_ignore(just("{"))
            .then_ignore(whitespace())
            .then(
                InputEntry::parser()
                    .then_ignore(whitespace())
                    .then(just(","))
                    .then_ignore(whitespace())
                    .map(|(entry, _)| entry)
                    .repeated(),
            )
            .map(|((), entries)| entries)
            .then(InputEntry::parser().or_not())
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
    use pretty_assertions::assert_eq;

    #[test]
    fn test_input_entry() {
        let cases = [
            (
                "foo = sig()",
                InputEntry::new(
                    Ident::new("foo", 0..3),
                    Ident::new("sig", 6..9),
                    vec![],
                    0..11,
                ),
            ),
            (
                "foo = percentage(3%)",
                InputEntry::new(
                    Ident::new("foo", 0..3),
                    Ident::new("percentage", 6..16),
                    vec!["3%".into()],
                    0..20,
                ),
            ),
            (
                "delay = time(24ms)",
                InputEntry::new(
                    Ident::new("delay", 0..5),
                    Ident::new("time", 8..12),
                    vec!["24ms".into()],
                    0..18,
                ),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(InputEntry::parser().parse(text), Ok(expected));
        }
    }

    #[test]
    fn test_inputs() {
        let cases = [
            (
                "inputs{x=sig()}",
                Inputs {
                    entries: vec![InputEntry::new(
                        Ident::new("x", 7..8),
                        Ident::new("sig", 9..12),
                        vec![],
                        7..14,
                    )],
                    span: 0..15,
                },
            ),
            (
                "inputs{\ns=sig(),\n feedback=percentage( 20%) ,\ndelay = time (20ms, )}",
                Inputs {
                    entries: vec![
                        InputEntry::new(
                            Ident::new("s", 8..9),
                            Ident::new("sig", 10..13),
                            vec![],
                            8..15,
                        ),
                        InputEntry::new(
                            Ident::new("feedback", 18..26),
                            Ident::new("percentage", 27..37),
                            vec!["20%".into()],
                            18..44,
                        ),
                        InputEntry::new(
                            Ident::new("delay", 46..51),
                            Ident::new("time", 54..58),
                            vec!["20ms".into()],
                            46..67,
                        ),
                    ],
                    span: 0..68,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Inputs::parser().parse(text), Ok(expected));
        }
    }
}
