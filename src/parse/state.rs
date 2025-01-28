use chumsky::{
    error::Simple,
    prelude::{choice, just},
    text::whitespace,
    Parser,
};

use super::path::Ident;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StateVarType {
    Float,
    Buffer,
}

impl StateVarType {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        choice((
            just("float").to(StateVarType::Float),
            just("buffer").to(StateVarType::Buffer),
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateEntry {
    name: Ident,
    ty: StateVarType,
}

impl StateEntry {
    #[cfg(test)]
    pub fn new(name: impl Into<String>, ty: StateVarType) -> Self {
        StateEntry {
            name: Ident::new(name),
            ty,
        }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser()
            .then_ignore(whitespace())
            .then_ignore(just(":"))
            .then_ignore(whitespace())
            .then(StateVarType::parser())
            .map(|(name, ty)| StateEntry { name, ty })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State(pub Vec<StateEntry>);

impl State {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        just("state")
            .ignored()
            .then_ignore(whitespace())
            .then_ignore(just("{"))
            .then_ignore(whitespace())
            .then(
                StateEntry::parser()
                    .then_ignore(whitespace())
                    .then(just(","))
                    .then_ignore(whitespace())
                    .map(|(entry, _)| entry)
                    .repeated(),
            )
            .map(|((), entries)| entries)
            .then(StateEntry::parser().or_not())
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
    use super::*;

    #[test]
    fn test_state_var_type() {
        let cases = [
            ("float", StateVarType::Float),
            ("buffer", StateVarType::Buffer),
        ];

        for (text, expected) in cases {
            assert_eq!(StateVarType::parser().parse(text), Ok(expected));
        }
    }

    #[test]
    fn test_state_entry() {
        let cases = [
            ("in:float", ("in", StateVarType::Float)),
            ("buf : buffer", ("buf", StateVarType::Buffer)),
        ];

        for (text, (name, ty)) in cases {
            assert_eq!(
                StateEntry::parser().parse(text),
                Ok(StateEntry {
                    name: Ident::new(name),
                    ty
                })
            );
        }
    }

    #[test]
    fn test_state() {
        let cases = [
            (
                "state{in:float}",
                State(vec![StateEntry::new("in", StateVarType::Float)]),
            ),
            (
                "state{in:float,}",
                State(vec![StateEntry::new("in", StateVarType::Float)]),
            ),
            (
                "state{in:float,buf:buffer}",
                State(vec![
                    StateEntry::new("in", StateVarType::Float),
                    StateEntry::new("buf", StateVarType::Buffer),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(State::parser().parse(text), Ok(expected));
        }
    }
}
