use chumsky::{error::Simple, text::TextParser, Parser};

use super::{inputs::Inputs, state::State, step::Step};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    State(State),
    Inputs(Inputs),
    Step(Step),
}

impl Item {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        State::parser()
            .padded()
            .map(Item::State)
            .or(Inputs::parser(sample_rate).padded().map(Item::Inputs))
            .or(Step::parser(sample_rate).padded().map(Item::Step))
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::block::Block;

    use super::*;

    #[test]
    fn test_item() {
        let cases = [
            ("state {}", Item::State(State(vec![]))),
            ("inputs {}", Item::Inputs(Inputs(vec![]))),
            (
                "step {}",
                Item::Step(Step(Block {
                    exprs: vec![],
                    ret_last: false,
                })),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Item::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
