use std::ops::Range;

use chumsky::{error::Simple, text::TextParser, Parser};

use super::{inputs::Inputs, state::State, step::Step};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    State(State),
    Inputs(Inputs),
    Step(Step),
}

impl Item {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        State::parser()
            .padded()
            .map(Item::State)
            .or(Inputs::parser().padded().map(Item::Inputs))
            .or(Step::parser().padded().map(Item::Step))
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Item::State(state) => state.span.clone(),
            Item::Inputs(inputs) => inputs.span.clone(),
            Item::Step(step) => step.span.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::block::Block;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_item() {
        let cases = [
            (
                "state {}",
                Item::State(State {
                    entries: vec![],
                    span: 0..8,
                }),
            ),
            (
                "inputs {}",
                Item::Inputs(Inputs {
                    entries: vec![],
                    span: 0..9,
                }),
            ),
            (
                "step {}",
                Item::Step(Step {
                    block: Block {
                        exprs: vec![],
                        ret_last: false,
                        span: 5..7,
                    },
                    span: 0..7,
                }),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Item::parser().parse(text), Ok(expected));
        }
    }
}
