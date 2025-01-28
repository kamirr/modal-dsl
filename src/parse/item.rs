use chumsky::{error::Simple, text::TextParser, Parser};

use super::{inputs::Inputs, state::State, step::Step};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    StateDef(State),
    InputsDef(Inputs),
    StepDef(Step),
}

impl Item {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        State::parser()
            .padded()
            .map(Item::StateDef)
            .or(Inputs::parser(sample_rate).padded().map(Item::InputsDef))
            .or(Step::parser(sample_rate).padded().map(Item::StepDef))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_item() {
        let cases = [
            ("state {}", Item::StateDef(State(vec![]))),
            ("inputs {}", Item::InputsDef(Inputs(vec![]))),
            ("step {}", Item::StepDef(Step(vec![]))),
        ];

        for (text, expected) in cases {
            assert_eq!(Item::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
