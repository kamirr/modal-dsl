use chumsky::{error::Simple, text::TextParser, Parser};
use inputs::Inputs;
use item::Item;
use state::State;
use step::Step;

pub mod binop;
pub mod block;
pub mod call;
pub mod expr;
pub mod inputs;
pub mod item;
pub mod let_;
pub mod literal;
pub mod path;
pub mod state;
pub mod step;
pub mod yield_;

pub mod kwords {
    pub const LET: &str = "let";
    pub const YIELD: &str = "yield";

    pub const ALL: &[&str] = [LET, YIELD].as_slice();
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program(pub Vec<Item>);

impl Program {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        Item::parser()
            .padded()
            .repeated()
            .at_least(3)
            .validate(|items, _span, emit| {
                let mut one_state = false;
                let mut one_inputs = false;
                let mut one_step = false;

                for item in items.iter() {
                    match item {
                        Item::State(state) => {
                            if !one_state {
                                one_state = true;
                            } else {
                                emit(Simple::custom(
                                    state.span.clone(),
                                    "Duplicate state definition",
                                ));
                            }
                        }
                        Item::Inputs(inputs) => {
                            if !one_inputs {
                                one_inputs = true;
                            } else {
                                emit(Simple::custom(
                                    inputs.span.clone(),
                                    "Duplicate inputs definition",
                                ));
                            }
                        }
                        Item::Step(step) => {
                            if !one_step {
                                one_step = true;
                            } else {
                                emit(Simple::custom(
                                    step.span.clone(),
                                    "Duplicate step definition",
                                ));
                            }
                        }
                    }
                }

                items
            })
            .map(Program)
    }

    pub fn state(&self) -> &State {
        self.0
            .iter()
            .find_map(|item| match item {
                Item::State(state) => Some(state),
                _ => None,
            })
            .unwrap()
    }

    pub fn inputs(&self) -> &Inputs {
        self.0
            .iter()
            .find_map(|item| match item {
                Item::Inputs(inputs) => Some(inputs),
                _ => None,
            })
            .unwrap()
    }

    pub fn step(&self) -> &Step {
        self.0
            .iter()
            .find_map(|item| match item {
                Item::Step(step) => Some(step),
                _ => None,
            })
            .unwrap()
    }
}
