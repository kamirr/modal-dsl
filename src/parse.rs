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
pub mod var;
pub mod yield_;

pub mod kwords {
    pub const LET: &str = "let";
    pub const YIELD: &str = "yield";

    pub const ALL: &[&str] = [LET, YIELD].as_slice();
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program(pub Vec<Item>);

impl Program {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Item::parser(sample_rate)
            .padded()
            .repeated()
            .validate(|items, span, emit| {
                let states = items.iter().filter(|i| matches!(i, Item::State(_))).count();
                let inputs = items.iter().filter(|i| matches!(i, Item::State(_))).count();
                let steps = items.iter().filter(|i| matches!(i, Item::Step(_))).count();

                for (var, name) in [(states, "state"), (inputs, "inputs"), (steps, "step")] {
                    if var == 0 {
                        emit(Simple::custom(span.clone(), format!("{name} not defined")));
                    } else if states > 1 {
                        emit(Simple::custom(
                            span.clone(),
                            format!("multiple {name} definitions"),
                        ));
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
