use chumsky::{error::Simple, Parser};

use super::state::State;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    StateDef(State),
}

impl Item {
    fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        State::parser().map(Item::StateDef)
    }
}
