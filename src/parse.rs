use chumsky::{error::Simple, text::TextParser, Parser};
use item::Item;

pub mod binop;
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
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        Item::parser(sample_rate).padded().repeated().map(Program)
    }
}
