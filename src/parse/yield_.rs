use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::TextParser,
    Parser,
};

use super::{expr::Expr, kwords::YIELD};

#[derive(Clone, Debug, PartialEq)]
pub struct Yield {
    pub value: Box<Expr>,
}

impl Yield {
    pub fn parser<'a>(
        expr: Recursive<'a, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + 'a {
        just(YIELD)
            .padded()
            .ignored()
            .then(expr)
            .map(|((), value)| Yield {
                value: Box::new(value),
            })
    }
}
