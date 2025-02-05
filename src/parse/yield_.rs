use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::whitespace,
    Parser,
};

use super::{expr::Expr, kwords};

#[derive(Clone, Debug, PartialEq)]
pub struct Yield {
    pub value: Box<Expr>,
    pub span: Range<usize>,
}

impl Yield {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        just(kwords::YIELD)
            .ignored()
            .then_ignore(whitespace().at_least(1))
            .then(expr)
            .map_with_span(|((), value), span| (value, span))
            .map(|(value, span)| Yield {
                value: Box::new(value),
                span,
            })
    }
}
