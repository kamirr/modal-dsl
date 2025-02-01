use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::TextParser,
    Parser,
};

use super::{expr::Expr, kwords::LET, path::Ident};

#[derive(Clone, Debug, PartialEq)]
pub struct Let {
    pub name: Ident,
    pub value: Box<Expr>,
    pub span: Range<usize>,
}

impl Let {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        just(LET)
            .padded()
            .ignored()
            .then(Ident::parser())
            .then_ignore(just("=").padded())
            .then(expr)
            .map_with_span(|(((), name), value), span| (name, value, span))
            .map(|(name, value, span)| Let {
                name,
                value: Box::new(value),
                span,
            })
    }
}
