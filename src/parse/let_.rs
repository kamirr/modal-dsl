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
}

impl Let {
    pub fn parser<'a>(
        expr: Recursive<'a, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + 'a {
        just(LET)
            .padded()
            .ignored()
            .then(Ident::parser())
            .then_ignore(just("=").padded())
            .then(expr)
            .map(|(((), name), value)| Let {
                name,
                value: Box::new(value),
            })
    }
}
