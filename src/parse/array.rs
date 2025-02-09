use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::TextParser,
    Parser,
};

use super::expr::Expr;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub exprs: Vec<Expr>,
    pub span: Range<usize>,
}

impl Array {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        expr.clone()
            .then_ignore(just(",").padded())
            .repeated()
            .then(expr.or_not())
            .padded()
            .delimited_by(just("["), just("]"))
            .map_with_span(|(mut exprs, tail), span| {
                exprs.extend(tail.into_iter());
                Array { exprs, span }
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::literal::{Literal, LiteralValue};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_array() {
        let cases = [
            (
                "[]",
                Array {
                    exprs: vec![],
                    span: 0..2,
                },
            ),
            (
                "[ 1, 2 ]",
                Array {
                    exprs: vec![
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(1.0),
                            span: 2..3,
                        }),
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(2.0),
                            span: 5..6,
                        }),
                    ],
                    span: 0..8,
                },
            ),
            (
                "[true,]",
                Array {
                    exprs: vec![Expr::Literal(Literal {
                        value: LiteralValue::Bool(true),
                        span: 1..5,
                    })],
                    span: 0..7,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Expr::parser().parse(text), Ok(expected.into()));
        }
    }
}
