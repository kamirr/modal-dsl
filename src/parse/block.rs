use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::TextParser,
    Parser,
};

use super::expr::Expr;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub ret_last: bool,
    pub span: Range<usize>,
}

impl Block {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        just("{")
            .padded()
            .ignored()
            .then(expr.clone().then_ignore(just(";").padded()).repeated())
            .then(expr.then(just(";").padded().or_not()).padded().or_not())
            .then_ignore(just("}").padded())
            .map_with_span(|(((), exprs), last), span| (exprs, last, span))
            .map(|(mut exprs, last, span)| {
                if let Some((last, semicolon)) = last {
                    exprs.push(last);
                    Block {
                        exprs,
                        ret_last: semicolon.is_none(),
                        span,
                    }
                } else {
                    Block {
                        exprs,
                        ret_last: false,
                        span,
                    }
                }
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
        let_::Let,
        literal::{Literal, LiteralValue},
        path::Ident,
        var::Var,
        yield_::Yield,
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_block() {
        let cases = [
            (
                "{yield 2.0;}",
                Block {
                    exprs: vec![Yield {
                        value: Box::new(Expr::Literal(Literal {
                            value: LiteralValue::Float(2.0),
                            span: 7..10,
                        })),
                        span: 1..10,
                    }
                    .into()],
                    ret_last: false,
                    span: 0..12,
                },
            ),
            (
                " { let two = 2; let three = 3e1; yield two / three; two } ",
                Block {
                    exprs: vec![
                        Let {
                            name: Ident::new("two", 7..10),
                            value: Box::new(Expr::Literal(Literal {
                                value: LiteralValue::Float(2.0),
                                span: 13..14,
                            })),
                            span: 3..14,
                        }
                        .into(),
                        Let {
                            name: Ident::new("three", 20..25),
                            value: Box::new(Expr::Literal(Literal {
                                value: LiteralValue::Float(30.0),
                                span: 28..31,
                            })),
                            span: 16..31,
                        }
                        .into(),
                        Yield {
                            value: Box::new(Expr::Binop(Binop {
                                left: Box::new(Expr::Var(Var::new(Ident::new("two", 39..42)))),
                                right: Box::new(Expr::Var(Var::new(Ident::new("three", 45..50)))),
                                op: BinopKind::Div,
                                span: 39..50,
                            })),
                            span: 33..50,
                        }
                        .into(),
                        Expr::Var(Var::new(Ident::new("two", 52..55))),
                    ],
                    ret_last: true,
                    span: 1..58,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Expr::parser(44100.0).parse(text), Ok(Expr::Block(expected)));
        }
    }
}
