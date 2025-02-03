use std::ops::Range;

use chumsky::{error::Simple, prelude::just, text::whitespace, Parser};

use super::{block::Block, expr::Expr};

#[derive(Clone, Debug, PartialEq)]
pub struct Step {
    pub block: Block,
    pub span: Range<usize>,
}

impl Step {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        just("step")
            .ignored()
            .then_ignore(whitespace())
            .then(Expr::parser(sample_rate))
            .map(|((), blk)| blk)
            .try_map(|expr, span| {
                if let Expr::Block(blk) = expr {
                    Ok(Step { block: blk, span })
                } else {
                    Err(Simple::custom(span, "Main step expression must be a block"))
                }
            })
            .validate(|step, span, emit| {
                if step.block.ret_last {
                    emit(Simple::custom(
                        span,
                        "Main block of step cannot omit the last semicolon",
                    ));
                }
                step
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
        yield_::Yield,
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_step() {
        let cases = [
            (
                "step{yield 2.0;}",
                Step {
                    block: Block {
                        exprs: vec![Yield {
                            value: Box::new(
                                Literal {
                                    value: LiteralValue::Float(2.0),
                                    span: 11..14,
                                }
                                .into(),
                            ),
                            span: 5..14,
                        }
                        .into()],
                        ret_last: false,
                        span: 4..16,
                    },
                    span: 0..16,
                },
            ),
            (
                "step { let two = 2; let three = 3e1; yield two / three; } ",
                Step {
                    block: Block {
                        exprs: vec![
                            Let {
                                name: Ident::new("two", 11..14),
                                value: Box::new(
                                    Literal {
                                        value: LiteralValue::Float(2.0),
                                        span: 17..18,
                                    }
                                    .into(),
                                ),
                                span: 7..18,
                            }
                            .into(),
                            Let {
                                name: Ident::new("three", 24..29),
                                value: Box::new(
                                    Literal {
                                        value: LiteralValue::Float(30.0),
                                        span: 32..35,
                                    }
                                    .into(),
                                ),
                                span: 20..35,
                            }
                            .into(),
                            Yield {
                                value: Box::new(
                                    Binop {
                                        left: Box::new(Ident::new("two", 43..46).into()),
                                        right: Box::new(Ident::new("three", 49..54).into()),
                                        op: BinopKind::Div,
                                        span: 43..54,
                                    }
                                    .into(),
                                ),
                                span: 37..54,
                            }
                            .into(),
                        ],
                        ret_last: false,
                        span: 5..58,
                    },
                    span: 0..58,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Step::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
