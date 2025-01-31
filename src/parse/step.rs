use chumsky::{error::Simple, prelude::just, text::whitespace, Parser};

use super::{block::Block, expr::Expr};

#[derive(Clone, Debug, PartialEq)]
pub struct Step(pub Block);

impl Step {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        just("step")
            .ignored()
            .then_ignore(whitespace())
            .then(Expr::parser(sample_rate))
            .map(|((), blk)| blk)
            .try_map(|expr, span| {
                if let Expr::Block(blk) = expr {
                    Ok(blk)
                } else {
                    Err(Simple::custom(span, "Main step expression must be a block"))
                }
            })
            .validate(|blk, span, emit| {
                if blk.ret_last {
                    emit(Simple::custom(
                        span,
                        "Main block of step cannot omit the last semicolon",
                    ));
                }
                blk
            })
            .map(Step)
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
        expr::Expr,
        let_::Let,
        literal::Literal,
        path::Ident,
        var::Var,
        yield_::Yield,
    };

    use super::*;

    #[test]
    fn test_state() {
        let cases = [
            (
                "step{yield 2.0;}",
                Step(Block {
                    exprs: vec![Yield {
                        value: Box::new(Expr::Literal(Literal::Float(2.0))),
                    }
                    .into()],
                    ret_last: false,
                }),
            ),
            (
                "step { let two = 2; let three = 3e1; yield two / three; } ",
                Step(Block {
                    exprs: vec![
                        Let {
                            name: Ident::new("two"),
                            value: Box::new(Expr::Literal(Literal::Float(2.0))),
                        }
                        .into(),
                        Let {
                            name: Ident::new("three"),
                            value: Box::new(Expr::Literal(Literal::Float(30.0))),
                        }
                        .into(),
                        Yield {
                            value: Box::new(
                                Binop {
                                    left: Box::new(Var::new("two").into()),
                                    right: Box::new(Var::new("three").into()),
                                    op: BinopKind::Div,
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    ],
                    ret_last: false,
                }),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Step::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
