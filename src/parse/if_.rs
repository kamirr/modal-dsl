use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{recursive, Recursive},
    text::{keyword, whitespace},
    Parser,
};

use super::{block::Block, expr::Expr, kwords};

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Box<Expr>>,
    pub span: Range<usize>,
}

impl If {
    #[cfg(test)]
    pub fn new(cond: Expr, then: Block, else_: Option<Expr>, span: Range<usize>) -> Self {
        If {
            cond: Box::new(cond),
            then,
            else_: else_.map(Box::new),
            span,
        }
    }

    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        recursive(|if_| {
            keyword(kwords::IF)
                .then_ignore(whitespace().at_least(1))
                .then(expr.clone())
                .then(Block::parser(expr.clone()))
                .then(
                    keyword(kwords::ELSE)
                        .then_ignore(whitespace())
                        .then(Block::parser(expr).map(Expr::Block).or(if_.map(Expr::If)))
                        .map(|((), blk)| blk)
                        .or_not(),
                )
                .map_with_span(|((((), cond), then), else_), span| If {
                    cond: Box::new(cond),
                    then,
                    else_: else_.map(Box::new),
                    span,
                })
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{
        binop::{Binop, BinopKind},
        literal::{Literal, LiteralValue},
        loop_::Break,
        path::Ident,
        unop::{Unop, UnopKind},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn test_if_else() {
        let code = "if 2.0{}else{}";
        let expected = If::new(
            Expr::Literal(Literal {
                value: LiteralValue::Float(2.0),
                span: 3..6,
            }),
            Block {
                exprs: Vec::new(),
                ret_last: false,
                span: 6..8,
            },
            Some(
                Block {
                    exprs: Vec::new(),
                    ret_last: false,
                    span: 12..14,
                }
                .into(),
            ),
            0..14,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }

    #[test]
    fn test_if() {
        let code = "if a > 0.0 { break }";
        let expected = If::new(
            Expr::Binop(Binop {
                left: Box::new(Expr::Var(Ident::new("a", 3..4))),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralValue::Float(0.0),
                    span: 7..10,
                })),
                op: BinopKind::Gt,
                span: 3..10,
            }),
            Block {
                exprs: vec![Expr::Break(Break {
                    expr: None,
                    span: 13..18,
                })],
                ret_last: true,
                span: 11..20,
            },
            None,
            0..20,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }

    #[test]
    fn test_if_elif_else() {
        let code = "if a < 1 { 1 } else if a < 2 { 2 } else { -1 }";
        let expected = If::new(
            Expr::Binop(Binop {
                left: Box::new(Expr::Var(Ident::new("a", 3..4))),
                right: Box::new(Expr::Literal(Literal {
                    value: LiteralValue::Float(1.0),
                    span: 7..8,
                })),
                op: BinopKind::Lt,
                span: 3..8,
            }),
            Block {
                exprs: vec![Expr::Literal(Literal {
                    value: LiteralValue::Float(1.0),
                    span: 11..12,
                })],
                ret_last: true,
                span: 9..15,
            },
            Some(Expr::If(If::new(
                Expr::Binop(Binop {
                    left: Box::new(Expr::Var(Ident::new("a", 23..24))),
                    right: Box::new(Expr::Literal(Literal {
                        value: LiteralValue::Float(2.0),
                        span: 27..28,
                    })),
                    op: BinopKind::Lt,
                    span: 23..28,
                }),
                Block {
                    exprs: vec![Expr::Literal(Literal {
                        value: LiteralValue::Float(2.0),
                        span: 31..32,
                    })],
                    ret_last: true,
                    span: 29..35,
                },
                Some(Expr::Block(Block {
                    exprs: vec![Expr::Unop(Unop {
                        expr: Box::new(Expr::Literal(Literal {
                            value: LiteralValue::Float(1.0),
                            span: 43..44,
                        })),
                        op: UnopKind::Neg,
                        span: 42..44,
                    })],
                    ret_last: true,
                    span: 40..46,
                })),
                20..46,
            ))),
            0..46,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }
}
