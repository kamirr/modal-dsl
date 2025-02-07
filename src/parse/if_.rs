use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::Recursive,
    text::{keyword, whitespace},
    Parser,
};

use super::{block::Block, expr::Expr, kwords};

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Block>,
    pub span: Range<usize>,
}

impl If {
    #[cfg(test)]
    pub fn new(cond: Expr, then: Block, else_: Option<Block>, span: Range<usize>) -> Self {
        If {
            cond: Box::new(cond),
            then,
            else_,
            span,
        }
    }

    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        keyword(kwords::IF)
            .then_ignore(whitespace().at_least(1))
            .then(expr.clone())
            .then(Block::parser(expr.clone()))
            .then(
                keyword(kwords::ELSE)
                    .then(Block::parser(expr))
                    .map(|((), blk)| blk)
                    .or_not(),
            )
            .map_with_span(|((((), cond), then), else_), span| If {
                cond: Box::new(cond),
                then,
                else_,
                span,
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
        literal::{Literal, LiteralValue},
        loop_::Break,
        path::Ident,
    };
    use pretty_assertions::assert_eq;

    use super::*;

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
            Some(Block {
                exprs: Vec::new(),
                ret_last: false,
                span: 12..14,
            }),
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
}
