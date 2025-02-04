use std::ops::Range;

use chumsky::{error::Simple, prelude::Recursive, text::keyword, Parser};

use super::{block::Block, expr::Expr, kwords};

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Block,
    pub span: Range<usize>,
}

impl If {
    #[cfg(test)]
    pub fn new(cond: Expr, then: Block, else_: Block, span: Range<usize>) -> Self {
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
            .then(expr.clone())
            .then(Block::parser(expr.clone()))
            .then_ignore(keyword(kwords::ELSE))
            .then(Block::parser(expr))
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
    use crate::parse::literal::{Literal, LiteralValue};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_if() {
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
            Block {
                exprs: Vec::new(),
                ret_last: false,
                span: 12..14,
            },
            0..14,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }
}
