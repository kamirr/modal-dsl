use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{choice, Recursive},
    text::{keyword, whitespace},
    Parser,
};

use super::{block::Block, expr::Expr, kwords};

#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    pub cond: Option<Box<Expr>>,
    pub body: Block,
    pub span: Range<usize>,
}

impl Loop {
    #[cfg(test)]
    pub(crate) fn loop_(body: Block, span: Range<usize>) -> Self {
        Loop {
            cond: None,
            body,
            span,
        }
    }

    #[cfg(test)]
    pub(crate) fn while_(cond: Expr, body: Block, span: Range<usize>) -> Self {
        Loop {
            cond: Some(Box::new(cond)),
            body,
            span,
        }
    }

    fn parser_loop(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        keyword(kwords::LOOP)
            .then(Block::parser(expr.clone()))
            .map_with_span(|((), body), span| Loop {
                cond: None,
                body,
                span,
            })
    }

    fn parser_while(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        keyword(kwords::WHILE)
            .then_ignore(whitespace().at_least(1))
            .then(expr.clone())
            .then(Block::parser(expr.clone()))
            .map_with_span(|(((), cond), body), span| Loop {
                cond: Some(Box::new(cond)),
                body,
                span,
            })
    }

    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        choice((Loop::parser_loop(expr.clone()), Loop::parser_while(expr)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub expr: Option<Box<Expr>>,
    pub span: Range<usize>,
}

impl Break {
    #[cfg(test)]
    pub(crate) fn new(span: Range<usize>) -> Self {
        Break { expr: None, span }
    }

    #[cfg(test)]
    pub(crate) fn with_expr(expr: Expr, span: Range<usize>) -> Self {
        Break {
            expr: Some(Box::new(expr)),
            span,
        }
    }

    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        keyword(kwords::BREAK)
            .then(
                whitespace()
                    .at_least(1)
                    .ignored()
                    .then(expr)
                    .map(|((), expr)| expr)
                    .or_not(),
            )
            .map_with_span(|((), expr), span| Break {
                expr: expr.map(Box::new),
                span,
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
        literal::{Literal, LiteralValue},
        path::{Ident, Path},
    };

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_loop() {
        let code = "loop{  }";
        let expected = Loop::loop_(
            Block {
                exprs: vec![],
                ret_last: false,
                span: 4..8,
            },
            0..8,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }

    #[test]
    fn test_while() {
        let code = "while x > 0 {  }";
        let expected = Loop::while_(
            Binop {
                left: Box::new(Path::new_single(Ident::new("x", 6..7)).into()),
                right: Box::new(
                    Literal {
                        value: LiteralValue::Float(0.0),
                        span: 10..11,
                    }
                    .into(),
                ),
                op: BinopKind::Gt,
                span: 6..11,
            }
            .into(),
            Block {
                exprs: vec![],
                ret_last: false,
                span: 12..16,
            },
            0..16,
        );

        assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
    }

    #[test]
    fn test_break() {
        let cases = [
            ("break", Break::new(0..5)),
            (
                "break x",
                Break::with_expr(Path::new_single(Ident::new("x", 6..7)).into(), 0..7),
            ),
        ];

        for (code, expected) in cases {
            assert_eq!(Expr::parser().parse(code), Ok(expected.into()));
        }
    }
}
