use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{choice, just},
    recursive,
    text::TextParser,
    Parser,
};
use derive_more::derive::From;

use super::{
    binop::{Binop, BinopKind},
    block::Block,
    call::Call,
    let_::Let,
    literal::Literal,
    var::Var,
    yield_::Yield,
};

#[derive(Clone, Debug, PartialEq, From)]
pub enum Expr {
    Block(Block),
    Call(Call),
    Var(Var),
    Let(Let),
    Yield(Yield),
    Literal(Literal),
    Binop(Binop),
}

impl Expr {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        recursive::recursive(|expr| {
            let atom = Literal::parser(sample_rate)
                .map(Expr::Literal)
                .or(Block::parser(expr.clone()).map(Expr::Block))
                .or(Call::parser(expr.clone()).map(Expr::Call))
                .or(Let::parser(expr.clone()).map(Expr::Let))
                .or(Yield::parser(expr.clone()).map(Expr::Yield))
                .or(Var::parser().map(Expr::Var))
                .or(expr.clone().delimited_by(just('('), just(')')))
                .padded()
                .boxed();

            let product = atom
                .clone()
                .then(
                    choice((BinopKind::Mul.parser(), BinopKind::Div.parser()))
                        .then(atom.clone())
                        .repeated(),
                )
                .map(|(lhs, seq)| {
                    seq.into_iter()
                        .fold(lhs, |lhs, (op, rhs)| op.apply(lhs, rhs).into())
                });

            let sum = product
                .clone()
                .then(
                    choice((BinopKind::Add.parser(), BinopKind::Sub.parser()))
                        .then(product.clone())
                        .repeated(),
                )
                .map(|(lhs, seq)| {
                    seq.into_iter()
                        .fold(lhs, |lhs, (op, rhs)| op.apply(lhs, rhs).into())
                });

            let assign = sum
                .clone()
                .then(BinopKind::Assign.parser().then(sum.clone()).repeated())
                .map(|(lhs, seq)| {
                    seq.into_iter()
                        .fold(lhs, |lhs, (op, rhs)| op.apply(lhs, rhs).into())
                });

            assign
        })
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Expr::Block(block) => block.span.clone(),
            Expr::Call(_call) => 0..0,
            Expr::Var(var) => var.name.span.clone(),
            Expr::Let(let_) => let_.span.clone(),
            Expr::Yield(yield_) => yield_.span.clone(),
            Expr::Literal(literal) => literal.span.clone(),
            Expr::Binop(binop) => binop.span.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::BinopKind,
        literal::{Literal, LiteralValue},
        path::Ident,
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_var() {
        let cases = [("foo", Ident::new("foo", 0..3))];
        for (text, expected) in cases {
            assert_eq!(
                Expr::parser(44100.0).parse(text),
                Ok(Var { name: expected }.into())
            )
        }
    }

    #[test]
    fn test_let() {
        let cases = [(
            "let foo = 4",
            Let {
                name: Ident::new("foo", 4..7),
                value: Box::new(
                    Literal {
                        value: LiteralValue::Float(4.0),
                        span: 10..11,
                    }
                    .into(),
                ),
                span: 0..11,
            },
        )];

        for (text, expected) in cases {
            assert_eq!(Expr::parser(44100.0).parse(text), Ok(Expr::Let(expected)));
        }
    }

    #[test]
    fn test_yield() {
        let cases = [(
            "yield foo",
            Yield {
                value: Box::new(
                    Var {
                        name: Ident::new("foo", 6..9),
                    }
                    .into(),
                ),
                span: 0..9,
            },
        )];

        for (text, expected) in cases {
            assert_eq!(Expr::parser(44100.0).parse(text), Ok(Expr::Yield(expected)));
        }
    }

    #[test]
    fn test_binop() {
        let cases = [
            (
                &["2 + 3 * 4"][..],
                Binop {
                    left: Box::new(
                        Literal {
                            value: LiteralValue::Float(2.0),
                            span: 0..1,
                        }
                        .into(),
                    ),
                    right: Box::new(
                        Binop {
                            left: Box::new(
                                Literal {
                                    value: LiteralValue::Float(3.0),
                                    span: 4..5,
                                }
                                .into(),
                            ),
                            right: Box::new(
                                Literal {
                                    value: LiteralValue::Float(4.0),
                                    span: 8..9,
                                }
                                .into(),
                            ),
                            op: BinopKind::Mul,
                            span: 4..9,
                        }
                        .into(),
                    ),
                    op: BinopKind::Add,
                    span: 0..9,
                },
            ),
            (
                &["2 * 3 + x"],
                Binop {
                    left: Box::new(
                        Binop {
                            left: Box::new(
                                Literal {
                                    value: LiteralValue::Float(2.0),
                                    span: 0..1,
                                }
                                .into(),
                            ),
                            right: Box::new(
                                Literal {
                                    value: LiteralValue::Float(3.0),
                                    span: 4..5,
                                }
                                .into(),
                            ),
                            op: BinopKind::Mul,
                            span: 0..5,
                        }
                        .into(),
                    ),
                    right: Box::new(
                        Var {
                            name: Ident::new("x", 8..9),
                        }
                        .into(),
                    ),
                    op: BinopKind::Add,
                    span: 0..9,
                },
            ),
        ];

        for (texts, expected) in cases {
            for text in texts {
                assert_eq!(
                    Expr::parser(44100.0).parse(*text),
                    Ok(expected.clone().into())
                )
            }
        }
    }
}
