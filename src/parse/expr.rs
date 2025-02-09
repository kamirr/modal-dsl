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
    array::Array,
    binop::{Binop, BinopKind},
    block::Block,
    call::Call,
    if_::If,
    let_::Let,
    literal::Literal,
    loop_::{Break, Loop},
    path::Path,
    unop::{Unop, UnopKind},
    yield_::Yield,
};

#[derive(Clone, Debug, PartialEq, From)]
pub enum Expr {
    Array(Array),
    Block(Block),
    Call(Call),
    If(If),
    Loop(Loop),
    Break(Break),
    Var(Path),
    Let(Let),
    Yield(Yield),
    Literal(Literal),
    Binop(Binop),
    Unop(Unop),
}

impl Expr {
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        recursive::recursive(|expr| {
            let atom = Literal::parser()
                .map(Expr::Literal)
                .or(Array::parser(expr.clone()).map(Expr::Array))
                .or(Block::parser(expr.clone()).map(Expr::Block))
                .or(Call::parser(expr.clone()).map(Expr::Call))
                .or(If::parser(expr.clone()).map(Expr::If))
                .or(Loop::parser(expr.clone()).map(Expr::Loop))
                .or(Break::parser(expr.clone()).map(Expr::Break))
                .or(Let::parser(expr.clone()).map(Expr::Let))
                .or(Yield::parser(expr.clone()).map(Expr::Yield))
                .or(Path::parser().map(Expr::Var))
                .or(expr.clone().delimited_by(just('('), just(')')))
                .padded()
                .boxed();

            let unop = choice((
                UnopKind::Deref.parser(),
                UnopKind::Neg.parser(),
                UnopKind::Not.parser(),
            ))
            .map_with_span(|unop_kind, span| (unop_kind, span))
            .repeated()
            .then(atom)
            .map(|(unop_kinds, atom)| {
                unop_kinds
                    .into_iter()
                    .rev()
                    .fold(atom, |expr, (unop_kind, kind_span)| {
                        let span = kind_span.start..expr.span().end;
                        Unop {
                            expr: Box::new(expr),
                            op: unop_kind,
                            span,
                        }
                        .into()
                    })
            })
            .boxed();

            let product = unop
                .clone()
                .then(
                    choice((BinopKind::Mul.parser(), BinopKind::Div.parser()))
                        .then(unop.clone())
                        .repeated(),
                )
                .map(|(lhs, seq)| {
                    seq.into_iter()
                        .fold(lhs, |lhs, (op, rhs)| op.apply(lhs, rhs).into())
                })
                .boxed();

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
                })
                .boxed();

            let cmp = sum
                .clone()
                .then(
                    choice((
                        BinopKind::Lt.parser(),
                        BinopKind::Lte.parser(),
                        BinopKind::Eq.parser(),
                        BinopKind::Gte.parser(),
                        BinopKind::Gt.parser(),
                    ))
                    .then(sum.clone())
                    .repeated(),
                )
                .map(|(lhs, seq)| {
                    seq.into_iter()
                        .fold(lhs, |lhs, (op, rhs)| op.apply(lhs, rhs).into())
                })
                .boxed();

            let assign = cmp
                .clone()
                .then(
                    BinopKind::Assign
                        .parser()
                        .then(cmp.clone())
                        .map(|(_op, rhs)| rhs)
                        .repeated(),
                )
                .map(|(lhs, seq)| seq.into_iter().rev().chain(std::iter::once(lhs)))
                .map(|mut seq| {
                    let rhs = seq.next().unwrap();
                    seq.fold(rhs, |rhs, lhs| BinopKind::Assign.apply(lhs, rhs).into())
                })
                .boxed();

            assign
        })
    }

    pub fn span(&self) -> Range<usize> {
        match self {
            Expr::Array(array) => array.span.clone(),
            Expr::Block(block) => block.span.clone(),
            Expr::Call(call) => call.span.clone(),
            Expr::If(if_) => if_.span.clone(),
            Expr::Loop(loop_) => loop_.span.clone(),
            Expr::Break(break_) => break_.span.clone(),
            Expr::Var(path) => path.span.clone(),
            Expr::Let(let_) => let_.span.clone(),
            Expr::Yield(yield_) => yield_.span.clone(),
            Expr::Literal(literal) => literal.span.clone(),
            Expr::Binop(binop) => binop.span.clone(),
            Expr::Unop(unop) => unop.span.clone(),
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
        let cases = [("foo", Path::new_single(Ident::new("foo", 0..3)))];
        for (text, expected) in cases {
            assert_eq!(Expr::parser().parse(text), Ok(expected.into()))
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
            assert_eq!(Expr::parser().parse(text), Ok(Expr::Let(expected)));
        }
    }

    #[test]
    fn test_yield() {
        let cases = [(
            "yield foo",
            Yield {
                value: Box::new(Path::new_single(Ident::new("foo", 6..9)).into()),
                span: 0..9,
            },
        )];

        for (text, expected) in cases {
            assert_eq!(Expr::parser().parse(text), Ok(Expr::Yield(expected)));
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
                    right: Box::new(Path::new_single(Ident::new("x", 8..9)).into()),
                    op: BinopKind::Add,
                    span: 0..9,
                },
            ),
        ];

        for (texts, expected) in cases {
            for text in texts {
                assert_eq!(Expr::parser().parse(*text), Ok(expected.clone().into()))
            }
        }
    }
}
