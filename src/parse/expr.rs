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
}

#[cfg(test)]
mod tests {
    use crate::parse::{binop::BinopKind, literal::Literal, path::Ident};

    use super::*;

    #[test]
    fn test_var() {
        let cases = [("foo", Ident::new("foo"))];
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
                name: Ident::new("foo"),
                value: Box::new(Literal::Float(4.0).into()),
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
                        name: Ident::new("foo"),
                    }
                    .into(),
                ),
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
                &["2 + 3 * 4", "2+3*4", "2 + ( 3 * 4)"][..],
                Binop {
                    left: Box::new(Literal::Float(2.0).into()),
                    right: Box::new(
                        Binop {
                            left: Box::new(Literal::Float(3.0).into()),
                            right: Box::new(Literal::Float(4.0).into()),
                            op: BinopKind::Mul,
                        }
                        .into(),
                    ),
                    op: BinopKind::Add,
                },
            ),
            (
                &["2 * 3 + x"],
                Binop {
                    left: Box::new(
                        Binop {
                            left: Box::new(Literal::Float(2.0).into()),
                            right: Box::new(Literal::Float(3.0).into()),
                            op: BinopKind::Mul,
                        }
                        .into(),
                    ),
                    right: Box::new(
                        Var {
                            name: Ident::new("x"),
                        }
                        .into(),
                    ),
                    op: BinopKind::Add,
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
