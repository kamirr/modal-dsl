use chumsky::{
    error::Simple,
    prelude::{choice, just},
    recursive,
    text::TextParser,
    Parser,
};

use super::{
    binop::{Binop, BinopKind},
    block::Block,
    call::Call,
    let_::Let,
    literal::Literal,
    path::Path,
    yield_::Yield,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Block(Block),
    Call(Call),
    Var(Path),
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
                .or(Path::parser().map(Expr::Var))
                .or(expr.clone().delimited_by(just('('), just(')')))
                .padded()
                .boxed();

            let product = atom
                .clone()
                .then(choice((BinopKind::Mul.parser(), BinopKind::Div.parser())))
                .then(atom.clone())
                .map(|((left, op), right)| {
                    Expr::Binop(Binop {
                        left: Box::new(left),
                        right: Box::new(right),
                        op,
                    })
                })
                .or(atom.clone());

            

            product
                .clone()
                .then(choice((BinopKind::Add.parser(), BinopKind::Sub.parser())))
                .then(product.clone())
                .map(|((left, op), right)| {
                    Expr::Binop(Binop {
                        left: Box::new(left),
                        right: Box::new(right),
                        op,
                    })
                })
                .or(product)
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{binop::BinopKind, literal::Literal, path::Ident};

    use super::*;

    #[test]
    fn test_var() {
        let cases = [
            ("foo", vec![Ident::new("foo")]),
            ("foo.bar", vec![Ident::new("foo"), Ident::new("bar")]),
        ];
        for (text, expected) in cases {
            assert_eq!(
                Expr::parser(44100.0).parse(text),
                Ok(Expr::Var(Path(expected)))
            )
        }
    }

    #[test]
    fn test_let() {
        let cases = [(
            "let foo = 4",
            Let {
                name: Ident::new("foo"),
                value: Box::new(Expr::Literal(Literal::Float(4.0))),
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
                value: Box::new(Expr::Var(Path(vec![Ident::new("foo")]))),
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
                    left: Box::new(Expr::Literal(Literal::Float(2.0))),
                    right: Box::new(Expr::Binop(Binop {
                        left: Box::new(Expr::Literal(Literal::Float(3.0))),
                        right: Box::new(Expr::Literal(Literal::Float(4.0))),
                        op: BinopKind::Mul,
                    })),
                    op: BinopKind::Add,
                },
            ),
            (
                &["2 * 3 + x"],
                Binop {
                    left: Box::new(Expr::Binop(Binop {
                        left: Box::new(Expr::Literal(Literal::Float(2.0))),
                        right: Box::new(Expr::Literal(Literal::Float(3.0))),
                        op: BinopKind::Mul,
                    })),
                    right: Box::new(Expr::Var(Path(vec![Ident::new("x")]))),
                    op: BinopKind::Add,
                },
            ),
        ];

        for (texts, expected) in cases {
            for text in texts {
                assert_eq!(
                    Expr::parser(44100.0).parse(*text),
                    Ok(Expr::Binop(expected.clone()))
                )
            }
        }
    }
}
