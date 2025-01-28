use chumsky::{error::Simple, prelude::just, text::TextParser, Parser};

use super::expr::Expr;

#[derive(Clone, Debug, PartialEq)]
pub struct Step(pub Vec<Expr>);

impl Step {
    pub fn parser(sample_rate: f32) -> impl Parser<char, Self, Error = Simple<char>> {
        just("step")
            .ignored()
            .then_ignore(just("{").padded())
            .then(
                Expr::parser(sample_rate)
                    .then_ignore(just(";").padded())
                    .repeated(),
            )
            .map(|((), entries)| entries)
            .map(Step)
            .then_ignore(just("}").padded())
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
        let_::Let,
        literal::Literal,
        path::{Ident, Path},
        yield_::Yield,
    };

    use super::*;

    #[test]
    fn test_state() {
        let cases = [
            (
                "step{yield 2.0;}",
                Step(vec![Expr::Yield(Yield {
                    value: Box::new(Expr::Literal(Literal::Float(2.0))),
                })]),
            ),
            (
                "step { let two = 2; let three = 3e1; yield two / three; } ",
                Step(vec![
                    Expr::Let(Let {
                        name: Ident::new("two"),
                        value: Box::new(Expr::Literal(Literal::Float(2.0))),
                    }),
                    Expr::Let(Let {
                        name: Ident::new("three"),
                        value: Box::new(Expr::Literal(Literal::Float(30.0))),
                    }),
                    Expr::Yield(Yield {
                        value: Box::new(Expr::Binop(Binop {
                            left: Box::new(Expr::Var(Path(vec![Ident::new("two")]))),
                            right: Box::new(Expr::Var(Path(vec![Ident::new("three")]))),
                            op: BinopKind::Div,
                        })),
                    }),
                ]),
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Step::parser(44100.0).parse(text), Ok(expected));
        }
    }
}
