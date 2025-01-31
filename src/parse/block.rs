use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::TextParser,
    Parser,
};

use super::expr::Expr;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub ret_last: bool,
}

impl Block {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        just("{")
            .padded()
            .ignored()
            .then(expr.clone().then_ignore(just(";").padded()).repeated())
            .then(expr.then(just(";").padded().or_not()).padded().or_not())
            .then_ignore(just("}").padded())
            .map(|(((), mut exprs), last)| {
                if let Some((last, semicolon)) = last {
                    exprs.push(last);
                    Block {
                        exprs,
                        ret_last: semicolon.is_none(),
                    }
                } else {
                    Block {
                        exprs,
                        ret_last: false,
                    }
                }
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        binop::{Binop, BinopKind},
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
                "{yield 2.0;}",
                Block {
                    exprs: vec![Yield {
                        value: Box::new(Expr::Literal(Literal::Float(2.0))),
                    }
                    .into()],
                    ret_last: false,
                },
            ),
            (
                " { let two = 2; let three = 3e1; yield two / three; two } ",
                Block {
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
                            value: Box::new(Expr::Binop(Binop {
                                left: Box::new(Expr::Var(Var::new("two"))),
                                right: Box::new(Expr::Var(Var::new("three"))),
                                op: BinopKind::Div,
                            })),
                        }
                        .into(),
                        Expr::Var(Var::new("two")),
                    ],
                    ret_last: true,
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(Expr::parser(44100.0).parse(text), Ok(Expr::Block(expected)));
        }
    }
}
