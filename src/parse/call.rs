use std::ops::Range;

use chumsky::{
    error::Simple,
    prelude::{just, Recursive},
    text::whitespace,
    Parser,
};

use super::{expr::Expr, path::Path};

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub path: Path,
    pub args: Vec<Expr>,
    pub span: Range<usize>,
}

impl Call {
    pub fn parser(
        expr: Recursive<'_, char, Expr, Simple<char>>,
    ) -> impl Parser<char, Self, Error = Simple<char>> + '_ {
        Path::parser()
            .then_ignore(whitespace())
            .then_ignore(just("("))
            .then(
                whitespace()
                    .ignored()
                    .then(expr.clone())
                    .then_ignore(whitespace())
                    .then_ignore(just(","))
                    .map(|((), expr)| expr)
                    .repeated(),
            )
            .then_ignore(whitespace())
            .then(expr.or_not())
            .then_ignore(whitespace())
            .then_ignore(just(",").or_not())
            .then_ignore(whitespace())
            .then_ignore(just(")"))
            .map_with_span(|((path, args), last), span| (path, args, last, span))
            .map(|(path, mut args, last, span)| Call {
                path,
                args: {
                    if let Some(last) = last {
                        args.push(last);
                    }
                    args
                },
                span,
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        literal::{Literal, LiteralValue},
        path::Ident,
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_call() {
        let cases = [
            (
                &["foo(0)"][..],
                Call {
                    path: Path(vec![Ident::new("foo", 0..3)]),
                    args: vec![Expr::Literal(Literal {
                        value: LiteralValue::Float(0.0),
                        span: 4..5,
                    })],
                    span: 0..6,
                },
            ),
            (
                &["foo ( 0 )"][..],
                Call {
                    path: Path(vec![Ident::new("foo", 0..3)]),
                    args: vec![Expr::Literal(Literal {
                        value: LiteralValue::Float(0.0),
                        span: 6..7,
                    })],
                    span: 0..9,
                },
            ),
            (
                &["foo.bar.baz(21,33)"],
                Call {
                    path: Path(vec![
                        Ident::new("foo", 0..3),
                        Ident::new("bar", 4..7),
                        Ident::new("baz", 8..11),
                    ]),
                    args: vec![
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(21.0),
                            span: 12..14,
                        }),
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(33.0),
                            span: 15..17,
                        }),
                    ],
                    span: 0..18,
                },
            ),
            (
                &["foo.bar.baz ( 2.1e1, 33.0 ,)"],
                Call {
                    path: Path(vec![
                        Ident::new("foo", 0..3),
                        Ident::new("bar", 4..7),
                        Ident::new("baz", 8..11),
                    ]),
                    args: vec![
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(21.0),
                            span: 14..19,
                        }),
                        Expr::Literal(Literal {
                            value: LiteralValue::Float(33.0),
                            span: 21..25,
                        }),
                    ],
                    span: 0..28,
                },
            ),
            (
                &["foo(bar(baz()))"],
                Call {
                    path: Path(vec![Ident::new("foo", 0..3)]),
                    args: vec![Expr::Call(Call {
                        path: Path(vec![Ident::new("bar", 4..7)]),
                        args: vec![Expr::Call(Call {
                            path: Path(vec![Ident::new("baz", 8..11)]),
                            args: vec![],
                            span: 8..13,
                        })],
                        span: 4..14,
                    })],
                    span: 0..15,
                },
            ),
        ];

        for (texts, expected) in cases {
            for &text in texts {
                assert_eq!(
                    Expr::parser().parse(text),
                    Ok(Expr::Call(expected.clone()))
                )
            }
        }
    }
}
