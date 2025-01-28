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
            .map(|((path, mut args), last)| Call {
                path,
                args: {
                    if let Some(last) = last {
                        args.push(last);
                    }
                    args
                },
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{literal::Literal, path::Ident};

    use super::*;

    #[test]
    fn test_call() {
        let cases = [
            (
                &["foo(0)", "foo ( 0 )"][..],
                Call {
                    path: Path(vec![Ident::new("foo")]),
                    args: vec![Expr::Literal(Literal::Float(0.0))],
                },
            ),
            (
                &["foo.bar.baz(21,33)", "foo.bar.baz ( 2.1e1, 33.0 ,)"],
                Call {
                    path: Path(vec![
                        Ident::new("foo"),
                        Ident::new("bar"),
                        Ident::new("baz"),
                    ]),
                    args: vec![
                        Expr::Literal(Literal::Float(21.0)),
                        Expr::Literal(Literal::Float(33.0)),
                    ],
                },
            ),
            (
                &["foo(bar(baz()))"],
                Call {
                    path: Path(vec![Ident::new("foo")]),
                    args: vec![Expr::Call(Call {
                        path: Path(vec![Ident::new("bar")]),
                        args: vec![Expr::Call(Call {
                            path: Path(vec![Ident::new("baz")]),
                            args: vec![],
                        })],
                    })],
                },
            ),
        ];

        for (texts, expected) in cases {
            for &text in texts {
                assert_eq!(
                    Expr::parser(44100.0).parse(text),
                    Ok(Expr::Call(expected.clone()))
                )
            }
        }
    }
}
