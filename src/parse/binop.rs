use std::ops::Range;

use chumsky::{error::Simple, prelude::just, Parser};

use super::expr::Expr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinopKind {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
}

impl BinopKind {
    pub fn parser(self) -> impl Parser<char, Self, Error = Simple<char>> + Clone {
        just(self.as_str()).to(self)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            BinopKind::Add => "+",
            BinopKind::Sub => "-",
            BinopKind::Mul => "*",
            BinopKind::Div => "/",
            BinopKind::Assign => "=",
        }
    }

    pub fn apply(self, lhs: Expr, rhs: Expr) -> Binop {
        let lspan = lhs.span();
        let rspan = rhs.span();
        Binop {
            left: Box::new(lhs),
            right: Box::new(rhs),
            op: self,
            span: lspan.start.min(rspan.start)..lspan.end.max(rspan.end),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binop {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: BinopKind,
    pub span: Range<usize>,
}
