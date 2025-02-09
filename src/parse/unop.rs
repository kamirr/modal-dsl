use chumsky::{error::Simple, prelude::just, Parser};
use std::ops::Range;

use super::expr::Expr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnopKind {
    Deref,
    Neg,
    Not,
}

impl UnopKind {
    pub fn parser(self) -> impl Parser<char, Self, Error = Simple<char>> + Clone {
        just(self.as_str()).to(self)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            UnopKind::Deref => "*",
            UnopKind::Neg => "-",
            UnopKind::Not => "!",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unop {
    pub expr: Box<Expr>,
    pub op: UnopKind,
    pub span: Range<usize>,
}
