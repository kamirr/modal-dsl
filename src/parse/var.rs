use chumsky::{error::Simple, Parser};

use super::path::Ident;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub name: Ident,
}

impl Var {
    #[cfg(test)]
    pub fn new(name: Ident) -> Self {
        Var { name }
    }

    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        Ident::parser().map(|name| Var { name })
    }
}
