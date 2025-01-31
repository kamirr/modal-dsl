use chumsky::Parser;
use modal_dsl::{compile::Compiler, parse::Program};

fn main() {
    let text = std::fs::read("examples/42.rs").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    compiled.init();
    assert_eq!(compiled.step(), 42.0);
}
