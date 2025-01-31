use chumsky::Parser;
use modal_dsl::{compile::Compiler, parse::Program};

fn main() {
    let text = std::fs::read("examples/fib.rs").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    compiled.init();
    let expected = [1.0, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0];
    for value in expected {
        assert_eq!(compiled.step(), value);
    }
}
