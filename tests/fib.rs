use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_fib() {
    let text = std::fs::read("examples/samples/fib.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let mut compiled = compiler.compile(&prog).unwrap();

    compiled.init();
    let expected = [1.0, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0];
    for value in expected {
        assert_eq!(compiled.step(), value);
    }
}
