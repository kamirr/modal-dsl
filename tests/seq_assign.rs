use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_seq_assign() {
    let text = std::fs::read("examples/samples/seq_assign.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let mut compiled = compiler.compile(&prog).unwrap();

    compiled.init();
    assert_eq!(compiled.step(), 3.0);
}
