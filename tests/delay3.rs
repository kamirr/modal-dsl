use chumsky::Parser;
use modal_dsl::{compile::Compiler, parse::Program};

#[test]
fn test_delay3() {
    let text = std::fs::read("examples/delay3.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    compiled.init();

    compiled.set_input("in", 1.0);
    assert_eq!(compiled.step(), 0.0);
    compiled.set_input("in", 2.0);
    assert_eq!(compiled.step(), 0.0);
    compiled.set_input("in", 3.0);
    assert_eq!(compiled.step(), 0.0);
    assert_eq!(compiled.step(), 1.0);
    assert_eq!(compiled.step(), 2.0);
    assert_eq!(compiled.step(), 3.0);
    assert_eq!(compiled.step(), 3.0);
}
