use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_42() {
    let text = std::fs::read("examples/samples/math.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let mut compiled = compiler.compile(&prog).unwrap();

    compiled.init();

    let x = -2.0;
    compiled.set_f32("in", x);
    let expected = x.sin() + x.cos() + x.tan() + x.abs().sqrt() + x.powf(2.0);
    assert_eq!(compiled.step(), expected);
}
