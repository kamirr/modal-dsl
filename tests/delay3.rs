use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_delay3() {
    let text = std::fs::read("examples/samples/delay3.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser().parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    let mut ready = compiled.init();

    ready.set_f32("in", 1.0);
    assert_eq!(ready.step(), 0.0);
    ready.set_f32("in", 2.0);
    assert_eq!(ready.step(), 0.0);
    ready.set_f32("in", 3.0);
    assert_eq!(ready.step(), 0.0);
    assert_eq!(ready.step(), 1.0);
    assert_eq!(ready.step(), 2.0);
    assert_eq!(ready.step(), 3.0);
    assert_eq!(ready.step(), 3.0);
}
