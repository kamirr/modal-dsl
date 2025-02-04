use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_if() {
    let text = std::fs::read("examples/samples/if.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser().parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    let mut ready = compiled.init();

    ready.set_f32("a", -2.0);
    ready.set_f32("b", 2.0);

    ready.set_f32("sel", -0.01);
    assert_eq!(ready.step(), -2.0);

    ready.set_f32("sel", 42.0);
    assert_eq!(ready.step(), 2.0);
}
