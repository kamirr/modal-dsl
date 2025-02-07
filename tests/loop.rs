use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

#[test]
fn test_loop() {
    let text = std::fs::read("examples/samples/loop.modal").unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser().parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    let mut ready = compiled.init();
    let cases = [(3.0, 2.0), (1024.0, 10.0), (2000.0, 11.0), (3000.0, 12.0)];
    for (arg, output) in cases {
        ready.set_f32("arg", arg);
        assert_eq!(ready.step(), output, "arg = {arg}");
    }
}
