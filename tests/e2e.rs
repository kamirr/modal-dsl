use chumsky::Parser;
use modal_dsl::{
    compile::{library::stdlib::stdlib, Compiler},
    parse::Program,
};

fn do_test(path: &str, steps: Vec<(Vec<(&str, f32)>, f32)>) {
    let text = std::fs::read(path).unwrap();
    let text = String::from_utf8(text).unwrap();

    let prog = Program::parser().parse(text.as_str()).unwrap();
    let mut compiler = Compiler::new(stdlib()).unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    let mut ready = compiled.init();

    for (ins, out) in steps {
        for (name, value) in ins {
            ready.set_f32(name, value);
        }
        assert_eq!(ready.step(), out);
    }
}

#[test]
fn test_42() {
    do_test("examples/samples/42.modal", vec![(vec![], 42.0)]);
}

#[test]
fn test_delay3() {
    do_test(
        "examples/samples/delay3_2.modal",
        vec![
            (vec![("in", 1.0)], 0.0),
            (vec![("in", 2.0)], 0.0),
            (vec![("in", 3.0)], 0.0),
            (vec![], 1.0),
            (vec![], 2.0),
            (vec![], 3.0),
            (vec![], 3.0),
        ],
    );
}

#[test]
fn test_fib() {
    do_test(
        "examples/samples/fib.modal",
        vec![
            (vec![], 1.0),
            (vec![], 1.0),
            (vec![], 2.0),
            (vec![], 3.0),
            (vec![], 5.0),
            (vec![], 8.0),
            (vec![], 13.0),
        ],
    );
}

#[test]
fn test_if() {
    do_test(
        "examples/samples/if.modal",
        vec![
            (vec![("a", -2.0), ("b", 2.0), ("sel", -0.001)], -2.0),
            (vec![("sel", 42.0)], 2.0),
        ],
    );
}

#[test]
fn test_loop() {
    do_test(
        "examples/samples/loop.modal",
        vec![
            (vec![("arg", 3.0)], 2.0),
            (vec![("arg", 1024.0)], 10.0),
            (vec![("arg", 2000.0)], 11.0),
            (vec![("arg", 3000.0)], 12.0),
        ],
    );
}

#[test]
fn test_math() {
    let x = -2.0f32;
    let expected = x.sin() + x.cos() + x.tan() + x.abs().sqrt() + x.powf(2.0);
    do_test(
        "examples/samples/math.modal",
        vec![(vec![("in", x)], expected)],
    );
}

#[test]
fn test_seq_assign() {
    do_test("examples/samples/seq_assign.modal", vec![(vec![], 3.0)]);
}
