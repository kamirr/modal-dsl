use chumsky::Parser;
use compile::Compiler;
use parse::Program;

mod compile;
mod parse;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let text = std::fs::read(&args[1]).unwrap();
    let text = String::from_utf8(text).unwrap();

    println!("{text}");

    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();

    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&prog).unwrap();

    println!("State mapping: {:#?}", compiled.state);

    compiled.init();
    for k in 0..12 {
        println!("[{k:02}]: {}", compiled.step());
    }
}
