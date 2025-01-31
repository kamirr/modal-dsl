use std::time::Instant;

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

    let now = Instant::now();
    let prog = Program::parser(44100.0).parse(text.as_str()).unwrap();
    println!("parsing took {:.2}ms", now.elapsed().as_secs_f32() * 1000.0);

    let now = Instant::now();
    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&prog).unwrap();
    println!(
        "compilation took {:.2}ms",
        now.elapsed().as_secs_f32() * 1000.0
    );

    println!("State mapping: {:#?}", compiled.state);

    compiled.init();
    for k in 0..12 {
        println!("[{k:02}]: {}", compiled.step());
    }
}
