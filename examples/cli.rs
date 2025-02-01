use std::time::Instant;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::Simple;
use chumsky::Parser;
use modal_dsl::compile::Compiler;
use modal_dsl::parse::Program;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let text = std::fs::read(&args[1]).unwrap();
    let src = String::from_utf8(text).unwrap();

    println!("{src}");

    let now = Instant::now();
    let (program, errs) = Program::parser(44100.0).parse_recovery(src.clone());
    report_errors(&errs, &src);

    println!("parsing took {:.2}ms", now.elapsed().as_secs_f32() * 1000.0);

    let now = Instant::now();
    let mut compiler = Compiler::new().unwrap();
    let compiled = compiler.compile(&program.unwrap()).unwrap();
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

fn report_errors(errs: &[Simple<char>], src: &str) {
    errs.into_iter().for_each(|e| {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => expected.to_string(),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                },
            )
        };

        let report = Report::build(ReportKind::Error, e.span())
            .with_code(3)
            .with_message(msg)
            .with_label(
                Label::new(e.span())
                    .with_message(match e.reason() {
                        chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
                        _ => format!(
                            "Unexpected {}",
                            e.found()
                                .map(|c| format!("token {}", c.fg(Color::Red)))
                                .unwrap_or_else(|| "end of input".to_string())
                        ),
                    })
                    .with_color(Color::Red),
            );

        let report = match e.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report.with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_color(Color::Yellow),
            ),
            chumsky::error::SimpleReason::Unexpected => report,
            chumsky::error::SimpleReason::Custom(_) => report,
        };

        report.finish().print(Source::from(&src)).unwrap();
    });
}
