
use std::fs::read_to_string;

use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;

use tracing::{
	instrument,
	debug, error, info, trace, warn,
};

type Reg = u8;

type ParseResult<T> = Result<T, pest::error::Error<Rule>>;

#[derive(Parser)]
#[grammar = "../sh2.pest"]
struct Sh2Parser;

#[derive(Debug, Default)]
struct Output(Vec<Asm>);

impl Output {
	fn push(&mut self, asm: Asm) {
		self.0.push(asm);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Asm {
	ClrMac,
}

#[instrument]
pub fn parser(input: &str) -> ParseResult<Output> {
	let mut output = Output::default();
	let source = Sh2Parser::parse(Rule::program, input)?;
	for src in source {
		trace!("Parsing: {src} - '{}'", src.as_str());
		match src.as_rule() {
			Rule::ins_clrmac => {
				output.push(Asm::ClrMac);
			}
			_ => {
				error!("unexpected {src} - '{}'", src.as_str());
			}
		}
	}
	Ok(output)
}

#[cfg(test)]
mod parser {
	use super::*;

	#[test]
	fn clrmac() {
		let input = "\tclrmac";
		let out = parser(input)
			.map_err(|e| panic!("{e}"))
			.unwrap();
		assert_eq!(out.0, vec![
			Asm::ClrMac,
		]);
	}
}

#[instrument]
fn output(asm: &[Asm]) -> Vec<u8> {
	let mut out = Vec::with_capacity(asm.len());
	for cmd in asm {
		match cmd {
			Asm::ClrMac => out.extend(&[0x00, 0x00, 0x02, 0x80]),
		}
	}
	out
}

#[cfg(test)]
mod output {
	use super::*;

	#[test]
	fn clrmac() {
		let input = "\tclrmac";
		let asm = parser(input)
			.map_err(|e| panic!("{e}"))
			.unwrap();
		let out = output(&asm.0);
		assert_eq!(out, vec![
			0x00, 0x00, 0x02, 0x80,
		]);
	}
}

/*
#[instrument]
fn main() {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let _input = read_to_string(&source).expect("unable to read source file");
	let _target = args.next().unwrap_or("asm.out".to_string());
}
*/

