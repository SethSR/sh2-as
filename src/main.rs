/*
use std::collections::HashMap;

use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::TokenType;

mod parser;
use parser::parser;
use parser::State;

mod resolver;
use resolver::resolver;

type Label = std::rc::Rc<str>;
type SectionMap = HashMap<u64, Vec<State>>;
type LabelMap = HashMap<Label, Option<u32>>;

fn main() -> miette::Result<()> {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let target = args.next().unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = true;

	let file = std::fs::read_to_string(source).into_diagnostic()?;

	let tokens = match lexer(&file) {
		Ok(tokens) => tokens,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	for token in &tokens {
		if token.get_type() == TokenType::IdUnknown || !is_silent {
			println!("{token:?}");
		}
	}

	let mut data = match parser(&tokens) {
		Ok(tables) => tables,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	if !is_silent {
		println!("State: initial");
		println!("{data:?}");
	}

	let limit = 10;
	for _ in 0..limit {
		if resolver(&mut data) {
			break;
		}
	}

	if !is_silent {
		println!("State: final");
		println!("{data:?}");
	}

	// TODO - srenshaw - This is just for debugging purposes. This is not the "real" output!
	for (_, section) in data.sections {
		let output = section
			.iter()
			.map(|state| state.completed_or(0xDEAD))
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output).into_diagnostic()?;
	}

	Ok(())
}
*/

use std::fs::read_to_string;

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sh2.pest"]
struct Sh2Parser;

fn main() {
	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let input = read_to_string(&source).expect("unable to read source file");

	let output = Sh2Parser::parse(Rule::program, &input);
	println!("{output:?}");
}

#[cfg(test)]
mod can_parse {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = Sh2Parser::parse($rule, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	fn org() {
		check!(Rule::dir_org, "org $06004000");
	}

	#[test]
	fn add_reg() {
		check!(Rule::ins_add, "add r3,pc");
		check!(Rule::ins_add, "add r5,r12");
	}

	#[test]
	fn add_imm() {
		check!(Rule::ins_add, "add #30,r2");
		check!(Rule::ins_add, "add #$FE,r8");
		check!(Rule::ins_add, "add #%101,r8");
	}

	#[test]
	fn addc() {
		check!(Rule::ins_addc, "addc r3,r1");
	}

	#[test]
	fn addv() {
		check!(Rule::ins_addv, "addv r4,r12");
	}

	#[test]
	fn and() {
		check!(Rule::ins_and, "and r3,r14");
		check!(Rule::ins_and, "and #$2,r0");
		check!(Rule::ins_and, "and #%10,r0");
		check!(Rule::ins_and, "and #5,r0");
		check!(Rule::ins_and, "and.b #$32,@(r0,gbr)");
	}

	#[test]
	fn bf() {
		check!(Rule::ins_bf, "bf trget_f");
	}

	#[test]
	fn bfs() {
		check!(Rule::ins_bf, "bf/s trget_f");
	}

	#[test]
	fn bra() {
		check!(Rule::ins_bra, "bra trget");
	}

	#[test]
	fn braf() {
		check!(Rule::ins_braf, "braf r4");
	}
}

