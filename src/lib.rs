
use pest::Parser;
use pest::iterators::Pair;
use pest::error::{Error, ErrorVariant};
use pest_derive::Parser;

use tracing::{
	instrument,
	debug, error, info, trace, warn,
};

type Reg = u8;

type ParseResult<T> = Result<T, Error<Rule>>;

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
	ClrT,
	Div0U,
	Nop,
	Rte,
	Rts,
	SetT,
	Sleep,
	Bf(i8),
	BfS(i8),
	Bra(i16),
}

fn extra_rules(src: Pair<Rule>) {
	#[cfg(not(test))]
	error!("unexpected {src} - '{}'", src.as_str());

	#[cfg(test)]
	panic!("unexpected {src} - '{}'", src.as_str());
}

fn rename_rules(rule: &Rule) -> String {
	match rule {
		Rule::EOI => "EOF",
		Rule::ins_line | Rule::ins => "instruction",
		Rule::dir_line | Rule::dir => "assembler directive",
		Rule::lbl_line => "Label",
		Rule::val_line => "Value",
		Rule::bin => "a binary value",
		_ => unreachable!("{rule:?}"),
	}.to_owned()
}

#[instrument]
pub fn parser(input: &str) -> ParseResult<Output> {
	let mut output = Output::default();
	let source = Sh2Parser::parse(Rule::program, input)
		.map_err(|e| e.renamed_rules(rename_rules))?;
	for src in source {
		trace!("Parsing: {src} - '{}'", src.as_str());
		output = match src.as_rule() {
			Rule::ins_line => parse_ins_line(src, output)?,
			Rule::dir_line => todo!("{src} - '{}'", src.as_str()),
			Rule::lbl_line => todo!("{src} - '{}'", src.as_str()),
			Rule::val_line => todo!("{src} - '{}'", src.as_str()),
			Rule::EOI => continue,
			_ => {
				extra_rules(src);
				continue;
			}
		};
	}
	Ok(output)
}

#[instrument]
fn parse_ins_line(source: Pair<Rule>, mut output: Output) -> ParseResult<Output> {
	let source = source.into_inner().next().unwrap();
	for src in source.into_inner() {
		trace!("{src} - '{}'", src.as_str());

		match src.as_rule() {
			Rule::ins_clrmac => output.push(Asm::ClrMac),
			Rule::ins_clrt => output.push(Asm::ClrT),
			Rule::ins_div0u => output.push(Asm::Div0U),
			Rule::ins_nop => output.push(Asm::Nop),
			Rule::ins_rte => output.push(Asm::Rte),
			Rule::ins_rts => output.push(Asm::Rts),
			Rule::ins_sett => output.push(Asm::SetT),
			Rule::ins_sleep => output.push(Asm::Sleep),
			Rule::ins_bf => {
				let arg = src.into_inner().next().unwrap();
				let disp = parse_disp_pc(arg)?;
				output.push(Asm::Bf(disp));
			}
			Rule::ins_bfs => {
				let arg = src.into_inner().next().unwrap();
				let disp = parse_disp_pc(arg)?;
				output.push(Asm::BfS(disp));
			}
			Rule::ins_bra => {
				let arg = src.into_inner().next().unwrap();
				let mut inner_args = arg.into_inner();
				let disp = parse_i12(inner_args.next().unwrap())?;
				output.push(Asm::Bra(disp));
			}
			_ => {
				extra_rules(src);
				continue;
			}
		}
	}
	Ok(output)
}

#[instrument]
fn parse_disp_pc(source: Pair<Rule>) -> ParseResult<i8> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	parse_i8(args.next().unwrap())
}

#[instrument]
fn parse_u8(source: Pair<Rule>) -> ParseResult<u8> {
	trace!("{source} - '{}'", source.as_str());

	fn err_msg(base: &str) -> String {
		format!("expected a {base} value between {} and {}", u8::MIN, u8::MAX)
	}

	let s = source.as_str().replace('_', "");
	let num = match source.as_rule() {
		Rule::hex => u8::from_str_radix(&s, 16).map_err(|_| err_msg("hexadecimal")),
		Rule::bin => u8::from_str_radix(&s, 2).map_err(|_| err_msg("binary")),
		Rule::dec => s.parse::<u8>().map_err(|_| err_msg("decimal")),
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	num.map_err(|message| Error::new_from_span(
		ErrorVariant::CustomError { message },
		source.as_span(),
	))
}

#[instrument]
fn parse_i8(source: Pair<Rule>) -> ParseResult<i8> {
	trace!("{source} - '{}'", source.as_str());

	fn err_msg(base: &str) -> String {
		format!("expected a {base} value between {} and {}", i8::MIN, i8::MAX)
	}

	let s = source.as_str().replace('_', "");
	let num = match source.as_rule() {
		Rule::hex => {
			u8::from_str_radix(&s, 16)
				.map(|n| n as i8)
				.map_err(|_| err_msg("hexadecimal"))
		}
		Rule::bin => {
			u8::from_str_radix(&s, 2)
				.map(|n| n as i8)
				.map_err(|_| err_msg("binary"))
		}
		Rule::dec => {
			s.parse::<i8>()
				.map_err(|_| err_msg("decimal"))
		}
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	num.map_err(|message| Error::new_from_span(
		ErrorVariant::CustomError { message },
		source.as_span(),
	))
}

#[instrument]
fn parse_i12(source: Pair<Rule>) -> ParseResult<i16> {
	trace!("{source} - '{}'", source.as_str());

	fn fix_value(n: u16) -> i16 {
		eprintln!("fix({n})");
		if (n & 0x800) > 0 {
			(n | 0xF000) as i16
		} else {
			(n & 0x0FFF) as i16
		}
	}

	fn err_msg(src: &Pair<Rule>) -> String {
		format!(
			"expected a value between -2048 ($800) and 2047 ($7FF), found {}",
			src.as_str(),
		)
	}

	fn check_range(n: i16, src: &Pair<Rule>) -> Result<i16, String> {
		eprintln!("check({n})");
		if (-2048..2048).contains(&n) {
			Ok(n)
		} else {
			Err(err_msg(src))
		}
	}

	let s = source.as_str().replace('_', "");
	let result = match source.as_rule() {
		Rule::hex => u16::from_str_radix(&s, 16).map(fix_value),
		Rule::bin => u16::from_str_radix(&s, 2).map(fix_value),
		Rule::dec => s.parse::<i16>(),
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	result
		.map_err(|_| err_msg(&source))
		.and_then(|n| check_range(n, &source))
		.map_err(|message| Error::new_from_span(
			ErrorVariant::CustomError { message },
			source.as_span(),
		))
}

#[cfg(test)]
mod parser {
	use super::*;

	macro_rules! test_single {
		($input:expr, $val:expr, u8) => {
			test_single!($input, $val, parse_u8)
		};
		($input:expr, $val:expr, i8) => {
			test_single!($input, $val, parse_i8)
		};
		($input:expr, $val:expr, $parse:ident) => {
			let mut source = Sh2Parser::parse(Rule::num, $input)
				.map_err(|e| panic!("{e}"))
				.unwrap();
			let src = source.next().unwrap();
			let num = $parse(src)
				.map_err(|e| panic!("{e}"))
				.unwrap();
			assert_eq!($val, num);
		};
		($input:expr, $asm:expr) => {
			let out = super::parser($input)
				.map_err(|e| panic!("{e}"))
				.unwrap();
			assert_eq!(out.0, vec![$asm]);
		};
	}

	#[test]
	fn u8_hex() {
		test_single!("$C9", 201, u8);
	}

	#[test]
	fn u8_bin() {
		test_single!("%1100_1001", 201, u8);
	}

	#[test]
	fn i8_hex_pos() {
		test_single!("$49", 73, i8);
	}

	#[test]
	fn i8_hex_neg() {
		test_single!("$C9", -55, i8);
	}

	#[test]
	fn i8_bin_pos() {
		test_single!("%0100_1001", 73, i8);
	}

	#[test]
	fn i8_bin_neg() {
		test_single!("%1100_1001", -55, i8);
	}

	#[test]
	fn clrmac() {
		test_single!("\tclrmac", Asm::ClrMac);
	}

	#[test]
	fn clrt() {
		test_single!("\tclrt", Asm::ClrT);
	}

	#[test]
	fn div0u() {
		test_single!("\tdiv0u", Asm::Div0U);
	}

	#[test]
	fn nop() {
		test_single!("\tnop", Asm::Nop);
	}

	#[test]
	fn rte() {
		test_single!("\trte", Asm::Rte);
	}

	#[test]
	fn rts() {
		test_single!("\trts", Asm::Rts);
	}

	#[test]
	fn sett() {
		test_single!("\tsett", Asm::SetT);
	}

	#[test]
	fn sleep() {
		test_single!("\tsleep", Asm::Sleep);
	}

	#[test]
	fn bf() {
		test_single!("\tbf @(34,pc)", Asm::Bf(34));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bf @(243,pc)
  | 	     ^-^
  |
  = expected a decimal value between -128 and 127"]
	fn bf_too_far_forward() {
		test_single!("\tbf @(243,pc)", Asm::Bf(0));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bf @(-243,pc)
  | 	     ^--^
  |
  = expected a decimal value between -128 and 127"]
	fn bf_too_far_behind() {
		test_single!("\tbf @(-243,pc)", Asm::Bf(0));
	}

	#[test]
	fn bfs() {
		test_single!("\tbf/s @(%1100_1001,pc)", Asm::BfS(-55));
	}

	#[test]
	fn bra() {
		test_single!("\tbra @($FFC,pc)", Asm::Bra(-4));
	}

	#[test]
	#[should_panic = " --> 1:1
  |
1 | stuff
  | ^---
  |
  = expected EOF, instruction, assembler directive, Label, or Value"]
	fn failure_no_starting_tab() {
		test_single!("stuff", Asm::ClrT);
	}

	#[test]
	#[should_panic = " --> 1:2
  |
1 | 	stuff
  | 	^---
  |
  = expected instruction or assembler directive"]
	fn failure_non_command() {
		test_single!("\tstuff", Asm::ClrT);
	}
}

#[instrument]
fn output(asm: &[Asm]) -> Vec<u8> {
	let mut out = Vec::with_capacity(asm.len());
	for cmd in asm {
		match cmd {
			Asm::ClrT   => out.push(0x0008),
			Asm::Nop    => out.push(0x0009),
			Asm::Rts    => out.push(0x000B),
			Asm::SetT   => out.push(0x0018),
			Asm::Div0U  => out.push(0x0019),
			Asm::Sleep  => out.push(0x001B),
			Asm::ClrMac => out.push(0x0028),
			Asm::Rte    => out.push(0x002B),
			Asm::Bf(d)  => out.push(0x8B00 | *d as u16),
			Asm::BfS(d) => out.push(0x8F00 | *d as u16),
			Asm::Bra(d) => out.push(0xA000 | (*d & 0xFFF) as u16),
		}
	}
	out.into_iter()
		.flat_map(|word: u16| word.to_be_bytes())
		.collect()
}

#[cfg(test)]
mod output {
	use super::*;

	fn test_output(input: &str, bytes: &[u8]) {
		let asm = parser(input)
			.map_err(|e| panic!("{e}"))
			.unwrap();
		let out = output(&asm.0);
		assert_eq!(out, bytes);
	}

	#[test]
	fn clrmac() {
		test_output("\tclrmac", &[0x00, 0x28]);
	}

	#[test]
	fn clrt() {
		test_output("\tclrt", &[0x00, 0x08]);
	}

	#[test]
	fn div0u() {
		test_output("\tdiv0u", &[0x00, 0x19]);
	}

	#[test]
	fn nop() {
		test_output("\tnop", &[0x00, 0x09]);
	}

	#[test]
	fn rte() {
		test_output("\trte", &[0x00, 0x2B]);
	}

	#[test]
	fn rts() {
		test_output("\trts", &[0x00, 0x0B]);
	}

	#[test]
	fn sett() {
		test_output("\tsett", &[0x00, 0x18]);
	}

	#[test]
	fn sleep() {
		test_output("\tsleep", &[0x00, 0x1B]);
	}

	#[test]
	fn bf() {
		test_output("\tbf @($78,pc)", &[0x8B, 0x78]);
	}

	#[test]
	fn bfs() {
		test_output("\tbf/s @($52,pc)", &[0x8F, 0x52]);
	}

	#[test]
	fn bra() {
		// -2000 -> 0x708 -> 0x8F8
		test_output("\tbra @(-2000,pc)", &[0xA8, 0x30]);
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

