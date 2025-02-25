
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
	BraF(Reg),
	Bsr(i16),
	BsrF(Reg),
	Bt(i8),
	BtS(i8),
	Dt(Reg),
	Jmp(Reg),
	Jsr(Reg),
	MovT(Reg),
	RotCL(Reg),
	RotCR(Reg),
	RotL(Reg),
	RotR(Reg),
	ShAL(Reg),
	ShAR(Reg),
	ShLL(Reg),
	ShLL2(Reg),
	ShLL8(Reg),
	ShLL16(Reg),
	ShLR(Reg),
	ShLR2(Reg),
	ShLR8(Reg),
	ShLR16(Reg),
	TaS(Reg),
	TrapA(u8),

	AddC(Reg, Reg),
	AddV(Reg, Reg),
	Div0S(Reg, Reg),
	Div1(Reg, Reg),
	ExtSByte(Reg, Reg),
	ExtSWord(Reg, Reg),
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
		Rule::sp => "SP register",
		Rule::reg => "general register",
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
			Rule::ins_clrt   => output.push(Asm::ClrT),
			Rule::ins_div0u  => output.push(Asm::Div0U),
			Rule::ins_nop    => output.push(Asm::Nop),
			Rule::ins_rte    => output.push(Asm::Rte),
			Rule::ins_rts    => output.push(Asm::Rts),
			Rule::ins_sett   => output.push(Asm::SetT),
			Rule::ins_sleep  => output.push(Asm::Sleep),
			Rule::ins_bf     => output.push(parse_ins_disp_pc(Asm::Bf, src)?),
			Rule::ins_bfs    => output.push(parse_ins_disp_pc(Asm::BfS, src)?),
			Rule::ins_bra    => output.push(parse_ins_disp_i12(Asm::Bra, src)?),
			Rule::ins_braf   => output.push(parse_ins_addr_reg_or_sp(Asm::BraF, src)?),
			Rule::ins_bsr    => output.push(parse_ins_disp_i12(Asm::Bsr, src)?),
			Rule::ins_bsrf   => output.push(parse_ins_addr_reg_or_sp(Asm::BsrF, src)?),
			Rule::ins_bt     => output.push(parse_ins_disp_pc(Asm::Bt, src)?),
			Rule::ins_bts    => output.push(parse_ins_disp_pc(Asm::BtS, src)?),
			Rule::ins_dt     => output.push(parse_ins_reg_or_sp(Asm::Dt, src)?),
			Rule::ins_jmp    => output.push(parse_ins_addr_reg_or_sp(Asm::Jmp, src)?),
			Rule::ins_jsr    => output.push(parse_ins_addr_reg_or_sp(Asm::Jsr, src)?),
			Rule::ins_movt   => output.push(parse_ins_reg_or_sp(Asm::MovT, src)?),
			Rule::ins_rotcl  => output.push(parse_ins_reg_or_sp(Asm::RotCL, src)?),
			Rule::ins_rotcr  => output.push(parse_ins_reg_or_sp(Asm::RotCR, src)?),
			Rule::ins_rotl   => output.push(parse_ins_reg_or_sp(Asm::RotL, src)?),
			Rule::ins_rotr   => output.push(parse_ins_reg_or_sp(Asm::RotR, src)?),
			Rule::ins_shal   => output.push(parse_ins_reg_or_sp(Asm::ShAL, src)?),
			Rule::ins_shar   => output.push(parse_ins_reg_or_sp(Asm::ShAR, src)?),
			Rule::ins_shll   => output.push(parse_ins_reg_or_sp(Asm::ShLL, src)?),
			Rule::ins_shll2  => output.push(parse_ins_reg_or_sp(Asm::ShLL2, src)?),
			Rule::ins_shll8  => output.push(parse_ins_reg_or_sp(Asm::ShLL8, src)?),
			Rule::ins_shll16 => output.push(parse_ins_reg_or_sp(Asm::ShLL16, src)?),
			Rule::ins_shlr   => output.push(parse_ins_reg_or_sp(Asm::ShLR, src)?),
			Rule::ins_shlr2  => output.push(parse_ins_reg_or_sp(Asm::ShLR2, src)?),
			Rule::ins_shlr8  => output.push(parse_ins_reg_or_sp(Asm::ShLR8, src)?),
			Rule::ins_shlr16 => output.push(parse_ins_reg_or_sp(Asm::ShLR16, src)?),
			Rule::ins_tas    => output.push(parse_ins_addr_reg_or_sp(Asm::TaS, src)?),
			Rule::ins_trapa => {
				let mut args = src.into_inner();
				let imm = parse_u8(args.next().unwrap())?;
				output.push(Asm::TrapA(imm));
			}
			Rule::ins_addc  => output.push(parse_ins_rs_rs(Asm::AddC, src)?),
			Rule::ins_addv  => output.push(parse_ins_rs_rs(Asm::AddV, src)?),
			Rule::ins_div0s => output.push(parse_ins_rs_rs(Asm::Div0S, src)?),
			Rule::ins_div1  => output.push(parse_ins_rs_rs(Asm::Div1, src)?),
			Rule::ins_extsb => output.push(parse_ins_rs_rs(Asm::ExtSByte, src)?),
			Rule::ins_extsw => output.push(parse_ins_rs_rs(Asm::ExtSWord, src)?),

			_ => {
				extra_rules(src);
				continue;
			}
		}
	}
	Ok(output)
}

#[instrument]
fn parse_ins_rs_rs(f: fn(Reg,Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let src = parse_reg_or_sp(args.next().unwrap())?;
	let dst = parse_reg_or_sp(args.next().unwrap())?;
	Ok(f(src, dst))
}

#[instrument]
fn parse_ins_disp_i12(f: fn(i16) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let inner_arg = arg.into_inner().next().unwrap();
	let disp = parse_i12(inner_arg)?;
	Ok(f(disp))
}

#[instrument]
fn parse_ins_disp_pc(f: fn(i8) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let disp = parse_disp_pc(arg)?;
	Ok(f(disp))
}

#[instrument]
fn parse_ins_reg_or_sp(f: fn(Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let reg = parse_reg_or_sp(arg)?;
	Ok(f(reg))
}

#[instrument]
fn parse_ins_addr_reg_or_sp(f: fn(Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let reg = parse_addr_reg_or_sp(arg)?;
	Ok(f(reg))
}

#[instrument]
fn parse_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let s = source.as_str();
	if s.chars().next() == Some('r') {
		s[1..].parse::<Reg>()
			.map_err(|_| Error::new_from_span(
				ErrorVariant::CustomError {
					message: format!("expected register (ex: r4), found {}", source.as_str()),
				},
				source.as_span(),
			))
	} else if s == "sp" {
		Ok(15)
	} else {
		Err(Error::new_from_span(
			ErrorVariant::CustomError {
				message: format!("expected register or SP, found {}", source.as_str()),
			},
			source.as_span(),
		))
	}
}

#[instrument]
fn parse_addr_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let s = &source.as_str()[1..];
	if s.chars().next() == Some('r') {
		s[1..].parse::<Reg>()
			.map_err(|_| Error::new_from_span(
				ErrorVariant::CustomError {
					message: format!("expected indirect register (ex: @r4), found {}", source.as_str()),
				},
				source.as_span(),
			))
	} else if s == "sp" {
		Ok(15)
	} else {
		Err(Error::new_from_span(
			ErrorVariant::CustomError {
				message: format!("expected indirect register or SP, found {}", source.as_str()),
			},
			source.as_span(),
		))
	}
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
	fn braf() {
		test_single!("\tbraf @r3", Asm::BraF(3));
	}

	#[test]
	fn bsr() {
		test_single!("\tbsr @($7FC,pc)", Asm::Bsr(2044));
	}

	#[test]
	fn bsrf() {
		test_single!("\tbsrf @r0", Asm::BsrF(0));
	}

	#[test]
	fn bt() {
		test_single!("\tbt @($07,pc)", Asm::Bt(7));
	}

	#[test]
	fn bts() {
		test_single!("\tbt/s @(-12,pc)", Asm::BtS(-12));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bt @(243,pc)
  | 	     ^-^
  |
  = expected a decimal value between -128 and 127"]
	fn bt_too_far_forward() {
		test_single!("\tbt @(243,pc)", Asm::Bt(0));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bt @(-243,pc)
  | 	     ^--^
  |
  = expected a decimal value between -128 and 127"]
	fn bt_too_far_behind() {
		test_single!("\tbt @(-243,pc)", Asm::Bt(0));
	}

	#[test]
	fn dt() {
		test_single!("\tdt r2", Asm::Dt(2));
	}

	#[test]
	fn jmp() {
		test_single!("\tjmp @r7", Asm::Jmp(7));
	}

	#[test]
	fn jsr() {
		test_single!("\tjsr @r9", Asm::Jsr(9));
	}

	#[test]
	fn movt() {
		test_single!("\tmovt r0", Asm::MovT(0));
	}

	#[test]
	fn rotcl() {
		test_single!("\trotcl r4", Asm::RotCL(4));
	}

	#[test]
	fn rotcr() {
		test_single!("\trotcr r5", Asm::RotCR(5));
	}

	#[test]
	fn rotl() {
		test_single!("\trotl r4", Asm::RotL(4));
	}

	#[test]
	fn rotr() {
		test_single!("\trotr r5", Asm::RotR(5));
	}

	#[test]
	fn shal() {
		test_single!("\tshal r10", Asm::ShAL(10));
	}

	#[test]
	fn shar() {
		test_single!("\tshar r11", Asm::ShAR(11));
	}

	#[test]
	fn shll() {
		test_single!("\tshll r12", Asm::ShLL(12));
	}

	#[test]
	fn shll2() {
		test_single!("\tshll2 r15", Asm::ShLL2(15));
	}

	#[test]
	fn shll8() {
		test_single!("\tshll8 r14", Asm::ShLL8(14));
	}

	#[test]
	fn shll16() {
		test_single!("\tshll16 r13", Asm::ShLL16(13));
	}

	#[test]
	fn shlr() {
		test_single!("\tshlr r12", Asm::ShLR(12));
	}

	#[test]
	fn shlr2() {
		test_single!("\tshlr2 r15", Asm::ShLR2(15));
	}

	#[test]
	fn shlr8() {
		test_single!("\tshlr8 r14", Asm::ShLR8(14));
	}

	#[test]
	fn shlr16() {
		test_single!("\tshlr16 r13", Asm::ShLR16(13));
	}

	#[test]
	fn tas() {
		test_single!("\ttas.b @r10", Asm::TaS(10));
	}

	#[test]
	fn trapa() {
		test_single!("\ttrapa #$AA", Asm::TrapA(170));
	}

	#[test]
	fn addc() {
		test_single!("\taddc r2,r7", Asm::AddC(2, 7));
	}

	#[test]
	fn addv() {
		test_single!("\taddv r2,r7", Asm::AddV(2, 7));
	}

	#[test]
	fn div0s() {
		test_single!("\tdiv0s sp,r0", Asm::Div0S(15, 0));
	}

	#[test]
	fn div1() {
		test_single!("\tdiv1 r1,r1", Asm::Div1(1, 1));
	}

	#[test]
	fn extsb() {
		test_single!("\texts.b sp,sp", Asm::ExtSByte(15, 15));
	}

	#[test]
	fn extsw() {
		test_single!("\texts.w sp,sp", Asm::ExtSWord(15, 15));
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
			Asm::ClrT       => out.push(0x0008),
			Asm::Nop        => out.push(0x0009),
			Asm::Rts        => out.push(0x000B),
			Asm::SetT       => out.push(0x0018),
			Asm::Div0U      => out.push(0x0019),
			Asm::Sleep      => out.push(0x001B),
			Asm::ClrMac     => out.push(0x0028),
			Asm::Rte        => out.push(0x002B),
			Asm::Bf(d)      => out.push(0x8B00 | *d as u16),
			Asm::BfS(d)     => out.push(0x8F00 | *d as u16),
			Asm::Bra(d)     => out.push(0xA000 | (*d & 0xFFF) as u16),
			Asm::BraF(r)    => out.push(0x0023 | (*r as u16) << 8),
			Asm::Bsr(d)     => out.push(0xB000 | (*d & 0xFFF) as u16),
			Asm::BsrF(r)    => out.push(0x0003 | (*r as u16) << 8),
			Asm::Bt(d)      => out.push(0x8900 | *d as u16),
			Asm::BtS(d)     => out.push(0x8D00 | *d as u16),
			Asm::Dt(r)      => out.push(0x4010 | (*r as u16) << 8),
			Asm::Jmp(r)     => out.push(0x402B | (*r as u16) << 8),
			Asm::Jsr(r)     => out.push(0x400B | (*r as u16) << 8),
			Asm::MovT(r)    => out.push(0x0029 | (*r as u16) << 8),
			Asm::RotCL(r)   => out.push(0x4044 | (*r as u16) << 8),
			Asm::RotCR(r)   => out.push(0x4045 | (*r as u16) << 8),
			Asm::RotL(r)    => out.push(0x4004 | (*r as u16) << 8),
			Asm::RotR(r)    => out.push(0x4005 | (*r as u16) << 8),
			Asm::ShAL(r)    => out.push(0x4020 | (*r as u16) << 8),
			Asm::ShAR(r)    => out.push(0x4021 | (*r as u16) << 8),
			Asm::ShLL(r)    => out.push(0x4000 | (*r as u16) << 8),
			Asm::ShLL2(r)   => out.push(0x4008 | (*r as u16) << 8),
			Asm::ShLL8(r)   => out.push(0x4018 | (*r as u16) << 8),
			Asm::ShLL16(r)  => out.push(0x4028 | (*r as u16) << 8),
			Asm::ShLR(r)    => out.push(0x4001 | (*r as u16) << 8),
			Asm::ShLR2(r)   => out.push(0x4009 | (*r as u16) << 8),
			Asm::ShLR8(r)   => out.push(0x4019 | (*r as u16) << 8),
			Asm::ShLR16(r)  => out.push(0x4029 | (*r as u16) << 8),
			Asm::TaS(r)     => out.push(0x401B | (*r as u16) << 8),
			Asm::TrapA(i)   => out.push(0xC300 | *i as u16),
			Asm::AddC(m,n)  => out.push(0x300E | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::AddV(m,n)  => out.push(0x300F | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Div0S(m,n) => out.push(0x2007 | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Div1(m,n)  => out.push(0x3004 | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtSByte(m,n) => out.push(0x600E | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtSWord(m,n) => out.push(0x600F | (*n as u16) << 8 | (*m as u16) << 4),
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

	#[test]
	fn braf() {
		test_output("\tbraf @sp", &[0x0F, 0x23]);
	}

	#[test]
	fn bsr() {
		test_output("\tbsr @(%0110_0011_1101,pc)", &[0xB6, 0x3D]);
	}

	#[test]
	fn bsrf() {
		test_output("\tbsrf @r8", &[0x08, 0x03]);
	}

	#[test]
	fn bt() {
		test_output("\tbt @(100,pc)", &[0x89, 0x64]);
	}

	#[test]
	fn bts() {
		test_output("\tbt/s @(%100,pc)", &[0x8D, 0x04]);
	}

	#[test]
	fn dt() {
		test_output("\tdt r10", &[0x4A, 0x10]);
	}

	#[test]
	fn jmp() {
		test_output("\tjmp @r13", &[0x4D, 0x2B]);
	}

	#[test]
	fn jsr() {
		test_output("\tjsr @r3", &[0x43, 0x0B]);
	}

	#[test]
	fn movt() {
		test_output("\tmovt r6", &[0x06, 0x29]);
	}

	#[test]
	fn rotcl() {
		test_output("\trotcl r15", &[0x4F, 0x44]);
	}

	#[test]
	fn rotcr() {
		test_output("\trotcr r14", &[0x4E, 0x45]);
	}

	#[test]
	fn rotl() {
		test_output("\trotl r4", &[0x44, 0x04]);
	}

	#[test]
	fn rotr() {
		test_output("\trotr r5", &[0x45, 0x05]);
	}

	#[test]
	fn shal() {
		test_output("\tshal r8", &[0x48, 0x20]);
	}

	#[test]
	fn shar() {
		test_output("\tshar r9", &[0x49, 0x21]);
	}

	#[test]
	fn shll() {
		test_output("\tshll r0", &[0x40, 0x00]);
	}

	#[test]
	fn shll2() {
		test_output("\tshll2 r2", &[0x42, 0x08]);
	}

	#[test]
	fn shll8() {
		test_output("\tshll8 r8", &[0x48, 0x18]);
	}

	#[test]
	fn shll16() {
		test_output("\tshll16 r15", &[0x4F, 0x28]);
	}

	#[test]
	fn shlr() {
		test_output("\tshlr r0", &[0x40, 0x01]);
	}

	#[test]
	fn shlr2() {
		test_output("\tshlr2 r2", &[0x42, 0x09]);
	}

	#[test]
	fn shlr8() {
		test_output("\tshlr8 r8", &[0x48, 0x19]);
	}

	#[test]
	fn shlr16() {
		test_output("\tshlr16 r15", &[0x4F, 0x29]);
	}

	#[test]
	fn tas() {
		test_output("\ttas.b @sp", &[0x4F, 0x1B]);
	}

	#[test]
	fn trapa() {
		test_output("\ttrapa #240", &[0xC3, 0xF0]);
	}

	#[test]
	fn addc() {
		test_output("\taddc r12, r8", &[0x38, 0xCE]);
	}

	#[test]
	fn addv() {
		test_output("\taddv r4, r11", &[0x3B, 0x4F]);
	}

	#[test]
	fn div0s() {
		test_output("\tdiv0s r14,r13", &[0x2D, 0xE7]);
	}

	#[test]
	fn div1() {
		test_output("\tdiv1 r1,r1", &[0x31, 0x14]);
	}

	#[test]
	fn extsb() {
		test_output("\texts.b r15,r0", &[0x60, 0xFE]);
	}

	#[test]
	fn extsw() {
		test_output("\texts.w r15,r1", &[0x61, 0xFF]);
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

