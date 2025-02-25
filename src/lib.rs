
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
	MovA(u8),
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
	ExtUByte(Reg, Reg),
	ExtUWord(Reg, Reg),
	MacWord(Reg, Reg),
	MacLong(Reg, Reg),
	Neg(Reg, Reg),
	NegC(Reg, Reg),
	Not(Reg, Reg),
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
		Rule::addr_reg_or_sp => "indirect register or SP (ex: '@r1')",
		Rule::reg_post_inc => "indirect register or SP w/ post-increment (ex: '@r2+')",
		Rule::reg_pre_dec => "indirect register or SP w/ pre-decrement (ex: '@-r3')",
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
			Rule::ins_trapa  => {
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
			Rule::ins_extub => output.push(parse_ins_rs_rs(Asm::ExtUByte, src)?),
			Rule::ins_extuw => output.push(parse_ins_rs_rs(Asm::ExtUWord, src)?),
			Rule::ins_macw  => output.push(parse_ins_pi_pi(Asm::MacWord, src)?),
			Rule::ins_macl  => output.push(parse_ins_pi_pi(Asm::MacLong, src)?),
			Rule::ins_mova  => {
				let mut args = src.into_inner();
				let disp = parse_disp_pc(args.next().unwrap())?;
				output.push(Asm::MovA(disp as u8));
			}
			Rule::ins_neg  => output.push(parse_ins_rs_rs(Asm::Neg, src)?),
			Rule::ins_negc => output.push(parse_ins_rs_rs(Asm::NegC, src)?),
			Rule::ins_not  => output.push(parse_ins_rs_rs(Asm::Not, src)?),

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
fn parse_ins_pi_pi(f: fn(Reg,Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let src = parse_reg_post_inc(args.next().unwrap())?;
	let dst = parse_reg_post_inc(args.next().unwrap())?;
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

fn error_message(span: pest::Span, message: &str) -> Error<Rule> {
	Error::new_from_span(
		ErrorVariant::CustomError { message: message.to_string() },
		span,
	)
}

fn reg_or_sp(s: &str, err_msg: Error<Rule>) -> ParseResult<Reg> {
	eprintln!("{s}");
	if s == "sp" {
		Ok(15)
	} else if s.chars().next() == Some('r') {
		s[1..].parse::<Reg>().map_err(|_| err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(), "expected register or SP");
	reg_or_sp(source.as_str(), err_msg)
}

#[instrument]
fn parse_addr_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(), "expected indirect register or SP");
	if source.as_str().starts_with('@') {
		reg_or_sp(&source.as_str()[1..], err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_post_inc(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(),
		"expected indirect register or SP w/ post-increment");
	if source.as_str().starts_with('@')
	&& source.as_str().ends_with('+') {
		let len = source.as_str().len() - 1;
		reg_or_sp(&source.as_str()[1..len], err_msg)
	} else {
		eprintln!("1");
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_pre_dec(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(),
		"expected indirect register or SP w/ pre-decrement");
	if source.as_str().starts_with("@-") {
		reg_or_sp(&source.as_str()[2..], err_msg)
	} else {
		Err(err_msg)
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
		($name:ident, $input:expr, $val:expr, u8) => {
			test_single!($name, $input, $val, parse_u8);
		};
		($name:ident, $input:expr, $val:expr, i8) => {
			test_single!($name, $input, $val, parse_i8);
		};
		($name:ident, $input:expr, $val:expr, $parse:ident) => {
			#[test]
			fn $name() {
				let mut source = Sh2Parser::parse(Rule::num, $input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				let src = source.next().unwrap();
				let num = $parse(src)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				assert_eq!($val, num);
			}
		};
		($input:expr, $asm:expr) => {
			let out = super::parser($input)
				.map_err(|e| panic!("{e}"))
				.unwrap();
			assert_eq!(out.0, vec![$asm]);
		};
		($name:ident, $input:expr, $asm:expr) => {
			#[test]
			fn $name() {
				let out = super::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				assert_eq!(out.0, vec![$asm]);
			}
		};
	}

	test_single!(u8_hex,     "$C9",        201, u8);
	test_single!(u8_bin,     "%1100_1001", 201, u8);
	test_single!(i8_hex_pos, "$49",         73, i8);
	test_single!(i8_hex_neg, "$C9",        -55, i8);
	test_single!(i8_bin_pos, "%0100_1001",  73, i8);
	test_single!(i8_bin_neg, "%1100_1001", -55, i8);

	test_single!(clrmac, "\tclrmac", Asm::ClrMac);
	test_single!(clrt,   "\tclrt",   Asm::ClrT);
	test_single!(div0u,  "\tdiv0u",  Asm::Div0U);
	test_single!(nop,    "\tnop",    Asm::Nop);
	test_single!(rte,    "\trte",    Asm::Rte);
	test_single!(rts,    "\trts",    Asm::Rts);
	test_single!(sett,   "\tsett",   Asm::SetT);
	test_single!(sleep,  "\tsleep",  Asm::Sleep);

	test_single!(bf,     "\tbf @(34,pc)",       Asm::Bf(34));
	test_single!(bfs,    "\tbf/s @($C9,pc)",    Asm::BfS(-55));
	test_single!(bra,    "\tbra @($FFC,pc)",    Asm::Bra(-4));
	test_single!(braf,   "\tbraf @r3",          Asm::BraF(3));
	test_single!(bsr,    "\tbsr @($7FC,pc)",    Asm::Bsr(2044));
	test_single!(bsrf,   "\tbsrf @r0",          Asm::BsrF(0));
	test_single!(bt,     "\tbt @($07,pc)",      Asm::Bt(7));
	test_single!(bts,    "\tbt/s @(-12,pc)",    Asm::BtS(-12));
	test_single!(dt,     "\tdt r2",             Asm::Dt(2));
	test_single!(jmp,    "\tjmp @r7",           Asm::Jmp(7));
	test_single!(jsr,    "\tjsr @r9",           Asm::Jsr(9));
	test_single!(mova,   "\tmova @($57,pc),r0", Asm::MovA(0x57));
	test_single!(movt,   "\tmovt r0",           Asm::MovT(0));
	test_single!(rotcl,  "\trotcl r4",          Asm::RotCL(4));
	test_single!(rotcr,  "\trotcr r5",          Asm::RotCR(5));
	test_single!(rotl,   "\trotl r4",           Asm::RotL(4));
	test_single!(rotr,   "\trotr r5",           Asm::RotR(5));
	test_single!(shal,   "\tshal r10",          Asm::ShAL(10));
	test_single!(shar,   "\tshar r11",          Asm::ShAR(11));
	test_single!(shll,   "\tshll r12",          Asm::ShLL(12));
	test_single!(shll2,  "\tshll2 r15",         Asm::ShLL2(15));
	test_single!(shll8,  "\tshll8 r14",         Asm::ShLL8(14));
	test_single!(shll16, "\tshll16 r13",        Asm::ShLL16(13));
	test_single!(shlr,   "\tshlr r12",          Asm::ShLR(12));
	test_single!(shlr2,  "\tshlr2 r15",         Asm::ShLR2(15));
	test_single!(shlr8,  "\tshlr8 r14",         Asm::ShLR8(14));
	test_single!(shlr16, "\tshlr16 r13",        Asm::ShLR16(13));
	test_single!(tas,    "\ttas.b @r10",        Asm::TaS(10));
	test_single!(trapa,  "\ttrapa #$AA",        Asm::TrapA(170));

	test_single!(addc,  "\taddc r2,r7",      Asm::AddC(2, 7));
	test_single!(addv,  "\taddv r2,r7",      Asm::AddV(2, 7));
	test_single!(div0s, "\tdiv0s sp,r0",     Asm::Div0S(15, 0));
	test_single!(div1,  "\tdiv1 r1,r1",      Asm::Div1(1, 1));
	test_single!(extsb, "\texts.b sp,sp",    Asm::ExtSByte(15, 15));
	test_single!(extsw, "\texts.w sp,sp",    Asm::ExtSWord(15, 15));
	test_single!(extub, "\textu.b sp,sp",    Asm::ExtUByte(15, 15));
	test_single!(extuw, "\textu.w sp,sp",    Asm::ExtUWord(15, 15));
	test_single!(macw,  "\tmac.w @r3+,@r6+", Asm::MacWord(3, 6));
	test_single!(macl,  "\tmac.l @r6+,@r3+", Asm::MacLong(6, 3));
	test_single!(neg,   "\tneg r3,r0",       Asm::Neg(3, 0));
	test_single!(negc,  "\tnegc r7,r7",      Asm::NegC(7, 7));
	test_single!(not,   "\tnot r9,r0",       Asm::Not(9, 0));

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

			Asm::Bf(d)     => out.push(0x8B00 | *d as u16),
			Asm::BfS(d)    => out.push(0x8F00 | *d as u16),
			Asm::Bra(d)    => out.push(0xA000 | (*d & 0xFFF) as u16),
			Asm::BraF(r)   => out.push(0x0023 | (*r as u16) << 8),
			Asm::Bsr(d)    => out.push(0xB000 | (*d & 0xFFF) as u16),
			Asm::BsrF(r)   => out.push(0x0003 | (*r as u16) << 8),
			Asm::Bt(d)     => out.push(0x8900 | *d as u16),
			Asm::BtS(d)    => out.push(0x8D00 | *d as u16),
			Asm::Dt(r)     => out.push(0x4010 | (*r as u16) << 8),
			Asm::Jmp(r)    => out.push(0x402B | (*r as u16) << 8),
			Asm::Jsr(r)    => out.push(0x400B | (*r as u16) << 8),
			Asm::MovA(d)   => out.push(0xC700 | *d as u16),
			Asm::MovT(r)   => out.push(0x0029 | (*r as u16) << 8),
			Asm::RotCL(r)  => out.push(0x4044 | (*r as u16) << 8),
			Asm::RotCR(r)  => out.push(0x4045 | (*r as u16) << 8),
			Asm::RotL(r)   => out.push(0x4004 | (*r as u16) << 8),
			Asm::RotR(r)   => out.push(0x4005 | (*r as u16) << 8),
			Asm::ShAL(r)   => out.push(0x4020 | (*r as u16) << 8),
			Asm::ShAR(r)   => out.push(0x4021 | (*r as u16) << 8),
			Asm::ShLL(r)   => out.push(0x4000 | (*r as u16) << 8),
			Asm::ShLL2(r)  => out.push(0x4008 | (*r as u16) << 8),
			Asm::ShLL8(r)  => out.push(0x4018 | (*r as u16) << 8),
			Asm::ShLL16(r) => out.push(0x4028 | (*r as u16) << 8),
			Asm::ShLR(r)   => out.push(0x4001 | (*r as u16) << 8),
			Asm::ShLR2(r)  => out.push(0x4009 | (*r as u16) << 8),
			Asm::ShLR8(r)  => out.push(0x4019 | (*r as u16) << 8),
			Asm::ShLR16(r) => out.push(0x4029 | (*r as u16) << 8),
			Asm::TaS(r)    => out.push(0x401B | (*r as u16) << 8),
			Asm::TrapA(i)  => out.push(0xC300 | *i as u16),

			Asm::AddC(m,n)     => out.push(0x300E | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::AddV(m,n)     => out.push(0x300F | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Div0S(m,n)    => out.push(0x2007 | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Div1(m,n)     => out.push(0x3004 | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtSByte(m,n) => out.push(0x600E | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtSWord(m,n) => out.push(0x600F | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtUByte(m,n) => out.push(0x600C | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::ExtUWord(m,n) => out.push(0x600D | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::MacWord(m,n)  => out.push(0x400F | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::MacLong(m,n)  => out.push(0x000F | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Neg(m,n)      => out.push(0x600B | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::NegC(m,n)     => out.push(0x600A | (*n as u16) << 8 | (*m as u16) << 4),
			Asm::Not(m,n)      => out.push(0x6007 | (*n as u16) << 8 | (*m as u16) << 4),
		}
	}

	out.into_iter()
		.flat_map(|word: u16| word.to_be_bytes())
		.collect()
}

#[cfg(test)]
mod output {
	macro_rules! test_output {
		($name:ident, $input:expr, $bytes:expr) => {
			#[test]
			fn $name() {
				let asm = super::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				let out = super::output(&asm.0);
				assert_eq!(out, $bytes);
			}
		};
	}

	test_output!(clrmac, "\tclrmac", &[0x00, 0x28]);
	test_output!(clrt,   "\tclrt",   &[0x00, 0x08]);
	test_output!(div0u,  "\tdiv0u",  &[0x00, 0x19]);
	test_output!(nop,    "\tnop",    &[0x00, 0x09]);
	test_output!(rte,    "\trte",    &[0x00, 0x2B]);
	test_output!(rts,    "\trts",    &[0x00, 0x0B]);
	test_output!(sett,   "\tsett",   &[0x00, 0x18]);
	test_output!(sleep,  "\tsleep",  &[0x00, 0x1B]);

	test_output!(bf,     "\tbf @($78,pc)",      &[0x8B, 0x78]);
	test_output!(bfs,    "\tbf/s @($52,pc)",    &[0x8F, 0x52]);
	test_output!(bra,    "\tbra @(-2000,pc)",   &[0xA8, 0x30]);
	test_output!(braf,   "\tbraf @sp",          &[0x0F, 0x23]);
	test_output!(bsr,    "\tbsr @($63D,pc)",    &[0xB6, 0x3D]);
	test_output!(bsrf,   "\tbsrf @r8",          &[0x08, 0x03]);
	test_output!(bt,     "\tbt @(100,pc)",      &[0x89, 0x64]);
	test_output!(bts,    "\tbt/s @(%100,pc)",   &[0x8D, 0x04]);
	test_output!(dt,     "\tdt r10",            &[0x4A, 0x10]);
	test_output!(jmp,    "\tjmp @r13",          &[0x4D, 0x2B]);
	test_output!(jsr,    "\tjsr @r3",           &[0x43, 0x0B]);
	test_output!(mova,   "\tmova @($80,pc),r0", &[0xC7, 0x80]);
	test_output!(movt,   "\tmovt r6",           &[0x06, 0x29]);
	test_output!(rotcl,  "\trotcl r15",         &[0x4F, 0x44]);
	test_output!(rotcr,  "\trotcr r14",         &[0x4E, 0x45]);
	test_output!(rotl,   "\trotl r4",           &[0x44, 0x04]);
	test_output!(rotr,   "\trotr r5",           &[0x45, 0x05]);
	test_output!(shal,   "\tshal r8",           &[0x48, 0x20]);
	test_output!(shar,   "\tshar r9",           &[0x49, 0x21]);
	test_output!(shll,   "\tshll r0",           &[0x40, 0x00]);
	test_output!(shll2,  "\tshll2 r2",          &[0x42, 0x08]);
	test_output!(shll8,  "\tshll8 r8",          &[0x48, 0x18]);
	test_output!(shll16, "\tshll16 r15",        &[0x4F, 0x28]);
	test_output!(shlr,   "\tshlr r0",           &[0x40, 0x01]);
	test_output!(shlr2,  "\tshlr2 r2",          &[0x42, 0x09]);
	test_output!(shlr8,  "\tshlr8 r8",          &[0x48, 0x19]);
	test_output!(shlr16, "\tshlr16 r15",        &[0x4F, 0x29]);
	test_output!(tas,    "\ttas.b @sp",         &[0x4F, 0x1B]);
	test_output!(trapa,  "\ttrapa #240",        &[0xC3, 0xF0]);

	test_output!(addc,   "\taddc r12, r8",    &[0x38, 0xCE]);
	test_output!(addv,   "\taddv r4, r11",    &[0x3B, 0x4F]);
	test_output!(div0s,  "\tdiv0s r14,r13",   &[0x2D, 0xE7]);
	test_output!(div1,   "\tdiv1 r1,r1",      &[0x31, 0x14]);
	test_output!(extsb,  "\texts.b r15,r0",   &[0x60, 0xFE]);
	test_output!(extsw,  "\texts.w r15,r1",   &[0x61, 0xFF]);
	test_output!(extub,  "\textu.b r15,r0",   &[0x60, 0xFC]);
	test_output!(extuw,  "\textu.w r15,r1",   &[0x61, 0xFD]);
	test_output!(macw,   "\tmac.w @sp+,@r3+", &[0x43, 0xFF]);
	test_output!(macl,   "\tmac.l @sp+,@r3+", &[0x03, 0xFF]);
	test_output!(neg,    "\tneg r4,sp",       &[0x6F, 0x4B]);
	test_output!(negc,   "\tnegc sp,r8",      &[0x68, 0xFA]);
	test_output!(not,    "\tnot r6,r6",       &[0x66, 0x67]);
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

