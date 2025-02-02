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

	match Sh2Parser::parse(Rule::program, &input) {
		Err(e) => panic!("{e}"),
		Ok(_) => {}
	}
}

#[cfg(test)]
mod can_parse {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse($rule, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	fn org() { check!(Rule::dir_org, ".org $06004000") }

	#[test]
	fn add_reg() {
		check!(Rule::ins_add, "add r5,r12");
	}

	#[test]
	fn add_imm() {
		check!(Rule::ins_add, "add #30,r2");
		check!(Rule::ins_add, "add #$FE,r8");
		check!(Rule::ins_add, "add #%101,r8");
	}

	#[test]
	fn addc() { check!(Rule::ins_addc, "addc r3,r1") }

	#[test]
	fn addv() { check!(Rule::ins_addv, "addv r4,r12") }

	#[test]
	fn and() {
		check!(Rule::ins_and, "and r3,r14");
		check!(Rule::ins_and, "and #$2,r0");
		check!(Rule::ins_and, "and #%10,r0");
		check!(Rule::ins_and, "and #5,r0");
		check!(Rule::ins_and, "and.b #$32,@(r0,gbr)");
	}

	#[test]
	fn bf() { check!(Rule::ins_bf, "bf trget_f") }

	#[test]
	fn bfs() { check!(Rule::ins_bfs, "bf/s trget_f") }

	#[test]
	fn bra() { check!(Rule::ins_bra, "bra trget") }

	#[test]
	fn braf() { check!(Rule::ins_braf, "braf r4") }

	#[test]
	fn bsr() { check!(Rule::ins_bsr, "bsr trget") }

	#[test]
	fn bsrf() { check!(Rule::ins_bsrf, "bsrf r7") }

	#[test]
	fn bt() { check!(Rule::ins_bt, "bt TRGET") }

	#[test]
	fn bts() { check!(Rule::ins_bts, "bt/s TRGET") }

	#[test]
	fn clrmac() { check!(Rule::ins_clrmac, "clrmac") }

	#[test]
	fn clrt() { check!(Rule::ins_clrt, "clrt") }

	#[test]
	fn cmp() {
		check!(Rule::ins_cmp, "cmp/eq #$80,r0");
		check!(Rule::ins_cmp, "cmp/eq r4,r5");
		check!(Rule::ins_cmp, "cmp/ge r14,r15");
		check!(Rule::ins_cmp, "cmp/gt r10,r10");
		check!(Rule::ins_cmp, "cmp/hi sp,r8");
		check!(Rule::ins_cmp, "cmp/hs r7,r3");
		check!(Rule::ins_cmp, "cmp/pl r1");
		check!(Rule::ins_cmp, "cmp/pz r6");
		check!(Rule::ins_cmp, "cmp/str r6,r0");
	}

	#[test]
	fn div0s() { check!(Rule::ins_div0s, "div0s sp,sp") }

	#[test]
	fn div0u() { check!(Rule::ins_div0u, "div0u") }

	#[test]
	fn div1() { check!(Rule::ins_div1, "div1 r3,sp") }

	#[test]
	fn dmul() {
		check!(Rule::ins_dmul, "dmuls r1,r3");
		check!(Rule::ins_dmul, "dmuls.l r4,r7");
		check!(Rule::ins_dmul, "dmulu r1,r3");
		check!(Rule::ins_dmul, "dmulu.l r4,r7");
	}

	#[test]
	fn dt() { check!(Rule::ins_dt, "dt r14") }

	//#[test]
	//fn () { check!(Rule::ins_, "") }
}

#[cfg(test)]
mod examples {
	use super::*;

	macro_rules! check {
		($input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse(Rule::program, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	fn bsr() {
		check!("
	bsr TRGET
	MOV r3,r4
	add R0,R1
TRGET:
	mov r2,R3
	rts
	MOV #1,r0
");
	}

	#[test]
	fn bsrf() {
		check!("
	mov.l #(TRGET-BSRF_PC),r0
	bsrf r0
	mov r3,r4
BSRF_PC:
	add r0,r1
TRGET:
	mov r2,r3
	rts
	mov #1,r0
");
	}

	#[test]
	fn bt() {
		check!("
	sett
	bf TRGET_F
	bt TRGET_T
	nop
	nop
TRGET_T:
");
	}

	#[test]
	fn bts() {
		check!("
	sett
	bf/s trget_f
	nop
	bt/s trget_t
	add r0,r1
	nop
trget_t:
")
	}

	#[test]
	fn div_32_by_16_unsigned() {
		// R1 (32 bits) / R0 (16 bits) = R1 (16 bits) : Unsigned
		check!("
	shll16 r0    ; Upper 16 bits = divisor, lower 16 = 0
	tst r0,r0    ; Zero division check
	bt ZERO_DIV
	cmp/hs r0,r1 ; Overflow check
	bt OVER_DIV
	div0u        ; Flag initialization
	.arepeat 16
	div1 r0,r1   ; Repeat 16 times
	.aendr
	rotcl r1
	extu.w r1,r2 ; R1 = Quotient
")
	}

	#[test]
	fn div_64_by_32_unsigned() {
		// R1:R2 (64 bits) / R0 (32 bits) = R2 (32 bits) : Unsigned
		check!("
	tst r0,r0    ; Zero division check
	bt ZERO_DIV
	cmp/hs r0,r1 ; Overflow check
	bt OVER_DIV
	div0u        ; Flag initialization
	.arepeat 32
	rotcl r2     ; Repeat 32
	div1 r0,r1
	.aendr
	rotcl r2     ; R2 = Quotient
")
	}

	#[test]
	fn div_32_by_16_signed() {
		// R1 (32 bits) / R0 (16 bits) = R1 (16 bits) : Signed
		check!("
	SHLL16 R0    ; Upper 16 bits = divisor, lower 16 bits = 0
	EXTS.W R1,R1 ; Sign-extends the dividend to 32 bits
	XOR    R2,R2 ; R2 = 0
	MOV    R1,R2
	ROTCL  R3
	SUBC   R2,R1 ; Decrements if the dividend is negative
	DIV0S  R0,R1 ; Flag initialization
	.arepeat 16
	DIV1   R0,R1 ; Repeat 16 times
	.aendr
	EXTS.W R1,R1
	ROTCL  R1    ; R1 = quotient (1's complement)
	ADDC   R2,R1 ; Increments and takes the 2's complement if the MSB of the quotient is 1
	EXTS.W R1,R1 ; R1 = quotient (2's compliment)
")
	}

	#[test]
	fn div_64_by_32_signed() {
		// R2 (64 bits) / R0 (32 bits) = R2 (32 bits) : Signed
		check!("
	MOV   R2,R3
	ROTCL R3
	SUBC  R1,R1 ; Sign-extends the dividend to 64 bits (R1:R2)
	XOR   R3,R3 ; R3 = 0
	SUBC  R3,R2 ; Decrements and takes the 1's complement if the dividend is negative
	DIV0S R0,R1 ; Flag initialization
	.arepeat 32
	DIV1  R0,R1 ; Repeat 32 times
	.aendr
	ROTCL R2    ; R2 = Quotient (1's complement)
	ADDC  R3,R2 ; Increments and takes 2's complement if the MSB of the quotient is 1. R2 = Quotient (2's complement)
")
	}
}

#[cfg(test)]
mod invalid {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse($rule, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	#[should_panic]
	fn add_reg_no_pc_src() { check!(Rule::ins_add, "add pc,r2") }

	#[test]
	#[should_panic]
	fn add_reg_no_pc_dst() { check!(Rule::ins_add, "add r3,pc") }

	#[test]
	#[should_panic]
	fn bsrf_no_pc() { check!(Rule::ins_bsrf, "bsrf pc") }
}

