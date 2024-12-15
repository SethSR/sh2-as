
use std::collections::HashMap;

use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::{Token, TokenType};

fn main() -> miette::Result<()> {
	let mut args = std::env::args();
	args.next();

	let source = args.next()
		.expect("missing source file");
	let target = args.next()
		.unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = true;

	let file = std::fs::read_to_string(source)
		.into_diagnostic()?;

	let tokens = lexer(&file);

	for token in &tokens {
		let out = token.to_debug_string(&file);
		if token.get_type() == TokenType::Unknown || !is_silent {
			println!("{out}");
		}
	}

	let (mut section_table, mut label_table) = parser(&file, &tokens)?;

	if !is_silent {
		for (address, section) in &section_table {
			println!("Address: ${address:08X}");
			for instr in section {
				println!("\t{instr:?}");
			}
		}

		println!("Labels:");
		for (label,_) in &label_table {
			println!("\t{label}");
		}
	}

	let limit = 10;
	for _ in 0..limit {
		if resolver(&mut section_table, &mut label_table) {
			break;
		}
	}

	for (address, section) in &section_table {
		println!("Address: ${address:08X}");
		for instr in section {
			println!("  {instr:?}");
		}
	}

	println!("Labels:");
	for (label,address) in &label_table {
		println!("  {label}: {address:08X?}");
	}

	// TODO - srenshaw - This is just for debugging purposes. This is not the "real" output!
	for (_, section) in section_table {
		let output = section.iter()
			.map(|state| match state {
				State::Complete(word) => *word,
				State::Incomplete(_) => 0xDEAD,
			})
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output)
			.into_diagnostic()?;
	}

	Ok(())
}

/* Parser */

type Reg = u8;

#[derive(Clone)]
enum Arg {
	DirImm(i64),
	DirReg(Reg),
	DispR0(Reg),
	DispReg(i8,Reg),
	DispPC(i8),
	DispGBR(i8),
	DispLabel(String,Reg),
	IndReg(Reg),
	Label(String),
	PostInc(Reg),
	PreDec(Reg),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self,
		fmt: &mut std::fmt::Formatter,
	) -> std::fmt::Result {
		match self {
			Arg::DirImm(imm) =>
				write!(fmt, "DirImm(${imm:08X})"),
			Arg::DirReg(reg) =>
				write!(fmt, "DirReg(R{reg})"),
			Arg::DispR0(reg) =>
				write!(fmt, "DispR0(R{reg})"),
			Arg::DispReg(disp,reg) =>
				write!(fmt, "DispReg({disp},R{reg})"),
			Arg::DispPC(disp) =>
				write!(fmt, "DispPC({disp})"),
			Arg::DispGBR(disp) =>
				write!(fmt, "DispGBR({disp})"),
			Arg::DispLabel(lbl,reg) =>
				write!(fmt, "DispLabel({lbl},R{reg})"),
			Arg::IndReg(reg) =>
				write!(fmt, "IndReg(R{reg})"),
			Arg::Label(lbl) =>
				write!(fmt, "Label({lbl})"),
			Arg::PostInc(reg) =>
				write!(fmt, "PostInc(R{reg})"),
			Arg::PreDec(reg) =>
				write!(fmt, "PreDec(R{reg})"),
		}
	}
}

#[derive(Debug,Clone,Copy)]
enum Size {
	Byte,
	Word,
	Long,
}

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
//    | Instruction    | Bit Layout       | Operation          | Cycles  | T Bit  |
pub(crate) enum Ins {
	/// | #imm,Rn        | 0111nnnniiiiiiii | Rn+imm -> Rn       | 1       | -      |
	ADD_Imm(i8,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1100 | Rn+Rm -> Rn        | 1       | -      |
	ADD_Reg(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1110 | Rn+Rm+T -> Rn,     | 1       | Carry  |
	/// |                |                  | Carry -> T         |         |        |
	ADDC(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1111 | Rn+Rm -> Rn,       | 1       | Over   |
	/// |                |                  | Overflow -> T      |         |        |
	ADDV(Reg,Reg),
	/// | #imm,R0        | 11001001iiiiiiii | R0 & imm -> R0     | 1       | -      |
	AND_Imm(i8),
	/// | Rm,Rn          | 0010nnnnmmmm1001 | Rn & Rm -> Rn      | 1       | -      |
	AND_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR) | 11001101iiiiiiii | (R0+GBR) & imm     | 3       | -      |
	/// |                |                  | -> (R0+GBR)        |         |        |
	AND_Byte(i8),
	/// | label          | 10011011dddddddd | if T = 0,          | 3/1     | -      |
	/// |                |                  | dispx2+PC -> PC;   |         |        |
	/// |                |                  | if T = 1, nop      |         |        |
	BF(String),
	/// | label          | 10001111dddddddd | if T = 0,          | 2/1     | -      |
	/// |                |                  | dispx2+PC -> PC;   |         |        |
	/// |                |                  | if T = 1, nop      |         |        |
	BFS(String),
	/// | label          | 1010dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                |                  | dispx2+PC -> PC    |         |        |
	BRA(String),
	/// | Rm             | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
	/// |                |                  | Rm+PC -> PC        |         |        |
	BRAF(Reg),
	/// | label          | 1011dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                |                  | PC -> PR,          |         |        |
	/// |                |                  | dispx2+PC -> PC    |         |        |
	BSR(String),
	/// | Rm             | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
	/// |                |                  | PC -> PR,          |         |        |
	/// |                |                  | Rm+PC -> PC        |         |        |
	BSRF(Reg),
	/// | label          | 10001001dddddddd | if T = 1,          | 3/1     | -      |
	/// |                |                  | dispx2+PC -> PC;   |         |        |
	/// |                |                  | if T = 0, nop      |         |        |
	BT(String),
	/// | label          | 10001101dddddddd | if T = 1,          | 2/1     | -      |
	/// |                |                  | dispx2+PC -> PC;   |         |        |
	/// |                |                  | if T = 0, nop      |         |        |
	BTS(String),
	/// |                | 0000000000101000 | 0 -> MACH, MACL    | 1       | -      |
	CLRMAC,
	/// |                | 0000000000001000 | 0 -> T             | 1       | 0      |
	CLRT,
	/// | #imm,R0        | 10001000iiiiiiii | if R0 = imm,       | 1       | Result |
	/// |                |                  | 1 -> T             |         |        |
	CMP_EQ_Imm(i8),
	/// | Rm,Rn          | 0011nnnnmmmm0000 | if Rn = Rm,        | 1       | Result |
	/// |                |                  | 1 -> T             |         |        |
	CMP_EQ_Reg(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm0011 | if Rn >= Rm with   | 1       | Result |
	/// |                |                  | signed data,       |         |        |
	/// |                |                  | 1 -> T             |         |        |
	CMP_GE(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm0111 | if Rn > Rm with    | 1       | Result |
	/// |                |                  | signed data,       |         |        |
	/// |                |                  | 1 -> T             |         |        |
	CMP_GT(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm0110 | if Rn > Rm with    | 1       | Result |
	/// |                |                  | unsigned data,     |         |        |
	/// |                |                  | 1 -> T             |         |        |
	CMP_HI(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm0010 | if Rn >= Rm with   | 1       | Result |
	/// |                |                  | unsigned data,     |         |        |
	/// |                |                  | 1 -> T             |         |        |
	CMP_HS(Reg,Reg),
	/// | Rn             | 0100nnnn00010101 | if Rn > 0,         | 1       | Result |
	/// |                |                  | 1 -> T             |         |        |
	CMP_PL(Reg),
	/// | Rn             | 0100nnnn00010001 | if Rn > 0,         | 1       | Result |
	/// |                |                  | 1 -> T             |         |        |
	CMP_PZ(Reg),
	/// | Rm,Rn          | 0010nnnnmmmm1100 | if Rn & Rm have    | 1       | Result |
	/// |                |                  | an equivalent      |         |        |
	/// |                |                  | byte, 1 -> T       |         |        |
	CMP_STR(Reg,Reg),
	/// | Rm,Rn          | 0010nnnnmmmm0111 | MSB of Rn -> Q,    | 1       | Result |
	/// |                |                  | MSB of Rm -> M,    |         |        |
	/// |                |                  | M ^ Q -> T         |         |        |
	DIV0S(Reg,Reg),
	/// |                | 0000000000011001 | 0 -> M/Q/T         | 1       | 0      |
	DIV0U,
	/// | Rm,Rn          | 0011nnnnmmmm0100 | Single-step        | 1       | Result |
	/// |                |                  | division (Rn/Rm)   |         |        |
	DIV1(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1101 | Signed operation   | 2 to 4  | -      |
	/// |                |                  | of Rn x Rm ->      |         |        |
	/// |                |                  | MACH, MACL         |         |        |
	DMULS(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm0101 | Unsigned operation | 2 to 4  | -      |
	/// |                |                  | of Rn x Rm -> MACH |         |        |
	/// |                |                  | MACL               |         |        |
	DMULU(Reg,Reg),
	/// | Rn             | 0100nnnn00010000 | Rn - 1 -> Rn, when | 1       | Result |
	/// |                |                  | Rn is 0, 1 -> T.   |         |        |
	/// |                |                  | When Rn is         |         |        |
	/// |                |                  | nonzero, 0 -> T    |         |        |
	DT(Reg),
	/// | Byte Rm,Rn     | 0110nnnnmmmm1110 | A byte in Rm is    | 1       | -      |
	/// |                |                  | sign-extended ->   |         |        |
	/// |                |                  | Rn                 |         |        |
	/// | Word Rm,Rn     | 0110nnnnmmmm1111 | A word in Rm is    | 1       | -      |
	/// |                |                  | sign-extended ->   |         |        |
	/// |                |                  | Rn                 |         |        |
	EXTS(Size,Reg,Reg),
	/// | Byte Rm,Rn     | 0110nnnnmmmm1100 | A byte in Rm is    | 1       | -      |
	/// |                |                  | sign-extended ->   |         |        |
	/// |                |                  | Rn                 |         |        |
	/// | Word Rm,Rn     | 0110nnnnmmmm1101 | A word in Rm is    | 1       | -      |
	/// |                |                  | sign-extended ->   |         |        |
	/// |                |                  | Rn                 |         |        |
	EXTU(Size,Reg,Reg),
	/// | @Rm            | 0100mmmm00101011 | Delayed branch,    | 2       | -      |
	/// |                |                  | Rm -> PC           |         |        |
	JMP(Reg),
	/// | @Rm            | 0100mmmm00001011 | Delayed branch,    | 2       | -      |
	/// |                |                  | PC -> PR,          |         |        |
	/// |                |                  | Rm -> PC           |         |        |
	JSR(Reg),
	/// | Rm,GBR         | 0100mmmm00011110 | Rm -> GBR          | 1       | -      |
	LDC_GBR(Reg),
	/// | Rm,SR          | 0100mmmm00001110 | Rm -> SR           | 1       | LSB    |
	LDC_SR(Reg),
	/// | Rm,VBR         | 0100mmmm00101110 | Rm -> VBR          | 1       | -      |
	LDC_VBR(Reg),
	/// | @Rm+,GBR       | 0100mmmm00010111 | (Rm) -> GBR,       | 3       | -      |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDC_GBR_Inc(Reg),
	/// | @Rm+,SR        | 0100mmmm00000111 | (Rm) -> SR,        | 3       | LSB    |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDC_SR_Inc(Reg),
	/// | @Rm+,VBR       | 0100mmmm00100111 | (Rm) -> VBR,       | 3       | -      |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDC_VBR_Inc(Reg),
	/// | Rm,MACH        | 0100mmmm00001010 | Rm -> MACH         | 1       | -      |
	LDS_MACH(Reg),
	/// | Rm,MACL        | 0100mmmm00011010 | Rm -> MACL         | 1       | -      |
	LDS_MACL(Reg),
	/// | Rm,PR          | 0100mmmm00101010 | Rm -> PR           | 1       | -      |
	LDS_PR(Reg),
	/// | @Rm+,MACH      | 0100mmmm00000110 | (Rm) -> MACH,      | 1       | -      |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDS_MACH_Inc(Reg),
	/// | @Rm+,MACL      | 0100mmmm00010110 | (Rm) -> MACL,      | 1       | -      |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDS_MACL_Inc(Reg),
	/// | @Rm+,PR        | 0100mmmm00100110 | (Rm) -> PR,        | 1       | -      |
	/// |                |                  | Rm + 4 -> Rm       |         |        |
	LDS_PR_Inc(Reg),
	/// | @Rm+,@Rn+      | 0000nnnnmmmm1111 | Signed operation   | 3/(2-4) | -      |
	/// |                |                  | of (Rn) x (Rm) +   |         |        |
	/// |                |                  | MAC -> MAC         |         |        |
	MAC_Long(Reg,Reg),
	/// | @Rm+,@Rn+      | 0100nnnnmmmm1111 | Signed operation   | 3/(2)   | -      |
	/// |                |                  | of (Rn) x (Rm) +   |         |        |
	/// |                |                  | MAC -> MAC         |         |        |
	MAC_Word(Reg,Reg),
	MOV(Size,Arg,Arg),
	/// | @(disp,PC),R0  | 11000111dddddddd | disp x 4+PC -> R0  | 1       | -      |
	MOVA(i8),
	/// | Rn             | 0000nnnn00101001 | T -> Rn            | 1       | -      |
	MOVT(Reg),
	/// | Rm,Rn          | 0000nnnnmmmm0111 | Rn x Rm -> MACL    | 2 to 4  | -      |
	MUL(Size,Reg,Reg),
	/// | Rm,Rn          | 0010nnnnmmmm1111 | Signed operation   | 1 to 3  | -      |
	/// |                |                  | of Rn x Rm -> MAC  |         |        |
	MULS(Reg,Reg),
	/// | Rm,Rn          | 0010nnnnmmmm1110 | Unsigned operation | 1 to 3  | -      |
	/// |                |                  | of Rn x Rm -> MAC  |         |        |
	MULU(Reg,Reg),
	/// | Rm,Rn          | 0110nnnnmmmm1011 | 0 - Rm -> Rn       | 1       | -      |
	NEG(Reg,Reg),
	/// | Rm,Rn          | 0110nnnnmmmm1010 | 0 - Rm - T -> Rn,  | 1       | Borrow |
	/// |                |                  | Borrow -> T        |         |        |
	NEGC(Reg,Reg),
	/// |                | 0000000000001001 | No operation       | 1       | -      |
	NOP,
	/// | Rm,Rn          | 0110nnnnmmmm0111 | ~Rm -> Rn          | 1       | -      |
	NOT(Reg,Reg),
	/// | #imm,R0        | 11001011iiiiiiii | R0|imm -> R0       | 1       | -      |
	OR_Imm(i8),
	/// | Rm,Rn          | 0010nnnnmmmm1011 | Rn|Rm -> Rn        | 1       | -      |
	OR_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR) | 11001111iiiiiiii | (R0+GBR)|imm ->    | 3       | -      |
	/// |                |                  | (R0+GBR)           |         |        |
	OR_Byte(i8),
	/// | Rn             | 0100nnnn00100100 | T <- Rn <- T       | 1       | MSB    |
	ROTCL(Reg),
	/// | Rn             | 0100nnnn00100101 | T -> Rn -> T       | 1       | LSB    |
	ROTCR(Reg),
	/// | Rn             | 0100nnnn00000100 | T <- Rn <- MSB     | 1       | MSB    |
	ROTL(Reg),
	/// | Rn             | 0100nnnn00000101 | LSB -> Rn -> T     | 1       | LSB    |
	ROTR(Reg),
	/// |                | 0000000000101011 | Delayed branch,    | 4       | LSB    |
	/// |                |                  | stack area -> PC/SR |        |        |
	RTE,
	/// |                | 0000000000001011 | Delayed branch,    | 2       | -      |
	/// |                |                  | PR -> PC           |         |        |
	RTS,
	/// |                | 0000000000011000 | 1 -> T             | 1       | 1      |
	SETT,
	/// | Rn             | 0100nnnn00100000 | T <- Rn <- 0       | 1       | MSB    |
	SHAL(Reg),
	/// | Rn             | 0100nnnn00100001 | MSB -> Rn -> T     | 1       | LSB    |
	SHAR(Reg),
	/// | Rn             | 0100nnnn00000000 | T <- Rn <- 0       | 1       | MSB    |
	SHLL(Reg),
	/// | Rn             | 0100nnnn00001000 | Rn << 2 -> Rn      | 1       | -      |
	SHLL2(Reg),
	/// | Rn             | 0100nnnn00011000 | Rn << 8 -> Rn      | 1       | -      |
	SHLL8(Reg),
	/// | Rn             | 0100nnnn00101000 | Rn << 16 -> Rn     | 1       | -      |
	SHLL16(Reg),
	/// | Rn             | 0100nnnn00000001 | 0 -> Rn -> T       | 1       | LSB    |
	SHLR(Reg),
	/// | Rn             | 0100nnnn00001001 | Rn >> 2 -> Rn      | 1       | -      |
	SHLR2(Reg),
	/// | Rn             | 0100nnnn00011001 | Rn >> 8 -> Rn      | 1       | -      |
	SHLR8(Reg),
	/// | Rn             | 0100nnnn00101001 | Rn >> 16 -> Rn     | 1       | -      |
	SHLR16(Reg),
	/// |                | 0000000000011011 | Sleep              | 3       | -      |
	SLEEP,
	/// | GBR,Rn         | 0000nnnn00010010 | GBR -> Rn          | 1       | -      |
	STC_GBR(Reg),
	/// | SR,Rn          | 0000nnnn00000010 | SR -> Rn           | 1       | -      |
	STC_SR(Reg),
	/// | VBR,Rn         | 0000nnnn00100010 | VBR -> Rn          | 1       | -      |
	STC_VBR(Reg),
	/// | GBR,@-Rn       | 0100nnnn00010011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                |                  | GBR -> (Rn)        |         |        |
	STC_GBR_Dec(Reg),
	/// | SR,@-Rn        | 0100nnnn00000011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                |                  | SR -> (Rn)         |         |        |
	STC_SR_Dec(Reg),
	/// | VBR,@-Rn       | 0100nnnn00100011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                |                  | VBR -> (Rn)        |         |        |
	STC_VBR_Dec(Reg),
	/// | MACH,Rn        | 0000nnnn00001010 | MACH -> Rn         | 1       | -      |
	STS_MACH(Reg),
	/// | MACL,Rn        | 0000nnnn00011010 | MACL -> Rn         | 1       | -      |
	STS_MACL(Reg),
	/// | PR,Rn          | 0000nnnn00101010 | PR -> Rn           | 1       | -      |
	STS_PR(Reg),
	/// | MACH,@-Rn      | 0100nnnn00000010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                |                  | MACH -> (Rn)       |         |        |
	STS_MACH_Dec(Reg),
	/// | MACL,@-Rn      | 0100nnnn00010010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                |                  | MACL -> (Rn)       |         |        |
	STS_MACL_Dec(Reg),
	/// | PR,@-Rn        | 0100nnnn00100010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                |                  | PR -> (Rn)         |         |        |
	STS_PR_Dec(Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1000 | Rn - Rm -> Rn      | 1       | -      |
	SUB(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1010 | Rn - Rm - T -> Rn, | 1       | Borrow |
	/// |                |                  | Borrow -> T        |         |        |
	SUBC(Reg,Reg),
	/// | Rm,Rn          | 0011nnnnmmmm1011 | Rn - Rm -> Rn,     | 1       | Under  |
	/// |                |                  | Underflow -> T     |         |        |
	SUBV(Reg,Reg),
	/// | Rm,Rn          | 0110nnnnmmmm1000 | Rm -> Swap upper   | 1       | -      |
	/// |                |                  | and lower 2 bytes  |         |        |
	/// |                |                  | -> Rn              |         |        |
	SWAP_Byte(Reg,Reg),
	/// | Rm,Rn          | 0110nnnnmmmm1001 | Rm -> Swap upper   | 1       | -      |
	/// |                |                  | and lower word ->  |         |        |
	/// |                |                  | Rn                 |         |        |
	SWAP_Word(Reg,Reg),
	/// | @Rn            | 0100nnnn00011011 | if (Rn) is 0, 1->T | 4       | Result |
	/// |                |                  | 1 -> MSB of (Rn)   |         |        |
	TAS(Reg),
	/// | #imm           | 11000011iiiiiiii | PC/SR -> stack     | 8       | -      |
	/// |                |                  | area, (imm x 4 +   |         |        |
	/// |                |                  | VBR) -> PC         |         |        |
	TRAPA(i8),
	/// | #imm,R0        | 11001000iiiiiiii | R0 & imm; if the   | 1       | Result |
	/// |                |                  | result is 0, 1->T  |         |        |
	TST_Imm(i8),
	/// | Rm,Rn          | 0010nnnnmmmm1000 | Rn & Rm; if the    | 1       | Result |
	/// |                |                  | result is 0, 1->T  |         |        |
	TST_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR) | 11001100iiiiiiii | (R0+GBR) & imm; if | 3       | Result |
	/// |                |                  | the result is 0,   |         |        |
	/// |                |                  | 1 -> T             |         |        |
	TST_Byte(i8),
	/// | #imm,R0        | 11001010iiiiiiii | R0 ^ imm -> R0     | 1       | -      |
	XOR_Imm(i8),
	/// | Rm,Rn          | 0010nnnnmmmm1010 | Rn ^ Rm -> Rn      | 1       | -      |
	XOR_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR) | 11001110iiiiiiii | (R0+GBR) ^ imm ->  | 3       | -      |
	/// |                |                  | (R0+GBR)           |         |        |
	XOR_Byte(i8),
	/// | Rm,Rn          | 0010nnnnmmmm1101 | Center 32 bits of  | 1       | -      |
	/// |                |                  | Rm and Rn -> Rn    |         |        |
	XTRCT(Reg,Reg),

	/*** Directives ***/
	Const(Size,Arg),
	Label(String),
}

#[derive(Clone)]
enum State {
	/// Instruction completed for output
	Complete(u16),
	/// Instruction / directive still waiting on label resolution
	Incomplete(Ins),
}

impl std::fmt::Debug for State {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Complete(inst) => write!(fmt, "Complete(${inst:04X})"),
			Self::Incomplete(inst) => write!(fmt, "Incomplete({inst:?})"),
		}
	}
}

type SectionTable = HashMap<u64, Vec<State>>;
type LabelTable = HashMap<String,Option<u32>>;

fn p_error(msg: &str) -> String {
	format!("ERROR: {msg}")
}

fn p_expected<'a,'b,'c>(
	file: &'a str,
	tok: &'b Token,
	msg: &'c str,
) -> String {
	let txt = tok.to_string(file);
	let (line,pos) = tok.pos();
	p_error(&format!("Expected {msg}, Found '{txt}' @ ({line}:{pos})"))
}

fn p_number(
	file: &str,
	tok: &Token,
) -> miette::Result<i64> {
	let txt = tok.to_string(file);
	match txt.chars().next() {
		Some('%') => {
			i64::from_str_radix(&txt[1..].replace('_',""), 2)
		}
		Some('$') => {
			i64::from_str_radix(&txt[1..].replace('_',""), 16)
		}
		Some(c) if c.is_numeric() => {
			i64::from_str_radix(&txt.replace('_',""), 10)
		}
		_ => unreachable!("number tokens should only have valid binary, decimal, or hexadecimal values"),
	}.into_diagnostic()
}

fn p_reg(file: &str, tok: &Token) -> miette::Result<Reg> {
	let txt = tok.to_string(file);
	u8::from_str_radix(&txt[1..], 10)
		.into_diagnostic()
}

fn p_next<'a>(
	tok_idx: &'_ mut usize,
	tokens: &'a [Token],
) -> &'a Token {
	*tok_idx += 1;
	&tokens[*tok_idx]
}

fn p_address(
	file: &str,
	tok_idx: &mut usize,
	tokens: &[Token],
) -> miette::Result<Arg> {
	let nxt_tok = p_next(tok_idx, tokens);
	if nxt_tok.get_type() == TokenType::Dash {
		let nxt_tok = p_next(tok_idx, tokens);
		if nxt_tok.get_type() == TokenType::Register {
			let reg = p_reg(file, nxt_tok)?;
			Ok(Arg::PreDec(reg))
		} else {
			eprintln!("{}", p_expected(file, nxt_tok,
				"Register"));
			todo!("on error, skip to next newline");
		}
	} else if nxt_tok.get_type() == TokenType::Register {
		let reg = p_reg(file, nxt_tok)?;

		let nxt_tok = p_next(tok_idx, tokens);
		if nxt_tok.get_type() == TokenType::Plus {
			Ok(Arg::PostInc(reg))
		} else {
			*tok_idx -= 1;
			Ok(Arg::IndReg(reg))
		}
	} else {
		eprintln!("{}", p_expected(file, nxt_tok,
			"Address specifier (@r_/ @-r_ / @r_+)"));
		todo!("on error, skip to next newline");
	}
}

type ErrFlag = usize;
fn p_size(
	file: &str,
	tok_idx: &mut usize,
	tokens: &[Token],
) -> Result<Size, (ErrFlag,String)> {
	let nxt_tok = p_next(tok_idx, tokens);
	if nxt_tok.get_type() != TokenType::Dot {
		return Err((0,p_expected(file, nxt_tok, "'.'")));
	}

	let nxt_tok = p_next(tok_idx, tokens);
	match nxt_tok.get_type() {
		TokenType::Byte => Ok(Size::Byte),
		TokenType::Word => Ok(Size::Word),
		TokenType::Long => Ok(Size::Long),
		_ => Err((1,p_expected(file, nxt_tok, "size specifier"))),
	}
}

fn match_token_with_msg<'a>(
	file: &'_ str,
	idx: &'_ mut usize,
	tokens: &'a [Token],
	tt: TokenType,
	msg: &'_ str,
) -> &'a Token {
	let tok = p_next(idx, tokens);
	if tok.get_type() != tt {
		p_expected(&file, tok, msg);
		todo!("on error, skip to next newline");
	}
	tok
}

fn match_token<'a>(
	file: &'_ str,
	idx: &'_ mut usize,
	tokens: &'a [Token],
	tt: TokenType,
) -> &'a Token {
	match_token_with_msg(file, idx, tokens, tt, &tt.to_string())
}

fn assert_within_i8(file: &str, tok: &Token, value: i64) {
	if !(i8::MIN as i64..i8::MAX as i64).contains(&value) {
		p_expected(&file, tok,
			"immediate value between -128 & 127");
		todo!("on error, skip to next newline");
	}
}

// TODO - srenshaw - Ensure the parser actually returns what it's supposed to.

/// Parses strings of tokens into valid instructions & directives
///
/// Given a sequence of valid tokens, the parser should return either a section and label table for
/// the analysis stage, or a sequence of all errors encountered while parsing the input.
fn parser(
	file: &str,
	tokens: &[Token],
) -> miette::Result<(SectionTable, LabelTable)> {
	let mut skey = 0;
	let mut section_table = SectionTable::new();
	let mut label_table = LabelTable::new();

	let mut tok_idx = 0;
	while tok_idx < tokens.len() {
		use TokenType::*;
		let cur_tok = &tokens[tok_idx];
		match cur_tok.get_type() {
			ADD => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let ins = match nxt_tok.get_type() {
					Dash => {
						let num_tok = match_token_with_msg(&file, &mut tok_idx, &tokens,
							Number, "Number after unary minus");
						let num = p_number(&file, num_tok)?;
						assert_within_i8(&file, num_tok, -num);
						let imm = (-num) as i8;
						match_token(&file, &mut tok_idx, &tokens, Comma);
						let reg_tok = match_token(&file, &mut tok_idx, &tokens, Register);
						let reg = p_reg(&file, reg_tok)?;
						Ins::ADD_Imm(imm, reg)
					}
					Number => {
						let num = p_number(&file, nxt_tok)?;
						assert_within_i8(&file, nxt_tok, num);
						let imm = num as i8;
						match_token(&file, &mut tok_idx, &tokens, Comma);
						let reg_tok = match_token(&file, &mut tok_idx, &tokens, Register);
						let reg = p_reg(&file, reg_tok)?;
						Ins::ADD_Imm(imm, reg)
					}
					Register => {
						let src = p_reg(&file, nxt_tok)?;
						match_token(&file, &mut tok_idx, &tokens, Comma);
						let reg_tok = match_token(&file, &mut tok_idx, &tokens, Register);
						let dst = p_reg(&file, reg_tok)?;
						Ins::ADD_Reg(src,dst)
					}
					_ => {
						p_expected(&file, nxt_tok,
							"Valid ADD source argument: Number or Register");
						todo!("on error, skip to next newline");
					}
				};

				section_table
					.entry(skey)
					.or_default()
					.push(State::Incomplete(ins));
			}
			ADDC => eprintln!("unexpected ADDC"),
			ADDV => eprintln!("unexpected ADDV"),
			AND => eprintln!("unexpected AND"),
			Address => eprintln!("unexpected Address"),
			BF => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.get_type() != Identifier {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"Label"));
					todo!("on error, skip to newline");
				}

				let txt = nxt_tok.to_string(&file);
				section_table
					.entry(skey)
					.or_default()
					.push(State::Incomplete(Ins::BF(txt)));
			}
			BRA => eprintln!("unexpected BRA"),
			BRAF => eprintln!("unexpected BRAF"),
			BSR => eprintln!("unexpected BSR"),
			BSRF => eprintln!("unexpected BSRF"),
			BT => eprintln!("unexpected BT"),
			Byte => eprintln!("unexpected size specifier"),
			CLRMAC => eprintln!("unexpected CLRMAC"),
			CLRT => eprintln!("unexpected CLRT"),
			CMP => eprintln!("unexpected CMP"),
			Colon => eprintln!("unexpected Colon"),
			Comma => eprintln!("unexpected Comma"),
			Comment => {} // skip comments
			Const => {
				let Ok(sz) = p_size(&file, &mut tok_idx, &tokens) else {
					todo!("on error, skip to next newline character");
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let value = match nxt_tok.get_type() {
					Number => {
						let imm = p_number(&file, nxt_tok)?;
						Arg::DirImm(imm)
					}
					Identifier => {
						let txt = nxt_tok.to_string(&file);
						Arg::Label(txt)
					}
					_ => {
						p_expected(&file, nxt_tok,
							"integer literal or label");
						todo!("on error, skip to next newline character");
					}
				};

				// TODO - srenshaw - ensure `value` and `size` match.
				section_table
					.entry(skey)
					.or_default()
					.push(State::Incomplete(Ins::Const(sz,value)));
			}
			CParen => eprintln!("unexpected close-parenthesis"),
			Dash => eprintln!("unexpected Dash"),
			DIV0S => eprintln!("unexpected DIV0S"),
			DIV0U => eprintln!("unexpected DIV0U"),
			DIV1 => eprintln!("unexpected DIV1"),
			DMULS => eprintln!("unexpected DMULS"),
			DMULU => eprintln!("unexpected DMULU"),
			DT => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.get_type() != Register {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"Register"));
					todo!("on error, skip to newline");
				}

				let reg = p_reg(&file, nxt_tok)?;
				section_table
					.entry(skey)
					.or_default()
					.push(State::Incomplete(Ins::DT(reg)));
			}
			Dot => eprintln!("unexpected Dot"),
			EQ => eprintln!("unexpected EQ"),
			Equal => eprintln!("unexpected Equal"),
			EXTS => eprintln!("unexpected EXTS"),
			EXTU => eprintln!("unexpected EXTU"),
			GE => eprintln!("unexpected GE"),
			GT => eprintln!("unexpected GT"),
			HI => eprintln!("unexpected HI"),
			HS => eprintln!("unexpected HS"),
			Identifier => {
				let label = cur_tok.to_string(&file);

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.get_type() == Colon {
					if label_table.contains_key(&label) {
						eprintln!("{}", p_error("Label '{label}' already defined"));
						todo!("on error, skip to newline");
					}

					section_table
						.entry(skey)
						.or_default()
						.push(State::Incomplete(Ins::Label(label.clone())));
					label_table.insert(label, None);
				} else {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"End of label declaration ':'"));
					todo!("on error, skip to newline");
				}
			}
			JMP => eprintln!("unexpected JMP"),
			JSR => eprintln!("unexpected JSR"),
			LDC => eprintln!("unexpected LDC"),
			LDS => eprintln!("unexpected LDS"),
			Long => eprintln!("unexpected size specifier"),
			MAC => eprintln!("unexpected MAC"),
			MOV => {
				let size = match p_size(&file, &mut tok_idx, &tokens) {
					Ok(size) => size,
					Err((n,msg)) => if n == 0 {
						tok_idx -= 1;
						Size::Word
					} else {
						eprintln!("{msg}");
						todo!("on error, skip to newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let src = match nxt_tok.get_type() {
					Identifier => {
						let txt = nxt_tok.to_string(&file);
						Arg::Label(txt)
					}
					Number => {
						let imm = p_number(&file, nxt_tok)?;
						Arg::DirImm(imm)
					}
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					Address => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt != Register {
							eprintln!("{}", p_expected(&file, nxt_tok,
								"A valid MOV source argument"));
							todo!("on error, skip to newline");
						}

						let reg = p_reg(&file, nxt_tok)?;
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt == Plus {
							Arg::PostInc(reg)
						} else {
							tok_idx -= 1;
							Arg::IndReg(reg)
						}
					}
					_ => {
						eprintln!("{}", p_expected(&file, nxt_tok,
							"A valid MOV source argument"));
						todo!("on error, skip to newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.get_type() != Comma {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"','"));
					todo!("on error, skip to newline");
				}

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let dst = match nxt_tok.get_type() {
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					Address => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						let is_pre_dec = nxt_tok.get_type() == Dash;
						let nxt_tok = if is_pre_dec {
							p_next(&mut tok_idx, &tokens)
						} else {
							nxt_tok
						};

						if nxt_tok.get_type() != Register {
							eprintln!("{}", p_expected(&file, nxt_tok,
								"Register"));
							todo!("on error, skip to newline");
						}

						let reg = p_reg(&file, nxt_tok)?;
						if is_pre_dec {
							Arg::PreDec(reg)
						} else {
							Arg::IndReg(reg)
						}
					}
					_ => {
						eprintln!("{}", p_expected(&file, nxt_tok,
							"A valid MOV destination argument"));
						todo!("on error, skip to newline");
					}
				};

				section_table
					.entry(skey)
					.or_default()
					.push(State::Incomplete(Ins::MOV(size,src,dst)));
			}
			MOVA => eprintln!("unexpected MOVA"),
			MOVT => eprintln!("unexpected MOVT"),
			MUL => eprintln!("unexpected MUL"),
			MULS => eprintln!("unexpected MULS"),
			MULU => eprintln!("unexpected MULU"),
			NEG => eprintln!("unexpected NEG"),
			NEGC => eprintln!("unexpected NEGC"),
			Newline => {} // skip newlines
			NOP => eprintln!("unexpected NOP"),
			NOT => eprintln!("unexpected NOT"),
			Number => eprintln!("unexpected Number"),
			OParen => eprintln!("unexpected open-parenthesis"),
			OR => eprintln!("unexpected OR"),
			Org => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				skey = p_number(&file, nxt_tok)? as u64;
			}
			PL => eprintln!("unexpected PL"),
			Plus => eprintln!("unexpected Plus"),
			PZ => eprintln!("unexpected PZ"),
			Register => eprintln!("unexpected Register"),
			ROTCL => eprintln!("unexpected ROTCL"),
			ROTCR => eprintln!("unexpected ROTCR"),
			ROTL => eprintln!("unexpected ROTL"),
			ROTR => eprintln!("unexpected ROTR"),
			RTE => eprintln!("unexpected RTE"),
			RTS => eprintln!("unexpected RTS"),
			SETT => eprintln!("unexpected SETT"),
			SHAL => eprintln!("unexpected SHAL"),
			SHAR => eprintln!("unexpected SHAR"),
			SHLL => eprintln!("unexpected SHLL"),
			SHLL2 => eprintln!("unexpected SHLL2"),
			SHLL8 => eprintln!("unexpected SHLL8"),
			SHLL16 => eprintln!("unexpected SHLL16"),
			SHLR => eprintln!("unexpected SHLR"),
			SHLR2 => eprintln!("unexpected SHLR2"),
			SHLR8 => eprintln!("unexpected SHLR8"),
			SHLR16 => eprintln!("unexpected SHLR16"),
			Slash => eprintln!("unexpected Slash"),
			SLEEP => eprintln!("unexpected SLEEP"),
			STC => eprintln!("unexpected STC"),
			STS => eprintln!("unexpected STS"),
			STR => eprintln!("unexpected STR"),
			SUB => eprintln!("unexpected SUB"),
			SUBC => eprintln!("unexpected SUBC"),
			SUBV => eprintln!("unexpected SUBV"),
			SWAP => eprintln!("unexpected SWAP"),
			TAS => eprintln!("unexpected TAS"),
			TRAPA => eprintln!("unexpected TRAPA"),
			TST => eprintln!("unexpected TST"),
			Word => eprintln!("unexpected size specifier"),
			XOR => eprintln!("unexpected XOR"),
			XTRCT => eprintln!("unexpected XTRCT"),
			Unknown => {
				let txt = cur_tok.to_string(&file);
				let (line,pos) = cur_tok.pos();
				eprintln!("unknown item '{txt}' @ ({line}:{pos})");
			}
		}
		tok_idx += 1;
	}

	Ok((section_table, label_table))
}

fn to_byte2(reg: &Reg) -> u16 {
	(reg << 12) as u16
}

fn to_byte3(reg: &Reg) -> u16 {
	(reg << 8) as u16
}

fn to_sbyte(size: &Size) -> u16 {
	match size {
		Size::Byte => 0b00,
		Size::Word => 0b01,
		Size::Long => 0b10,
	}
}

fn resolver(
	section_table: &mut SectionTable,
	label_table: &mut LabelTable,
) -> bool {
	let mut is_resolved = true;

	*section_table = section_table.iter()
		.map(|(&section_start, section)| {
			let mut results = Vec::with_capacity(section.len());
			for instr in section {
				use Arg::*;
				use Ins::*;
				use Size::*;
				use State::*;
				match instr {
					Incomplete(ADD_Reg(rsrc,rdst)) => {
						let base = 0b0111_0000_0000_1100;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						results.push(Complete(base | nbyte | mbyte));
					}
					Incomplete(ADD_Imm(isrc,rdst)) => {
						let base = 0b0111_0000_00000000;
						let nbyte = to_byte2(rdst);
						let iword = *isrc as i8 as u8 as u16;
						results.push(Complete(base | nbyte | iword));
					}
					Incomplete(Const(Byte,DirImm(_))) => {
						eprintln!("Attempting to declare a byte constant. This doesn't work currently.");
					}
					Incomplete(Const(Word,DirImm(value))) => {
						results.push(Complete(*value as u16));
					}
					Incomplete(Const(Long,DirImm(value))) => {
						results.push(Complete((*value >> 16) as u16));
						results.push(Complete(*value as u16));
					}
					Incomplete(BF(label)) => {
						if !label_table.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(lbl_addr) = label_table[label] {
							let base = 0b10001011_00000000;
							let cur_addr = section_start as u32 + results.len() as u32 * 2;
							let offset = lbl_addr as i64 - cur_addr as i64;
							let disp = offset / 2;
							if !(i8::MIN as i64..=i8::MAX as i64).contains(&disp) {
								todo!("Relative address too big! Switch to memory load and move?");
							}
							let disp = disp as i8 as u8 as u16;
							results.push(Complete(base | disp));
						}
					}
					Incomplete(DT(reg)) => {
						let base = 0b0100_0000_00010000;
						let nbyte = to_byte2(reg);
						results.push(Complete(base | nbyte));
					}
					Incomplete(Ins::Label(label)) => {
						if !label_table.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(addr) = label_table[label] {
							todo!("Label '{label}' already defined to {addr:08X}");
						}
						label_table.insert(label.clone(),
							Some(section_start as u32 + results.len() as u32 * 2));
					}
					Incomplete(MOV(Word,Arg::Label(lsrc),DirReg(rdst))) => {
						if !label_table.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = label_table[lsrc] {
							let cur_addr = section_start as u32 + results.len() as u32 * 2;
							let offset = lbl_addr as i64 - cur_addr as i64;
							let disp = offset / 2;
							if !(i8::MIN as i64..=i8::MAX as i64).contains(&disp) {
								todo!("Relative address too big! Switch to memory load and move?");
							}
							let disp = disp as i8;
							results.push(Incomplete(MOV(Word,DispPC(disp),DirReg(*rdst))));
							is_resolved = false;
						} else {
							results.push(Incomplete(MOV(Word,Arg::Label(lsrc.clone()),DirReg(*rdst))));
							is_resolved = false;
						}
					}
					Incomplete(MOV(Long,Arg::Label(lsrc),DirReg(rdst))) => {
						if !label_table.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = label_table[lsrc] {
							let cur_addr = section_start as u32 + results.len() as u32 * 2;
							let offset = lbl_addr as i64 - cur_addr as i64;
							let disp = offset / 4;
							if !(i8::MIN as i64..=i8::MAX as i64).contains(&disp) {
								todo!("Relative address too big! Switch to memory load and move?");
							}
							let disp = disp as i8;
							results.push(Incomplete(MOV(Long,DispPC(disp),DirReg(*rdst))));
							is_resolved = false;
						} else {
							results.push(Incomplete(MOV(Long,Arg::Label(lsrc.clone()),DirReg(*rdst))));
							is_resolved = false;
						}
					}
					Incomplete(MOV(Byte,DirImm(isrc),DirReg(rdst))) |
					Incomplete(MOV(Word,DirImm(isrc),DirReg(rdst))) |
					Incomplete(MOV(Long,DirImm(isrc),DirReg(rdst))) => {
						let base = 0b1110_0000_0000_0000;
						let nbyte = to_byte2(rdst);
						let imm = *isrc as i64;
						if !(i16::MIN as i64..=i16::MAX as i64).contains(&imm) {
							// 32-bit immediate
							eprintln!("Moving 32-bit immediates is not implemented yet. Declare a labeled constant and move the label instead.");
						} else if !(i8::MIN as i64..=i8::MAX as i64).contains(&imm) {
							// 16-bit immediate
							eprintln!("Moving 16-bit immediates is not implemented yet. Declare a labeled constant and move the label instead.");
						} else {
							// 8-bit immediate
							let iword = (*isrc & 0xFF) as u16;
							results.push(Complete(base | nbyte | iword));
						}
					}
					Incomplete(MOV(Word,DispPC(disp),DirReg(rdst))) => {
						let base = 0b1001_0000_00000000;
						let nbyte = to_byte2(rdst);
						let dword = *disp as u8 as u16;
						results.push(Complete(base | nbyte | dword));
					}
					Incomplete(MOV(Long,DispPC(disp),DirReg(rdst))) => {
						let base = 0b1101_0000_00000000;
						let nbyte = to_byte2(rdst);
						let dword = *disp as u8 as u16;
						results.push(Complete(base | nbyte | dword));
					}
					Incomplete(MOV(Byte,DirReg(rsrc),DirReg(rdst))) |
					Incomplete(MOV(Word,DirReg(rsrc),DirReg(rdst))) |
					Incomplete(MOV(Long,DirReg(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0011;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						results.push(Complete(base | nbyte | mbyte));
					}
					Incomplete(MOV(size,DirReg(rsrc),IndReg(rdst))) => {
						let base = 0b0010_0000_0000_0000;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,IndReg(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0000;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,DirReg(rsrc),PreDec(rdst))) => {
						let base = 0b0010_0000_0000_0100;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,PostInc(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0100;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size @ Byte,DirReg(0),DispReg(disp,rdst))) |
					Incomplete(MOV(size @ Word,DirReg(0),DispReg(disp,rdst))) => {
						let base = 0b10000000_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let nbyte = to_byte3(rdst);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | nbyte | dbyte));
					}
					Incomplete(MOV(Long,DirReg(rsrc),DispReg(disp,rdst))) => {
						let base = 0b0001_0000_0000_0000;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(MOV(size @ Byte,DispReg(disp,rsrc),DirReg(0))) |
					Incomplete(MOV(size @ Word,DispReg(disp,rsrc),DirReg(0))) => {
						let base = 0b10000100_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let mbyte = to_byte3(rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | mbyte | dbyte));
					}
					Incomplete(MOV(Long,DispReg(disp,rsrc),DirReg(rdst))) => {
						let base = 0b0101_0000_0000_0000;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(MOV(size,DirReg(rsrc),DispR0(rdst))) => {
						let base = 0b0000_0000_0000_0100;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,DispR0(rsrc),DirReg(rdst))) => {
						let base = 0b0000_0000_0000_1100;
						let nbyte = to_byte2(rdst);
						let mbyte = to_byte3(rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,DirReg(0),DispGBR(disp))) => {
						let base = 0b11000000_00000000;
						let sbyte = to_sbyte(size) << 8;
						let dword = *disp as u8 as u16;
						results.push(Complete(base | sbyte | dword));
					}
					Incomplete(MOV(size,DispGBR(disp),DirReg(0))) => {
						let base = 0b11000100_00000000;
						let sbyte = to_sbyte(size) << 8;
						let dword = *disp as u8 as u16;
						results.push(Complete(base | sbyte | dword));
					}
					Complete(_) => results.push(instr.clone()),
					_ => todo!("Invalid instruction: {instr:?}"),
				}
			}

			(section_start, results)
		}).collect();

	is_resolved
}

