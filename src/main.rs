
use std::collections::HashMap;

use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::TokenType;

mod parser;
use parser::parser;
use parser::{Arg,Ins,Reg,Output,Size,State};

type Label = std::rc::Rc<str>;
type SectionMap = HashMap<u64, Vec<State>>;
type LabelMap = HashMap<Label, Option<u32>>;

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
		if token.get_type() == TokenType::Unknown || !is_silent {
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
		let output = section.iter()
			.map(|state| state.completed_or(0xDEAD))
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output)
			.into_diagnostic()?;
	}

	Ok(())
}

fn to_byte2(reg: Reg) -> u16 {
	(reg as u16) << 12
}

fn to_byte3(reg: Reg) -> u16 {
	(reg as u16) << 8
}

fn to_sbyte(size: &Size) -> u16 {
	match size {
		Size::Byte => 0b00,
		Size::Word => 0b01,
		Size::Long => 0b10,
	}
}

fn n_type(base: u16, rn: Reg) -> State {
	State::Complete(base | to_byte2(rn))
}

fn m_type(base: u16, rm: Reg) -> State {
	State::Complete(base | to_byte2(rm))
}

fn nm_type(base: u16, rm: Reg, rn: Reg) -> State {
	State::Complete(base | to_byte2(rn) | to_byte3(rm))
}

fn d_type(base: u16, disp: u8) -> State {
	State::Complete(base | disp as u16)
}

fn d12_type(base: u16, disp: u16) -> State {
	State::Complete(base | (disp & 0x0FFF))
}

fn nd8_type(base: u16, rn: Reg, disp: u8) -> State {
	State::Complete(base | to_byte2(rn) | (disp & 0x0F) as u16)
}

fn i_type(base: u16, imm: i8) -> State {
	State::Complete(base | imm as u8 as u16)
}

fn ni_type(base: u16, rn: Reg, imm: i8) -> State {
	State::Complete(base | to_byte2(rn) | imm as u8 as u16)
}

fn to_d8(
	data: &LabelMap,
	label: &str,
	cur_addr: u32,
	base: u16,
) -> Option<State> {
	if !data.contains_key(label) {
		todo!("Unknown label '{label}'");
	}

	data[label].map(|lbl_addr| {
		let offset = lbl_addr as i64 - cur_addr as i64;
		let disp = offset / 2;
		if !(i8::MIN as i64..=i8::MAX as i64).contains(&disp) {
			todo!("Relative address too big! Switch to memory load and move?");
		}
		d_type(base, disp as i8 as u8)
	})
}

fn to_d12(
	data: &LabelMap,
	label: &str,
	cur_addr: u32,
	base: u16,
) -> Option<State> {
	if !data.contains_key(label) {
		todo!("Unknown label '{label}'");
	}

	data[label].map(|lbl_addr| {
		let offset = lbl_addr as i64 - cur_addr as i64;
		let disp = offset / 2;
		if !(0x800 as i16 as i64..=0x7FF as i16 as i64).contains(&disp) {
			todo!("Relative address too big! Switch to memory load and move?");
		}
		d12_type(base, disp as i16 as u16)
	})
}

fn resolver(
	data: &mut Output,
) -> bool {
	let mut is_resolved = true;

	data.sections = data.sections.iter()
		.map(|(&section_start, section)| {
			let section_start = section_start as u32;
			let mut results = Vec::with_capacity(section.len());
			for instr in section {
				use Arg::*;
				use Ins::*;
				use Size::*;
				use State::*;

				let section_pos = results.len() as u32;
				match instr {
					// | #imm,Rn           | 0111nnnniiiiiiii | Rn+imm -> Rn       | 1       | -      |
					Incomplete(ADD_Imm(imm,rn)) => results.push(ni_type(0b0111_0000_00000000, *rn, *imm)),
					// | Rm,Rn             | 0011nnnnmmmm1100 | Rn+Rm -> Rn        | 1       | -      |
					Incomplete(ADD_Reg(rm,rn)) => results.push(nm_type(0b0111_0000_0000_1100, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1110 | Rn+Rm+T -> Rn,     | 1       | Carry  |
					// |                   |                  | Carry -> T         |         |        |
					Incomplete(ADDC(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1110, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1111 | Rn+Rm -> Rn,       | 1       | Over   |
					// |                   |                  | Overflow -> T      |         |        |
					Incomplete(ADDV(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1111, *rm, *rn)),
					// | #imm,R0           | 11001001iiiiiiii | R0 & imm -> R0     | 1       | -      |
					Incomplete(AND_Imm(imm)) => results.push(d_type(0b11001001_00000000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1001 | Rn & Rm -> Rn      | 1       | -      |
					Incomplete(AND_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1001, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001101iiiiiiii | (R0+GBR) & imm     | 3       | -      |
					// |                   |                  | -> (R0+GBR)        |         |        |
					Incomplete(AND_Byte(imm)) => results.push(d_type(0b11001101_00000000, *imm)),
					// | label             | 10011011dddddddd | if T = 0,          | 3/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 1, nop      |         |        |
					Incomplete(BF(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001011_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BF(label.clone())))),
					// | label             | 10001111dddddddd | if T = 0,          | 2/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 1, nop      |         |        |
					Incomplete(BFS(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001111_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BFS(label.clone())))),
					// | label             | 1010dddddddddddd | Delayed branch,    | 2       | -      |
					// |                   |                  | dispx2+PC -> PC    |         |        |
					Incomplete(BRA(label)) => to_d12(&data.labels, label, section_start + section_pos * 2, 0b1010_000000000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BRA(label.clone())))),
					// | Rm                | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
					// |                   |                  | Rm+PC -> PC        |         |        |
					Incomplete(BRAF(rm)) => results.push(m_type(0b0000_0000_00100011, *rm)),
					// | label             | 1011dddddddddddd | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | dispx2+PC -> PC    |         |        |
					Incomplete(BSR(label)) => to_d12(&data.labels, label, section_start + section_pos * 2, 0b1011_000000000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BSR(label.clone())))),
					// | Rm                | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | Rm+PC -> PC        |         |        |
					Incomplete(BSRF(rm)) => results.push(m_type(0b0000_0000_00000011, *rm)),
					// | label             | 10001001dddddddd | if T = 1,          | 3/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 0, nop      |         |        |
					Incomplete(BT(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001001_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BT(label.clone())))),
					// | label             | 10001101dddddddd | if T = 1,          | 2/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 0, nop      |         |        |
					Incomplete(BTS(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001101_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BTS(label.clone())))),
					// |                   | 0000000000101000 | 0 -> MACH, MACL    | 1       | -      |
					Incomplete(CLRMAC) => results.push(Complete(0b0000000000101000)),
					// |                   | 0000000000001000 | 0 -> T             | 1       | 0      |
					Incomplete(CLRT) => results.push(Complete(0b0000000000001000)),
					// | #imm,R0           | 10001000iiiiiiii | if R0 = imm,       | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_EQ_Imm(imm)) => results.push(i_type(0b10001000_00000000, *imm)),
					// | Rm,Rn             | 0011nnnnmmmm0000 | if Rn = Rm,        | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_EQ_Reg(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0000, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0011 | if Rn >= Rm with   | 1       | Result |
					// |                   |                  | signed data,       |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_GE(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0011, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0111 | if Rn > Rm with    | 1       | Result |
					// |                   |                  | signed data,       |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_GT(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0111, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0110 | if Rn > Rm with    | 1       | Result |
					// |                   |                  | unsigned data,     |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_HI(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0110, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0010 | if Rn >= Rm with   | 1       | Result |
					// |                   |                  | unsigned data,     |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_HS(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0010, *rm, *rn)),
					// | Rn                | 0100nnnn00010101 | if Rn > 0,         | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_PL(rn)) => results.push(n_type(0b0100_0000_00010101, *rn)),
					// | Rn                | 0100nnnn00010001 | if Rn > 0,         | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CMP_PZ(rn)) => results.push(n_type(0b0100_0000_00010001, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1100 | if Rn & Rm have    | 1       | Result |
					// |                   |                  | an equivalent      |         |        |
					// |                   |                  | byte, 1 -> T       |         |        |
					Incomplete(CMP_STR(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1100, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm0111 | MSB of Rn -> Q,    | 1       | Result |
					// |                   |                  | MSB of Rm -> M,    |         |        |
					// |                   |                  | M ^ Q -> T         |         |        |
					Incomplete(DIV0S(rm,rn)) => results.push(nm_type(0b0010_0000_0000_0111, *rm, *rn)),
					// |                   | 0000000000011001 | 0 -> M/Q/T         | 1       | 0      |
					Incomplete(DIV0U) => results.push(Complete(0b0000000000011001)),
					// | Rm,Rn             | 0011nnnnmmmm0100 | Single-step        | 1       | Result |
					// |                   |                  | division (Rn/Rm)   |         |        |
					Incomplete(DIV1(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0100, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1101 | Signed operation   | 2 to 4  | -      |
					// |                   |                  | of Rn x Rm ->      |         |        |
					// |                   |                  | MACH, MACL         |         |        |
					Incomplete(DMULS(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1101, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0101 | Unsigned operation | 2 to 4  | -      |
					// |                   |                  | of Rn x Rm -> MACH |         |        |
					// |                   |                  | MACL               |         |        |
					Incomplete(DMULU(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0101, *rm, *rn)),
					// | Rn                | 0100nnnn00010000 | Rn - 1 -> Rn, when | 1       | Result |
					// |                   |                  | Rn is 0, 1 -> T.   |         |        |
					// |                   |                  | When Rn is         |         |        |
					// |                   |                  | nonzero, 0 -> T    |         |        |
					Incomplete(DT(rn)) => results.push(n_type(0b0100_0000_00010000, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1110 | A byte in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(EXTS(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1110, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1111 | A word in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(EXTS(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1111, *rm, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1100 | A byte in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(EXTU(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1100, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1101 | A word in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(EXTU(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1101, *rm, *rn)),
					// | @Rm               | 0100mmmm00101011 | Delayed branch,    | 2       | -      |
					// |                   |                  | Rm -> PC           |         |        |
					Incomplete(JMP(rm)) => results.push(m_type(0b0100_0000_00101011, *rm)),
					// | @Rm               | 0100mmmm00001011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | Rm -> PC           |         |        |
					Incomplete(JSR(rm)) => results.push(m_type(0b0100_0000_00001011, *rm)),
					// | Rm,GBR            | 0100mmmm00011110 | Rm -> GBR          | 1       | -      |
					Incomplete(LDC_GBR(rm)) => results.push(m_type(0b0100_0000_00011110, *rm)),
					// | Rm,SR             | 0100mmmm00001110 | Rm -> SR           | 1       | LSB    |
					Incomplete(LDC_SR(rm)) => results.push(m_type(0b0100_0000_00001110, *rm)),
					// | Rm,VBR            | 0100mmmm00101110 | Rm -> VBR          | 1       | -      |
					Incomplete(LDC_VBR(rm)) => results.push(m_type(0b0100_0000_00101110, *rm)),
					// | @Rm+,GBR          | 0100mmmm00010111 | (Rm) -> GBR,       | 3       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDC_GBR_Inc(rm)) => results.push(m_type(0b0100_0000_00010111, *rm)),
					// | @Rm+,SR           | 0100mmmm00000111 | (Rm) -> SR,        | 3       | LSB    |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDC_SR_Inc(rm)) => results.push(m_type(0b0100_0000_00000111, *rm)),
					// | @Rm+,VBR          | 0100mmmm00100111 | (Rm) -> VBR,       | 3       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDC_VBR_Inc(rm)) => results.push(m_type(0b0100_0000_00100111, *rm)),
					// | Rm,MACH           | 0100mmmm00001010 | Rm -> MACH         | 1       | -      |
					Incomplete(LDS_MACH(rm)) => results.push(m_type(0b0100_0000_00001010, *rm)),
					// | Rm,MACL           | 0100mmmm00011010 | Rm -> MACL         | 1       | -      |
					Incomplete(LDS_MACL(rm)) => results.push(m_type(0b0100_0000_00011010, *rm)),
					// | Rm,PR             | 0100mmmm00101010 | Rm -> PR           | 1       | -      |
					Incomplete(LDS_PR(rm)) => results.push(m_type(0b0100_0000_00101010, *rm)),
					// | @Rm+,MACH         | 0100mmmm00000110 | (Rm) -> MACH,      | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDS_MACH_Inc(rm)) => results.push(m_type(0b0100_0000_00000110, *rm)),
					// | @Rm+,MACL         | 0100mmmm00010110 | (Rm) -> MACL,      | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDS_MACL_Inc(rm)) => results.push(m_type(0b0100_0000_00010110, *rm)),
					// | @Rm+,PR           | 0100mmmm00100110 | (Rm) -> PR,        | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LDS_PR_Inc(rm)) => results.push(m_type(0b0100_0000_00100110, *rm)),
					// | @Rm+,@Rn+         | 0000nnnnmmmm1111 | Signed operation   | 3/(2-4) | -      |
					// |                   |                  | of (Rn) x (Rm) +   |         |        |
					// |                   |                  | MAC -> MAC         |         |        |
					Incomplete(MAC_Long(rm,rn)) => results.push(nm_type(0b0000_0000_0000_1111, *rm, *rn)),
					// | @Rm+,@Rn+         | 0100nnnnmmmm1111 | Signed operation   | 3/(2)   | -      |
					// |                   |                  | of (Rn) x (Rm) +   |         |        |
					// |                   |                  | MAC -> MAC         |         |        |
					Incomplete(MAC_Word(rm,rn)) => results.push(nm_type(0b0100_0000_0000_1111, *rm, *rn)),
					// | #imm,Rn           | 1110nnnniiiiiiii | imm -> Sign        | 1       | -      |
					// |                   |                  | extension -> Rn    |         |        |
					// TODO - srenshaw - Need to decide how to handle non-8-bit MOV immediate mnemonics
					Incomplete(MOV_Imm(imm,rn)) => {
						let base = 0b1110_0000_0000_0000;
						let n = to_byte2(*rn);
						let imm = *imm as i64;
						if !(i16::MIN as i64..=i16::MAX as i64).contains(&imm) {
							// 32-bit immediate
							eprintln!("Moving 32-bit immediates is not implemented yet. Declare a labeled constant and move the label instead.");
						} else if !(i8::MIN as i64..=i8::MAX as i64).contains(&imm) {
							// 16-bit immediate
							eprintln!("Moving 16-bit immediates is not implemented yet. Declare a labeled constant and move the label instead.");
						} else {
							// 8-bit immediate
							let iword = imm as i8 as u8 as u16;
							results.push(Complete(base | n | iword));
						}
					}
					// | Rm,Rn             | 0110nnnnmmmm0011 | Rm -> Rn           | 1       | -      |
					// |                   |                  |                    |         |        |
					Incomplete(MOV_Reg(rm,rn)) => results.push(nm_type(0b0110_0000_0000_0011, *rm, *rn)),
					Incomplete(MOV(Word,Arg::Label(lsrc),DirReg(rdst))) => {
						if !data.labels.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = data.labels[lsrc] {
							let cur_addr = section_start + section_pos * 2;
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
						if !data.labels.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = data.labels[lsrc] {
							let cur_addr = section_start + section_pos * 2;
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
					Incomplete(MOV(Word,DispPC(disp),DirReg(rn))) => results.push(nd8_type(0b1001_0000_00000000, *rn, *disp as u8)),
					Incomplete(MOV(Long,DispPC(disp),DirReg(rn))) => results.push(nd8_type(0b1101_0000_00000000, *rn, *disp as u8)),
					Incomplete(MOV(Byte,DirReg(rm),DirReg(rn))) |
					Incomplete(MOV(Word,DirReg(rm),DirReg(rn))) |
					Incomplete(MOV(Long,DirReg(rm),DirReg(rn))) => results.push(nm_type(0b0110_0000_0000_0011, *rm, *rn)),
					Incomplete(MOV(size,DirReg(rsrc),IndReg(rdst))) => {
						let base = 0b0010_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,IndReg(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,DirReg(rsrc),PreDec(rdst))) => {
						let base = 0b0010_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,PostInc(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size @ Byte,DirReg(0),DispReg(disp,rdst))) |
					Incomplete(MOV(size @ Word,DirReg(0),DispReg(disp,rdst))) => {
						let base = 0b10000000_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let nbyte = to_byte3(*rdst);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | nbyte | dbyte));
					}
					Incomplete(MOV(Long,DirReg(rsrc),DispReg(disp,rdst))) => {
						let base = 0b0001_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(MOV(size @ Byte,DispReg(disp,rsrc),DirReg(0))) |
					Incomplete(MOV(size @ Word,DispReg(disp,rsrc),DirReg(0))) => {
						let base = 0b10000100_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | mbyte | dbyte));
					}
					Incomplete(MOV(Long,DispReg(disp,rsrc),DirReg(rdst))) => {
						let base = 0b0101_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(MOV(size,DirReg(rsrc),DispR0(rdst))) => {
						let base = 0b0000_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(MOV(size,DispR0(rsrc),DirReg(rdst))) => {
						let base = 0b0000_0000_0000_1100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
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
					// | @(disp,PC),R0     | 11000111dddddddd | disp x 4+PC -> R0  | 1       | -      |
					Incomplete(MOVA(disp)) => results.push(d_type(0b11000111_00000000, *disp as u8)),
					// | Rn                | 0000nnnn00101001 | T -> Rn            | 1       | -      |
					Incomplete(MOVT(rn)) => results.push(n_type(0b0000_0000_00101001, *rn)),
					// | Rm,Rn             | 0000nnnnmmmm0111 | Rn x Rm -> MACL    | 2 to 4  | -      |
					Incomplete(MUL(rm,rn)) => results.push(nm_type(0b0000_0000_0000_0111, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1111 | Signed operation   | 1 to 3  | -      |
					// |                   |                  | of Rn x Rm -> MAC  |         |        |
					Incomplete(MULS(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1111, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1110 | Unsigned operation | 1 to 3  | -      |
					// |                   |                  | of Rn x Rm -> MAC  |         |        |
					Incomplete(MULU(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1110, *rm, *rn)),
					// | Rm,Rn             | 0110nnnnmmmm1011 | 0 - Rm -> Rn       | 1       | -      |
					Incomplete(NEG(rm,rn)) => results.push(nm_type(0b0110_0000_0000_1011, *rm, *rn)),
					// | Rm,Rn             | 0110nnnnmmmm1010 | 0 - Rm - T -> Rn,  | 1       | Borrow |
					// |                   |                  | Borrow -> T        |         |        |
					Incomplete(NEGC(rm,rn)) => results.push(nm_type(0b0110_0000_0000_1010, *rm, *rn)),
					// |                   | 0000000000001001 | No operation       | 1       | -      |
					Incomplete(NOP) => results.push(Complete(0b0000000000001001)),
					// | Rm,Rn             | 0110nnnnmmmm0111 | ~Rm -> Rn          | 1       | -      |
					Incomplete(NOT(rm,rn)) => results.push(nm_type(0b0110_0000_0000_0111, *rm, *rn)),
					// | #imm,R0           | 11001011iiiiiiii | R0|imm -> R0       | 1       | -      |
					Incomplete(OR_Imm(imm)) => results.push(d_type(0b11001011_00000000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1011 | Rn|Rm -> Rn        | 1       | -      |
					Incomplete(OR_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1011, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001111iiiiiiii | (R0+GBR)|imm ->    | 3       | -      |
					// |                   |                  | (R0+GBR)           |         |        |
					Incomplete(OR_Byte(imm)) => results.push(d_type(0b11001111_00000000, *imm)),
					// | Rn                | 0100nnnn00100100 | T <- Rn <- T       | 1       | MSB    |
					Incomplete(ROTCL(rn)) => results.push(n_type(0b0100_0000_00100100, *rn)),
					// | Rn                | 0100nnnn00100101 | T -> Rn -> T       | 1       | LSB    |
					Incomplete(ROTCR(rn)) => results.push(n_type(0b0100_0000_00100101, *rn)),
					// | Rn                | 0100nnnn00000100 | T <- Rn <- MSB     | 1       | MSB    |
					Incomplete(ROTL(rn)) => results.push(n_type(0b0100_0000_0000_0100, *rn)),
					// | Rn                | 0100nnnn00000101 | LSB -> Rn -> T     | 1       | LSB    |
					Incomplete(ROTR(rn)) => results.push(n_type(0b0100_0000_00000101, *rn)),
					// |                   | 0000000000101011 | Delayed branch,    | 4       | LSB    |
					// |                   |                  | stack area -> PC/SR |        |        |
					Incomplete(RTE) => {}
					// |                   | 0000000000001011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PR -> PC           |         |        |
					Incomplete(RTS) => {}
					// |                   | 0000000000011000 | 1 -> T             | 1       | 1      |
					Incomplete(SETT) => {}
					// | Rn                | 0100nnnn00100000 | T <- Rn <- 0       | 1       | MSB    |
					Incomplete(SHAL(rn)) => results.push(n_type(0b0100_0000_00100000, *rn)),
					// | Rn                | 0100nnnn00100001 | MSB -> Rn -> T     | 1       | LSB    |
					Incomplete(SHAR(rn)) => results.push(n_type(0b0100_0000_00100001, *rn)),
					// | Rn                | 0100nnnn00000000 | T <- Rn <- 0       | 1       | MSB    |
					Incomplete(SHLL(rn)) => results.push(n_type(0b0100_0000_00000000, *rn)),
					// | Rn                | 0100nnnn00001000 | Rn << 2 -> Rn      | 1       | -      |
					Incomplete(SHLL2(rn)) => results.push(n_type(0b0100_0000_00001000, *rn)),
					// | Rn                | 0100nnnn00011000 | Rn << 8 -> Rn      | 1       | -      |
					Incomplete(SHLL8(rn)) => results.push(n_type(0b0100_0000_00011000, *rn)),
					// | Rn                | 0100nnnn00101000 | Rn << 16 -> Rn     | 1       | -      |
					Incomplete(SHLL16(rn)) => results.push(n_type(0b0100_0000_00101000, *rn)),
					// | Rn                | 0100nnnn00000001 | 0 -> Rn -> T       | 1       | LSB    |
					Incomplete(SHLR(rn)) => results.push(n_type(0b0100_0000_00000001, *rn)),
					// | Rn                | 0100nnnn00001001 | Rn >> 2 -> Rn      | 1       | -      |
					Incomplete(SHLR2(rn)) => results.push(n_type(0b0100_0000_00001001, *rn)),
					// | Rn                | 0100nnnn00011001 | Rn >> 8 -> Rn      | 1       | -      |
					Incomplete(SHLR8(rn)) => results.push(n_type(0b0100_0000_00011001, *rn)),
					// | Rn                | 0100nnnn00101001 | Rn >> 16 -> Rn     | 1       | -      |
					Incomplete(SHLR16(rn)) => results.push(n_type(0b0100_0000_00101001, *rn)),
					// |                   | 0000000000011011 | Sleep              | 3       | -      |
					Incomplete(SLEEP) => {}
					// | GBR,Rn            | 0000nnnn00010010 | GBR -> Rn          | 1       | -      |
					Incomplete(STC_GBR(rn)) => results.push(n_type(0b0000_0000_00010010, *rn)),
					// | SR,Rn             | 0000nnnn00000010 | SR -> Rn           | 1       | -      |
					Incomplete(STC_SR(rn)) => results.push(n_type(0b0000_0000_00000010, *rn)),
					// | VBR,Rn            | 0000nnnn00100010 | VBR -> Rn          | 1       | -      |
					Incomplete(STC_VBR(rn)) => results.push(n_type(0b0000_0000_00100010, *rn)),
					// | GBR,@-Rn          | 0100nnnn00010011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | GBR -> (Rn)        |         |        |
					Incomplete(STC_GBR_Dec(rn)) => results.push(n_type(0b0100_0000_00010011, *rn)),
					// | SR,@-Rn           | 0100nnnn00000011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | SR -> (Rn)         |         |        |
					Incomplete(STC_SR_Dec(rn)) => results.push(n_type(0b0100_0000_00000011, *rn)),
					// | VBR,@-Rn          | 0100nnnn00100011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | VBR -> (Rn)        |         |        |
					Incomplete(STC_VBR_Dec(rn)) => results.push(n_type(0b0100_0000_00100011, *rn)),
					// | MACH,Rn           | 0000nnnn00001010 | MACH -> Rn         | 1       | -      |
					Incomplete(STS_MACH(rn)) => results.push(n_type(0b0000_0000_00001010, *rn)),
					// | MACL,Rn           | 0000nnnn00011010 | MACL -> Rn         | 1       | -      |
					Incomplete(STS_MACL(rn)) => results.push(n_type(0b0000_0000_00011010, *rn)),
					// | PR,Rn             | 0000nnnn00101010 | PR -> Rn           | 1       | -      |
					Incomplete(STS_PR(rn)) => results.push(n_type(0b0000_0000_00101010, *rn)),
					// | MACH,@-Rn         | 0100nnnn00000010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | MACH -> (Rn)       |         |        |
					Incomplete(STS_MACH_Dec(rn)) => results.push(n_type(0b0100_0000_00000010, *rn)),
					// | MACL,@-Rn         | 0100nnnn00010010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | MACL -> (Rn)       |         |        |
					Incomplete(STS_MACL_Dec(rn)) => results.push(n_type(0b0100_0000_00010010, *rn)),
					// | PR,@-Rn           | 0100nnnn00100010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | PR -> (Rn)         |         |        |
					Incomplete(STS_PR_Dec(rn)) => results.push(n_type(0b0100_0000_00100010, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1000 | Rn - Rm -> Rn      | 1       | -      |
					Incomplete(SUB(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1000, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1010 | Rn - Rm - T -> Rn, | 1       | Borrow |
					// |                   |                  | Borrow -> T        |         |        |
					Incomplete(SUBC(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1010, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1011 | Rn - Rm -> Rn,     | 1       | Under  |
					// |                   |                  | Underflow -> T     |         |        |
					Incomplete(SUBV(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1011, *rm, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1000 | Rm -> Swap upper   | 1       | -      |
					// |                   |                  | and lower 2 bytes  |         |        |
					// |                   |                  | -> Rn              |         |        |
					Incomplete(SWAP(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1000, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1001 | Rm -> Swap upper   | 1       | -      |
					// |                   |                  | and lower word ->  |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(SWAP(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1001, *rm, *rn)),
					// | @Rn               | 0100nnnn00011011 | if (Rn) is 0, 1->T | 4       | Result |
					// |                   |                  | 1 -> MSB of (Rn)   |         |        |
					Incomplete(TAS(rn)) => results.push(n_type(0b0100_0000_00011011, *rn)),
					// | #imm              | 11000011iiiiiiii | PC/SR -> stack     | 8       | -      |
					// |                   |                  | area, (imm x 4 +   |         |        |
					// |                   |                  | VBR) -> PC         |         |        |
					Incomplete(TRAPA(imm)) => results.push(d_type(0b11000011_00000000, *imm)),
					// | #imm,R0           | 11001000iiiiiiii | R0 & imm; if the   | 1       | Result |
					// |                   |                  | result is 0, 1->T  |         |        |
					Incomplete(TST_Imm(imm)) => results.push(d_type(0b11001000_00000000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1000 | Rn & Rm; if the    | 1       | Result |
					// |                   |                  | result is 0, 1->T  |         |        |
					Incomplete(TST_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1000, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001100iiiiiiii | (R0+GBR) & imm; if | 3       | Result |
					// |                   |                  | the result is 0,   |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(TST_Byte(imm)) => results.push(d_type(0b11001100_00000000, *imm)),
					// | #imm,R0           | 11001010iiiiiiii | R0 ^ imm -> R0     | 1       | -      |
					Incomplete(XOR_Imm(imm)) => results.push(d_type(0b11001010_00000000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1010 | Rn ^ Rm -> Rn      | 1       | -      |
					Incomplete(XOR_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1010, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001110iiiiiiii | (R0+GBR) ^ imm ->  | 3       | -      |
					// |                   |                  | (R0+GBR)           |         |        |
					Incomplete(XOR_Byte(imm)) => results.push(d_type(0b11001110_00000000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1101 | Center 32 bits of  | 1       | -      |
					// |                   |                  | Rm and Rn -> Rn    |         |        |
					Incomplete(XTRCT(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1101, *rm, *rn)),

					Incomplete(Const_Imm(Byte,_)) => {
						eprintln!("Attempting to declare a byte constant. This doesn't work currently.");
					}
					Incomplete(Const_Imm(Word,value)) => {
						results.push(Complete(*value as i16 as u16));
					}
					Incomplete(Const_Imm(Long,value)) => {
						results.push(Complete((*value >> 16) as i16 as u16));
						results.push(Complete(*value as i16 as u16));
					}
					Incomplete(Const_Label(Byte,_)) => eprintln!("Attempting to declare a byte constant. This doesn't work currently."),
					Incomplete(Const_Label(Word,_)) => eprintln!("Attempting to declare a word constant with a label. This doesn't work currently."),
					Incomplete(Const_Label(Long,_)) => eprintln!("Attempting to declare a long constant with a label. This doesn't work currently."),
					Incomplete(Ins::Label(label)) => {
						if !data.labels.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(addr) = data.labels[label] {
							todo!("Label '{label}' already defined to {addr:08X}");
						}
						data.labels.insert(label.clone(),
							Some(section_start as u32 + results.len() as u32 * 2));
					}

					Complete(_) => results.push(instr.clone()),
					_ => todo!("Invalid instruction: {instr:?}"),
				}
			}

			(section_start as u64, results)
		}).collect();

	is_resolved
}

