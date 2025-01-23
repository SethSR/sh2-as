use std::collections::HashMap;

use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::TokenType;

mod parser;
use parser::parser;
use parser::{Arg, Ins, Output, Reg, Size, State};

type Label = std::rc::Rc<str>;
type SectionMap = HashMap<u64, Vec<State>>;
type LabelMap = HashMap<Label, Option<u32>>;

fn main() -> miette::Result<()> {
	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let target = args.next().unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = false;

	let file = std::fs::read_to_string(source).into_diagnostic()?;

	let tokens = lexer(&file);

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

fn to_d8(data: &LabelMap, label: &str, cur_addr: u32, base: u16) -> Option<State> {
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

fn to_d12(data: &LabelMap, label: &str, cur_addr: u32, base: u16) -> Option<State> {
	if !data.contains_key(label) {
		todo!("Unknown label '{label}'");
	}

	data[label].map(|lbl_addr| {
		let offset = lbl_addr as i64 - cur_addr as i64;
		let disp = offset / 2;
		if !(0x800_i16 as i64..=0x7FF_i16 as i64).contains(&disp) {
			todo!("Relative address too big! Switch to memory load and move?");
		}
		d12_type(base, disp as i16 as u16)
	})
}

fn resolver(data: &mut Output) -> bool {
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
					Incomplete(AddImm(imm,rn)) => results.push(ni_type(0b0111_0000_0000_0000, *rn, *imm)),
					// | Rm,Rn             | 0011nnnnmmmm1100 | Rn+Rm -> Rn        | 1       | -      |
					Incomplete(AddReg(rm,rn)) => results.push(nm_type(0b0111_0000_0000_1100, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1110 | Rn+Rm+T -> Rn,     | 1       | Carry  |
					// |                   |                  | Carry -> T         |         |        |
					Incomplete(AddC(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1110, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1111 | Rn+Rm -> Rn,       | 1       | Over   |
					// |                   |                  | Overflow -> T      |         |        |
					Incomplete(AddV(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1111, *rm, *rn)),
					// | #imm,R0           | 11001001iiiiiiii | R0 & imm -> R0     | 1       | -      |
					Incomplete(AndImm(imm)) => results.push(d_type(0b1100_1001_0000_0000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1001 | Rn & Rm -> Rn      | 1       | -      |
					Incomplete(AndReg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1001, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001101iiiiiiii | (R0+GBR) & imm     | 3       | -      |
					// |                   |                  | -> (R0+GBR)        |         |        |
					Incomplete(AndByte(imm)) => results.push(d_type(0b1100_1101_0000_0000, *imm)),
					// | label             | 10011011dddddddd | if T = 0,          | 3/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 1, nop      |         |        |
					Incomplete(Bf(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001011_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(Bf(label.clone())))),
					// | label             | 10001111dddddddd | if T = 0,          | 2/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 1, nop      |         |        |
					Incomplete(BfS(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001111_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BfS(label.clone())))),
					// | label             | 1010dddddddddddd | Delayed branch,    | 2       | -      |
					// |                   |                  | dispx2+PC -> PC    |         |        |
					Incomplete(Bra(label)) => to_d12(&data.labels, label, section_start + section_pos * 2, 0b1010_000000000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(Bra(label.clone())))),
					// | Rm                | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
					// |                   |                  | Rm+PC -> PC        |         |        |
					Incomplete(BraF(rm)) => results.push(m_type(0b0000_0000_0010_0011, *rm)),
					// | label             | 1011dddddddddddd | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | dispx2+PC -> PC    |         |        |
					Incomplete(Bsr(label)) => to_d12(&data.labels, label, section_start + section_pos * 2, 0b1011_000000000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(Bsr(label.clone())))),
					// | Rm                | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | Rm+PC -> PC        |         |        |
					Incomplete(BsrF(rm)) => results.push(m_type(0b0000_0000_0000_0011, *rm)),
					// | label             | 10001001dddddddd | if T = 1,          | 3/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 0, nop      |         |        |
					Incomplete(Bt(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001001_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(Bt(label.clone())))),
					// | label             | 10001101dddddddd | if T = 1,          | 2/1     | -      |
					// |                   |                  | dispx2+PC -> PC;   |         |        |
					// |                   |                  | if T = 0, nop      |         |        |
					Incomplete(BtS(label)) => to_d8(&data.labels, label, section_start + section_pos * 2, 0b10001101_00000000)
						.map(|com| results.push(com))
						.unwrap_or_else(|| results.push(Incomplete(BtS(label.clone())))),
					// |                   | 0000000000101000 | 0 -> MACH, MACL    | 1       | -      |
					Incomplete(ClrMac) => results.push(Complete(0b0000_0000_0010_1000)),
					// |                   | 0000000000001000 | 0 -> T             | 1       | 0      |
					Incomplete(ClrT) => results.push(Complete(0b0000_0000_0000_1000)),
					// | #imm,R0           | 10001000iiiiiiii | if R0 = imm,       | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpEqImm(imm)) => results.push(i_type(0b1000_1000_0000_0000, *imm)),
					// | Rm,Rn             | 0011nnnnmmmm0000 | if Rn = Rm,        | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpEqReg(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0000, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0011 | if Rn >= Rm with   | 1       | Result |
					// |                   |                  | signed data,       |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpGE(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0011, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0111 | if Rn > Rm with    | 1       | Result |
					// |                   |                  | signed data,       |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpGT(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0111, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0110 | if Rn > Rm with    | 1       | Result |
					// |                   |                  | unsigned data,     |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpHI(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0110, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0010 | if Rn >= Rm with   | 1       | Result |
					// |                   |                  | unsigned data,     |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpHS(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0010, *rm, *rn)),
					// | Rn                | 0100nnnn00010101 | if Rn > 0,         | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpPL(rn)) => results.push(n_type(0b0100_0000_0001_0101, *rn)),
					// | Rn                | 0100nnnn00010001 | if Rn > 0,         | 1       | Result |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(CmpPZ(rn)) => results.push(n_type(0b0100_0000_0001_0001, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1100 | if Rn & Rm have    | 1       | Result |
					// |                   |                  | an equivalent      |         |        |
					// |                   |                  | byte, 1 -> T       |         |        |
					Incomplete(CmpStr(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1100, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm0111 | MSB of Rn -> Q,    | 1       | Result |
					// |                   |                  | MSB of Rm -> M,    |         |        |
					// |                   |                  | M ^ Q -> T         |         |        |
					Incomplete(Div0S(rm,rn)) => results.push(nm_type(0b0010_0000_0000_0111, *rm, *rn)),
					// |                   | 0000000000011001 | 0 -> M/Q/T         | 1       | 0      |
					Incomplete(Div0U) => results.push(Complete(0b0000_0000_0001_1001)),
					// | Rm,Rn             | 0011nnnnmmmm0100 | Single-step        | 1       | Result |
					// |                   |                  | division (Rn/Rm)   |         |        |
					Incomplete(Div1(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0100, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1101 | Signed operation   | 2 to 4  | -      |
					// |                   |                  | of Rn x Rm ->      |         |        |
					// |                   |                  | MACH, MACL         |         |        |
					Incomplete(DMulS(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1101, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm0101 | Unsigned operation | 2 to 4  | -      |
					// |                   |                  | of Rn x Rm -> MACH |         |        |
					// |                   |                  | MACL               |         |        |
					Incomplete(DMulU(rm,rn)) => results.push(nm_type(0b0011_0000_0000_0101, *rm, *rn)),
					// | Rn                | 0100nnnn00010000 | Rn - 1 -> Rn, when | 1       | Result |
					// |                   |                  | Rn is 0, 1 -> T.   |         |        |
					// |                   |                  | When Rn is         |         |        |
					// |                   |                  | nonzero, 0 -> T    |         |        |
					Incomplete(Dt(rn)) => results.push(n_type(0b0100_0000_0001_0000, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1110 | A byte in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(ExtS(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1110, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1111 | A word in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(ExtS(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1111, *rm, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1100 | A byte in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(ExtU(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1100, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1101 | A word in Rm is    | 1       | -      |
					// |                   |                  | sign-extended ->   |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(ExtU(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1101, *rm, *rn)),
					// | @Rm               | 0100mmmm00101011 | Delayed branch,    | 2       | -      |
					// |                   |                  | Rm -> PC           |         |        |
					Incomplete(Jmp(rm)) => results.push(m_type(0b0100_0000_0010_1011, *rm)),
					// | @Rm               | 0100mmmm00001011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PC -> PR,          |         |        |
					// |                   |                  | Rm -> PC           |         |        |
					Incomplete(Jsr(rm)) => results.push(m_type(0b0100_0000_0000_1011, *rm)),
					// | Rm,GBR            | 0100mmmm00011110 | Rm -> GBR          | 1       | -      |
					Incomplete(LdcGBR(rm)) => results.push(m_type(0b0100_0000_0001_1110, *rm)),
					// | Rm,SR             | 0100mmmm00001110 | Rm -> SR           | 1       | LSB    |
					Incomplete(LdcSR(rm)) => results.push(m_type(0b0100_0000_0000_1110, *rm)),
					// | Rm,VBR            | 0100mmmm00101110 | Rm -> VBR          | 1       | -      |
					Incomplete(LdcVBR(rm)) => results.push(m_type(0b0100_0000_0010_1110, *rm)),
					// | @Rm+,GBR          | 0100mmmm00010111 | (Rm) -> GBR,       | 3       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdcGBR_Inc(rm)) => results.push(m_type(0b0100_0000_0001_0111, *rm)),
					// | @Rm+,SR           | 0100mmmm00000111 | (Rm) -> SR,        | 3       | LSB    |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdcSR_Inc(rm)) => results.push(m_type(0b0100_0000_0000_0111, *rm)),
					// | @Rm+,VBR          | 0100mmmm00100111 | (Rm) -> VBR,       | 3       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdcVBR_Inc(rm)) => results.push(m_type(0b0100_0000_0010_0111, *rm)),
					// | Rm,MACH           | 0100mmmm00001010 | Rm -> MACH         | 1       | -      |
					Incomplete(LdsMACH(rm)) => results.push(m_type(0b0100_0000_0000_1010, *rm)),
					// | Rm,MACL           | 0100mmmm00011010 | Rm -> MACL         | 1       | -      |
					Incomplete(LdsMACL(rm)) => results.push(m_type(0b0100_0000_0001_1010, *rm)),
					// | Rm,PR             | 0100mmmm00101010 | Rm -> PR           | 1       | -      |
					Incomplete(LdsPR(rm)) => results.push(m_type(0b0100_0000_0010_1010, *rm)),
					// | @Rm+,MACH         | 0100mmmm00000110 | (Rm) -> MACH,      | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdsMACH_Inc(rm)) => results.push(m_type(0b0100_0000_0000_0110, *rm)),
					// | @Rm+,MACL         | 0100mmmm00010110 | (Rm) -> MACL,      | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdsMACL_Inc(rm)) => results.push(m_type(0b0100_0000_0001_0110, *rm)),
					// | @Rm+,PR           | 0100mmmm00100110 | (Rm) -> PR,        | 1       | -      |
					// |                   |                  | Rm + 4 -> Rm       |         |        |
					Incomplete(LdsPR_Inc(rm)) => results.push(m_type(0b0100_0000_0010_0110, *rm)),
					// | @Rm+,@Rn+         | 0000nnnnmmmm1111 | Signed operation   | 3/(2-4) | -      |
					// |                   |                  | of (Rn) x (Rm) +   |         |        |
					// |                   |                  | MAC -> MAC         |         |        |
					Incomplete(MacLong(rm,rn)) => results.push(nm_type(0b0000_0000_0000_1111, *rm, *rn)),
					// | @Rm+,@Rn+         | 0100nnnnmmmm1111 | Signed operation   | 3/(2)   | -      |
					// |                   |                  | of (Rn) x (Rm) +   |         |        |
					// |                   |                  | MAC -> MAC         |         |        |
					Incomplete(MacWord(rm,rn)) => results.push(nm_type(0b0100_0000_0000_1111, *rm, *rn)),
					// | #imm,Rn           | 1110nnnniiiiiiii | imm -> Sign        | 1       | -      |
					// |                   |                  | extension -> Rn    |         |        |
					// TODO - srenshaw - Need to decide how to handle non-8-bit MOV immediate mnemonics
					Incomplete(MovImm(imm,rn)) => {
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
					Incomplete(MovReg(rm,rn)) => results.push(nm_type(0b0110_0000_0000_0011, *rm, *rn)),
					Incomplete(Mov(Word,Arg::Label(lsrc),DirReg(rdst))) => {
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
							results.push(Incomplete(Mov(Word,DispPC(disp),DirReg(*rdst))));
							is_resolved = false;
						} else {
							results.push(Incomplete(Mov(Word,Arg::Label(lsrc.clone()),DirReg(*rdst))));
							is_resolved = false;
						}
					}
					Incomplete(Mov(Long,Arg::Label(lsrc),DirReg(rdst))) => {
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
							results.push(Incomplete(Mov(Long,DispPC(disp),DirReg(*rdst))));
							is_resolved = false;
						} else {
							results.push(Incomplete(Mov(Long,Arg::Label(lsrc.clone()),DirReg(*rdst))));
							is_resolved = false;
						}
					}
					Incomplete(Mov(Word,DispPC(disp),DirReg(rn))) => results.push(nd8_type(0b1001_0000_0000_0000, *rn, *disp as u8)),
					Incomplete(Mov(Long,DispPC(disp),DirReg(rn))) => results.push(nd8_type(0b1101_0000_0000_0000, *rn, *disp as u8)),
					Incomplete(Mov(Byte,DirReg(rm),DirReg(rn))) |
					Incomplete(Mov(Word,DirReg(rm),DirReg(rn))) |
					Incomplete(Mov(Long,DirReg(rm),DirReg(rn))) => results.push(nm_type(0b0110_0000_0000_0011, *rm, *rn)),
					Incomplete(Mov(size,DirReg(rsrc),IndReg(rdst))) => {
						let base = 0b0010_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size,IndReg(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size,DirReg(rsrc),PreDec(rdst))) => {
						let base = 0b0010_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size,PostInc(rsrc),DirReg(rdst))) => {
						let base = 0b0110_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size @ Byte,DirReg(0),DispReg(disp,rdst))) |
					Incomplete(Mov(size @ Word,DirReg(0),DispReg(disp,rdst))) => {
						let base = 0b1000_0000_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let nbyte = to_byte3(*rdst);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | nbyte | dbyte));
					}
					Incomplete(Mov(Long,DirReg(rsrc),DispReg(disp,rdst))) => {
						let base = 0b0001_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(Mov(size @ Byte,DispReg(disp,rsrc),DirReg(0))) |
					Incomplete(Mov(size @ Word,DispReg(disp,rsrc),DirReg(0))) => {
						let base = 0b1000_0100_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | sbyte | mbyte | dbyte));
					}
					Incomplete(Mov(Long,DispReg(disp,rsrc),DirReg(rdst))) => {
						let base = 0b0101_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let dbyte = (*disp as u8 as u16) & 0x0F;
						results.push(Complete(base | nbyte | mbyte | dbyte));
					}
					Incomplete(Mov(size,DirReg(rsrc),DispR0(rdst))) => {
						let base = 0b0000_0000_0000_0100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size,DispR0(rsrc),DirReg(rdst))) => {
						let base = 0b0000_0000_0000_1100;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						let sbyte = to_sbyte(size);
						results.push(Complete(base | nbyte | mbyte | sbyte));
					}
					Incomplete(Mov(size,DirReg(0),DispGBR(disp))) => {
						let base = 0b1100_0000_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let dword = *disp as u8 as u16;
						results.push(Complete(base | sbyte | dword));
					}
					Incomplete(Mov(size,DispGBR(disp),DirReg(0))) => {
						let base = 0b1100_0100_0000_0000;
						let sbyte = to_sbyte(size) << 8;
						let dword = *disp as u8 as u16;
						results.push(Complete(base | sbyte | dword));
					}
					Incomplete(Mov(Long,DispLabel(lbl,rsrc),DirReg(rdst))) => {
						if !data.values.contains_key(lbl) {
							todo!("unknown value: '{lbl}'");
						}
						let disp = data.values[lbl];
						let disp = disp >> 2;
						if !(-8..7).contains(&disp) {
							todo!("value too large to use as displacement: {disp}");
						}
						let dbyte = (disp as u16) & 0x000F;
						let base = 0b0101_0000_0000_0000;
						let nbyte = to_byte2(*rdst);
						let mbyte = to_byte3(*rsrc);
						results.push(Complete(base | nbyte | mbyte | dbyte))
					}
					// | @(disp,PC),R0     | 11000111dddddddd | disp x 4+PC -> R0  | 1       | -      |
					Incomplete(MovA(disp)) => results.push(d_type(0b1100_0111_0000_0000, *disp as u8)),
					// | Rn                | 0000nnnn00101001 | T -> Rn            | 1       | -      |
					Incomplete(MovT(rn)) => results.push(n_type(0b0000_0000_0010_1001, *rn)),
					// | Rm,Rn             | 0000nnnnmmmm0111 | Rn x Rm -> MACL    | 2 to 4  | -      |
					Incomplete(Mul(rm,rn)) => results.push(nm_type(0b0000_0000_0000_0111, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1111 | Signed operation   | 1 to 3  | -      |
					// |                   |                  | of Rn x Rm -> MAC  |         |        |
					Incomplete(MulS(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1111, *rm, *rn)),
					// | Rm,Rn             | 0010nnnnmmmm1110 | Unsigned operation | 1 to 3  | -      |
					// |                   |                  | of Rn x Rm -> MAC  |         |        |
					Incomplete(MulU(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1110, *rm, *rn)),
					// | Rm,Rn             | 0110nnnnmmmm1011 | 0 - Rm -> Rn       | 1       | -      |
					Incomplete(Neg(rm,rn)) => results.push(nm_type(0b0110_0000_0000_1011, *rm, *rn)),
					// | Rm,Rn             | 0110nnnnmmmm1010 | 0 - Rm - T -> Rn,  | 1       | Borrow |
					// |                   |                  | Borrow -> T        |         |        |
					Incomplete(NegC(rm,rn)) => results.push(nm_type(0b0110_0000_0000_1010, *rm, *rn)),
					// |                   | 0000000000001001 | No operation       | 1       | -      |
					Incomplete(Nop) => results.push(Complete(0b0000_0000_0000_1001)),
					// | Rm,Rn             | 0110nnnnmmmm0111 | ~Rm -> Rn          | 1       | -      |
					Incomplete(Not(rm,rn)) => results.push(nm_type(0b0110_0000_0000_0111, *rm, *rn)),
					// | #imm,R0           | 11001011iiiiiiii | R0|imm -> R0       | 1       | -      |
					Incomplete(OrImm(imm)) => results.push(d_type(0b1100_1011_0000_0000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1011 | Rn|Rm -> Rn        | 1       | -      |
					Incomplete(OrReg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1011, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001111iiiiiiii | (R0+GBR)|imm ->    | 3       | -      |
					// |                   |                  | (R0+GBR)           |         |        |
					Incomplete(OrByte(imm)) => results.push(d_type(0b1100_1111_0000_0000, *imm)),
					// | Rn                | 0100nnnn00100100 | T <- Rn <- T       | 1       | MSB    |
					Incomplete(RotCL(rn)) => results.push(n_type(0b0100_0000_0010_0100, *rn)),
					// | Rn                | 0100nnnn00100101 | T -> Rn -> T       | 1       | LSB    |
					Incomplete(RotCR(rn)) => results.push(n_type(0b0100_0000_0010_0101, *rn)),
					// | Rn                | 0100nnnn00000100 | T <- Rn <- MSB     | 1       | MSB    |
					Incomplete(RotL(rn)) => results.push(n_type(0b0100_0000_0000_0100, *rn)),
					// | Rn                | 0100nnnn00000101 | LSB -> Rn -> T     | 1       | LSB    |
					Incomplete(RotR(rn)) => results.push(n_type(0b0100_0000_0000_0101, *rn)),
					// |                   | 0000000000101011 | Delayed branch,    | 4       | LSB    |
					// |                   |                  | stack area -> PC/SR |        |        |
					Incomplete(Rte) => {}
					// |                   | 0000000000001011 | Delayed branch,    | 2       | -      |
					// |                   |                  | PR -> PC           |         |        |
					Incomplete(Rts) => {}
					// |                   | 0000000000011000 | 1 -> T             | 1       | 1      |
					Incomplete(SetT) => {}
					// | Rn                | 0100nnnn00100000 | T <- Rn <- 0       | 1       | MSB    |
					Incomplete(ShAL(rn)) => results.push(n_type(0b0100_0000_0010_0000, *rn)),
					// | Rn                | 0100nnnn00100001 | MSB -> Rn -> T     | 1       | LSB    |
					Incomplete(ShAR(rn)) => results.push(n_type(0b0100_0000_0010_0001, *rn)),
					// | Rn                | 0100nnnn00000000 | T <- Rn <- 0       | 1       | MSB    |
					Incomplete(ShLL(rn)) => results.push(n_type(0b0100_0000_0000_0000, *rn)),
					// | Rn                | 0100nnnn00001000 | Rn << 2 -> Rn      | 1       | -      |
					Incomplete(ShLL2(rn)) => results.push(n_type(0b0100_0000_0000_1000, *rn)),
					// | Rn                | 0100nnnn00011000 | Rn << 8 -> Rn      | 1       | -      |
					Incomplete(ShLL8(rn)) => results.push(n_type(0b0100_0000_0001_1000, *rn)),
					// | Rn                | 0100nnnn00101000 | Rn << 16 -> Rn     | 1       | -      |
					Incomplete(ShLL16(rn)) => results.push(n_type(0b0100_0000_0010_1000, *rn)),
					// | Rn                | 0100nnnn00000001 | 0 -> Rn -> T       | 1       | LSB    |
					Incomplete(ShLR(rn)) => results.push(n_type(0b0100_0000_0000_0001, *rn)),
					// | Rn                | 0100nnnn00001001 | Rn >> 2 -> Rn      | 1       | -      |
					Incomplete(ShLR2(rn)) => results.push(n_type(0b0100_0000_0000_1001, *rn)),
					// | Rn                | 0100nnnn00011001 | Rn >> 8 -> Rn      | 1       | -      |
					Incomplete(ShLR8(rn)) => results.push(n_type(0b0100_0000_0001_1001, *rn)),
					// | Rn                | 0100nnnn00101001 | Rn >> 16 -> Rn     | 1       | -      |
					Incomplete(ShLR16(rn)) => results.push(n_type(0b0100_0000_0010_1001, *rn)),
					// |                   | 0000000000011011 | Sleep              | 3       | -      |
					Incomplete(Sleep) => {}
					// | GBR,Rn            | 0000nnnn00010010 | GBR -> Rn          | 1       | -      |
					Incomplete(StcGBR(rn)) => results.push(n_type(0b0000_0000_0001_0010, *rn)),
					// | SR,Rn             | 0000nnnn00000010 | SR -> Rn           | 1       | -      |
					Incomplete(StcSR(rn)) => results.push(n_type(0b0000_0000_0000_0010, *rn)),
					// | VBR,Rn            | 0000nnnn00100010 | VBR -> Rn          | 1       | -      |
					Incomplete(StcVBR(rn)) => results.push(n_type(0b0000_0000_0010_0010, *rn)),
					// | GBR,@-Rn          | 0100nnnn00010011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | GBR -> (Rn)        |         |        |
					Incomplete(StcGBR_Dec(rn)) => results.push(n_type(0b0100_0000_0001_0011, *rn)),
					// | SR,@-Rn           | 0100nnnn00000011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | SR -> (Rn)         |         |        |
					Incomplete(StcSR_Dec(rn)) => results.push(n_type(0b0100_0000_0000_0011, *rn)),
					// | VBR,@-Rn          | 0100nnnn00100011 | Rn - 4 -> Rn,      | 2       | -      |
					// |                   |                  | VBR -> (Rn)        |         |        |
					Incomplete(StcVBR_Dec(rn)) => results.push(n_type(0b0100_0000_0010_0011, *rn)),
					// | MACH,Rn           | 0000nnnn00001010 | MACH -> Rn         | 1       | -      |
					Incomplete(StsMACH(rn)) => results.push(n_type(0b0000_0000_0000_1010, *rn)),
					// | MACL,Rn           | 0000nnnn00011010 | MACL -> Rn         | 1       | -      |
					Incomplete(StsMACL(rn)) => results.push(n_type(0b0000_0000_0001_1010, *rn)),
					// | PR,Rn             | 0000nnnn00101010 | PR -> Rn           | 1       | -      |
					Incomplete(StsPR(rn)) => results.push(n_type(0b0000_0000_0010_1010, *rn)),
					// | MACH,@-Rn         | 0100nnnn00000010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | MACH -> (Rn)       |         |        |
					Incomplete(StsMACH_Dec(rn)) => results.push(n_type(0b0100_0000_0000_0010, *rn)),
					// | MACL,@-Rn         | 0100nnnn00010010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | MACL -> (Rn)       |         |        |
					Incomplete(StsMACL_Dec(rn)) => results.push(n_type(0b0100_0000_0001_0010, *rn)),
					// | PR,@-Rn           | 0100nnnn00100010 | Rn - 4 -> Rn,      | 1       | -      |
					// |                   |                  | PR -> (Rn)         |         |        |
					Incomplete(StsPR_Dec(rn)) => results.push(n_type(0b0100_0000_0010_0010, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1000 | Rn - Rm -> Rn      | 1       | -      |
					Incomplete(Sub(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1000, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1010 | Rn - Rm - T -> Rn, | 1       | Borrow |
					// |                   |                  | Borrow -> T        |         |        |
					Incomplete(SubC(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1010, *rm, *rn)),
					// | Rm,Rn             | 0011nnnnmmmm1011 | Rn - Rm -> Rn,     | 1       | Under  |
					// |                   |                  | Underflow -> T     |         |        |
					Incomplete(SubV(rm,rn)) => results.push(nm_type(0b0011_0000_0000_1011, *rm, *rn)),
					// | .B Rm,Rn          | 0110nnnnmmmm1000 | Rm -> Swap upper   | 1       | -      |
					// |                   |                  | and lower 2 bytes  |         |        |
					// |                   |                  | -> Rn              |         |        |
					Incomplete(Swap(Byte,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1000, *rm, *rn)),
					// | .W Rm,Rn          | 0110nnnnmmmm1001 | Rm -> Swap upper   | 1       | -      |
					// |                   |                  | and lower word ->  |         |        |
					// |                   |                  | Rn                 |         |        |
					Incomplete(Swap(Word,rm,rn)) => results.push(nm_type(0b0110_0000_0000_1001, *rm, *rn)),
					// | @Rn               | 0100nnnn00011011 | if (Rn) is 0, 1->T | 4       | Result |
					// |                   |                  | 1 -> MSB of (Rn)   |         |        |
					Incomplete(Tas(rn)) => results.push(n_type(0b0100_0000_0001_1011, *rn)),
					// | #imm              | 11000011iiiiiiii | PC/SR -> stack     | 8       | -      |
					// |                   |                  | area, (imm x 4 +   |         |        |
					// |                   |                  | VBR) -> PC         |         |        |
					Incomplete(TrapA(imm)) => results.push(d_type(0b1100_0011_0000_0000, *imm)),
					// | #imm,R0           | 11001000iiiiiiii | R0 & imm; if the   | 1       | Result |
					// |                   |                  | result is 0, 1->T  |         |        |
					Incomplete(Tst_Imm(imm)) => results.push(d_type(0b1100_1000_0000_0000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1000 | Rn & Rm; if the    | 1       | Result |
					// |                   |                  | result is 0, 1->T  |         |        |
					Incomplete(Tst_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1000, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001100iiiiiiii | (R0+GBR) & imm; if | 3       | Result |
					// |                   |                  | the result is 0,   |         |        |
					// |                   |                  | 1 -> T             |         |        |
					Incomplete(Tst_Byte(imm)) => results.push(d_type(0b1100_1100_0000_0000, *imm)),
					// | #imm,R0           | 11001010iiiiiiii | R0 ^ imm -> R0     | 1       | -      |
					Incomplete(Xor_Imm(imm)) => results.push(d_type(0b1100_1010_0000_0000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1010 | Rn ^ Rm -> Rn      | 1       | -      |
					Incomplete(Xor_Reg(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1010, *rm, *rn)),
					// | #imm,@(R0,GBR)    | 11001110iiiiiiii | (R0+GBR) ^ imm ->  | 3       | -      |
					// |                   |                  | (R0+GBR)           |         |        |
					Incomplete(Xor_Byte(imm)) => results.push(d_type(0b1100_1110_0000_0000, *imm)),
					// | Rm,Rn             | 0010nnnnmmmm1101 | Center 32 bits of  | 1       | -      |
					// |                   |                  | Rm and Rn -> Rn    |         |        |
					Incomplete(Xtrct(rm,rn)) => results.push(nm_type(0b0010_0000_0000_1101, *rm, *rn)),

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
					Incomplete(Const_Label(Byte,lbl)) => eprintln!("Attempting to declare a byte constant '{lbl}'. This doesn't work currently."),
					Incomplete(Const_Label(Word,lbl)) => eprintln!("Attempting to declare a word constant '{lbl}'. This doesn't work currently."),
					Incomplete(Const_Label(Long,lbl)) => eprintln!("Attempting to declare a long constant '{lbl}'. This doesn't work currently."),
					Incomplete(Ins::Label(label)) => {
						if !data.labels.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(addr) = data.labels[label] {
							todo!("Label '{label}' already defined to {addr:08X}");
						}
						data.labels.insert(label.clone(),
							Some(section_start + results.len() as u32 * 2));
					}

					Complete(_) => results.push(instr.clone()),
					_ => todo!("Invalid instruction: {instr:?}"),
				}
			}

			(section_start as u64, results)
		}).collect();

	is_resolved
}
