
use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::TokenType;

mod parser;
use parser::parser;
use parser::{Arg,Ins,Reg,Output,Size,State};

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

	let mut data = match parser(&file, &tokens) {
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

fn to_byte2(reg: &Reg) -> u16 {
	(*reg as u16) << 12
}

fn to_byte3(reg: &Reg) -> u16 {
	(*reg as u16) << 8
}

fn to_sbyte(size: &Size) -> u16 {
	match size {
		Size::Byte => 0b00,
		Size::Word => 0b01,
		Size::Long => 0b10,
	}
}

fn resolver(
	data: &mut Output,
) -> bool {
	let mut is_resolved = true;

	data.sections = data.sections.iter()
		.map(|(&section_start, section)| {
			let mut results = Vec::with_capacity(section.len());
			for instr in section {
				use parser::Arg::*;
				use parser::Ins::*;
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
					Incomplete(BF(label)) => {
						if !data.labels.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(lbl_addr) = data.labels[label] {
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
						if !data.labels.contains_key(label) {
							todo!("Unknown label '{label}'");
						}
						if let Some(addr) = data.labels[label] {
							todo!("Label '{label}' already defined to {addr:08X}");
						}
						data.labels.insert(label.clone(),
							Some(section_start as u32 + results.len() as u32 * 2));
					}
					Incomplete(MOV(Word,Arg::Label(lsrc),DirReg(rdst))) => {
						if !data.labels.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = data.labels[lsrc] {
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
						if !data.labels.contains_key(lsrc) {
							todo!("Unknown label '{lsrc}'");
						}
						if let Some(lbl_addr) = data.labels[lsrc] {
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
					Incomplete(MOV_Imm(isrc,rdst)) => {
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
							let iword = *isrc as u16;
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

