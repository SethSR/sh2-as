
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::asm::Asm;
use crate::tokens::{Token, Type};

#[derive(Default)]
pub struct Parser {
	index: usize,
	file_path: PathBuf,

	is_recording_macro: Option<Box<str>>,
	macro_buffer: Vec<Option<Asm>>,
	macros: HashMap<Box<str>, Vec<Option<Asm>>>,
	undefined_macro_labels: HashMap<Box<str>, Vec<usize>>,

	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
	undefined_ins_labels: HashMap<Box<str>, Vec<usize>>,

	address: u32,
	asm: Vec<(u32, Option<Asm>)>,
}

pub fn eval(tokens: &[Token], file_path: PathBuf) -> Result<Parser, String> {
	let mut parser = Parser::default();
	parser.file_path = file_path;

	parser.parse(tokens);

	Ok(parser)
}

impl Parser {
	pub fn output(&self) -> Vec<Asm> {
		let mut out = vec![];
		for (_,asm) in &self.asm {
			match asm {
				Some(asm) => {
					out.push(asm.clone());
				}
				None => {
					let start = self.index.saturating_sub(5);
					let end = self.index.saturating_add(5).min(self.asm.len());
					let window = self.asm[start..=end].iter()
						.map(|s| format!("{s:?}"))
						.collect::<Vec<_>>()
						.join("\n\t");
					panic!("Unfinished instruction @ index {}\nwindow:\n\t{window}", self.index);
				}
			}
		}
		out
	}

	fn push(&mut self, asm: Asm) {
		if self.is_recording_macro.is_some() {
			self.macro_buffer.push(Some(asm));
		} else {
			self.asm.push((self.address, Some(asm)));
			self.address += 1;
		}
	}

	fn push_label(&mut self, label: Box<str>) {
		if self.is_recording_macro.is_some() {
			self.undefined_macro_labels.entry(label)
				.or_default()
				.push(self.macro_buffer.len());
			self.macro_buffer.push(None);
		} else {
			self.undefined_ins_labels.entry(label)
				.or_default()
				.push(self.asm.len());
			self.asm.push((self.address, None));
			self.address += 1;
		}
	}

	fn parse(&mut self, tokens: &[Token]) {
		while tokens[self.index].tt != Type::Eof {
			let token = &tokens[self.index];

			match token.tt {
				Type::Label(ref s) => {
					let s: Box<str> = s.to_lowercase().into();
					match self.next(tokens).tt {
						Type::Eq => {
							let msg = format!("found label '{s}' with value");
							match self.next(tokens).tt {
								Type::Bin(n) => {
									println!("{msg} {}", dbg_bin(&n));
									let n = i64::from_str_radix(&n, 2).unwrap();
									self.values.insert(s, n);
								}
								Type::Dec(n) => {
									println!("{msg} {}", dbg_dec(&n));
									let n = i64::from_str_radix(&n, 10).unwrap();
									self.values.insert(s, n);
								}
								Type::Hex(n) => {
									println!("{msg} {}", dbg_hex(&n));
									let n = i64::from_str_radix(&n, 16).unwrap();
									self.values.insert(s, n);
								}
								_ => {
									self.unexpected_next(tokens, token);
								}
							}
						}
						Type::Colon => {
							println!("found label '{s}'");
							if self.is_recording_macro.is_some() {
								if let Some(indexes) = self.undefined_macro_labels.get(&s) {
									for idx in indexes {
										println!("undefined macro label: '{:?}'", self.asm[*idx]);
									}
								}
							} else {
								if let Some(indexes) = self.undefined_ins_labels.get(&s) {
									for idx in indexes {
										println!("undefined instruction label: '{:?}'", self.asm[*idx]);
									}
								}
							}
							self.labels.insert(s, self.address);
						}
						Type::MacroStart => {
							println!("found macro start for '{s}' - {:?}", self.macros);
							self.is_recording_macro = Some(s);
						}
						_ => {
							self.index -= 1;
							if self.macros.contains_key(&s) {
								println!("found macro use for '{s}'");
							} else {
								println!("Label ERROR '{s}' {:?}", self.macros);
								self.unexpected_next(tokens, token);
							}
						}
					}
				}

				Type::MacroEnd => {
					if let Some(s) = self.is_recording_macro.take() {
						println!("found macro end for '{s}' - {:?}", self.macros);
						self.macros.insert(s, self.macro_buffer.clone());
						self.macro_buffer.clear();
					} else {
						todo!("found ENDM with no preceding MACRO directive")
					}
				}

				/* Directives */
				Type::Org => {
					let msg = "setting current address";
					match self.next(tokens).tt {
						Type::Bin(n) => {
							println!("{msg} {}", dbg_bin(&n));
							self.address = u32::from_str_radix(&n, 2).unwrap();
						}
						Type::Dec(n) => {
							println!("{msg} {}", dbg_dec(&n));
							self.address = u32::from_str_radix(&n, 10).unwrap();
						}
						Type::Hex(n) => {
							println!("{msg} {}", dbg_hex(&n));
							self.address = u32::from_str_radix(&n, 16).unwrap();
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				}

				Type::Include => {
					if let Ok(s) = self.match_string(tokens) {
						let s = Path::join(&self.file_path, &*s);
						println!("include ASM file '{}'", s.display());
						let file = std::fs::read_to_string(&s)
							.expect(&format!("unable to open file '{}'", s.display()));
						let file_tokens = crate::lexer::eval(&file).unwrap();

						let index = self.index;
						self.index = 0;
						self.parse(&file_tokens);
						self.index = index;
					} else {
						self.unexpected_next(tokens, token);
					}
				}

				Type::BInclude => {
					if let Ok(s) = self.match_string(tokens) {
						let s = Path::join(&self.file_path, &*s);
						println!("include BINARY file '{}'", s.display());
						self.asm.extend(std::fs::read(&s)
							.expect(&format!("unable to open file '{}'", s.display()))
							.into_iter()
							.map(|byte| {
								self.address += 1;
								(self.address - 1, Some(Asm::Byte(byte)))
							}));
					} else {
						self.unexpected_next(tokens, token);
					}
				}

				Type::Align => {
					let n = self.match_immediate(tokens).unwrap();
					println!("aligning to the next {n}-byte boundary");
					let n = n as u32;
					self.address += n - (self.address % n);
				}

				Type::Const => {
					if self.match_token(tokens, Type::Dot).is_ok() {
						match self.next(tokens).tt {
							Type::Byte => {
								if let Ok(s) = self.match_string(tokens) {
									println!("declaring byte-string '{s}'");

									for c in s.bytes() {
										self.push(Asm::Byte(c));
									}

									while self.match_token(tokens, Type::Comma).is_ok() {
										if let Ok(n) = self.match_immediate(tokens) {
											assert!(u8_sized(n));
											println!("- with extra byte {n}");
											self.push(Asm::Byte(n as u8));
										}
									}
								} else {
									self.unexpected_next(tokens, token);
								}
							}
							Type::Word => {
								match self.next(tokens).tt {
									Type::Dash => {
										let n = self.match_immediate(tokens).unwrap();
										assert!(u16_sized(n));
										println!("declaring constant negative {n}");
										self.push(Asm::Word((-n) as u16));
									}
									Type::Bin(n) => {
										println!("declaring constant {}", dbg_bin(&n));
										let n = u16::from_str_radix(&n, 2).unwrap();
										self.push(Asm::Word(n));
									}
									Type::Dec(n) => {
										println!("declaring constant {}", dbg_dec(&n));
										let n = u16::from_str_radix(&n, 10).unwrap();
										self.push(Asm::Word(n));
									}
									Type::Hex(n) => {
										println!("declaring constant {}", dbg_hex(&n));
										let n = u16::from_str_radix(&n, 16).unwrap();
										self.push(Asm::Word(n));
									}
									_ => {
										self.unexpected_next(tokens, token);
									}
								}
							}
							Type::Long => {
								match self.next(tokens).tt {
									Type::Label(s) => {
										println!("declaring constant with address of label '{s}'");
										if let Some(n) = self.labels.get(&s) {
											self.push(Asm::Long(*n));
										} else {
											println!("found unknown label: '{s}'");
											self.push_label(s);
										}
									}
									Type::Bin(n) => {
										println!("declaring constant {}", dbg_bin(&n));
										let n = u32::from_str_radix(&n, 2).unwrap();
										self.push(Asm::Long(n));
									}
									Type::Dec(n) => {
										println!("declaring constant {}", dbg_dec(&n));
										let n = u32::from_str_radix(&n, 10).unwrap();
										self.push(Asm::Long(n));
									}
									Type::Hex(n) => {
										println!("declaring constant {}", dbg_hex(&n));
										let n = u32::from_str_radix(&n, 16).unwrap();
										self.push(Asm::Long(n));
									}
									_ => {
										self.unexpected_next(tokens, token);
									}
								}
							}
							_ => {
								self.unexpected_next(tokens, token);
							}
						}
					} else {
						self.unexpected_next(tokens, token);
					}
				}

				Type::Space => {}

				Type::LtOrg => {
					println!("emptying literal pool");
				}

				/* Instructions */
				Type::ClrMac => self.push(Asm::ClrMac),
				Type::ClrT => self.push(Asm::ClrT),
				Type::Div0U => self.push(Asm::Div0U),
				Type::Nop => self.push(Asm::Nop),
				Type::Rte => self.push(Asm::Rte),
				Type::Rts => self.push(Asm::Rts),
				Type::SetT => self.push(Asm::SetT),
				Type::Sleep => self.push(Asm::Sleep),

				Type::Bf => {
					let s = self.match_label(tokens).unwrap();
					println!("branching on clear T-flag to label '{s}'");
				}
				Type::BfS => {}

				Type::Bra => {
					let next = self.next(tokens);
					match next.tt {
						Type::Label(s) => {
							println!("branching to location @ '{s}'");
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				}

				Type::BraF => {}

				Type::Bsr => {
					match self.next(tokens).tt {
						Type::Label(s) => {
							println!("branching to subroutine '{s}'");
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				}

				Type::BsrF => {}
				Type::Bt => {
					let s = self.match_label(tokens).unwrap();
					println!("branching on set T-flag to label '{s}'");
				}
				Type::BtS => {}
				Type::Dt => {
					let r = self.match_reg(tokens).unwrap();
					println!("decrementing and testing {}", dbg_reg(r));
					self.push(Asm::Dt(r));
				}
				Type::Jmp => {}
				Type::Jsr => {}
				Type::MovA => {}
				Type::MovT => {}
				Type::RotCL => {
					let r = self.match_reg(tokens).unwrap();
					println!("rotating {} left into C-flag", dbg_reg(r));
					self.push(Asm::RotCL(r));
				}
				Type::RotCR => {
					let r = self.match_reg(tokens).unwrap();
					println!("rotating {} right into C-flag", dbg_reg(r));
					self.push(Asm::RotCR(r));
				}
				Type::RotL => {
					let r = self.match_reg(tokens).unwrap();
					println!("rotating {} left", dbg_reg(r));
					self.push(Asm::RotL(r));
				}
				Type::RotR => {
					let r = self.match_reg(tokens).unwrap();
					println!("rotating {} right", dbg_reg(r));
					self.push(Asm::RotR(r));
				}
				Type::ShAL => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} arithmetically left", dbg_reg(r));
					self.push(Asm::ShAL(r));
				}
				Type::ShAR => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} arithmetically right", dbg_reg(r));
					self.push(Asm::ShAR(r));
				}
				Type::ShLL => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} left 1 bit", dbg_reg(r));
					self.push(Asm::ShLL(r));
				}
				Type::ShLL2 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} left 2 bits", dbg_reg(r));
					self.push(Asm::ShLL2(r));
				}
				Type::ShLL8 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} left 8 bits", dbg_reg(r));
					self.push(Asm::ShLL8(r));
				}
				Type::ShLL16 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} left 16 bits", dbg_reg(r));
					self.push(Asm::ShLL16(r));
				}
				Type::ShLR => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} right 1 bit", dbg_reg(r));
					self.push(Asm::ShLR(r));
				}
				Type::ShLR2 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} right 2 bits", dbg_reg(r));
					self.push(Asm::ShLR2(r));
				}
				Type::ShLR8 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} right 8 bits", dbg_reg(r));
					self.push(Asm::ShLR8(r));
				}
				Type::ShLR16 => {
					let r = self.match_reg(tokens).unwrap();
					println!("shifting {} right 16 bits", dbg_reg(r));
					self.push(Asm::ShLR16(r));
				}
				Type::Tas => {}
				Type::TrapA => {}

				Type::AddC => {}
				Type::AddV => {}
				Type::Div0S => {}
				Type::Div1 => {}
				Type::ExtS => {}
				Type::ExtU => {}
				Type::Mac => {}
				Type::Neg => {}
				Type::NegC => {}
				Type::Not => {}

				Type::Sub => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("subtracting {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::Sub(rm,rn));
				}

				Type::SubC => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("subtracting w/ carry {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::SubC(rm,rn));
				}

				Type::SubV => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("subtracting w/ overflow {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::SubV(rm,rn));
				}

				Type::Swap => {
					if self.match_token(tokens, Type::Dot).is_ok() {
						match self.next(tokens).tt {
							Type::Byte => {
								let rm = self.match_reg(tokens).unwrap();
								self.match_token(tokens, Type::Comma).unwrap();
								let rn = self.match_reg(tokens).unwrap();
								println!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
								self.push(Asm::SwapByte(rm,rn));
							}
							Type::Word => {
								let rm = self.match_reg(tokens).unwrap();
								self.match_token(tokens, Type::Comma).unwrap();
								let rn = self.match_reg(tokens).unwrap();
								println!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
								self.push(Asm::SwapWord(rm,rn));
							}
							_ => {
								self.unexpected_next(tokens, token);
							}
						}
					} else {
						let rm = self.match_reg(tokens).unwrap();
						self.match_token(tokens, Type::Comma).unwrap();
						let rn = self.match_reg(tokens).unwrap();
						println!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
						self.push(Asm::SwapWord(rm,rn));
					}
				}

				Type::Xtrct => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("extracting {} into {}", dbg_reg(rm), dbg_reg(rn));
				}

				Type::And => self.cmp(tokens, "anding", token),
				Type::Or => self.cmp(tokens, "oring", token),
				Type::Tst => self.cmp(tokens, "testing", token),
				Type::Xor => self.cmp(tokens, "xoring", token),

				Type::CmpEq => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpGe => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpGt => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpHi => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpHs => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpStr => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpPl => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}
				Type::CmpPz => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("comparing {} into {}", dbg_reg(rm), dbg_reg(rn));
				}

				Type::Add => {
					if self.match_token(tokens, Type::Hash).is_ok() {
						if self.match_token(tokens, Type::Dash).is_ok() {
							self.imm(tokens, &format!("adding negative"), token);
							self.match_token(tokens, Type::Comma).unwrap();
							let rn = self.match_reg(tokens).unwrap();
							println!("- into {}", dbg_reg(rn));
						} else {
							match self.next(tokens).tt {
								Type::Bin(n) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("adding {} into {}", dbg_bin(&n), dbg_reg(rn));
								}
								Type::Dec(n) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("adding {} into {}", dbg_dec(&n), dbg_reg(rn));
								}
								Type::Hex(n) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("adding {} into {}", dbg_hex(&n), dbg_reg(rn));
								}
								Type::Char(c) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("adding char '{c}' into {}", dbg_reg(rn));
								}
								_ => {
									self.unexpected_next(tokens, token);
								}
							}
						}
					} else {
						let rm = self.match_reg(tokens).unwrap();
						self.match_token(tokens, Type::Comma).unwrap();
						let rn = self.match_reg(tokens).unwrap();
						println!("adding {} into {}", dbg_reg(rm), dbg_reg(rn));
					}
				}

				Type::Mul => {}

				Type::MulS => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("multiplying {} into {}", dbg_reg(rm), dbg_reg(rn));
				}

				Type::MulU => {
					let rm = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let rn = self.match_reg(tokens).unwrap();
					println!("multiplying {} into {}", dbg_reg(rm), dbg_reg(rn));
				}

				Type::LdC => {
					self.match_token(tokens, Type::Dot).unwrap();
					self.match_token(tokens, Type::Long).unwrap();
					self.match_token(tokens, Type::At).unwrap();
					let r = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Plus).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let cr = self.creg(tokens);
					println!("loading value in {} into {}", dbg_addr_inc(r), dbg_creg(cr));
				}

				Type::LdS => {
					self.match_token(tokens, Type::Dot).unwrap();
					self.match_token(tokens, Type::Long).unwrap();
					self.match_token(tokens, Type::At).unwrap();
					let r = self.match_reg(tokens).unwrap();
					self.match_token(tokens, Type::Plus).unwrap();
					self.match_token(tokens, Type::Comma).unwrap();
					let sr = self.sreg(tokens);
					println!("loading value in {} into {}", dbg_addr_inc(r), dbg_sreg(sr));
				}

				Type::StC => {
					if self.match_token(tokens, Type::Dot).is_ok() {
						self.match_token(tokens, Type::Long).unwrap();
						let cr = self.creg(tokens);
						self.match_token(tokens, Type::Comma).unwrap();
						self.match_token(tokens, Type::At).unwrap();
						self.match_token(tokens, Type::Dash).unwrap();
						let r = self.match_reg(tokens).unwrap();
						println!("storing {} into {}", dbg_creg(cr), dbg_addr_dec(r));
					} else {
						let cr = self.creg(tokens);
						self.match_token(tokens, Type::Comma).unwrap();
						let r = self.match_reg(tokens).unwrap();
						println!("storing {} into {}", dbg_creg(cr), dbg_reg(r));
					}
				}

				Type::StS => {
					if self.match_token(tokens, Type::Dot).is_ok() {
						self.match_token(tokens, Type::Long).unwrap();
						let sr = self.sreg(tokens);
						self.match_token(tokens, Type::Comma).unwrap();
						self.match_token(tokens, Type::At).unwrap();
						self.match_token(tokens, Type::Dash).unwrap();
						let rn = self.match_reg(tokens).unwrap();
						println!("storing {} into {}", dbg_sreg(sr), dbg_addr_dec(rn));
					} else {
						let sr = self.sreg(tokens);
						self.match_token(tokens, Type::Comma).unwrap();
						let rn = self.match_reg(tokens).unwrap();
						println!("storing {} into {}", dbg_sreg(sr), dbg_reg(rn));
					}
				}

				Type::DMulS => {}
				Type::DMulU => {}

				Type::Mov => {
					match self.next(tokens).tt {
						Type::Hash => {
							match self.next(tokens).tt {
								Type::Dash => {
									match self.next(tokens).tt {
										Type::Bin(s) => {
											self.match_token(tokens, Type::Comma).unwrap();
											let rn = self.match_reg(tokens).unwrap();
											let n = i64::from_str_radix(&s, 2).unwrap();
											if i8_sized(n) {
												println!("moving {} into {}", dbg_bin(&s), dbg_reg(rn));
											} else {
												println!("waiting for LTORG to place value {n} for MOV into {}",
													dbg_reg(rn));
											}
										}
										Type::Dec(s) => {
											self.match_token(tokens, Type::Comma).unwrap();
											let rn = self.match_reg(tokens).unwrap();
											let n = i64::from_str_radix(&s, 10).unwrap();
											if i8_sized(n) {
												println!("moving {} into {}", dbg_dec(&s), dbg_reg(rn));
											} else {
												println!("waiting for LTORG to place value {n} for MOV into {}",
													dbg_reg(rn));
											}
										}
										Type::Hex(s) => {
											self.match_token(tokens, Type::Comma).unwrap();
											let rn = self.match_reg(tokens).unwrap();
											let n = i64::from_str_radix(&s, 16).unwrap();
											if i8_sized(n) {
												println!("moving {} into {}", dbg_hex(&s), dbg_reg(rn));
											} else {
												println!("waiting for LTORG to place value {n} for MOV into {}",
													dbg_reg(rn));
											}
										}
										_ => {
											self.unexpected_next(tokens, token);
										}
									}
								}
								Type::Bin(s) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									let n = i64::from_str_radix(&s, 2).unwrap();
									if i8_sized(n) {
										println!("moving {} into {}", dbg_bin(&s), dbg_reg(rn));
									} else {
										println!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
									}
								}
								Type::Dec(s) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									let n = i64::from_str_radix(&s, 10).unwrap();
									if i8_sized(n) {
										println!("moving {} into {}", dbg_dec(&s), dbg_reg(rn));
									} else {
										println!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
									}
								}
								Type::Hex(s) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									let n = i64::from_str_radix(&s, 16).unwrap();
									if i8_sized(n) {
										println!("moving {} into {}", dbg_hex(&s), dbg_reg(rn));
									} else {
										println!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
									}
								}
								Type::Label(s) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("moving address of label '{s}' into {}", dbg_reg(rn));
								}
								Type::Char(c) => {
									self.match_token(tokens, Type::Comma).unwrap();
									let rn = self.match_reg(tokens).unwrap();
									println!("moving char '{c}' into {}", dbg_reg(rn));
								}
								_ => {
									self.unexpected_next(tokens, token);
								}
							}
						}
						Type::Reg(rm) => {
							self.match_token(tokens, Type::Comma).unwrap();
							let rn = self.match_reg(tokens).unwrap();
							println!("moving {} into {}", dbg_reg(rm), dbg_reg(rn));
						}
						Type::Dot => {
							match self.next(tokens).tt {
								Type::Byte => {
									match self.next(tokens).tt {
										Type::At => {
											let rm = self.match_reg(tokens).unwrap();
											self.match_token(tokens, Type::Plus).unwrap();
											self.match_token(tokens, Type::Comma).unwrap();
											let rn = self.match_reg(tokens).unwrap();
											println!("moving {} into {}", dbg_reg(rm), dbg_addr_inc(rn));
										}
										Type::Reg(rm) => {
											self.match_token(tokens, Type::Comma).unwrap();
											self.match_token(tokens, Type::At).unwrap();
											let rn = self.match_reg(tokens).unwrap();
											println!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
										}
										_ => {
											self.unexpected_next(tokens, token);
										}
									}
								}
								Type::Word => {
									match self.next(tokens).tt {
										Type::At => {
											let rm = self.match_reg(tokens).unwrap();
											if self.match_token(tokens, Type::Plus).is_ok() {
												self.match_token(tokens, Type::Comma).unwrap();
												let rn = self.match_reg(tokens).unwrap();
												println!("moving {} into {}", dbg_reg(rm), dbg_addr_inc(rn));
											} else {
												self.match_token(tokens, Type::Comma).unwrap();
												let rn = self.match_reg(tokens).unwrap();
												println!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
											}
										}
										Type::Label(s) => {
											self.match_token(tokens, Type::Comma).unwrap();
											if self.match_token(tokens, Type::At).is_ok() {
												if self.match_token(tokens, Type::Dash).is_ok() {
													let rn = self.match_reg(tokens).unwrap();
													println!("moving value at label '{s}' into {}", dbg_addr_dec(rn));
												} else {
													let rn = self.match_reg(tokens).unwrap();
													println!("moving value at label '{s}' into {}", dbg_addr(rn));
												}
											} else {
												let rn = self.match_reg(tokens).unwrap();
												println!("moving value at label '{s}' into {}", dbg_reg(rn));
											}
										}
										Type::Reg(rm) => {
											self.match_token(tokens, Type::Comma).unwrap();
											self.match_token(tokens, Type::At).unwrap();
											if self.match_token(tokens, Type::Dash).is_ok() {
												let rn = self.match_reg(tokens).unwrap();
												println!("moving {} into {}", dbg_reg(rm), dbg_addr_dec(rn));
											} else {
												let rn = self.match_reg(tokens).unwrap();
												println!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
											}
										}
										_ => {
											self.unexpected_next(tokens, token);
										}
									}
								}
								Type::Long => {
									match self.next(tokens).tt {
										Type::Hash => {
											let s = self.match_label(tokens).unwrap();
											self.match_token(tokens, Type::Comma).unwrap();
											let r = self.match_reg(tokens).unwrap();
											println!("moving address of label '{s}' into {}", dbg_reg(r));
										}
										Type::Label(s) => {
											self.match_token(tokens, Type::Comma).unwrap();
											let r = self.match_reg(tokens).unwrap();
											println!("moving value at label '{s}' into {}", dbg_reg(r));
										}
										Type::At => {
											match self.next(tokens).tt {
												Type::OParen => {
													let s = self.match_label(tokens).unwrap();
													self.match_token(tokens, Type::Comma).unwrap();
													let rm = self.match_reg(tokens).unwrap();
													self.match_token(tokens, Type::CParen).unwrap();
													self.match_token(tokens, Type::Comma).unwrap();
													let rn = self.match_reg(tokens).unwrap();
													println!("moving value in {} with offset from label '{s}' into {}",
														dbg_addr(rm), dbg_reg(rn));
												}
												Type::Reg(rm) => {
													if self.match_token(tokens, Type::Plus).is_ok() {
														self.match_token(tokens, Type::Comma).unwrap();
														let rn = self.match_reg(tokens).unwrap();
														println!("moving value in {} into {}", dbg_addr_inc(rm), dbg_reg(rn));
													} else {
														self.match_token(tokens, Type::Comma).unwrap();
														let rn = self.match_reg(tokens).unwrap();
														println!("moving value in {} into {}", dbg_addr(rm), dbg_reg(rn));
													}
												}
												_ => {
													self.unexpected_next(tokens, token);
												}
											}
										}
										Type::Reg(rm) => {
											self.match_token(tokens, Type::Comma).unwrap();
											self.match_token(tokens, Type::At).unwrap();
											match self.next(tokens).tt {
												Type::Dash => {
													let rn = self.match_reg(tokens).unwrap();
													println!("moving {} into {}", dbg_reg(rm), dbg_addr_dec(rn));
												}
												Type::OParen => {
													let s = self.match_label(tokens).unwrap();
													self.match_token(tokens, Type::Comma).unwrap();
													let rn = self.match_reg(tokens).unwrap();
													self.match_token(tokens, Type::CParen).unwrap();
													println!("moving {} into {} with offset from label '{s}'",
														dbg_reg(rm), dbg_addr(rn));
												}
												_ => {
													self.unexpected_next(tokens, token);
												}
											}
										}
										_ => {
											self.unexpected_next(tokens, token);
										}
									}
								}
								_ => {
									self.unexpected_next(tokens, token);
								}
							}
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				}

				_ => {
					self.unexpected(tokens, token);
				}
			}

			self.index += 1;
		}
	}

	fn creg(&mut self, tokens: &[Token]) -> &'static str {
		if self.match_token(tokens, Type::Sr).is_ok() {
			"SR"
		} else if self.match_token(tokens, Type::Gbr).is_ok() {
			"GBR"
		} else if self.match_token(tokens, Type::Vbr).is_ok() {
			"VBR"
		} else {
			self.expected(tokens, "SR, GBR, or VBR");
			""
		}
	}

	fn sreg(&mut self, tokens: &[Token]) -> &'static str {
		if self.match_token(tokens, Type::Pr).is_ok() {
			"PR"
		} else if self.match_token(tokens, Type::Macl).is_ok() {
			"MACL"
		} else if self.match_token(tokens, Type::Mach).is_ok() {
			"MACH"
		} else {
			self.expected(tokens, "PR, MACL, or MACH");
			""
		}
	}

	fn imm(&mut self, tokens: &[Token], msg: &str, token: &Token) {
		match self.next(tokens).tt {
			Type::Bin(n) => {
				println!("{msg} {}", dbg_bin(&n));
			}
			Type::Dec(n) => {
				println!("{msg} {}", dbg_dec(&n));
			}
			Type::Hex(n) => {
				println!("{msg} {}", dbg_hex(&n));
			}
			_ => {
				self.unexpected_next(tokens, token);
			}
		}
	}

	fn cmp(&mut self, tokens: &[Token], msg: &str, token: &Token) {
		self.match_token(tokens, Type::Hash).unwrap();
		self.imm(tokens, msg, token);
		self.match_token(tokens, Type::Comma).unwrap();
		let r = self.match_reg(tokens).unwrap();
		println!("{msg} {}", dbg_reg(r));
	}
}

impl Parser {
	fn next(&mut self, tokens: &[Token]) -> Token {
		self.index += 1;
		tokens[self.index].clone()
	}

	fn expected(&self, tokens: &[Token], msg: &str) -> String {
		if tokens.len() > self.index + 1 {
			format!("expected {msg}, found '{:?}'", tokens[self.index + 1])
		} else {
			format!("expected {msg}, found EoF")
		}
	}

	fn unexpected(&self, tokens: &[Token], token: &Token) -> String {
		let start = self.index.saturating_sub(5);
		let end = (self.index + 6).min(tokens.len());
		let window = tokens[start..end]
			.iter()
			.map(|t| format!("{t:?}"))
			.collect::<Vec<_>>()
			.join("\n\t");

		if self.index > 0 {
			let prev = &tokens[self.index - 1];
			todo!("unexpected token '{prev:?}' before '{token:?}'\nwindow:\n\t{window}");
		} else {
			todo!("unexpected token '{token:?}'\nwindow:\n\t{window}")
		}
	}

	fn unexpected_next(&self, tokens: &[Token], token: &Token) -> String {
		let start = self.index.saturating_sub(5);
		let end = tokens.len().min(self.index + 6);
		let window = tokens[start..end]
			.iter()
			.map(|t| format!("{t:?}"))
			.collect::<Vec<_>>()
			.join("\n\t");

		let index = tokens.len().min(self.index + 1);
		let next = &tokens[index];
		todo!("unexpected token '{token:?}' before '{next:?}'\nwindow\n\t{window}")
	}

	fn match_immediate(&mut self, tokens: &[Token]) -> Result<i64, String> {
		match self.next(tokens).tt {
			Type::Bin(n) => {
				i64::from_str_radix(&n, 2).map_err(|e| format!("{e}"))
			}
			Type::Dec(n) => {
				i64::from_str_radix(&n, 10).map_err(|e| format!("{e}"))
			}
			Type::Hex(n) => {
				i64::from_str_radix(&n, 16).map_err(|e| format!("{e}"))
			}
			_ => {
				self.index -= 1;
				Err(self.expected(tokens, "Immediate value"))
			}
		}
	}

	fn match_label(&mut self, tokens: &[Token]) -> Result<Box<str>, String> {
		if let Token { tt: Type::Label(s), ..} = self.next(tokens) {
			Ok(s)
		} else {
			self.index -= 1;
			Err(self.expected(tokens, "Label"))
		}
	}

	fn match_string(&mut self, tokens: &[Token]) -> Result<Box<str>, String> {
		if let Token { tt: Type::String(s), ..} = self.next(tokens) {
			Ok(s)
		} else {
			self.index -= 1;
			Err(self.expected(tokens, "String literal"))
		}
	}

	fn match_reg(&mut self, tokens: &[Token]) -> Result<u8, String> {
		match self.next(tokens).tt {
			Type::Reg(r) => Ok(r),
			_ => {
				self.index -= 1;
				Err(self.expected(tokens, "General Register"))
			}
		}
	}

	fn match_token(&mut self, tokens: &[Token], tt: Type) -> Result<(), String> {
		if self.next(tokens).tt == tt {
			Ok(())
		} else {
			self.index -= 1;
			Err(self.expected(tokens, &format!("{tt:?}")))
		}
	}
}

fn i8_sized(n: i64) -> bool {
	(i8::MIN as i64..=i8::MAX as i64).contains(&n)
}

fn u8_sized(n: i64) -> bool {
	(u8::MIN as i64..=u8::MAX as i64).contains(&n)
}

fn u16_sized(n: i64) -> bool {
	(u16::MIN as i64..=u16::MAX as i64).contains(&n)
}

fn dbg_reg(r: u8) -> String {
	format!("General Register R{r}")
}

fn dbg_creg(s: &str) -> String {
	format!("Control Register {s}")
}

fn dbg_sreg(s: &str) -> String {
	format!("Special Register {s}")
}

fn dbg_addr(r: u8) -> String {
	format!("address @ R{r}")
}

fn dbg_addr_inc(r: u8) -> String {
	format!("incremented {}", dbg_addr(r))
}

fn dbg_addr_dec(r: u8) -> String {
	format!("decremented {}", dbg_addr(r))
}

fn dbg_bin(n: &str) -> String {
	format!("Immediate value '%{n}'")
}

fn dbg_dec(n: &str) -> String {
	format!("Immediate value '{n}'")
}

fn dbg_hex(n: &str) -> String {
	format!("Immediate value '${n}'")
}

