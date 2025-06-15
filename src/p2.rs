
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use tracing::trace;

use crate::asm::Asm;
use crate::tokens::{Token, Type};

type SectionMap = HashMap<u32, Vec<u8>>;

#[derive(Default)]
pub struct Parser {
	index: usize,
	file_path: PathBuf,

	/// Flag for whether we are recording a macro
	curr_macro: Option<Box<str>>,
	/// Records the token stream, so it can be "played back" when invoked.
	macros: HashMap<Box<str>, Vec<Token>>,

	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
	undefined_labels: HashMap<Box<str>, HashMap<u32, Vec<(u32, LabelUse)>>>,

	curr_base_addr: u32,
	sections: SectionMap,
}

pub fn eval(tokens: &[Token], file_path: PathBuf) -> Result<Parser, String> {
	let mut parser = Parser::default();
	parser.file_path = file_path;

	parser.parse(tokens);

	Ok(parser)
}

enum LabelUse {
	ConstLong,
	BranchTrue,
}

impl Parser {
	pub fn output(&self) -> SectionMap {
		self.sections.clone()
	}

	fn push(&mut self, asm: Asm) {
		let section = self.sections.entry(self.curr_base_addr)
			.or_default();
		asm.output(section);
	}

	fn save_label(&mut self, label: Box<str>, base_addr: u32, offset: u32, label_use: LabelUse) {
		// Find the label
		self.undefined_labels.entry(label)
			.or_default()
			// Find section waiting for label definition
			.entry(base_addr)
			.or_default()
			// Save where this label is used and what it's used for
			.push((offset, label_use));
	}

	fn backfill_label(&mut self, label: &str, curr_addr: u32) {
		if let Some(section_uses) = self.undefined_labels.remove(label) {
			for (base_addr, offsets) in section_uses {
				for (offset, label_use) in offsets {
					self.sections.entry(base_addr).and_modify(|section| match label_use {
						LabelUse::ConstLong => {
							let bytes = curr_addr.to_be_bytes();

							section[offset as usize..][..bytes.len()]
								.copy_from_slice(&bytes);
						}
						LabelUse::BranchTrue => {
							let address = self.curr_base_addr as i64 + offset as i64;
							let branch_offset = (curr_addr as i64 - address + 8) >> 1;
							assert!(i8_sized(branch_offset), "branch too far");
							let mut out = vec![];
							Asm::Bt(branch_offset as i8).output(&mut out);

							section[offset as usize..][..out.len()]
								.copy_from_slice(&out);
						}
					});
				}
			}
		}
	}

	fn parse(&mut self, tokens: &[Token]) {
		while tokens[self.index].tt != Type::Eof {
			let token = &tokens[self.index];
			self.parse_token(tokens, token).unwrap();
		}
	}

	fn parse_token(
		&mut self,
		tokens: &[Token],
		token: &Token,
	) -> Result<(), String> {
		// Don't process tokens while recording a macro, we'll do that whenever we see it used
		// somewhere.
		if let Some(label) = &self.curr_macro {
			match token.tt {
				Type::MacroEnd => {
					trace!("found macro end for '{label}'");
					self.curr_macro.take();
				}
				Type::MacroStart => {
					todo!("nested macros are currently unsupported");
				}
				_ => {
					self.macros.entry(label.clone())
						.or_default()
						.push(token.clone());
				}
			}
			self.index += 1;
			return Ok(());
		}

		match token.tt {
			Type::Label(ref s) => {
				let s: Box<str> = s.to_lowercase().into();
				match self.next(tokens).tt {
					Type::Eq => {
						let msg = format!("found label '{s}' with value");
						match self.next(tokens).tt {
							Type::Bin(n) => {
								trace!("{msg} {}", dbg_bin(&n));
								let n = i64::from_str_radix(&n, 2)
									.map_err(|e| format!("{e}"))?;
								self.values.insert(s, n);
							}
							Type::Dec(n) => {
								trace!("{msg} {}", dbg_dec(&n));
								let n = i64::from_str_radix(&n, 10)
									.map_err(|e| format!("{e}"))?;
								self.values.insert(s, n);
							}
							Type::Hex(n) => {
								trace!("{msg} {}", dbg_hex(&n));
								let n = i64::from_str_radix(&n, 16)
									.map_err(|e| format!("{e}"))?;
								self.values.insert(s, n);
							}
							_ => {
								self.unexpected_next(tokens, token);
							}
						}
					}
					Type::Colon => {
						trace!("found label '{s}'");
						let section_bytes = self.sections.entry(self.curr_base_addr)
							.or_default()
							.len() as u32;
						let curr_addr = self.curr_base_addr + section_bytes;
						// eprintln!("TODO - Backfill label '{s}' in incomplete instructions");
						self.labels.insert(s.clone(), curr_addr);
						self.backfill_label(&s, curr_addr);
					}
					Type::MacroStart => {
						trace!("found macro start for '{s}'");
						self.curr_macro = Some(s);
					}
					_ => {
						self.index -= 1;
						if self.macros.contains_key(&s) {
							trace!("found macro use for '{s}'");
						} else {
							eprintln!("Label ERROR '{s}' {:?}", self.macros);
							self.unexpected_next(tokens, token);
						}
					}
				}
			}

			Type::MacroEnd => {
				if self.curr_macro.is_none() {
					todo!("found ENDM with no preceding MACRO directive")
				}
			}

			/* Directives */
			Type::Org => {
				let msg = "setting current address";
				match self.next(tokens).tt {
					Type::Bin(n) => {
						trace!("{msg} {}", dbg_bin(&n));
						self.curr_base_addr = u32::from_str_radix(&n, 2)
							.map_err(|e| format!("{e}"))?;
					}
					Type::Dec(n) => {
						trace!("{msg} {}", dbg_dec(&n));
						self.curr_base_addr = u32::from_str_radix(&n, 10)
							.map_err(|e| format!("{e}"))?;
					}
					Type::Hex(n) => {
						trace!("{msg} {}", dbg_hex(&n));
						self.curr_base_addr = u32::from_str_radix(&n, 16)
							.map_err(|e| format!("{e}"))?;
					}
					_ => {
						self.unexpected_next(tokens, token);
					}
				}
			}

			Type::Include => {
				if let Ok(s) = self.match_string(tokens) {
					let s = Path::join(&self.file_path, &*s);
					trace!("include ASM file '{}'", s.display());
					let file = std::fs::read_to_string(&s)
						.expect(&format!("unable to open file '{}'", s.display()));
					let file_tokens = crate::lexer::eval(&file)
						.map_err(|e| format!("{e}"))?;

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
					trace!("include BINARY file '{}'", s.display());
					let bytes = std::fs::read(&s)
						.expect(&format!("unable to open file '{}'", s.display()));
					self.sections.entry(self.curr_base_addr)
						.or_default()
						.extend(bytes);
				} else {
					self.unexpected_next(tokens, token);
				}
			}

			Type::Align => {
				let n = self.match_immediate(tokens)?;
				trace!("aligning to the next {n}-byte boundary");
				let n = n as usize;
				let section = self.sections.entry(self.curr_base_addr)
					.or_default();
				for _ in 0..(n - (section.len() % n)) {
					section.push(0x00);
				}
			}

			Type::Const => {
				if self.match_token(tokens, Type::Dot).is_ok() {
					match self.next(tokens).tt {
						Type::Byte => {
							if let Ok(s) = self.match_string(tokens) {
								trace!("declaring byte-string '{s}'");

								for c in s.bytes() {
									self.push(Asm::Byte(c));
								}

								while self.match_token(tokens, Type::Comma).is_ok() {
									if let Ok(n) = self.match_immediate(tokens) {
										assert!(u8_sized(n));
										trace!("- with extra byte {n}");
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
									let n = self.match_immediate(tokens)?;
									assert!(u16_sized(n));
									trace!("declaring constant negative {n}");
									self.push(Asm::Word((-n) as u16));
								}
								Type::Bin(n) => {
									trace!("declaring constant {}", dbg_bin(&n));
									let n = u16::from_str_radix(&n, 2)
										.map_err(|e| format!("{e}"))?;
									self.push(Asm::Word(n));
								}
								Type::Dec(n) => {
									trace!("declaring constant {}", dbg_dec(&n));
									let n = u16::from_str_radix(&n, 10)
										.map_err(|e| format!("{e}"))?;
									self.push(Asm::Word(n));
								}
								Type::Hex(n) => {
									trace!("declaring constant {}", dbg_hex(&n));
									let n = u16::from_str_radix(&n, 16)
										.map_err(|e| format!("{e}"))?;
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
									trace!("declaring constant with address of label '{s}'");
									if let Some(n) = self.labels.get(&s) {
										self.push(Asm::Long(*n));
									} else {
										trace!("found unknown label: '{s}'");
										let section = self.sections.entry(self.curr_base_addr)
											.or_default();
										let section_len = section.len() as u32;

										section.extend([0x00, 0x00, 0x00, 0x00]);
										self.save_label(s, self.curr_base_addr, section_len, LabelUse::ConstLong);
									}
								}
								Type::Bin(n) => {
									trace!("declaring constant {}", dbg_bin(&n));
									let n = u32::from_str_radix(&n, 2)
										.map_err(|e| format!("{e}"))?;
									self.push(Asm::Long(n));
								}
								Type::Dec(n) => {
									trace!("declaring constant {}", dbg_dec(&n));
									let n = u32::from_str_radix(&n, 10)
										.map_err(|e| format!("{e}"))?;
									self.push(Asm::Long(n));
								}
								Type::Hex(n) => {
									trace!("declaring constant {}", dbg_hex(&n));
									let n = u32::from_str_radix(&n, 16)
										.map_err(|e| format!("{e}"))?;
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
				trace!("emptying literal pool");
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
				let s = self.match_label(tokens)?;
				trace!("branching on clear T-flag to label '{s}'");
			}
			Type::BfS => {}

			Type::Bra => {
				let next = self.next(tokens);
				match next.tt {
					Type::Label(s) => {
						trace!("branching to location @ '{s}'");
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
						trace!("branching to subroutine '{s}'");
					}
					_ => {
						self.unexpected_next(tokens, token);
					}
				}
			}

			Type::BsrF => {}

			Type::Bt => {
				let s = self.match_label(tokens)?;
				trace!("branching on set T-flag to label '{s}'");
				if let Some(addr) = self.labels.get(&s) {
					let section_len = self.sections.entry(self.curr_base_addr)
						.or_default()
						.len() as u32;
					let section_addr = self.curr_base_addr + section_len;
					let offset = (section_addr - addr) >> 1;
					assert!(i8_sized(offset as i64),
						"branch target too far for BT instruction - limit(-256..256) target({s}) offset({offset})",
					);
					self.push(Asm::Bt(offset as i8));
				} else {
					let section_len = self.sections.entry(self.curr_base_addr)
						.or_default()
						.len() as u32;
					self.save_label(s, self.curr_base_addr, section_len, LabelUse::BranchTrue);
					self.push(Asm::Bt(-1));
				}
			}

			Type::BtS => {}

			Type::Dt => {
				let r = self.match_reg(tokens)?;
				trace!("decrementing and testing {}", dbg_reg(r));
				self.push(Asm::Dt(r));
			}

			Type::Jmp => {}

			Type::Jsr => {}

			Type::MovA => {
				self.match_token(tokens, Type::At)?;
			}

			Type::MovT => {
				let rn = self.match_reg(tokens)?;
				trace!("moving T-flag to {}", dbg_reg(rn));
				self.push(Asm::MovT(rn));
			}

			Type::RotCL => {
				let r = self.match_reg(tokens)?;
				trace!("rotating {} left into C-flag", dbg_reg(r));
				self.push(Asm::RotCL(r));
			}
			Type::RotCR => {
				let r = self.match_reg(tokens)?;
				trace!("rotating {} right into C-flag", dbg_reg(r));
				self.push(Asm::RotCR(r));
			}
			Type::RotL => {
				let r = self.match_reg(tokens)?;
				trace!("rotating {} left", dbg_reg(r));
				self.push(Asm::RotL(r));
			}
			Type::RotR => {
				let r = self.match_reg(tokens)?;
				trace!("rotating {} right", dbg_reg(r));
				self.push(Asm::RotR(r));
			}

			Type::ShAL => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} arithmetically left", dbg_reg(r));
				self.push(Asm::ShAL(r));
			}
			Type::ShAR => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} arithmetically right", dbg_reg(r));
				self.push(Asm::ShAR(r));
			}
			Type::ShLL => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} left 1 bit", dbg_reg(r));
				self.push(Asm::ShLL(r));
			}
			Type::ShLL2 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} left 2 bits", dbg_reg(r));
				self.push(Asm::ShLL2(r));
			}
			Type::ShLL8 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} left 8 bits", dbg_reg(r));
				self.push(Asm::ShLL8(r));
			}
			Type::ShLL16 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} left 16 bits", dbg_reg(r));
				self.push(Asm::ShLL16(r));
			}
			Type::ShLR => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} right 1 bit", dbg_reg(r));
				self.push(Asm::ShLR(r));
			}
			Type::ShLR2 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} right 2 bits", dbg_reg(r));
				self.push(Asm::ShLR2(r));
			}
			Type::ShLR8 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} right 8 bits", dbg_reg(r));
				self.push(Asm::ShLR8(r));
			}
			Type::ShLR16 => {
				let r = self.match_reg(tokens)?;
				trace!("shifting {} right 16 bits", dbg_reg(r));
				self.push(Asm::ShLR16(r));
			}

			Type::Tas => {
				self.match_tokens(tokens, &[Type::Dot, Type::Byte])?;
				let rn = self.match_addr(tokens)?;
				trace!("testing byte @ {}", dbg_addr(rn));
				self.push(Asm::TaS(rn));
			}

			Type::TrapA => {
				self.match_token(tokens, Type::Hash)?;
				let imm = self.match_immediate(tokens)?;
				let n = (-imm) >> 2;
				assert!(u8_sized(n), "VBR offset out-of-range: value({imm:X}) limit(0..1024)");
				self.push(Asm::TrapA(n as u8));
			}

			Type::AddC => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("adding w/ carry {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::AddC(rm,rn));
			}

			Type::AddV => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("adding w/ overflow {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::AddV(rm,rn));
			}

			Type::Div0S => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("setting up signed division with {} and {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Div0S(rm,rn));
			}

			Type::Div1 => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("single-step dividing {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Div1(rm,rn));
			}

			Type::ExtS => {
				self.match_token(tokens, Type::Dot)?;
				match self.next(tokens).tt {
					Type::Byte => {
						let (rm,rn) = self.match_reg_pair(tokens)?;
						trace!("Extending to word as signed {} into {}", dbg_reg(rm), dbg_reg(rn));
						self.push(Asm::ExtSByte(rm,rn));
					}
					Type::Word => {
						let (rm,rn) = self.match_reg_pair(tokens)?;
						trace!("Extending to long as signed {} into {}", dbg_reg(rm), dbg_reg(rn));
						self.push(Asm::ExtSWord(rm,rn));
					}
					_ => {
						self.expected(tokens, "b or w");
					}
				}
			}

			Type::ExtU => {
				self.match_token(tokens, Type::Dot)?;
				match self.next(tokens).tt {
					Type::Byte => {
						let (rm,rn) = self.match_reg_pair(tokens)?;
						trace!("Extending to word as unsigned {} into {}", dbg_reg(rm), dbg_reg(rn));
						self.push(Asm::ExtUByte(rm,rn));
					}
					Type::Word => {
						let (rm,rn) = self.match_reg_pair(tokens)?;
						trace!("Extending to long as unsigned {} into {}", dbg_reg(rm), dbg_reg(rn));
						self.push(Asm::ExtUWord(rm,rn));
					}
					_ => {
						self.expected(tokens, "b or w");
					}
				}
			}

			Type::Mac => {
				self.match_token(tokens, Type::Dot)?;
				match self.next(tokens).tt {
					Type::Word => {
						let rm = self.match_addr_inc(tokens)?;
						self.match_token(tokens, Type::Comma)?;
						let rn = self.match_addr_inc(tokens)?;
						trace!("Multiply-accumulating {} and {}", dbg_addr_inc(rm), dbg_addr_inc(rn));
						self.push(Asm::MacWord(rm,rn));
					}
					Type::Long => {
						let rm = self.match_addr_inc(tokens)?;
						self.match_token(tokens, Type::Comma)?;
						let rn = self.match_addr_inc(tokens)?;
						trace!("Multiply-accumulating {} and {}", dbg_addr_inc(rm), dbg_addr_inc(rn));
						self.push(Asm::MacLong(rm,rn));
					}
					_ => {
						self.expected(tokens, "w or l");
					}
				}
			}

			Type::Neg => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("Negating {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Neg(rm,rn));
			}

			Type::NegC => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("Negating w/ carry {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::NegC(rm,rn));
			}

			Type::Not => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("Inverting {} bits into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Not(rm,rn));
			}

			Type::Sub => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("subtracting {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Sub(rm,rn));
			}

			Type::SubC => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("subtracting w/ carry {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::SubC(rm,rn));
			}

			Type::SubV => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("subtracting w/ overflow {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::SubV(rm,rn));
			}

			Type::Swap => {
				if self.match_token(tokens, Type::Dot).is_ok() {
					match self.next(tokens).tt {
						Type::Byte => {
							let rm = self.match_reg(tokens)?;
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_reg(tokens)?;
							trace!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
							self.push(Asm::SwapByte(rm,rn));
						}
						Type::Word => {
							let rm = self.match_reg(tokens)?;
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_reg(tokens)?;
							trace!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
							self.push(Asm::SwapWord(rm,rn));
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("swapping {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::SwapWord(rm,rn));
				}
			}

			Type::Xtrct => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("extracting {0} x {1} into {1}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::Xtrct(rm, rn));
			}

			Type::And => {
				if self.match_token(tokens, Type::Hash).is_ok() {
				let is_neg = self.match_token(tokens, Type::Dash).is_ok();
					let imm = self.match_immediate(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let rn = self.match_reg(tokens)?;
					assert_eq!(rn, 0);
					let imm = if is_neg { -imm } else { imm };
					assert!(i8_sized(imm), "Immediate value {imm} too large for AND instruction.");
					trace!("logical-anding {imm} into {}", dbg_reg(rn));
					self.push(Asm::AndImm(imm as u8));
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("logical-anding {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::AndReg(rm,rn));
				}
			}

			Type::Or => {
				if self.match_token(tokens, Type::Hash).is_ok() {
				let is_neg = self.match_token(tokens, Type::Dash).is_ok();
					let imm = self.match_immediate(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let rn = self.match_reg(tokens)?;
					assert_eq!(rn, 0);
					let imm = if is_neg { -imm } else { imm };
					assert!(i8_sized(imm), "Immediate value {imm} too large for OR instruction.");
					trace!("logical-oring {imm} into {}", dbg_reg(rn));
					self.push(Asm::OrImm(imm as u8));
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("logical-oring {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::OrReg(rm,rn));
				}
			}

			Type::Tst => {
				if self.match_token(tokens, Type::Hash).is_ok() {
				let is_neg = self.match_token(tokens, Type::Dash).is_ok();
					let imm = self.match_immediate(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let rn = self.match_reg(tokens)?;
					assert_eq!(rn, 0);
					let imm = if is_neg { -imm } else { imm };
					assert!(i8_sized(imm), "Immediate value {imm} too large for TST instruction.");
					trace!("testing {imm} into {}", dbg_reg(rn));
					self.push(Asm::TstImm(imm as u8));
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("testing {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::TstReg(rm,rn));
				}
			}

			Type::Xor => {
				if self.match_token(tokens, Type::Hash).is_ok() {
				let is_neg = self.match_token(tokens, Type::Dash).is_ok();
					let imm = self.match_immediate(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let rn = self.match_reg(tokens)?;
					assert_eq!(rn, 0);
					let imm = if is_neg { -imm } else { imm };
					assert!(i8_sized(imm), "Immediate value {imm} too large for XOR instruction.");
					trace!("logical-xoring {imm} into {}", dbg_reg(rn));
					self.push(Asm::XorImm(imm as u8));
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("logical-xoring {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::XorReg(rm,rn));
				}
			}

			Type::CmpEq => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} == {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpEqReg(rm, rn));
			}
			Type::CmpGe => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} >= {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpGE(rm, rn));
			}
			Type::CmpGt => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} > {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpGT(rm, rn));
			}
			Type::CmpHi => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} > {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpHI(rm, rn));
			}
			Type::CmpHs => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} >= {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpHS(rm, rn));
			}
			Type::CmpStr => {
				let rm = self.match_reg(tokens)?;
				self.match_token(tokens, Type::Comma)?;
				let rn = self.match_reg(tokens)?;
				trace!("comparing {} str {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::CmpSTR(rm, rn));
			}
			Type::CmpPl => {
				let rn = self.match_reg(tokens)?;
				trace!("comparing 0 < {}", dbg_reg(rn));
				self.push(Asm::CmpPL(rn));
			}
			Type::CmpPz => {
				let rn = self.match_reg(tokens)?;
				trace!("comparing 0 <= {}", dbg_reg(rn));
				self.push(Asm::CmpPZ(rn));
			}

			Type::Add => {
				if self.match_token(tokens, Type::Hash).is_ok() {
					if self.match_token(tokens, Type::Dash).is_ok() {
						let n = self.match_immediate(tokens)?;
						assert!(i8_sized(n), "Currently, cannot add word or long size immediate values.");
						self.match_token(tokens, Type::Comma)?;
						let rn = self.match_reg(tokens)?;
						trace!("adding negative {n} into {}", dbg_reg(rn));
						self.push(Asm::AddImm((-n) as i8, rn));
					} else {
						match self.next(tokens).tt {
							Type::Bin(n) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("adding {} into {}", dbg_bin(&n), dbg_reg(rn));
								let n = i8::from_str_radix(&n, 2)
									.map_err(|e| format!("{e}"))?;
								self.push(Asm::AddImm(n, rn));
							}
							Type::Dec(n) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("adding {} into {}", dbg_dec(&n), dbg_reg(rn));
								let n = i8::from_str_radix(&n, 10)
									.map_err(|e| format!("{e}"))?;
								self.push(Asm::AddImm(n, rn));
							}
							Type::Hex(n) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("adding {} into {}", dbg_hex(&n), dbg_reg(rn));
								let n = i8::from_str_radix(&n, 16)
									.map_err(|e| format!("{e}"))?;
								self.push(Asm::AddImm(n, rn));
							}
							Type::Char(c) => {
								assert!(c.is_ascii(), "Currently, only ASCII characters supported in ADD-Immediate instructions.");
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("adding char '{c}' into {}", dbg_reg(rn));
								self.push(Asm::AddImm(c as i8, rn));
							}
							_ => {
								self.unexpected_next(tokens, token);
							}
						}
					}
				} else {
					let (rm,rn) = self.match_reg_pair(tokens)?;
					trace!("adding {} into {}", dbg_reg(rm), dbg_reg(rn));
					self.push(Asm::AddReg(rm, rn));
				}
			}

			Type::Mul => {}

			Type::MulS => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("multiplying {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::MulS(rm, rn));
			}

			Type::MulU => {
				let (rm,rn) = self.match_reg_pair(tokens)?;
				trace!("multiplying {} into {}", dbg_reg(rm), dbg_reg(rn));
				self.push(Asm::MulU(rm, rn));
			}

			Type::LdC => {
				if self.match_token(tokens, Type::Dot).is_ok() {
					self.match_token(tokens, Type::Long)?;
					let rm = self.match_addr_inc(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let msg = format!("value in {}", dbg_addr_inc(rm));
					match self.next(tokens).tt {
						Type::Sr => {
							trace!("loading {msg} into Control Register SR");
							self.push(Asm::LdcSrInc(rm));
						}
						Type::Gbr => {
							trace!("loading {msg} into Control Register GBR");
							self.push(Asm::LdcGbrInc(rm));
						}
						Type::Vbr => {
							trace!("loading {msg} into Control Register VBR");
							self.push(Asm::LdcVbrInc(rm));
						}
						_ => {
							self.expected(tokens, "SR, GBR, or VBR");
						}
					}
				} else {
					let rm = self.match_reg(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let msg = dbg_reg(rm);
					match self.next(tokens).tt {
						Type::Sr => {
							trace!("loading {msg} into Control Register SR");
							self.push(Asm::LdcSr(rm));
						}
						Type::Gbr => {
							trace!("loading {msg} into Control Register GBR");
							self.push(Asm::LdcGbr(rm));
						}
						Type::Vbr => {
							trace!("loading {msg} into Control Register VBR");
							self.push(Asm::LdcVbr(rm));
						}
						_ => {
							self.expected(tokens, "SR, GBR, or VBR");
						}
					}
				}
			}

			Type::LdS => {
				if self.match_tokens(tokens, &[Type::Dot, Type::Long]).is_ok() {
					let rm = self.match_addr_inc(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let msg = format!("value in {}", dbg_addr_inc(rm));
					match self.next(tokens).tt {
						Type::Macl => {
							trace!("loading {msg} into System Register MACL");
							self.push(Asm::LdsMaclInc(rm));
						}
						Type::Mach => {
							trace!("loading {msg} into System Register MACH");
							self.push(Asm::LdsMachInc(rm));
						}
						Type::Pr => {
							trace!("loading {msg} into System Register PR");
							self.push(Asm::LdsPrInc(rm));
						}
						_ => {
							self.expected(tokens, "MACL, MACH, or PR");
						}
					}
				} else {
					let rm = self.match_reg(tokens)?;
					self.match_token(tokens, Type::Comma)?;
					let msg = dbg_reg(rm);
					match self.next(tokens).tt {
						Type::Macl => {
							trace!("loading {msg} into System Register MACL");
							self.push(Asm::LdsMacl(rm));
						}
						Type::Mach => {
							trace!("loading {msg} into System Register MACH");
							self.push(Asm::LdsMach(rm));
						}
						Type::Pr => {
							trace!("loading {msg} into System Register PR");
							self.push(Asm::LdsPr(rm));
						}
						_ => {
							self.expected(tokens, "MACL, MACH, or PR");
						}
					}
				}
			}

			Type::StC => {
				if self.match_tokens(tokens, &[Type::Dot, Type::Long]).is_ok() {
					match self.next(tokens).tt {
						Type::Gbr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_addr_dec(tokens)?;
							trace!("storing Control Register GBR into {}", dbg_addr_dec(rm));
							self.push(Asm::StcGbrDec(rm));
						}
						Type::Vbr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_addr_dec(tokens)?;
							trace!("storing Control Register VBR into {}", dbg_addr_dec(rm));
							self.push(Asm::StcVbrDec(rm));
						}
						Type::Sr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_addr_dec(tokens)?;
							trace!("storing Control Register SR into {}", dbg_addr_dec(rm));
							self.push(Asm::StcSrDec(rm));
						}
						_ => {
							self.expected(tokens, "GBR, VBR, or SR");
						}
					}
				} else {
					match self.next(tokens).tt {
						Type::Gbr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_reg(tokens)?;
							trace!("storing Control Register GBR into {}", dbg_reg(rm));
							self.push(Asm::StcGbr(rm));
						}
						Type::Vbr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_reg(tokens)?;
							trace!("storing Control Register VBR into {}", dbg_reg(rm));
							self.push(Asm::StcVbr(rm));
						}
						Type::Sr => {
							self.match_token(tokens, Type::Comma)?;
							let rm = self.match_reg(tokens)?;
							trace!("storing Control Register SR into {}", dbg_reg(rm));
							self.push(Asm::StcSr(rm));
						}
						_ => {
							self.expected(tokens, "GBR, VBR, or SR");
						}
					}
				}
			}

			Type::StS => {
				if self.match_tokens(tokens, &[Type::Dot, Type::Long]).is_ok() {
					match self.next(tokens).tt {
						Type::Macl => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_addr_dec(tokens)?;
							trace!("storing System Register MACL into {}", dbg_addr_dec(rn));
							self.push(Asm::StsMaclDec(rn));
						}
						Type::Mach => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_addr_dec(tokens)?;
							trace!("storing System Register MACH into {}", dbg_addr_dec(rn));
							self.push(Asm::StsMachDec(rn));
						}
						Type::Pr => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_addr_dec(tokens)?;
							trace!("storing System Register PR into {}", dbg_addr_dec(rn));
							self.push(Asm::StsPrDec(rn));
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
				} else {
					match self.next(tokens).tt {
						Type::Macl => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_reg(tokens)?;
							trace!("storing System Register MACL into {}", dbg_reg(rn));
							self.push(Asm::StsMacl(rn));
						}
						Type::Mach => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_reg(tokens)?;
							trace!("storing System Register MACH into {}", dbg_reg(rn));
							self.push(Asm::StsMach(rn));
						}
						Type::Pr => {
							self.match_token(tokens, Type::Comma)?;
							let rn = self.match_reg(tokens)?;
							trace!("storing System Register PR into {}", dbg_reg(rn));
							self.push(Asm::StsPr(rn));
						}
						_ => {
							self.unexpected_next(tokens, token);
						}
					}
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
										self.match_token(tokens, Type::Comma)?;
										let rn = self.match_reg(tokens)?;
										let n = i64::from_str_radix(&s, 2)
											.map_err(|e| format!("{e}"))?;
										if i8_sized(n) {
											trace!("moving {} into {}", dbg_bin(&s), dbg_reg(rn));
										} else {
											trace!("waiting for LTORG to place value {n} for MOV into {}",
												dbg_reg(rn));
										}
									}
									Type::Dec(s) => {
										self.match_token(tokens, Type::Comma)?;
										let rn = self.match_reg(tokens)?;
										let n = i64::from_str_radix(&s, 10)
											.map_err(|e| format!("{e}"))?;
										if i8_sized(n) {
											trace!("moving {} into {}", dbg_dec(&s), dbg_reg(rn));
										} else {
											trace!("waiting for LTORG to place value {n} for MOV into {}",
												dbg_reg(rn));
										}
									}
									Type::Hex(s) => {
										self.match_token(tokens, Type::Comma)?;
										let rn = self.match_reg(tokens)?;
										let n = i64::from_str_radix(&s, 16)
											.map_err(|e| format!("{e}"))?;
										if i8_sized(n) {
											trace!("moving {} into {}", dbg_hex(&s), dbg_reg(rn));
										} else {
											trace!("waiting for LTORG to place value {n} for MOV into {}",
												dbg_reg(rn));
										}
									}
									_ => {
										self.unexpected_next(tokens, token);
									}
								}
							}
							Type::Bin(s) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								let n = i64::from_str_radix(&s, 2)
									.map_err(|e| format!("{e}"))?;
								if i8_sized(n) {
									trace!("moving {} into {}", dbg_bin(&s), dbg_reg(rn));
								} else {
									trace!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
								}
							}
							Type::Dec(s) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								let n = i64::from_str_radix(&s, 10)
									.map_err(|e| format!("{e}"))?;
								if i8_sized(n) {
									trace!("moving {} into {}", dbg_dec(&s), dbg_reg(rn));
								} else {
									trace!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
								}
							}
							Type::Hex(s) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								let n = i64::from_str_radix(&s, 16)
									.map_err(|e| format!("{e}"))?;
								if i8_sized(n) {
									trace!("moving {} into {}", dbg_hex(&s), dbg_reg(rn));
								} else {
									trace!("waiting for LTORG to place value {n} for MOV into {}", dbg_reg(rn));
								}
							}
							Type::Label(s) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("moving address of label '{s}' into {}", dbg_reg(rn));
							}
							Type::Char(c) => {
								self.match_token(tokens, Type::Comma)?;
								let rn = self.match_reg(tokens)?;
								trace!("moving char '{c}' into {}", dbg_reg(rn));
							}
							_ => {
								self.unexpected_next(tokens, token);
							}
						}
					}
					Type::Reg(rm) => {
						self.match_token(tokens, Type::Comma)?;
						let rn = self.match_reg(tokens)?;
						trace!("moving {} into {}", dbg_reg(rm), dbg_reg(rn));
					}
					Type::Dot => {
						match self.next(tokens).tt {
							Type::Byte => {
								match self.next(tokens).tt {
									Type::At => {
										let rm = self.match_reg(tokens)?;
										self.match_token(tokens, Type::Plus)?;
										self.match_token(tokens, Type::Comma)?;
										let rn = self.match_reg(tokens)?;
										trace!("moving {} into {}", dbg_reg(rm), dbg_addr_inc(rn));
									}
									Type::Reg(rm) => {
										self.match_token(tokens, Type::Comma)?;
										self.match_token(tokens, Type::At)?;
										let rn = self.match_reg(tokens)?;
										trace!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
									}
									_ => {
										self.unexpected_next(tokens, token);
									}
								}
							}
							Type::Word => {
								match self.next(tokens).tt {
									Type::At => {
										let rm = self.match_reg(tokens)?;
										if self.match_token(tokens, Type::Plus).is_ok() {
											self.match_token(tokens, Type::Comma)?;
											let rn = self.match_reg(tokens)?;
											trace!("moving {} into {}", dbg_reg(rm), dbg_addr_inc(rn));
										} else {
											self.match_token(tokens, Type::Comma)?;
											let rn = self.match_reg(tokens)?;
											trace!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
										}
									}
									Type::Label(s) => {
										self.match_token(tokens, Type::Comma)?;
										if self.match_token(tokens, Type::At).is_ok() {
											if self.match_token(tokens, Type::Dash).is_ok() {
												let rn = self.match_reg(tokens)?;
												trace!("moving value at label '{s}' into {}", dbg_addr_dec(rn));
											} else {
												let rn = self.match_reg(tokens)?;
												trace!("moving value at label '{s}' into {}", dbg_addr(rn));
											}
										} else {
											let rn = self.match_reg(tokens)?;
											trace!("moving value at label '{s}' into {}", dbg_reg(rn));
										}
									}
									Type::Reg(rm) => {
										self.match_token(tokens, Type::Comma)?;
										self.match_token(tokens, Type::At)?;
										if self.match_token(tokens, Type::Dash).is_ok() {
											let rn = self.match_reg(tokens)?;
											trace!("moving {} into {}", dbg_reg(rm), dbg_addr_dec(rn));
										} else {
											let rn = self.match_reg(tokens)?;
											trace!("moving {} into {}", dbg_reg(rm), dbg_addr(rn));
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
										let s = self.match_label(tokens)?;
										self.match_token(tokens, Type::Comma)?;
										let r = self.match_reg(tokens)?;
										trace!("moving address of label '{s}' into {}", dbg_reg(r));
									}
									Type::Label(s) => {
										self.match_token(tokens, Type::Comma)?;
										let r = self.match_reg(tokens)?;
										trace!("moving value at label '{s}' into {}", dbg_reg(r));
									}
									Type::At => {
										match self.next(tokens).tt {
											Type::OParen => {
												let s = self.match_label(tokens)?;
												self.match_token(tokens, Type::Comma)?;
												let rm = self.match_reg(tokens)?;
												self.match_token(tokens, Type::CParen)?;
												self.match_token(tokens, Type::Comma)?;
												let rn = self.match_reg(tokens)?;
												trace!("moving value in {} with offset from label '{s}' into {}",
													dbg_addr(rm), dbg_reg(rn));
											}
											Type::Reg(rm) => {
												if self.match_token(tokens, Type::Plus).is_ok() {
													self.match_token(tokens, Type::Comma)?;
													let rn = self.match_reg(tokens)?;
													trace!("moving value in {} into {}", dbg_addr_inc(rm), dbg_reg(rn));
												} else {
													self.match_token(tokens, Type::Comma)?;
													let rn = self.match_reg(tokens)?;
													trace!("moving value in {} into {}", dbg_addr(rm), dbg_reg(rn));
												}
											}
											_ => {
												self.unexpected_next(tokens, token);
											}
										}
									}
									Type::Reg(rm) => {
										self.match_token(tokens, Type::Comma)?;
										self.match_token(tokens, Type::At)?;
										match self.next(tokens).tt {
											Type::Dash => {
												let rn = self.match_reg(tokens)?;
												trace!("moving {} into {}", dbg_reg(rm), dbg_addr_dec(rn));
											}
											Type::OParen => {
												let s = self.match_label(tokens)?;
												self.match_token(tokens, Type::Comma)?;
												let rn = self.match_reg(tokens)?;
												self.match_token(tokens, Type::CParen)?;
												trace!("moving {} into {} with offset from label '{s}'",
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
		Ok(())
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
		if let Type::Label(s) = self.next(tokens).tt {
			Ok(s)
		} else {
			self.index -= 1;
			Err(self.expected(tokens, "Label"))
		}
	}

	fn match_string(&mut self, tokens: &[Token]) -> Result<Box<str>, String> {
		if let Type::String(s) = self.next(tokens).tt {
			Ok(s)
		} else {
			self.index -= 1;
			Err(self.expected(tokens, "String literal"))
		}
	}

	fn match_reg(&mut self, tokens: &[Token]) -> Result<u8, String> {
		if let Type::Reg(r) = self.next(tokens).tt {
			Ok(r)
		} else {
			self.index -= 1;
			Err(self.expected(tokens, "General Register"))
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

	fn match_reg_pair(&mut self, tokens: &[Token]) -> Result<(u8,u8), String> {
		let rm = self.match_reg(tokens)?;
		self.match_token(tokens, Type::Comma)?;
		let rn = self.match_reg(tokens)?;
		Ok((rm,rn))
	}

	fn match_tokens(&mut self, tokens: &[Token], tts: &[Type]) -> Result<(), String> {
		for tt in tts {
			self.match_token(tokens, tt.clone())?;
		}
		Ok(())
	}

	fn match_addr(&mut self, tokens: &[Token]) -> Result<u8, String> {
		self.match_token(tokens, Type::At)?;
		self.match_reg(tokens)
	}

	fn match_addr_dec(&mut self, tokens: &[Token]) -> Result<u8, String> {
		self.match_tokens(tokens, &[Type::At, Type::Dash])?;
		self.match_reg(tokens)
	}

	fn match_addr_inc(&mut self, tokens: &[Token]) -> Result<u8, String> {
		self.match_token(tokens, Type::At)?;
		let r = self.match_reg(tokens)?;
		self.match_token(tokens, Type::Plus)?;
		Ok(r)
	}

	fn match_indirect(&mut self, tokens: &[Token]) -> Result<(u8, String), String> {
		if let Ok(r) = self.match_addr_dec(tokens) {
			Ok((r, dbg_addr_dec(r)))
		} else if let Ok(r) = self.match_addr_inc(tokens) {
			Ok((r, dbg_addr_inc(r)))
		} else if let Ok(r) = self.match_addr(tokens) {
			Ok((r, dbg_addr(r)))
		} else {
			Err(self.expected(tokens, "@-Reg, @Reg+, or @Reg"))
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

#[cfg(test)]
mod tests {
	use crate::lexer;
	use crate::asm::Asm;

	use super::*;

	fn check(input: &str, asm: Asm) {
		let tokens = lexer::eval(input)
			.unwrap();
		let mut parser = Parser::default();
		parser.parse(&tokens);

		let mut out = vec![];
		asm.output(&mut out);

		assert_eq!(parser.sections[&0], out);
	}

	#[test]
	fn add_reg() {
		check("add r3,r5", Asm::AddReg(3,5));
	}

	#[test]
	fn add_imm() {
		check("add #4,r8", Asm::AddImm(4,8));
	}

	#[test]
	fn add_neg_imm() {
		check("add #-7,r9", Asm::AddImm(-7,9));
	}

	#[test]
	fn loop_labels() {
		let tokens = lexer::eval("
start:
	sett
	bt end
	add r2,r3
end:
	add r7,r8
").unwrap();
		assert!(!tokens.is_empty());
		let mut parser = Parser::default();
		parser.parse(&tokens);
		assert!(!parser.sections.is_empty());

		let mut out = vec![];
		for asm in [
			Asm::SetT,
			Asm::Bt(6),
			Asm::AddReg(2,3),
			Asm::AddReg(7,8),
		] {
			asm.output(&mut out);
		}

		assert_eq!(parser.sections[&0], out);
	}
}

