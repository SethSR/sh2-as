
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use tracing::{instrument, debug, error, trace};

use crate::tokens::{Token, Type as TT};
use crate::a2::{Asm, Type as AT};

pub fn eval(tokens: &[Token], source_root: PathBuf) -> Vec<Asm> {
	let mut parser = Parser::new(tokens, source_root);
	parser.process();
	//eprintln!("");
	parser.output();
	parser.out
}

#[derive(Debug)]
pub struct Parser<'a> {
	source_root: PathBuf,

	index: usize,
	tokens: &'a [Token],

	// Preprocessor
	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
	macros: HashMap<Box<str>, Vec<Token>>,

	// Output
	out: Vec<Asm>,
}

impl<'a> Parser<'a> {
	/// Record labels, record and expand macros, expand included ASM and binary files, and track
	/// output addresses.
	#[instrument(skip_all)]
	fn process(&mut self) {
		let mut waiting_words = 0;
		let mut waiting_longs = 0;
		let mut address = 0;

		while let Some(token) = self.next() {
			match token.tt {
				TT::Org => {
					let imm = self.immediate().unwrap();
					if imm > u32::MAX as i64 {
						todo!("address value too large: found({imm}), limit(0..={})", u32::MAX)
					}
					address = imm as u32;
					trace!("new section at address = {address:08X}");
				}

				TT::Include => {
					let name = self.string().unwrap();
					let path = Path::join(&self.source_root, name.to_string());
					let file = std::fs::read_to_string(path).unwrap();
					let tokens = crate::lexer::eval(&file).unwrap();
					let mut parser = Parser::new(&tokens, self.source_root.clone());
					parser.process();
					self.labels.extend(parser.labels);
					self.values.extend(parser.values);
					self.macros.extend(parser.macros);
					trace!("found assembly include: '{name}'");
				}

				TT::BInclude => {
					let name = self.string().unwrap();
					let path = Path::join(&self.source_root, name.to_string());
					// TODO - srenshaw - It's pretty wasteful to load the whole file just for the number of
					// bytes. We should probably store this for the output stage, or something similar.
					let file = std::fs::read(path).unwrap();
					address += file.len() as u32;
					trace!("found binary include: '{name}'");
				}

				TT::Align => {
					let imm = self.immediate().unwrap();
					if imm == 2 {
						// If we're offset by 1, add 1 to realign.
						address += address & 1;
					} else if imm == 4 {
						// If we're offset, use the inverted offset to realign, while ensuring we don't add
						// anything if we're already aligned.
						address += (4 - (address & 3)) & 3;
					} else {
						todo!("align must be followed by '2' or '4'");
					}
				}

				TT::LtOrg => {
					address += waiting_words * 2;
					address += waiting_longs * 4;
					waiting_words = 0;
					waiting_longs = 0;
				}

				TT::Label(ref name) => {
					let name = name.clone();
					if self.token(TT::Colon).is_some() {
						if self.labels.contains_key(&name) {
							todo!("duplicate label: {name}");
						} else {
							trace!("found label: '{name}'");
							self.labels.insert(name.clone(), address);
						}
					} else if self.token(TT::Eq).is_some() {
						let value = self.immediate().unwrap();
						if self.values.contains_key(&name) {
							todo!("duplicate value: {name} = {value}");
						} else {
							trace!("found value: {name} = {value}");
							self.values.insert(name.clone(), value);
						}
					}
				}

				TT::MacroStart => {
					let name = self.label().unwrap();
					let start = self.index;
					let mut end = start;
					while let Some(token) = self.next() {
						if matches!(token, Token { tt: TT::MacroEnd, ..}) {
							let tokens = self.tokens[start..end].to_vec();
							self.macros.insert(name.clone(), tokens);
							break;
						}
						end = self.index;
					}
					trace!("found macro start: '{name}'");
				}
				TT::MacroEnd => {
					todo!("unexpected macro end");
				}

				TT::Const => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						if self.neg_immediate().is_some() {
							address += 1;
						} else if let Some(name) = self.string() {
							address += name.len() as u32;
							while self.token(TT::Comma)
								.and_then(|_| self.immediate())
								.is_some()
							{
								address += 1;
							}
						} else {
							let token = &self.tokens[self.index];
							todo!("unexpected token: {token:?}");
						}
						address += 1;
					} else if self.token(TT::Word).is_some() {
						self.neg_immediate().unwrap();
						address += 2;
					} else if self.token(TT::Long).is_some() {
						if self.neg_immediate().is_some() || self.label().is_some() {
							address += 4;
						} else {
							let token = &self.tokens[self.index];
							todo!("unexpected token: {token:?}");
						}
					} else {
						let token = &self.tokens[self.index];
						todo!("unexpected token: {token:?}");
					}
				}

				TT::Space => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						let imm = self.immediate().unwrap();
						address += imm as u32;
					} else if self.token(TT::Word).is_some() {
						let imm = self.immediate().unwrap();
						address += 2 * imm as u32;
					} else if self.token(TT::Long).is_some() {
						let imm = self.immediate().unwrap();
						address += 4 * imm as u32;
					}
				}

				TT::Mov |
				TT::Add |
				TT::CmpEq => {
					if self.token(TT::Hash).is_some() {
						address += 2;
						if let Some(imm) = self.neg_immediate() {
							if i8_sized(imm) {
								// regular instruction
							} else if i16_sized(imm) {
								waiting_words += 1;
							} else if i32_sized(imm) {
								waiting_longs += 1;
							} else {
								todo!("immediate value too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
							}
						}
					}
				}

				TT::And |
				TT::Or |
				TT::Tst |
				TT::Xor => {
					if self.token(TT::Hash).is_some() {
						let imm = self.immediate().unwrap();
						address += 2;
						if i8_sized(imm) {
							// regular instruction
						} else if i16_sized(imm) {
							waiting_words += 1;
						} else if i32_sized(imm) {
							waiting_longs += 1;
						} else {
							todo!("immediate value too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
						}
					}
				}

				TT::NewLine | TT::Eof |
				TT::String(_) | TT::Char(_) |
				TT::Bin(_) | TT::Dec(_) | TT::Hex(_) |
				TT::Reg(_) | TT::Pc |
				TT::Gbr | TT::Vbr | TT::Sr |
				TT::Macl | TT::Mach | TT::Pr |
				TT::Plus | TT::Dash | TT::Star | TT::Slash |
				TT::At | TT::OParen | TT::CParen |
				TT::Colon | TT::Dot | TT::Comma |
				TT::Eq | TT::Hash |
				TT::Byte | TT::Word | TT::Long => {}

				_ => {
					address += 2;
				}
			}
		}
	}

	#[instrument(skip_all)]
	fn output(&mut self) {
		while let Some(token) = self.next() {
			match token.tt {
				TT::Mov => {}

				TT::MovA => {
					let d = self.disp_pc().unwrap();
					self.token(TT::Comma).unwrap();
					self.r0().unwrap();
					let imm = d / 4;
					if !(u8::MIN as u16..=u8::MAX as u16).contains(&imm) {
						todo!("displacement value too large: found({d}), limit(0..={})", u8::MAX as u16 * 4);
					}
					self.out.push(Asm::imm8(AT::MovA, imm as u8));
				}

				TT::MovT => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::MovT, rn));
				}

				TT::Swap => {}

				TT::Xtrct => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Xtrct, rn, rm));
				}

				TT::Add => {}

				TT::AddC => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::AddC, rn, rm));
				}

				TT::AddV => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::AddV, rn, rm));
				}

				TT::CmpEq => {}

				TT::CmpHs => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpHs, rn, rm));
				}

				TT::CmpGe => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpGe, rn, rm));
				}

				TT::CmpHi => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpHi, rn, rm));
				}

				TT::CmpGt => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpGt, rn, rm));
				}

				TT::CmpPl => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpPl, rn, rm));
				}

				TT::CmpPz => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpPz, rn, rm));
				}

				TT::CmpStr => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::CmpStr, rn, rm));
				}

				TT::Div1 => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Div1, rn, rm));
				}

				TT::Div0S => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Div0S, rn, rm));
				}

				TT::Div0U => {
					self.out.push(Asm::none(AT::Div0U));
				}

				TT::DMulS => {
					self.token(TT::Dot).unwrap();
					self.token(TT::Long).unwrap();
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::DMulS, rn, rm));
				}

				TT::DMulU => {
					self.token(TT::Dot).unwrap();
					self.token(TT::Long).unwrap();
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::DMulU, rn, rm));
				}

				TT::Dt => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::Dt, rn));
				}

				TT::ExtS => {}

				TT::ExtU => {}

				TT::Mac => {}

				TT::Mul => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Mul, rn, rm));
				}

				TT::MulS => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::MulS, rn, rm));
				}

				TT::MulU => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::MulU, rn, rm));
				}

				TT::Neg => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Neg, rn, rm));
				}

				TT::NegC => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::NegC, rn, rm));
				}

				TT::Sub => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Sub, rn, rm));
				}

				TT::SubC => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::SubC, rn, rm));
				}

				TT::SubV => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::SubV, rn, rm));
				}

				TT::And => {}

				TT::Not => {
					let (rm,rn) = self.reg2().unwrap();
					self.out.push(Asm::reg2(AT::Not, rn, rm));
				}

				TT::Or => {}

				TT::Tas => {
					self.token(TT::At).unwrap();
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::Tas, rn));
				}

				TT::Tst => {}

				TT::Xor => {}

				TT::RotL => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::RotL, rn));
				}

				TT::RotR => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::RotR, rn));
				}

				TT::RotCL => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::RotCL, rn));
				}

				TT::RotCR => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::RotCR, rn));
				}

				TT::ShAL => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShAL, rn));
				}

				TT::ShAR => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShAR, rn));
				}

				TT::ShLL => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLL, rn));
				}

				TT::ShLR => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLR, rn));
				}

				TT::ShLL2 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLL2, rn));
				}

				TT::ShLR2 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLR2, rn));
				}

				TT::ShLL8 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLL8, rn));
				}

				TT::ShLR8 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLR8, rn));
				}

				TT::ShLL16 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLL16, rn));
				}

				TT::ShLR16 => {
					let rn = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::ShLR16, rn));
				}

				TT::Bf => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BfS => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bt => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BtS => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bra => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BraF => {
					let rm = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::BraF, rm));
				}

				TT::Bsr => {
					let label = self.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BsrF => {
					let rm = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::BsrF, rm));
				}

				TT::Jmp => {
					self.token(TT::At).unwrap();
					let rm = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::Jmp, rm));
				}

				TT::Jsr => {
					self.token(TT::At).unwrap();
					let rm = self.reg().unwrap();
					self.out.push(Asm::reg1(AT::Jsr, rm));
				}

				TT::Rts => {
					self.out.push(Asm::none(AT::Rts));
				}

				TT::ClrT => {
					self.out.push(Asm::none(AT::ClrT));
				}

				TT::ClrMac => {
					self.out.push(Asm::none(AT::ClrMac));
				}

				TT::LdC => {}

				TT::LdS => {}

				TT::Nop => {
					self.out.push(Asm::none(AT::Nop));
				}

				TT::Rte => {
					self.out.push(Asm::none(AT::Rte));
				}

				TT::SetT => {
					self.out.push(Asm::none(AT::SetT));
				}

				TT::Sleep => {
					self.out.push(Asm::none(AT::Sleep));
				}

				TT::StC => {}

				TT::StS => {}

				TT::TrapA => {
					self.token(TT::Hash).unwrap();
					let imm = self.immediate().unwrap();
					let d = imm / 4;
					if !(u8::MIN as i64..=u8::MAX as i64).contains(&d) {
						todo!("trap offset too large: found({imm}), limit(0..={})", u8::MAX as i64 * 4);
					}
					self.out.push(Asm::imm8(AT::TrapA, d as u8));
				}

				_ => {
					trace!("unexpected Token: {token:?}");
				}
			}
		}
	}
}

impl<'a> Parser<'a> {
	fn new(tokens: &'a [Token], source_root: PathBuf) -> Self {
		Self {
			source_root,

			index: 0,
			tokens,

			labels: HashMap::default(),
			values: HashMap::default(),
			macros: HashMap::default(),

			out: Vec::default(),
		}
	}

	fn next(&mut self) -> Option<&Token> {
		let token = self.tokens.get(self.index)?;
		self.index += 1;
		Some(token)
	}

	fn token(&mut self, tt: TT) -> Option<()> {
		if self.next()
			.filter(|token| token.tt == tt)
			.is_some()
		{
			Some(())
		} else {
			self.index = self.index.saturating_sub(1);
			None
		}
	}

	fn string(&mut self) -> Option<Box<str>> {
		if let Some(Token { tt: TT::String(name), ..}) = self.next() {
			Some(name.clone())
		} else {
			self.index = self.index.saturating_sub(1);
			None
		}
	}

	fn label(&mut self) -> Option<Box<str>> {
		if let Some(Token { tt: TT::Label(name), ..}) = self.next() {
			Some(name.clone())
		} else {
			self.index = self.index.saturating_sub(1);
			None
		}
	}

	fn reg(&mut self) -> Option<u8> {
		if let Some(Token { tt: TT::Reg(r), ..}) = self.next() {
			Some(*r)
		} else {
			self.index = self.index.saturating_sub(1);
			None
		}
	}

	fn r0(&mut self) -> Option<()> {
		if let Some(0) = self.reg() {
			Some(())
		} else {
			None
		}
	}

	fn reg2(&mut self) -> Option<(u8,u8)> {
		let index = self.index;
		self.reg()
			.filter(|_| self.token(TT::Comma).is_some())
			.zip(self.reg())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn disp_pc(&mut self) -> Option<u16> {
		let index = self.index;
		self.token(TT::At)
			.and_then(|_| self.token(TT::OParen))
			.and_then(|_| self.immediate())
			.or_else(|| {
				let label = self.label()?;
				self.values.get(&label).copied()
			})
			.filter(|_| self.token(TT::Comma).is_some())
			.filter(|_| self.token(TT::Pc).is_some())
			.filter(|_| self.token(TT::CParen).is_some())
			.and_then(|d| if (u16::MIN as i64..=u16::MAX as i64).contains(&d) {
				Some(d as u16)
			} else {
				eprintln!("displacement value too large: found({d}), limit({}..={})", i32::MIN, i32::MAX);
				None
			})
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn immediate(&mut self) -> Option<i64> {
		match self.next() {
			Some(Token { tt: TT::Bin(n), ..}) => {
				i64::from_str_radix(n, 2).ok()
			}
			Some(Token { tt: TT::Dec(n), ..}) => {
				n.parse::<i64>().ok()
			}
			Some(Token { tt: TT::Hex(n), ..}) => {
				i64::from_str_radix(n, 16).ok()
			}
			_ => {
				self.index -= 1;
				None
			}
		}
	}

	fn neg_immediate(&mut self) -> Option<i64> {
		if let Some(Token { tt: TT::Dash, ..}) = self.next() {
			self.immediate()
				.map(|n| -n)
		} else {
			self.index -= 1;
			self.immediate()
		}
	}
}

fn i8_sized(imm: i64) -> bool {
	(i8::MIN as i64..=i8::MAX as i64).contains(&imm)
}

fn i16_sized(imm: i64) -> bool {
	(i16::MIN as i64..=i16::MAX as i64).contains(&imm)
}

fn i32_sized(imm: i64) -> bool {
	(i32::MIN as i64..=i32::MAX as i64).contains(&imm)
}

#[test]
fn no_output_from_empty_source() {
	let input = "";
	let tokens = crate::lexer::eval(input).unwrap();
	let mut parser = Parser::new(&tokens, "".into());
	parser.process();
	parser.output();
	assert!(parser.out.is_empty());
}

#[test]
fn preprocessed_labels_have_the_correct_addresses() {
	let input = "
start:
	mov #3,r0
	cmp/eq #0,r0
	bt start
end:
	add r0,r1
	";
	let tokens = crate::lexer::eval(input).unwrap();
	let mut parser = Parser::new(&tokens, "".into());
	parser.process();
	assert_eq!(parser.labels["start"], 0, "'start' label should start at address 0");
	assert_eq!(parser.labels["end"], 6, "'end' label should start at address 6");
}

