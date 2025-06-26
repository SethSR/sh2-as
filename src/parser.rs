
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use tracing::{instrument, debug, error, trace};

use crate::tokens::{Token, Type as TT};
use crate::asm::{Asm, Type as AT};

#[derive(Debug)]
enum IR {
	/// xxxx xxxx xxxx xxxx
	Zero(AT),
	/// xxxx rrrr xxxx xxxx
	One(AT, u8),
	/// xxxx nnnn mmmm xxxx
	Two(AT, u8, u8),
	/// xxxx xxxx rrrr dddd
	Dsp4(AT, u8, u8),
	/// xxxx nnnn mmmm dddd
	NM4(AT, u8, u8, u8),
	/// xxxx xxxx iiii iiii
	Imm(AT, u8),
	/// xxxx dddd dddd dddd
	Imm12(AT, u16),
	/// xxxx nnnn iiii iiii
	NI(AT, u8, u8),

	Jmp(AT, Box<str>),

	Byte(u8),
	Word(u16),
	Long(u32),

	Placeholder,
	Placeholder4,
}

#[derive(Debug)]
enum PH {
	Reg(u8),
	// #Label,Rn
	HLbl(Box<str>,u8),
	// Label,Rn
	Lbl(Box<str>,u8),
	// @(Label,Rm),Rn
	Dsp(Box<str>,u8,u8),
}

pub fn eval(tokens: &[Token], source_root: PathBuf) -> Vec<Asm> {
	let mut parser = Parser::new(tokens, source_root);
	parser.process();
	eprintln!("{:?}", parser.preprocessor);
	parser.output();
	parser.out
}

#[derive(Debug, Default)]
struct Preprocessor {
	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
	macros: HashMap<Box<str>, Vec<Token>>,
	waiting: Vec<(u32, AT, PH)>,

	address: u32,
	intermediates: Vec<(u32, IR)>,
}

#[derive(Debug)]
pub struct Parser<'a> {
	source_root: PathBuf,

	index: usize,
	tokens: &'a [Token],

	preprocessor: Preprocessor,

	// Output
	out: Vec<Asm>,
}

impl<'a> Parser<'a> {
	#[instrument(skip_all)]
	fn process(&mut self) {
		let mut waiting_words = 0;
		let mut waiting_longs = 0;

		while let Some(token) = self.next() {
			match token.tt {
				TT::Org => {
					let imm = self.immediate().unwrap();
					if imm > u32::MAX as i64 {
						panic!("address value too large: found({imm}), limit(0..={})", u32::MAX)
					}
					self.preprocessor.address = imm as u32;
					trace!("new section at address = {:08X}", self.preprocessor.address);
				}

				TT::Include => {
					let name = self.string().unwrap();
					let path = Path::join(&self.source_root, name.to_string());
					let file = std::fs::read_to_string(path).unwrap();
					let tokens = crate::lexer::eval(&file).unwrap();
					let mut parser = Parser::new(&tokens, self.source_root.clone());
					parser.process();
					self.preprocessor.labels.extend(parser.preprocessor.labels);
					self.preprocessor.values.extend(parser.preprocessor.values);
					self.preprocessor.macros.extend(parser.preprocessor.macros);
					trace!("found assembly include: '{name}'");
				}

				TT::BInclude => {
					let name = self.string().unwrap();
					let path = Path::join(&self.source_root, name.to_string());
					let file = std::fs::read(path).unwrap();
					for byte in file {
						self.push(IR::Byte(byte));
					}
					trace!("found binary include: '{name}'");
				}

				TT::Align => {
					let imm = self.immediate().unwrap();
					if imm == 2 {
						// If we're offset by 1, add 1 to realign.
						if self.preprocessor.address & 1 != 0 {
							self.push(IR::Byte(0));
						}
					} else if imm == 4 {
						// If we're offset, use the inverted offset to realign, while ensuring we don't add
						// anything if we're already aligned.
						for _ in 0..(4 - (self.preprocessor.address & 3)) & 3 {
							self.push(IR::Byte(0));
						}
					} else {
						panic!("align must be followed by '2' or '4'");
					}
				}

				TT::LtOrg => {
					self.preprocessor.address += waiting_words * 2;
					self.preprocessor.address += waiting_longs * 4;
					waiting_words = 0;
					waiting_longs = 0;
				}

				TT::Label(ref name) => {
					let name = name.clone();
					if self.token(TT::Colon).is_some() {
						if self.preprocessor.labels.contains_key(&name) {
							panic!("duplicate label: {name}");
						} else {
							trace!("found label: '{name}'");
							self.preprocessor.labels.insert(name.clone(), self.preprocessor.address);
						}
					} else if self.token(TT::Eq).is_some() {
						let value = self.immediate().unwrap();
						if self.preprocessor.values.contains_key(&name) {
							panic!("duplicate value: {name} = {value}");
						} else {
							trace!("found value: {name} = {value}");
							self.preprocessor.values.insert(name.clone(), value);
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
							self.preprocessor.macros.insert(name.clone(), tokens);
							break;
						}
						end = self.index;
					}
					trace!("found macro start: '{name}'");
				}
				TT::MacroEnd => {
					panic!("unexpected macro end");
				}

				TT::Const => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						if let Some(imm) = self.neg_immediate() {
							if !i8_sized(imm) {
								panic!("wrong size: {imm}");
							}
							self.push(IR::Byte(imm as u8));
						} else if let Some(name) = self.string() {
							for c in name.chars() {
								self.push(IR::Byte(c as u8));
							}
							while let Some(imm) = self.token(TT::Comma)
								.and_then(|_| self.immediate())
							{
								if i8_sized(imm) || u8_sized(imm) {
									self.push(IR::Byte(imm as u8));
								} else {
									panic!("byte constant too large {imm}");
								}
							}
						} else {
							self.unexpected(line!());
						}
					} else if self.token(TT::Word).is_some() {
						let imm = self.neg_immediate().unwrap();
						if i16_sized(imm) || u16_sized(imm) {
							self.push(IR::Word(imm as u16));
						} else {
							panic!("too large {imm}");
						}
					} else if self.token(TT::Long).is_some() {
						if let Some(imm) = self.neg_immediate() {
							if i32_sized(imm) || u32_sized(imm) {
								self.push(IR::Long(imm as u32));
							} else {
								panic!("too large {imm}");
							}
						} else if let Some(label) = self.label() {
							self.push(IR::Placeholder4);
							error!("handle long-constant labels: {label}");
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::Space => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						let imm = self.immediate().unwrap();
						for _ in 0..imm {
							self.push(IR::Byte(0));
						}
					} else if self.token(TT::Word).is_some() {
						let imm = self.immediate().unwrap();
						for _ in 0..imm {
							self.push(IR::Word(0));
						}
					} else if self.token(TT::Long).is_some() {
						let imm = self.immediate().unwrap();
						for _ in 0..imm {
							self.push(IR::Long(0));
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::Mov => {
					if self.token(TT::Hash).is_some() {
						let imm = self.neg_immediate().unwrap();
						self.token(TT::Comma).unwrap();
						let rn = self.reg().unwrap();
						if i8_sized(imm) {
							self.push(IR::NI(AT::MovImm, rn, imm as u8));
						} else if i16_sized(imm) {
							self.push_placeholder(AT::MovPcRegW, rn);
						} else if i32_sized(imm) {
							self.push_placeholder(AT::MovPcRegL, rn);
						} else {
							self.unexpected(line!());
						}
					} else if let Some(rm) = self.reg() {
						self.token(TT::Comma).unwrap();
						let rn = self.reg().unwrap();
						self.push(IR::Two(AT::MovRegReg, rn, rm));
					} else if self.token(TT::Dot).is_some() {
						if self.token(TT::Byte).is_some() {
							if let Some(rm) = self.idx() {
								self.token(TT::Comma).unwrap();
								let rn = self.reg().unwrap();
								// MOV.B @(R0,Rm),Rn
								self.push(IR::Two(AT::MovR0RegB, rn, rm));
							} else if let Some(d) = self.disp_gbr() {
								self.token(TT::Comma).unwrap();
								assert_eq!(Some(0), self.reg());
								// MOV.B @(disp,GBR),R0
								self.push(IR::Imm(AT::MovGbrR0B, d));
							} else if let Some((d,rm)) = self.disp_reg() {
								self.token(TT::Comma).unwrap();
								assert_eq!(Some(0), self.reg());
								// MOV.B @(disp,Rm),R0
								self.push(IR::Dsp4(AT::MovDspR0B, rm, d));
							} else if let Some(rm) = self.inc() {
								self.token(TT::Comma).unwrap();
								let rn = self.reg().unwrap();
								// MOV.B @Rm+,Rn
								self.push(IR::Two(AT::MovIncRegB, rn, rm));
							} else if let Some(rm) = self.adr() {
								self.token(TT::Comma).unwrap();
								let rn = self.reg().unwrap();
								// MOV.B @Rm,Rn
								self.push(IR::Two(AT::MovAdrRegB, rn, rm));
							} else if let Some(0) = self.reg() {
								self.token(TT::Comma).unwrap();
								if let Some(d) = self.disp_gbr() {
									// MOV.B R0,@(disp,GBR)
									self.push(IR::Imm(AT::MovR0GbrB, d));
								} else if let Some((d,rn)) = self.disp_reg() {
									// MOV.B R0,@(disp,Rn)
									self.push(IR::Dsp4(AT::MovR0DspB, rn, d));
								} else if let Some(rn) = self.idx() {
									// MOV.B R0,@(R0,Rn)
									self.push(IR::Two(AT::MovRegR0B, rn, 0));
								} else if let Some(rn) = self.dec() {
									// MOV.B R0,@-Rn
									self.push(IR::Two(AT::MovRegDecB, rn, 0));
								} else if let Some(rn) = self.adr() {
									// MOV.B R0,@Rn
									self.push(IR::Two(AT::MovRegAdrB, rn, 0));
								} else {
									self.unexpected(line!());
								}
							} else if let Some(rm) = self.reg() {
								self.token(TT::Comma).unwrap();
								if let Some(rn) = self.idx() {
									// MOV.B Rm,@(R0,Rn)
									self.push(IR::Two(AT::MovRegR0B, rn, rm));
								} else if let Some(rn) = self.dec() {
									// MOV.B Rm,@-Rn
									self.push(IR::Two(AT::MovRegDecB, rn, rm));
								} else if let Some(rn) = self.adr() {
									// MOV.B Rm,@Rn
									self.push(IR::Two(AT::MovRegAdrB, rn, rm));
								} else {
									panic!("unexpected token: {:?}", self.tokens[self.index]);
								}
							} else {
								self.unexpected(line!());
							}
						} else if self.token(TT::Word).is_some() {
							todo!("handle MOV.W instructions");
						} else if self.token(TT::Long).is_some() {
							todo!("handle MOV.L instructions");
						} else {
							self.unexpected(line!());
						}
					} else {
						panic!("unexpected token for MOV: {:?}", self.tokens[self.index]);
					}
				}

				TT::Add |
				TT::CmpEq => {
					if self.token(TT::Hash).is_some() {
						self.preprocessor.address += 2;
						if let Some(imm) = self.neg_immediate() {
							self.token(TT::Comma).unwrap();
							if i8_sized(imm) {
								// regular instruction
							} else {
								panic!("immediate value too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
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
						self.preprocessor.address += 2;
						if i8_sized(imm) {
							// regular instruction
						} else {
							panic!("immediate value too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
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
					self.preprocessor.address += 2;
				}
			}
		}
	}

	#[instrument(skip_all)]
	fn output(&mut self) {
		self.index = 0;

		while let Some(token) = self.next().cloned() {
			match token.tt {
				TT::LtOrg | TT::NewLine | TT::Eof => {
					continue;
				}
				TT::Include | TT::BInclude |
				TT::Label(_) |
				TT::Align => {
					self.index += 1;
					continue;
				}
				TT::Const => {
					let index = self.index;
					if self.token(TT::Dot).is_some() {
						if self.token(TT::Word).is_some() || self.token(TT::Long).is_some() {
							self.index += 1;
							continue;
						} else if self.token(TT::Byte).is_some() && self.string().is_some() {
							while self.token(TT::Comma).is_some() && self.immediate().is_some() {}
							continue;
						}
					}
					self.index = index;
				}
				_ => {}
			}

			println!("{:08X} : {:?}", 2 * self.out.len(), token.tt);

			match token.tt {
				TT::Mov => {}

				TT::MovA => {
					let d = self.disp_pc().unwrap();
					self.token(TT::Comma).unwrap();
					self.r0().unwrap();
					let imm = d / 4;
					if !(u8::MIN as u16..=u8::MAX as u16).contains(&imm) {
						panic!("displacement value too large: found({d}), limit(0..={})", u8::MAX as u16 * 4);
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

				TT::Add => {
					if self.token(TT::Hash).is_some() {
						let imm = self.neg_immediate().unwrap();
						if i8_sized(imm) {
							self.token(TT::Comma).unwrap();
							let rn = self.reg().unwrap();
							self.out.push(Asm::imm_reg(AT::AddImmReg, imm as u8, rn));
						} else {
							panic!("ADD output too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
						}
					} else {
						let (rm,rn) = self.reg2().unwrap();
						self.out.push(Asm::reg2(AT::AddRegReg, rn ,rm));
					}
				}

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

				TT::ExtS => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.out.push(Asm::reg2(AT::ExtSB, rn, rm));
					} else if self.token(TT::Word).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.out.push(Asm::reg2(AT::ExtSW, rn, rm));
					} else {
						panic!("unexpected token for EXTS: {:?}", self.tokens[self.index]);
					}
				}

				TT::ExtU => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Byte).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.out.push(Asm::reg2(AT::ExtUB, rn, rm));
					} else if self.token(TT::Word).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.out.push(Asm::reg2(AT::ExtUW, rn, rm));
					} else {
						panic!("unexpected token for EXTU: {:?}", self.tokens[self.index]);
					}
				}

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
					if !self.preprocessor.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BfS => {
					let label = self.label().unwrap();
					if !self.preprocessor.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bt => {
					let label = self.label().unwrap();
					if !self.preprocessor.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BtS => {
					let label = self.label().unwrap();
					if !self.preprocessor.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bra => {
					let label = self.label().unwrap();
					if !self.preprocessor.labels.contains_key(&label) {
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
					if !self.preprocessor.labels.contains_key(&label) {
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
						panic!("trap offset too large: found({imm}), limit(0..={})", u8::MAX as i64 * 4);
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

impl Preprocessor {
	fn push(&mut self, ir: IR) {
		match ir {
			IR::Byte(_) => {
				self.intermediates.push((self.address, ir));
				self.address += 1;
			}
			IR::Long(_) | IR::Placeholder4 => {
				self.intermediates.push((self.address, ir));
				self.address += 4;
			}
			_ => {
				self.intermediates.push((self.address, ir));
				self.address += 2;
			}
		}
	}

	fn push_placeholder(&mut self, at: AT, ph: PH) {
		self.waiting.push((self.address, at, ph));
		self.push(IR::Placeholder);
	}
}

impl<'a> Parser<'a> {
	fn new(tokens: &'a [Token], source_root: PathBuf) -> Self {
		Self {
			source_root,

			index: 0,
			tokens,

			preprocessor: Preprocessor::default(),

			out: Vec::default(),
		}
	}

	fn unexpected(&self, line: u32) {
		let start = self.tokens[..self.index].iter().rposition(|t| t.tt == TT::NewLine);
		let end = self.tokens[self.index..].iter().position(|t| t.tt == TT::NewLine);
		match (start, end) {
			(Some(start), Some(end)) => eprintln!("{:?}", &self.tokens[start..self.index+end]),
			(None, Some(end)) => eprintln!("{:?}", &self.tokens[..self.index+end]),
			(Some(start), None) => eprintln!("{:?}", &self.tokens[start..]),
			(None, None) => eprintln!("{:?}", self.tokens),
		}
		panic!("[{line:03}]: unexpected token: {:?}", self.tokens[self.index]);
	}

	fn push(&mut self, ir: IR) {
		self.preprocessor.push(ir);
	}

	fn push_placeholder(&mut self, at: AT, ph: PH) {
		self.preprocessor.push_placeholder(at, ph);
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

	fn char(&mut self) -> Option<char> {
		if let Some(Token { tt: TT::Char(c), ..}) = self.next() {
			Some(*c)
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
				self.preprocessor.values.get(&label).copied()
			})
			.filter(|_| self.token(TT::Comma).is_some())
			.filter(|_| self.token(TT::Pc).is_some())
			.filter(|_| self.token(TT::CParen).is_some())
			.and_then(|d| if u16_sized(d) {
				Some(d as u16)
			} else {
				error!("displacement value too large: found({d}), limit({}..={})", i32::MIN, i32::MAX);
				None
			})
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn disp_gbr(&mut self) -> Option<u8> {
		let index = self.index;
		self.token(TT::At)
			.filter(|_| self.token(TT::OParen).is_some())
			.and_then(|_| self.immediate())
			.filter(|_| self.token(TT::Comma).is_some())
			.filter(|_| self.token(TT::Gbr).is_some())
			.filter(|_| self.token(TT::CParen).is_some())
			.and_then(|d| if i8_sized(d) {
				Some(d as u8)
			} else {
				error!("displacement value too large: found({d}), limit({}..={})", i8::MIN, i8::MAX);
				None
			})
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn disp_reg(&mut self) -> Option<(u8,u8)> {
		let index = self.index;
		self.token(TT::At)
			.filter(|_| self.token(TT::OParen).is_some())
			.and_then(|_| self.immediate())
			.and_then(|d| if u8_sized(d) {
				Some(d as u8)
			} else {
				error!("displacement value too large: found({d}), limit(0..={})", u8::MAX);
				None
			})
			.filter(|_| self.token(TT::Comma).is_some())
			.zip(self.reg())
			.filter(|_| self.token(TT::CParen).is_some())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn label_reg(&mut self) -> Option<(Box<str>,u8)> {
		let index = self.index;
		self.token(TT::At)
			.filter(|_| self.token(TT::OParen).is_some())
			.and_then(|_| self.label())
			.filter(|_| self.token(TT::Comma).is_some())
			.zip(self.reg())
			.filter(|_| self.token(TT::CParen).is_some())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn idx(&mut self) -> Option<u8> {
		let index = self.index;
		self.token(TT::At)
			.filter(|_| self.token(TT::OParen).is_some())
			.filter(|_| self.r0().is_some())
			.filter(|_| self.token(TT::Comma).is_some())
			.and_then(|_| self.reg())
			.filter(|_| self.token(TT::CParen).is_some())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn inc(&mut self) -> Option<u8> {
		let index = self.index;
		self.token(TT::At)
			.and_then(|_| self.reg())
			.filter(|_| self.token(TT::Plus).is_some())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn adr(&mut self) -> Option<u8> {
		let index = self.index;
		self.token(TT::At)
			.and_then(|_| self.reg())
			.or_else(|| {
				self.index = index;
				None
			})
	}

	fn dec(&mut self) -> Option<u8> {
		let index = self.index;
		self.token(TT::At)
			.filter(|_| self.token(TT::Dash).is_some())
			.and_then(|_| self.reg())
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

fn u8_sized(imm: i64) -> bool {
	(0..=u8::MAX as i64).contains(&imm)
}

fn i16_sized(imm: i64) -> bool {
	(i16::MIN as i64..=i16::MAX as i64).contains(&imm)
}

fn u16_sized(imm: i64) -> bool {
	(0..=u16::MAX as i64).contains(&imm)
}

fn i32_sized(imm: i64) -> bool {
	(i32::MIN as i64..=i32::MAX as i64).contains(&imm)
}

fn u32_sized(imm: i64) -> bool {
	(0..=u32::MAX as i64).contains(&imm)
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
	assert_eq!(parser.preprocessor.labels["start"], 0, "'start' label should start at address 0");
	assert_eq!(parser.preprocessor.labels["end"], 6, "'end' label should start at address 6");
}

