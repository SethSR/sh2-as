
use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};

use tracing::{instrument, error, trace};

use crate::tokens::{Token, Type as TT};
use crate::asm::{Asm, Type as AT};

#[derive(Debug, Clone)]
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
	/// xxxx nnnn iiii iiii
	NI(AT, u8, u8),

	Jmp(AT, Box<str>),

	Byte(u8),
	Word(u16),
	Long(u32),

	Placeholder,
	Placeholder4,
}

#[derive(Debug, Clone, Copy)]
enum Sz {
	Word(i64),
	Long(i64),
}

#[derive(Debug, Clone)]
enum PH {
	Reg(u8),
	Label(Box<str>),
	// Label,Rn
	LabelReg(Box<str>,u8),
	// #Label,Rn
	ImmLabelReg(Box<str>,u8),
	// @(Label,Rm),Rn
	// Rm,@(Label,Rn)
	Dsp(Box<str>,u8,u8),
}

pub fn eval(tokens: &[Token], source_root: PathBuf) -> Vec<Asm> {
	let mut parser = Parser::new(tokens, source_root);
	parser.process();
	parser.output();
	parser.out
}

#[derive(Debug, Default, Clone)]
struct Preprocessor {
	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
	macros: HashMap<Box<str>, Vec<Token>>,
	constants: VecDeque<(u32,i64)>,
	waiting_constants: Vec<Sz>,
	waiting: Vec<(u32, Option<AT>, PH)>,

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
					trace!("found assembly include: '{name}'");
					let path = Path::join(&self.source_root, name.to_string());
					let file = std::fs::read_to_string(path).unwrap();
					let tokens = crate::lexer::eval(&file).unwrap();
					let mut parser = Parser::new(&tokens, self.source_root.clone());
					parser.preprocessor = self.preprocessor.clone();
					parser.process();
					self.preprocessor = parser.preprocessor.clone();
				}

				TT::BInclude => {
					let name = self.string().unwrap();
					trace!("found binary include: '{name}'");
					let path = Path::join(&self.source_root, name.to_string());
					let file = std::fs::read(path).unwrap();
					for byte in file {
						self.push(IR::Byte(byte));
					}
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
					let waiting_constants: Vec<Sz> = self.preprocessor.waiting_constants.drain(..).collect();
					for item in waiting_constants {
						match item {
							Sz::Word(imm) => {
								self.preprocessor.constants.push_back((self.preprocessor.address, imm));
								self.push(IR::Word(imm as u16));
							}
							Sz::Long(imm) => {
								self.preprocessor.constants.push_back((self.preprocessor.address, imm));
								self.push(IR::Long(imm as u32));
							}
						}
					}
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
							self.push_const_placeholder(label);
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
						if let Some((imm,rn)) = self.pair(|p| p.neg_immediate(), |p| p.reg()) {
							if i8_sized(imm) {
								self.push(IR::NI(AT::MovImm, rn, imm as u8));
							} else if i16_sized(imm) {
								self.preprocessor.waiting_constants.push(Sz::Word(imm));
								self.push_placeholder(AT::MovPcRegW, PH::Reg(rn));
							} else if i32_sized(imm) {
								self.preprocessor.waiting_constants.push(Sz::Long(imm));
								self.push_placeholder(AT::MovPcRegL, PH::Reg(rn));
							} else {
								self.unexpected(line!());
							}
						} else if let Some((label,rn)) = self.label_reg() {
							self.push_placeholder(AT::MovPcRegW, PH::ImmLabelReg(label,rn));
						} else if let Some((c,rn)) = self.char_reg() {
							self.push(IR::NI(AT::MovImm, rn, c as u8));
						} else {
							self.unexpected(line!());
						}
					} else if let Some((rm,rn)) = self.reg2() {
						self.push(IR::Two(AT::MovRegReg, rn, rm));
					} else if self.token(TT::Dot).is_some() {
						if self.token(TT::Byte).is_some() {
							if let Some((rm,rn)) = self.idx_reg() {
								// MOV.B @(R0,Rm),Rn
								self.push(IR::Two(AT::MovR0RegB, rn, rm));
							} else if let Some((d,0)) = self.dspgbr_reg() {
								// MOV.B @(disp,GBR),R0
								self.push(IR::Imm(AT::MovGbrR0B, d));
							} else if let Some(((d,rm),0)) = self.dspreg_reg() {
								// MOV.B @(disp,Rm),R0
								self.push(IR::Dsp4(AT::MovDspR0B, rm, d));
							} else if let Some((rm,rn)) = self.inc_reg() {
								// MOV.B @Rm+,Rn
								self.push(IR::Two(AT::MovIncRegB, rn, rm));
							} else if let Some((rm,rn)) = self.adr_reg() {
								// MOV.B @Rm,Rn
								self.push(IR::Two(AT::MovAdrRegB, rn, rm));
							} else if let Some((0,d)) = self.reg_dspgbr() {
								// MOV.B R0,@(disp,GBR)
								self.push(IR::Imm(AT::MovR0GbrB, d));
							} else if let Some((0,(d,rn))) = self.reg_dspreg() {
								// MOV.B R0,@(disp,Rn)
								self.push(IR::Dsp4(AT::MovR0DspB, rn, d));
							} else if let Some((rm,rn)) = self.reg_idx() {
								// MOV.B Rm,@(R0,Rn)
								self.push(IR::Two(AT::MovRegR0B, rn, rm));
							} else if let Some((rm,rn)) = self.reg_dec() {
								// MOV.B Rm,@-Rn
								self.push(IR::Two(AT::MovRegDecB, rn, rm));
							} else if let Some((rm,rn)) = self.reg_adr() {
								// MOV.B Rm,@Rn
								self.push(IR::Two(AT::MovRegAdrB, rn, rm));
							} else {
								self.unexpected(line!());
							}
						} else if self.token(TT::Word).is_some() {
							if let Some((label,rn)) = self.label_reg() {
								self.push_placeholder(AT::MovPcRegW, PH::LabelReg(label,rn));
							} else if let Some((rm,rn)) = self.idx_reg() {
								// MOV.W @(R0,Rm),Rn
								self.push(IR::Two(AT::MovR0RegW, rn, rm));
							} else if let Some((d,0)) = self.dspgbr_reg() {
								// MOV.W @(disp,GBR),R0
								self.push(IR::Imm(AT::MovGbrR0W, d));
							} else if let Some((d,rn)) = self.pair(|p| p.disp_pc(), |p| p.reg()) {
								// MOV.W @(disp,PC),Rn
								self.push(IR::NI(AT::MovPcRegW, rn, d as u8));
							} else if let Some((d,rn)) = self.pair(|p| p.label_pc(), |p| p.reg()) {
								// MOV.W @(label,PC),Rn
								self.push(IR::NI(AT::MovPcRegW, rn, d as u8));
							} else if let Some(((d,rm),0)) = self.dspreg_reg() {
								// MOV.W @(disp,Rm),R0
								self.push(IR::Dsp4(AT::MovDspR0W, rm, d));
							} else if let Some((rm,rn)) = self.inc_reg() {
								// MOV.W @Rm+,Rn
								self.push(IR::Two(AT::MovIncRegW, rn, rm));
							} else if let Some((rm,rn)) = self.adr_reg() {
								// MOV.W @Rm,Rn
								self.push(IR::Two(AT::MovAdrRegW, rn, rm));
							} else if let Some((0,d)) = self.reg_dspgbr() {
								// MOV.W R0,@(disp,GBR)
								self.push(IR::Imm(AT::MovR0GbrW, d));
							} else if let Some((0,(d,rn))) = self.reg_dspreg() {
								// MOV.W R0,@(disp,Rn)
								self.push(IR::Dsp4(AT::MovR0DspW, rn, d));
							} else if let Some((rm,rn)) = self.reg_idx() {
								// MOV.W Rm,@(R0,Rn)
								self.push(IR::Two(AT::MovRegR0W, rn, rm));
							} else if let Some((rm,rn)) = self.reg_dec() {
								// MOV.W R0,@-Rn
								self.push(IR::Two(AT::MovRegDecW, rn, rm));
							} else if let Some((rm,rn)) = self.reg_adr() {
								// MOV.W R0,@Rn
								self.push(IR::Two(AT::MovRegAdrW, rn, rm));
							} else {
								self.unexpected(line!());
							}
						} else if self.token(TT::Long).is_some() {
							if let Some((label,rn)) = self.pair(|p| p.token(TT::Hash).and_then(|_| p.label()), |p| p.reg()) {
								self.push_placeholder(AT::MovPcRegL, PH::ImmLabelReg(label,rn));
							} else if let Some((label,rn)) = self.pair(|p| p.label(), |p| p.reg()) {
								self.push_placeholder(AT::MovPcRegL, PH::LabelReg(label,rn));
							} else if let Some((rm,rn)) = self.idx_reg() {
								// MOV.L @(R0,Rm),Rn
								self.push(IR::Two(AT::MovR0RegL, rn, rm));
							} else if let Some((d,0)) = self.dspgbr_reg() {
								// MOV.L @(disp,GBR),R0
								self.push(IR::Imm(AT::MovGbrR0L, d));
							} else if let Some((d,rn)) = self.pair(|p| p.disp_pc(), |p| p.reg()) {
								// MOV.L @(disp,PC),Rn
								self.push(IR::NI(AT::MovPcRegL, rn, d as u8));
							} else if let Some((d,rn)) = self.pair(|p| p.label_pc(), |p| p.reg()) {
								// MOV.L @(label,PC),Rn
								self.push(IR::NI(AT::MovPcRegL, rn, d as u8));
							} else if let Some(((d,rm),rn)) = self.dspreg_reg() {
								// MOV.L @(disp,Rm),Rn
								self.push(IR::NM4(AT::MovDspRegL, rn, rm, d));
							} else if let Some(((label,rm),rn)) = self.pair(|p| p.label_reg(), |p| p.reg()) {
								self.push_placeholder(AT::MovDspRegL, PH::Dsp(label,rm,rn));
							} else if let Some((rm,rn)) = self.inc_reg() {
								// MOV.L @Rm+,Rn
								self.push(IR::Two(AT::MovIncRegL, rn, rm));
							} else if let Some((rm,rn)) = self.adr_reg() {
								// MOV.L @Rm,Rn
								self.push(IR::Two(AT::MovAdrRegL, rn, rm));
							} else if let Some((0,d)) = self.reg_dspgbr() {
								// MOV.L R0,@(disp,GBR)
								self.push(IR::Imm(AT::MovR0GbrL, d));
							} else if let Some((rm,(d,rn))) = self.reg_dspreg() {
								// MOV.L Rm,@(disp,Rn)
								self.push(IR::NM4(AT::MovRegDspL, rn, rm, d));
							} else if let Some((rm,(label,rn))) = self.pair(|p| p.reg(), |p| p.label_reg()) {
								self.push_placeholder(AT::MovRegDspL, PH::Dsp(label,rm,rn));
							} else if let Some((rm,rn)) = self.reg_idx() {
								// MOV.L Rm,@(R0,Rn)
								self.push(IR::Two(AT::MovRegR0L, rn, rm));
							} else if let Some((rm,rn)) = self.reg_dec() {
								// MOV.L Rm,@-Rn
								self.push(IR::Two(AT::MovRegDecL, rn, rm));
							} else if let Some((rm,rn)) = self.reg_adr() {
								// MOV.L Rm,@Rn
								self.push(IR::Two(AT::MovRegAdrL, rn, rm));
							} else {
								self.unexpected(line!());
							}
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::MovA => {
					let (d,_) = self.pair(|p| p.disp_pc(), |p| p.token(TT::Reg(0))).unwrap();
					let imm = d / 4;
					if !u8_sized(imm as i64) {
						panic!("displacement value too large: found({d}), limit(0..={})", u8::MAX as u16 * 4);
					}
					self.push(IR::Imm(AT::MovA, imm as u8));
				}

				TT::MovT => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::MovT, rn));
				}

				TT::Swap => {
					if self.token_list(&[TT::Dot, TT::Byte]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::SwapB, rn, rm));
					} else if self.token_list(&[TT::Dot, TT::Word]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::SwapW, rn, rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::Xtrct => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Xtrct, rn, rm));
				}

				TT::Add => {
					if self.token(TT::Hash).is_some() {
						if let Some((imm,rn)) = self.pair(|p| p.neg_immediate(), |p| p.reg()) {
							if i8_sized(imm) {
								self.push(IR::NI(AT::AddImmReg, rn, imm as u8));
							} else {
								panic!("ADD input too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
							}
						} else if let Some((c,rn)) = self.char_reg() {
							self.push(IR::NI(AT::AddImmReg, rn, c as u8));
						} else {
							self.unexpected(line!());
						}
					} else if let Some((rm,rn)) = self.reg2() {
						self.push(IR::Two(AT::AddRegReg, rn ,rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::AddC => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::AddC, rn, rm));
				}

				TT::AddV => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::AddV, rn, rm));
				}

				TT::CmpEq => {
					if self.token(TT::Hash).is_some() {
						if let Some((imm,rn)) = self.pair(|p| p.neg_immediate(), |p| p.reg()) {
							if i8_sized(imm) {
								self.push(IR::NI(AT::CmpEqImm, rn, imm as u8));
							} else {
								panic!("CMP/EQ input too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
							}
						} else {
							self.unexpected(line!());
						}
					} else if let Some((rm,rn)) = self.reg2() {
						self.push(IR::Two(AT::CmpEqReg, rn ,rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::CmpHs => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpHs, rn, rm));
				}

				TT::CmpGe => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpGe, rn, rm));
				}

				TT::CmpHi => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpHi, rn, rm));
				}

				TT::CmpGt => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpGt, rn, rm));
				}

				TT::CmpPl => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpPl, rn, rm));
				}

				TT::CmpPz => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpPz, rn, rm));
				}

				TT::CmpStr => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::CmpStr, rn, rm));
				}

				TT::Div1 => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Div1, rn, rm));
				}

				TT::Div0S => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Div0S, rn, rm));
				}

				TT::Div0U => {
					self.push(IR::Zero(AT::Div0U));
				}

				TT::DMulS => {
					self.token(TT::Dot).unwrap();
					self.token(TT::Long).unwrap();
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::DMulS, rn, rm));
				}

				TT::DMulU => {
					self.token(TT::Dot).unwrap();
					self.token(TT::Long).unwrap();
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::DMulU, rn, rm));
				}

				TT::Dt => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::Dt, rn));
				}

				TT::ExtS => {
					if self.token_list(&[TT::Dot, TT::Byte]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::ExtSB, rn, rm));
					} else if self.token_list(&[TT::Dot, TT::Word]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::ExtSW, rn, rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::ExtU => {
					if self.token_list(&[TT::Dot, TT::Byte]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::ExtUB, rn, rm));
					} else if self.token_list(&[TT::Dot, TT::Word]).is_some() {
						let (rm,rn) = self.reg2().unwrap();
						self.push(IR::Two(AT::ExtUW, rn, rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::Mac => {
					self.token(TT::Dot).unwrap();
					if self.token(TT::Word).is_some() {
						let (rm,rn) = self.pair(|p| p.inc(), |p| p.inc()).unwrap();
						self.push(IR::Two(AT::MacW, rn, rm));
					} else if self.token(TT::Long).is_some() {
						let (rm,rn) = self.pair(|p| p.inc(), |p| p.inc()).unwrap();
						self.push(IR::Two(AT::MacL, rn, rm));
					} else {
						self.unexpected(line!());
					}
				}

				TT::Mul => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Mul, rn, rm));
				}

				TT::MulS => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::MulS, rn, rm));
				}

				TT::MulU => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::MulU, rn, rm));
				}

				TT::Neg => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Neg, rn, rm));
				}

				TT::NegC => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::NegC, rn, rm));
				}

				TT::Sub => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Sub, rn, rm));
				}

				TT::SubC => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::SubC, rn, rm));
				}

				TT::SubV => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::SubV, rn, rm));
				}

				TT::And => {
					self.logic(AT::AndImmR0, AT::AndImmGbr, AT::AndRegReg);
				}

				TT::Not => {
					let (rm,rn) = self.reg2().unwrap();
					self.push(IR::Two(AT::Not, rn, rm));
				}

				TT::Or => {
					self.logic(AT::OrImmR0, AT::OrImmGbr, AT::OrRegReg);
				}

				TT::Tas => {
					let rn = self.adr().unwrap();
					self.push(IR::One(AT::Tas, rn));
				}

				TT::Tst => {
					self.logic(AT::TstImmR0, AT::TstImmGbr, AT::TstRegReg);
				}

				TT::Xor => {
					self.logic(AT::XorImmR0, AT::XorImmGbr, AT::XorRegReg);
				}

				TT::RotL => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::RotL, rn));
				}

				TT::RotR => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::RotR, rn));
				}

				TT::RotCL => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::RotCL, rn));
				}

				TT::RotCR => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::RotCR, rn));
				}

				TT::ShAL => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShAL, rn));
				}

				TT::ShAR => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShAR, rn));
				}

				TT::ShLL => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLL, rn));
				}

				TT::ShLR => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLR, rn));
				}

				TT::ShLL2 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLL2, rn));
				}

				TT::ShLR2 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLR2, rn));
				}

				TT::ShLL8 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLL8, rn));
				}

				TT::ShLR8 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLR8, rn));
				}

				TT::ShLL16 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLL16, rn));
				}

				TT::ShLR16 => {
					let rn = self.reg().unwrap();
					self.push(IR::One(AT::ShLR16, rn));
				}

				TT::Bf => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::Bf, label));
				}

				TT::BfS => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::BfS, label));
				}

				TT::Bt => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::Bt, label));
				}

				TT::BtS => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::BtS, label));
				}

				TT::Bra => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::Bra, label));
				}

				TT::BraF => {
					let rm = self.reg().unwrap();
					self.push(IR::One(AT::BraF, rm));
				}

				TT::Bsr => {
					let label = self.label().unwrap();
					self.push(IR::Jmp(AT::Bsr, label));
				}

				TT::BsrF => {
					let rm = self.reg().unwrap();
					self.push(IR::One(AT::BsrF, rm));
				}

				TT::Jmp => {
					let rm = self.adr().unwrap();
					self.push(IR::One(AT::Jmp, rm));
				}

				TT::Jsr => {
					let rm = self.adr().unwrap();
					self.push(IR::One(AT::Jsr, rm));
				}

				TT::Rts => {
					self.push(IR::Zero(AT::Rts));
				}

				TT::ClrT => {
					self.push(IR::Zero(AT::ClrT));
				}

				TT::ClrMac => {
					self.push(IR::Zero(AT::ClrMac));
				}

				TT::LdC => {
					if let Some(rm) = self.reg_spec(TT::Sr) {
						self.push(IR::One(AT::LdcRegSr, rm));
					} else if let Some(rm) = self.reg_spec(TT::Gbr) {
						self.push(IR::One(AT::LdcRegGbr, rm));
					} else if let Some(rm) = self.reg_spec(TT::Vbr) {
						self.push(IR::One(AT::LdcRegVbr, rm));
					} else if self.token_list(&[TT::Dot, TT::Long]).is_some() {
						if let Some(rm) = self.inc_spec(TT::Sr) {
							self.push(IR::One(AT::LdcIncSr, rm));
						} else if let Some(rm) = self.inc_spec(TT::Gbr) {
							self.push(IR::One(AT::LdcIncGbr, rm));
						} else if let Some(rm) = self.inc_spec(TT::Vbr) {
							self.push(IR::One(AT::LdcIncVbr, rm));
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::LdS => {
					if let Some(rm) = self.reg_spec(TT::Mach) {
						self.push(IR::One(AT::LdsRegMach, rm));
					} else if let Some(rm) = self.reg_spec(TT::Macl) {
						self.push(IR::One(AT::LdsRegMacl, rm));
					} else if let Some(rm) = self.reg_spec(TT::Pr) {
						self.push(IR::One(AT::LdsRegPr, rm));
					} else if self.token_list(&[TT::Dot, TT::Long]).is_some() {
						if let Some(rm) = self.inc_spec(TT::Mach) {
							self.push(IR::One(AT::LdsIncMach, rm));
						} else if let Some(rm) = self.inc_spec(TT::Macl) {
							self.push(IR::One(AT::LdsIncMacl, rm));
						} else if let Some(rm) = self.inc_spec(TT::Pr) {
							self.push(IR::One(AT::LdsIncPr, rm));
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::Nop => {
					self.push(IR::Zero(AT::Nop));
				}

				TT::Rte => {
					self.push(IR::Zero(AT::Rte));
				}

				TT::SetT => {
					self.push(IR::Zero(AT::SetT));
				}

				TT::Sleep => {
					self.push(IR::Zero(AT::Sleep));
				}

				TT::StC => {
					if let Some(rn) = self.spec_reg(TT::Sr) {
						self.push(IR::One(AT::StcSrReg, rn));
					} else if let Some(rn) = self.spec_reg(TT::Gbr) {
						self.push(IR::One(AT::StcGbrReg, rn));
					} else if let Some(rn) = self.spec_reg(TT::Vbr) {
						self.push(IR::One(AT::StcVbrReg, rn));
					} else if self.token_list(&[TT::Dot, TT::Long]).is_some() {
						if let Some(rn) = self.spec_dec(TT::Sr) {
							self.push(IR::One(AT::StcSrDec, rn));
						} else if let Some(rn) = self.spec_dec(TT::Gbr) {
							self.push(IR::One(AT::StcGbrDec, rn));
						} else if let Some(rn) = self.spec_dec(TT::Vbr) {
							self.push(IR::One(AT::StcVbrDec, rn));
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::StS => {
					if let Some(rn) = self.spec_reg(TT::Mach) {
						self.push(IR::One(AT::StsMachReg, rn));
					} else if let Some(rn) = self.spec_reg(TT::Macl) {
						self.push(IR::One(AT::StsMaclReg, rn));
					} else if let Some(rn) = self.spec_reg(TT::Pr) {
						self.push(IR::One(AT::StsPrReg, rn));
					} else if self.token_list(&[TT::Dot, TT::Long]).is_some() {
						if let Some(rn) = self.spec_dec(TT::Mach) {
							self.push(IR::One(AT::StsMachDec, rn));
						} else if let Some(rn) = self.spec_dec(TT::Macl) {
							self.push(IR::One(AT::StsMaclDec, rn));
						} else if let Some(rn) = self.spec_dec(TT::Pr) {
							self.push(IR::One(AT::StsPrDec, rn));
						} else {
							self.unexpected(line!());
						}
					} else {
						self.unexpected(line!());
					}
				}

				TT::TrapA => {
					self.token(TT::Hash).unwrap();
					let imm = self.immediate().unwrap();
					let d = imm / 4;
					if !u8_sized(d) {
						panic!("trap offset too large: found({imm}), limit(0..={})", u8::MAX as i64 * 4);
					}
					self.push(IR::Imm(AT::TrapA, d as u8));
				}

				// Skip empty lines
				TT::NewLine => {}

				TT::String(_) | TT::Char(_) |
				TT::Bin(_) | TT::Dec(_) | TT::Hex(_) |
				TT::Reg(_) | TT::Pc |
				TT::Gbr | TT::Vbr | TT::Sr |
				TT::Macl | TT::Mach | TT::Pr |
				TT::Plus | TT::Dash | TT::Star | TT::Slash |
				TT::At | TT::OParen | TT::CParen |
				TT::Colon | TT::Dot | TT::Comma |
				TT::Eq | TT::Hash |
				TT::Byte | TT::Word | TT::Long => {
					self.unexpected(line!());
				}
			}
		}
	}

	#[instrument(skip_all)]
	fn output(&mut self) {
		for (addr, at, ph) in self.preprocessor.waiting.drain(..) {
			let index = self.preprocessor.intermediates.iter().position(|(ir_addr,_)| *ir_addr == addr).unwrap();
			if let Some(at) = at {
				match at {
					AT::MovPcRegW => match ph {
						PH::Reg(rn) => {
							let (caddr,_) = self.preprocessor.constants.pop_front().unwrap();
							let offset = (caddr - addr) >> 1;
							if !u8_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u8::MAX);
								continue;
							}
							self.preprocessor.intermediates[index] = (addr, IR::NI(at, rn, offset as u8));
						}
						PH::LabelReg(label,rn) | PH::ImmLabelReg(label,rn) => {
							let laddr = self.preprocessor.labels[&label];
							let offset = (laddr - addr) >> 1;
							if !u8_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u8::MAX);
								continue;
							}
							self.preprocessor.intermediates[index] = (addr, IR::NI(at, rn, offset as u8));
						}
						_ => todo!("{at:?} - {ph:?}"),
					}
					AT::MovPcRegL => match ph {
						PH::Reg(rn) => {
							let (caddr,_) = self.preprocessor.constants.pop_front().unwrap();
							let offset = (caddr - addr) >> 2;
							if !u8_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u8::MAX);
								continue;
							}
							self.preprocessor.intermediates[index] = (addr, IR::NI(at, rn, offset as u8));
						}
						PH::LabelReg(label,rn) | PH::ImmLabelReg(label,rn) => {
							let laddr = self.preprocessor.labels[&label];
							let offset = (laddr - addr) >> 2;
							if !u8_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u8::MAX);
								continue;
							}
							self.preprocessor.intermediates[index] = (addr, IR::NI(at, rn, offset as u8));
						}
						_ => todo!("{at:?} - {ph:?}"),
					}
					AT::MovDspRegL | AT::MovRegDspL => match ph {
						PH::Dsp(label,rm,rn) => {
							let d = self.preprocessor.values[&label];
							self.preprocessor.intermediates[index] = (addr, IR::NM4(at, rn, rm, d as u8));
						}
						_ => todo!("{at:?} - {ph:?}"),
					}
					_ => {
						panic!("Unexpected instruction type found for delayed output: '{at:?}'");
					}
				}
			} else {
				match ph {
					PH::Label(label) => {
						let laddr = self.preprocessor.labels[&label];
						self.preprocessor.intermediates[index] = (addr, IR::Long(laddr));
					}
					_ => todo!("{at:?} - {ph:?}"),
				}
			}
		}

		for (addr,ir) in self.preprocessor.intermediates.drain(..) {
			eprintln!("[{addr:08X}] {ir:X?}");
			match ir {
				IR::Zero(at) => self.out.push(Asm::none(at)),
				IR::One(at,r) => self.out.push(Asm::reg1(at, r)),
				IR::Two(at,rn,rm) => self.out.push(Asm::reg2(at, rn, rm)),
				IR::Dsp4(at,r,d) => self.out.push(Asm::reg1_imm(at, r, d)),
				IR::NM4(at,n,m,d) => self.out.push(Asm::reg2_imm(at, n, m, d)),
				IR::Imm(at,d) => self.out.push(Asm::imm8(at, d)),
				IR::NI(at,r,d) => self.out.push(Asm::reg1_imm(at, r, d)),
				IR::Jmp(at,label) => {
					match at {
						AT::Bf | AT::BfS | AT::Bt | AT::BtS => {
							let laddr = self.preprocessor.labels[&label];
							let offset = (laddr - addr) >> 1;
							if !u8_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u8::MAX);
								continue;
							}
							self.out.push(Asm::imm8(at, offset as u8));
						}
						AT::Bra | AT::Bsr => {
							let laddr = self.preprocessor.labels[&label];
							let offset = (laddr - addr) >> 1;
							if !u16_sized(offset as i64) {
								error!("{at:?} offset too large: found({offset}), limit(0..={})", u16::MAX);
								continue;
							}
							self.out.push(Asm::imm12(at, offset as u16));
						}
						_ => {
							panic!("unexpected AsmType for Jump IR during output: {at:?}")
						}
					}
				}
				IR::Byte(_value) => {}
				IR::Word(_value) => {}
				IR::Long(_value) => {}
				IR::Placeholder | IR::Placeholder4 => unreachable!("found placeholder during output"),
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
		self.waiting.push((self.address, Some(at), ph));
		self.push(IR::Placeholder);
	}

	fn push_const_placeholder(&mut self, label: Box<str>) {
		self.waiting.push((self.address, None, PH::Label(label)));
		self.push(IR::Placeholder4);
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
		if let Some(token) = self.tokens.get(self.index) {
			panic!("[{line:03}]: unexpected token: {token:?}");
		} else {
			panic!("[{line:03}]: unexpected EOF");
		}
	}

	fn push(&mut self, ir: IR) {
		self.preprocessor.push(ir);
	}

	fn push_placeholder(&mut self, at: AT, ph: PH) {
		self.preprocessor.push_placeholder(at, ph);
	}

	fn push_const_placeholder(&mut self, label: Box<str>) {
		self.preprocessor.push_const_placeholder(label);
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

	fn token_list(&mut self, tts: &[TT]) -> Option<()> {
		let index = self.index;
		for tt in tts {
			if self.next()
				.filter(|token| &token.tt == tt)
				.is_none()
			{
				self.index = index;
				return None;
			}
		}
		Some(())
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

	fn single<A>(&mut self,
		fn_a: impl FnOnce(&mut Parser) -> Option<A>,
	) -> Option<A> {
		let index = self.index;
		fn_a(self).or_else(|| {
			self.index = index;
			None
		})
	}

	fn pair<A,B>(
		&mut self,
		fn_a: impl FnOnce(&mut Parser) -> Option<A>,
		fn_b: impl FnOnce(&mut Parser) -> Option<B>,
	) -> Option<(A,B)> {
		self.single(|p| fn_a(p)
			.filter(|_| p.token(TT::Comma).is_some())
			.zip(fn_b(p))
		)
	}

	fn at_pair<A,B>(
		&mut self,
		fn_a: impl FnOnce(&mut Parser) -> Option<A>,
		fn_b: impl FnOnce(&mut Parser) -> Option<B>,
	) -> Option<(A,B)> {
		self.single(|p| p.token_list(&[TT::At, TT::OParen])
			.and_then(|_| p.pair(fn_a, fn_b))
			.filter(|_| p.token(TT::CParen).is_some())
		)
	}

	/// @(imm,PC)
	fn disp_pc(&mut self) -> Option<u16> {
		self.at_pair(
			|p| p.immediate()
				.and_then(|d| if u16_sized(d) {
					Some(d as u16)
				} else {
					error!("displacement value too large: found({d}), limit({}..={})", i32::MIN, i32::MAX);
					None
				}),
			|p| p.token(TT::Pc),
		).map(|(d,_)| d)
	}
	
	/// @(label,PC)
	fn label_pc(&mut self) -> Option<u16> {
		self.at_pair(
			|p| p.label()
				.and_then(|label| p.preprocessor.values.get(&label).copied())
				.and_then(|d| if u16_sized(d) {
					Some(d as u16)
				} else {
					error!("displacement value too large: found({d}), limit({}..={})", i32::MIN, i32::MAX);
					None
				}),
			|p| p.token(TT::Pc),
		).map(|(d,_)| d)
	}

	/// @(imm,GBR)
	fn disp_gbr(&mut self) -> Option<u8> {
		self.at_pair(
			|p| p.immediate()
				.and_then(|d| if i8_sized(d) {
					Some(d as u8)
				} else {
					error!("displacement value too large: found({d}), limit({}..={})", i8::MIN, i8::MAX);
					None
				}),
			|p| p.token(TT::Gbr),
		).map(|(d,_)| d)
	}

	/// @(imm,reg)
	fn disp_reg(&mut self) -> Option<(u8,u8)> {
		self.at_pair(
			|p| p.immediate()
				.and_then(|d| if u8_sized(d) {
					Some(d as u8)
				} else {
					error!("displacement value too large: found({d}), limit(0..={})", u8::MAX);
					None
				}),
			|p| p.reg(),
		)
	}

	fn label_reg(&mut self) -> Option<(Box<str>,u8)> {
		self.at_pair(|p| p.label(), |p| p.reg())
	}

	fn idx(&mut self) -> Option<u8> {
		self.at_pair(|p| p.token(TT::Reg(0)), |p| p.reg())
			.map(|(_,r)| r)
	}

	fn reg2(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.reg(), |p| p.reg())
	}

	fn inc(&mut self) -> Option<u8> {
		self.single(|p| p.token(TT::At)
			.and_then(|_| p.reg())
			.filter(|_| p.token(TT::Plus).is_some())
		)
	}

	fn adr(&mut self) -> Option<u8> {
		self.single(|p| p.token(TT::At)
			.and_then(|_| p.reg())
		)
	}

	fn dec(&mut self) -> Option<u8> {
		self.single(|p| p.token_list(&[TT::At, TT::Dash])
			.and_then(|_| p.reg())
		)
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

	fn logic(&mut self, at_imm_r0: AT, at_imm_gbr: AT, at_reg_reg: AT) {
		let index = self.index;
		// LOGIC #imm,R0
		if let Some(imm) = self.token(TT::Hash)
			.and_then(|_| self.neg_immediate())
			.filter(|_| self.token_list(&[TT::Comma, TT::Reg(0)]).is_some())
			.and_then(|imm| if i8_sized(imm) {
				Some(imm as u8)
			} else {
				error!("input too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
				None
			})
		{
			self.push(IR::Imm(at_imm_r0, imm));
		}
		// LOGIC.B #imm,@(R0,GBR)
		else if let Some(imm) = self.token_list(&[TT::Dot, TT::Byte, TT::Hash])
			.and_then(|_| self.neg_immediate())
			.filter(|_| self.token_list(&[
				TT::Comma, TT::At, TT::OParen, TT::Reg(0), TT::Comma, TT::Gbr, TT::CParen,
			]).is_some())
			.and_then(|imm| if i8_sized(imm) {
				Some(imm as u8)
			} else {
				error!("input too large: found({imm}), limit({}..={})", i32::MIN, i32::MAX);
				None
			})
		{
			self.push(IR::Imm(at_imm_gbr, imm));
		}
		// LOGIC Rm,Rn
		else if let Some((rm,rn)) = self.reg2()
		{
			self.push(IR::Two(at_reg_reg, rn, rm));
		}
		else
		{
			self.index = index;
			self.unexpected(line!());
		}
	}

	fn idx_reg(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.idx(), |p| p.reg())
	}


	fn spec_reg(&mut self, special_register: TT) -> Option<u8> {
		self.pair(|p| p.token(special_register), |p| p.reg())
			.map(|(_,r)| r)
	}

	fn spec_dec(&mut self, special_register: TT) -> Option<u8> {
		self.pair(|p| p.token(special_register), |p| p.dec())
			.map(|(_,r)| r)
	}

	fn reg_spec(&mut self, special_register: TT) -> Option<u8> {
		self.pair(|p| p.reg(), |p| p.token(special_register))
			.map(|(r,_)| r)
	}

	fn inc_spec(&mut self, special_register: TT) -> Option<u8> {
		self.pair(|p| p.inc(), |p| p.token(special_register))
			.map(|(r,_)| r)
	}

	fn dspgbr_reg(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.disp_gbr(), |p| p.reg())
	}

	fn dspreg_reg(&mut self) -> Option<((u8,u8),u8)> {
		self.pair(|p| p.disp_reg(), |p| p.reg())
	}

	fn inc_reg(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.inc(), |p| p.reg())
	}

	fn adr_reg(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.adr(), |p| p.reg())
	}

	fn char_reg(&mut self) -> Option<(char,u8)> {
		self.pair(|p| p.char(), |p| p.reg())
	}

	fn reg_dspgbr(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.reg(), |p| p.disp_gbr())
	}

	fn reg_dspreg(&mut self) -> Option<(u8,(u8,u8))> {
		self.pair(|p| p.reg(), |p| p.disp_reg())
	}

	fn reg_idx(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.reg(), |p| p.idx())
	}

	fn reg_dec(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.reg(), |p| p.dec())
	}

	fn reg_adr(&mut self) -> Option<(u8,u8)> {
		self.pair(|p| p.reg(), |p| p.adr())
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
fn loop_labels_have_the_correct_addresses() {
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
	let pp = parser.preprocessor;
	assert_eq!(pp.labels["start"], 0, "'start' label should start at address 0\nWaiting{:?}\nIntermediates{:?}",
		pp.waiting, pp.intermediates);
	assert_eq!(pp.labels["end"], 6, "'end' label should start at address 6\nWaiting{:?}\nIntermediates{:?}",
		pp.waiting, pp.intermediates);
}

#[test]
fn subroutine_labels_have_the_correct_addresses() {
	let input = "
start:
	bsr fn1
	nop
	bsr fn2
	nop
	rts
	nop
fn1:
	rts
	mov #4,r1
fn2:
	add #5,r1
	rts
	mov r1,r0
	";
	let tokens = crate::lexer::eval(input).unwrap();
	let mut parser = Parser::new(&tokens, "".into());
	parser.process();
	let pp = parser.preprocessor;
	assert_eq!(pp.labels["start"], 0, "'start' label should start at address 0\nWaiting{:?}\nIntermediates{:?}",
		pp.waiting, pp.intermediates);
	assert_eq!(pp.labels["fn1"], 12, "'fn1' label should start at address 12\nWaiting{:?}\nIntermediates{:?}",
		pp.waiting, pp.intermediates);
	assert_eq!(pp.labels["fn2"], 16, "'fn2' label should start at address 16\nWaiting{:?}\nIntermediates{:?}",
		pp.waiting, pp.intermediates);
}

