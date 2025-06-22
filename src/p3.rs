
use std::collections::HashMap;
use std::path::PathBuf;

use tracing::{instrument, debug, error, trace};

use crate::tokens::{Token, Type as TT};
use crate::a2::{Asm, Type as AT};

pub fn eval(tokens: &[Token], _source_root: PathBuf) -> Vec<Asm> {
	let output = Preprocessor {
		data: Parser::new(tokens),
	}
		.process();
	output
		.output()
}

#[derive(Debug)]
pub struct Parser<'a> {
	index: usize,
	tokens: &'a [Token],
}

struct Preprocessor<'a> {
	data: Parser<'a>,
}

impl<'a> Preprocessor<'a> {
	/// Record labels, record and expand macros, expand included ASM and binary files, and track
	/// output addresses.
	#[instrument(skip_all)]
	fn process(mut self) -> Output<'a> {
		let mut labels = HashMap::new();
		let mut values = HashMap::new();
		let mut address = 0;

		while let Some(token) = self.data.next() {
			match token.tt {
				TT::Org => {
					let imm = self.data.immediate().unwrap();
					if imm > u32::MAX as i64 {
						todo!("address value too large: found({imm}), limit(0..={})", u32::MAX)
					}
					address = imm as u32;
					trace!("new section at address = {address:08X}");
				}

				TT::Include => {
					let name = self.data.string().unwrap();
					debug!("found assembly include: '{name}'");
				}

				TT::BInclude => {
					let name = self.data.string().unwrap();
					debug!("found binary include: '{name}'");
				}

				TT::Align => {
					let imm = self.data.immediate().unwrap();
					if imm == 2 {
						if address & 1 != 0 {
							address += 1;
						}
					} else if imm == 4 {
						match address & 3 {
							0 => {}
							1 => address += 3,
							2 => address += 2,
							3 => address += 1,
							_ => unreachable!(),
						}
					} else {
						todo!("align must be followed by '2' or '4'");
					}
				}

				TT::LtOrg => {
					debug!("found literal organize directive");
				}

				TT::Label(ref name) => {
					let name = name.clone();
					if self.data.token(TT::Colon).is_some() {
						if labels.contains_key(&name) {
							todo!("duplicate label: {name}");
						} else {
							trace!("found label: '{name}'");
							labels.insert(name.clone(), address);
						}
					} else if self.data.token(TT::Eq).is_some() {
						let value = self.data.immediate().unwrap();
						if values.contains_key(&name) {
							todo!("duplicate value: {name} = {value}");
						} else {
							trace!("found value: {name} = {value}");
							values.insert(name.clone(), value);
						}
					}
				}

				TT::MacroStart => {
					debug!("found macro start: '{:?}'", self.data.next());
				}
				TT::MacroEnd => {
					debug!("found macro end");
				}

				TT::Const => {
					self.data.token(TT::Dot).unwrap();
					if self.data.token(TT::Byte).is_some() {
						if let Some(_) = self.data.immediate() {
							address += 1;
						} else if let Some(name) = self.data.string() {
							address += name.len() as u32;
							while let Some(_) = self.data.token(TT::Comma).and_then(|_| self.data.immediate()) {
								address += 1;
							}
						} else {
							let token = &self.data.tokens[self.data.index];
							todo!("unexpected token: {token:?}");
						}
						address += 1;
					} else if self.data.token(TT::Word).is_some() {
						self.data.immediate().unwrap();
						address += 2;
					} else if self.data.token(TT::Long).is_some() {
						if let Some(_) = self.data.immediate() {
							address += 4;
						} else if let Some(_) = self.data.label() {
							address += 4;
						} else {
							let token = &self.data.tokens[self.data.index];
							todo!("unexpected token: {token:?}");
						}
					} else {
						let token = &self.data.tokens[self.data.index];
						todo!("unexpected token: {token:?}");
					}
				}

				_ => {
					trace!("{token:?}");
					address += 2;
				}
			}
		}

		Output {
			data: Parser::new(self.data.tokens),
			labels,
			values,
		}
	}
}

#[derive(Debug)]
struct Output<'a> {
	data: Parser<'a>,
	labels: HashMap<Box<str>, u32>,
	values: HashMap<Box<str>, i64>,
}

impl Output<'_> {
	#[instrument(skip_all)]
	fn output(mut self) -> Vec<Asm> {
		let mut out = vec![];

		while let Some(token) = self.data.next() {
			match token.tt {
				TT::Mov => {}

				TT::MovA => {
					let d = self.data.disp_pc().unwrap();
					self.data.token(TT::Comma).unwrap();
					self.data.r0().unwrap();
					let imm = d / 4;
					if !(u8::MIN as u16..=u8::MAX as u16).contains(&imm) {
						todo!("displacement value too large: found({d}), limit(0..={})", u8::MAX as u16 * 4);
					}
					out.push(Asm::imm8(AT::MovA, imm as u8));
				}

				TT::MovT => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::MovT, rn));
				}

				TT::Swap => {}

				TT::Xtrct => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Xtrct, rn, rm));
				}

				TT::Add => {}

				TT::AddC => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::AddC, rn, rm));
				}

				TT::AddV => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::AddV, rn, rm));
				}

				TT::CmpEq => {}

				TT::CmpHs => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpHs, rn, rm));
				}

				TT::CmpGe => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpGe, rn, rm));
				}

				TT::CmpHi => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpHi, rn, rm));
				}

				TT::CmpGt => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpGt, rn, rm));
				}

				TT::CmpPl => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpPl, rn, rm));
				}

				TT::CmpPz => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpPz, rn, rm));
				}

				TT::CmpStr => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::CmpStr, rn, rm));
				}

				TT::Div1 => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Div1, rn, rm));
				}

				TT::Div0S => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Div0S, rn, rm));
				}

				TT::Div0U => {
					out.push(Asm::none(AT::Div0U));
				}

				TT::DMulS => {
					self.data.token(TT::Dot).unwrap();
					self.data.token(TT::Long).unwrap();
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::DMulS, rn, rm));
				}

				TT::DMulU => {
					self.data.token(TT::Dot).unwrap();
					self.data.token(TT::Long).unwrap();
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::DMulU, rn, rm));
				}

				TT::Dt => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::Dt, rn));
				}

				TT::ExtS => {}

				TT::ExtU => {}

				TT::Mac => {}

				TT::Mul => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Mul, rn, rm));
				}

				TT::MulS => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::MulS, rn, rm));
				}

				TT::MulU => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::MulU, rn, rm));
				}

				TT::Neg => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Neg, rn, rm));
				}

				TT::NegC => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::NegC, rn, rm));
				}

				TT::Sub => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Sub, rn, rm));
				}

				TT::SubC => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::SubC, rn, rm));
				}

				TT::SubV => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::SubV, rn, rm));
				}

				TT::And => {}

				TT::Not => {
					let (rm,rn) = self.data.reg2().unwrap();
					out.push(Asm::reg2(AT::Not, rn, rm));
				}

				TT::Or => {}

				TT::Tas => {
					self.data.token(TT::At).unwrap();
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::Tas, rn));
				}

				TT::Tst => {}

				TT::Xor => {}

				TT::RotL => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::RotL, rn));
				}

				TT::RotR => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::RotR, rn));
				}

				TT::RotCL => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::RotCL, rn));
				}

				TT::RotCR => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::RotCR, rn));
				}

				TT::ShAL => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShAL, rn));
				}

				TT::ShAR => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShAR, rn));
				}

				TT::ShLL => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLL, rn));
				}

				TT::ShLR => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLR, rn));
				}

				TT::ShLL2 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLL2, rn));
				}

				TT::ShLR2 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLR2, rn));
				}

				TT::ShLL8 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLL8, rn));
				}

				TT::ShLR8 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLR8, rn));
				}

				TT::ShLL16 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLL16, rn));
				}

				TT::ShLR16 => {
					let rn = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::ShLR16, rn));
				}

				TT::Bf => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BfS => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bt => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BtS => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::Bra => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BraF => {
					let rm = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::BraF, rm));
				}

				TT::Bsr => {
					let label = self.data.label().unwrap();
					if !self.labels.contains_key(&label) {
						error!("unknown label: '{label}'");
						continue;
					}
					debug!("handle label outputs");
				}

				TT::BsrF => {
					let rm = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::BsrF, rm));
				}

				TT::Jmp => {
					self.data.token(TT::At).unwrap();
					let rm = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::Jmp, rm));
				}

				TT::Jsr => {
					self.data.token(TT::At).unwrap();
					let rm = self.data.reg().unwrap();
					out.push(Asm::reg1(AT::Jsr, rm));
				}

				TT::Rts => {
					out.push(Asm::none(AT::Rts));
				}

				TT::ClrT => {
					out.push(Asm::none(AT::ClrT));
				}

				TT::ClrMac => {
					out.push(Asm::none(AT::ClrMac));
				}

				TT::LdC => {}

				TT::LdS => {}

				TT::Nop => {
					out.push(Asm::none(AT::Nop));
				}

				TT::Rte => {
					out.push(Asm::none(AT::Rte));
				}

				TT::SetT => {
					out.push(Asm::none(AT::SetT));
				}

				TT::Sleep => {
					out.push(Asm::none(AT::Sleep));
				}

				TT::StC => {}

				TT::StS => {}

				TT::TrapA => {
					self.data.token(TT::Hash).unwrap();
					let imm = self.data.immediate().unwrap();
					let d = imm / 4;
					if !(u8::MIN as i64..=u8::MAX as i64).contains(&d) {
						todo!("trap offset too large: found({imm}), limit(0..={})", u8::MAX as i64 * 4);
					}
					out.push(Asm::imm8(AT::TrapA, d as u8));
				}

				_ => {
					trace!("unexpected Token: {token:?}");
				}
			}
		}

		out
	}
}

impl<'a> Parser<'a> {
	fn new(tokens: &'a [Token]) -> Self {
		Self {
			index: 0,
			tokens,
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
				i64::from_str_radix(n, 10).ok()
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
}

