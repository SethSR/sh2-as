use crate::lexer::{Token, TokenType};
use crate::Label;

mod arg;
mod instructions;
mod output;
mod state;

pub(crate) use arg::Arg;
pub(crate) use instructions::Ins;
pub(crate) use output::Output;
pub(crate) use state::State;

pub(crate) type Reg = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Size {
	Byte,
	Word,
	Long,
}

#[derive(Debug)]
pub(crate) enum Error {
	Expected(Token, String),
	ExpectedNum(Token, String, i64),
	NumTokExpected(Token),
	NumParse(std::num::ParseIntError),
	LabelDefined(Label),
	UnknownToken(Token),
	UnexpectedToken(Token),
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(fmt, "ERROR: ")?;
		match self {
			Self::Expected(tok, msg) => {
				let (line, pos) = tok.pos();
				write!(fmt, "Expected '{msg}', Found '{tok}' @ [{line}:{pos}]")
			}
			Self::ExpectedNum(tok, msg, num) => {
				let (line, pos) = tok.pos();
				write!(
					fmt,
					"Expected '{msg}', Found '{tok}' ({num}) @ [{line}:{pos}]"
				)
			}
			Self::NumTokExpected(tok) => {
				let (line, pos) = tok.pos();
				write!(fmt, "Number token is missing the referent @ [{line},{pos}]")
			}
			Self::NumParse(e) => {
				write!(fmt, "{e}")
			}
			Self::LabelDefined(lbl) => {
				write!(fmt, "Label '{lbl}' already defined")
			}
			Self::UnknownToken(tok) => {
				let (line, pos) = tok.pos();
				write!(fmt, "unknown item '{tok}' @ [{line}:{pos}]")
			}
			Self::UnexpectedToken(tok) => {
				let (line, pos) = tok.pos();
				write!(fmt, "unexpected {tok} @ [{line}:{pos}]")
			}
		}
	}
}

impl From<std::num::ParseIntError> for Error {
	fn from(other: std::num::ParseIntError) -> Self {
		Self::NumParse(other)
	}
}

struct Parser<'tok> {
	index: usize,
	tokens: &'tok [Token],
	errors: Vec<Error>,
}

macro_rules! error {
	($t:ty, $data:expr, $msg:expr) => {{
		$data.next_line();
		Err::<$t, Error>($data.expected($msg))
	}};
}

impl Parser<'_> {
	fn is_done(&self) -> bool {
		self.index >= self.tokens.len()
	}

	fn try_peek(&self, i: usize) -> Option<&Token> {
		self.tokens.get(self.index + i)
	}
	fn peek(&self, i: usize) -> &Token {
		self.try_peek(i).unwrap()
	}
	fn curr(&self) -> &Token {
		self.peek(0)
	}

	fn expected(&mut self, msg: &str) -> Error {
		let tok = self.curr().clone();
		match tok.get_type() {
			TokenType::IdNumber => match self.signed(Size::Long) {
				Err(_) => Error::NumTokExpected(tok),
				Ok(num) => Error::ExpectedNum(tok, msg.into(), num),
			},
			_ => Error::Expected(tok, msg.into()),
		}
	}

	fn next(&mut self) -> &Token {
		self.index += 1;
		self.curr()
	}

	fn next_line(&mut self) {
		loop {
			let Some(tok) = self.try_peek(1) else { break };
			let tok = tok.clone();
			self.next();
			if tok.get_type() == TokenType::SymNewline {
				break;
			}
		}
	}

	fn to_i64(txt: &str, radix: u32, sz: Size, is_signed: bool) -> Result<i64, Error> {
		let txt = txt.replace('_', "");
		let out = if is_signed {
			match sz {
				Size::Byte => i8::from_str_radix(&txt, radix).map(|n| n as i64)?,
				Size::Word => i16::from_str_radix(&txt, radix).map(|n| n as i64)?,
				Size::Long => i32::from_str_radix(&txt, radix).map(|n| n as i64)?,
			}
		} else {
			match sz {
				Size::Byte => u8::from_str_radix(&txt, radix).map(|n| n as i64)?,
				Size::Word => u16::from_str_radix(&txt, radix).map(|n| n as i64)?,
				Size::Long => u32::from_str_radix(&txt, radix).map(|n| n as i64)?,
			}
		};
		Ok(out)
	}

	fn i64_from_dec(txt: &str, sz: Size, is_signed: bool) -> Result<i64, Error> {
		Self::to_i64(txt, 10, sz, is_signed)
	}
	fn i64_from_bin(txt: &str, sz: Size) -> Result<i64, Error> {
		Self::to_i64(&txt[1..], 2, sz, false)
	}
	fn i64_from_hex(txt: &str, sz: Size) -> Result<i64, Error> {
		Self::to_i64(&txt[1..], 16, sz, false)
	}

	fn unsigned(&mut self, sz: Size) -> Result<i64, Error> {
		let txt = self.curr().to_string();
		match txt.chars().next() {
			Some('%') => Self::i64_from_bin(&txt, sz),
			Some('$') => Self::i64_from_hex(&txt, sz),
			Some(c) if c.is_numeric() => Self::i64_from_dec(&txt, sz, false),
			_ => error!(i64, self, "parsing non-numeric"),
		}
	}

	fn signed(&mut self, sz: Size) -> Result<i64, Error> {
		let txt = self.curr().to_string();
		match txt.chars().next() {
			Some('%') => Self::i64_from_bin(&txt, sz),
			Some('$') => Self::i64_from_hex(&txt, sz),
			Some('-') => Self::i64_from_dec(&txt, sz, true),
			Some(c) if c.is_numeric() => Self::i64_from_dec(&txt, sz, false),
			_ => error!(i64, self, "parsing non-numeric"),
		}
	}

	fn reg(&self) -> Result<Reg, Error> {
		let txt = self.curr().to_string();
		let out = if txt.to_lowercase() == "pc" {
			15
		} else {
			txt[1..].parse::<u8>()?
		};
		Ok(out)
	}

	fn address(&mut self) -> Result<Arg, Error> {
		use TokenType as TT;

		match self.next().get_type() {
			TT::SymDash => self.match_reg().map(Arg::PreDec),
			TT::IdRegister => {
				let reg = self.reg()?;
				if self.match_token(TT::SymPlus).is_ok() {
					Ok(Arg::PostInc(reg))
				} else {
					Ok(Arg::IndReg(reg))
				}
			}
			TT::SymOParen => match self.next().get_type() {
				TT::IdLabel => {
					let Some(lbl) = self.curr().get_id() else {
						return error!(Arg, self, "Unknown Value");
					};
					self.match_token(TT::SymComma)?;
					let reg = self.match_reg()?;
					self.match_token(TT::SymCParen)?;
					Ok(Arg::DispLabel(lbl, reg))
				}
				TT::IdNumber => {
					let num = self.signed(Size::Byte)?;
					self.match_token(TT::SymComma)?;
					match self.next().get_type() {
						TT::SymGBR => self
							.assert_within_i8(num)
							.and_then(|imm| self.match_token(TT::SymCParen).map(|_| imm))
							.map(Arg::DispGBR),
						TT::SymPC => self
							.assert_within_i8(num)
							.and_then(|imm| self.match_token(TT::SymCParen).map(|_| imm))
							.map(Arg::DispPC),
						TT::IdRegister => self
							.assert_within_i4(num)
							.and_then(|imm| self.reg().map(|r| (imm, r)))
							.and_then(|pair| self.match_token(TT::SymCParen).map(|_| pair))
							.map(|(disp, reg)| Arg::DispReg(disp, reg)),
						_ => error!(Arg, self, "GBR, PC, or Register"),
					}
				}
				TT::IdRegister => self
					.reg_args()
					.and_then(|(r0, reg)| self.assert_r0(r0).map(|_| reg))
					.and_then(|reg| self.match_token(TT::SymCParen).map(|_| reg))
					.map(Arg::DispR0),
				_ => error!(Arg, self, "Number or R0"),
			},
			_ => error!(Arg, self, "Address specifier (@r_/ @-r_ / @r_+)"),
		}
	}

	fn match_size(&mut self) -> Result<Size, Error> {
		match self.next().get_type() {
			TokenType::SymByte => Ok(Size::Byte),
			TokenType::SymWord => Ok(Size::Word),
			TokenType::SymLong => Ok(Size::Long),
			_ => error!(Size, self, "size specifier"),
		}
	}

	fn size(&mut self) -> Result<Size, Error> {
		self.match_token(TokenType::SymDot)?;
		self.match_size()
	}

	fn assert_token_with_offset_or_err(
		&mut self,
		offset: usize,
		tt: TokenType,
		msg: &str,
	) -> Result<(), Error> {
		if self.peek(offset).get_type() != tt {
			error!((), self, msg)
		} else {
			Ok(())
		}
	}

	fn match_token_or_err<'a>(&'a mut self, tt: TokenType, msg: &str) -> Result<&'a Token, Error> {
		self.assert_token_with_offset_or_err(1, tt, msg)?;
		Ok(self.next())
	}

	fn match_ident_or_err(&mut self, msg: &str) -> Result<Label, Error> {
		self.assert_token_with_offset_or_err(1, TokenType::IdLabel, msg)?;
		Ok(self.next().get_id().unwrap())
	}

	fn match_token(&mut self, tt: TokenType) -> Result<&Token, Error> {
		self.match_token_or_err(tt, &tt.to_string())
	}

	fn match_tokens(&mut self, tts: &[TokenType]) -> Result<(), Error> {
		for tt in tts {
			self.match_token(*tt)?;
		}
		Ok(())
	}

	fn match_unsigned(&mut self, sz: Size) -> Result<i64, Error> {
		self.match_token(TokenType::IdNumber)?;
		self.unsigned(sz)
	}

	fn match_signed(&mut self, sz: Size) -> Result<i64, Error> {
		self.match_token(TokenType::IdNumber)?;
		self.signed(sz)
	}

	fn match_reg(&mut self) -> Result<Reg, Error> {
		self.match_token(TokenType::IdRegister)?;
		self.reg()
	}

	fn assert_r0(&mut self, reg: u8) -> Result<(), Error> {
		if reg == 0 {
			Ok(())
		} else {
			error!((), self, "R0")
		}
	}

	fn match_r0(&mut self) -> Result<(), Error> {
		let reg = self.match_reg()?;
		self.assert_r0(reg)?;
		Ok(())
	}

	fn reg_args(&mut self) -> Result<(Reg, Reg), Error> {
		let src = self.reg()?;
		self.match_token(TokenType::SymComma)?;
		let dst = self.match_reg()?;
		Ok((src, dst))
	}

	fn match_reg_args(&mut self) -> Result<(Reg, Reg), Error> {
		self.assert_token_with_offset_or_err(
			1,
			TokenType::IdRegister,
			&TokenType::IdRegister.to_string(),
		)?;
		self.next();
		self.reg_args()
	}

	fn assert_within_i4(&mut self, value: i64) -> Result<i8, Error> {
		if (-8..7).contains(&value) {
			Ok(((value as u8) & 0x0F) as i8)
		} else {
			error!(i8, self, "immediate value between -8 & 7")
		}
	}

	fn assert_within_i8(&mut self, value: i64) -> Result<i8, Error> {
		if (i8::MIN as i64..i8::MAX as i64).contains(&value) {
			Ok(value as i8)
		} else {
			error!(
				i8,
				self,
				&format!("immediate value between {} & {}", i8::MIN, i8::MAX)
			)
		}
	}

	fn assert_within_i16(&mut self, value: i64) -> Result<i16, Error> {
		if (i16::MIN as i64..i16::MAX as i64).contains(&value) {
			Ok(value as i16)
		} else {
			error!(
				i16,
				self,
				&format!("immediate value between {} & {}", i16::MIN, i16::MAX)
			)
		}
	}

	fn assert_within_i32(&mut self, value: i64) -> Result<i32, Error> {
		if (i32::MIN as i64..i32::MAX as i64).contains(&value) {
			Ok(value as i32)
		} else {
			error!(
				i32,
				self,
				&format!("immediate value between {} & {}", i32::MIN, i32::MAX)
			)
		}
	}

	fn assert_within_u8(&mut self, value: i64) -> Result<u8, Error> {
		if (u8::MIN as i64..u8::MAX as i64).contains(&value) {
			Ok(value as u8)
		} else {
			error!(u8, self, "immediate value between 0 & 255")
		}
	}

	fn simple_branch_ins(
		&mut self,
		fn_fast: impl Fn(Label) -> Ins,
		fn_delay: impl Fn(Label) -> Ins,
	) -> Result<Ins, Error> {
		use TokenType as TT;
		match self.next().get_type() {
			TT::IdLabel => self.curr().get_id().map(fn_fast).ok_or_else(|| {
				self.next_line();
				self.expected("Label")
			}),
			TT::SymSlash => self
				.match_token(TT::SymDelay)
				.map(|_| ())
				.and_then(|_| self.match_ident_or_err("Label"))
				.map(fn_delay),
			_ => error!(Ins, self, "Label"),
		}
	}

	fn logic_ins(
		&mut self,
		fn_imm: impl Fn(u8) -> Ins,
		fn_reg: impl Fn(Reg, Reg) -> Ins,
		fn_byte: impl Fn(u8) -> Ins,
		ins_name: &str,
	) -> Result<Ins, Error> {
		use TokenType as TT;
		match self.next().get_type() {
			// INS r#,r#
			TT::IdRegister => self.reg_args().map(|(src, dst)| fn_reg(src, dst)),
			// INS.B #imm,@(r#,GBR)
			TT::SymDot => self
				.match_tokens(&[TT::SymByte, TT::SymImmediate])
				.and_then(|_| self.match_signed(Size::Byte))
				.and_then(|num| self.assert_within_u8(num))
				.and_then(|imm| {
					self
						.match_tokens(&[TT::SymComma, TT::SymAddress, TT::SymOParen])
						.map(|_| imm)
				})
				.and_then(|imm| self.match_r0().map(|_| imm))
				.and_then(|imm| {
					self
						.match_tokens(&[TT::SymComma, TT::SymGBR, TT::SymCParen])
						.map(|_| imm)
				})
				.map(fn_byte),
			// INS #imm,r0
			TT::SymImmediate => self
				.match_unsigned(Size::Byte)
				.and_then(|num| self.assert_within_u8(num))
				.and_then(|imm| self.match_token(TT::SymComma).map(|_| imm))
				.and_then(|imm| self.match_r0().map(|_| imm))
				.map(fn_imm),
			_ => error!(
				Ins,
				self,
				&format!("Register, size-specifier, or #<immediate> for {ins_name}")
			),
		}
	}

	fn dmul_ins(&mut self, fn_ins: impl Fn(Reg, Reg) -> Ins) -> Result<Ins, Error> {
		self.size().and_then(|sz| {
			if sz == Size::Long {
				self.match_reg_args().map(|(src, dst)| fn_ins(src, dst))
			} else {
				error!(Ins, self, "Size specifier Long('l')")
			}
		})
	}

	fn ext_ins(&mut self, fn_ins: impl Fn(Size, Reg, Reg) -> Ins) -> Result<Ins, Error> {
		self.size().and_then(|sz| {
			if sz == Size::Long {
				error!(Ins, self, "Size specifier Byte('b') or Word('w')")
			} else {
				self.match_reg_args().map(|(src, dst)| fn_ins(sz, src, dst))
			}
		})
	}
}

// TODO - srenshaw - Ensure the parser actually returns what it's supposed to.

/// Parses strings of tokens into valid instructions & directives
///
/// Given a sequence of valid tokens, the parser should return either a section and label table for
/// the analysis stage, or a sequence of all errors encountered while parsing the input.
pub fn parser(tokens: &[Token]) -> Result<Output, Vec<Error>> {
	let mut skey = 0;
	let mut output = Output::default();

	let mut data = Parser {
		tokens,
		index: 0,
		errors: Vec::new(),
	};

	while !data.is_done() {
		use TokenType as TT;
		let cur_tok = data.curr();
		eprintln!("{cur_tok:?}");
		match cur_tok.get_type() {
			TT::IdComment => {} // skip comments
			TT::IdLabel => {
				let lbl = cur_tok.get_id().expect("identifier without referent");
				match data.next().get_type() {
					TT::SymEqual => data
						.match_signed(Size::Byte)
						.and_then(|num| data.assert_within_i8(num))
						.map(|imm| output.values.insert(lbl.clone(), imm))
						.map(|_| ())
						.unwrap_or_default(),
					TT::SymColon => {
						if output.labels.contains_key(&lbl) {
							data.errors.push(Error::LabelDefined(lbl));
							data.next_line();
							continue;
						}
						output.labels.insert(lbl.clone(), None);
						output.add_to_section(skey, Ins::Label(lbl))
					}
					_ => {
						let msg = data.expected(": or \"= <number>\" to declare a new label or value");
						data.errors.push(msg);
						data.next_line();
						continue;
					}
				}
			}
			TT::IdUnknown => data.errors.push(Error::UnknownToken(cur_tok.clone())),
			TT::InsAdd => match data.next().get_type() {
				TT::SymImmediate => data
					.match_signed(Size::Byte)
					.and_then(|num| data.assert_within_i8(num))
					.and_then(|imm| data.match_token(TT::SymComma).map(|_| imm))
					.and_then(|imm| data.match_reg().map(|r| (imm, r)))
					.map(|(imm, reg)| Ins::AddImm(imm, reg)),
				TT::IdRegister => data.reg_args().map(|(src, dst)| Ins::AddReg(src, dst)),
				_ => error!(Ins, data, "Valid ADD source argument: Number or Register"),
			}
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsAddC => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::AddC(src, dst)))
				.unwrap_or_default(),
			TT::InsAddV => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::AddV(src, dst)))
				.unwrap_or_default(),
			TT::InsAnd => match data.logic_ins(Ins::AndImm, Ins::AndReg, Ins::AndByte, "AND") {
				Err(e) => data.errors.push(e),
				Ok(ins) => output.add_to_section(skey, ins),
			},
			TT::InsBf => data
				.simple_branch_ins(Ins::Bf, Ins::BfS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsBra => data
				.match_ident_or_err("Label")
				.map(|lbl| output.add_to_section(skey, Ins::Bra(lbl)))
				.unwrap_or_default(),
			TT::InsBraF => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BraF(reg)))
				.unwrap_or_default(),
			TT::InsBsr => data
				.match_ident_or_err("Label")
				.map(|lbl| output.add_to_section(skey, Ins::Bsr(lbl)))
				.unwrap_or_default(),
			TT::InsBsrF => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BsrF(reg)))
				.unwrap_or_default(),
			TT::InsBt => data
				.simple_branch_ins(Ins::Bt, Ins::BtS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsClrMac => output.add_to_section(skey, Ins::ClrMac),
			TT::InsClrT => output.add_to_section(skey, Ins::ClrT),
			TT::InsCmp => {
				let mut func = || -> Result<Ins, Error> {
					data.match_token(TT::SymSlash)?;
					match data.next().get_type() {
						TT::SymEQ => {
							if data.next().get_type() == TT::SymImmediate {
								let num = data.match_signed(Size::Byte)?;
								let imm = data.assert_within_i8(num)?;
								data.match_token(TT::SymComma)?;
								data.match_r0()?;
								Ok(Ins::CmpEqImm(imm))
							} else {
								data
									.match_reg_args()
									.map(|(src, dst)| Ins::CmpEqReg(src, dst))
							}
						}
						TT::SymGE => data.match_reg_args().map(|(src, dst)| Ins::CmpGE(src, dst)),
						TT::SymGT => data.match_reg_args().map(|(src, dst)| Ins::CmpGT(src, dst)),
						TT::SymHI => data.match_reg_args().map(|(src, dst)| Ins::CmpHI(src, dst)),
						TT::SymHS => data.match_reg_args().map(|(src, dst)| Ins::CmpHS(src, dst)),
						TT::SymPL => data.match_reg().map(Ins::CmpPL),
						TT::SymPZ => data.match_reg().map(Ins::CmpPZ),
						TT::SymStr => data
							.match_reg_args()
							.map(|(src, dst)| Ins::CmpStr(src, dst)),
						_ => error!(Ins, data, "Comparator type (EQ,GT,GE,HI,HS,PL,PZ,STR)"),
					}
				};
				match func() {
					Ok(ins) => output.add_to_section(skey, ins),
					Err(e) => data.errors.push(e),
				}
			}
			TT::InsDiv0S => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Div0S(src, dst)))
				.unwrap_or_default(),
			TT::InsDiv0U => output.add_to_section(skey, Ins::Div0U),
			TT::InsDiv1 => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Div1(src, dst)))
				.unwrap_or_default(),
			TT::InsDMulS => data
				.dmul_ins(Ins::DMulS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsDMulU => data
				.dmul_ins(Ins::DMulU)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsDT => data
				.match_reg()
				.map(Ins::Dt)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsExtS => data
				.ext_ins(Ins::ExtS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsExtU => data
				.ext_ins(Ins::ExtU)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsJmp => {
				// TODO - srenshaw - Add label handling for JMP
				data
					.match_token(TT::SymAddress)
					.map(|_| ())
					.and_then(|_| data.match_reg())
					.map(Ins::Jmp)
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			TT::InsJsr => {
				// TODO - srenshaw - Add label handling for JSR
				data
					.match_token(TT::SymAddress)
					.map(|_| ())
					.and_then(|_| data.match_reg())
					.map(Ins::Jsr)
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			TT::InsLdc => match data.next().get_type() {
				// LDC r#,GBR
				// LDC r#,SR
				// LDC r#,VBR
				TT::IdRegister => data
					.reg()
					.and_then(|reg| data.match_token(TT::SymComma).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymGBR => Ok(Ins::LdcGBR(reg)),
						TT::SymSR => Ok(Ins::LdcSR(reg)),
						TT::SymVBR => Ok(Ins::LdcVBR(reg)),
						_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
					}),
				// LDC.L @r#+,GBR
				// LDC.L @r#+,SR
				// LDC.L @r#+,VBR
				TT::SymDot => data
					.match_tokens(&[TT::SymLong, TT::SymAddress])
					.and_then(|_| data.match_reg())
					.and_then(|reg| data.match_tokens(&[TT::SymPlus, TT::SymComma]).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymGBR => Ok(Ins::LdcGBR_Inc(reg)),
						TT::SymSR => Ok(Ins::LdcSR_Inc(reg)),
						TT::SymVBR => Ok(Ins::LdcVBR_Inc(reg)),
						_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
					}),
				_ => error!(Ins, data, "Valid LDC instruction"),
			}
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsLds => match data.next().get_type() {
				// LDS r#,MACH
				// LDS r#,MACL
				// LDS r#,PR
				TT::IdRegister => data
					.reg()
					.and_then(|reg| data.match_token(TT::SymComma).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymMACH => Ok(Ins::LdsMACH(reg)),
						TT::SymMACL => Ok(Ins::LdsMACL(reg)),
						TT::SymPR => Ok(Ins::LdsPR(reg)),
						_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
					}),
				// LDS.L @r#+,MACH
				// LDS.L @r#+,MACL
				// LDS.L @r#+,PR
				TT::SymDot => data
					.match_tokens(&[TT::SymLong, TT::SymAddress])
					.and_then(|_| data.match_reg())
					.and_then(|reg| data.match_tokens(&[TT::SymPlus, TT::SymComma]).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymMACH => Ok(Ins::LdsMACH_Inc(reg)),
						TT::SymMACL => Ok(Ins::LdsMACL_Inc(reg)),
						TT::SymPR => Ok(Ins::LdsPR_Inc(reg)),
						_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
					}),
				_ => error!(Ins, data, "Valid LDS instruction"),
			}
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsMac => {
				let ins_func = match data.size() {
					Ok(Size::Byte) => {
						let msg = data.expected("Size specifier Word('w') or Long('l')");
						data.errors.push(msg);
						data.next_line();
						continue;
					}
					Ok(Size::Word) => Ins::MacWord,
					Ok(Size::Long) => Ins::MacLong,
					Err(_) => continue,
				};
				data
					.match_token(TT::SymAddress)
					.map(|_| ())
					.and_then(|_| data.match_reg())
					.and_then(|reg| {
						data
							.match_tokens(&[TT::SymPlus, TT::SymComma, TT::SymAddress])
							.map(|_| reg)
					})
					.and_then(|reg| data.match_reg().map(|r| (reg, r)))
					.and_then(|pair| data.match_token(TT::SymPlus).map(|_| pair))
					.map(|(src, dst)| ins_func(src, dst))
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			// MOV #imm,r#
			// MOV r#,r#
			//
			// MOV.B @(disp,GBR),r0
			// MOV.W @(disp,GBR),r0
			// MOV.L @(disp,GBR),r0
			// MOV_FromGBR(Size,i8)
			//
			// MOV.B @(disp,r#),r0
			// MOV.W @(disp,r#),r0
			// MOV.L @(disp,r#),r#
			// MOV_FromDisp(Size,i4,Reg,Reg) // assert!(Dst == r0 || Size == Long)
			//
			// MOV.B @(r0,r#),r#
			// MOV.W @(r0,r#),r#
			// MOV.L @(r0,r#),r#
			// MOV_FromR0(Size,Reg,Reg)
			//
			// MOV.W @(disp,PC),r#
			// MOV.L @(disp,PC),r#
			// MOV_FromPC(Size,i8,Reg) // !Byte
			// MOV_FromLabel(Size,Label,Reg) // !Byte
			//
			// MOV.B @r#+,r#
			// MOV.W @r#+,r#
			// MOV.L @r#+,r#
			// MOV_FromInc(Size,Reg,Reg)
			//
			// MOV.B @r#,r#
			// MOV.W @r#,r#
			// MOV.L @r#,r#
			// MOV_FromReg(Size,Reg,Reg)
			//
			// MOV.B r0,@(disp,GBR)
			// MOV.W r0,@(disp,GBR)
			// MOV.L r0,@(disp,GBR)
			// MOV_ToGBR(Size,i8)
			//
			// MOV.B r0,@(disp,r#)
			// MOV.W r0,@(disp,r#)
			// MOV.L r#,@(disp,r#)
			// MOV_ToDisp(Size,i4,Reg,Reg) // assert!(Src == r0 || Size == Long)
			//
			// MOV.B r#,@(r0,r#)
			// MOV.W r#,@(r0,r#)
			// MOV.L r#,@(r0,r#)
			// MOV_ToR0(Size,Reg,Reg)
			//
			// MOV.B r#,@-r#
			// MOV.W r#,@-r#
			// MOV.L r#,@-r#
			// MOV_ToDec(Size,Reg,Reg)
			//
			// MOV.B r#,@r#
			// MOV.W r#,@r#
			// MOV.L r#,@r#
			// MOV_ToReg(Size,Reg,Reg)
			TT::InsMov => match data.next().get_type() {
				// MOV Rm,Rn
				TT::IdRegister => data.reg_args().map(|(src, dst)| Ins::MovReg(src, dst)),
				TT::SymDot => || -> Result<Ins, Error> {
					let size = data.match_size()?;

					let src = match data.next().get_type() {
						TT::IdLabel => data.curr().get_id().map(Arg::Label).ok_or_else(|| {
							data.next_line();
							data.expected("Label")
						}),
						TT::IdRegister => data.reg().map(Arg::DirReg),
						TT::SymAddress => data.address(),
						_ => error!(Arg, data, "Register, Displacement, or Address"),
					}?;

					data.match_token(TT::SymComma)?;

					let dst = match data.next().get_type() {
						TT::IdRegister => data.reg().map(Arg::DirReg),
						TT::SymAddress => data.address(),
						_ => error!(Arg, data, "Register, Displacement, or Address"),
					}?;

					Ok(Ins::Mov(size, src, dst))
				}(),
				// MOV #imm,Rn
				TT::SymImmediate => data
					.match_signed(Size::Byte)
					.and_then(|num| data.assert_within_i8(num))
					.and_then(|imm| data.match_token(TT::SymComma).map(|_| imm))
					.and_then(|imm| data.match_reg().map(|r| (imm, r)))
					.map(|(imm, reg)| Ins::MovImm(imm, reg)),
				_ => error!(Ins, data, "size specifier, 8-bit immediate, or Register"),
			}
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsMovA => data
				.match_tokens(&[TT::SymAddress, TT::SymOParen])
				.and_then(|_| data.match_signed(Size::Byte))
				.and_then(|num| data.assert_within_i8(num))
				.and_then(|imm| {
					data
						.match_tokens(&[TT::SymComma, TT::SymPC, TT::SymCParen, TT::SymComma])
						.map(|_| imm)
				})
				.and_then(|imm| data.match_r0().map(|_| imm))
				.map(|num| output.add_to_section(skey, Ins::MovA(num)))
				.unwrap_or_default(),
			TT::InsMovT => data
				.match_reg()
				.map(|dst| output.add_to_section(skey, Ins::MovT(dst)))
				.unwrap_or_default(),
			TT::InsMul => data
				.match_tokens(&[TT::SymDot, TT::SymLong])
				.and_then(|_| data.match_reg_args())
				.map(|(src, dst)| output.add_to_section(skey, Ins::Mul(src, dst)))
				.unwrap_or_default(),
			TT::InsMulS => data
				.match_tokens(&[TT::SymDot, TT::SymWord])
				.and_then(|_| data.match_reg_args())
				.map(|(src, dst)| output.add_to_section(skey, Ins::MulS(src, dst)))
				.unwrap_or_default(),
			TT::InsMulU => data
				.match_tokens(&[TT::SymDot, TT::SymWord])
				.and_then(|_| data.match_reg_args())
				.map(|(src, dst)| output.add_to_section(skey, Ins::MulU(src, dst)))
				.unwrap_or_default(),
			TT::InsNeg => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Neg(src, dst)))
				.unwrap_or_default(),
			TT::InsNegC => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::NegC(src, dst)))
				.unwrap_or_default(),
			TT::InsNop => output.add_to_section(skey, Ins::Nop),
			TT::InsNot => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Not(src, dst)))
				.unwrap_or_default(),
			TT::InsOr => data
				.logic_ins(Ins::OrImm, Ins::OrReg, Ins::OrByte, "OR")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsRotCL => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::RotCL(reg)))
				.unwrap_or_default(),
			TT::InsRotCR => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::RotCR(reg)))
				.unwrap_or_default(),
			TT::InsRotL => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::RotL(reg)))
				.unwrap_or_default(),
			TT::InsRotR => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::RotR(reg)))
				.unwrap_or_default(),
			TT::InsRte => output.add_to_section(skey, Ins::Rte),
			TT::InsRts => output.add_to_section(skey, Ins::Rts),
			TT::InsSetT => output.add_to_section(skey, Ins::SetT),
			TT::InsShAL => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShAL(reg)))
				.unwrap_or_default(),
			TT::InsShAR => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShAR(reg)))
				.unwrap_or_default(),
			TT::InsShLL => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLL(reg)))
				.unwrap_or_default(),
			TT::InsShLL2 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLL2(reg)))
				.unwrap_or_default(),
			TT::InsShLL8 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLL8(reg)))
				.unwrap_or_default(),
			TT::InsShLL16 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLL16(reg)))
				.unwrap_or_default(),
			TT::InsShLR => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLR(reg)))
				.unwrap_or_default(),
			TT::InsShLR2 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLR2(reg)))
				.unwrap_or_default(),
			TT::InsShLR8 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLR8(reg)))
				.unwrap_or_default(),
			TT::InsShLR16 => data
				.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ShLR16(reg)))
				.unwrap_or_default(),
			TT::InsSleep => output.add_to_section(skey, Ins::Sleep),
			TT::InsStc => || -> Result<Ins, Error> {
				fn comma_reg(p: &mut Parser) -> Result<Reg, Error> {
					p.match_token(TT::SymComma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Result<Reg, Error> {
					p.match_tokens(&[TT::SymComma, TT::SymAddress, TT::SymDash])?;
					p.match_reg()
				}
				match data.next().get_type() {
					TT::SymGBR => comma_reg(&mut data).map(Ins::StcGBR),
					TT::SymSR => comma_reg(&mut data).map(Ins::StcSR),
					TT::SymVBR => comma_reg(&mut data).map(Ins::StcVBR),
					TT::SymDot => {
						data.match_token(TT::SymLong)?;
						match data.next().get_type() {
							TT::SymGBR => pre_dec(&mut data).map(Ins::StcGBR_Dec),
							TT::SymSR => pre_dec(&mut data).map(Ins::StcSR_Dec),
							TT::SymVBR => pre_dec(&mut data).map(Ins::StcVBR_Dec),
							_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
						}
					}
					_ => error!(Ins, data, "Valid STC instruction"),
				}
			}()
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsSts => || -> Result<Ins, Error> {
				fn comma_reg(p: &mut Parser) -> Result<Reg, Error> {
					p.match_token(TT::SymComma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Result<Reg, Error> {
					p.match_tokens(&[TT::SymComma, TT::SymAddress, TT::SymDash])?;
					p.match_reg()
				}
				match data.next().get_type() {
					TT::SymMACH => comma_reg(&mut data).map(Ins::StsMACH),
					TT::SymMACL => comma_reg(&mut data).map(Ins::StsMACL),
					TT::SymPR => comma_reg(&mut data).map(Ins::StsPR),
					TT::SymDot => {
						data.match_token(TT::SymLong)?;
						match data.next().get_type() {
							TT::SymMACH => pre_dec(&mut data).map(Ins::StsMACH_Dec),
							TT::SymMACL => pre_dec(&mut data).map(Ins::StsMACL_Dec),
							TT::SymPR => pre_dec(&mut data).map(Ins::StsPR_Dec),
							_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
						}
					}
					_ => error!(Ins, data, "Valid STS instruction"),
				}
			}()
			.map(|ins| output.add_to_section(skey, ins))
			.unwrap_or_default(),
			TT::InsSub => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Sub(src, dst)))
				.unwrap_or_default(),
			TT::InsSubC => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::SubC(src, dst)))
				.unwrap_or_default(),
			TT::InsSubV => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::SubV(src, dst)))
				.unwrap_or_default(),
			TT::InsSwap => data
				.match_token(TT::SymDot)
				.map(|_| ())
				.and_then(|_| data.match_size())
				.and_then(|sz| data.match_reg_args().map(|pair| (sz, pair)))
				.map(|(sz, (src, dst))| Ins::Swap(sz, src, dst))
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTas => data
				.match_tokens(&[TT::SymDot, TT::SymByte, TT::SymAddress])
				.and_then(|_| data.match_reg())
				.map(Ins::Tas)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTrapA => data
				.match_token(TT::SymImmediate)
				.map(|_| ())
				.and_then(|_| data.match_signed(Size::Byte))
				.and_then(|num| data.assert_within_u8(num))
				.map(Ins::TrapA)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTst => data
				.logic_ins(Ins::Tst_Imm, Ins::Tst_Reg, Ins::Tst_Byte, "TST")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsXor => data
				.logic_ins(Ins::Xor_Imm, Ins::Xor_Reg, Ins::Xor_Byte, "XOR")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsXtrct => data
				.match_reg_args()
				.map(|(src, dst)| output.add_to_section(skey, Ins::Xtrct(src, dst)))
				.unwrap_or_default(),
			TT::SymConst => data
				.size()
				.and_then(|sz| match data.next().get_type() {
					TT::IdNumber => data
						.signed(sz)
						.and_then(|num| match sz {
							Size::Byte => data.assert_within_i8(num).map(|n| n as i64),
							Size::Word => data.assert_within_i16(num).map(|n| n as i64),
							Size::Long => data.assert_within_i32(num).map(|n| n as i64),
						})
						.map(|imm| Ins::Const_Imm(sz, imm)),
					TT::IdLabel => data
						.curr()
						.get_id()
						.map(|label| Ins::Const_Label(sz, label))
						.ok_or_else(|| {
							data.next_line();
							data.expected("identifier with label")
						}),
					_ => error!(Ins, data, "integer literal or label"),
				})
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::SymNewline => {} // skip newlines
			TT::SymOrg => data
				.match_unsigned(Size::Long)
				.map(|addr| skey = addr as u64)
				.unwrap_or_default(),
			_ => data.errors.push(Error::UnexpectedToken(cur_tok.clone())),
		}
		data.index += 1;
	}

	if data.errors.is_empty() {
		Ok(output)
	} else {
		Err(data.errors)
	}
}

#[cfg(test)]
mod can_parse {
	use std::collections::HashMap;

	use crate::lexer::lexer;

	use super::*;

	type TestResult = Result<(), Vec<Error>>;

	type SectionMap<T> = HashMap<u64, Vec<T>>;
	type LabelMap = HashMap<Label, Option<u32>>;
	type ValueMap = HashMap<Label, i8>;

	type SectionPair<T> = <SectionMap<T> as IntoIterator>::Item;

	fn check_inst(state: State, ins: Ins) {
		assert_eq!(state, State::Incomplete(ins));
	}
	fn check_inst_p((state, ins): (State, Ins)) {
		check_inst(state, ins);
	}

	fn check_section(p_addr: u64, p_section: Vec<State>, e_addr: u64, e_section: Vec<Ins>) {
		assert_eq!(p_addr, e_addr, "section address mismatch");
		let ps_len = p_section.len();
		let es_len = e_section.len();
		assert_eq!(ps_len, es_len, "expected {} output instruction(s)", es_len);
		p_section.into_iter().zip(e_section).for_each(check_inst_p);
	}
	fn check_section_p((p_section, e_section): (SectionPair<State>, SectionPair<Ins>)) {
		check_section(p_section.0, p_section.1, e_section.0, e_section.1);
	}

	fn check_sections(p_sections: SectionMap<State>, e_sections: SectionMap<Ins>) {
		let ps_len = p_sections.len();
		let es_len = e_sections.len();
		assert_eq!(ps_len, es_len, "expected {} output section(s)", es_len);
		p_sections
			.into_iter()
			.zip(e_sections)
			.for_each(check_section_p);
	}

	fn single_section(ins_seq: &[Ins]) -> SectionMap<Ins> {
		[(0, ins_seq.to_vec())].into()
	}

	fn check_labels(p_labels: LabelMap, e_labels: &[Label]) {
		let pl_len = p_labels.len();
		let el_len = e_labels.len();
		assert_eq!(pl_len, el_len, "expected {} output label(s)", el_len);
		assert!(
			e_labels.into_iter().all(|lbl| p_labels.contains_key(lbl)),
			"unmatched labels"
		);
	}

	fn check_values(p_values: ValueMap, e_values: ValueMap) {
		let pv_len = p_values.len();
		let ev_len = e_values.len();
		assert_eq!(pv_len, ev_len, "expected {} output value(s)", ev_len);
		assert!(
			e_values.keys().all(|lbl| p_values.contains_key(lbl)),
			"unmatched values"
		);
	}

	fn check_program(
		input: &str,
		e_labels: &[Label],
		e_values: ValueMap,
		e_sections: SectionMap<Ins>,
	) -> TestResult {
		let tokens = lexer(input);
		let out = parser(&tokens)?;
		check_labels(out.labels, e_labels);
		check_values(out.values, e_values);
		check_sections(out.sections, e_sections);
		Ok(())
	}

	fn section(input: &str, expected: &[Ins]) -> TestResult {
		check_program(input, &[], [].into(), [(0, expected.to_vec())].into())
	}

	fn inst(input: &str, ins: Ins) -> TestResult {
		section(input, &[ins])
	}

	#[test]
	fn add1() -> TestResult {
		inst("ADD R0,R1", Ins::AddReg(0, 1))
	}

	#[test]
	fn add2() -> TestResult {
		inst("ADD #$01,R2", Ins::AddImm(1, 2))
	}

	// FIXME - srenshaw - Why is the parser not returning an error here?
	/*
		#[test]
		fn add3() -> TestResult {
			let input = "ADD #$FE,R3";
			let tokens = lexer(input);
			let out = parser(&tokens)?;
			assert!(out.labels.is_empty(), "output labels not empty");
			assert!(out.values.is_empty(), "output values not empty");
			assert_eq!(out.sections.len(), 1, "more than 1 output section");
			//assert_eq!(out.sections[&0], "ERROR: Expected '', Found ''");
			Ok(())
		}
	*/

	#[test]
	fn add4() -> TestResult {
		inst("ADD #-2,R2", Ins::AddImm(-2, 2))
	}

	#[test]
	fn addc() -> TestResult {
		inst("ADDC R3,R1", Ins::AddC(3, 1))
	}

	#[test]
	fn addv() -> TestResult {
		inst("ADDV R0,R1", Ins::AddV(0, 1))
	}

	#[test]
	fn and_reg_reg() -> TestResult {
		inst("AND R0,R1", Ins::AndReg(0, 1))
	}

	#[test]
	fn and_imm() -> TestResult {
		inst("AND #$0F,R0", Ins::AndImm(0x0F))
	}

	#[test]
	#[should_panic(expected = "Expected 'R0', Found 'R4' @ [1:10]")]
	fn and_imm_requires_r0() {
		match inst("AND #$0F,R4", Ins::AndImm(0x0F)) {
			Err(errors) => {
				assert_eq!(errors.len(), 1);
				panic!("{}", errors[0]);
			}
			Ok(_) => {}
		}
	}

	#[test]
	fn and_byte() -> TestResult {
		inst("AND.B #$80,@(R0,GBR)", Ins::AndByte(0x80))
	}

	#[test]
	fn bf() -> TestResult {
		check_program(
			"CLRT
BT TRGET_T
BF TRGET_F
NOP
NOP
TRGET_F:",
			&["TRGET_F".into()],
			[].into(),
			single_section(&[
				Ins::ClrT,
				Ins::Bt("TRGET_T".into()),
				Ins::Bf("TRGET_F".into()),
				Ins::Nop,
				Ins::Nop,
				Ins::Label("TRGET_F".into()),
			]),
		)
	}

	#[test]
	fn bfs() -> TestResult {
		check_program(
			"CLRT
BT/S TRGET_T
NOP
BF/S TRGET_F
ADD R0,R1
NOP
TRGET_F:",
			&["TRGET_F".into()],
			[].into(),
			single_section(&[
				Ins::ClrT,
				Ins::BtS("TRGET_T".into()),
				Ins::Nop,
				Ins::BfS("TRGET_F".into()),
				Ins::AddReg(0, 1),
				Ins::Nop,
				Ins::Label("TRGET_F".into()),
			]),
		)
	}

	#[test]
	fn bra() -> TestResult {
		check_program(
			"BRA TRGET
ADD R0,R1
NOP
TRGET:",
			&["TRGET".into()],
			[].into(),
			single_section(&[
				Ins::Bra("TRGET".into()),
				Ins::AddReg(0, 1),
				Ins::Nop,
				Ins::Label("TRGET".into()),
			]),
		)
	}

	#[test]
	fn bsr() -> TestResult {
		check_program(
			"BSR TRGET
	MOV R3,R4
	ADD R0,R1

TRGET:
	MOV R2,R3
	RTS
	MOV #1,R0",
			&["TRGET".into()],
			[].into(),
			single_section(&[
				Ins::Bsr("TRGET".into()),
				Ins::MovReg(3,4),
				Ins::AddReg(0,1),
				Ins::Label("TRGET".into()),
				Ins::MovReg(2,3),
				Ins::Rts,
				Ins::MovImm(1,0),
			]),
		)
	}

	#[test]
	fn bt() -> TestResult {
		check_program(
			"SETT
	BF TRGET_F
	BT TRGET_T
	NOP
	NOP
TRGET_T:",
			&["TRGET_T".into()],
			[].into(),
			single_section(&[
				Ins::SetT,
				Ins::Bf("TRGET_F".into()),
				Ins::Bt("TRGET_T".into()),
				Ins::Nop,
				Ins::Nop,
				Ins::Label("TRGET_T".into()),
			]),
		)
	}

	#[test]
	fn bts() -> TestResult {
		check_program(
			"SETT
	BF/S TRGET_F
	NOP
	BT/S TRGET_T
	ADD R0,R1
	NOP
TRGET_T:",
			&["TRGET_T".into()],
			[].into(),
			single_section(&[
				Ins::SetT,
				Ins::BfS("TRGET_F".into()),
				Ins::Nop,
				Ins::BtS("TRGET_T".into()),
				Ins::AddReg(0,1),
				Ins::Nop,
				Ins::Label("TRGET_T".into()),
			]),
		)
	}

	#[test]
	fn clrmac() -> TestResult {
		check_program(
			"CLRMAC
	MAC.W @R0+,@R1+
	MAC.W @R0+,@R1+",
			&[],
			[].into(),
			single_section(&[
				Ins::ClrMac,
				Ins::MacWord(0,1),
				Ins::MacWord(0,1),
			]),
		)
	}
}

