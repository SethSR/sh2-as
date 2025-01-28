use std::fmt;

use crate::Label;

#[derive(Debug)]
pub(crate) enum Error {
	NegHexadecimal(usize, usize),
	NegBinary(usize, usize),
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "ERROR: ")?;
		match self {
			Self::NegBinary(line, pos) => write!(fmt, "Binary values cannot be negated @ [{line}:{pos}]"),
			Self::NegHexadecimal(line, pos) => write!(fmt, "Hexadecimal values cannot be negated @ [{line}:{pos}]"),
		}
	}
}

#[derive(Clone)]
pub(crate) struct Token {
	tt: TokenType,
	ex: Option<Label>,
	line: u16,
	pos: u16,
}

impl std::cmp::PartialEq for Token {
	fn eq(&self, rhs: &Self) -> bool {
		self.get_type() == rhs.get_type() && self.ex == rhs.ex
	}
}

impl Token {
	fn new(tt: TokenType, line: usize, pos: usize) -> Self {
		Self {
			tt,
			ex: None,
			line: line as u16 + 1,
			pos: pos as u16,
		}
	}

	fn ident(tt: TokenType, s: Label, line: usize, pos: usize) -> Self {
		let mut this = Self::new(tt, line, pos);
		this.ex = Some(s);
		this
	}

	fn num(s: Label, line: usize, pos: usize) -> Self {
		let mut this = Self::new(TokenType::IdNumber, line, pos);
		this.ex = Some(s);
		this
	}

	fn comment(s: Label, line: usize, pos: usize) -> Self {
		let mut this = Self::new(TokenType::IdComment, line, pos);
		this.ex = Some(s);
		this
	}

	pub(crate) fn get_id(&self) -> Option<Label> {
		self.ex.clone()
	}

	pub(crate) fn get_type(&self) -> TokenType {
		self.tt
	}

	pub(crate) fn pos(&self) -> (u16, u16) {
		(self.line, self.pos)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		if let Some(txt) = &self.ex {
			write!(fmt, "{txt}")
		} else {
			write!(fmt, "{}", self.tt)
		}
	}
}

impl fmt::Debug for Token {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		let Token { tt, ex, line, pos } = self;
		if let Some(txt) = ex {
			write!(fmt, "{tt:?}({txt}) [{line}:{pos}]")
		} else {
			write!(fmt, "{tt:?} [{line}:{pos}]")
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenType {
	IdComment,
	IdLabel,
	IdNumber,
	IdRegister,
	IdUnknown,
	InsAdd,
	InsAddC,
	InsAddV,
	InsAnd,
	InsBf,
	InsBra,
	InsBraF,
	InsBsr,
	InsBsrF,
	InsBt,
	InsClrMac,
	InsClrT,
	InsCmp,
	InsDiv0S,
	InsDiv0U,
	InsDiv1,
	InsDMulS,
	InsDMulU,
	InsDT,
	InsExtS,
	InsExtU,
	InsJmp,
	InsJsr,
	InsLdc,
	InsLds,
	InsMac,
	InsMov,
	InsMovA,
	InsMovT,
	InsMul,
	InsMulS,
	InsMulU,
	InsNeg,
	InsNegC,
	InsNop,
	InsNot,
	InsOr,
	InsRotCL,
	InsRotCR,
	InsRotL,
	InsRotR,
	InsRte,
	InsRts,
	InsSetT,
	InsShAL,
	InsShAR,
	InsShLL,
	InsShLL16,
	InsShLL2,
	InsShLL8,
	InsShLR,
	InsShLR16,
	InsShLR2,
	InsShLR8,
	InsSleep,
	InsStc,
	InsSts,
	InsSub,
	InsSubC,
	InsSubV,
	InsSwap,
	InsTas,
	InsTrapA,
	InsTst,
	InsXor,
	InsXtrct,
	SymAddress,
	SymByte,
	SymCParen,
	SymColon,
	SymComma,
	SymConst,
	SymDash,
	SymDelay,
	SymDot,
	SymEQ,
	SymEqual,
	SymGBR,
	SymGE,
	SymGT,
	SymHI,
	SymHS,
	SymImmediate,
	SymLong,
	SymMACH,
	SymMACL,
	SymNewline,
	SymOParen,
	SymOrg,
	SymPC,
	SymPL,
	SymPR,
	SymPZ,
	SymPlus,
	SymSR,
	SymStr,
	SymSlash,
	SymVBR,
	SymWord,
}

impl fmt::Display for TokenType {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		use TokenType as TT;
		let s = match self {
			TT::IdComment => "Comment",
			TT::IdLabel => "Label",
			TT::IdNumber => "Number (bin/dec/hex)",
			TT::IdRegister => "Register (R0-15,PC)",
			TT::IdUnknown => "Unknown",
			TT::InsAdd => "ADD",
			TT::InsAddC => "ADDC",
			TT::InsAddV => "ADDV",
			TT::InsAnd => "AND",
			TT::InsBf => "BF",
			TT::InsBra => "BRA",
			TT::InsBraF => "BRAF",
			TT::InsBsr => "BSR",
			TT::InsBsrF => "BSRF",
			TT::InsBt => "BT",
			TT::InsClrMac => "CLRMAC",
			TT::InsClrT => "CLRT",
			TT::InsCmp => "CMP",
			TT::InsDiv0S => "DIV0S",
			TT::InsDiv0U => "DIV0U",
			TT::InsDiv1 => "DIV1",
			TT::InsDMulS => "DMULS",
			TT::InsDMulU => "DMULU",
			TT::InsDT => "DT",
			TT::InsExtS => "EXTS",
			TT::InsExtU => "EXTU",
			TT::InsJmp => "JMP",
			TT::InsJsr => "JSR",
			TT::InsLdc => "LDC",
			TT::InsLds => "LDS",
			TT::InsMac => "MAC",
			TT::InsMov => "MOV",
			TT::InsMovA => "MOVA",
			TT::InsMovT => "MOVT",
			TT::InsMul => "MUL",
			TT::InsMulS => "MULS",
			TT::InsMulU => "MULU",
			TT::InsNeg => "NEG",
			TT::InsNegC => "NEGC",
			TT::InsNop => "NOP",
			TT::InsNot => "NOT",
			TT::InsOr => "OR",
			TT::InsRotCL => "ROTCL",
			TT::InsRotCR => "ROTCR",
			TT::InsRotL => "ROTL",
			TT::InsRotR => "ROTR",
			TT::InsRte => "RTE",
			TT::InsRts => "RTS",
			TT::InsSetT => "SETT",
			TT::InsShAL => "SHAL",
			TT::InsShAR => "SHAR",
			TT::InsShLL => "SHLL",
			TT::InsShLL16 => "SHLL16",
			TT::InsShLL2 => "SHLL2",
			TT::InsShLL8 => "SHLL8",
			TT::InsShLR => "SHLR",
			TT::InsShLR16 => "SHLR16",
			TT::InsShLR2 => "SHLR2",
			TT::InsShLR8 => "SHLR8",
			TT::InsSleep => "SLEEP",
			TT::InsStc => "STC",
			TT::InsSts => "STS",
			TT::InsSub => "SUB",
			TT::InsSubC => "SUBC",
			TT::InsSubV => "SUBV",
			TT::InsSwap => "SWAP",
			TT::InsTas => "TAS",
			TT::InsTrapA => "TRAPA",
			TT::InsTst => "TST",
			TT::InsXor => "XOR",
			TT::InsXtrct => "XTRCT",
			TT::SymAddress => "@",
			TT::SymByte => "b",
			TT::SymCParen => ")",
			TT::SymColon => ":",
			TT::SymComma => ",",
			TT::SymConst => "dc",
			TT::SymDash => "-",
			TT::SymDelay => "S",
			TT::SymDot => ".",
			TT::SymEQ => "EQ",
			TT::SymEqual => "=",
			TT::SymGBR => "GBR",
			TT::SymGE => "GE",
			TT::SymGT => "GT",
			TT::SymHI => "HI",
			TT::SymHS => "HS",
			TT::SymImmediate => "#",
			TT::SymLong => "l",
			TT::SymMACH => "MACH",
			TT::SymMACL => "MACL",
			TT::SymNewline => "\\n",
			TT::SymOParen => "(",
			TT::SymOrg => "Org",
			TT::SymPC => "PC",
			TT::SymPL => "PL",
			TT::SymPR => "PR",
			TT::SymPZ => "PZ",
			TT::SymPlus => "+",
			TT::SymSR => "SR",
			TT::SymStr => "STR",
			TT::SymSlash => "/",
			TT::SymVBR => "VBR",
			TT::SymWord => "w",
		};
		write!(fmt, "{s}")
	}
}

fn next(idx: &mut usize, chars: &mut impl Iterator<Item = (usize, char)>) {
	*idx += 1;
	chars.next();
}

fn skip_while<I: Iterator<Item = (usize, char)>>(
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
	pred: fn(char) -> bool,
) -> usize {
	let mut index = cur_idx;
	loop {
		match chars.peek() {
			Some((idx, ch)) => {
				index = *idx;
				if !pred(*ch) {
					break;
				}
				next(char_idx, chars);
			}
			None => {
				index += 1;
				break;
			}
		}
	}
	index - cur_idx
}

fn tokenize<'a, I: Iterator<Item = (usize, char)>>(
	input: &'a str,
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
	pred: fn(char) -> bool,
) -> &'a str {
	let len = skip_while(cur_idx, chars, char_idx, pred);
	&input[cur_idx..][..len]
}

fn next_line<I: Iterator<Item = (usize, char)>>(
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
) -> usize {
	skip_while(cur_idx, chars, char_idx, |ch| ch != '\n')
}

pub(crate) fn lexer(input: &str) -> Result<Vec<Token>, Vec<Error>> {
	use TokenType as TT;

	let mut results = Vec::new();
	let mut errors = Vec::new();
	let mut line_idx = 0;
	let mut char_idx = 0;
	let mut chars = input.char_indices().peekable();

	let mut id_storage = std::collections::HashMap::<&str, Label>::default();

	while let Some(&(cur_idx, cur_char)) = chars.peek() {
		match cur_char {
			' ' | '\t' => next(&mut char_idx, &mut chars),
			'\n' => {
				chars.next();
				results.push(Token::new(TT::SymNewline, line_idx, char_idx));
				line_idx += 1;
				char_idx = 0;
			}
			',' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymComma, line_idx, char_idx));
			}
			'+' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymPlus, line_idx, char_idx));
			}
			'-' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymDash, line_idx, char_idx));
			}
			'/' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymSlash, line_idx, char_idx));
			}
			'@' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymAddress, line_idx, char_idx));
			}
			':' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymColon, line_idx, char_idx));
			}
			'.' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymDot, line_idx, char_idx));
			}
			'#' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymImmediate, line_idx, char_idx));
			}
			'0'..='9' => {
				next(&mut char_idx, &mut chars);
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					ch.is_ascii_digit() || '_' == ch
				});
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			'=' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymEqual, line_idx, char_idx));
			}
			'(' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymOParen, line_idx, char_idx));
			}
			')' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(TT::SymCParen, line_idx, char_idx));
			}
			'$' => {
				next(&mut char_idx, &mut chars);
				if let Some((idx, '-')) = chars.peek() {
					errors.push(Error::NegHexadecimal(line_idx, char_idx - idx + 1));
					next(&mut char_idx, &mut chars);
					continue;
				}
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					ch.is_ascii_digit() || ('a'..='f').contains(&ch) || ('A'..='F').contains(&ch) || '_' == ch
				});
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			'%' => {
				next(&mut char_idx, &mut chars);
				if let Some((idx, '-')) = chars.peek() {
					errors.push(Error::NegBinary(line_idx, char_idx - idx + 1));
					next(&mut char_idx, &mut chars);
					continue;
				}
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					['0', '1'].contains(&ch) || '_' == ch
				});
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			';' => {
				let char_idx_s = char_idx;
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				let token = &input[cur_idx..][..size];
				// NOTE - srenshaw - We just assume every comment is unique.
				results.push(Token::comment(token.into(), line_idx, char_idx_s + 1));
			}

			c if c.is_alphabetic() => {
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch.is_ascii_digit() || '_' == ch
				});
				let tt = match token.to_lowercase().as_str() {
					"add" => TT::InsAdd,
					"addc" => TT::InsAddC,
					"addv" => TT::InsAddV,
					"and" => TT::InsAnd,
					"b" => TT::SymByte,
					"bf" => TT::InsBf,
					"bra" => TT::InsBra,
					"braf" => TT::InsBraF,
					"bsr" => TT::InsBsr,
					"bsrf" => TT::InsBsrF,
					"bt" => TT::InsBt,
					"clrmac" => TT::InsClrMac,
					"clrt" => TT::InsClrT,
					"cmp" => TT::InsCmp,
					"dc" => TT::SymConst,
					"div0s" => TT::InsDiv0S,
					"div0u" => TT::InsDiv0U,
					"div1" => TT::InsDiv1,
					"dmuls" => TT::InsDMulS,
					"dmulu" => TT::InsDMulU,
					"dt" => TT::InsDT,
					"eq" => TT::SymEQ,
					"exts" => TT::InsExtS,
					"extu" => TT::InsExtU,
					"gbr" => TT::SymGBR,
					"ge" => TT::SymGE,
					"gt" => TT::SymGT,
					"hi" => TT::SymHI,
					"hs" => TT::SymHS,
					"jmp" => TT::InsJmp,
					"jsr" => TT::InsJsr,
					"l" => TT::SymLong,
					"ldc" => TT::InsLdc,
					"lds" => TT::InsLds,
					"mac" => TT::InsMac,
					"mach" => TT::SymMACH,
					"macl" => TT::SymMACL,
					"mov" => TT::InsMov,
					"mova" => TT::InsMovA,
					"movt" => TT::InsMovT,
					"mul" => TT::InsMul,
					"muls" => TT::InsMulS,
					"mulu" => TT::InsMulU,
					"neg" => TT::InsNeg,
					"negc" => TT::InsNegC,
					"nop" => TT::InsNop,
					"not" => TT::InsNot,
					"or" => TT::InsOr,
					"org" => TT::SymOrg,
					"pc" => TT::SymPC,
					"pl" => TT::SymPL,
					"pr" => TT::SymPR,
					"pz" => TT::SymPZ,
					"rotcl" => TT::InsRotCL,
					"rotcr" => TT::InsRotCR,
					"rotl" => TT::InsRotL,
					"rotr" => TT::InsRotR,
					"rte" => TT::InsRte,
					"rts" => TT::InsRts,
					"r0" => TT::IdRegister,
					"r1" => TT::IdRegister,
					"r2" => TT::IdRegister,
					"r3" => TT::IdRegister,
					"r4" => TT::IdRegister,
					"r5" => TT::IdRegister,
					"r6" => TT::IdRegister,
					"r7" => TT::IdRegister,
					"r8" => TT::IdRegister,
					"r9" => TT::IdRegister,
					"r10" => TT::IdRegister,
					"r11" => TT::IdRegister,
					"r12" => TT::IdRegister,
					"r13" => TT::IdRegister,
					"r14" => TT::IdRegister,
					"r15" => TT::IdRegister,
					"s" => TT::SymDelay,
					"sett" => TT::InsSetT,
					"shal" => TT::InsShAL,
					"shar" => TT::InsShAR,
					"shll" => TT::InsShLL,
					"shll16" => TT::InsShLL16,
					"shll2" => TT::InsShLL2,
					"shll8" => TT::InsShLL8,
					"shlr" => TT::InsShLR,
					"shlr16" => TT::InsShLR16,
					"shlr2" => TT::InsShLR2,
					"shlr8" => TT::InsShLR8,
					"sleep" => TT::InsSleep,
					"sr" => TT::SymSR,
					"stc" => TT::InsStc,
					"str" => TT::SymStr,
					"sts" => TT::InsSts,
					"sub" => TT::InsSub,
					"subc" => TT::InsSubC,
					"subv" => TT::InsSubV,
					"swap" => TT::InsSwap,
					"tas" => TT::InsTas,
					"trapa" => TT::InsTrapA,
					"tst" => TT::InsTst,
					"vbr" => TT::SymVBR,
					"w" => TT::SymWord,
					"xor" => TT::InsXor,
					"xtrct" => TT::InsXtrct,
					_ => TT::IdLabel,
				};
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::ident(
					tt,
					s.clone(),
					line_idx,
					char_idx - token.len() + 1,
				));
			}
			_ => {
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				char_idx += size;
				let token = &input[cur_idx..][..size];
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::ident(TT::IdUnknown, s.clone(), line_idx, char_idx));
			}
		}
	}

	if errors.is_empty() {
		Ok(results)
	} else {
		Err(errors)
	}
}

#[cfg(test)]
mod can_lex {
	use super::*;

	type TestResult = Result<(), Vec<Error>>;

	fn token_test(tt: TokenType, s: &str) -> Token {
		Token {
			tt,
			ex: Some(s.into()),
			line: 0,
			pos: 0,
		}
	}

	fn symbol_test(tt: TokenType) -> Token {
		Token {
			tt,
			ex: None,
			line: 0,
			pos: 0,
		}
	}

	fn match_token(input: &str, tt: TokenType) -> TestResult {
		let out = lexer(input)?;
		assert_eq!(out.len(), 1);
		assert_eq!(out[0], token_test(tt, input));
		Ok(())
	}

	#[test]
	fn comment() -> TestResult {
		match_token("; add and tst subv", TokenType::IdComment)
	}

	#[test]
	fn label() -> TestResult {
		match_token("StUfF", TokenType::IdLabel)
	}

	#[test]
	fn number() -> TestResult {
		let out = lexer("34")?;
		assert_eq!(out.len(), 1);
		assert_eq!(out[0], token_test(TokenType::IdNumber, "34"));
		Ok(())
	}

	#[test]
	fn number_neg() -> TestResult {
		let out = lexer("-52")?;
		assert_eq!(out.len(), 2);
		assert_eq!(out[0], symbol_test(TokenType::SymDash));
		assert_eq!(out[1], token_test(TokenType::IdNumber, "52"));
		Ok(())
	}

	#[test]
	fn number_no_mid_dash() -> TestResult {
		let out = lexer("2-3")?;
		assert_eq!(out.len(), 3);
		assert_eq!(out[0], token_test(TokenType::IdNumber, "2"));
		assert_eq!(out[1], symbol_test(TokenType::SymDash));
		assert_eq!(out[2], token_test(TokenType::IdNumber, "3"));
		Ok(())
	}

	#[test]
	fn number_hex() -> TestResult {
		let out = lexer("$2e")?;
		assert_eq!(out.len(), 1);
		assert_eq!(out[0], token_test(TokenType::IdNumber, "$2e"));
		Ok(())
	}

	#[test]
	#[should_panic(expected="ERROR: Hexadecimal values cannot be negated @ [0:1]")]
	fn number_hex_no_neg() {
		if let Err(es) = lexer("$-2e") {
			assert_eq!(es.len(), 1, "should have 1 error");
			panic!("{}", es[0]);
		}
	}

	#[test]
	fn number_bin() -> TestResult {
		let out = lexer("%101")?;
		assert_eq!(out.len(), 1);
		assert_eq!(out[0], token_test(TokenType::IdNumber, "%101"));
		Ok(())
	}

	#[test]
	#[should_panic(expected="ERROR: Binary values cannot be negated @ [0:1]")]
	fn number_bin_no_neg() {
		if let Err(es) = lexer("%-101") {
			assert_eq!(es.len(), 1, "should have 1 error");
			panic!("{}", es[0]);
		}
	}

	#[test]
	fn register() -> TestResult {
		let tt = TokenType::IdRegister;
		let out = lexer("r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 R12 R13 R14 r15 PC")?;
		assert_eq!(out.len(), 17);
		assert_eq!(out[0], token_test(tt, "r0"));
		assert_eq!(out[1], token_test(tt, "r1"));
		assert_eq!(out[2], token_test(tt, "r2"));
		assert_eq!(out[3], token_test(tt, "r3"));
		assert_eq!(out[4], token_test(tt, "r4"));
		assert_eq!(out[5], token_test(tt, "r5"));
		assert_eq!(out[6], token_test(tt, "r6"));
		assert_eq!(out[7], token_test(tt, "r7"));
		assert_eq!(out[8], token_test(tt, "r8"));
		assert_eq!(out[9], token_test(tt, "r9"));
		assert_eq!(out[10], token_test(tt, "r10"));
		assert_eq!(out[11], token_test(tt, "r11"));
		assert_eq!(out[12], token_test(tt, "R12"));
		assert_eq!(out[13], token_test(tt, "R13"));
		assert_eq!(out[14], token_test(tt, "R14"));
		assert_eq!(out[15], token_test(tt, "r15"));
		assert_eq!(out[16], token_test(TokenType::SymPC, "PC"));
		Ok(())
	}

	#[test]
	fn unknown() -> TestResult {
		match_token("^?^", TokenType::IdUnknown)
	}

	#[test]
	fn add() -> TestResult {
		let tt = TokenType::InsAdd;
		match_token("add", tt)?;
		match_token("ADD", tt)
	}

	#[test]
	fn addc() -> TestResult {
		let tt = TokenType::InsAddC;
		match_token("addc", tt)?;
		match_token("ADDC", tt)
	}

	#[test]
	fn addv() -> TestResult {
		let tt = TokenType::InsAddV;
		match_token("addv", tt)?;
		match_token("ADDV", tt)
	}

	#[test]
	fn and() -> TestResult {
		let tt = TokenType::InsAnd;
		match_token("and", tt)?;
		match_token("AND", tt)
	}

	#[test]
	fn bf() -> TestResult {
		let tt = TokenType::InsBf;
		match_token("bf", tt)?;
		match_token("BF", tt)
	}

	#[test]
	fn bra() -> TestResult {
		let tt = TokenType::InsBra;
		match_token("bra", tt)?;
		match_token("BRA", tt)
	}

	#[test]
	fn braf() -> TestResult {
		let tt = TokenType::InsBraF;
		match_token("braf", tt)?;
		match_token("BRAF", tt)
	}

	#[test]
	fn bsr() -> TestResult {
		let tt = TokenType::InsBsr;
		match_token("bsr", tt)?;
		match_token("BSR", tt)
	}

	#[test]
	fn bsrf() -> TestResult {
		let tt = TokenType::InsBsrF;
		match_token("bsrf", tt)?;
		match_token("BSRF", tt)
	}

	#[test]
	fn bt() -> TestResult {
		let tt = TokenType::InsBt;
		match_token("bt", tt)?;
		match_token("BT", tt)
	}

	#[test]
	fn clrmac() -> TestResult {
		let tt = TokenType::InsClrMac;
		match_token("clrmac", tt)?;
		match_token("CLRMAC", tt)
	}

	#[test]
	fn clrt() -> TestResult {
		let tt = TokenType::InsClrT;
		match_token("clrt", tt)?;
		match_token("CLRT", tt)
	}

	#[test]
	fn cmp() -> TestResult {
		let tt = TokenType::InsCmp;
		match_token("cmp", tt)?;
		match_token("CMP", tt)
	}

	#[test]
	fn div0s() -> TestResult {
		let tt = TokenType::InsDiv0S;
		match_token("div0s", tt)?;
		match_token("DIV0S", tt)
	}

	#[test]
	fn div0u() -> TestResult {
		let tt = TokenType::InsDiv0U;
		match_token("div0u", tt)?;
		match_token("DIV0U", tt)
	}

	#[test]
	fn div1() -> TestResult {
		let tt = TokenType::InsDiv1;
		match_token("div1", tt)?;
		match_token("DIV1", tt)
	}

	#[test]
	fn dmuls() -> TestResult {
		let tt = TokenType::InsDMulS;
		match_token("dmuls", tt)?;
		match_token("DMULS", tt)
	}

	#[test]
	fn dmulu() -> TestResult {
		let tt = TokenType::InsDMulU;
		match_token("dmulu", tt)?;
		match_token("DMULU", tt)
	}

	#[test]
	fn dt() -> TestResult {
		let tt = TokenType::InsDT;
		match_token("dt", tt)?;
		match_token("DT", tt)
	}

	#[test]
	fn exts() -> TestResult {
		let tt = TokenType::InsExtS;
		match_token("exts", tt)?;
		match_token("EXTS", tt)
	}

	#[test]
	fn extu() -> TestResult {
		let tt = TokenType::InsExtU;
		match_token("extu", tt)?;
		match_token("EXTU", tt)
	}

	#[test]
	fn jmp() -> TestResult {
		let tt = TokenType::InsJmp;
		match_token("jmp", tt)?;
		match_token("JMP", tt)
	}

	#[test]
	fn jsr() -> TestResult {
		let tt = TokenType::InsJsr;
		match_token("jsr", tt)?;
		match_token("JSR", tt)
	}

	#[test]
	fn ldc() -> TestResult {
		let tt = TokenType::InsLdc;
		match_token("ldc", tt)?;
		match_token("LDC", tt)
	}

	#[test]
	fn lds() -> TestResult {
		let tt = TokenType::InsLds;
		match_token("lds", tt)?;
		match_token("LDS", tt)
	}

	#[test]
	fn mac() -> TestResult {
		let tt = TokenType::InsMac;
		match_token("mac", tt)?;
		match_token("MAC", tt)
	}

	#[test]
	fn mov() -> TestResult {
		let tt = TokenType::InsMov;
		match_token("mov", tt)?;
		match_token("MOV", tt)
	}

	#[test]
	fn mova() -> TestResult {
		let tt = TokenType::InsMovA;
		match_token("mova", tt)?;
		match_token("MOVA", tt)
	}

	#[test]
	fn movt() -> TestResult {
		let tt = TokenType::InsMovT;
		match_token("movt", tt)?;
		match_token("MOVT", tt)
	}

	#[test]
	fn mul() -> TestResult {
		let tt = TokenType::InsMul;
		match_token("mul", tt)?;
		match_token("MUL", tt)
	}

	#[test]
	fn muls() -> TestResult {
		let tt = TokenType::InsMulS;
		match_token("muls", tt)?;
		match_token("MULS", tt)
	}

	#[test]
	fn mulu() -> TestResult {
		let tt = TokenType::InsMulU;
		match_token("mulu", tt)?;
		match_token("MULU", tt)
	}

	#[test]
	fn neg() -> TestResult {
		let tt = TokenType::InsNeg;
		match_token("neg", tt)?;
		match_token("NEG", tt)
	}

	#[test]
	fn negc() -> TestResult {
		let tt = TokenType::InsNegC;
		match_token("negc", tt)?;
		match_token("NEGC", tt)
	}

	#[test]
	fn nop() -> TestResult {
		let tt = TokenType::InsNop;
		match_token("nop", tt)?;
		match_token("NOP", tt)
	}

	#[test]
	fn not() -> TestResult {
		let tt = TokenType::InsNot;
		match_token("not", tt)?;
		match_token("NOT", tt)
	}

	#[test]
	fn or() -> TestResult {
		let tt = TokenType::InsOr;
		match_token("or", tt)?;
		match_token("OR", tt)
	}

	#[test]
	fn rotcl() -> TestResult {
		let tt = TokenType::InsRotCL;
		match_token("rotcl", tt)?;
		match_token("ROTCL", tt)
	}

	#[test]
	fn rotcr() -> TestResult {
		let tt = TokenType::InsRotCR;
		match_token("rotcr", tt)?;
		match_token("ROTCR", tt)
	}

	#[test]
	fn rotl() -> TestResult {
		let tt = TokenType::InsRotL;
		match_token("rotl", tt)?;
		match_token("ROTL", tt)
	}

	#[test]
	fn rotr() -> TestResult {
		let tt = TokenType::InsRotR;
		match_token("rotr", tt)?;
		match_token("ROTR", tt)
	}

	#[test]
	fn rte() -> TestResult {
		let tt = TokenType::InsRte;
		match_token("rte", tt)?;
		match_token("RTE", tt)
	}

	#[test]
	fn rts() -> TestResult {
		let tt = TokenType::InsRts;
		match_token("rts", tt)?;
		match_token("RTS", tt)
	}

	#[test]
	fn sett() -> TestResult {
		let tt = TokenType::InsSetT;
		match_token("sett", tt)?;
		match_token("SEtt", tt)
	}

	#[test]
	fn shal() -> TestResult {
		let tt = TokenType::InsShAL;
		match_token("shal", tt)?;
		match_token("SHAL", tt)
	}

	#[test]
	fn shar() -> TestResult {
		let tt = TokenType::InsShAR;
		match_token("shar", tt)?;
		match_token("SHAR", tt)
	}

	#[test]
	fn shll() -> TestResult {
		let tt = TokenType::InsShLL;
		match_token("shll", tt)?;
		match_token("SHLL", tt)
	}

	#[test]
	fn shll16() -> TestResult {
		let tt = TokenType::InsShLL16;
		match_token("shll16", tt)?;
		match_token("SHLL16", tt)
	}

	#[test]
	fn shll2() -> TestResult {
		let tt = TokenType::InsShLL2;
		match_token("shll2", tt)?;
		match_token("SHLL2", tt)
	}

	#[test]
	fn shll8() -> TestResult {
		let tt = TokenType::InsShLL8;
		match_token("shll8", tt)?;
		match_token("SHLL8", tt)
	}

	#[test]
	fn shlr() -> TestResult {
		let tt = TokenType::InsShLR;
		match_token("shlr", tt)?;
		match_token("SHLR", tt)
	}

	#[test]
	fn shlr16() -> TestResult {
		let tt = TokenType::InsShLR16;
		match_token("shlr16", tt)?;
		match_token("SHLR16", tt)
	}

	#[test]
	fn shlr2() -> TestResult {
		let tt = TokenType::InsShLR2;
		match_token("shlr2", tt)?;
		match_token("SHLR2", tt)
	}

	#[test]
	fn shlr8() -> TestResult {
		let tt = TokenType::InsShLR8;
		match_token("shlr8", tt)?;
		match_token("SHLR8", tt)
	}

	#[test]
	fn sleep() -> TestResult {
		let tt = TokenType::InsSleep;
		match_token("sleep", tt)?;
		match_token("SLEEP", tt)
	}

	#[test]
	fn stc() -> TestResult {
		let tt = TokenType::InsStc;
		match_token("stc", tt)?;
		match_token("STC", tt)
	}

	#[test]
	fn sts() -> TestResult {
		let tt = TokenType::InsSts;
		match_token("sts", tt)?;
		match_token("STS", tt)
	}

	#[test]
	fn sub() -> TestResult {
		let tt = TokenType::InsSub;
		match_token("sub", tt)?;
		match_token("SUB", tt)
	}

	#[test]
	fn subc() -> TestResult {
		let tt = TokenType::InsSubC;
		match_token("subc", tt)?;
		match_token("SUBC", tt)
	}

	#[test]
	fn subv() -> TestResult {
		let tt = TokenType::InsSubV;
		match_token("subv", tt)?;
		match_token("SUBV", tt)
	}

	#[test]
	fn swap() -> TestResult {
		let tt = TokenType::InsSwap;
		match_token("swap", tt)?;
		match_token("SWAP", tt)
	}

	#[test]
	fn tas() -> TestResult {
		let tt = TokenType::InsTas;
		match_token("tas", tt)?;
		match_token("TAS", tt)
	}

	#[test]
	fn trapa() -> TestResult {
		let tt = TokenType::InsTrapA;
		match_token("trapa", tt)?;
		match_token("TRAPA", tt)
	}

	#[test]
	fn tst() -> TestResult {
		let tt = TokenType::InsTst;
		match_token("tst", tt)?;
		match_token("TST", tt)
	}

	#[test]
	fn xor() -> TestResult {
		let tt = TokenType::InsXor;
		match_token("xor", tt)?;
		match_token("XOR", tt)
	}

	#[test]
	fn xtrct() -> TestResult {
		let tt = TokenType::InsXtrct;
		match_token("xtrct", tt)?;
		match_token("XTRCT", tt)
	}

	fn match_symbol(s: &str, tt: TokenType) -> TestResult {
		let out = lexer(s)?;
		assert_eq!(out.len(), 1);
		assert_eq!(out[0], Token {
			tt,
			ex: None,
			line: 0,
			pos: 0
		});
		Ok(())
	}

	#[test]
	fn address() -> TestResult {
		match_symbol("@", TokenType::SymAddress)
	}

	#[test]
	fn byte() -> TestResult {
		match_token("b", TokenType::SymByte)?;
		match_token("B", TokenType::SymByte)
	}

	#[test]
	fn cparen() -> TestResult {
		match_symbol(")", TokenType::SymCParen)
	}

	#[test]
	fn colon() -> TestResult {
		match_symbol(":", TokenType::SymColon)
	}

	#[test]
	fn comma() -> TestResult {
		match_symbol(",", TokenType::SymComma)
	}

	#[test]
	fn declare_const() -> TestResult {
		match_token("dc", TokenType::SymConst)?;
		match_token("DC", TokenType::SymConst)
	}

	#[test]
	fn dash() -> TestResult {
		match_symbol("-", TokenType::SymDash)
	}

	#[test]
	fn delay() -> TestResult {
		match_token("s", TokenType::SymDelay)?;
		match_token("S", TokenType::SymDelay)
	}

	#[test]
	fn dot() -> TestResult {
		match_symbol(".", TokenType::SymDot)
	}

	#[test]
	fn eq() -> TestResult {
		match_token("eq", TokenType::SymEQ)?;
		match_token("EQ", TokenType::SymEQ)
	}

	#[test]
	fn equal() -> TestResult {
		match_symbol("=", TokenType::SymEqual)
	}

	#[test]
	fn gbr() -> TestResult {
		match_token("gbr", TokenType::SymGBR)?;
		match_token("GBR", TokenType::SymGBR)
	}

	#[test]
	fn ge() -> TestResult {
		match_token("ge", TokenType::SymGE)?;
		match_token("GE", TokenType::SymGE)
	}

	#[test]
	fn gt() -> TestResult {
		match_token("gt", TokenType::SymGT)?;
		match_token("GT", TokenType::SymGT)
	}

	#[test]
	fn hi() -> TestResult {
		match_token("hi", TokenType::SymHI)?;
		match_token("HI", TokenType::SymHI)
	}

	#[test]
	fn hs() -> TestResult {
		match_token("hs", TokenType::SymHS)?;
		match_token("HS", TokenType::SymHS)
	}

	#[test]
	fn immediate() -> TestResult {
		match_symbol("#", TokenType::SymImmediate)
	}

	#[test]
	fn long() -> TestResult {
		match_token("l", TokenType::SymLong)
	}

	#[test]
	fn mach() -> TestResult {
		match_token("mach", TokenType::SymMACH)?;
		match_token("MACH", TokenType::SymMACH)
	}

	#[test]
	fn macl() -> TestResult {
		match_token("macl", TokenType::SymMACL)?;
		match_token("MACL", TokenType::SymMACL)
	}

	#[test]
	fn newline() -> TestResult {
		match_symbol("\n", TokenType::SymNewline)
	}

	#[test]
	fn oparen() -> TestResult {
		match_symbol("(", TokenType::SymOParen)
	}

	#[test]
	fn org() -> TestResult {
		match_token("org", TokenType::SymOrg)?;
		match_token("ORG", TokenType::SymOrg)
	}

	#[test]
	fn pl() -> TestResult {
		match_token("pl", TokenType::SymPL)?;
		match_token("PL", TokenType::SymPL)
	}

	#[test]
	fn pr() -> TestResult {
		match_token("pr", TokenType::SymPR)?;
		match_token("PR", TokenType::SymPR)
	}

	#[test]
	fn pz() -> TestResult {
		match_token("pz", TokenType::SymPZ)?;
		match_token("PZ", TokenType::SymPZ)
	}

	#[test]
	fn plus() -> TestResult {
		match_symbol("+", TokenType::SymPlus)
	}

	#[test]
	fn sr() -> TestResult {
		match_token("sr", TokenType::SymSR)?;
		match_token("SR", TokenType::SymSR)
	}

	#[test]
	fn str() -> TestResult {
		match_token("str", TokenType::SymStr)?;
		match_token("STR", TokenType::SymStr)
	}

	#[test]
	fn slash() -> TestResult {
		match_symbol("/", TokenType::SymSlash)
	}

	#[test]
	fn vbr() -> TestResult {
		match_token("vbr", TokenType::SymVBR)?;
		match_token("VBR", TokenType::SymVBR)
	}

	#[test]
	fn word() -> TestResult {
		match_token("w", TokenType::SymWord)
	}
}
