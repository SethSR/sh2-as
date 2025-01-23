use std::fmt;

use crate::Label;

#[derive(Clone)]
pub(crate) struct Token {
	tt: TokenType,
	ex: Option<Label>,
	line: u16,
	pos: u16,
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

fn tokenize<'a, I: Iterator<Item = (usize, char)>>(
	input: &'a str,
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
	pred: fn(char) -> bool,
) -> &'a str {
	let mut index = cur_idx + 1;
	while let Some((idx, ch)) = chars.peek() {
		index = *idx;
		if !pred(*ch) {
			break;
		}
		next(char_idx, chars);
	}
	&input[cur_idx..][..index - cur_idx]
}

fn next_line<I: Iterator<Item = (usize, char)>>(
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
) -> usize {
	let mut index = cur_idx + 1;
	while let Some((cmt_idx, cmt_char)) = chars.peek() {
		*char_idx += 1;
		index = *cmt_idx;
		if *cmt_char == '\n' {
			break;
		}
		next(char_idx, chars);
	}
	index - cur_idx
}

pub(crate) fn lexer(input: &str) -> Vec<Token> {
	use TokenType as TT;

	let mut results = Vec::new();
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
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					ch.is_ascii_digit() || '_' == ch || '-' == ch
				});
				if token.len() > 1 {
					let s = id_storage.entry(token).or_insert_with(|| token.into());
					results.push(Token::num(s.clone(), line_idx, char_idx - token.len()));
				} else {
					results.push(Token::new(TT::SymDash, line_idx, char_idx));
				}
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
				let token = tokenize(input, cur_idx, &mut chars, &mut char_idx, |ch| {
					ch.is_ascii_digit() || ('a'..='f').contains(&ch) || ('A'..='F').contains(&ch) || '_' == ch
				});
				let s = id_storage.entry(token).or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			'%' => {
				next(&mut char_idx, &mut chars);
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

	results
}
