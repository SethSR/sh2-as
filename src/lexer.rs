
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
	fn new(
		tt: TokenType,
		line: usize,
		pos: usize,
	) -> Self {
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

	pub(crate) fn pos(&self) -> (u16,u16) {
		(self.line,self.pos)
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

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub(crate) enum TokenType {
	IdComment,
	IdLabel,
	IdNumber,
	IdRegister,
	IdUnknown,
	InsADD,
	InsADDC,
	InsADDV,
	InsAND,
	InsBF,
	InsBRA,
	InsBRAF,
	InsBSR,
	InsBSRF,
	InsBT,
	InsCLRMAC,
	InsCLRT,
	InsCMP,
	InsDIV0S,
	InsDIV0U,
	InsDIV1,
	InsDMULS,
	InsDMULU,
	InsDT,
	InsEXTS,
	InsEXTU,
	InsJMP,
	InsJSR,
	InsLDC,
	InsLDS,
	InsMAC,
	InsMOV,
	InsMOVA,
	InsMOVT,
	InsMUL,
	InsMULS,
	InsMULU,
	InsNEG,
	InsNEGC,
	InsNOP,
	InsNOT,
	InsOR,
	InsROTCL,
	InsROTCR,
	InsROTL,
	InsROTR,
	InsRTE,
	InsRTS,
	InsSETT,
	InsSHAL,
	InsSHAR,
	InsSHLL,
	InsSHLL16,
	InsSHLL2,
	InsSHLL8,
	InsSHLR,
	InsSHLR16,
	InsSHLR2,
	InsSHLR8,
	InsSLEEP,
	InsSTC,
	InsSTS,
	InsSUB,
	InsSUBC,
	InsSUBV,
	InsSWAP,
	InsTAS,
	InsTRAPA,
	InsTST,
	InsXOR,
	InsXTRCT,
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
	SymSTR,
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
			TT::InsADD => "ADD",
			TT::InsADDC => "ADDC",
			TT::InsADDV => "ADDV",
			TT::InsAND => "AND",
			TT::InsBF => "BF",
			TT::InsBRA => "BRA",
			TT::InsBRAF => "BRAF",
			TT::InsBSR => "BSR",
			TT::InsBSRF => "BSRF",
			TT::InsBT => "BT",
			TT::InsCLRMAC => "CLRMAC",
			TT::InsCLRT => "CLRT",
			TT::InsCMP => "CMP",
			TT::InsDIV0S => "DIV0S",
			TT::InsDIV0U => "DIV0U",
			TT::InsDIV1 => "DIV1",
			TT::InsDMULS => "DMULS",
			TT::InsDMULU => "DMULU",
			TT::InsDT => "DT",
			TT::InsEXTS => "EXTS",
			TT::InsEXTU => "EXTU",
			TT::InsJMP => "JMP",
			TT::InsJSR => "JSR",
			TT::InsLDC => "LDC",
			TT::InsLDS => "LDS",
			TT::InsMAC => "MAC",
			TT::InsMOV => "MOV",
			TT::InsMOVA => "MOVA",
			TT::InsMOVT => "MOVT",
			TT::InsMUL => "MUL",
			TT::InsMULS => "MULS",
			TT::InsMULU => "MULU",
			TT::InsNEG => "NEG",
			TT::InsNEGC => "NEGC",
			TT::InsNOP => "NOP",
			TT::InsNOT => "NOT",
			TT::InsOR => "OR",
			TT::InsROTCL => "ROTCL",
			TT::InsROTCR => "ROTCR",
			TT::InsROTL => "ROTL",
			TT::InsROTR => "ROTR",
			TT::InsRTE => "RTE",
			TT::InsRTS => "RTS",
			TT::InsSETT => "SETT",
			TT::InsSHAL => "SHAL",
			TT::InsSHAR => "SHAR",
			TT::InsSHLL => "SHLL",
			TT::InsSHLL16 => "SHLL16",
			TT::InsSHLL2 => "SHLL2",
			TT::InsSHLL8 => "SHLL8",
			TT::InsSHLR => "SHLR",
			TT::InsSHLR16 => "SHLR16",
			TT::InsSHLR2 => "SHLR2",
			TT::InsSHLR8 => "SHLR8",
			TT::InsSLEEP => "SLEEP",
			TT::InsSTC => "STC",
			TT::InsSTS => "STS",
			TT::InsSUB => "SUB",
			TT::InsSUBC => "SUBC",
			TT::InsSUBV => "SUBV",
			TT::InsSWAP => "SWAP",
			TT::InsTAS => "TAS",
			TT::InsTRAPA => "TRAPA",
			TT::InsTST => "TST",
			TT::InsXOR => "XOR",
			TT::InsXTRCT => "XTRCT",
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
			TT::SymSTR => "STR",
			TT::SymSlash => "/",
			TT::SymVBR => "VBR",
			TT::SymWord => "w",
		};
		write!(fmt, "{s}")
	}
}

fn next(
	idx: &mut usize,
	chars: &mut impl Iterator<Item=(usize,char)>,
) {
	*idx += 1;
	chars.next();
}

fn tokenize<'a, I: Iterator<Item=(usize,char)>>(
	input: &'a str,
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
	pred: fn(char) -> bool,
) -> &'a str {
	let mut index = cur_idx + 1;
	while let Some((idx,ch)) = chars.peek() {
		index = *idx;
		if !pred(*ch) {
			break;
		}
		next(char_idx, chars);
	}
	&input[cur_idx..][..index - cur_idx]
}

fn next_line<I: Iterator<Item=(usize,char)>>(
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
) -> usize {
	let mut index = cur_idx + 1;
	while let Some((cmt_idx,cmt_char)) = chars.peek() {
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
				let token = tokenize(input,
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch) || '_' == ch || '-' == ch
				);
				if token.len() > 1 {
					let s = id_storage.entry(token)
						.or_insert_with(|| token.into());
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
				let token = tokenize(input,
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch) || '_' == ch);
				let s = id_storage.entry(token)
					.or_insert_with(|| token.into());
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
				let token = tokenize(input,
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch)
						|| ('a'..='f').contains(&ch)
						|| ('A'..='F').contains(&ch)
						|| '_' == ch
				);
				let s = id_storage.entry(token)
					.or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			'%' => {
				next(&mut char_idx, &mut chars);
				let token = tokenize(input,
					cur_idx, &mut chars, &mut char_idx,
					|ch| ['0','1'].contains(&ch) || '_' == ch);
				let s = id_storage.entry(token)
					.or_insert_with(|| token.into());
				results.push(Token::num(s.clone(), line_idx, char_idx - token.len() + 1));
			}
			';' => {
				let char_idx_s = char_idx;
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				let token = &input[cur_idx..][..size];
				// NOTE - srenshaw - We just assume every comment is unique.
				results.push(Token::comment(token.into(), line_idx, char_idx_s+1));
			}

			c if c.is_alphabetic() => {
				let token = tokenize(input,
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('a'..='z').contains(&ch)
						|| ('A'..='Z').contains(&ch)
						|| ('0'..='9').contains(&ch)
						|| '_' == ch);
				let tt = match token.to_lowercase().as_str() {
					"add" => TT::InsADD,
					"addc" => TT::InsADDC,
					"addv" => TT::InsADDV,
					"and" => TT::InsAND,
					"b" => TT::SymByte,
					"bf" => TT::InsBF,
					"bra" => TT::InsBRA,
					"braf" => TT::InsBRAF,
					"bsr" => TT::InsBSR,
					"bsrf" => TT::InsBSRF,
					"bt" => TT::InsBT,
					"clrmac" => TT::InsCLRMAC,
					"clrt" => TT::InsCLRT,
					"cmp" => TT::InsCMP,
					"dc" => TT::SymConst,
					"div0s" => TT::InsDIV0S,
					"div0u" => TT::InsDIV0U,
					"div1" => TT::InsDIV1,
					"dmuls" => TT::InsDMULS,
					"dmulu" => TT::InsDMULU,
					"dt" => TT::InsDT,
					"eq" => TT::SymEQ,
					"exts" => TT::InsEXTS,
					"extu" => TT::InsEXTU,
					"gbr" => TT::SymGBR,
					"ge" => TT::SymGE,
					"gt" => TT::SymGT,
					"hi" => TT::SymHI,
					"hs" => TT::SymHS,
					"jmp" => TT::InsJMP,
					"jsr" => TT::InsJSR,
					"l" => TT::SymLong,
					"ldc" => TT::InsLDC,
					"lds" => TT::InsLDS,
					"mac" => TT::InsMAC,
					"mach" => TT::SymMACH,
					"macl" => TT::SymMACL,
					"mov" => TT::InsMOV,
					"mova" => TT::InsMOVA,
					"movt" => TT::InsMOVT,
					"mul" => TT::InsMUL,
					"muls" => TT::InsMULS,
					"mulu" => TT::InsMULU,
					"neg" => TT::InsNEG,
					"negc" => TT::InsNEGC,
					"nop" => TT::InsNOP,
					"not" => TT::InsNOT,
					"or" => TT::InsOR,
					"org" => TT::SymOrg,
					"pc" => TT::SymPC,
					"pl" => TT::SymPL,
					"pr" => TT::SymPR,
					"pz" => TT::SymPZ,
					"rotcl" => TT::InsROTCL,
					"rotcr" => TT::InsROTCR,
					"rotl" => TT::InsROTL,
					"rotr" => TT::InsROTR,
					"rte" => TT::InsRTE,
					"rts" => TT::InsRTS,
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
					"sett" => TT::InsSETT,
					"shal" => TT::InsSHAL,
					"shar" => TT::InsSHAR,
					"shll" => TT::InsSHLL,
					"shll16" => TT::InsSHLL16,
					"shll2" => TT::InsSHLL2,
					"shll8" => TT::InsSHLL8,
					"shlr" => TT::InsSHLR,
					"shlr16" => TT::InsSHLR16,
					"shlr2" => TT::InsSHLR2,
					"shlr8" => TT::InsSHLR8,
					"sleep" => TT::InsSLEEP,
					"sr" => TT::SymSR,
					"stc" => TT::InsSTC,
					"str" => TT::SymSTR,
					"sts" => TT::InsSTS,
					"sub" => TT::InsSUB,
					"subc" => TT::InsSUBC,
					"subv" => TT::InsSUBV,
					"swap" => TT::InsSWAP,
					"tas" => TT::InsTAS,
					"trapa" => TT::InsTRAPA,
					"tst" => TT::InsTST,
					"vbr" => TT::SymVBR,
					"w" => TT::SymWord,
					"xor" => TT::InsXOR,
					"xtrct" => TT::InsXTRCT,
					_ => TT::IdLabel,
				};
				let s = id_storage.entry(token)
					.or_insert_with(|| token.into());
				results.push(Token::ident(tt, s.clone(), line_idx, char_idx - token.len() + 1));
			}
			_ => {
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				char_idx += size;
				let token = &input[cur_idx..][..size];
				let s = id_storage.entry(token)
					.or_insert_with(|| token.into());
				results.push(Token::ident(TT::IdUnknown, s.clone(), line_idx, char_idx));
			}
		}
	}

	results
}

