
use std::fmt;

use crate::Label;

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
		let mut this = Self::new(TokenType::Number, line, pos);
		this.ex = Some(s);
		this
	}

	fn comment(s: Label, line: usize, pos: usize) -> Self {
		let mut this = Self::new(TokenType::Comment, line, pos);
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

#[repr(u8)]
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub(crate) enum TokenType {
	ADD,
	ADDC,
	ADDV,
	AND,
	Address,
	BF,
	BRA,
	BRAF,
	BSR,
	BSRF,
	BT,
	Byte,
	CLRMAC,
	CLRT,
	CMP,
	CParen,
	Colon,
	Comma,
	Comment,
	Const,
	Delay,
	DIV0S,
	DIV0U,
	DIV1,
	DMULS,
	DMULU,
	DT,
	Dash,
	Dot,
	EQ,
	EXTS,
	EXTU,
	Equal,
	GBR,
	GE,
	GT,
	HI,
	HS,
	Identifier,
	Immediate,
	JMP,
	JSR,
	LDC,
	LDS,
	Long,
	MAC,
	MACH,
	MACL,
	MOV,
	MOVA,
	MOVT,
	MUL,
	MULS,
	MULU,
	NEG,
	NEGC,
	NOP,
	NOT,
	Newline,
	Number,
	OParen,
	OR,
	Org,
	PC,
	PL,
	PR,
	PZ,
	Plus,
	ROTCL,
	ROTCR,
	ROTL,
	ROTR,
	RTE,
	RTS,
	Register,
	SETT,
	SHAL,
	SHAR,
	SHLL,
	SHLL16,
	SHLL2,
	SHLL8,
	SHLR,
	SHLR16,
	SHLR2,
	SHLR8,
	SLEEP,
	SR,
	STC,
	STR,
	STS,
	SUB,
	SUBC,
	SUBV,
	SWAP,
	Slash,
	TAS,
	TRAPA,
	TST,
	Unknown,
	VBR,
	Word,
	XOR,
	XTRCT,
}

impl fmt::Display for TokenType {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		use TokenType as TT;
		let s = match self {
			TT::ADD => "ADD",
			TT::ADDC => "ADDC",
			TT::ADDV => "ADDV",
			TT::AND => "AND",
			TT::Address => "@",
			TT::BF => "BF",
			TT::BRA => "BRA",
			TT::BRAF => "BRAF",
			TT::BSR => "BSR",
			TT::BSRF => "BSRF",
			TT::BT => "BT",
			TT::Byte => "b",
			TT::CLRMAC => "CLRMAC",
			TT::CLRT => "CLRT",
			TT::CMP => "CMP",
			TT::CParen => ")",
			TT::Colon => ":",
			TT::Comma => ",",
			TT::Comment => "Comment",
			TT::Const => "dc",
			TT::DIV0S => "DIV0S",
			TT::DIV0U => "DIV0U",
			TT::DIV1 => "DIV1",
			TT::DMULS => "DMULS",
			TT::DMULU => "DMULU",
			TT::DT => "DT",
			TT::Dash => "-",
			TT::Delay => "S",
			TT::Dot => ".",
			TT::EQ => "EQ",
			TT::EXTS => "EXTS",
			TT::EXTU => "EXTU",
			TT::Equal => "=",
			TT::GBR => "GBR",
			TT::GE => "GE",
			TT::GT => "GT",
			TT::HI => "HI",
			TT::HS => "HS",
			TT::Identifier => "Identifier",
			TT::Immediate => "#",
			TT::JMP => "JMP",
			TT::JSR => "JSR",
			TT::LDC => "LDC",
			TT::LDS => "LDS",
			TT::Long => "l",
			TT::MAC => "MAC",
			TT::MACH => "MACH",
			TT::MACL => "MACL",
			TT::MOV => "MOV",
			TT::MOVA => "MOVA",
			TT::MOVT => "MOVT",
			TT::MUL => "MUL",
			TT::MULS => "MULS",
			TT::MULU => "MULU",
			TT::NEG => "NEG",
			TT::NEGC => "NEGC",
			TT::NOP => "NOP",
			TT::NOT => "NOT",
			TT::Newline => "\\n",
			TT::Number => "Number (bin/dec/hex)",
			TT::OParen => "(",
			TT::OR => "OR",
			TT::Org => "Org",
			TT::PC => "PC",
			TT::PL => "PL",
			TT::PR => "PR",
			TT::PZ => "PZ",
			TT::Plus => "+",
			TT::ROTCL => "ROTCL",
			TT::ROTCR => "ROTCR",
			TT::ROTL => "ROTL",
			TT::ROTR => "ROTR",
			TT::RTE => "RTE",
			TT::RTS => "RTS",
			TT::Register => "Register (R0-15,PC)",
			TT::SETT => "SETT",
			TT::SHAL => "SHAL",
			TT::SHAR => "SHAR",
			TT::SHLL => "SHLL",
			TT::SHLL16 => "SHLL16",
			TT::SHLL2 => "SHLL2",
			TT::SHLL8 => "SHLL8",
			TT::SHLR => "SHLR",
			TT::SHLR16 => "SHLR16",
			TT::SHLR2 => "SHLR2",
			TT::SHLR8 => "SHLR8",
			TT::SLEEP => "SLEEP",
			TT::SR => "SR",
			TT::STC => "STC",
			TT::STR => "STR",
			TT::STS => "STS",
			TT::SUB => "SUB",
			TT::SUBC => "SUBC",
			TT::SUBV => "SUBV",
			TT::SWAP => "SWAP",
			TT::Slash => "/",
			TT::TAS => "TAS",
			TT::TRAPA => "TRAPA",
			TT::TST => "TST",
			TT::Unknown => "Unknown",
			TT::VBR => "VBR",
			TT::Word => "w",
			TT::XOR => "XOR",
			TT::XTRCT => "XTRCT",
		};
		write!(fmt, "'{s}'")
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
	use TokenType::*;

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
				results.push(Token::new(Newline, line_idx, char_idx));
				line_idx += 1;
				char_idx = 0;
			}
			',' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Comma, line_idx, char_idx));
			}
			'+' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Plus, line_idx, char_idx));
			}
			'-' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dash, line_idx, char_idx));
			}
			'/' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Slash, line_idx, char_idx));
			}
			'@' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Address, line_idx, char_idx));
			}
			':' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Colon, line_idx, char_idx));
			}
			'.' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dot, line_idx, char_idx));
			}
			'#' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Immediate, line_idx, char_idx));
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
				results.push(Token::new(Equal, line_idx, char_idx));
			}
			'(' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(OParen, line_idx, char_idx));
			}
			')' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(CParen, line_idx, char_idx));
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
					"add" => ADD,
					"addc" => ADDC,
					"addv" => ADDV,
					"and" => AND,
					"b" => Byte,
					"bf" => BF,
					"bra" => BRA,
					"braf" => BRAF,
					"bsr" => BSR,
					"bsrf" => BSRF,
					"bt" => BT,
					"clrmac" => CLRMAC,
					"clrt" => CLRT,
					"cmp" => CMP,
					"dc" => Const,
					"div0s" => DIV0S,
					"div0u" => DIV0U,
					"div1" => DIV1,
					"dmuls" => DMULS,
					"dmulu" => DMULU,
					"dt" => DT,
					"eq" => EQ,
					"exts" => EXTS,
					"extu" => EXTU,
					"gbr" => GBR,
					"ge" => GE,
					"gt" => GT,
					"hi" => HI,
					"hs" => HS,
					"jmp" => JMP,
					"jsr" => JSR,
					"l" => Long,
					"ldc" => LDC,
					"lds" => LDS,
					"mac" => MAC,
					"mach" => MACH,
					"macl" => MACL,
					"mov" => MOV,
					"mova" => MOVA,
					"movt" => MOVT,
					"mul" => MUL,
					"muls" => MULS,
					"mulu" => MULU,
					"neg" => NEG,
					"negc" => NEGC,
					"nop" => NOP,
					"not" => NOT,
					"or" => OR,
					"org" => Org,
					"pc" => PC,
					"pl" => PL,
					"pr" => PR,
					"pz" => PZ,
					"rotcl" => ROTCL,
					"rotcr" => ROTCR,
					"rotl" => ROTL,
					"rotr" => ROTR,
					"rte" => RTE,
					"rts" => RTS,
					"r0" => Register,
					"r1" => Register,
					"r2" => Register,
					"r3" => Register,
					"r4" => Register,
					"r5" => Register,
					"r6" => Register,
					"r7" => Register,
					"r8" => Register,
					"r9" => Register,
					"r10" => Register,
					"r11" => Register,
					"r12" => Register,
					"r13" => Register,
					"r14" => Register,
					"r15" => Register,
					"s" => Delay,
					"sett" => SETT,
					"shal" => SHAL,
					"shar" => SHAR,
					"shll" => SHLL,
					"shll16" => SHLL16,
					"shll2" => SHLL2,
					"shll8" => SHLL8,
					"shlr" => SHLR,
					"shlr16" => SHLR16,
					"shlr2" => SHLR2,
					"shlr8" => SHLR8,
					"sleep" => SLEEP,
					"sr" => SR,
					"stc" => STC,
					"str" => STR,
					"sts" => STS,
					"sub" => SUB,
					"subc" => SUBC,
					"subv" => SUBV,
					"swap" => SWAP,
					"tas" => TAS,
					"trapa" => TRAPA,
					"tst" => TST,
					"vbr" => VBR,
					"w" => Word,
					"xor" => XOR,
					"xtrct" => XTRCT,
					_ => Identifier,
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
				results.push(Token::ident(Unknown, s.clone(), line_idx, char_idx));
			}
		}
	}

	results
}

