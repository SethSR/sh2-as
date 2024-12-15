
use std::fmt;

pub(crate) struct Token {
	tt: TokenType,
	index: u16,
	size: u8,
	line: u16,
	pos: u16,
}

impl Token {
	fn new(
		tt: TokenType,
		index: usize,
		size: usize,
		line: usize,
		pos: usize,
	) -> Self {
		Self {
			tt,
			index: index as u16,
			size: size as u8,
			line: line as u16 + 1,
			pos: pos as u16,
		}
	}

	pub(crate) fn get_type(&self) -> TokenType {
		self.tt
	}

	pub(crate) fn to_string(&self, file: &str) -> String {
		file[self.index as usize..][..self.size as usize].to_owned()
	}

	pub(crate) fn to_debug_string(&self, file: &str) -> String {
		let Token { tt, index, size, line, pos } = self;
		if *tt == TokenType::Newline {
			format!("{tt:?} [{line}:{pos}]")
		} else {
			let txt = &file[*index as usize..][..*size as usize];
			format!("{tt:?}({txt}) [{line}:{pos}]")
		}
	}

	pub(crate) fn pos(&self) -> (u16,u16) {
		(self.line,self.pos)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		let Token { tt, ..} = self;
		write!(fmt, "{tt:?}")
	}
}

impl fmt::Debug for Token {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		let Token { tt, line, pos, ..} = self;
		write!(fmt, "{tt:?} [{line}:{pos}]")
	}
}

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
	GE,
	GT,
	HI,
	HS,
	Identifier,
	JMP,
	JSR,
	LDC,
	LDS,
	Long,
	MAC,
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
	PL,
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
	Word,
	XOR,
	XTRCT,
}

impl fmt::Display for TokenType {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		use TokenType::*;
		let s = match self {
			Address => "@",
			Byte => "b",
			CParen => ")",
			Colon => ":",
			Comma => ",",
			Const => "dc",
			Dash => "-",
			Delay => "S",
			Dot => ".",
			Equal => "=",
			Long => "l",
			Newline => "\\n",
			Number => "Number (bin/dec/hex)",
			OParen => "(",
			Plus => "+",
			Register => "Register (R0-15,PC)",
			Slash => "/",
			Word => "w",
			_ => &format!("{self:?}"),
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

fn tokenize<I: Iterator<Item=(usize,char)>>(
	cur_idx: usize,
	chars: &mut std::iter::Peekable<I>,
	char_idx: &mut usize,
	pred: fn(char) -> bool,
) -> usize {
	let mut index = cur_idx + 1;
	while let Some((idx,ch)) = chars.peek() {
		index = *idx;
		if !pred(*ch) {
			break;
		}
		next(char_idx, chars);
	}
	index - cur_idx
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

	while let Some(&(cur_idx, cur_char)) = chars.peek() {
		match cur_char {
			' ' | '\t' => next(&mut char_idx, &mut chars),
			'\n' => {
				chars.next();
				results.push(Token::new(Newline, cur_idx, 1, line_idx, char_idx));
				line_idx += 1;
				char_idx = 0;
			}
			',' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Comma, cur_idx, 1, line_idx, char_idx));
			}
			'+' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Plus, cur_idx, 1, line_idx, char_idx));
			}
			'-' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dash, cur_idx, 1, line_idx, char_idx));
			}
			'/' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Slash, cur_idx, 1, line_idx, char_idx));
			}
			'@' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Address, cur_idx, 1, line_idx, char_idx));
			}
			':' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Colon, cur_idx, 1, line_idx, char_idx));
			}
			'.' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dot, cur_idx, 1, line_idx, char_idx));
			}
			'0'..='9' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch) || '_' == ch);
				results.push(Token::new(Number, cur_idx, size, line_idx, char_idx - size + 1));
			}
			'=' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Equal, cur_idx, 1, line_idx, char_idx));
			}
			'(' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(OParen,cur_idx,1, line_idx, char_idx));
			}
			')' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(CParen,cur_idx,1, line_idx, char_idx));
			}
			'$' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch)
						|| ('a'..='f').contains(&ch)
						|| ('A'..='F').contains(&ch)
						|| '_' == ch
				);
				results.push(Token::new(Number, cur_idx, size, line_idx, char_idx - size + 1));
			}
			'%' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ['0','1'].contains(&ch) || '_' == ch);
				results.push(Token::new(Number, cur_idx, size, line_idx, char_idx - size + 1));
			}
			'r' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch));
				results.push(Token::new(Register, cur_idx, size, line_idx, char_idx - size + 1));
			}
			';' => {
				let char_idx_s = char_idx;
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				results.push(Token::new(Comment, cur_idx, size, line_idx, char_idx_s+1));
			}

			c if c.is_alphabetic() => {
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('a'..='z').contains(&ch)
						|| ('A'..='Z').contains(&ch)
						|| ('0'..='9').contains(&ch)
						|| '_' == ch);
				let tt = match input[cur_idx..][..size].to_lowercase().as_str() {
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
					"pl" => PL,
					"pz" => PZ,
					"rotcl" => ROTCL,
					"rotcr" => ROTCR,
					"rotl" => ROTL,
					"rotr" => ROTR,
					"rte" => RTE,
					"rts" => RTS,
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
					"w" => Word,
					"xor" => XOR,
					"xtrct" => XTRCT,
					_ => Identifier,
				};
				results.push(Token::new(tt,cur_idx,size, line_idx, char_idx - size + 1));
			}
			_ => {
				let size = next_line(cur_idx, &mut chars, &mut char_idx);
				char_idx += size;
				results.push(Token::new(Unknown, cur_idx, size, line_idx, char_idx));
			}
		}
	}

	results
}


