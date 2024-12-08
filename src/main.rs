
use std::collections::{HashMap, HashSet};

use miette::{IntoDiagnostic, miette};

const SIZE_BYTE: usize = 1;
const SIZE_WORD: usize = 2;
const SIZE_LONG: usize = 4;

fn main() -> miette::Result<()> {
	let mut args = std::env::args();
	args.next();

	let source = args.next()
		.expect("missing source file");
	let target = args.next()
		.unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = true;

	let file = std::fs::read_to_string(source)
		.into_diagnostic()?;

	let tokens = lexer(&file);

	for token in &tokens {
		let txt = p_str(&file, &token);
		let out = format!("{:?}\t'{txt}'", token.tt);
		if let TokenType::Unknown(ln,ch) = token.tt {
			println!("{out} ({ln},{ch})");
		} else if !is_silent {
			println!("{out}");
		}
	}

	let (section_table, label_table) = parser(&file, &tokens)?;

	for (address, section) in section_table {
		println!("Address: ${address:08X}");
		for instr in section {
			println!("\t{instr:?}");
		}
	}

	println!("Labels:");
	for label in label_table {
		println!("\t{label}");
	}

	todo!("output to '{target}'");
}

/* Lexer */

#[derive(Debug)]
struct Token {
	tt: TokenType,
	index: u16,
	size: u8,
}

impl Token {
	fn new(tt: TokenType, index: usize, size: usize) -> Self {
		Self { tt, index: index as u16, size: size as u8 }
	}
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
enum TokenType {
	Add,
	Address,
	BF,
	BT,
	Byte,
	Colon,
	Comma,
	Comment,
	Const,
	Dash,
	DT,
	Dot,
	Identifier,
	Long,
	Mov,
	Newline,
	Number,
	Org,
	Plus,
	Register,
	Word,
	Unknown(usize,usize),
}

fn lexer(input: &str) -> Vec<Token> {
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
		chars: &mut I,
		char_idx: &mut usize,
		line_idx: &mut usize,
	) -> usize {
		let mut index = cur_idx + 1;
		while let Some((cmt_idx,cmt_char)) = chars.next() {
			*char_idx += 1;
			index = cmt_idx;
			if cmt_char == '\n' {
				*line_idx += 1;
				*char_idx = 0;
				break;
			}
		}
		index - cur_idx
	}

	fn ident<I: Iterator<Item=(usize,char)>>(
		id: &str,
		cur_idx: usize,
		chars: &mut std::iter::Peekable<I>,
		char_idx: &mut usize,
		line_idx: &mut usize,
	) -> Result<usize,usize> {
		let mut idx = cur_idx;
		for ch in id.chars() {
			match chars.peek() {
				Some(&(i,c)) if c == ch => {
					idx = i;
					next(char_idx, chars);
				}
				_ => {
					let size = next_line(
						cur_idx, chars, char_idx, line_idx);
					*char_idx += size;
					return Err(size);
				}
			}
		}
		Ok(idx - cur_idx + 1)
	}

	let mut results = Vec::new();
	let mut line_idx = 0;
	let mut char_idx = 0;
	let mut chars = input.char_indices().peekable();

	use TokenType::*;
	while let Some(&(cur_idx, cur_char)) = chars.peek() {
		match cur_char {
			' ' | '\t' => next(&mut char_idx, &mut chars),
			'\n' => {
				line_idx += 1;
				char_idx = 0;
				chars.next();
				results.push(Token::new(Newline, cur_idx, 1));
			}
			',' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Comma, cur_idx, 1));
			}
			'+' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Plus, cur_idx, 1));
			}
			'-' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dash, cur_idx, 1));
			}
			'@' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Address, cur_idx, 1));
			}
			':' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Colon, cur_idx, 1));
			}
			'.' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Dot, cur_idx, 1));
			}
			'0'..='9' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch) || '_' == ch);
				results.push(Token::new(Number, cur_idx, size));
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
				results.push(Token::new(Number, cur_idx, size));
			}
			'%' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ['0','1'].contains(&ch) || '_' == ch);
				results.push(Token::new(Number, cur_idx, size));
			}
			'a' => match ident("add",
				cur_idx, &mut chars,
				&mut char_idx, &mut line_idx,
			) {
				Ok(sz) => results.push(Token::new(
					Add, cur_idx, sz)),
				Err(sz) => results.push(Token::new(
					Unknown(line_idx,char_idx), cur_idx, sz)),
			}
			'b' => {
				next(&mut char_idx, &mut chars);
				match chars.peek() {
					Some(&(_,'f')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(BF, cur_idx, 2));
					}
					Some(&(_,'t')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(BT, cur_idx, 2));
					}
					_ => results.push(Token::new(Byte, cur_idx, 1)),
				}
			}
			'd' => {
				next(&mut char_idx, &mut chars);
				match chars.peek() {
					Some(&(_,'c')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(Const, cur_idx, 2));
					}
					Some(&(_,'t')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(DT, cur_idx, 2));
					}
					_ => results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, 1)),
				}
			}
			'l' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Long, cur_idx, 1));
			}
			'm' => {
				match ident("mov",
					cur_idx, &mut chars,
					&mut char_idx, &mut line_idx,
				) {
					Ok(sz) => results.push(Token::new(
						Mov, cur_idx, sz)),
					Err(sz) => results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, sz)),
				}
			}
			'o' => match ident("org",
				cur_idx, &mut chars,
				&mut char_idx, &mut line_idx)
			{
				Ok(idx) => results.push(Token::new(
					Org, cur_idx, idx)),
				Err(sz) => results.push(Token::new(
					Unknown(line_idx,char_idx), cur_idx, sz)),
			}
			'r' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch));
				results.push(Token::new(Register, cur_idx, size));
			}
			'w' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Word, cur_idx, 1));
			}
			';' => {
				let size = next_line(
					cur_idx, &mut chars,
					&mut char_idx, &mut line_idx);
				char_idx += size;
				results.push(Token::new(Comment, cur_idx, size));
			}
			c if c.is_alphabetic() => {
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('a'..='z').contains(&ch)
						|| ('A'..='Z').contains(&ch)
						|| '_' == ch);
				results.push(Token::new(Identifier,cur_idx,size));
			}
			_ => {
				let size = next_line(
					cur_idx, &mut chars,
					&mut char_idx, &mut line_idx);
				char_idx += size;
				results.push(Token::new(
					Unknown(line_idx,char_idx), cur_idx, size));
			}
		}
	}

	results
}

/* Parser */

type Addr = u32;
type Reg = usize;

enum Arg {
	DirImm(u64),
	DirReg(Reg),
	IndImm(Addr),
	IndReg(Reg),
	Label(String),
	PostInc(Reg),
	PreDec(Reg),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self,
		fmt: &mut std::fmt::Formatter,
	) -> std::fmt::Result {
		match self {
			Arg::DirImm(imm) =>
				write!(fmt, "DirImm(${imm:08X})"),
			Arg::DirReg(reg) =>
				write!(fmt, "DirReg(R{reg})"),
			Arg::IndImm(addr) =>
				write!(fmt, "IndImm(${addr:08X})"),
			Arg::IndReg(reg) =>
				write!(fmt, "IndReg(R{reg})"),
			Arg::Label(lbl) =>
				write!(fmt, "Label({lbl})"),
			Arg::PostInc(reg) =>
				write!(fmt, "PostInc(R{reg})"),
			Arg::PreDec(reg) =>
				write!(fmt, "PreDec(R{reg})"),
		}
	}
}

#[derive(Debug)]
enum Ins {
	Add(usize,Arg,Arg),
	Const(usize,Arg),
	BF(String),
	DT(Reg),
	Label(String),
	Mov(usize,Arg,Arg),
}

type SectionTable = HashMap<u64, Vec<Ins>>;
type LabelTable = HashSet<String>;

fn p_str(
	file: &str,
	tok: &Token,
) -> String {
	let (idx,sz) = (tok.index, tok.size);
	file[idx as usize..][..sz as usize].to_owned()
}

fn p_expected<'a,'b,'c>(
	file: &'a str,
	tok: &'b Token,
	msg: &'c str,
) -> String {
	let txt = p_str(file, tok);
	format!("ERROR: Expected {msg}, Found '{txt}'")
}

fn p_number(
	file: &str,
	tok: &Token,
) -> miette::Result<u64> {
	let txt = p_str(file, tok);
	match txt.chars().next() {
		Some('%') => {
			u64::from_str_radix(&txt[1..].replace('_',""), 2)
		}
		Some('$') => {
			u64::from_str_radix(&txt[1..].replace('_',""), 16)
		}
		Some(c) if c.is_numeric() => {
			u64::from_str_radix(&txt.replace('_',""), 10)
		}
		_ => unreachable!("number tokens should only have valid binary, decimal, or hexadecimal values"),
	}.into_diagnostic()
}

fn p_reg(file: &str, tok: &Token) -> miette::Result<usize> {
	let txt = p_str(file, tok);
	usize::from_str_radix(&txt[1..], 10)
		.into_diagnostic()
}

fn p_next<'a>(
	tok_idx: &'_ mut usize,
	tokens: &'a [Token],
) -> &'a Token {
	*tok_idx += 1;
	&tokens[*tok_idx]
}

fn p_address(
	file: &str,
	tok_idx: &mut usize,
	tokens: &[Token],
) -> miette::Result<Arg> {
	let nxt_tok = p_next(tok_idx, tokens);
	if nxt_tok.tt == TokenType::Dash {
		let nxt_tok = p_next(tok_idx, tokens);
		if nxt_tok.tt == TokenType::Register {
			let reg = p_reg(file, nxt_tok)?;
			Ok(Arg::PreDec(reg))
		} else {
			eprintln!("{}", p_expected(file, nxt_tok, "Register"));
			todo!("on error, skip to next newline");
		}
	} else if nxt_tok.tt == TokenType::Register {
		let reg = p_reg(file, nxt_tok)?;

		let nxt_tok = p_next(tok_idx, tokens);
		if nxt_tok.tt == TokenType::Plus {
			Ok(Arg::PostInc(reg))
		} else {
			*tok_idx -= 1;
			Ok(Arg::IndReg(reg))
		}
	} else {
		eprintln!("{}", p_expected(file, nxt_tok, "Address specifier (@r_/ @-r_ / @r_+)"));
		todo!("on error, skip to next newline");
	}
}

fn p_size(
	file: &str,
	tok_idx: &mut usize,
	tokens: &[Token],
) -> Result<usize, (usize,String)> {
	let nxt_tok = p_next(tok_idx, tokens);
	if nxt_tok.tt != TokenType::Dot {
		return Err((0,p_expected(file, nxt_tok, "'.'")));
	}

	let nxt_tok = p_next(tok_idx, tokens);
	match nxt_tok.tt {
		TokenType::Byte => Ok(SIZE_BYTE),
		TokenType::Word => Ok(SIZE_WORD),
		TokenType::Long => Ok(SIZE_LONG),
		_ => Err((1,p_expected(file, nxt_tok, "size specifier"))),
	}
}

fn parser(
	file: &str,
	tokens: &[Token],
) -> miette::Result<(SectionTable, LabelTable)> {
	let mut skey = 0;
	let mut section_table = SectionTable::new();
	let mut label_table = LabelTable::new();

	let mut tok_idx = 0;
	while tok_idx < tokens.len() {
		use TokenType::*;
		let cur_tok = &tokens[tok_idx];
		match cur_tok.tt {
			Add => {
				let size = match p_size(&file, &mut tok_idx, &tokens) {
					Ok(size) => size,
					Err((n,msg)) => if n == 0 {
						tok_idx -= 1;
						SIZE_WORD
					} else {
						eprintln!("{msg}");
						todo!("on error, skip to next newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let src = match nxt_tok.tt {
					Dash => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt == Number {
							let num = p_number(&file, nxt_tok)?;
							Arg::DirImm((!num).wrapping_add(1))
						} else {
							p_expected(&file, nxt_tok, "Number after minus sign");
							todo!("on error, skip to next newline character");
						}
					}
					Number => {
						let imm = p_number(&file, nxt_tok)?;
						Arg::DirImm(imm)
					}
					Address => {
						p_address(&file, &mut tok_idx, &tokens)?
					}
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					_ => {
						p_expected(&file, nxt_tok, "A valid ADD source argument");
						todo!("on error, skip to next newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.tt != Comma {
					p_expected(&file, nxt_tok, "','");
					todo!("on error, skip to next newline");
				}

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let dst = match nxt_tok.tt {
					Address => {
						p_address(&file, &mut tok_idx, &tokens)?
					}
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					_ => {
						p_expected(&file, nxt_tok, "A valid ADD destination argument");
						todo!("on error, skip to next newline");
					}
				};

				section_table
					.entry(skey)
					.or_default()
					.push(Ins::Add(size,src,dst));
			}
			Address => {
				eprintln!("unexpected Address");
			}
			BF => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.tt != Identifier {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"Label"));
					todo!("on error, skip to newline");
				}

				let txt = p_str(&file, &nxt_tok);
				section_table
					.entry(skey)
					.or_default()
					.push(Ins::BF(txt));
			}
			BT => {
				eprintln!("unexpected BT");
			}
			Byte => {
				eprintln!("unexpected size specifier");
			}
			Colon => {
				eprintln!("unexpected Colon");
			}
			Comma => {
				eprintln!("unexpected Comma");
			}
			Comment => {} // skip comments
			Const => {
				let Ok(sz) = p_size(&file, &mut tok_idx, &tokens) else {
					todo!("on error, skip to next newline character");
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let value = match nxt_tok.tt {
					Number => {
						let imm = p_number(&file, nxt_tok)?;
						Arg::DirImm(imm)
					}
					Identifier => {
						let txt = p_str(&file, nxt_tok);
						Arg::Label(txt)
					}
					_ => {
						p_expected(&file, nxt_tok,
							"address literal or label");
						todo!("on error, skip to next newline character");
					}
				};

				// TODO - srenshaw - ensure `value` and `size` match.
				section_table
					.entry(skey)
					.or_default()
					.push(Ins::Const(sz,value));
			}
			Dash => {
				eprintln!("unexpected Dash");
			}
			DT => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.tt != Register {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"Register"));
					todo!("on error, skip to newline");
				}

				let reg = p_reg(&file, nxt_tok)?;
				section_table
					.entry(skey)
					.or_default()
					.push(Ins::DT(reg));
			}
			Dot => {
				eprintln!("unexpected Dot");
			}
			Identifier => {
				let label = p_str(&file, &cur_tok);

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.tt == Colon {
					section_table
						.entry(skey)
						.or_default()
						.push(Ins::Label(label.clone()));
					label_table.insert(label);
				} else {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"End of label declaration ':'"));
					todo!("on error, skip to newline");
				}
			}
			Long => {
				eprintln!("unexpected size specifier");
			}
			Mov => {
				let size = match p_size(&file, &mut tok_idx, &tokens) {
					Ok(size) => size,
					Err((n,msg)) => if n == 0 {
						tok_idx -= 1;
						SIZE_WORD
					} else {
						eprintln!("{msg}");
						todo!("on error, skip to newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let src = match nxt_tok.tt {
					Identifier => {
						let txt = p_str(&file, &nxt_tok);
						Arg::Label(txt)
					}
					Number => {
						let imm = p_number(&file, nxt_tok)?;
						Arg::DirImm(imm)
					}
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					Address => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt != Register {
							eprintln!("{}", p_expected(&file, nxt_tok,
								"A valid MOV source argument"));
							todo!("on error, skip to newline");
						}

						let reg = p_reg(&file, nxt_tok)?;
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt == Plus {
							Arg::PostInc(reg)
						} else {
							tok_idx -= 1;
							Arg::IndReg(reg)
						}
					}
					_ => {
						eprintln!("{}", p_expected(&file, nxt_tok,
							"A valid MOV source argument"));
						todo!("on error, skip to newline");
					}
				};

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				if nxt_tok.tt != Comma {
					eprintln!("{}", p_expected(&file, nxt_tok,
						"','"));
					todo!("on error, skip to newline");
				}

				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let dst = match nxt_tok.tt {
					Register => {
						let reg = p_reg(&file, nxt_tok)?;
						Arg::DirReg(reg)
					}
					Address => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						let is_pre_dec = nxt_tok.tt == Dash;
						let nxt_tok = if is_pre_dec {
							p_next(&mut tok_idx, &tokens)
						} else {
							nxt_tok
						};

						if nxt_tok.tt != Register {
							eprintln!("{}", p_expected(&file, nxt_tok,
								"Register"));
							todo!("on error, skip to newline");
						}

						let reg = p_reg(&file, nxt_tok)?;
						if is_pre_dec {
							Arg::PreDec(reg)
						} else {
							Arg::IndReg(reg)
						}
					}
					_ => {
						eprintln!("{}", p_expected(&file, nxt_tok,
							"A valid MOV destination argument"));
						todo!("on error, skip to newline");
					}
				};

				section_table
					.entry(skey)
					.or_default()
					.push(Ins::Mov(size,src,dst));
			}
			Newline => {} // skip newlines
			Number => {
				eprintln!("unexpected Number");
			}
			Org => {
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				skey = p_number(&file, nxt_tok)?;
			}
			Plus => {
				eprintln!("unexpected Plus");
			}
			Register => {
				eprintln!("unexpected Register");
			}
			Word => {
				eprintln!("unexpected size specifier");
			}
			Unknown(ln,ch) => {
				let txt = p_str(&file, &cur_tok);
				eprintln!("unknown item @ line {ln}, char {ch}: '{txt}'");
			}
		}
		tok_idx += 1;
	}

	Ok((section_table, label_table))
}

