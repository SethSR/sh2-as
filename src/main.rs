
use std::collections::{HashMap, HashSet};

use miette::{IntoDiagnostic, miette};

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

	if !is_silent {
		for (address, section) in &section_table {
			println!("Address: ${address:08X}");
			for instr in section {
				println!("\t{instr:?}");
			}
		}

		println!("Labels:");
		for label in &label_table {
			println!("\t{label}");
		}
	}

	let (section_table, label_table) = resolver(&section_table, &label_table);

	for (address, section) in &section_table {
		println!("Address: ${address:08X}");
		for instr in section {
			println!("  {instr:?}");
		}
	}

	println!("Labels:");
	for (label,address) in &label_table {
		println!("  {label}: {address:08X?}");
	}

	// TODO - srenshaw - This is just for debugging purposes. This is not the "real" output!
	for (_, section) in section_table {
		let output = section.iter()
			.map(|state| match state {
				State::Com(word) => *word,
				State::Inc(_) => 0xDEAD,
			})
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output)
			.into_diagnostic()?;
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
	DirImm(i64),
	DirReg(Reg),
	DispR0(Reg),
	DispReg(i8,Reg),
	DispPC(i8),
	DispGBR(i8),
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
			Arg::DispR0(reg) =>
				write!(fmt, "DispR0(R{reg})"),
			Arg::DispReg(disp,reg) =>
				write!(fmt, "DispReg({disp},R{reg})"),
			Arg::DispPC(disp) =>
				write!(fmt, "DispPC({disp})"),
			Arg::DispGBR(disp) =>
				write!(fmt, "DispGBR({disp})"),
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
enum Size {
	Byte,
	Word,
	Long,
}

#[derive(Debug)]
enum Ins {
	Add(Arg,Arg),
	Const(Size,Arg),
	BF(String),
	DT(Reg),
	Label(String),
	Mov(Size,Arg,Arg),
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

fn p_error(msg: &str) -> String {
	format!("ERROR: {msg}")
}

fn p_expected<'a,'b,'c>(
	file: &'a str,
	tok: &'b Token,
	msg: &'c str,
) -> String {
	let txt = p_str(file, tok);
	p_error(&format!("Expected {msg}, Found '{txt}'"))
}

fn p_number(
	file: &str,
	tok: &Token,
) -> miette::Result<i64> {
	let txt = p_str(file, tok);
	match txt.chars().next() {
		Some('%') => {
			i64::from_str_radix(&txt[1..].replace('_',""), 2)
		}
		Some('$') => {
			i64::from_str_radix(&txt[1..].replace('_',""), 16)
		}
		Some(c) if c.is_numeric() => {
			i64::from_str_radix(&txt.replace('_',""), 10)
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
			eprintln!("{}", p_expected(file, nxt_tok,
				"Register"));
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
		eprintln!("{}", p_expected(file, nxt_tok,
			"Address specifier (@r_/ @-r_ / @r_+)"));
		todo!("on error, skip to next newline");
	}
}

type ErrFlag = usize;
fn p_size(
	file: &str,
	tok_idx: &mut usize,
	tokens: &[Token],
) -> Result<Size, (ErrFlag,String)> {
	let nxt_tok = p_next(tok_idx, tokens);
	if nxt_tok.tt != TokenType::Dot {
		return Err((0,p_expected(file, nxt_tok, "'.'")));
	}

	let nxt_tok = p_next(tok_idx, tokens);
	match nxt_tok.tt {
		TokenType::Byte => Ok(Size::Byte),
		TokenType::Word => Ok(Size::Word),
		TokenType::Long => Ok(Size::Long),
		_ => Err((1,p_expected(file, nxt_tok, "size specifier"))),
	}
}

// TODO - srenshaw - Ensure the parser actually returns what it's supposed to.

/// Parses strings of tokens into valid instructions & directives
///
/// Given a sequence of valid tokens, the parser should return either a section and label table for
/// the analysis stage, or a sequence of all errors encountered while parsing the input.
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
				let nxt_tok = p_next(&mut tok_idx, &tokens);
				let src = match nxt_tok.tt {
					Dash => {
						let nxt_tok = p_next(&mut tok_idx, &tokens);
						if nxt_tok.tt == Number {
							let num = p_number(&file, nxt_tok)?;
							Arg::DirImm(-num)
						} else {
							p_expected(&file, nxt_tok,
								"Number after minus sign");
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
						p_expected(&file, nxt_tok,
							"A valid ADD source argument");
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
						p_expected(&file, nxt_tok,
							"A valid ADD destination argument");
						todo!("on error, skip to next newline");
					}
				};

				section_table
					.entry(skey)
					.or_default()
					.push(Ins::Add(src,dst));
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
							"integer literal or label");
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
					if label_table.contains(&label) {
						eprintln!("{}", p_error("Label '{label}' already defined"));
						todo!("on error, skip to newline");
					}

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
						Size::Word
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
				skey = p_number(&file, nxt_tok)? as u64;
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

enum State {
	/// Instruction completed for output
	Com(u16),
	/// Instruction / directive still waiting on label resolution
	Inc(Ins),
}

impl std::fmt::Debug for State {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Com(inst) => write!(fmt, "Com(${inst:04X})"),
			Self::Inc(inst) => write!(fmt, "Inc({inst:?})"),
		}
	}
}

fn to_byte2(reg: &usize) -> u16 {
	(reg << 12) as u16
}

fn to_byte3(reg: &usize) -> u16 {
	(reg << 8) as u16
}

fn to_sbyte(size: &Size) -> u16 {
	match size {
		Size::Byte => 0b00,
		Size::Word => 0b01,
		Size::Long => 0b10,
	}
}

fn resolver(
	section_table: &SectionTable,
	label_table: &LabelTable,
) -> (
	HashMap<u64, Vec<State>>,
	HashMap<String, Option<u32>>,
) {
	let mut label_table = label_table
		.into_iter()
		.map(|label| (label.clone(),None))
		.collect::<HashMap<String, Option<u32>>>();

	let mut new_section_table = HashMap::<u64,Vec<State>>::new();
	for (&section_start, section) in section_table {
		let mut results = Vec::with_capacity(section.len());
		for instr in section {
			use Arg::*;
			use Ins::*;
			use Size::*;
			use State::*;
			match instr {
				Add(src,dst) => {}
				Const(Byte,DirImm(_)) => {
					eprintln!("Attempting to declare a byte constant. This doesn't work currently.");
				}
				Const(Word,DirImm(value)) => {
					results.push(Com(*value as u16));
				}
				Const(Long,DirImm(value)) => {
					results.push(Com((value >> 16) as u16));
					results.push(Com(*value as u16));
				}
				BF(label) => {}
				DT(reg) => {}
				Ins::Label(label) => {
					if !label_table.contains_key(label) {
						todo!("Unknown label '{label}'");
					}
					if let Some(addr) = label_table[label] {
						todo!("Label '{label}' already defined to {addr:08X}");
					}
					label_table.insert(label.clone(),
						Some(section_start as u32 + results.len() as u32 * 2));
				}
				Mov(Long,Arg::Label(lsrc),DirReg(rdst)) => {
					if !label_table.contains_key(lsrc) {
						todo!("Unknown label '{lsrc}'");
					}
					if let Some(lbl_addr) = label_table[lsrc] {
						let cur_addr = section_start as u32 + results.len() as u32 * 2;
						let offset = lbl_addr as i64 - cur_addr as i64;
						let disp = offset / 4;
						if disp <= i8::MIN as i64 || disp >= i8::MAX as i64 {
							todo!("Relative address too big! Switch to memory load and move?");
						}
						let disp = disp as i8;
						results.push(Inc(Mov(Long,DispPC(disp),DirReg(*rdst))));
					}
				}
				Mov(Byte,DirImm(isrc),DirReg(rdst)) |
				Mov(Word,DirImm(isrc),DirReg(rdst)) |
				Mov(Long,DirImm(isrc),DirReg(rdst)) => {
					let base = 0b1110_0000_0000_0000;
					let nbyte = to_byte2(rdst);
					let imm = *isrc as i64;
					if imm < i16::MIN as i64 || imm > i16::MAX as i64 {
						// 32-bit immediate
						eprintln!("Attempting to load a 32-bit value. This doesn't work currently.");
					} else if imm < i8::MIN as i64 || imm > i8::MAX as i64 {
						// 16-bit immediate
						eprintln!("Attempting to load a 16-bit value. This doesn't work currently.");
					} else {
						// 8-bit immediate
						let iword = (isrc & 0xFF) as u16;
						results.push(Com(base | nbyte | iword));
					}
				}
				Mov(Word,DispPC(disp),DirReg(rdst)) => {
					let base = 0b1001_0000_00000000;
					let nbyte = to_byte2(rdst);
					let dword = *disp as u8 as u16;
					results.push(Com(base | nbyte | dword));
				}
				Mov(Long,DispPC(disp),DirReg(rdst)) => {
					let base = 0b1101_0000_00000000;
					let nbyte = to_byte2(rdst);
					let dword = *disp as u8 as u16;
					results.push(Com(base | nbyte | dword));
				}
				Mov(Byte,DirReg(rsrc),DirReg(rdst)) |
				Mov(Word,DirReg(rsrc),DirReg(rdst)) |
				Mov(Long,DirReg(rsrc),DirReg(rdst)) => {
					let base = 0b0110_0000_0000_0011;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					results.push(Com(base | nbyte | mbyte));
				}
				Mov(size,DirReg(rsrc),IndReg(rdst)) => {
					let base = 0b0010_0000_0000_0000;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size,IndReg(rsrc),DirReg(rdst)) => {
					let base = 0b0110_0000_0000_0000;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size,DirReg(rsrc),PreDec(rdst)) => {
					let base = 0b0010_0000_0000_0100;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size,PostInc(rsrc),DirReg(rdst)) => {
					let base = 0b0110_0000_0000_0100;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size @ Byte,DirReg(0),DispReg(disp,rdst)) |
				Mov(size @ Word,DirReg(0),DispReg(disp,rdst)) => {
					let base = 0b10000000_0000_0000;
					let sbyte = to_sbyte(size) << 8;
					let nbyte = to_byte3(rdst);
					let dbyte = (*disp as u8 as u16) & 0x0F;
					results.push(Com(base | sbyte | nbyte | dbyte));
				}
				Mov(Long,DirReg(rsrc),DispReg(disp,rdst)) => {
					let base = 0b0001_0000_0000_0000;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let dbyte = (*disp as u8 as u16) & 0x0F;
					results.push(Com(base | nbyte | mbyte | dbyte));
				}
				Mov(size @ Byte,DispReg(disp,rsrc),DirReg(0)) |
				Mov(size @ Word,DispReg(disp,rsrc),DirReg(0)) => {
					let base = 0b10000100_0000_0000;
					let sbyte = to_sbyte(size) << 8;
					let mbyte = to_byte3(rsrc);
					let dbyte = (*disp as u8 as u16) & 0x0F;
					results.push(Com(base | sbyte | mbyte | dbyte));
				}
				Mov(Long,DispReg(disp,rsrc),DirReg(rdst)) => {
					let base = 0b0101_0000_0000_0000;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let dbyte = (*disp as u8 as u16) & 0x0F;
					results.push(Com(base | nbyte | mbyte | dbyte));
				}
				Mov(size,DirReg(rsrc),DispR0(rdst)) => {
					let base = 0b0000_0000_0000_0100;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size,DispR0(rsrc),DirReg(rdst)) => {
					let base = 0b0000_0000_0000_1100;
					let nbyte = to_byte2(rdst);
					let mbyte = to_byte3(rsrc);
					let sbyte = to_sbyte(size);
					results.push(Com(base | nbyte | mbyte | sbyte));
				}
				Mov(size,DirReg(0),DispGBR(disp)) => {
					let base = 0b11000000_00000000;
					let sbyte = to_sbyte(size) << 8;
					let dword = *disp as u8 as u16;
					results.push(Com(base | sbyte | dword));
				}
				Mov(size,DispGBR(disp),DirReg(0)) => {
					let base = 0b11000100_00000000;
					let sbyte = to_sbyte(size) << 8;
					let dword = *disp as u8 as u16;
					results.push(Com(base | sbyte | dword));
				}
				_ => todo!("Invalid instruction: {instr:?}"),
			}
		}
		new_section_table.insert(section_start, results);
	}

	(new_section_table, label_table)
}

