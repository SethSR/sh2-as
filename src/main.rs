
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

	let results = lexer(&file);

	for result in results {
		let (idx,sz) = (result.index, result.size);
		let out = format!("{:?}\t'{}'", result.tt,
			&file[idx as usize..][..sz as usize]);
		if let TokenType::Unknown(ln,ch) = result.tt {
			println!("{out} ({ln},{ch})");
		} else if !is_silent {
			println!("{out}");
		}
	}

	todo!("output to '{target}'");
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
			'#' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Immediate, cur_idx, 1));
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
			'r' => {
				next(&mut char_idx, &mut chars);
				let size = tokenize(
					cur_idx, &mut chars, &mut char_idx,
					|ch| ('0'..='9').contains(&ch));
				results.push(Token::new(Register, cur_idx, size));
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

#[derive(Debug)]
enum TokenType {
	Comment,
	Org,
	Mov,
	Add,
	Plus,
	Dash,
	Register,
	Comma,
	Dot,
	Immediate,
	Number,
	Identifier,
	Colon,
	Address,
	Unknown(usize,usize),
}

