
use miette::{IntoDiagnostic, miette};

fn main() -> miette::Result<()> {
	let mut args = std::env::args();
	args.next();

	let source = args.next()
		.expect("missing source file");
	let target = args.next()
		.unwrap_or("asm.out".to_string());

	let file = std::fs::read_to_string(source)
		.into_diagnostic()?;

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

	fn next_line(
		cur_idx: usize,
		chars: &mut impl Iterator<Item = (usize,char)>,
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

	let mut results = Vec::new();
	let mut line_idx = 0;
	let mut char_idx = 0;
	let mut chars = file.char_indices().peekable();

	fn next(
		idx: &mut usize,
		chars: &mut impl Iterator<Item=(usize,char)>,
	) {
		*idx += 1;
		chars.next();
	}

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
			'#' => {
				next(&mut char_idx, &mut chars);
				results.push(Token::new(Immediate, cur_idx, 1));
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
			'a' => {
				next(&mut char_idx, &mut chars);
				let Some((_,'d')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					char_idx += size;
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				let Some(&(idx,'d')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					char_idx += size;
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				results.push(Token::new(
					Add, cur_idx, idx - cur_idx + 1));
			}
			'm' => {
				next(&mut char_idx, &mut chars);
				let Some((_,'o')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				let Some((_,'v')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				let Some((_,'.')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				match chars.peek() {
					Some(&(idx,'b')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(
							MovB, cur_idx, idx - cur_idx + 1));
					}
					Some(&(idx,'w')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(
							MovW, cur_idx, idx - cur_idx + 1));
					}
					Some(&(idx,'l')) => {
						next(&mut char_idx, &mut chars);
						results.push(Token::new(
							MovL, cur_idx, idx - cur_idx + 1));
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
			'o' => {
				next(&mut char_idx, &mut chars);
				let Some((_,'r')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					char_idx += size;
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				let Some(&(idx,'g')) = chars.peek() else {
					let size = next_line(
						cur_idx, &mut chars,
						&mut char_idx, &mut line_idx);
					char_idx += size;
					results.push(Token::new(
						Unknown(line_idx,char_idx), cur_idx, size));
					break;
				};
				next(&mut char_idx, &mut chars);
				results.push(Token::new(
					Org, cur_idx, idx - cur_idx + 1));
			}
			';' => {
				let size = next_line(
					cur_idx, &mut chars,
					&mut char_idx, &mut line_idx);
				char_idx += size;
				results.push(Token::new(Comment, cur_idx, size));
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

	for result in results {
		print!("{:?}", result.tt);
		let (idx,sz) = (result.index, result.size);
		println!("\t'{}'", &file[idx as usize..][..sz as usize]);
	}

	Ok(())
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
	MovB,
	MovW,
	MovL,
	Add,
	Plus,
	Dash,
	Register,
	Comma,
	Immediate,
	Number,
	Address,
	Unknown(usize,usize),
}

