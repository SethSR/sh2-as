
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

	fn next_line(
		chars: &mut impl Iterator<Item = (usize,char)>,
		cur_idx: usize,
		line_idx: &mut usize,
		char_idx: &mut usize,
	) -> (usize,usize) {
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
		(cur_idx, (index - cur_idx))
	}

	let mut results = Vec::new();
	let mut line_idx = 0;
	let mut char_idx = 0;
	let mut chars = file.char_indices().peekable();

	use TokenType::*;
	while let Some((cur_idx, cur_char)) = chars.next() {
		char_idx += 1;

		match cur_char {
			' ' | '\t' => {}
			'\n' => {
				line_idx += 1;
				char_idx = 0;
			}
			',' => results.push(Token::new(Comma, cur_idx, 1)),
			'@' => results.push(Token::new(Address, cur_idx, 1)),
			'#' => results.push(Token::new(Immediate, cur_idx, 1)),
			'$' => {
				let mut index = cur_idx + 1;
				while let Some((idx,ch)) = chars.peek() {
					char_idx += 1;
					index = *idx;
					if !('0'..='9').contains(ch)
					&& !('a'..='f').contains(ch)
					&& !('A'..='F').contains(ch)
					&& '_' != *ch
					{
						break;
					}
					chars.next();
				}
				results.push(Token::new(Number, cur_idx, index - cur_idx));
			}
			'%' => {
				let mut index = cur_idx + 1;
				while let Some((idx,ch)) = chars.peek() {
					char_idx += 1;
					index = *idx;
					if !['0', '1'].contains(ch) && '_' != *ch {
						break;
					}
					chars.next();
				}
				results.push(Token::new(Number, cur_idx, index - cur_idx));
			}
			'r' => {
				let mut index = cur_idx + 1;
				while let Some((idx,ch)) = chars.peek() {
					char_idx += 1;
					index = *idx;
					if !('0'..='9').contains(ch) {
						break;
					}
					chars.next();
				}
				results.push(Token::new(Register, cur_idx, index - cur_idx));
			}
			'a' => {
				let Some((_,'d')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				let Some((idx,'d')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				results.push(Token::new(Add, cur_idx, idx - cur_idx + 1));
			}
			'm' => {
				let Some((_,'o')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				let Some((_,'v')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				let Some((_,'.')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				match chars.next() {
					Some((idx,'b')) => results.push(Token::new(MovB, cur_idx, idx - cur_idx + 1)),
					Some((idx,'w')) => results.push(Token::new(MovW, cur_idx, idx - cur_idx + 1)),
					Some((idx,'l')) => results.push(Token::new(MovL, cur_idx, idx - cur_idx + 1)),
					_ => {
						let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
						results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					}
				}
			}
			'o' => {
				let Some((_,'r')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				let Some((idx,'g')) = chars.next() else {
					let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
					results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
					break;
				};
				results.push(Token::new(Org, cur_idx, idx - cur_idx + 1));
			}
			';' => {
				let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
				results.push(Token::new(Comment, idx, size));
			}
			_ => {
				let (idx,size) = next_line(&mut chars, cur_idx, &mut line_idx, &mut char_idx);
				results.push(Token::new(Unknown(line_idx,char_idx), idx, size));
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
	Register,
	Comma,
	Immediate,
	Number,
	Address,
	Unknown(usize,usize),
}

