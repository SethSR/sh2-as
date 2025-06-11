
use std::collections::HashSet;

use crate::asm::Asm;

#[derive(Debug)]
enum State {
	Line,

	Immediate,

	Comment(usize),

	Id(usize),

	Label(usize),

	Bin(usize),

	Dec(usize),

	Hex(usize),
}

#[derive(Debug)]
enum Token {
	Comment(usize, usize),
	Ins(usize, usize),
	Dir(usize, usize),
	Label(usize, usize),
	Bin(usize, usize),
	Dec(usize, usize),
	Hex(usize, usize),
}

struct Lexer {
	line: u32,
	col: u32,

	index: usize,
	state: State,

	out: Vec<Token>,
}

impl Default for Lexer {
	fn default() -> Self {
		Self {
			line: 1,
			col: 0,
			index: 0,
			state: State::Line,
			out: vec![],
		}
	}
}

impl Lexer {
	fn next_line(&mut self) {
		self.line += 1;
		self.col = 1;
	}

	fn add_token(
		&mut self,
		state: State,
		start: usize,
		f: impl Fn(usize, usize) -> Token,
	) {
		self.out.push(f(start, self.index));
		self.state = state;
	}

	fn unexpected(&self, msg: &str) -> String {
		format!("Unexpected '{msg}' @ ({}:{}) in {:?}",
			self.line, self.col, self.state)
	}

	fn unknown(&self, item: &str) -> String {
		format!("Unknown IDENTIFIER '{item}' @ ({}:{}) in {:?}",
			self.line, self.col, self.state)
	}
}

fn lexer(input: &str) -> Result<Vec<Token>, String> {
	let directives = {
		let mut s = HashSet::<String>::default();
		s.insert("org".to_string());
		s.insert("include".to_string());
		s.insert("binclude".to_string());
		s.insert("align".to_string());
		s.insert("dc.b".to_string());
		s.insert("dc.w".to_string());
		s.insert("dc.l".to_string());
		s.insert("ds.b".to_string());
		s.insert("ds.w".to_string());
		s.insert("ds.l".to_string());
		s
	};

	let instructions = {
		let mut s = HashSet::<String>::default();
		s.insert("clrmac".to_string());
		s.insert("clrt".to_string());
		s.insert("div0u".to_string());
		s.insert("nop".to_string());
		s.insert("rte".to_string());
		s.insert("rts".to_string());
		s.insert("sett".to_string());
		s.insert("sleep".to_string());

		s.insert("bf".to_string());
		s.insert("bfs".to_string());
		s.insert("bra".to_string());
		s.insert("braf".to_string());
		s.insert("bsr".to_string());
		s.insert("bsrf".to_string());
		s.insert("bt".to_string());
		s.insert("bts".to_string());
		s.insert("dt".to_string());
		s.insert("jmp".to_string());
		s.insert("jsr".to_string());
		s.insert("mova".to_string());
		s.insert("movt".to_string());
		s.insert("rotcl".to_string());
		s.insert("rotcr".to_string());
		s.insert("rotl".to_string());
		s.insert("rotr".to_string());
		s.insert("shal".to_string());
		s.insert("shar".to_string());
		s.insert("shll".to_string());
		s.insert("shll2".to_string());
		s.insert("shll8".to_string());
		s.insert("shll16".to_string());
		s.insert("shlr".to_string());
		s.insert("shlr2".to_string());
		s.insert("shlr8".to_string());
		s.insert("shlr16".to_string());
		s.insert("tas".to_string());
		s.insert("trapa".to_string());

		s.insert("addc".to_string());
		s.insert("addv".to_string());
		s.insert("div0s".to_string());
		s.insert("div1".to_string());
		s.insert("exts.b".to_string());
		s.insert("exts.w".to_string());
		s.insert("extu.b".to_string());
		s.insert("extu.w".to_string());
		s.insert("mac.w".to_string());
		s.insert("mac.l".to_string());
		s.insert("neg".to_string());
		s.insert("negc".to_string());
		s.insert("not".to_string());
		s.insert("sub".to_string());
		s.insert("subc".to_string());
		s.insert("subv".to_string());
		s.insert("swap.b".to_string());
		s.insert("swap.w".to_string());
		s.insert("xtrct".to_string());

		s.insert("and".to_string());
		s.insert("and.b".to_string());
		s.insert("or".to_string());
		s.insert("or.b".to_string());
		s.insert("tst".to_string());
		s.insert("tst.b".to_string());
		s.insert("xor".to_string());
		s.insert("xor.b".to_string());

		s.insert("cmp/eq".to_string());
		s.insert("cmp/ge".to_string());
		s.insert("cmp/gt".to_string());
		s.insert("cmp/hi".to_string());
		s.insert("cmp/hs".to_string());
		s.insert("cmp/str".to_string());
		s.insert("cmp/pl".to_string());
		s.insert("cmp/pz".to_string());

		s.insert("add".to_string());
		s.insert("mul".to_string());
		s.insert("muls".to_string());
		s.insert("mulu".to_string());
		s.insert("ldc".to_string());
		s.insert("ldc.l".to_string());
		s.insert("lds".to_string());
		s.insert("lds.l".to_string());

		s.insert("stc".to_string());
		s.insert("stc.l".to_string());
		s.insert("sts".to_string());
		s.insert("sts.l".to_string());

		s.insert("dmuls".to_string());
		s.insert("dmulu".to_string());

		s.insert("mov".to_string());
		s.insert("mov.b".to_string());
		s.insert("mov.w".to_string());
		s.insert("mov.l".to_string());
		s
	};

	let mut lxr = Lexer::default();

	for (idx, ch) in input.char_indices() {
		lxr.index = idx;
		lxr.col += 1;

		match lxr.state {
			State::Line => match ch {
				' ' => {}
				'\n' => {
					lxr.next_line();
				}
				';' => {
					lxr.state = State::Comment(lxr.index);
				}
				'a'..='z' | 'A'..='Z' => {
					lxr.state = State::Id(lxr.index);
				}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Comment(start) => if ch == '\n' {
				lxr.next_line();
				lxr.add_token(State::Line, start, Token::Comment);
			}

			State::Id(start) => match ch {
				'\n' => {
					lxr.next_line();

					let item = &input[start..lxr.index];
					let token = if directives.contains(item) {
						Token::Dir(start, lxr.index)
					} else if instructions.contains(item) {
						Token::Ins(start, lxr.index)
					} else {
						Token::Id(start, lxr.index)
					};

					lxr.out.push(token);
					lxr.state = State::Line;
				}
				' ' => {
					let item = &input[start..lxr.index];
					let (token, state) = if directives.contains(item) {
						(Token::Dir(start, lxr.index), State::Dir)
					} else if instructions.contains(item) {
						(Token::Ins(start, lxr.index), State::Ins1)
					} else {
						(Token::Id(start, lxr.index), State::Label)
					};

					lxr.out.push(token);
					lxr.state = state;
				}
				'#' => {
					let item = &input[start..lxr.index];
					let (token, state) = if directives.contains(item) {
						(Token::Dir(start, lxr.index), State::Immediate)
					} else if instructions.contains(item) {
						(Token::Ins(start, lxr.index), State::Immediate)
					} else {
						return Err(lxr.unknown(item));
					};

					lxr.out.push(token);
					lxr.state = state;
				}
				':' => {
					lxr.out.push(Token::Label(start, lxr.index));
					lxr.state = State::Line;
				}
				'=' => {
					lxr.out.push(Token::Label(start, lxr.index));
					lxr.state = State::Immediate;
				}
				'a'..='z' | 'A'..='Z' => {}
				'0'..='9' => {}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Immediate => match ch {
				'\n' => {
					return Err(lxr.unexpected("\\n"));
				}
				' ' => {
					return Err(lxr.unexpected("SPACE"));
				}
				';' => {
					return Err(lxr.unexpected(";"));
				}
				'%' => {
					lxr.state = State::Bin(lxr.index);
				}
				'0'..='9' => {
					lxr.state = State::Dec(lxr.index);
				}
				'$' => {
					lxr.state = State::Hex(lxr.index);
				}
				'a'..='z' | 'A'..='Z' => {
					lxr.state = State::Label(lxr.index);
				}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Ins1 => match ch {
				'#' => lxr.state = State::Immediate,
				'@' => todo!("address-type"),
				'r' => todo!("register"),
				',' => {
					lxr.state = State::Ins2,
				}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Ins2 => match ch {
				'#' => lxr.state = State::Immediate,
				'@' => todo!("address-type"),
				'r' => todo!("register"),
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Label(start) => match ch {
				'\n' => {
					lxr.next_line();
					lxr.add_token(State::Line, start, Token::Label);
				}
				' ' => {
					lxr.add_token(State::Line, start, Token::Label);
				}
				';' => {
					lxr.add_token(State::Comment(lxr.index), start, Token::Label);
				}
				',' => {
					lxr.add_token(State::Line, start, Token::Label);
				}
				'a'..='z' | 'A'..='Z' => {}
				'0'..='9' => {}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Bin(start) => match ch {
				'\n' => {
					lxr.next_line();
					lxr.add_token(State::Line, start, Token::Bin);
				}
				' ' => {
					lxr.add_token(State::Line, start, Token::Bin);
				}
				';' => {
					lxr.add_token(State::Comment(lxr.index), start, Token::Bin);
				}
				',' => {
					lxr.add_token(State::Line, start, Token::Bin);
				}
				'0'..='9' => {}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Dec(start) => match ch {
				'\n' => {
					lxr.next_line();
					lxr.add_token(State::Line, start, Token::Dec);
				}
				' ' => {
					lxr.add_token(State::Line, start, Token::Dec);
				}
				';' => {
					lxr.add_token(State::Comment(lxr.index), start, Token::Dec);
				}
				',' => {
					lxr.add_token(State::Line, start, Token::Dec);
				}
				'0'..='9' => {}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}

			State::Hex(start) => match ch {
				'\n' => {
					lxr.next_line();
					lxr.add_token(State::Line, start, Token::Hex);
				}
				' ' => {
					lxr.add_token(State::Line, start, Token::Hex);
				}
				';' => {
					lxr.add_token(State::Comment(lxr.index), start, Token::Hex);
				}
				',' => {
					lxr.add_token(State::Line, start, Token::Hex);
				}
				'0'..='9' => {}
				_ => return Err(lxr.unexpected(&String::from(ch))),
			}
		}
	}

	Ok(lxr.out)
}

pub fn parser(input: &str) -> Result<Vec<Asm>, String> {
	let tokens = lexer(input)?;

	println!("{tokens:?}");

	for token in tokens {
		match token {
			Token::Comment(a,b) => {
				println!("Cmt: '{}'", &input[a..b])
			}
			Token::Ins(a,b) => {
				println!("Ins: '{}'", &input[a..b])
			}
			Token::Dir(a,b) => {
				println!("Dir: '{}'", &input[a..b])
			}
			Token::Label(a,b) => {
				println!("Lbl: '{}'", &input[a..b])
			}
			Token::Bin(a,b) => {
				println!("Bin: '{}'", &input[a..b])
			}
			Token::Dec(a,b) => {
				println!("Dec: '{}'", &input[a..b])
			}
			Token::Hex(a,b) => {
				println!("Hex: '{}'", &input[a..b])
			}
		}
	}

	Ok(vec![])
}
