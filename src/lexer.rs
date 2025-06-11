
use miette::LabeledSpan;

use crate::tokens::{Token, Type};

pub(crate) struct Lexer<'a> {
	source: &'a str,
	rest: &'a str,
	index: usize,
}

impl<'a> Lexer<'a> {
	pub(crate) fn new(input: &'a str) -> Self {
		Self {
			source: input,
			rest: input,
			index: 0,
		}
	}

	fn next(&mut self, index: usize) {
		self.index += index;
		self.rest = &self.rest[index..];
	}
}

impl Iterator for Lexer<'_> {
	type Item = Result<Token, miette::Error>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			let mut chars = self.rest.chars();
			let c = chars.next()?;
			let c_at = self.index;
			self.next(c.len_utf8());

			let output = |tt| Some(Ok(Token::new(tt, c_at)));

			match c {
				'$' => break output(Type::Dollar),
				'%' => break output(Type::Percent),
				'(' => break output(Type::OParen),
				')' => break output(Type::CParen),
				'*' => break output(Type::Star),
				'+' => break output(Type::Plus),
				',' => break output(Type::Comma),
				'.' => break output(Type::Dot),
				':' => break output(Type::Colon),
				'@' => break output(Type::At),
				'=' => break output(Type::Eq),
				'/' => break output(Type::Slash),
				'-' => break output(Type::Dash),

				';' => { // Comment
					let mut inner_chars = self.rest.char_indices();
					let mut index = 0;
					loop {
						if let Some((j,x)) = inner_chars.next() {
							index = j;
							if x != '\n' {
								continue;
							}
						} else {
							index += self.rest.len() - index;
						}
						self.next(index);
						break;
					}
				}

				'"' => { // String
					let mut inner_chars = self.rest.char_indices();
					let mut index;
					loop {
						if let Some((j,x)) = inner_chars.next() {
							index = j;
							if x != '"' {
								continue;
							}
						} else {
							index = self.rest.len();
						}
						self.next(index);
						break;
					}

					let s = &self.source[c_at..self.index];
					break output(Type::String(s.into()));
				}

				'a'..='z' | 'A'..='Z' | '_' => {
					let mut inner_chars = self.rest.char_indices();
					let mut index;
					loop {
						if let Some((j,x)) = inner_chars.next() {
							index = j;
							if x.is_ascii_lowercase()
							|| x.is_ascii_uppercase()
							|| x.is_ascii_digit()
							|| x == '_'
							|| x == '/'
							{
								continue;
							}
						} else {
							index = self.rest.len();
						}
						break self.next(index);
					}

					let ident = &self.source[c_at..self.index];
					break output(match ident {
						"org"      => Type::Org,
						"include"  => Type::Include,
						"binclude" => Type::BInclude,
						"align"    => Type::Align,
						"dc"       => Type::Const,
						"ds"       => Type::Space,
						"ltorg"    => Type::LtOrg,
						"add"      => Type::Add,
						"addc"     => Type::AddC,
						"addv"     => Type::AddV,
						"and"      => Type::And,
						"b"        => Type::Byte,
						"bf"       => Type::Bf,
						"bf/s"     => Type::BfS,
						"bra"      => Type::Bra,
						"braf"     => Type::BraF,
						"bsr"      => Type::Bsr,
						"bsrf"     => Type::BsrF,
						"bt"       => Type::Bt,
						"bt/s"     => Type::BtS,
						"clrmac"   => Type::ClrMac,
						"clrt"     => Type::ClrT,
						"cmp/eq"   => Type::CmpEq,
						"cmp/ge"   => Type::CmpGe,
						"cmp/gt"   => Type::CmpGt,
						"cmp/hi"   => Type::CmpHi,
						"cmp/hs"   => Type::CmpHs,
						"cmp/pl"   => Type::CmpPl,
						"cmp/pz"   => Type::CmpPz,
						"cmp/str"  => Type::CmpStr,
						"div0s"    => Type::Div0S,
						"div0u"    => Type::Div0U,
						"div1"     => Type::Div1,
						"dmuls"    => Type::DMulS,
						"dmulu"    => Type::DMulU,
						"dt"       => Type::Dt,
						"exts"     => Type::ExtS,
						"extu"     => Type::ExtU,
						"jmp"      => Type::Jmp,
						"jsr"      => Type::Jsr,
						"l"        => Type::Long,
						"ldc"      => Type::LdC,
						"lds"      => Type::LdS,
						"mac"      => Type::Mac,
						"mov"      => Type::Mov,
						"mova"     => Type::MovA,
						"movt"     => Type::MovT,
						"mul"      => Type::Mul,
						"muls"     => Type::MulS,
						"mulu"     => Type::MulU,
						"neg"      => Type::Neg,
						"negc"     => Type::NegC,
						"nop"      => Type::Nop,
						"not"      => Type::Not,
						"or"       => Type::Or,
						"rotcl"    => Type::RotCL,
						"rotcr"    => Type::RotCR,
						"rotl"     => Type::RotL,
						"rotr"     => Type::RotR,
						"rte"      => Type::Rte,
						"rts"      => Type::Rts,
						"sett"     => Type::SetT,
						"shal"     => Type::ShAL,
						"shar"     => Type::ShAR,
						"shll"     => Type::ShLL,
						"shll2"    => Type::ShLL2,
						"shll8"    => Type::ShLL8,
						"shll16"   => Type::ShLL16,
						"shlr"     => Type::ShLR,
						"shlr2"    => Type::ShLR2,
						"shlr8"    => Type::ShLR8,
						"shlr16"   => Type::ShLR16,
						"sleep"    => Type::Sleep,
						"stc"      => Type::StC,
						"sts"      => Type::StS,
						"sub"      => Type::Sub,
						"subc"     => Type::SubC,
						"subv"     => Type::SubV,
						"swap"     => Type::Swap,
						"tas"      => Type::Tas,
						"trapa"    => Type::TrapA,
						"tst"      => Type::Tst,
						"w"        => Type::Word,
						"xor"      => Type::Xor,
						"xtrct"    => Type::Xtrct,

						s => if ident.contains('/') {
							todo!("handle Labels with slashes '/' in them")
						} else {
							Type::Label(s.into())
						}
					});
				}

				'0'..='9' => {
					let mut inner_chars = self.rest.char_indices();
					let mut index;
					loop {
						if let Some((j,x)) = inner_chars.next() {
							index = j;
							if x.is_ascii_digit() || x == '_' {
								continue;
							}
						} else {
							index = self.rest.len();
						}
						self.next(index);
						break;
					}

					let n = &self.source[c_at..self.index];
					break output(Type::Imm(n.into()));
				}

				w if w.is_whitespace() => {
					let mut inner_chars = self.rest.char_indices();
					loop {
						if let Some((j,x)) = inner_chars.next() {
							if x.is_whitespace() {
								continue;
							}
							self.next(j);
						}
						break;
					}
				}

				_ => break Some(Err(miette::miette! {
					labels = vec![
						LabeledSpan::at(
							c_at..self.index,
							"this character",
						),
					],
					"Unexpected char '{c}'"
				}.with_source_code(self.source.to_string()))),
			}
		}
	}
}

pub(crate) fn eval(
	input: &str,
) -> miette::Result<Vec<Token>> {
	use std::iter::once;

	Lexer::new(input)
		.chain(once(Ok(Token::new(Type::Eof, input.len()))))
		.collect()
}

#[cfg(test)]
mod tokenizes {
	use super::Type;

	fn lex_test(
		input: &str,
		check: &[Type],
	) -> miette::Result<()> {
		let mut check = check.to_vec();
		check.push(Type::Eof);

		let tokens = crate::lexer::eval(input)?;
		assert_eq!(tokens, check);
		Ok(())
	}

	#[test]
	fn empty_input() -> miette::Result<()> {
		lex_test("", &[])
	}

	#[test]
	fn white_spaces_as_empty_input() -> miette::Result<()> {
		lex_test("  \n\t  ", &[])
	}

	#[test]
	fn comments_as_empty_input() -> miette::Result<()> {
		lex_test("; var if hello", &[])
	}

	/*
	#[test]
	fn keywords() -> miette::Result<()> {
		lex_test("add addc addv and bf bra braf bsr bsrf bt", &[
			Type::Var,
			Type::Fun,
			Type::Rec,
			Type::If,
			Type::While,
			Type::Else,
			Type::True,
			Type::False,
		])
	}
	*/

	#[test]
	fn labels() -> miette::Result<()> {
		lex_test("abc_123 _123", &[
			Type::Label("abc_123".into()),
			Type::Label("_123".into()),
		])
	}

	#[test]
	fn numbers() -> miette::Result<()> {
		lex_test("1 5_6 ", &[
			Type::Imm("1".into()),
			Type::Imm("5_6".into()),
		])
	}
}
