
use tracing::error;

use crate::instructions::{Asm, Dir};

pub fn unroller(assembly: Vec<Asm>) -> Vec<Asm> {
	let mut rep_count = None;
	let mut rep_asm = vec![];
	let mut out = Vec::with_capacity(assembly.len() + 128);

	for asm in assembly {
		match asm {
			Asm::Dir(dir) => match dir {
				Dir::Repeat(num) => {
					if rep_count.is_some() {
						error!("found nested '.repeat' directive, ignoring");
					} else {
						rep_count = Some(num);
					}
				}
				Dir::EndRepeat => match rep_count {
					Some(count) => {
						for _ in 0..count {
							out.extend(rep_asm.iter().cloned());
						}
						rep_count = None;
					}
					None => error!("'.endr' directive with no preceding '.repeat'"),
				}
				dir @ Dir::Label(_) |
				dir @ Dir::Org(_) |
				dir @ Dir::Include(_) |
				dir @ Dir::BinInclude(_) => {
					if rep_count.is_some() {
						error!("{dir:?} within '.repeat' directive, ignoring");
					}
				}
				dir @ Dir::ConstImmByte(_) |
				dir @ Dir::ConstImmWord(_) |
				dir @ Dir::ConstImmLong(_) |
				dir @ Dir::ConstLabelLong(_) |
				dir @ Dir::Align(_) => {
					if rep_count.is_some() {
						rep_asm.push(dir.into());
					} else {
						out.push(dir.into());
					}
				}
			}
			Asm::Ins(ins) => if rep_count.is_some() {
				rep_asm.push(ins.into());
			} else {
				out.push(ins.into());
			}
		}
	}

	out
}

#[test]
fn repeat_directive_unrolls_instructions() {
	use crate::instructions::Ins;
	use crate::parser;

	let input = "
	.repeat 2
	SLEEP
	DIV0U
	.endr
";
	let out = parser(input);
	let out = unroller(out.assembly);
	assert_eq!(out, vec![
		Asm::Ins(Ins::Sleep),
		Asm::Ins(Ins::Div0U),
		Asm::Ins(Ins::Sleep),
		Asm::Ins(Ins::Div0U),
	]);
}

