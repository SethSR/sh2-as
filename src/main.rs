
use std::fs::read_to_string;

use tracing::{
	warn,
	instrument,
};

mod instructions;

mod arg;
use arg::Arg;

mod parser;
use parser::{parser, LabelMap, Output};

mod unroller;
use unroller::unroller;

//mod resolver;
//use resolver::resolver;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Size {
	Byte,
	Word,
	Long,
}

type Reg = u8;
type Label = std::rc::Rc<str>;
#[instrument]
fn main() {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let input = read_to_string(&source).expect("unable to read source file");

	//let target = args.next().unwrap_or("asm.out".to_string());

	let Output { assembly, mut labels } = parser(&input);
/*
	println!("values: {}", values.len());
	for (name, value) in &values {
		println!("  {name} = {value}");
	}
*/
	println!("labels: {}", labels.len());
	for (name, addr) in &labels {
		println!("  {name} : {addr:0X?}");
	}
/*
	println!("assembly: {}", assembly.len());
	for (i, ins) in assembly.iter().enumerate() {
		println!("${i:04X}: {ins:0X?}");
	}
*/

	let out = unroller(assembly);

	let internal_label_count = 0;
	let (out, internal_label_count) = displacer(out, internal_label_count, &mut labels);
	println!("{out:?}");
	println!("{internal_label_count}");

	//let out = resolver(assembly, labels);
	//println!("{out:?}");
}

use instructions::{Asm, Dir, Ins};
use parser::LabelType;
fn displacer(
	assembly: Vec<Asm>,
	mut temp_label_count: usize,
	labels: &mut LabelMap,
) -> (Vec<Asm>, usize) {
	let mut out = vec![];

	enum Data {
		Word(Label, i16),
		Long(Label, i32),
	}

	let mut temp_labels = Vec::<Data>::default();

	let mut temp_label = || {
		temp_label_count += 1;
		format!("____{}", temp_label_count - 1)
	};

	for asm in assembly {
		match asm {
			asm @ Asm::Dir(_) => out.push(asm.clone()),

			Asm::Ins(ins) => match ins {
				Ins::Mov_Imm_Word(imm, dst) => {
					let label: Label = temp_label().into();
					temp_labels.push(Data::Word(label.clone(), imm));
					labels.insert(label.clone(), LabelType::Unknown);
					out.push(Ins::Mov(Size::Word, Arg::Label(label), Arg::DirReg(dst)).into());
				}
				Ins::Mov_Imm_Long(imm, dst) => {
					let label: Label = temp_label().into();
					temp_labels.push(Data::Long(label.clone(), imm));
					labels.insert(label.clone(), LabelType::Unknown);
					out.push(Ins::Mov(Size::Long, Arg::Label(label), Arg::DirReg(dst)).into());
				}

				Ins::Jmp(_) |
				Ins::Bra(_) |
				Ins::Rts |
				Ins::Rte => {
					out.push(ins.clone().into());
					for data in temp_labels.drain(..) {
						match data {
							Data::Word(label, imm) => {
								out.push(Dir::Label(label).into());
								out.push(Dir::ConstImmWord(imm as u16).into());
							}
							Data::Long(label, imm) => {
								out.push(Dir::Label(label).into());
								out.push(Dir::ConstImmLong(imm as u32).into());
							}
						}
					}
				}

				ins => out.push(ins.clone().into()),
			}
		}
	}

	(out, temp_label_count)
}

#[test]
#[should_panic]
fn mov_w_doesnt_parse_32_bit_immediates() {
	let input = "
	MOV.W #$40402020,R3
";
	let out = parser(input);
	assert_eq!(out.assembly, vec![
		Asm::Ins(Ins::Mov_Imm_Long(0x40402020, 3)),
	]);
}

#[test]
fn test_displacer() {
	let input = "
	MOV.W #$4321,R4
	MOV.L #$10203040,R5
	BSR BOGUS
	RTS
";
	let Output { assembly, mut labels } = parser(input);
	let (out, label_count) = displacer(assembly, 0, &mut labels);
	assert_eq!(label_count, 2);
	assert_eq!(out, vec![
		Asm::Ins(Ins::Mov(Size::Word, Arg::Label("____0".into()), Arg::DirReg(4))),
		Asm::Ins(Ins::Mov(Size::Long, Arg::Label("____1".into()), Arg::DirReg(5))),
		Asm::Ins(Ins::Bsr("BOGUS".into())),
		Asm::Ins(Ins::Rts),
		Asm::Dir(Dir::Label("____0".into())),
		Asm::Dir(Dir::ConstImmWord(0x4321)),
		Asm::Dir(Dir::Label("____1".into())),
		Asm::Dir(Dir::ConstImmLong(0x10203040)),
	]);
}
