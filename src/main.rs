/*
use std::collections::HashMap;

use miette::IntoDiagnostic;

mod lexer;
use lexer::lexer;
use lexer::TokenType;

mod parser;
use parser::parser;
use parser::State;

mod resolver;
use resolver::resolver;

type Label = std::rc::Rc<str>;
type SectionMap = HashMap<u64, Vec<State>>;
type LabelMap = HashMap<Label, Option<u32>>;

fn main() -> miette::Result<()> {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let target = args.next().unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = true;

	let file = std::fs::read_to_string(source).into_diagnostic()?;

	let tokens = match lexer(&file) {
		Ok(tokens) => tokens,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	for token in &tokens {
		if token.get_type() == TokenType::IdUnknown || !is_silent {
			println!("{token:?}");
		}
	}

	let mut data = match parser(&tokens) {
		Ok(tables) => tables,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	if !is_silent {
		println!("State: initial");
		println!("{data:?}");
	}

	let limit = 10;
	for _ in 0..limit {
		if resolver(&mut data) {
			break;
		}
	}

	if !is_silent {
		println!("State: final");
		println!("{data:?}");
	}

	// TODO - srenshaw - This is just for debugging purposes. This is not the "real" output!
	for (_, section) in data.sections {
		let output = section
			.iter()
			.map(|state| state.completed_or(0xDEAD))
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output).into_diagnostic()?;
	}

	Ok(())
}
*/

use std::collections::HashMap;
use std::fs::read_to_string;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;

mod instructions;
use instructions::Ins;

mod arg;
use arg::Arg;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Size {
	Byte,
	Word,
	Long,
}

type Reg = u8;
type Label = std::rc::Rc<str>;

enum LabelType {
	Value(i32),
	Addr(u32),
}

#[derive(Parser)]
#[grammar = "sh2.pest"]
struct Sh2Parser;

fn reg(item: Pair<Rule>) -> Reg {
	item.as_str()[1..].parse::<u8>().unwrap()
}

fn reg_or_sp(item: Pair<Rule>) -> Reg {
	match item.as_rule() {
		Rule::reg => reg(item),
		Rule::r0 => 0,
		Rule::sp => 15,
		_ => unreachable!("unexpected token: {item}"),
	}
}

fn reg_post_inc(item: Pair<Rule>) -> Reg {
	if item.as_rule() == Rule::reg_post_inc {
		let s = item.as_str().to_lowercase();
		let s = &s[1..s.len()-1];
		if "sp" == s {
			15
		} else {
			s.parse::<Reg>().unwrap()
		}
	} else {
		unreachable!("expected @R<num>+")
	}
}

fn reg_pre_dec(item: Pair<Rule>) -> Reg {
	if item.as_rule() == Rule::reg_pre_dec {
		let s = &item.as_str().to_lowercase()[2..];
		if "sp" == s {
			15
		} else {
			s.parse::<Reg>().unwrap()
		}
	} else {
		unreachable!("expected @-R<num>")
	}
}

fn hex8u(item: Pair<Rule>) -> u8 {
	u8::from_str_radix(&item.as_str().replace('_',""), 16).unwrap()
}

fn hex16u(item: Pair<Rule>) -> u16 {
	u16::from_str_radix(&item.as_str().replace('_',""), 16).unwrap()
}

fn hex32u(item: Pair<Rule>) -> u32 {
	u32::from_str_radix(&item.as_str().replace('_',""), 16).unwrap()
}

fn hex8s(item: Pair<Rule>) -> i8 {
	hex8u(item) as i8
}

fn hex16s(item: Pair<Rule>) -> i16 {
	hex16u(item) as i16
}

fn hex32s(item: Pair<Rule>) -> i32 {
	hex32u(item) as i32
}

fn bin8u(item: Pair<Rule>) -> u8 {
	u8::from_str_radix(&item.as_str().replace('_',""), 2).unwrap()
}

fn bin16u(item: Pair<Rule>) -> u16 {
	u16::from_str_radix(&item.as_str().replace('_',""), 2).unwrap()
}

fn bin32u(item: Pair<Rule>) -> u32 {
	u32::from_str_radix(&item.as_str().replace('_',""), 2).unwrap()
}

fn bin8s(item: Pair<Rule>) -> i8 {
	bin8u(item) as i8
}

fn bin16s(item: Pair<Rule>) -> i16 {
	bin16u(item) as i16
}

fn bin32s(item: Pair<Rule>) -> i32 {
	bin32u(item) as i32
}

fn dec8s(item: Pair<Rule>) -> i8 {
	item.as_str().replace('_',"").parse::<i8>().unwrap()
}

fn dec16s(item: Pair<Rule>) -> i16 {
	item.as_str().replace('_',"").parse::<i16>().unwrap()
}

fn dec32s(item: Pair<Rule>) -> i32 {
	item.as_str().replace('_',"").parse::<i32>().unwrap()
}

fn num8u(item: Pair<Rule>) -> u8 {
	match item.as_rule() {
		Rule::hex => hex8u(item),
		Rule::bin => bin8u(item),
		Rule::dec => dec8s(item).try_into().unwrap(),
		_ => unreachable!("expected an 8-bit unsigned number"),
	}
}

fn num8s(item: Pair<Rule>) -> i8 {
	match item.as_rule() {
		Rule::hex => hex8s(item),
		Rule::bin => bin8s(item),
		Rule::dec => dec8s(item),
		_ => unreachable!("expected an 8-bit signed number"),
	}
}

fn num16s(item: Pair<Rule>) -> i16 {
	match item.as_rule() {
		Rule::hex => hex16s(item),
		Rule::bin => bin16s(item),
		Rule::dec => dec16s(item),
		_ => unreachable!("expected an 16-bit signed number"),
	}
}

fn num32s(item: Pair<Rule>) -> i32 {
	match item.as_rule() {
		Rule::hex => hex32s(item),
		Rule::bin => bin32s(item),
		Rule::dec => dec32s(item),
		_ => unreachable!("expected an 32-bit signed number"),
	}
}

fn reg_pair(mut args: Pairs<Rule>) -> (Reg,Reg) {
	let src = reg_or_sp(args.next().unwrap());
	let dst = reg_or_sp(args.next().unwrap());
	(src,dst)
}

fn disp_pc(item: Pair<Rule>) -> i8 {
	if item.as_rule() == Rule::disp_pc {
		item.as_str().parse::<i8>().unwrap()
	} else {
		unreachable!("expected @(disp,PC)")
	}
}

fn main() {
	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let input = read_to_string(&source).expect("unable to read source file");

	//let target = args.next().unwrap_or("asm.out".to_string());

	let mut repetitions = None;
	let mut skey = 0;

	let mut sections = HashMap::<u32, Vec<Ins>>::default();
	let mut labels = HashMap::<Label, LabelType>::default();

	match Sh2Parser::parse(Rule::program, &input) {
		Err(e) => eprintln!("{e}"),
		Ok(results) => for line in results {
			let ins = match line.as_rule() {
				Rule::dir_constant_b => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					match value.as_rule() {
						Rule::hex | Rule::bin | Rule::dec => {
							let num = num8s(value);
							sections.entry(skey)
								.or_insert(Vec::default())
								.push(Ins::Const_Imm(Size::Byte, num as i64));
							continue;
						}
						Rule::lbl => {
							// TODO - srenshaw - If this label is already defined, then if it's a value label,
							// use the defined value. If it's a address label, then use the address. If this
							// label is not defined, then save it as a Const_Label for resolving later.
							todo!("implement label constants");
						}
						_ => unreachable!("unexpected constant value: {value}"),
					}
				}
				Rule::dir_constant_w => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					match value.as_rule() {
						Rule::hex | Rule::bin | Rule::dec => {
							let num = num16s(value);
							sections.entry(skey)
								.or_insert(Vec::default())
								.push(Ins::Const_Imm(Size::Word, num as i64));
							continue;
						}
						Rule::lbl => {
							// TODO - srenshaw - If this label is already defined, then if it's a value label,
							// use the defined value. If it's a address label, then use the address. If this
							// label is not defined, then save it as a Const_Label for resolving later.
							todo!("implement label constants");
						}
						_ => unreachable!("unexpected constant value: {value}"),
					}
				}
				Rule::dir_constant_l => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					match value.as_rule() {
						Rule::hex | Rule::bin | Rule::dec => {
							let num = num32s(value);
							sections.entry(skey)
								.or_insert(Vec::default())
								.push(Ins::Const_Imm(Size::Long, num as i64));
							continue;
						}
						Rule::lbl => {
							// TODO - srenshaw - If this label is already defined, then if it's a value label,
							// use the defined value. If it's a address label, then use the address. If this
							// label is not defined, then save it as a Const_Label for resolving later.
							todo!("implement label constants");
						}
						_ => unreachable!("unexpected constant value: {value}"),
					}
				}
				Rule::dir_org => {
					let mut args = line.into_inner();
					skey = args.next().unwrap().as_str().parse::<u32>().unwrap();
					println!("set skey to {skey}");
					continue;
				}
				Rule::dir_repeat => {
					let mut args = line.into_inner();
					repetitions = Some((
						args.next().unwrap().as_str().parse::<u32>().unwrap(),
						Vec::with_capacity(64),
					));
					continue;
				}
				Rule::dir_endr => {
					if let Some((num, instrs)) = repetitions {
						let section = sections.entry(skey).or_insert(Vec::default());
						for _ in 0..num {
							section.extend_from_slice(&instrs);
						}
						repetitions = None;
					} else {
						eprintln!("`end repeat` found with no preceding `repeat` directive.");
					}
					continue;
				}
				Rule::dir_binclude => {
					// TODO - srenshaw - Enable this once we add actual error-handling.

					//let mut args = line.into_inner();
					//let file_path = args.next().unwrap().as_str();
					//let file = std::fs::read(file_path).expect("unable to open file");
					//println!("file size: {}", file.len());
					continue;
				}
				Rule::ins_add_imm => {
					let mut args = line.into_inner();
					let src = num8s(args.next().unwrap());
					let dst = reg(args.next().unwrap());
					Ins::AddImm(src,dst)
				}
				Rule::ins_add_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AddReg(src,dst)
				}
				Rule::ins_addc => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AddC(src,dst)
				}
				Rule::ins_addv => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AddV(src,dst)
				}
				Rule::ins_and_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::AndByte(src)
				}
				Rule::ins_and_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::AndImm(src)
				}
				Rule::ins_and_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AndReg(src,dst)
				}
				Rule::ins_bf => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::Bf(lbl.into())
				}
				Rule::ins_bfs => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::BfS(lbl.into())
				}
				Rule::ins_bra => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::Bra(lbl.into())
				}
				Rule::ins_braf => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::BraF(reg)
				}
				Rule::ins_bsr => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::Bsr(lbl.into())
				}
				Rule::ins_bsrf => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::BsrF(reg)
				}
				Rule::ins_bt => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::Bt(lbl.into())
				}
				Rule::ins_bts => {
					let mut args = line.into_inner();
					let lbl = args.next().unwrap().as_str();
					Ins::BtS(lbl.into())
				}
				Rule::ins_clrmac => Ins::ClrMac,
				Rule::ins_clrt => Ins::ClrT,
				Rule::ins_cmp_eq_imm => {
					let mut args = line.into_inner();
					let src = num8s(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as CMP/EQ dst");
					Ins::CmpEqImm(src)
				}
				Rule::ins_cmp_eq_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpEqReg(src,dst)
				}
				Rule::ins_cmp_ge => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpGE(src,dst)
				}
				Rule::ins_cmp_gt => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpGT(src,dst)
				}
				Rule::ins_cmp_hi => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpHI(src,dst)
				}
				Rule::ins_cmp_hs => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpHS(src,dst)
				}
				Rule::ins_cmp_pl => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					Ins::CmpPL(src)
				}
				Rule::ins_cmp_pz => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					Ins::CmpPZ(src)
				}
				Rule::ins_cmp_str => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::CmpStr(src,dst)
				}
				Rule::ins_div0s => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Div0S(src,dst)
				}
				Rule::ins_div0u => Ins::Div0U,
				Rule::ins_div1 => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Div1(src,dst)
				}
				Rule::ins_dmuls => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::DMulS(src,dst)
				}
				Rule::ins_dmulu => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::DMulU(src,dst)
				}
				Rule::ins_dt => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::Dt(reg)
				}
				Rule::ins_extsb => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::ExtS(Size::Byte,src,dst)
				}
				Rule::ins_extsw => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::ExtS(Size::Word,src,dst)
				}
				Rule::ins_extub => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::ExtU(Size::Byte,src,dst)
				}
				Rule::ins_extuw => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::ExtU(Size::Word,src,dst)
				}
				Rule::ins_jmp => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::Jmp(reg)
				}
				Rule::ins_jsr => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::Jsr(reg)
				}
				Rule::ins_ldc => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::gbr => Ins::LdcGBR(src),
						Rule::sr => Ins::LdcSR(src),
						Rule::vbr => Ins::LdcVBR(src),
						_ => unreachable!("expected GBR, SR, or VBR as LDC dst"),
					}
				}
				Rule::ins_ldc_inc => {
					let mut args = line.into_inner();
					let src = reg_post_inc(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::gbr => Ins::LdcGBR_Inc(src),
						Rule::sr => Ins::LdcSR_Inc(src),
						Rule::vbr => Ins::LdcVBR_Inc(src),
						_ => unreachable!("expected GBR, SR, or VBR as LDC.L dst"),
					}
				}
				Rule::ins_lds => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::macl => Ins::LdsMACL(src),
						Rule::mach => Ins::LdsMACH(src),
						Rule::pr => Ins::LdsPR(src),
						_ => unreachable!("expected GBR, SR, or VBR as LDC dst"),
					}
				}
				Rule::ins_lds_inc => {
					let mut args = line.into_inner();
					let src = reg_post_inc(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::macl => Ins::LdsMACL_Inc(src),
						Rule::mach => Ins::LdsMACH_Inc(src),
						Rule::pr => Ins::LdsPR_Inc(src),
						_ => unreachable!("expected GBR, SR, or VBR as LDC.L dst"),
					}
				}
				Rule::ins_macw => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::MacWord(src,dst)
				}
				Rule::ins_macl => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::MacLong(src,dst)
				}
				Rule::ins_mov => {
					println!("implement MOV instruction");
					continue
				}
				Rule::ins_mova => {
					let mut args = line.into_inner();
					let imm = disp_pc(args.next().unwrap());
					assert_eq!(0, reg(args.next().unwrap()));
					Ins::MovA(imm)
				}
				Rule::ins_movt => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::MovT(reg)
				}
				Rule::ins_mul => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Mul(src,dst)
				}
				Rule::ins_muls => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::MulS(src,dst)
				}
				Rule::ins_mulu => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::MulU(src,dst)
				}
				Rule::ins_neg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Neg(src,dst)
				}
				Rule::ins_negc => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::NegC(src,dst)
				}
				Rule::ins_nop => Ins::Nop,
				Rule::ins_not => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Not(src,dst)
				}
				Rule::ins_or => continue,
				Rule::ins_or_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::AndByte(src)
				}
				Rule::ins_or_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::AndImm(src)
				}
				Rule::ins_or_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AndReg(src,dst)
				}
				Rule::ins_rotcl => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::RotCL(reg)
				}
				Rule::ins_rotcr => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::RotCL(reg)
				}
				Rule::ins_rotl => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::RotL(reg)
				}
				Rule::ins_rotr => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::RotR(reg)
				}
				Rule::ins_rte => Ins::Rte,
				Rule::ins_rts => Ins::Rts,
				Rule::ins_sett => Ins::SetT,
				Rule::ins_shal => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShAL(reg)
				}
				Rule::ins_shar => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShAR(reg)
				}
				Rule::ins_shll => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLL(reg)
				}
				Rule::ins_shll16 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLL16(reg)
				}
				Rule::ins_shll2 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLL2(reg)
				}
				Rule::ins_shll8 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLL8(reg)
				}
				Rule::ins_shlr => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLR(reg)
				}
				Rule::ins_shlr16 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLR16(reg)
				}
				Rule::ins_shlr2 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLR2(reg)
				}
				Rule::ins_shlr8 => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::ShLR8(reg)
				}
				Rule::ins_sleep => Ins::Sleep,
				Rule::ins_stc => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::gbr => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StcGBR(reg)
						}
						Rule::sr => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StcSR(reg)
						}
						Rule::vbr => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StcVBR(reg)
						}
						_ => unreachable!("expected GBR, SR, or VBR for STC dst"),
					}
				}
				Rule::ins_stc_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::gbr => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StcGBR_Dec(reg)
						}
						Rule::sr => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StcSR_Dec(reg)
						}
						Rule::vbr => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StcVBR_Dec(reg)
						}
						_ => unreachable!("expected GBR, SR, or VBR for STC.L dst"),
					}
				}
				Rule::ins_sts => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::macl => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StsMACL(reg)
						}
						Rule::mach => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StsMACH(reg)
						}
						Rule::pr => {
							let reg = reg_or_sp(args.next().unwrap());
							Ins::StsPR(reg)
						}
						_ => unreachable!("expected MACL, MACH, PR for STS dst")
					}
				}
				Rule::ins_sts_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::macl => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StsMACL_Dec(reg)
						}
						Rule::mach => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StsMACH_Dec(reg)
						}
						Rule::pr => {
							let reg = reg_pre_dec(args.next().unwrap());
							Ins::StsPR_Dec(reg)
						}
						_ => unreachable!("expected MACL, MACH, or PR for STS.L dst"),
					}
				}
				Rule::ins_sub => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Sub(src,dst)
				}
				Rule::ins_subc => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::SubC(src,dst)
				}
				Rule::ins_subv => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::SubV(src,dst)
				}
				Rule::ins_swapb => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Swap(Size::Byte,src,dst)
				}
				Rule::ins_swapw => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Swap(Size::Word,src,dst)
				}
				Rule::ins_tas => {
					let mut args = line.into_inner();
					let reg = reg_or_sp(args.next().unwrap());
					Ins::Tas(reg)
				}
				Rule::ins_trapa => {
					let mut args = line.into_inner();
					let imm = num8u(args.next().unwrap());
					Ins::TrapA(imm)
				}
				Rule::ins_tst => continue,
				Rule::ins_tst_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::AndByte(src)
				}
				Rule::ins_tst_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::AndImm(src)
				}
				Rule::ins_tst_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AndReg(src,dst)
				}
				Rule::ins_xor => continue,
				Rule::ins_xor_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::AndByte(src)
				}
				Rule::ins_xor_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::AndImm(src)
				}
				Rule::ins_xor_reg => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::AndReg(src,dst)
				}
				Rule::ins_xtrct => {
					let (src,dst) = reg_pair(line.into_inner());
					Ins::Xtrct(src,dst)
				}
				Rule::val_line => {
					let mut args = line.into_inner();
					let label = args.next().unwrap().as_str();
					let value = args.next().unwrap().as_str();
					println!("Value: {label} = {value}");
					continue;
				}
				Rule::lbl_line => {
					let s = line.as_str();
					let label: Label = s[..s.len()-1].into();
					println!("Label: {label}");
					continue;
				}
				Rule::EOI => continue,
				_ => unreachable!("unexpected token found: {line}"),
			};
			println!("{ins:?}");
		}
	}
}

#[cfg(test)]
mod can_parse {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse($rule, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	fn org() { check!(Rule::dir_org, ".org $06004000") }

	#[test]
	fn add_reg() {
		check!(Rule::ins_add, "add r5,r12");
	}

	#[test]
	fn add_imm() {
		check!(Rule::ins_add, "add #30,r2");
		check!(Rule::ins_add, "add #$FE,r8");
		check!(Rule::ins_add, "add #%101,r8");
	}

	#[test]
	fn addc() { check!(Rule::ins_addc, "addc r3,r1") }

	#[test]
	fn addv() { check!(Rule::ins_addv, "addv r4,r12") }

	#[test]
	fn and() {
		check!(Rule::ins_and, "and r3,r14");
		check!(Rule::ins_and, "and #$2,r0");
		check!(Rule::ins_and, "and #%10,r0");
		check!(Rule::ins_and, "and #5,r0");
		check!(Rule::ins_and, "and.b #$32,@(r0,gbr)");
	}

	#[test]
	fn bf() { check!(Rule::ins_bf, "bf trget_f") }

	#[test]
	fn bfs() { check!(Rule::ins_bfs, "bf/s trget_f") }

	#[test]
	fn bra() { check!(Rule::ins_bra, "bra trget") }

	#[test]
	fn braf() { check!(Rule::ins_braf, "braf r4") }

	#[test]
	fn bsr() { check!(Rule::ins_bsr, "bsr trget") }

	#[test]
	fn bsrf() { check!(Rule::ins_bsrf, "bsrf r7") }

	#[test]
	fn bt() { check!(Rule::ins_bt, "bt TRGET") }

	#[test]
	fn bts() { check!(Rule::ins_bts, "bt/s TRGET") }

	#[test]
	fn clrmac() { check!(Rule::ins_clrmac, "clrmac") }

	#[test]
	fn clrt() { check!(Rule::ins_clrt, "clrt") }

	#[test]
	fn cmp() {
		check!(Rule::ins_cmp, "cmp/eq #$80,r0");
		check!(Rule::ins_cmp, "cmp/eq r4,r5");
		check!(Rule::ins_cmp, "cmp/ge r14,r15");
		check!(Rule::ins_cmp, "cmp/gt r10,r10");
		check!(Rule::ins_cmp, "cmp/hi sp,r8");
		check!(Rule::ins_cmp, "cmp/hs r7,r3");
		check!(Rule::ins_cmp, "cmp/pl r1");
		check!(Rule::ins_cmp, "cmp/pz r6");
		check!(Rule::ins_cmp, "cmp/str r6,r0");
	}

	#[test]
	fn div0s() { check!(Rule::ins_div0s, "div0s sp,sp") }

	#[test]
	fn div0u() { check!(Rule::ins_div0u, "div0u") }

	#[test]
	fn div1() { check!(Rule::ins_div1, "div1 r3,sp") }

	#[test]
	fn dmul() {
		check!(Rule::ins_dmuls, "dmuls r1,r3");
		check!(Rule::ins_dmuls, "dmuls.l r4,r7");
		check!(Rule::ins_dmulu, "dmulu r1,r3");
		check!(Rule::ins_dmulu, "dmulu.l r4,r7");
	}

	#[test]
	fn dt() { check!(Rule::ins_dt, "dt r14") }

	//#[test]
	//fn () { check!(Rule::ins_, "") }
}

#[cfg(test)]
mod examples {
	use super::*;

	macro_rules! check {
		($input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse(Rule::program, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	fn bsr() {
		check!("
	bsr TRGET
	MOV r3,r4
	add R0,R1
TRGET:
	mov r2,R3
	rts
	MOV #1,r0
");
	}

	#[test]
	fn bsrf() {
		check!("
	mov.l #(TRGET-BSRF_PC),r0
	bsrf r0
	mov r3,r4
BSRF_PC:
	add r0,r1
TRGET:
	mov r2,r3
	rts
	mov #1,r0
");
	}

	#[test]
	fn bt() {
		check!("
	sett
	bf TRGET_F
	bt TRGET_T
	nop
	nop
TRGET_T:
");
	}

	#[test]
	fn bts() {
		check!("
	sett
	bf/s trget_f
	nop
	bt/s trget_t
	add r0,r1
	nop
trget_t:
")
	}

	#[test]
	fn div_32_by_16_unsigned() {
		// R1 (32 bits) / R0 (16 bits) = R1 (16 bits) : Unsigned
		check!("
	shll16 r0    ; Upper 16 bits = divisor, lower 16 = 0
	tst r0,r0    ; Zero division check
	bt ZERO_DIV
	cmp/hs r0,r1 ; Overflow check
	bt OVER_DIV
	div0u        ; Flag initialization
	.arepeat 16
	div1 r0,r1   ; Repeat 16 times
	.aendr
	rotcl r1
	extu.w r1,r2 ; R1 = Quotient
")
	}

	#[test]
	fn div_64_by_32_unsigned() {
		// R1:R2 (64 bits) / R0 (32 bits) = R2 (32 bits) : Unsigned
		check!("
	tst r0,r0    ; Zero division check
	bt ZERO_DIV
	cmp/hs r0,r1 ; Overflow check
	bt OVER_DIV
	div0u        ; Flag initialization
	.arepeat 32
	rotcl r2     ; Repeat 32
	div1 r0,r1
	.aendr
	rotcl r2     ; R2 = Quotient
")
	}

	#[test]
	fn div_32_by_16_signed() {
		// R1 (32 bits) / R0 (16 bits) = R1 (16 bits) : Signed
		check!("
	SHLL16 R0    ; Upper 16 bits = divisor, lower 16 bits = 0
	EXTS.W R1,R1 ; Sign-extends the dividend to 32 bits
	XOR    R2,R2 ; R2 = 0
	MOV    R1,R2
	ROTCL  R3
	SUBC   R2,R1 ; Decrements if the dividend is negative
	DIV0S  R0,R1 ; Flag initialization
	.arepeat 16
	DIV1   R0,R1 ; Repeat 16 times
	.aendr
	EXTS.W R1,R1
	ROTCL  R1    ; R1 = quotient (1's complement)
	ADDC   R2,R1 ; Increments and takes the 2's complement if the MSB of the quotient is 1
	EXTS.W R1,R1 ; R1 = quotient (2's compliment)
")
	}

	#[test]
	fn div_64_by_32_signed() {
		// R2 (64 bits) / R0 (32 bits) = R2 (32 bits) : Signed
		check!("
	MOV   R2,R3
	ROTCL R3
	SUBC  R1,R1 ; Sign-extends the dividend to 64 bits (R1:R2)
	XOR   R3,R3 ; R3 = 0
	SUBC  R3,R2 ; Decrements and takes the 1's complement if the dividend is negative
	DIV0S R0,R1 ; Flag initialization
	.arepeat 32
	DIV1  R0,R1 ; Repeat 32 times
	.aendr
	ROTCL R2    ; R2 = Quotient (1's complement)
	ADDC  R3,R2 ; Increments and takes 2's complement if the MSB of the quotient is 1. R2 = Quotient (2's complement)
")
	}
}

#[cfg(test)]
mod invalid {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = crate::Sh2Parser::parse($rule, $input) {
				panic!("{e}");
			}
		}}
	}

	#[test]
	#[should_panic]
	fn add_reg_no_pc_src() { check!(Rule::ins_add, "add pc,r2") }

	#[test]
	#[should_panic]
	fn add_reg_no_pc_dst() { check!(Rule::ins_add, "add r3,pc") }

	#[test]
	#[should_panic]
	fn bsrf_no_pc() { check!(Rule::ins_bsrf, "bsrf pc") }
}

