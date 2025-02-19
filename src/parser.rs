
use std::collections::HashMap;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;

use tracing::error;

use crate::{Arg, Reg, Size};

use crate::instructions::{Asm, Dir, Ins};

type Label = std::rc::Rc<str>;

#[derive(Parser)]
#[grammar = "sh2.pest"]
struct Sh2Parser;

pub type LabelMap = HashMap::<Label, LabelType>;

#[derive(Debug)]
pub enum LabelType {
	Unknown,
	Label(u32),
	Value(u32),
}

pub struct Output {
	pub assembly: Vec<Asm>,
	pub labels: LabelMap,
}

fn reg(item: Pair<Rule>) -> Reg {
	item.as_str()[1..].parse::<u8>().unwrap()
}

fn reg_or_sp(item: Pair<Rule>) -> Reg {
	match item.as_rule() {
		Rule::reg => reg(item),
		Rule::r0 => 0,
		Rule::sp => 15,
		_ => {
			error!("expected Reg or SP, found: {item}-{}", item.as_str());
			unreachable!("expected Reg or SP, found: {item}-{}", item.as_str());
		}
	}
}

fn addr_reg_or_sp(item: Pair<Rule>) -> Reg {
	if item.as_rule() == Rule::addr_reg_or_sp {
		let s = item.as_str().to_lowercase();
		let s = &s[1..];
		if "sp" == s {
			15
		} else {
			s[1..].parse::<Reg>().unwrap()
		}
	} else {
		unreachable!("expected @Reg, found: {item}-{}", item.as_str())
	}
}

fn reg_post_inc(item: Pair<Rule>) -> Reg {
	if item.as_rule() == Rule::reg_post_inc {
		let s = item.as_str().to_lowercase();
		let s = &s[1..s.len()-1];
		if "sp" == s {
			15
		} else {
			s[1..].parse::<Reg>().unwrap()
		}
	} else {
		unreachable!("expected @Reg+, found: {item}-{}", item.as_str())
	}
}

fn reg_pre_dec(item: Pair<Rule>) -> Reg {
	if item.as_rule() == Rule::reg_pre_dec {
		let s = &item.as_str().to_lowercase()[2..];
		if "sp" == s {
			15
		} else {
			s[1..].parse::<Reg>().unwrap()
		}
	} else {
		unreachable!("expected @-Reg, found: {item}-{}", item.as_str())
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

fn disp_gbr(item: Pair<Rule>) -> Arg {
	if item.as_rule() == Rule::disp_gbr {
		let disp = item.as_str().parse::<i8>().unwrap();
		Arg::DispGBR(disp)
	} else {
		unreachable!("expected @(disp,GBR)")
	}
}

fn disp_pc(item: Pair<Rule>) -> Arg {
	if item.as_rule() == Rule::disp_pc {
		let disp = item.as_str().parse::<i8>().unwrap();
		Arg::DispPC(disp)
	} else {
		unreachable!("expected @(disp,PC)")
	}
}

fn disp_reg(item: Pair<Rule>) -> Arg {
	if item.as_rule() != Rule::disp_reg {
		unreachable!("expected @(disp,Reg)")
	}

	let mut args = item.into_inner();
	let disp = args.next().unwrap();
	let reg = reg_or_sp(args.next().unwrap());
	match disp.as_rule() {
		Rule::lbl => Arg::DispLabel(disp.as_str().into(),reg),
		Rule::hex | Rule::bin | Rule::dec => {
			let disp = num32s(disp);
			if (i8::MIN as i32..i8::MAX as i32).contains(&disp) {
				Arg::DispRegByte(disp as i8,reg)
			} else if (i16::MIN as i32..i16::MAX as i32).contains(&disp) {
				Arg::DispRegWord(disp as i16,reg)
			} else {
				Arg::DispRegLong(disp,reg)
			}
		}
		_ => unreachable!("expected @(disp,Reg), found: {disp}-{}", disp.as_str()),
	}
}

fn disp_r0(item: Pair<Rule>) -> Arg {
	if item.as_rule() == Rule::disp_r0 {
		let mut args = item.into_inner();
		let disp = reg_or_sp(args.next().unwrap());
		assert_eq!(0, reg(args.next().unwrap()));
		Arg::DispR0(disp)
	} else {
		unreachable!("expected @(disp,R0)")
	}
}

fn reg2_inst(line: Pair<Rule>, out_fn: fn (Reg,Reg) -> Ins) -> Ins {
	let (src,dst) = reg_pair(line.into_inner());
	out_fn(src,dst)
}

fn size_reg_inst(line: Pair<Rule>, size: Size, out_fn: fn (Size,Reg,Reg) -> Ins) -> Ins {
	let (src,dst) = reg_pair(line.into_inner());
	out_fn(size,src,dst)
}


fn label_inst(line: Pair<Rule>, out_fn: fn(Label) -> Ins) -> Ins {
	let mut args = line.into_inner();
	let lbl = args.next().unwrap().as_str();
	out_fn(lbl.into())
}

fn reg_inst(line: Pair<Rule>, out_fn: fn(Reg) -> Ins) -> Ins {
	let mut args = line.into_inner();
	let reg = reg_or_sp(args.next().unwrap());
	out_fn(reg)
}

fn mov_common(size: Size, line: Pair<Rule>) -> Ins {
	let mut args = line.clone().into_inner();
	let src = args.next().unwrap();
	let dst = args.next().unwrap();
	match src.as_rule() {
		Rule::hex | Rule::bin | Rule::dec => {
			let src = num32s(src);
			let dst = reg_or_sp(dst);
			if (i8::MIN as i32..i8::MAX as i32).contains(&src) {
				match size {
					Size::Byte => Ins::Mov_Imm_Byte(src as i8, dst),
					Size::Word => Ins::Mov_Imm_Word(src as i16, dst),
					Size::Long => Ins::Mov_Imm_Long(src, dst),
				}
			} else if (i16::MIN as i32..i16::MAX as i32).contains(&src) {
				match size {
					Size::Byte => panic!("mov.b src too large: ${src:0X}"),
					Size::Word => Ins::Mov_Imm_Word(src as i16, dst),
					Size::Long => Ins::Mov_Imm_Long(src, dst),
				}
			} else {
				match size {
					Size::Byte => panic!("mov.b src too large: ${src:0X}"),
					Size::Word => panic!("mov.w src too large: ${src:0X}"),
					Size::Long => Ins::Mov_Imm_Long(src, dst),
				}
			}
		}
		Rule::addr_reg_or_sp => {
			let src = addr_reg_or_sp(src);
			let dst = reg_or_sp(dst);
			Ins::Mov(size, Arg::IndReg(src), Arg::DirReg(dst))
		}
		Rule::disp_gbr => {
			let src = disp_gbr(src);
			assert_eq!(0, reg(dst));
			Ins::Mov(size, src, Arg::DirReg(0))
		}
		Rule::disp_r0 => {
			let src = disp_r0(src);
			let dst = reg_or_sp(dst);
			Ins::Mov(size, src, Arg::DirReg(dst))
		}
		Rule::r0 => {
			assert_eq!(0, reg(src));
			let dst = disp_gbr(dst);
			Ins::Mov(size, Arg::DirReg(0), dst)
		}
		Rule::reg | Rule::sp => {
			let src = reg_or_sp(src);
			match dst.as_rule() {
				Rule::addr_reg_or_sp => {
					let dst = addr_reg_or_sp(dst);
					Ins::Mov(size, Arg::DirReg(src), Arg::IndReg(dst))
				}
				Rule::disp_r0 => {
					let dst = disp_r0(dst);
					Ins::Mov(size, Arg::DirReg(src), dst)
				}
				Rule::reg | Rule::sp => {
					let dst = reg_or_sp(dst);
					Ins::Mov_Reg(src, dst)
				}
				Rule::reg_pre_dec => {
					let dst = reg_pre_dec(dst);
					Ins::Mov(size, Arg::DirReg(src), Arg::PreDec(dst))
				}
				_ => unreachable!("expected valid common MOV dst, found: {dst}-{}", dst.as_str()),
			}
		}
		Rule::reg_post_inc => {
			let src = reg_post_inc(src);
			let dst = reg_or_sp(dst);
			Ins::Mov(size, Arg::PostInc(src), Arg::DirReg(dst))
		}
		Rule::lbl => {
			let dst = reg_or_sp(dst);
			Ins::Mov(size, Arg::Label(src.as_str().into()), Arg::DirReg(dst))
		}
		_ => unreachable!("expected common MOV arguments, found: {line}-{}", line.as_str()),
	}
}

pub fn parser(input: &str) -> Output {
	enum LabelState {
		Unknown,
		Label,
		Value(i32),
	}

	let mut assembly = Vec::<Asm>::default();
	let mut labels = HashMap::<Label, LabelState>::default();

	match Sh2Parser::parse(Rule::program, input) {
		Err(e) => eprintln!("{e}"),
		Ok(results) => for line in results {
			let asm: Asm = match line.as_rule() {
				Rule::dir_constant_b => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					if matches!(value.as_rule(), Rule::hex | Rule::bin | Rule::dec) {
						let num = num8s(value);
						Dir::ConstImmByte(num as u8).into()
					} else {
						unreachable!("unexpected constant value: {value}");
					}
				}
				Rule::dir_constant_w => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					if matches!(value.as_rule(), Rule::hex | Rule::bin | Rule::dec) {
						let num = num16s(value);
						Dir::ConstImmWord(num as u16).into()
					} else {
						unreachable!("unexpected constant value: {value}");
					}
				}
				Rule::dir_constant_l => {
					let mut args = line.into_inner();
					let value = args.next().unwrap();
					match value.as_rule() {
						Rule::hex | Rule::bin | Rule::dec => {
							let num = num32s(value);
							Dir::ConstImmLong(num as u32).into()
						}
						Rule::lbl => {
							let label: Label = value.as_str().into();
							match labels.get(&label) {
								None => {
									labels.insert(label.clone(), LabelState::Unknown);
									Dir::ConstLabelLong(label).into()
								}
								Some(LabelState::Unknown) |
								Some(LabelState::Label) => Dir::ConstLabelLong(label).into(),
								Some(LabelState::Value(val)) => Dir::ConstImmLong(*val as u32).into(),
							}
						}
						_ => unreachable!("unexpected constant value: {value}"),
					}
				}
				Rule::dir_align => {
					let mut args = line.into_inner();
					let item = args.next().unwrap().as_str();
					let alignment = item.parse::<u8>().unwrap();
					assert!(alignment.is_power_of_two());
					Dir::Align(alignment).into()
				}
				Rule::dir_org => {
					let mut args = line.into_inner();
					let section_addr = num32s(args.next().unwrap()) as u32;
					Dir::Org(section_addr).into()
				}
				Rule::dir_repeat => {
					let mut args = line.into_inner();
					Dir::Repeat(num8u(args.next().unwrap())).into()
				}
				Rule::dir_endr => Dir::EndRepeat.into(),
				Rule::dir_include => {
					let mut args = line.into_inner();
					let file = args.next().unwrap().as_str();
					Dir::Include(file.into()).into()
				}
				Rule::dir_binclude => {
					let mut args = line.into_inner();
					let file = args.next().unwrap().as_str();
					Dir::BinInclude(file.into()).into()
				}
				Rule::ins_add_imm => {
					let mut args = line.into_inner();
					let src = num8s(args.next().unwrap());
					let dst = reg(args.next().unwrap());
					Ins::Add_Imm(src,dst).into()
				}
				Rule::ins_add_reg => reg2_inst(line, Ins::Add_Reg).into(),
				Rule::ins_addc => reg2_inst(line, Ins::AddC).into(),
				Rule::ins_addv => reg2_inst(line, Ins::AddV).into(),
				Rule::ins_and_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::And_Byte(src).into()
				}
				Rule::ins_and_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::And_Imm(src).into()
				}
				Rule::ins_and_reg => reg2_inst(line, Ins::And_Reg).into(),
				Rule::ins_bf => label_inst(line, Ins::Bf).into(),
				Rule::ins_bfs => label_inst(line, Ins::BfS).into(),
				Rule::ins_bra => label_inst(line, Ins::Bra).into(),
				Rule::ins_braf => reg_inst(line, Ins::BraF).into(),
				Rule::ins_bsr => label_inst(line, Ins::Bsr).into(),
				Rule::ins_bsrf => reg_inst(line, Ins::BsrF).into(),
				Rule::ins_bt => label_inst(line, Ins::Bt).into(),
				Rule::ins_bts => label_inst(line, Ins::BtS).into(),
				Rule::ins_clrmac => Ins::ClrMac.into(),
				Rule::ins_clrt => Ins::ClrT.into(),
				Rule::ins_cmp_eq_imm => {
					let mut args = line.into_inner();
					let src = num8s(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as CMP/EQ dst");
					Ins::CmpEq_Imm(src).into()
				}
				Rule::ins_cmp_eq_reg => reg2_inst(line, Ins::CmpEq_Reg).into(),
				Rule::ins_cmp_ge => reg2_inst(line, Ins::CmpGE).into(),
				Rule::ins_cmp_gt => reg2_inst(line, Ins::CmpGT).into(),
				Rule::ins_cmp_hi => reg2_inst(line, Ins::CmpHI).into(),
				Rule::ins_cmp_hs => reg2_inst(line, Ins::CmpHS).into(),
				Rule::ins_cmp_pl => reg_inst(line, Ins::CmpPL).into(),
				Rule::ins_cmp_pz => reg_inst(line, Ins::CmpPZ).into(),
				Rule::ins_cmp_str => reg2_inst(line, Ins::CmpStr).into(),
				Rule::ins_div0s => reg2_inst(line, Ins::Div0S).into(),
				Rule::ins_div0u => Ins::Div0U.into(),
				Rule::ins_div1 => reg2_inst(line, Ins::Div1).into(),
				Rule::ins_dmuls => reg2_inst(line, Ins::DMulS).into(),
				Rule::ins_dmulu => reg2_inst(line, Ins::DMulU).into(),
				Rule::ins_dt => reg_inst(line, Ins::Dt).into(),
				Rule::ins_extsb => size_reg_inst(line, Size::Byte, Ins::ExtS).into(),
				Rule::ins_extsw => size_reg_inst(line, Size::Word, Ins::ExtS).into(),
				Rule::ins_extub => size_reg_inst(line, Size::Byte, Ins::ExtU).into(),
				Rule::ins_extuw => size_reg_inst(line, Size::Word, Ins::ExtU).into(),
				Rule::ins_jmp => {
					let mut args = line.into_inner();
					let src = addr_reg_or_sp(args.next().unwrap());
					Ins::Jmp(src).into()
				}
				Rule::ins_jsr => reg_inst(line, Ins::Jsr).into(),
				Rule::ins_ldc => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::gbr => Ins::LdcGBR(src).into(),
						Rule::sr => Ins::LdcSR(src).into(),
						Rule::vbr => Ins::LdcVBR(src).into(),
						_ => unreachable!("expected GBR, SR, or VBR as LDC dst"),
					}
				}
				Rule::ins_ldc_inc => {
					let mut args = line.into_inner();
					let src = reg_post_inc(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::gbr => Ins::LdcGBR_Inc(src).into(),
						Rule::sr => Ins::LdcSR_Inc(src).into(),
						Rule::vbr => Ins::LdcVBR_Inc(src).into(),
						_ => unreachable!("expected GBR, SR, or VBR as LDC.L dst"),
					}
				}
				Rule::ins_lds => {
					let mut args = line.into_inner();
					let src = reg_or_sp(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::macl => Ins::LdsMACL(src).into(),
						Rule::mach => Ins::LdsMACH(src).into(),
						Rule::pr => Ins::LdsPR(src).into(),
						_ => unreachable!("expected GBR, SR, or VBR as LDC dst"),
					}
				}
				Rule::ins_lds_inc => {
					let mut args = line.into_inner();
					let src = reg_post_inc(args.next().unwrap());
					let dst = args.next().unwrap();
					match dst.as_rule() {
						Rule::macl => Ins::LdsMACL_Inc(src).into(),
						Rule::mach => Ins::LdsMACH_Inc(src).into(),
						Rule::pr => Ins::LdsPR_Inc(src).into(),
						_ => unreachable!("expected GBR, SR, or VBR as LDC.L dst"),
					}
				}
				Rule::ins_macw => reg2_inst(line, Ins::Mac_Word).into(),
				Rule::ins_macl => reg2_inst(line, Ins::Mac_Long).into(),
				Rule::ins_movb => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_reg => {
							let src = disp_reg(src);
							assert_eq!(0, reg(args.next().unwrap()));
							Ins::Mov(Size::Byte, src, Arg::DirReg(0)).into()
						}
						Rule::r0 => {
							assert_eq!(0, reg(src));
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Byte, Arg::DirReg(0), dst).into()
						}
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Byte, src, Arg::DirReg(dst)).into()
						}
						_ => mov_common(Size::Byte, line).into(),
					}
				}
				Rule::ins_movw => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_reg => {
							let src = disp_reg(src);
							assert_eq!(0, reg(args.next().unwrap()));
							Ins::Mov(Size::Word, src, Arg::DirReg(0)).into()
						}
						Rule::r0 => {
							assert_eq!(0, reg(src));
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Word, Arg::DirReg(0), dst).into()
						}
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Word, src, Arg::DirReg(dst)).into()
						}
						_ => mov_common(Size::Word, line).into(),
					}
				}
				Rule::ins_movl => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Long, src, Arg::DirReg(dst)).into()
						}
						Rule::disp_reg => {
							let src = disp_reg(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Long, src, Arg::DirReg(dst)).into()
						}
						Rule::reg_or_sp => {
							let src = reg_or_sp(src);
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Long, Arg::DirReg(src), dst).into()
						}
						_ => mov_common(Size::Long, line).into(),
					}
				}
				Rule::ins_mova => {
					let mut args = line.into_inner();
					let Arg::DispPC(imm) = disp_pc(args.next().unwrap()) else {
						unreachable!("expected disp_pc");
					};
					assert_eq!(0, reg(args.next().unwrap()));
					Ins::MovA(imm).into()
				}
				Rule::ins_movt => reg_inst(line, Ins::MovT).into(),
				Rule::ins_mul => reg2_inst(line, Ins::Mul).into(),
				Rule::ins_muls => reg2_inst(line, Ins::MulS).into(),
				Rule::ins_mulu => reg2_inst(line, Ins::MulU).into(),
				Rule::ins_neg => reg2_inst(line, Ins::Neg).into(),
				Rule::ins_negc => reg2_inst(line, Ins::NegC).into(),
				Rule::ins_nop => Ins::Nop.into(),
				Rule::ins_not => reg2_inst(line, Ins::Not).into(),
				Rule::ins_or_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Or_Byte(src).into()
				}
				Rule::ins_or_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Or_Imm(src).into()
				}
				Rule::ins_or_reg => reg2_inst(line, Ins::Or_Reg).into(),
				Rule::ins_rotcl => reg_inst(line, Ins::RotCL).into(),
				Rule::ins_rotcr => reg_inst(line, Ins::RotCR).into(),
				Rule::ins_rotl => reg_inst(line, Ins::RotL).into(),
				Rule::ins_rotr => reg_inst(line, Ins::RotR).into(),
				Rule::ins_rte => Ins::Rte.into(),
				Rule::ins_rts => Ins::Rts.into(),
				Rule::ins_sett => Ins::SetT.into(),
				Rule::ins_shal => reg_inst(line, Ins::ShAL).into(),
				Rule::ins_shar => reg_inst(line, Ins::ShAR).into(),
				Rule::ins_shll => reg_inst(line, Ins::ShLL).into(),
				Rule::ins_shll16 => reg_inst(line, Ins::ShLL16).into(),
				Rule::ins_shll2 => reg_inst(line, Ins::ShLL2).into(),
				Rule::ins_shll8 => reg_inst(line, Ins::ShLL8).into(),
				Rule::ins_shlr => reg_inst(line, Ins::ShLR).into(),
				Rule::ins_shlr16 => reg_inst(line, Ins::ShLR16).into(),
				Rule::ins_shlr2 => reg_inst(line, Ins::ShLR2).into(),
				Rule::ins_shlr8 => reg_inst(line, Ins::ShLR8).into(),
				Rule::ins_sleep => Ins::Sleep.into(),
				Rule::ins_stc => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_or_sp(args.next().unwrap());
					match src.as_rule() {
						Rule::gbr => Ins::StcGBR(reg).into(),
						Rule::sr => Ins::StcSR(reg).into(),
						Rule::vbr => Ins::StcVBR(reg).into(),
						_ => unreachable!("expected GBR, SR, or VBR for STC dst"),
					}
				}
				Rule::ins_stc_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_pre_dec(args.next().unwrap());
					match src.as_rule() {
						Rule::gbr => Ins::StcGBR_Dec(reg).into(),
						Rule::sr => Ins::StcSR_Dec(reg).into(),
						Rule::vbr => Ins::StcVBR_Dec(reg).into(),
						_ => unreachable!("expected GBR, SR, or VBR for STC.L dst"),
					}
				}
				Rule::ins_sts => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_or_sp(args.next().unwrap());
					match src.as_rule() {
						Rule::macl => Ins::StsMACL(reg).into(),
						Rule::mach => Ins::StsMACH(reg).into(),
						Rule::pr => Ins::StsPR(reg).into(),
						_ => unreachable!("expected MACL, MACH, PR for STS dst")
					}
				}
				Rule::ins_sts_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_pre_dec(args.next().unwrap());
					match src.as_rule() {
						Rule::macl => Ins::StsMACL_Dec(reg).into(),
						Rule::mach => Ins::StsMACH_Dec(reg).into(),
						Rule::pr => Ins::StsPR_Dec(reg).into(),
						_ => unreachable!("expected MACL, MACH, or PR for STS.L dst"),
					}
				}
				Rule::ins_sub => reg2_inst(line, Ins::Sub).into(),
				Rule::ins_subc => reg2_inst(line, Ins::SubC).into(),
				Rule::ins_subv => reg2_inst(line, Ins::SubV).into(),
				Rule::ins_swapb => size_reg_inst(line, Size::Byte, Ins::Swap).into(),
				Rule::ins_swapw => size_reg_inst(line, Size::Word, Ins::Swap).into(),
				Rule::ins_tas => reg_inst(line, Ins::Tas).into(),
				Rule::ins_trapa => {
					let mut args = line.into_inner();
					let imm = num8u(args.next().unwrap());
					Ins::TrapA(imm).into()
				}
				Rule::ins_tst_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Tst_Byte(src).into()
				}
				Rule::ins_tst_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Tst_Imm(src).into()
				}
				Rule::ins_tst_reg => reg2_inst(line, Ins::Tst_Reg).into(),
				Rule::ins_xor_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Xor_Byte(src).into()
				}
				Rule::ins_xor_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Xor_Imm(src).into()
				}
				Rule::ins_xor_reg => reg2_inst(line, Ins::Xor_Reg).into(),
				Rule::ins_xtrct => reg2_inst(line, Ins::Xtrct).into(),
				Rule::val_line => {
					let mut args = line.into_inner();
					let label: Label = args.next().unwrap().as_str().into();
					let value = num32s(args.next().unwrap());
					if labels.contains_key(&label) {
						panic!("duplicate label: '{label}'");
					}
					labels.insert(label, LabelState::Value(value));
					continue;
				}
				Rule::lbl_line => {
					let s = line.as_str();
					let label: Label = s[..s.len()-1].into();
					if labels.contains_key(&label) {
						panic!("duplicate label: '{label}'");
					}
					labels.insert(label.clone(), LabelState::Label);
					continue;
				}
				Rule::EOI => continue,
				_ => unreachable!("unexpected token found: {line}"),
			};

			assembly.push(asm);
		}
	}

	let mut unknown_label_count = 0;
	let labels = labels.into_iter()
		.flat_map(|(label, lbl_type)| match lbl_type {
			LabelState::Unknown => {
				eprintln!("Unknown label: '{label}'");
				unknown_label_count += 1;
				None
			}
			LabelState::Label => Some((label, LabelType::Unknown)),
			LabelState::Value(val) => Some((label, LabelType::Value(val as u32))),
		})
		.collect();

	if unknown_label_count > 0 {
		panic!("{unknown_label_count} unknown labels");
	}

	Output {
		assembly,
		labels,
	}
}

#[cfg(test)]
mod can_parse {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = super::Sh2Parser::parse($rule, $input) {
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

	#[test]
	fn exts() {
		check!(Rule::ins_extsb, "exts.b r2,r5");
		check!(Rule::ins_extsw, "exts.w r7,sp");
	}

	#[test]
	fn extu() {
		check!(Rule::ins_extub, "extu.b r9,r1");
		check!(Rule::ins_extuw, "extu.w sp,r4");
	}

	#[test]
	fn jmp() {
		check!(Rule::ins_jmp, "jmp @r9");
		check!(Rule::ins_jmp, "jmp @sp");
	}

	//#[test]
	//fn () { check!(Rule::ins_, "") }
}

#[cfg(test)]
mod examples {
	use super::*;

	macro_rules! check {
		($input:expr) => {{
			if let Err(e) = super::Sh2Parser::parse(Rule::program, $input) {
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
	.repeat 16
	div1 r0,r1   ; Repeat 16 times
	.endr
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
	.repeat 32
	rotcl r2     ; Repeat 32
	div1 r0,r1
	.endr
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
	.repeat 16
	DIV1   R0,R1 ; Repeat 16 times
	.endr
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
	.repeat 32
	DIV1  R0,R1 ; Repeat 32 times
	.endr
	ROTCL R2    ; R2 = Quotient (1's complement)
	ADDC  R3,R2 ; Increments and takes 2's complement if the MSB of the quotient is 1. R2 = Quotient (2's complement)
")
	}

	#[test]
	fn dt() {
		check!("
	MOV #4,R5 ; Sets the number of loops
LOOP:
	ADD R0,R1
	DT  R5    ; Decrements the R5 value and checks whether it has become 0.
	BF  LOOP  ; Branches to LOOP if T=0. (In this example, loops 4 times.)
")
	}

	#[test]
	fn exts() {
		check!("
	EXTS.B R0,R1
	EXTS.W R0,R1
")
	}

	#[test]
	fn extu() {
		check!("
	EXTU.B R0,R1
	EXTU.W R0,R1
")
	}

	#[test]
	fn jmp() {
		check!("
	MOV.L #JMP_TABLE,R0
	JMP @R0
	MOV R0,R1
	.align 4
JMP_TABLE:
	.dc.l TRGET

TRGET:
	ADD #1,R1
")
	}
}

#[cfg(test)]
mod invalid {
	use super::*;

	macro_rules! check {
		($rule:expr, $input:expr) => {{
			if let Err(e) = super::Sh2Parser::parse($rule, $input) {
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

