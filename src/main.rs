/*
fn main() -> miette::Result<()> {

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

use tracing::{
	trace,
	debug,
	info,
	warn,
	error,
	instrument,
};

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
		_ => {
			error!("expected Reg or SP, found: {item}-{}", item.as_str());
			unreachable!()
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
				Ins::Mov_Imm_Byte(src as i8, dst)
			} else if (i16::MIN as i32..i16::MAX as i32).contains(&src) {
				Ins::Mov_Imm_Word(src as i16, dst)
			} else {
				Ins::Mov_Imm_Long(src, dst)
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

#[instrument]
fn main() {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let input = read_to_string(&source).expect("unable to read source file");

	//let target = args.next().unwrap_or("asm.out".to_string());

	let Output { sections, mut labels, values } = parser(&input);

/*
	println!("values: {}", values.len());
	for (name, value) in &values {
		println!("  {name} = {value}");
	}
	println!("labels: {}", labels.len());
	for (name, addr) in &labels {
		println!("  {name} : ${addr:0X?}");
	}
*/
	println!("sections: {}", sections.len());
	for (addr, section) in &sections {
		println!("  addr: ${addr:0X}");
		for (i, ins) in section.iter().enumerate() {
			println!("{:0X}: {ins:0X?}", addr + i as u32 * 2);
		}
	}

	// TODO - srenshaw - Time to start resolving word/long immediate placement and label addressing.
	#[derive(Debug, Clone)]
	enum Item {
		Byte(u8),
		Word(u16),
		Ins(Ins),
	}

	let mut sections: HashMap<u32, Vec<Item>> = sections.into_iter()
		.map(|(addr,section)| (addr, section.into_iter().map(Item::Ins).collect()))
		.collect();

	fn fmt_x(base: u16) -> Item {
		Item::Word(base)
	}

	fn fmt_n(base: u16, dst: &Reg) -> Item {
		let n = ((*dst) as u16) << 8;
		Item::Word(base | n)
	}

	fn fmt_m(base: u16, src: &Reg) -> Item {
		let m = ((*src) as u16) << 8;
		Item::Word(base | m)
	}

	fn fmt_nm(base: u16, src: &Reg, dst: &Reg) -> Item {
		let n = ((*dst) as u16) << 8;
		let m = ((*src) as u16) << 4;
		Item::Word(base | n | m)
	}

	fn fmt_md(base: u16, src: &Reg, disp: &i8) -> Item {
		let m = ((*src) as u16) << 4;
		let d = ((*disp) as u8 as u16) & 0xF;
		Item::Word(base | m | d)
	}

	fn fmt_nd4(base: u16, dst: &Reg, disp: &i8) -> Item {
		let n = ((*dst) as u16) << 4;
		let d = ((*disp) as u8 as u16) & 0xF;
		Item::Word(base | n | d)
	}

	fn fmt_nmd(base: u16, src: &Reg, dst: &Reg, disp: &i8) -> Item {
		let n = ((*dst) as u16) << 8;
		let m = ((*src) as u16) << 4;
		let d = ((*disp) as u8 as u16) & 0xF;
		Item::Word(base | n | m | d)
	}

	fn fmt_d(base: u16, disp: i8) -> Item {
		let d = (disp as u8 as u16) & 0xFF;
		Item::Word(base | d)
	}

	fn fmt_d12(base: u16, disp: i16) -> Item {
		let d = (disp as u16) & 0xFFF;
		Item::Word(base | d)
	}

	fn fmt_nd8(base: u16, dst: &Reg, disp: &i8) -> Item {
		let n = ((*dst) as u16) << 8;
		let d = ((*disp) as u8 as u16) & 0xFF;
		Item::Word(base | n | d)
	}

	fn fmt_i(base: u16, imm: u8) -> Item {
		let i = (imm as u16) & 0xFF;
		Item::Word(base | i)
	}

	#[instrument]
	fn fmt_ni(base: u16, imm: u8, dst: Reg) -> Item {
		let n = (dst as u16) << 8;
		let i = (imm as u16) & 0xFF;
		Item::Word(base | n | i)
	}

	fn resolve_branch(
		ins_name: &str,
		lbl: &Label,
		labels: &HashMap<Label, Option<u32>>,
		section_addr: u32,
		new_section: &mut Vec<Item>,
		width_check: fn(i32) -> bool,
		format: fn(i32) -> Item,
	) {
		if let Some(addr) = labels[lbl] {
			let diff = addr.abs_diff(section_addr);
			let disp = if addr >= section_addr { diff } else { !diff + 1 } as i32;
			let disp = disp >> 1;
			if width_check(disp) {
				new_section.push(format(disp));
			} else {
				error!("{ins_name} label '{lbl}' too far: {disp}");
			}
		} else {
			error!("{ins_name} label '{lbl}' not found");
		}
	}

	let mut is_unresolved = true;
	while is_unresolved {
		is_unresolved = false;
		for (section_base, section) in &mut sections {
			let mut new_section = Vec::with_capacity(section.len() + 128);
			let mut is_address_known = true;

			for (idx, item) in section.iter().enumerate() {
				use Size as S;
				use Arg as A;

				let section_addr = section_base + idx as u32 * 2;

				match item {
					Item::Ins(ins) => match ins {
						// Data Transfer Instructions //
						Ins::Mov_Imm_Byte(imm, dst) => new_section.push(fmt_ni(0xE000, *imm as u8, *dst)),
						Ins::Mov_Imm_Word(_imm, _dst) => error!("implement MOV #<i16>,Reg"),
						Ins::Mov_Imm_Long(_imm, _dst) => error!("implement MOV #<i32>,Reg"),
						Ins::Mov_Reg(src, dst) => new_section.push(fmt_nm(0x6003, src, dst)),
						Ins::Mov(size, src, dst) => match (size, src, dst) {
							(S::Byte, A::DirReg(0), A::DispGBR(disp)) => new_section.push(fmt_d(0xC000, *disp)),
							(S::Byte, A::DirReg(0), A::DispRegByte(disp, dst)) => new_section.push(fmt_nd4(0x8000, dst, disp)),
							(S::Byte, A::DirReg(0), A::DispRegLong(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.B R0,@(<i32>,Rn)");
							}
							(S::Byte, A::DirReg(0), A::DispRegWord(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.B R0,@(<i16>,Rn)");
							}
							(S::Byte, A::DirReg(src), A::DispR0(dst)) => new_section.push(fmt_nm(0x0004, src, dst)),
							(S::Byte, A::DirReg(src), A::IndReg(dst)) => new_section.push(fmt_nm(0x2000, src, dst)),
							(S::Byte, A::DirReg(src), A::PreDec(dst)) => new_section.push(fmt_nm(0x2004, src, dst)),
							(S::Byte, A::DispGBR(disp), A::DirReg(0)) => new_section.push(fmt_d(0xC400, *disp)),
							(S::Byte, A::DispRegByte(disp,src), A::DirReg(0)) => new_section.push(fmt_md(0x8400, src, disp)),
							(S::Byte, A::DispRegLong(_disp, _src), A::DirReg(0)) => {
								is_address_known = false;
								error!("implement MOV.B @(<i32>,Rm),R0");
							}
							(S::Byte, A::DispRegWord(_disp, _src), A::DirReg(0)) => {
								is_address_known = false;
								error!("implement MOV.B @(<i16>,Rm),R0");
							}
							(S::Byte, A::IndReg(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6000, src, dst)),
							(S::Byte, A::PostInc(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6004, src, dst)),
							(S::Long, A::DirReg(0), A::DispGBR(disp)) => new_section.push(fmt_d(0xC200, *disp)),
							(S::Long, A::DirReg(src), A::DispR0(dst)) => new_section.push(fmt_nm(0x0006, src, dst)),
							(S::Long, A::DirReg(src), A::DispRegByte(disp,dst)) => new_section.push(fmt_nmd(0x1000, src, dst, disp)),
							(S::Long, A::DirReg(_src), A::DispRegLong(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.L Rm,@(<i32>,Rn)");
							}
							(S::Long, A::DirReg(_src), A::DispRegWord(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.L Rm,@(<i16>,Rn)");
							}
							(S::Long, A::DirReg(src), A::IndReg(dst)) => new_section.push(fmt_nm(0x2002, src, dst)),
							(S::Long, A::DirReg(src), A::PreDec(dst)) => new_section.push(fmt_nm(0x2006, src, dst)),
							(S::Long, A::DispGBR(disp), A::DirReg(0)) => new_section.push(fmt_d(0xC600, *disp)),
							(S::Long, A::DispPC(disp), A::DirReg(dst)) => new_section.push(fmt_nd8(0xD000, dst, disp)),
							(S::Long, A::DispR0(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x000E, src, dst)),
							(S::Long, A::DispRegByte(disp,src), A::DirReg(dst)) => new_section.push(fmt_nmd(0x5000, src, dst, disp)),
							(S::Long, A::DispRegLong(_disp, _src), A::DirReg(_dst)) => {
								is_address_known = false;
								error!("implement MOV.L @(<i32>,Rm),Rn");
							}
							(S::Long, A::DispRegWord(_disp, _src), A::DirReg(_dst)) => {
								is_address_known = false;
								error!("implement MOV.L @(<i16>,Rm),Rn");
							}
							(S::Long, A::IndReg(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6002, src, dst)),
							(S::Long, A::PostInc(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6006, src, dst)),
							(S::Word, A::DirReg(0), A::DispGBR(disp)) => new_section.push(fmt_d(0xC100, *disp)),
							(S::Word, A::DirReg(0), A::DispRegByte(disp,dst)) => new_section.push(fmt_nd4(0x8100, dst, disp)),
							(S::Word, A::DirReg(0), A::DispRegLong(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.W R0,@(<i32>,Rn)");
							}
							(S::Word, A::DirReg(0), A::DispRegWord(_disp, _dst)) => {
								is_address_known = false;
								error!("implement MOV.W R0,@(<i16>,Rn)");
							}
							(S::Word, A::DirReg(src), A::DispR0(dst)) => new_section.push(fmt_nm(0x0005, src, dst)),
							(S::Word, A::DirReg(src), A::IndReg(dst)) => new_section.push(fmt_nm(0x2001, src, dst)),
							(S::Word, A::DirReg(src), A::PreDec(dst)) => new_section.push(fmt_nm(0x2005, src, dst)),
							(S::Word, A::DispGBR(disp), A::DirReg(0)) => new_section.push(fmt_d(0xC500, *disp)),
							(S::Word, A::DispPC(disp), A::DirReg(dst)) => new_section.push(fmt_nd8(0x9000, dst, disp)),
							(S::Word, A::DispR0(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x000D, src, dst)),
							(S::Word, A::DispRegByte(disp,src), A::DirReg(0)) => new_section.push(fmt_md(0x8500, src, disp)),
							(S::Word, A::DispRegLong(_disp, _src), A::DirReg(0)) => {
								is_address_known = false;
								error!("implement MOV.W @(<i32>,Rm),R0");
							}
							(S::Word, A::DispRegWord(_disp, _src), A::DirReg(0)) => {
								is_address_known = false;
								error!("implement MOV.W @(<i16>,Rm),R0");
							}
							(S::Word, A::IndReg(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6001, src, dst)),
							(S::Word, A::PostInc(src), A::DirReg(dst)) => new_section.push(fmt_nm(0x6005, src, dst)),
							(sz,s,d) => {
								error!("expected valid MOV instruction, found ({sz:?},{s:?},{d:?})");
							}
						}
						Ins::MovA(disp) => new_section.push(fmt_d(0xC700, *disp)),
						Ins::MovT(dst) => new_section.push(fmt_n(0x0029, dst)),
						Ins::Swap(S::Byte, src, dst) => new_section.push(fmt_nm(0x6008, src, dst)),
						Ins::Swap(S::Word, src, dst) => new_section.push(fmt_nm(0x6009, src, dst)),
						Ins::Xtrct(src,dst) => new_section.push(fmt_nm(0x200D, src, dst)),

						// Arithmetic Instructions //
						Ins::Add_Reg      (src, dst) => new_section.push(fmt_nm(0x300C, src, dst)),
						Ins::Add_Imm      (imm, dst) => new_section.push(fmt_ni(0x7000, *imm as u8, *dst)),
						Ins::AddC         (src, dst) => new_section.push(fmt_nm(0x300E, src, dst)),
						Ins::AddV         (src, dst) => new_section.push(fmt_nm(0x0000, src, dst)),
						Ins::CmpEq_Imm    (imm)      => new_section.push(fmt_i (0x8800, *imm as u8)),
						Ins::CmpEq_Reg    (src, dst) => new_section.push(fmt_nm(0x3000, src, dst)),
						Ins::CmpHS        (src, dst) => new_section.push(fmt_nm(0x3002, src, dst)),
						Ins::CmpGE        (src, dst) => new_section.push(fmt_nm(0x3003, src, dst)),
						Ins::CmpHI        (src, dst) => new_section.push(fmt_nm(0x3006, src, dst)),
						Ins::CmpGT        (src, dst) => new_section.push(fmt_nm(0x3007, src, dst)),
						Ins::CmpPL             (dst) => new_section.push(fmt_n (0x4015, dst)),
						Ins::CmpPZ             (dst) => new_section.push(fmt_n (0x4011, dst)),
						Ins::CmpStr       (src, dst) => new_section.push(fmt_nm(0x200C, src, dst)),
						Ins::Div1         (src, dst) => new_section.push(fmt_nm(0x3004, src, dst)),
						Ins::Div0S        (src, dst) => new_section.push(fmt_nm(0x2007, src, dst)),
						Ins::Div0U                   => new_section.push(fmt_x (0x0019)),
						Ins::DMulS        (src, dst) => new_section.push(fmt_nm(0x300D, src, dst)),
						Ins::DMulU        (src, dst) => new_section.push(fmt_nm(0x3005, src, dst)),
						Ins::Dt                (dst) => new_section.push(fmt_n (0x4010, dst)),
						Ins::ExtS(S::Byte, src, dst) => new_section.push(fmt_nm(0x600E, src, dst)),
						Ins::ExtS(S::Word, src, dst) => new_section.push(fmt_nm(0x600F, src, dst)),
						Ins::ExtU(S::Byte, src, dst) => new_section.push(fmt_nm(0x600C, src, dst)),
						Ins::ExtU(S::Word, src, dst) => new_section.push(fmt_nm(0x600D, src, dst)),
						Ins::Mac_Long     (src, dst) => new_section.push(fmt_nm(0x000F, src, dst)),
						Ins::Mac_Word     (src, dst) => new_section.push(fmt_nm(0x400F, src, dst)),
						Ins::Mul          (src, dst) => new_section.push(fmt_nm(0x0007, src, dst)),
						Ins::MulS         (src, dst) => new_section.push(fmt_nm(0x200F, src, dst)),
						Ins::MulU         (src, dst) => new_section.push(fmt_nm(0x200E, src, dst)),
						Ins::Neg          (src, dst) => new_section.push(fmt_nm(0x600B, src, dst)),
						Ins::NegC         (src, dst) => new_section.push(fmt_nm(0x600A, src, dst)),
						Ins::Sub          (src, dst) => new_section.push(fmt_nm(0x3008, src, dst)),
						Ins::SubC         (src, dst) => new_section.push(fmt_nm(0x300A, src, dst)),
						Ins::SubV         (src, dst) => new_section.push(fmt_nm(0x300B, src, dst)),

						// Logic Operation Instructions //
						Ins::And_Reg (src, dst) => new_section.push(fmt_nm(0x2009, src, dst)),
						Ins::And_Imm (imm)      => new_section.push(fmt_i (0xC900, *imm)),
						Ins::And_Byte(imm)      => new_section.push(fmt_i (0xCD00, *imm)),
						Ins::Not     (src, dst) => new_section.push(fmt_nm(0x6007, src, dst)),
						Ins::Or_Reg  (src, dst) => new_section.push(fmt_nm(0x200B, src, dst)),
						Ins::Or_Imm  (imm)      => new_section.push(fmt_i (0xCB00, *imm)),
						Ins::Or_Byte (imm)      => new_section.push(fmt_i (0xCF00, *imm)),
						Ins::Tas          (dst) => new_section.push(fmt_n (0x401B, dst)),
						Ins::Tst_Reg (src, dst) => new_section.push(fmt_nm(0x2008, src, dst)),
						Ins::Tst_Imm (imm)      => new_section.push(fmt_i (0xC800, *imm)),
						Ins::Tst_Byte(imm)      => new_section.push(fmt_i (0xCC00, *imm)),
						Ins::Xor_Reg (src, dst) => new_section.push(fmt_nm(0x200A, src, dst)),
						Ins::Xor_Imm (imm)      => new_section.push(fmt_i (0xCA00, *imm)),
						Ins::Xor_Byte(imm)      => new_section.push(fmt_i (0xCE00, *imm)),

						// Shift Instructions //
						Ins::RotL  (dst) => new_section.push(fmt_n(0x4004, dst)),
						Ins::RotR  (dst) => new_section.push(fmt_n(0x4005, dst)),
						Ins::RotCL (dst) => new_section.push(fmt_n(0x4024, dst)),
						Ins::RotCR (dst) => new_section.push(fmt_n(0x4025, dst)),
						Ins::ShAL  (dst) => new_section.push(fmt_n(0x4020, dst)),
						Ins::ShAR  (dst) => new_section.push(fmt_n(0x4021, dst)),
						Ins::ShLL  (dst) => new_section.push(fmt_n(0x4000, dst)),
						Ins::ShLR  (dst) => new_section.push(fmt_n(0x4001, dst)),
						Ins::ShLL2 (dst) => new_section.push(fmt_n(0x4008, dst)),
						Ins::ShLR2 (dst) => new_section.push(fmt_n(0x4009, dst)),
						Ins::ShLL8 (dst) => new_section.push(fmt_n(0x4018, dst)),
						Ins::ShLR8 (dst) => new_section.push(fmt_n(0x4019, dst)),
						Ins::ShLL16(dst) => new_section.push(fmt_n(0x4028, dst)),
						Ins::ShLR16(dst) => new_section.push(fmt_n(0x4029, dst)),

						// Branch Instructions //
						Ins::Bf(lbl) => {
							resolve_branch("BF", lbl, &labels, section_addr, &mut new_section,
								|disp| (i8::MIN as i32..=i8::MAX as i32).contains(&disp),
								|disp| fmt_d(0x8B00, disp as i8));
						}
						Ins::BfS(lbl) => {
							resolve_branch("BF/S", lbl, &labels, section_addr, &mut new_section,
								|disp| (i8::MIN as i32..=i8::MAX as i32).contains(&disp),
								|disp| fmt_d(0x8F00, disp as i8));
						}
						Ins::Bt(lbl) => {
							resolve_branch("BT", lbl, &labels, section_addr, &mut new_section,
								|disp| (i8::MIN as i32..=i8::MAX as i32).contains(&disp),
								|disp| fmt_d(0x8900, disp as i8));
						}
						Ins::BtS(lbl) => {
							resolve_branch("BT/S", lbl, &labels, section_addr, &mut new_section,
								|disp| (i8::MIN as i32..i8::MAX as i32).contains(&disp),
								|disp| fmt_d(0xBD00, disp as i8));
						}
						Ins::Bra(lbl) => {
							resolve_branch("BRA", lbl, &labels, section_addr, &mut new_section,
								|disp| (-2048..2048).contains(&disp),
								|disp| fmt_d12(0xA000, disp as i16 & 0x0FFF));
						}
						Ins::BraF(dst) => new_section.push(fmt_m(0x0023, dst)),
						Ins::Bsr(lbl) => {
							resolve_branch("BSR", lbl, &labels, section_addr, &mut new_section,
								|disp| (-2048..2048).contains(&disp),
								|disp| fmt_d12(0xB000, disp as i16 & 0x0FFF));
						}
						Ins::BsrF(dst) => new_section.push(fmt_m(0x0003, dst)),
						Ins::Jmp(dst) => {
							new_section.push(fmt_m(0x402B, dst));
							if is_address_known {
								todo!("output generated out-of-band values");
							}
						}
						Ins::Jsr(dst) => new_section.push(fmt_m(0x400B, dst)),
						Ins::Rts => new_section.push(fmt_x(0x000B)),

						// System Control Instructions //
						Ins::ClrT             => new_section.push(fmt_x(0x0008)),
						Ins::ClrMac           => new_section.push(fmt_x(0x0028)),
						Ins::LdcSR      (src) => new_section.push(fmt_m(0x400E, src)),
						Ins::LdcGBR     (src) => new_section.push(fmt_m(0x401E, src)),
						Ins::LdcVBR     (src) => new_section.push(fmt_m(0x402E, src)),
						Ins::LdcSR_Inc  (src) => new_section.push(fmt_m(0x4007, src)),
						Ins::LdcGBR_Inc (src) => new_section.push(fmt_m(0x4017, src)),
						Ins::LdcVBR_Inc (src) => new_section.push(fmt_m(0x4027, src)),
						Ins::LdsMACH    (src) => new_section.push(fmt_m(0x400A, src)),
						Ins::LdsMACL    (src) => new_section.push(fmt_m(0x401A, src)),
						Ins::LdsPR      (src) => new_section.push(fmt_m(0x402A, src)),
						Ins::LdsMACH_Inc(src) => new_section.push(fmt_m(0x4006, src)),
						Ins::LdsMACL_Inc(src) => new_section.push(fmt_m(0x4016, src)),
						Ins::LdsPR_Inc  (src) => new_section.push(fmt_m(0x4026, src)),
						Ins::Nop              => new_section.push(fmt_x(0x0009)),
						Ins::Rte              => new_section.push(fmt_x(0x002B)),
						Ins::SetT             => new_section.push(fmt_x(0x0018)),
						Ins::Sleep            => new_section.push(fmt_x(0x001B)),
						Ins::StcSR      (dst) => new_section.push(fmt_n(0x0002, dst)),
						Ins::StcGBR     (dst) => new_section.push(fmt_n(0x0012, dst)),
						Ins::StcVBR     (dst) => new_section.push(fmt_n(0x0022, dst)),
						Ins::StcSR_Dec  (dst) => new_section.push(fmt_n(0x4003, dst)),
						Ins::StcGBR_Dec (dst) => new_section.push(fmt_n(0x4013, dst)),
						Ins::StcVBR_Dec (dst) => new_section.push(fmt_n(0x4023, dst)),
						Ins::StsMACH    (dst) => new_section.push(fmt_n(0x000A, dst)),
						Ins::StsMACL    (dst) => new_section.push(fmt_n(0x001A, dst)),
						Ins::StsPR      (dst) => new_section.push(fmt_n(0x002A, dst)),
						Ins::StsMACH_Dec(dst) => new_section.push(fmt_n(0x4002, dst)),
						Ins::StsMACL_Dec(dst) => new_section.push(fmt_n(0x4012, dst)),
						Ins::StsPR_Dec  (dst) => new_section.push(fmt_n(0x4022, dst)),
						Ins::TrapA      (imm) => new_section.push(fmt_i(0xC300, *imm)),

						// Assembler Directives //
						Ins::Const_Imm(S::Byte, imm) => todo!("byte constant: {imm}"),/*{
							if (i8::MIN as i64..i8::MAX as i64).contains(imm) {
								new_section.push(Item::Byte(*imm as u8));
							} else {
								error!("byte constant too large: {imm}");
							}
						}*/
						Ins::Const_Imm(S::Word, imm) => {
							if (i16::MIN as i64..i16::MAX as i64).contains(imm) {
								new_section.push(Item::Word(*imm as u16));
							} else {
								error!("word constant too large: {imm}");
							}
						}
						Ins::Const_Imm(S::Long, imm) => {
							if (i32::MIN as i64..i32::MAX as i64).contains(imm) {
								new_section.push(Item::Word((*imm >> 16) as u16));
								new_section.push(Item::Word(*imm as u16));
							} else {
								error!("long constant too large: {imm}");
							}
						}
						Ins::Const_Label(S::Byte, lbl) => todo!("byte constant with label: '{lbl}'"),
						Ins::Const_Label(S::Word, lbl) => {
							if let Some(value) = values.get(lbl) {
								if (i16::MIN as i32..i16::MAX as i32).contains(value) {
									new_section.push(Item::Word(*value as u16));
								} else {
									error!("label value too large for word constant: '{lbl}' = {value}");
								}
							} else if labels.contains_key(lbl) {
								error!("address labels cannot be used for word constants");
							} else {
								error!("unknown label: '{lbl}'");
							}
						}
						Ins::Const_Label(S::Long, lbl) => {
							if let Some(value) = values.get(lbl) {
								new_section.push(Item::Word((*value >> 16) as u16));
								new_section.push(Item::Word(*value as u16));
							} else if let Some(addr) = labels[lbl] {
								new_section.push(Item::Word((addr >> 16) as u16));
								new_section.push(Item::Word(addr as u16));
							} else {
								error!("unknown label: '{lbl}'");
							}
						}
						Ins::Label(lbl) => {
							if !is_address_known {
								new_section.push(Item::Ins(Ins::Label(lbl.clone())));
							} else if labels[lbl].is_none() {
								is_address_known = false;
								debug!("add label {lbl} : {section_addr:0X}");
								labels.insert(lbl.clone(), Some(section_addr));
							} else {
								error!("duplicate label '{lbl}' found! Ignoring.");
							}
						}

						ins => {
							error!("found unexpected instruction: {ins:?}");
						}
					}
					item => new_section.push(item.clone()),
				}
			}

			*section = new_section;
			is_unresolved |= !is_address_known;
		}
	}

/*
	println!("labels: {}", labels.len());
	for (label, addr) in labels {
		println!("  {label} = {addr:0X?}");
	}
	println!("sections: {}", sections.len());
	for (addr, section) in &sections {
		println!("  section: {addr:0X}");
		for item in section {
			match item {
				Item::Byte(_) => {}
				Item::Word(_) => {}
				Item::Ins(ins) => println!("    {ins:?}"),
			}
		}
	}
*/
}

struct Output {
	sections: HashMap::<u32, Vec<Ins>>,
	labels: HashMap::<Label, Option<u32>>,
	values: HashMap<Label, i32>,
}

enum LabelType {
	Unknown,
	Label,
	Value(i32),
}

fn parser(input: &str) -> Output {
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
							Ins::Const_Imm(Size::Byte, num as i64)
						}
						Rule::lbl => {
							let label: Label = value.as_str().into();
							match labels.get(&label) {
								None => {
									labels.insert(label.clone(), LabelType::Unknown);
									Ins::Const_Label(Size::Byte, label)
								}
								Some(LabelType::Unknown) |
								Some(LabelType::Label) => {
									Ins::Const_Label(Size::Byte, label)
								}
								Some(LabelType::Value(val)) => {
									if (i8::MIN as i32..i8::MAX as i32).contains(val) {
										Ins::Const_Imm(Size::Byte, *val as i64)
									} else {
										panic!("value too big for byte-constant directive");
									}
								}
							}
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
							Ins::Const_Imm(Size::Word, num as i64)
						}
						Rule::lbl => {
							let label: Label = value.as_str().into();
							match labels.get(&label) {
								None => {
									labels.insert(label.clone(), LabelType::Unknown);
									Ins::Const_Label(Size::Word, label)
								}
								Some(LabelType::Unknown) |
								Some(LabelType::Label) => {
									Ins::Const_Label(Size::Word, label)
								}
								Some(LabelType::Value(val)) => {
									if (i16::MIN as i32..i16::MAX as i32).contains(val) {
										Ins::Const_Imm(Size::Word, *val as i64)
									} else {
										panic!("value too big for word-constant directive");
									}
								}
							}
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
							Ins::Const_Imm(Size::Long, num as i64)
						}
						Rule::lbl => {
							let label: Label = value.as_str().into();
							match labels.get(&label) {
								None => {
									labels.insert(label.clone(), LabelType::Unknown);
									Ins::Const_Label(Size::Long, label)
								}
								Some(LabelType::Unknown) |
								Some(LabelType::Label) => {
									Ins::Const_Label(Size::Long, label)
								}
								Some(LabelType::Value(val)) => {
									Ins::Const_Imm(Size::Long, *val as i64)
								}
							}
						}
						_ => unreachable!("unexpected constant value: {value}"),
					}
				}
				Rule::dir_org => {
					let mut args = line.into_inner();
					skey = num32s(args.next().unwrap()) as u32;
					continue;
				}
				Rule::dir_repeat => {
					let mut args = line.into_inner();
					repetitions = Some((
						num32s(args.next().unwrap()),
						Vec::with_capacity(64),
					));
					continue;
				}
				Rule::dir_endr => {
					if let Some((num, instrs)) = repetitions {
						let section = sections.entry(skey).or_default();
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
					Ins::Add_Imm(src,dst)
				}
				Rule::ins_add_reg => reg2_inst(line, Ins::Add_Reg),
				Rule::ins_addc => reg2_inst(line, Ins::AddC),
				Rule::ins_addv => reg2_inst(line, Ins::AddV),
				Rule::ins_and_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::And_Byte(src)
				}
				Rule::ins_and_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::And_Imm(src)
				}
				Rule::ins_and_reg => reg2_inst(line, Ins::And_Reg),
				Rule::ins_bf => label_inst(line, Ins::Bf),
				Rule::ins_bfs => label_inst(line, Ins::BfS),
				Rule::ins_bra => label_inst(line, Ins::Bra),
				Rule::ins_braf => reg_inst(line, Ins::BraF),
				Rule::ins_bsr => label_inst(line, Ins::Bsr),
				Rule::ins_bsrf => reg_inst(line, Ins::BsrF),
				Rule::ins_bt => label_inst(line, Ins::Bt),
				Rule::ins_bts => label_inst(line, Ins::BtS),
				Rule::ins_clrmac => Ins::ClrMac,
				Rule::ins_clrt => Ins::ClrT,
				Rule::ins_cmp_eq_imm => {
					let mut args = line.into_inner();
					let src = num8s(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as CMP/EQ dst");
					Ins::CmpEq_Imm(src)
				}
				Rule::ins_cmp_eq_reg => reg2_inst(line, Ins::CmpEq_Reg),
				Rule::ins_cmp_ge => reg2_inst(line, Ins::CmpGE),
				Rule::ins_cmp_gt => reg2_inst(line, Ins::CmpGT),
				Rule::ins_cmp_hi => reg2_inst(line, Ins::CmpHI),
				Rule::ins_cmp_hs => reg2_inst(line, Ins::CmpHS),
				Rule::ins_cmp_pl => reg_inst(line, Ins::CmpPL),
				Rule::ins_cmp_pz => reg_inst(line, Ins::CmpPZ),
				Rule::ins_cmp_str => reg2_inst(line, Ins::CmpStr),
				Rule::ins_div0s => reg2_inst(line, Ins::Div0S),
				Rule::ins_div0u => Ins::Div0U,
				Rule::ins_div1 => reg2_inst(line, Ins::Div1),
				Rule::ins_dmuls => reg2_inst(line, Ins::DMulS),
				Rule::ins_dmulu => reg2_inst(line, Ins::DMulU),
				Rule::ins_dt => reg_inst(line, Ins::Dt),
				Rule::ins_extsb => size_reg_inst(line, Size::Byte, Ins::ExtS),
				Rule::ins_extsw => size_reg_inst(line, Size::Word, Ins::ExtS),
				Rule::ins_extub => size_reg_inst(line, Size::Byte, Ins::ExtU),
				Rule::ins_extuw => size_reg_inst(line, Size::Word, Ins::ExtU),
				Rule::ins_jmp => reg_inst(line, Ins::Jmp),
				Rule::ins_jsr => reg_inst(line, Ins::Jsr),
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
				Rule::ins_macw => reg2_inst(line, Ins::Mac_Word),
				Rule::ins_macl => reg2_inst(line, Ins::Mac_Long),
				Rule::ins_mov => continue,
				Rule::ins_movb => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_reg => {
							let src = disp_reg(src);
							assert_eq!(0, reg(args.next().unwrap()));
							Ins::Mov(Size::Byte, src, Arg::DirReg(0))
						}
						Rule::r0 => {
							assert_eq!(0, reg(src));
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Byte, Arg::DirReg(0), dst)
						}
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Byte, src, Arg::DirReg(dst))
						}
						_ => mov_common(Size::Byte, line),
					}
				}
				Rule::ins_movw => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_reg => {
							let src = disp_reg(src);
							assert_eq!(0, reg(args.next().unwrap()));
							Ins::Mov(Size::Word, src, Arg::DirReg(0))
						}
						Rule::r0 => {
							assert_eq!(0, reg(src));
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Word, Arg::DirReg(0), dst)
						}
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Word, src, Arg::DirReg(dst))
						}
						_ => mov_common(Size::Word, line),
					}
				}
				Rule::ins_movl => {
					let mut args = line.clone().into_inner();
					let src = args.next().unwrap();
					match src.as_rule() {
						Rule::disp_pc => {
							let src = disp_pc(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Long, src, Arg::DirReg(dst))
						}
						Rule::disp_reg => {
							let src = disp_reg(src);
							let dst = reg_or_sp(args.next().unwrap());
							Ins::Mov(Size::Long, src, Arg::DirReg(dst))
						}
						Rule::reg_or_sp => {
							let src = reg_or_sp(src);
							let dst = disp_reg(args.next().unwrap());
							Ins::Mov(Size::Long, Arg::DirReg(src), dst)
						}
						_ => mov_common(Size::Long, line),
					}
				}
				Rule::ins_mova => {
					let mut args = line.into_inner();
					let Arg::DispPC(imm) = disp_pc(args.next().unwrap()) else {
						unreachable!("expected disp_pc");
					};
					assert_eq!(0, reg(args.next().unwrap()));
					Ins::MovA(imm)
				}
				Rule::ins_movt => reg_inst(line, Ins::MovT),
				Rule::ins_mul => reg2_inst(line, Ins::Mul),
				Rule::ins_muls => reg2_inst(line, Ins::MulS),
				Rule::ins_mulu => reg2_inst(line, Ins::MulU),
				Rule::ins_neg => reg2_inst(line, Ins::Neg),
				Rule::ins_negc => reg2_inst(line, Ins::NegC),
				Rule::ins_nop => Ins::Nop,
				Rule::ins_not => reg2_inst(line, Ins::Not),
				Rule::ins_or_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Or_Byte(src)
				}
				Rule::ins_or_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Or_Imm(src)
				}
				Rule::ins_or_reg => reg2_inst(line, Ins::Or_Reg),
				Rule::ins_rotcl => reg_inst(line, Ins::RotCL),
				Rule::ins_rotcr => reg_inst(line, Ins::RotCR),
				Rule::ins_rotl => reg_inst(line, Ins::RotL),
				Rule::ins_rotr => reg_inst(line, Ins::RotR),
				Rule::ins_rte => Ins::Rte,
				Rule::ins_rts => Ins::Rts,
				Rule::ins_sett => Ins::SetT,
				Rule::ins_shal => reg_inst(line, Ins::ShAL),
				Rule::ins_shar => reg_inst(line, Ins::ShAR),
				Rule::ins_shll => reg_inst(line, Ins::ShLL),
				Rule::ins_shll16 => reg_inst(line, Ins::ShLL16),
				Rule::ins_shll2 => reg_inst(line, Ins::ShLL2),
				Rule::ins_shll8 => reg_inst(line, Ins::ShLL8),
				Rule::ins_shlr => reg_inst(line, Ins::ShLR),
				Rule::ins_shlr16 => reg_inst(line, Ins::ShLR16),
				Rule::ins_shlr2 => reg_inst(line, Ins::ShLR2),
				Rule::ins_shlr8 => reg_inst(line, Ins::ShLR8),
				Rule::ins_sleep => Ins::Sleep,
				Rule::ins_stc => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_or_sp(args.next().unwrap());
					match src.as_rule() {
						Rule::gbr => Ins::StcGBR(reg),
						Rule::sr => Ins::StcSR(reg),
						Rule::vbr => Ins::StcVBR(reg),
						_ => unreachable!("expected GBR, SR, or VBR for STC dst"),
					}
				}
				Rule::ins_stc_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_pre_dec(args.next().unwrap());
					match src.as_rule() {
						Rule::gbr => Ins::StcGBR_Dec(reg),
						Rule::sr => Ins::StcSR_Dec(reg),
						Rule::vbr => Ins::StcVBR_Dec(reg),
						_ => unreachable!("expected GBR, SR, or VBR for STC.L dst"),
					}
				}
				Rule::ins_sts => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_or_sp(args.next().unwrap());
					match src.as_rule() {
						Rule::macl => Ins::StsMACL(reg),
						Rule::mach => Ins::StsMACH(reg),
						Rule::pr => Ins::StsPR(reg),
						_ => unreachable!("expected MACL, MACH, PR for STS dst")
					}
				}
				Rule::ins_sts_dec => {
					let mut args = line.into_inner();
					let src = args.next().unwrap();
					let reg = reg_pre_dec(args.next().unwrap());
					match src.as_rule() {
						Rule::macl => Ins::StsMACL_Dec(reg),
						Rule::mach => Ins::StsMACH_Dec(reg),
						Rule::pr => Ins::StsPR_Dec(reg),
						_ => unreachable!("expected MACL, MACH, or PR for STS.L dst"),
					}
				}
				Rule::ins_sub => reg2_inst(line, Ins::Sub),
				Rule::ins_subc => reg2_inst(line, Ins::SubC),
				Rule::ins_subv => reg2_inst(line, Ins::SubV),
				Rule::ins_swapb => size_reg_inst(line, Size::Byte, Ins::Swap),
				Rule::ins_swapw => size_reg_inst(line, Size::Word, Ins::Swap),
				Rule::ins_tas => reg_inst(line, Ins::Tas),
				Rule::ins_trapa => {
					let mut args = line.into_inner();
					let imm = num8u(args.next().unwrap());
					Ins::TrapA(imm)
				}
				Rule::ins_tst_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Tst_Byte(src)
				}
				Rule::ins_tst_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Tst_Imm(src)
				}
				Rule::ins_tst_reg => reg2_inst(line, Ins::Tst_Reg),
				Rule::ins_xor_byt => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(
						Rule::disp_r0_gbr,
						args.next().unwrap().as_rule(),
						"expected @(R0,GBR) as AND dst",
					);
					Ins::Xor_Byte(src)
				}
				Rule::ins_xor_imm => {
					let mut args = line.into_inner();
					let src = num8u(args.next().unwrap());
					assert_eq!(0, reg_or_sp(args.next().unwrap()), "expected R0 as AND dst");
					Ins::Xor_Imm(src)
				}
				Rule::ins_xor_reg => reg2_inst(line, Ins::Xor_Reg),
				Rule::ins_xtrct => reg2_inst(line, Ins::Xtrct),
				Rule::val_line => {
					let mut args = line.into_inner();
					let label: Label = args.next().unwrap().as_str().into();
					let value = num32s(args.next().unwrap());
					if labels.contains_key(&label) {
						panic!("duplicate label: '{label}'");
					}
					labels.insert(label, LabelType::Value(value));
					continue;
				}
				Rule::lbl_line => {
					let s = line.as_str();
					let label: Label = s[..s.len()-1].into();
					if labels.contains_key(&label) {
						panic!("duplicate label: '{label}'");
					}
					labels.insert(label.clone(), LabelType::Label);
					Ins::Label(label)
				}
				Rule::EOI => continue,
				_ => unreachable!("unexpected token found: {line}"),
			};

			if let Some((_, repeat)) = &mut repetitions {
				repeat.push(ins);
			} else {
				let section = sections.entry(skey).or_default();
				section.push(ins);
			}
		}
	}

	if repetitions.is_some() {
		panic!("missing an .aendr directive");
	}

	let mut out_labels = HashMap::<Label, Option<u32>>::default();
	let mut out_values = HashMap::<Label, i32>::default();
	let mut unknown_label_count = 0;
	for (label, lbl_type) in labels {
		match lbl_type {
			LabelType::Unknown => {
				eprintln!("Unknown label: '{label}'");
				unknown_label_count += 1;
			}
			LabelType::Label => {
				out_labels.insert(label, None);
			}
			LabelType::Value(val) => {
				out_values.insert(label, val);
			}
		}
	}
	if unknown_label_count > 0 {
		panic!("{unknown_label_count} unknown labels");
	}

	Output {
		sections,
		labels: out_labels,
		values: out_values,
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

