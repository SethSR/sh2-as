
use std::collections::HashMap;

use tracing::{debug, error, instrument};

use crate::{Arg, Ins, Label, Reg, Size};
use crate::parser::{SectionMap, LabelMap, ValueMap};

// TODO - srenshaw - Time to start resolving word/long immediate placement and label addressing.
#[derive(Debug, Clone)]
pub enum Item {
	Byte(u8),
	Word(u16),
	Ins(Ins),
}

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

pub fn resolver(sections: &mut SectionMap<Item>, labels: &mut LabelMap, values: &ValueMap) {
	let mut is_unresolved = true;
	while is_unresolved {
		is_unresolved = false;
		for (section_base, section) in &mut *sections {
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
}

