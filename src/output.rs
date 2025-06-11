
use tracing::{instrument, warn};

use crate::asm::{Asm, Reg, Size};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Output {
	pub(crate) asm: Vec<Asm>,
	pub(crate) repeat: Option<(usize, Vec<Asm>)>,
}

impl Output {
	pub(crate) fn push(&mut self, asm: Asm) {
		if let Some((_,repeat)) = &mut self.repeat {
			repeat.push(asm);
		} else {
			self.asm.push(asm);
		}
	}
}

#[instrument]
pub fn output(asm: &[Asm]) -> Vec<u8> {
	fn push(base: u16, m: &Reg, n: &Reg, output: &mut Vec<u16>) {
		output.push(base | (*n as u16) << 8 | (*m as u16) << 4);
	}

	let mut out = Vec::with_capacity(asm.len());
	for cmd in asm {
		match cmd {
			Asm::ClrT       => out.push(0x0008),
			Asm::Nop        => out.push(0x0009),
			Asm::Rts        => out.push(0x000B),
			Asm::SetT       => out.push(0x0018),
			Asm::Div0U      => out.push(0x0019),
			Asm::Sleep      => out.push(0x001B),
			Asm::ClrMac     => out.push(0x0028),
			Asm::Rte        => out.push(0x002B),

			Asm::Bf(d)      => out.push(0x8B00 | *d as u8 as u16),
			Asm::BfS(d)     => out.push(0x8F00 | *d as u8 as u16),
			Asm::Bra(d)     => out.push(0xA000 | (*d & 0xFFF) as u16),
			Asm::BraF(r)    => out.push(0x0023 | (*r as u16) << 8),
			Asm::Bsr(d)     => out.push(0xB000 | (*d & 0xFFF) as u16),
			Asm::BsrF(r)    => out.push(0x0003 | (*r as u16) << 8),
			Asm::Bt(d)      => out.push(0x8900 | *d as u8 as u16),
			Asm::BtS(d)     => out.push(0x8D00 | *d as u8 as u16),
			Asm::Dt(r)      => out.push(0x4010 | (*r as u16) << 8),
			Asm::Jmp(r)     => out.push(0x402B | (*r as u16) << 8),
			Asm::Jsr(r)     => out.push(0x400B | (*r as u16) << 8),
			Asm::MovA(d)    => out.push(0xC700 | *d as u16),
			Asm::MovT(r)    => out.push(0x0029 | (*r as u16) << 8),
			Asm::RotCL(r)   => out.push(0x4024 | (*r as u16) << 8),
			Asm::RotCR(r)   => out.push(0x4025 | (*r as u16) << 8),
			Asm::RotL(r)    => out.push(0x4004 | (*r as u16) << 8),
			Asm::RotR(r)    => out.push(0x4005 | (*r as u16) << 8),
			Asm::ShAL(r)    => out.push(0x4020 | (*r as u16) << 8),
			Asm::ShAR(r)    => out.push(0x4021 | (*r as u16) << 8),
			Asm::ShLL(r)    => out.push(0x4000 | (*r as u16) << 8),
			Asm::ShLL2(r)   => out.push(0x4008 | (*r as u16) << 8),
			Asm::ShLL8(r)   => out.push(0x4018 | (*r as u16) << 8),
			Asm::ShLL16(r)  => out.push(0x4028 | (*r as u16) << 8),
			Asm::ShLR(r)    => out.push(0x4001 | (*r as u16) << 8),
			Asm::ShLR2(r)   => out.push(0x4009 | (*r as u16) << 8),
			Asm::ShLR8(r)   => out.push(0x4019 | (*r as u16) << 8),
			Asm::ShLR16(r)  => out.push(0x4029 | (*r as u16) << 8),
			Asm::TaS(r)     => out.push(0x401B | (*r as u16) << 8),
			Asm::TrapA(i)   => out.push(0xC300 | *i as u16),

			Asm::AddC(m,n)      => push(0x300E, m, n, &mut out),
			Asm::AddV(m,n)      => push(0x300F, m, n, &mut out),
			Asm::Div0S(m,n)     => push(0x2007, m, n, &mut out),
			Asm::Div1(m,n)      => push(0x3004, m, n, &mut out),
			Asm::ExtSByte(m,n)  => push(0x600E, m, n, &mut out),
			Asm::ExtSWord(m,n)  => push(0x600F, m, n, &mut out),
			Asm::ExtUByte(m,n)  => push(0x600C, m, n, &mut out),
			Asm::ExtUWord(m,n)  => push(0x600D, m, n, &mut out),
			Asm::MacWord(m,n)   => push(0x400F, m, n, &mut out),
			Asm::MacLong(m,n)   => push(0x000F, m, n, &mut out),
			Asm::Neg(m,n)       => push(0x600B, m, n, &mut out),
			Asm::NegC(m,n)      => push(0x600A, m, n, &mut out),
			Asm::Not(m,n)       => push(0x6007, m, n, &mut out),
			Asm::Sub(m,n)       => push(0x3008, m, n, &mut out),
			Asm::SubC(m,n)      => push(0x300A, m, n, &mut out),
			Asm::SubV(m,n)      => push(0x300B, m, n, &mut out),
			Asm::SwapByte(m,n)  => push(0x6008, m, n, &mut out),
			Asm::SwapWord(m,n)  => push(0x6009, m, n, &mut out),
			Asm::Xtrct(m,n)     => push(0x200D, m, n, &mut out),

			Asm::AndReg(m,n)    => push(0x2009, m, n, &mut out),
			Asm::AndImm(i)  => out.push(0xC900 | *i as u16),
			Asm::AndByte(i) => out.push(0xCD00 | *i as u16),
			Asm::OrReg(m,n)     => push(0x200B, m, n, &mut out),
			Asm::OrImm(i)   => out.push(0xCB00 | *i as u16),
			Asm::OrByte(i)  => out.push(0xCF00 | *i as u16),
			Asm::TstReg(m,n)    => push(0x2008, m, n, &mut out),
			Asm::TstImm(i)  => out.push(0xC800 | *i as u16),
			Asm::TstByte(i) => out.push(0xCC00 | *i as u16),
			Asm::XorReg(m,n)    => push(0x200A, m, n, &mut out),
			Asm::XorImm(i)  => out.push(0xCA00 | *i as u16),
			Asm::XorByte(i) => out.push(0xCE00 | *i as u16),

			Asm::CmpEqReg(m,n)   => push(0x3000, m, n, &mut out),
			Asm::CmpGE(m,n)      => push(0x3003, m, n, &mut out),
			Asm::CmpGT(m,n)      => push(0x3007, m, n, &mut out),
			Asm::CmpHI(m,n)      => push(0x3006, m, n, &mut out),
			Asm::CmpHS(m,n)      => push(0x3002, m, n, &mut out),
			Asm::CmpPL(r)    => out.push(0x4015 | (*r as u16) << 8),
			Asm::CmpPZ(r)    => out.push(0x4011 | (*r as u16) << 8),
			Asm::CmpSTR(m,n)     => push(0x200C, m, n, &mut out),
			Asm::CmpEqImm(i) => out.push(0x8800 | *i as u8 as u16),

			Asm::AddImm(i,r)   => out.push(0x7000 | (*r as u16) << 8 | *i as u8 as u16),
			Asm::AddReg(m,n)   =>     push(0x300C, m, n, &mut out),
			Asm::Mul(m,n)      =>     push(0x0007, m, n, &mut out),
			Asm::MulS(m,n)     =>     push(0x200F, m, n, &mut out),
			Asm::MulU(m,n)     =>     push(0x200E, m, n, &mut out),
			Asm::LdcGbr(r)     => out.push(0x401E | (*r as u16) << 8),
			Asm::LdcSr(r)      => out.push(0x400E | (*r as u16) << 8),
			Asm::LdcVbr(r)     => out.push(0x402E | (*r as u16) << 8),
			Asm::LdcGbrInc(r)  => out.push(0x4017 | (*r as u16) << 8),
			Asm::LdcSrInc(r)   => out.push(0x4007 | (*r as u16) << 8),
			Asm::LdcVbrInc(r)  => out.push(0x4027 | (*r as u16) << 8),
			Asm::LdsMach(r)    => out.push(0x400A | (*r as u16) << 8),
			Asm::LdsMacl(r)    => out.push(0x401A | (*r as u16) << 8),
			Asm::LdsPr(r)      => out.push(0x402A | (*r as u16) << 8),
			Asm::LdsMachInc(r) => out.push(0x4006 | (*r as u16) << 8),
			Asm::LdsMaclInc(r) => out.push(0x4016 | (*r as u16) << 8),
			Asm::LdsPrInc(r)   => out.push(0x4026 | (*r as u16) << 8),
			Asm::StcGbr(r)     => out.push(0x0012 | (*r as u16) << 8),
			Asm::StcSr(r)      => out.push(0x0002 | (*r as u16) << 8),
			Asm::StcVbr(r)     => out.push(0x0022 | (*r as u16) << 8),
			Asm::StcGbrDec(r)  => out.push(0x4013 | (*r as u16) << 8),
			Asm::StcSrDec(r)   => out.push(0x4003 | (*r as u16) << 8),
			Asm::StcVbrDec(r)  => out.push(0x4023 | (*r as u16) << 8),
			Asm::StsMach(r)    => out.push(0x000A | (*r as u16) << 8),
			Asm::StsMacl(r)    => out.push(0x001A | (*r as u16) << 8),
			Asm::StsPr(r)      => out.push(0x002A | (*r as u16) << 8),
			Asm::StsMachDec(r) => out.push(0x4002 | (*r as u16) << 8),
			Asm::StsMaclDec(r) => out.push(0x4012 | (*r as u16) << 8),
			Asm::StsPrDec(r)   => out.push(0x4022 | (*r as u16) << 8),
			Asm::DMulS(m,n)    =>     push(0x300D, m, n, &mut out),
			Asm::DMulU(m,n)    =>     push(0x3005, m, n, &mut out),

			Asm::MovImm(i,r)                => out.push(0xE000 | (*r as u16) << 8 | *i as u16),
			Asm::MovReg(m,n)                    => push(0x6003, m, n, &mut out),
			Asm::MovAddrToReg(Size::Byte,m,n)   => push(0x6000, m, n, &mut out),
			Asm::MovAddrToReg(Size::Word,m,n)   => push(0x6001, m, n, &mut out),
			Asm::MovAddrToReg(Size::Long,m,n)   => push(0x6002, m, n, &mut out),
			Asm::MovRegToAddr(Size::Byte,m,n)   => push(0x2000, m, n, &mut out),
			Asm::MovRegToAddr(Size::Word,m,n)   => push(0x2001, m, n, &mut out),
			Asm::MovRegToAddr(Size::Long,m,n)   => push(0x2002, m, n, &mut out),
			Asm::MovGbrToR0(Size::Byte,d)   => out.push(0xC400 | *d as u16),
			Asm::MovGbrToR0(Size::Word,d)   => out.push(0xC500 | *d as u16),
			Asm::MovGbrToR0(Size::Long,d)   => out.push(0xC600 | *d as u16),
			Asm::MovR0ToGbr(Size::Byte,d)   => out.push(0xC000 | *d as u16),
			Asm::MovR0ToGbr(Size::Word,d)   => out.push(0xC100 | *d as u16),
			Asm::MovR0ToGbr(Size::Long,d)   => out.push(0xC200 | *d as u16),
			Asm::MovDispR0ToReg(Size::Byte,m,n) => push(0x000C, m, n, &mut out),
			Asm::MovDispR0ToReg(Size::Word,m,n) => push(0x000D, m, n, &mut out),
			Asm::MovDispR0ToReg(Size::Long,m,n) => push(0x000E, m, n, &mut out),
			Asm::MovRegToDispR0(Size::Byte,m,n) => push(0x0004, m, n, &mut out),
			Asm::MovRegToDispR0(Size::Word,m,n) => push(0x0005, m, n, &mut out),
			Asm::MovRegToDispR0(Size::Long,m,n) => push(0x0006, m, n, &mut out),
			Asm::MovIncToReg(Size::Byte,m,n)    => push(0x6004, m, n, &mut out),
			Asm::MovIncToReg(Size::Word,m,n)    => push(0x6005, m, n, &mut out),
			Asm::MovIncToReg(Size::Long,m,n)    => push(0x6006, m, n, &mut out),
			Asm::MovRegToDec(Size::Byte,m,n)    => push(0x2004, m, n, &mut out),
			Asm::MovRegToDec(Size::Word,m,n)    => push(0x2005, m, n, &mut out),
			Asm::MovRegToDec(Size::Long,m,n)    => push(0x2006, m, n, &mut out),

			Asm::MovByteDispRegToR0(d,m)    => out.push(0x8400 | (*m as u16) << 4 | *d),
			Asm::MovByteR0ToDispReg(d,n)    => out.push(0x8000 | (*n as u16) << 4 | *d),
			Asm::MovWordDispRegToR0(d,m)    => out.push(0x8500 | (*m as u16) << 4 | *d),
			Asm::MovWordR0ToDispReg(d,n)    => out.push(0x8100 | (*n as u16) << 4 | *d),
			Asm::MovWordDispPCToReg(d,n)    => out.push(0x9000 | (*n as u16) << 8 | *d as u16),
			Asm::MovLongDispPCToReg(d,n)    => out.push(0xD000 | (*n as u16) << 8 | *d as u16),
			Asm::MovLongDispRegToReg(d,m,n) => out.push(0x5000 | (*n as u16) << 8 | (*m as u16) << 4 | *d),
			Asm::MovLongRegToDispReg(m,d,n) => out.push(0x1000 | (*n as u16) << 8 | (*m as u16) << 4 | *d),

			Asm::Byte(b) => {
				warn!("placing a single byte with padding: {b}");
				out.push((*b as u16) << 8);
			}
			Asm::Word(w) => out.push(*w),
			Asm::Long(l) => {
				out.push((l >> 16) as u16);
				out.push(*l as u16);
			}
		}
	}

	out.into_iter()
		.flat_map(|word: u16| word.to_be_bytes())
		.collect()
}

#[cfg(test)]
mod tests {
	macro_rules! test_output {
		($name:ident, $input:expr, $bytes:expr) => {
			#[test]
			fn $name() {
				let asm = crate::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				let out = crate::output::output(&asm);
				assert_eq!(out, $bytes);
			}
		};
	}

	test_output!(clrmac, "\tclrmac", &[0x00, 0x28]);
	test_output!(clrt,   "\tclrt",   &[0x00, 0x08]);
	test_output!(div0u,  "\tdiv0u",  &[0x00, 0x19]);
	test_output!(nop,    "\tnop",    &[0x00, 0x09]);
	test_output!(rte,    "\trte",    &[0x00, 0x2B]);
	test_output!(rts,    "\trts",    &[0x00, 0x0B]);
	test_output!(sett,   "\tsett",   &[0x00, 0x18]);
	test_output!(sleep,  "\tsleep",  &[0x00, 0x1B]);

	test_output!(bf,     "\tbf @($78,pc)",      &[0x8B, 0x78]);
	test_output!(bfs,    "\tbf/s @($52,pc)",    &[0x8F, 0x52]);
	test_output!(bra,    "\tbra @(-2000,pc)",   &[0xA8, 0x30]);
	test_output!(braf,   "\tbraf @sp",          &[0x0F, 0x23]);
	test_output!(bsr,    "\tbsr @($63D,pc)",    &[0xB6, 0x3D]);
	test_output!(bsrf,   "\tbsrf @r8",          &[0x08, 0x03]);
	test_output!(bt,     "\tbt @(100,pc)",      &[0x89, 0x64]);
	test_output!(bts,    "\tbt/s @(%100,pc)",   &[0x8D, 0x04]);
	test_output!(dt,     "\tdt r10",            &[0x4A, 0x10]);
	test_output!(jmp,    "\tjmp @r13",          &[0x4D, 0x2B]);
	test_output!(jsr,    "\tjsr @r3",           &[0x43, 0x0B]);
	test_output!(mova,   "\tmova @($80,pc),r0", &[0xC7, 0x80]);
	test_output!(movt,   "\tmovt r6",           &[0x06, 0x29]);
	test_output!(rotcl,  "\trotcl r15",         &[0x4F, 0x24]);
	test_output!(rotcr,  "\trotcr r14",         &[0x4E, 0x25]);
	test_output!(rotl,   "\trotl r4",           &[0x44, 0x04]);
	test_output!(rotr,   "\trotr r5",           &[0x45, 0x05]);
	test_output!(shal,   "\tshal r8",           &[0x48, 0x20]);
	test_output!(shar,   "\tshar r9",           &[0x49, 0x21]);
	test_output!(shll,   "\tshll r0",           &[0x40, 0x00]);
	test_output!(shll2,  "\tshll2 r2",          &[0x42, 0x08]);
	test_output!(shll8,  "\tshll8 r8",          &[0x48, 0x18]);
	test_output!(shll16, "\tshll16 r15",        &[0x4F, 0x28]);
	test_output!(shlr,   "\tshlr r0",           &[0x40, 0x01]);
	test_output!(shlr2,  "\tshlr2 r2",          &[0x42, 0x09]);
	test_output!(shlr8,  "\tshlr8 r8",          &[0x48, 0x19]);
	test_output!(shlr16, "\tshlr16 r15",        &[0x4F, 0x29]);
	test_output!(tas,    "\ttas.b @sp",         &[0x4F, 0x1B]);
	test_output!(trapa,  "\ttrapa #240",        &[0xC3, 0xF0]);

	test_output!(addc,   "\taddc r12, r8",    &[0x38, 0xCE]);
	test_output!(addv,   "\taddv r4, r11",    &[0x3B, 0x4F]);
	test_output!(div0s,  "\tdiv0s r14,r13",   &[0x2D, 0xE7]);
	test_output!(div1,   "\tdiv1 r1,r1",      &[0x31, 0x14]);
	test_output!(extsb,  "\texts.b r15,r0",   &[0x60, 0xFE]);
	test_output!(extsw,  "\texts.w r15,r1",   &[0x61, 0xFF]);
	test_output!(extub,  "\textu.b r15,r0",   &[0x60, 0xFC]);
	test_output!(extuw,  "\textu.w r15,r1",   &[0x61, 0xFD]);
	test_output!(macw,   "\tmac.w @sp+,@r3+", &[0x43, 0xFF]);
	test_output!(macl,   "\tmac.l @sp+,@r3+", &[0x03, 0xFF]);
	test_output!(neg,    "\tneg r4,sp",       &[0x6F, 0x4B]);
	test_output!(negc,   "\tnegc sp,r8",      &[0x68, 0xFA]);
	test_output!(not,    "\tnot r6,r6",       &[0x66, 0x67]);
	test_output!(sub,    "\tsub r3,r3",       &[0x33, 0x38]);
	test_output!(subc,   "\tsubc r3,r3",      &[0x33, 0x3A]);
	test_output!(subv,   "\tsubv r3,r3",      &[0x33, 0x3B]);
	test_output!(swapb,  "\tswap.b r7,r2",    &[0x62, 0x78]);
	test_output!(swapw,  "\tswap.w r8,r3",    &[0x63, 0x89]);
	test_output!(xtrct,  "\txtrct r11,r10",   &[0x2A, 0xBD]);

	test_output!(and,    "\tand r2,r3",            &[0x23, 0x29]);
	test_output!(andi,   "\tand #25,r0",           &[0xC9, 0x19]);
	test_output!(andb,   "\tand.b #$EE,@(r0,gbr)", &[0xCD, 0xEE]);
	test_output!(or,     "\tor r2,r3",             &[0x23, 0x2B]);
	test_output!(ori,    "\tor #25,r0",            &[0xCB, 0x19]);
	test_output!(orb,    "\tor.b #$EE,@(r0,gbr)",  &[0xCF, 0xEE]);
	test_output!(tst,    "\ttst r2,r3",            &[0x23, 0x28]);
	test_output!(tsti,   "\ttst #25,r0",           &[0xC8, 0x19]);
	test_output!(tstb,   "\ttst.b #$EE,@(r0,gbr)", &[0xCC, 0xEE]);
	test_output!(xor,    "\txor r2,r3",            &[0x23, 0x2A]);
	test_output!(xori,   "\txor #25,r0",           &[0xCA, 0x19]);
	test_output!(xorb,   "\txor.b #$EE,@(r0,gbr)", &[0xCE, 0xEE]);

	test_output!(cmpeqi, "\tcmp/eq #$7F,r0",  &[0x88, 0x7F]);
	test_output!(cmpeqr, "\tcmp/eq r1,r2",    &[0x32, 0x10]);
	test_output!(cmpge,  "\tcmp/ge r3,r4",    &[0x34, 0x33]);
	test_output!(cmpgt,  "\tcmp/gt r5,r6",    &[0x36, 0x57]);
	test_output!(cmphi,  "\tcmp/hi r7,r8",    &[0x38, 0x76]);
	test_output!(cmphs,  "\tcmp/hs r9,r10",   &[0x3A, 0x92]);
	test_output!(cmpstr, "\tcmp/str r11,r12", &[0x2C, 0xBC]);
	test_output!(cmppl,  "\tcmp/pl r13",      &[0x4D, 0x15]);
	test_output!(cmppz,  "\tcmp/pz r14",      &[0x4E, 0x11]);

	test_output!(add,      "\tadd r6,r7",       &[0x37, 0x6C]);
	test_output!(addi,     "\tadd #37,r4",      &[0x74, 0x25]);
	test_output!(mul,      "\tmul.l r0,sp",     &[0x0F, 0x07]);
	test_output!(muls,     "\tmuls.w r14,r13",  &[0x2D, 0xEF]);
	test_output!(mulu,     "\tmulu.w r11,r12",  &[0x2C, 0xBE]);
	test_output!(ldcgbr,   "\tldc r9,gbr",      &[0x49, 0x1E]);
	test_output!(ldcsr,    "\tldc r9,sr",       &[0x49, 0x0E]);
	test_output!(ldcvbr,   "\tldc r9,vbr",      &[0x49, 0x2E]);
	test_output!(ldcgbr2,  "\tldc.l @r7+,gbr",  &[0x47, 0x17]);
	test_output!(ldcsr2,   "\tldc.l @r7+,sr",   &[0x47, 0x07]);
	test_output!(ldcvbr2,  "\tldc.l @r7+,vbr",  &[0x47, 0x27]);
	test_output!(ldsmach,  "\tlds r4,mach",     &[0x44, 0x0A]);
	test_output!(ldsmacl,  "\tlds r4,macl",     &[0x44, 0x1A]);
	test_output!(ldspr,    "\tlds r4,pr",       &[0x44, 0x2A]);
	test_output!(ldsmach2, "\tlds.l @r2+,mach", &[0x42, 0x06]);
	test_output!(ldsmacl2, "\tlds.l @r2+,macl", &[0x42, 0x16]);
	test_output!(ldspr2,   "\tlds.l @r2+,pr",   &[0x42, 0x26]);
	test_output!(stcgbr,   "\tstc gbr,r4",      &[0x04, 0x12]);
	test_output!(stcsr,    "\tstc sr,r4",       &[0x04, 0x02]);
	test_output!(stcvbr,   "\tstc vbr,r4",      &[0x04, 0x22]);
	test_output!(stcgbr2,  "\tstc.l gbr,@-r5",  &[0x45, 0x13]);
	test_output!(stcsr2,   "\tstc.l sr,@-r5",   &[0x45, 0x03]);
	test_output!(stcvbr2,  "\tstc.l vbr,@-r5",  &[0x45, 0x23]);
	test_output!(stsmach,  "\tsts mach,r4",     &[0x04, 0x0A]);
	test_output!(stsmacl,  "\tsts macl,r4",     &[0x04, 0x1A]);
	test_output!(stspr,    "\tsts pr,r4",       &[0x04, 0x2A]);
	test_output!(stsmach2, "\tsts.l mach,@-r5", &[0x45, 0x02]);
	test_output!(stsmacl2, "\tsts.l macl,@-r5", &[0x45, 0x12]);
	test_output!(stspr2,   "\tsts.l pr,@-r5",   &[0x45, 0x22]);
	test_output!(dmuls,    "\tdmuls.l r0,r0",   &[0x30, 0x0D]);
	test_output!(dmulu,    "\tdmulu.l r0,r0",   &[0x30, 0x05]);

	test_output!(movi,   "\tmov #78,R3",         &[0xE3, 0x4E]);
	test_output!(mov,    "\tmov r13,r9",         &[0x69, 0xD3]);
	test_output!(movbar, "\tmov.b @r7,r9",       &[0x69, 0x70]);
	test_output!(movwar, "\tmov.w @r7,r9",       &[0x69, 0x71]);
	test_output!(movlar, "\tmov.l @r7,r9",       &[0x69, 0x72]);
	test_output!(movbra, "\tmov.b r3,@r4",       &[0x24, 0x30]);
	test_output!(movwra, "\tmov.w r3,@r4",       &[0x24, 0x31]);
	test_output!(movlra, "\tmov.l r3,@r4",       &[0x24, 0x32]);
	test_output!(movbg0, "\tmov.b @(45,gbr),r0", &[0xC4, 0x2D]);
	test_output!(movwg0, "\tmov.w @(45,gbr),r0", &[0xC5, 0x2D]);
	test_output!(movlg0, "\tmov.l @(45,gbr),r0", &[0xC6, 0x2D]);
	test_output!(movb0g, "\tmov.b r0,@(35,gbr)", &[0xC0, 0x23]);
	test_output!(movw0g, "\tmov.w r0,@(35,gbr)", &[0xC1, 0x23]);
	test_output!(movl0g, "\tmov.l r0,@(35,gbr)", &[0xC2, 0x23]);
	test_output!(movbd0, "\tmov.b @(r0,r2),r6",  &[0x06, 0x2C]);
	test_output!(movwd0, "\tmov.w @(r0,r2),r6",  &[0x06, 0x2D]);
	test_output!(movld0, "\tmov.l @(r0,r2),r6",  &[0x06, 0x2E]);
	test_output!(movb0d, "\tmov.b r7,@(r0,r5)",  &[0x05, 0x74]);
	test_output!(movw0d, "\tmov.w r7,@(r0,r5)",  &[0x05, 0x75]);
	test_output!(movl0d, "\tmov.l r7,@(r0,r5)",  &[0x05, 0x76]);
	test_output!(movbir, "\tmov.b @r11+,r14",    &[0x6E, 0xB4]);
	test_output!(movwir, "\tmov.w @r11+,r14",    &[0x6E, 0xB5]);
	test_output!(movlir, "\tmov.l @r11+,r14",    &[0x6E, 0xB6]);
	test_output!(movbrd, "\tmov.b r15,@-sp",     &[0x2F, 0xF4]);
	test_output!(movwrd, "\tmov.w r15,@-sp",     &[0x2F, 0xF5]);
	test_output!(movlrd, "\tmov.l r15,@-sp",     &[0x2F, 0xF6]);

	test_output!(movbr0, "\tmov.b @(-7,r2),r0",  &[0x84, 0x29]);
	test_output!(movb0r, "\tmov.b r0,@(4,r5)",   &[0x80, 0x54]);
	test_output!(movwr0, "\tmov.w @(-3,r4),r0",  &[0x85, 0x4D]);
	test_output!(movw0r, "\tmov.w r0,@(1,r6)",   &[0x81, 0x61]);
	test_output!(movwpr, "\tmov.w @(89,pc),r3",  &[0x93, 0x59]);
	test_output!(movlpr, "\tmov.l @(34,pc),r7",  &[0xD7, 0x22]);
	test_output!(movlr2, "\tmov.l @(6,r4),r2",   &[0x52, 0x46]);
	test_output!(movl2r, "\tmov.l r1,@(-2,r1)",  &[0x11, 0x1E]);
}

