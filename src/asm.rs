
use std::fmt;

use tracing::{instrument, warn};

use crate::i4::I4;

pub(crate) type Reg = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size { Byte, Word, Long }

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Asm {
	///               $0028 | CLRMAC
	ClrMac,
	///               $0008 | CLRT
	ClrT,
	///               $0019 | DIV0U
	Div0U,
	///               $0009 | NOP
	Nop,
	///               $002B | RTE
	Rte,
	///               $000B | RTS
	Rts,
	///               $0018 | SETT
	SetT,
	///               $001B | SLEEP
	Sleep,

	///               $8Bdd | BF label
	Bf(i8),
	///               $8Fdd | BF/S label
	BfS(i8),
	///               $Addd | BRA label
	Bra(i16),
	///               $0m23 | BRAF Rm
	BraF(Reg),
	///               $Bddd | BSR label
	Bsr(i16),
	///               $0m03 | BSRF Rm
	BsrF(Reg),
	///               $89dd | BT label
	Bt(i8),
	///               $8Ddd | BT/S label
	BtS(i8),
	///               $4n10 | DT Rn
	Dt(Reg),
	///               $4m2B | JMP @Rm
	Jmp(Reg),
	///               $4m0B | JSR @Rm
	Jsr(Reg),
	///               $C7dd | MOVA @(disp,PC),R0
	MovA(u8),
	///               $0n29 | MOVT Rn
	MovT(Reg),
	///               $4n24 | ROTCL Rn
	RotCL(Reg),
	///               $4n25 | ROTCR Rn
	RotCR(Reg),
	///               $4n04 | ROTL Rn
	RotL(Reg),
	///               $4n05 | ROTR Rn
	RotR(Reg),
	///               $4n20 | SHAL Rn
	ShAL(Reg),
	///               $4n21 | SHAR Rn
	ShAR(Reg),
	///               $4n00 | SHLL Rn
	ShLL(Reg),
	///               $4n08 | SHLL2 Rn
	ShLL2(Reg),
	///               $4n18 | SHLL8 Rn
	ShLL8(Reg),
	///               $4n28 | SHLL16 Rn
	ShLL16(Reg),
	///               $4n01 | SHLR Rn
	ShLR(Reg),
	///               $4n09 | SHLR2 Rn
	ShLR2(Reg),
	///               $4n19 | SHLR8 Rn
	ShLR8(Reg),
	///               $4n29 | SHLR16 Rn
	ShLR16(Reg),
	///               $4n1B | TAS.B @Rn
	TaS(Reg),
	///               $C3ii | TRAPA #imm
	TrapA(u8),

	///               $3nmE | ADDC Rm,Rn
	AddC(Reg, Reg),
	///               $3nmF | ADDV Rm,Rn
	AddV(Reg, Reg),
	///               $2nm7 | DIV0S Rm,Rn
	Div0S(Reg, Reg),
	///               $3nm4 | DIV1 Rm,Rn
	Div1(Reg, Reg),
	///               $6nmE | EXTS.B Rm,Rn
	ExtSByte(Reg, Reg),
	///               $6nmF | EXTS.W Rm,Rn
	ExtSWord(Reg, Reg),
	///               $6nmC | EXTU.B Rm,Rn
	ExtUByte(Reg, Reg),
	///               $6nmD | EXTU.W Rm,Rn
	ExtUWord(Reg, Reg),
	///               $4nmF | MAC.W Rm,Rn
	MacWord(Reg, Reg),
	///               $0nmF | MAC.L Rm,Rn
	MacLong(Reg, Reg),
	///               $6nmB | NEG Rm,Rn
	Neg(Reg, Reg),
	///               $6nmA | NEGC Rm,Rn
	NegC(Reg, Reg),
	///               $6nm7 | NOT Rm,Rn
	Not(Reg, Reg),
	///               $3nm8 | SUB Rm,Rn
	Sub(Reg, Reg),
	///               $3nmA | SUBC Rm,Rn
	SubC(Reg, Reg),
	///               $3nmB | SUBV Rm,Rn
	SubV(Reg, Reg),
	///               $6nm8 | SWAP.B Rm,Rn
	SwapByte(Reg, Reg),
	///               $6nm9 | SWAP.W Rm,Rn
	SwapWord(Reg, Reg),
	///               $2nmD | XTRCT Rm,Rn
	Xtrct(Reg, Reg),

	///               $2nm9 | AND Rm,Rn
	AndReg(Reg, Reg),
	///               $C9ii | AND #imm,R0
	AndImm(u8),
	///               $CDii | AND.B #imm,@(R0,GBR)
	AndByte(u8),
	///               $2nmB | OR Rm,Rn
	OrReg(Reg, Reg),
	///               $CBii | OR #imm,R0
	OrImm(u8),
	///               $CFii | OR.B #imm,@(R0,GBR)
	OrByte(u8),
	///               $2nm8 | TST Rm,Rn
	TstReg(Reg, Reg),
	///               $C8ii | TST #imm,R0
	TstImm(u8),
	///               $CCii | TST.B #imm,@(R0,GBR)
	TstByte(u8),
	///               $2nmA | XOR Rm,Rn
	XorReg(Reg, Reg),
	///               $CAii | XOR #imm,R0
	XorImm(u8),
	///               $CEii | XOR.B #imm,@(R0,GBR)
	XorByte(u8),

	///               $88ii | CMP/EQ #imm,R0
	CmpEqImm(i8),
	///               $3nm0 | CMP/EQ Rm,Rn
	CmpEqReg(Reg, Reg),
	///               $3nm3 | CMP/GE Rm,Rn
	CmpGE(Reg, Reg),
	///               $3nm7 | CMP/GT Rm,Rn
	CmpGT(Reg, Reg),
	///               $3nm6 | CMP/HI Rm,Rn
	CmpHI(Reg, Reg),
	///               $3nm2 | CMP/HS Rm,Rn
	CmpHS(Reg, Reg),
	///               $2nmC | CMP/STR Rm,Rn
	CmpSTR(Reg, Reg),
	///               $4n15 | CMP/PL Rn
	CmpPL(Reg),
	///               $4n11 | CMP/PZ Rn
	CmpPZ(Reg),

	///               $7nii | ADD #imm,Rn
	AddImm(i8, Reg),
	///               $3nmC | ADD Rm,Rn
	AddReg(Reg, Reg),
	///               $0nm7 | MUL.L Rm,Rn
	Mul(Reg, Reg),
	///               $2nmF | MULS.W Rm,Rn
	MulS(Reg, Reg),
	///               $2nmE | MULU.W Rm,Rn
	MulU(Reg, Reg),
	///               $4m1E | LDC Rm,GBR
	LdcGbr(Reg),
	///               $4m0E | LDC Rm,SR
	LdcSr(Reg),
	///               $4m2E | LDC Rm,VBR
	LdcVbr(Reg),
	///               $4m17 | LDC.L @Rm+,GBR
	LdcGbrInc(Reg),
	///               $4m07 | LDC.L @Rm+,SR
	LdcSrInc(Reg),
	///               $4m27 | LDC.L @Rm+,VBR
	LdcVbrInc(Reg),
	///               $4m0A | LDS Rm,MACH
	LdsMach(Reg),
	///               $4m1A | LDS Rm,MACL
	LdsMacl(Reg),
	///               $4m2A | LDS Rm,PR
	LdsPr(Reg),
	///               $4m06 | LDS.L @Rm+,MACH
	LdsMachInc(Reg),
	///               $4m16 | LDS.L @Rm+,MACL
	LdsMaclInc(Reg),
	///               $4m26 | LDS.L @Rm+,PR
	LdsPrInc(Reg),

	///               $0n12 | STC GBR,Rn
	StcGbr(Reg),
	///               $0n02 | STC SR,Rn
	StcSr(Reg),
	///               $0n22 | STC VBR,Rn
	StcVbr(Reg),
	///               $4n12 | STC.L GBR,@-Rn
	StcGbrDec(Reg),
	///               $4n02 | STC.L SR,@-Rn
	StcSrDec(Reg),
	///               $4n22 | STC.L VBR,@-Rn
	StcVbrDec(Reg),
	///               $0n0A | STS MACH,Rn
	StsMach(Reg),
	///               $0n1A | STS MACL,Rn
	StsMacl(Reg),
	///               $0n2A | STS PR,Rn
	StsPr(Reg),
	///               $4n0A | STS.L MACH,Rn
	StsMachDec(Reg),
	///               $4n1A | STS.L MACL,Rn
	StsMaclDec(Reg),
	///               $4n2A | STS.L PR,Rn
	StsPrDec(Reg),

	///               $3nmD | DMULS.L Rm,Rn
	DMulS(Reg, Reg),
	///               $3nm5 | DMULU.L Rm,Rn
	DMulU(Reg, Reg),

	///                              $Enii | MOV #imm,Rn
	MovImm(i8, Reg),
	///                              $6nm3 | MOV Rm,Rn
	MovReg(Reg, Reg),
	///                              $6nm0 | MOV.B @Rm,Rn
	///                              $6nm1 | MOV.W @Rm,Rn
	///                              $6nm2 | MOV.L @Rm,Rn
	MovAddrToReg(Size, Reg, Reg),
	///                              $2nm0 | MOV.B Rm,@Rn
	///                              $2nm1 | MOV.W Rm,@Rn
	///                              $2nm2 | MOV.L Rm,@Rn
	MovRegToAddr(Size, Reg, Reg),
	///                              $C4dd | MOV.B @(disp,GBR),R0
	///                              $C5dd | MOV.W @(disp,GBR),R0
	///                              $C6dd | MOV.W @(disp,GBR),R0
	MovGbrToR0(Size, i8),
	///                              $C0dd | MOV.B R0,@(disp,GBR)
	///                              $C1dd | MOV.W R0,@(disp,GBR)
	///                              $C2dd | MOV.L R0,@(disp,GBR)
	MovR0ToGbr(Size, i8),
	///                              $0nmC | MOV.B @(R0,Rm),Rn
	///                              $0nmD | MOV.W @(R0,Rm),Rn
	///                              $0nmE | MOV.L @(R0,Rm),Rn
	MovDispR0ToReg(Size, Reg, Reg),
	///                              $0nm4 | MOV.B Rm,@(R0,Rn)
	///                              $0nm5 | MOV.W Rm,@(R0,Rn)
	///                              $0nm6 | MOV.L Rm,@(R0,Rn)
	MovRegToDispR0(Size, Reg, Reg),
	///                              $6nm4 | MOV.B @Rm+,Rn
	///                              $6nm5 | MOV.W @Rm+,Rn
	///                              $6nm6 | MOV.L @Rm+,Rn
	MovIncToReg(Size, Reg, Reg),
	///                              $2nm4 | MOV.B Rm,@-Rn
	///                              $2nm5 | MOV.W Rm,@-Rn
	///                              $2nm6 | MOV.L Rm,@-Rn
	MovRegToDec(Size, Reg, Reg),

	///                              $84md | MOV.B @(disp,Rm),R0
	MovByteDispRegToR0(I4, Reg),
	///                              $80nd | MOV.B R0,@(disp,Rn)
	MovByteR0ToDispReg(I4, Reg),
	///                              $85md | MOV.W @(disp,Rm),R0
	MovWordDispRegToR0(I4, Reg),
	///                              $81nd | MOV.W R0,@(disp,Rn)
	MovWordR0ToDispReg(I4, Reg),
	///                              $9ndd | MOV.W @(disp,PC),Rn
	MovWordDispPCToReg(i8, Reg),
	///                              $Dndd | MOV.L @(disp,PC),Rn
	MovLongDispPCToReg(i8, Reg),
	///                              $5nmd | MOV.L @(disp,Rm),Rn
	MovLongDispRegToReg(I4, Reg, Reg),
	///                              $1nmd | MOV.L Rm,@(disp,Rn)
	MovLongRegToDispReg(Reg, I4, Reg),

	/* Directives */
	Byte(u8),
	Word(u16),
	Long(u32),
}

impl fmt::Debug for Asm {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use Size as Sz;

		match self {
			Self::ClrMac => write!(f, "\tclrmac"),
			Self::ClrT   => write!(f, "\tclrt"),
			Self::Div0U  => write!(f, "\tdiv0u"),
			Self::Nop    => write!(f, "\tnop"),
			Self::Rte    => write!(f, "\trte"),
			Self::Rts    => write!(f, "\trts"),
			Self::SetT   => write!(f, "\tsett"),
			Self::Sleep  => write!(f, "\tsleep"),

			Self::Bf(label)   => write!(f, "\tbf #{label}"),
			Self::BfS(label)  => write!(f, "\tbf/s #{label}"),
			Self::Bra(label)  => write!(f, "\tbra #{label}"),
			Self::BraF(m)     => write!(f, "\tbraf r{m}"),
			Self::Bsr(label)  => write!(f, "\tbsr #{label}"),
			Self::BsrF(m)     => write!(f, "\tbsrf r{m}"),
			Self::Bt(label)   => write!(f, "\tbt #{label}"),
			Self::BtS(label)  => write!(f, "\tbt/s #{label}"),
			Self::Dt(n)       => write!(f, "\tdt r{n}"),
			Self::Jmp(m)      => write!(f, "\tjmp @r{m}"),
			Self::Jsr(m)      => write!(f, "\tjsr @r{m}"),
			Self::MovA(label) => write!(f, "\tmova @({label},pc),r0"),
			Self::MovT(n)     => write!(f, "\tmovt r{n}"),
			Self::RotCL(n)    => write!(f, "\trotcl r{n}"),
			Self::RotCR(n)    => write!(f, "\trotcr r{n}"),
			Self::RotL(n)     => write!(f, "\trotl r{n}"),
			Self::RotR(n)     => write!(f, "\trotr r{n}"),
			Self::ShAL(n)     => write!(f, "\tshal r{n}"),
			Self::ShAR(n)     => write!(f, "\tshar r{n}"),
			Self::ShLL(n)     => write!(f, "\tshll r{n}"),
			Self::ShLL2(n)    => write!(f, "\tshll2 r{n}"),
			Self::ShLL8(n)    => write!(f, "\tshll8 r{n}"),
			Self::ShLL16(n)   => write!(f, "\tshll16 r{n}"),
			Self::ShLR(n)     => write!(f, "\tshlr r{n}"),
			Self::ShLR2(n)    => write!(f, "\tshlr2 r{n}"),
			Self::ShLR8(n)    => write!(f, "\tshlr8 r{n}"),
			Self::ShLR16(n)   => write!(f, "\tshlr16 r{n}"),
			Self::TaS(n)      => write!(f, "\ttas.b @r{n}"),
			Self::TrapA(imm)  => write!(f, "\ttrapa #{imm}"),

			Self::AddC(m,n)     => write!(f, "\taddc r{m},r{n}"),
			Self::AddV(m,n)     => write!(f, "\taddv r{m},r{n}"),
			Self::Div0S(m,n)    => write!(f, "\tdiv0s r{m},r{n}"),
			Self::Div1(m,n)     => write!(f, "\tdiv1 r{m},r{n}"),
			Self::ExtSByte(m,n) => write!(f, "\texts.b r{m},r{n}"),
			Self::ExtSWord(m,n) => write!(f, "\texts.w r{m},r{n}"),
			Self::ExtUByte(m,n) => write!(f, "\textu.b r{m},r{n}"),
			Self::ExtUWord(m,n) => write!(f, "\textu.w r{m},r{n}"),
			Self::MacWord(m,n)  => write!(f, "\tmac.w r{m},r{n}"),
			Self::MacLong(m,n)  => write!(f, "\tmac.l r{m},r{n}"),
			Self::Neg(m,n)      => write!(f, "\tneg r{m},r{n}"),
			Self::NegC(m,n)     => write!(f, "\tnegc r{m},r{n}"),
			Self::Not(m,n)      => write!(f, "\tnot r{m},r{n}"),
			Self::Sub(m,n)      => write!(f, "\tsub r{m},r{n}"),
			Self::SubC(m,n)     => write!(f, "\tsubc r{m},r{n}"),
			Self::SubV(m,n)     => write!(f, "\tsubv r{m},r{n}"),
			Self::SwapByte(m,n) => write!(f, "\tswap.b r{m},r{n}"),
			Self::SwapWord(m,n) => write!(f, "\tswap.w r{m},r{n}"),
			Self::Xtrct(m,n)    => write!(f, "\txtrct r{m},r{n}"),

			Self::AndReg(m,n)  => write!(f, "\tand r{m},r{n}"),
			Self::AndImm(imm)  => write!(f, "\tand #{imm},r0"),
			Self::AndByte(imm) => write!(f, "\tand.b #{imm},@(r0,gbr)"),
			Self::OrReg(m,n)   => write!(f, "\tor r{m},r{n}"),
			Self::OrImm(imm)   => write!(f, "\tor #{imm},r0"),
			Self::OrByte(imm)  => write!(f, "\tor.b #{imm},@(r0,gbr)"),
			Self::TstReg(m,n)  => write!(f, "\ttst r{m},r{n}"),
			Self::TstImm(imm)  => write!(f, "\ttst #{imm},r0"),
			Self::TstByte(imm) => write!(f, "\ttst.b #{imm},@(r0,gbr)"),
			Self::XorReg(m,n)  => write!(f, "\txor r{m},r{n}"),
			Self::XorImm(imm)  => write!(f, "\txor #{imm},r0"),
			Self::XorByte(imm) => write!(f, "\txor.b #{imm},@(r0,gbr)"),

			Self::CmpEqImm(imm) => write!(f, "\tcmp/eq #{imm},r0"),
			Self::CmpEqReg(m,n) => write!(f, "\tcmp/eq r{m},r{n}"),
			Self::CmpGE(m,n)    => write!(f, "\tcmp/ge r{m},r{n}"),
			Self::CmpGT(m,n)    => write!(f, "\tcmp/gt r{m},r{n}"),
			Self::CmpHI(m,n)    => write!(f, "\tcmp/hi r{m},r{n}"),
			Self::CmpHS(m,n)    => write!(f, "\tcmp/hs r{m},r{n}"),
			Self::CmpSTR(m,n)   => write!(f, "\tcmp/str r{m},r{n}"),
			Self::CmpPL(n)      => write!(f, "\tcmp/pl r{n}"),
			Self::CmpPZ(n)      => write!(f, "\tcmp/pz r{n}"),

			Self::AddImm(imm,n) => write!(f, "\tadd #{imm},r{n}"),
			Self::AddReg(m,n)   => write!(f, "\tadd r{m},r{n}"),
			Self::Mul(m,n)      => write!(f, "\tmul.l r{m},r{n}"),
			Self::MulS(m,n)     => write!(f, "\tmuls.w r{m},r{n}"),
			Self::MulU(m,n)     => write!(f, "\tmulu.w r{m},r{n}"),
			Self::DMulS(m,n)    => write!(f, "\tdmuls.l r{m},r{n}"),
			Self::DMulU(m,n)    => write!(f, "\tdmulu.l r{m},r{n}"),

			Self::LdcGbr(m)     => write!(f, "\tldc r{m},gbr"),
			Self::LdcSr(m)      => write!(f, "\tldc r{m},sr"),
			Self::LdcVbr(m)     => write!(f, "\tldc r{m},vbr"),
			Self::LdcGbrInc(m)  => write!(f, "\tldc.l @r{m}+,gbr"),
			Self::LdcSrInc(m)   => write!(f, "\tldc.l @r{m}+,sr"),
			Self::LdcVbrInc(m)  => write!(f, "\tldc.l @r{m}+,vbr"),
			Self::LdsMach(m)    => write!(f, "\tlds r{m},mach"),
			Self::LdsMacl(m)    => write!(f, "\tlds r{m},macl"),
			Self::LdsPr(m)      => write!(f, "\tlds r{m},pr"),
			Self::LdsMachInc(m) => write!(f, "\tlds.l @r{m}+,mach"),
			Self::LdsMaclInc(m) => write!(f, "\tlds.l @r{m}+,macl"),
			Self::LdsPrInc(m)   => write!(f, "\tlds.l @r{m}+,pr"),

			Self::StcGbr(n)     => write!(f, "\tstc gbr,r{n}"),
			Self::StcSr(n)      => write!(f, "\tstc sr,r{n}"),
			Self::StcVbr(n)     => write!(f, "\tstc vbr,r{n}"),
			Self::StcGbrDec(n)  => write!(f, "\tstc.l gbr,@-r{n}"),
			Self::StcSrDec(n)   => write!(f, "\tstc.l sr,@-r{n}"),
			Self::StcVbrDec(n)  => write!(f, "\tstc.l vbr,@-r{n}"),
			Self::StsMach(n)    => write!(f, "\tsts mach,r{n}"),
			Self::StsMacl(n)    => write!(f, "\tsts macl,r{n}"),
			Self::StsPr(n)      => write!(f, "\tsts pr,r{n}"),
			Self::StsMachDec(n) => write!(f, "\tsts.l mach,r{n}"),
			Self::StsMaclDec(n) => write!(f, "\tsts.l macl,r{n}"),
			Self::StsPrDec(n)   => write!(f, "\tsts.l pr,r{n}"),

			Self::MovImm(imm,n)                   => write!(f, "\tmov #{imm},r{n}"),
			Self::MovReg(m,n)                     => write!(f, "\tmov r{m},r{n}"),
			Self::MovAddrToReg(Sz::Byte, m,n)   => write!(f, "\tmov.b @r{m},r{n}"),
			Self::MovAddrToReg(Sz::Word, m,n)   => write!(f, "\tmov.w @r{m},r{n}"),
			Self::MovAddrToReg(Sz::Long, m,n)   => write!(f, "\tmov.l @r{m},r{n}"),
			Self::MovRegToAddr(Sz::Byte, m,n)   => write!(f, "\tmov.b r{m},@r{n}"),
			Self::MovRegToAddr(Sz::Word, m,n)   => write!(f, "\tmov.w r{m},@r{n}"),
			Self::MovRegToAddr(Sz::Long, m,n)   => write!(f, "\tmov.l r{m},@r{n}"),
			Self::MovGbrToR0(Sz::Byte, d)       => write!(f, "\tmov.b @({d},gbr),r0"),
			Self::MovGbrToR0(Sz::Word, d)       => write!(f, "\tmov.w @({d},gbr),r0"),
			Self::MovGbrToR0(Sz::Long, d)       => write!(f, "\tmov.w @({d},gbr),r0"),
			Self::MovR0ToGbr(Sz::Byte, d)       => write!(f, "\tmov.b r0,@({d},gbr)"),
			Self::MovR0ToGbr(Sz::Word, d)       => write!(f, "\tmov.w r0,@({d},gbr)"),
			Self::MovR0ToGbr(Sz::Long, d)       => write!(f, "\tmov.l r0,@({d},gbr)"),
			Self::MovDispR0ToReg(Sz::Byte, m,n) => write!(f, "\tmov.b @(r0,r{m}),r{n}"),
			Self::MovDispR0ToReg(Sz::Word, m,n) => write!(f, "\tmov.w @(r0,r{m}),r{n}"),
			Self::MovDispR0ToReg(Sz::Long, m,n) => write!(f, "\tmov.l @(r0,r{m}),r{n}"),
			Self::MovRegToDispR0(Sz::Byte, m,n) => write!(f, "\tmov.b r{m},@(r0,r{n})"),
			Self::MovRegToDispR0(Sz::Word, m,n) => write!(f, "\tmov.w r{m},@(r0,r{n})"),
			Self::MovRegToDispR0(Sz::Long, m,n) => write!(f, "\tmov.l r{m},@(r0,r{n})"),
			Self::MovIncToReg(Sz::Byte, m,n)    => write!(f, "\tmov.b @r{m}+,r{n}"),
			Self::MovIncToReg(Sz::Word, m,n)    => write!(f, "\tmov.w @r{m}+,r{n}"),
			Self::MovIncToReg(Sz::Long, m,n)    => write!(f, "\tmov.l @r{m}+,r{n}"),
			Self::MovRegToDec(Sz::Byte, m,n)    => write!(f, "\tmov.b r{m},@-r{n}"),
			Self::MovRegToDec(Sz::Word, m,n)    => write!(f, "\tmov.w r{m},@-r{n}"),
			Self::MovRegToDec(Sz::Long, m,n)    => write!(f, "\tmov.l r{m},@-r{n}"),

			Self::MovByteDispRegToR0(d,m)    => write!(f, "\tmov.b @({d},r{m}),r0"),
			Self::MovByteR0ToDispReg(d,n)    => write!(f, "\tmov.b r0,@({d},r{n})"),
			Self::MovWordDispRegToR0(d,m)    => write!(f, "\tmov.w @({d},r{m}),r0"),
			Self::MovWordR0ToDispReg(d,n)    => write!(f, "\tmov.w r0,@({d},r{n})"),
			Self::MovWordDispPCToReg(d,n)    => write!(f, "\tmov.w @({d},pc),r{n}"),
			Self::MovLongDispPCToReg(d,n)    => write!(f, "\tmov.l @({d},pc),r{n}"),
			Self::MovLongDispRegToReg(d,m,n) => write!(f, "\tmov.l @({d},r{m}),r{n}"),
			Self::MovLongRegToDispReg(m,d,n) => write!(f, "\tmov.l r{m},@({d},r{n})"),

			Self::Byte(i) => write!(f, "${i:02X}"),
			Self::Word(i) => write!(f, "${i:04X}"),
			Self::Long(i) => write!(f, "${i:08X}"),
		}
	}
}

impl Asm {
	pub fn output(self, out: &mut Vec<u8>) {
		match self {
			Self::ClrT   => out.extend([0x00, 0x08]),
			Self::Nop    => out.extend([0x00, 0x09]),
			Self::Rts    => out.extend([0x00, 0x0B]),
			Self::SetT   => out.extend([0x00, 0x18]),
			Self::Div0U  => out.extend([0x00, 0x19]),
			Self::Sleep  => out.extend([0x00, 0x1B]),
			Self::ClrMac => out.extend([0x00, 0x28]),
			Self::Rte    => out.extend([0x00, 0x2B]),

			Self::Bf(d)     => out.extend([0x8B, d as u8]),
			Self::BfS(d)    => out.extend([0x8F, d as u8]),
			Self::Bra(d)    => out.extend([0xA0 | ((d >> 8) & 0x0F) as u8, d as u8]),
			Self::BraF(r)   => out.extend([r, 0x23]),
			Self::Bsr(d)    => out.extend([0xB0 | ((d >> 8) & 0x0F) as u8, d as u8]),
			Self::BsrF(r)   => out.extend([r, 0x03]),
			Self::Bt(d)     => out.extend([0x89, d as u8]),
			Self::BtS(d)    => out.extend([0x8D, d as u8]),
			Self::Dt(r)     => out.extend([0x40 | r, 0x10]),
			Self::Jmp(r)    => out.extend([0x40 | r, 0x2B]),
			Self::Jsr(r)    => out.extend([0x40 | r, 0x0B]),
			Self::MovA(d)   => out.extend([0xC7, d]),
			Self::MovT(r)   => out.extend([r, 0x29]),
			Self::RotCL(r)  => out.extend([0x40 | r, 0x24]),
			Self::RotCR(r)  => out.extend([0x40 | r, 0x25]),
			Self::RotL(r)   => out.extend([0x40 | r, 0x04]),
			Self::RotR(r)   => out.extend([0x40 | r, 0x05]),
			Self::ShAL(r)   => out.extend([0x40 | r, 0x20]),
			Self::ShAR(r)   => out.extend([0x40 | r, 0x21]),
			Self::ShLL(r)   => out.extend([0x40 | r, 0x00]),
			Self::ShLL2(r)  => out.extend([0x40 | r, 0x08]),
			Self::ShLL8(r)  => out.extend([0x40 | r, 0x18]),
			Self::ShLL16(r) => out.extend([0x40 | r, 0x28]),
			Self::ShLR(r)   => out.extend([0x40 | r, 0x01]),
			Self::ShLR2(r)  => out.extend([0x40 | r, 0x09]),
			Self::ShLR8(r)  => out.extend([0x40 | r, 0x19]),
			Self::ShLR16(r) => out.extend([0x40 | r, 0x29]),
			Self::TaS(r)    => out.extend([0x40 | r, 0x1B]),
			Self::TrapA(i)  => out.extend([0xC3, i]),

			Self::AddC(m,n)     => out.extend([0x30 | n, 0x0E | m << 4]),
			Self::AddV(m,n)     => out.extend([0x30 | n, 0x0F | m << 4]),
			Self::Div0S(m,n)    => out.extend([0x20 | n, 0x07 | m << 4]),
			Self::Div1(m,n)     => out.extend([0x30 | n, 0x04 | m << 4]),
			Self::ExtSByte(m,n) => out.extend([0x60 | n, 0x0E | m << 4]),
			Self::ExtSWord(m,n) => out.extend([0x60 | n, 0x0F | m << 4]),
			Self::ExtUByte(m,n) => out.extend([0x60 | n, 0x0C | m << 4]),
			Self::ExtUWord(m,n) => out.extend([0x60 | n, 0x0D | m << 4]),
			Self::MacWord(m,n)  => out.extend([0x40 | n, 0x0F | m << 4]),
			Self::MacLong(m,n)  => out.extend([n, 0x0F | m << 4]),
			Self::Neg(m,n)      => out.extend([0x60 | n, 0x0B | m << 4]),
			Self::NegC(m,n)     => out.extend([0x60 | n, 0x0A | m << 4]),
			Self::Not(m,n)      => out.extend([0x60 | n, 0x07 | m << 4]),
			Self::Sub(m,n)      => out.extend([0x30 | n, 0x08 | m << 4]),
			Self::SubC(m,n)     => out.extend([0x30 | n, 0x0A | m << 4]),
			Self::SubV(m,n)     => out.extend([0x30 | n, 0x0B | m << 4]),
			Self::SwapByte(m,n) => out.extend([0x60 | n, 0x08 | m << 4]),
			Self::SwapWord(m,n) => out.extend([0x60 | n, 0x09 | m << 4]),
			Self::Xtrct(m,n)    => out.extend([0x20 | n, 0x0D | m << 4]),

			Self::AndReg(m,n) => out.extend([0x20 | n, 0x09 | m << 4]),
			Self::AndImm(i)   => out.extend([0xC9, i]),
			Self::AndByte(i)  => out.extend([0xCD, i]),
			Self::OrReg(m,n)  => out.extend([0x20 | n, 0x0B | m << 4]),
			Self::OrImm(i)    => out.extend([0xCB, i]),
			Self::OrByte(i)   => out.extend([0xCF, i]),
			Self::TstReg(m,n) => out.extend([0x20 | n, 0x08 | m << 4]),
			Self::TstImm(i)   => out.extend([0xC8, i]),
			Self::TstByte(i)  => out.extend([0xCC, i]),
			Self::XorReg(m,n) => out.extend([0x20 | n, 0x0A | m << 4]),
			Self::XorImm(i)   => out.extend([0xCA, i]),
			Self::XorByte(i)  => out.extend([0xCE, i]),

			Self::CmpEqReg(m,n) => out.extend([0x30 | n, m << 4]),
			Self::CmpGE(m,n)    => out.extend([0x30 | n, 0x03 | m << 4]),
			Self::CmpGT(m,n)    => out.extend([0x30 | n, 0x07 | m << 4]),
			Self::CmpHI(m,n)    => out.extend([0x30 | n, 0x06 | m << 4]),
			Self::CmpHS(m,n)    => out.extend([0x30 | n, 0x02 | m << 4]),
			Self::CmpPL(r)      => out.extend([0x40 | r, 0x15]),
			Self::CmpPZ(r)      => out.extend([0x40 | r, 0x11]),
			Self::CmpSTR(m,n)   => out.extend([0x20 | n, 0x0C | m << 4]),
			Self::CmpEqImm(i)   => out.extend([0x88, i as u8]),

			Self::AddImm(i,r)   => out.extend([0x70 | r, i as u8]),
			Self::AddReg(m,n)   => out.extend([0x30 | n, 0x0C | m << 4]),
			Self::Mul(m,n)      => out.extend([n, 0x07 | m << 4]),
			Self::MulS(m,n)     => out.extend([0x20 | n, 0x0F | m << 4]),
			Self::MulU(m,n)     => out.extend([0x20 | n, 0x0E | m << 4]),
			Self::LdcGbr(r)     => out.extend([0x40 | r, 0x1E]),
			Self::LdcSr(r)      => out.extend([0x40 | r, 0x0E]),
			Self::LdcVbr(r)     => out.extend([0x40 | r, 0x2E]),
			Self::LdcGbrInc(r)  => out.extend([0x40 | r, 0x17]),
			Self::LdcSrInc(r)   => out.extend([0x40 | r, 0x07]),
			Self::LdcVbrInc(r)  => out.extend([0x40 | r, 0x27]),
			Self::LdsMach(r)    => out.extend([0x40 | r, 0x0A]),
			Self::LdsMacl(r)    => out.extend([0x40 | r, 0x1A]),
			Self::LdsPr(r)      => out.extend([0x40 | r, 0x2A]),
			Self::LdsMachInc(r) => out.extend([0x40 | r, 0x06]),
			Self::LdsMaclInc(r) => out.extend([0x40 | r, 0x16]),
			Self::LdsPrInc(r)   => out.extend([0x40 | r, 0x26]),
			Self::StcGbr(r)     => out.extend([r, 0x12]),
			Self::StcSr(r)      => out.extend([r, 0x02]),
			Self::StcVbr(r)     => out.extend([r, 0x22]),
			Self::StcGbrDec(r)  => out.extend([0x40 | r, 0x13]),
			Self::StcSrDec(r)   => out.extend([0x40 | r, 0x03]),
			Self::StcVbrDec(r)  => out.extend([0x40 | r, 0x23]),
			Self::StsMach(r)    => out.extend([r, 0x0A]),
			Self::StsMacl(r)    => out.extend([r, 0x1A]),
			Self::StsPr(r)      => out.extend([r, 0x2A]),
			Self::StsMachDec(r) => out.extend([0x40 | r, 0x02]),
			Self::StsMaclDec(r) => out.extend([0x40 | r, 0x12]),
			Self::StsPrDec(r)   => out.extend([0x40 | r, 0x22]),
			Self::DMulS(m,n)    => out.extend([0x30 | n, 0x0D | m << 4]),
			Self::DMulU(m,n)    => out.extend([0x30 | n, 0x05 | m << 4]),

			Self::MovImm(i,r)                    => out.extend([0xE0 | r, i as u8]),
			Self::MovReg(m,n)                    => out.extend([0x60 | n, 0x03 | m << 4]),
			Self::MovAddrToReg(Size::Byte,m,n)   => out.extend([0x60 | n, m << 4]),
			Self::MovAddrToReg(Size::Word,m,n)   => out.extend([0x60 | n, 0x01 | m << 4]),
			Self::MovAddrToReg(Size::Long,m,n)   => out.extend([0x60 | n, 0x02 | m << 4]),
			Self::MovRegToAddr(Size::Byte,m,n)   => out.extend([0x20 | n, m << 4]),
			Self::MovRegToAddr(Size::Word,m,n)   => out.extend([0x20 | n, 0x01 | m << 4]),
			Self::MovRegToAddr(Size::Long,m,n)   => out.extend([0x20 | n, 0x02 | m << 4]),
			Self::MovGbrToR0(Size::Byte,d)       => out.extend([0xC4, d as u8]),
			Self::MovGbrToR0(Size::Word,d)       => out.extend([0xC5, d as u8]),
			Self::MovGbrToR0(Size::Long,d)       => out.extend([0xC6, d as u8]),
			Self::MovR0ToGbr(Size::Byte,d)       => out.extend([0xC0, d as u8]),
			Self::MovR0ToGbr(Size::Word,d)       => out.extend([0xC1, d as u8]),
			Self::MovR0ToGbr(Size::Long,d)       => out.extend([0xC2, d as u8]),
			Self::MovDispR0ToReg(Size::Byte,m,n) => out.extend([n, 0x0C | m << 4]),
			Self::MovDispR0ToReg(Size::Word,m,n) => out.extend([n, 0x0D | m << 4]),
			Self::MovDispR0ToReg(Size::Long,m,n) => out.extend([n, 0x0E | m << 4]),
			Self::MovRegToDispR0(Size::Byte,m,n) => out.extend([n, 0x04 | m << 4]),
			Self::MovRegToDispR0(Size::Word,m,n) => out.extend([n, 0x05 | m << 4]),
			Self::MovRegToDispR0(Size::Long,m,n) => out.extend([n, 0x06 | m << 4]),
			Self::MovIncToReg(Size::Byte,m,n)    => out.extend([0x60 | n, 0x04 | m << 4]),
			Self::MovIncToReg(Size::Word,m,n)    => out.extend([0x60 | n, 0x05 | m << 4]),
			Self::MovIncToReg(Size::Long,m,n)    => out.extend([0x60 | n, 0x06 | m << 4]),
			Self::MovRegToDec(Size::Byte,m,n)    => out.extend([0x20 | n, 0x04 | m << 4]),
			Self::MovRegToDec(Size::Word,m,n)    => out.extend([0x20 | n, 0x05 | m << 4]),
			Self::MovRegToDec(Size::Long,m,n)    => out.extend([0x20 | n, 0x06 | m << 4]),

			Self::MovByteDispRegToR0(d,m)    => out.extend([0x84, m << 4 | d]),
			Self::MovByteR0ToDispReg(d,n)    => out.extend([0x80, n << 4 | d]),
			Self::MovWordDispRegToR0(d,m)    => out.extend([0x85, m << 4 | d]),
			Self::MovWordR0ToDispReg(d,n)    => out.extend([0x81, n << 4 | d]),
			Self::MovWordDispPCToReg(d,n)    => out.extend([0x90 | n, d as u8]),
			Self::MovLongDispPCToReg(d,n)    => out.extend([0xD0 | n, d as u8]),
			Self::MovLongDispRegToReg(d,m,n) => out.extend([0x50 | n, m << 4 | d]),
			Self::MovLongRegToDispReg(m,d,n) => out.extend([0x10 | n, m << 4 | d]),

			Self::Byte(b) => out.push(b),
			Self::Word(w) => out.extend(w.to_be_bytes()),
			Self::Long(l) => out.extend(l.to_be_bytes()),
		}
	}
}

#[instrument]
pub fn output(asm: &[Asm]) -> Vec<u8> {
	let mut out = Vec::with_capacity(asm.len());

	for idx in 0..asm.len() {
		match asm[idx] {
			a @ Asm::Byte(_) => {
				if idx % 2 == 0 && !matches!(asm.get(idx + 1), Some(Asm::Byte(_)) | None) {
					warn!("adding 1-byte padding after single byte output @ index {idx}");
					a.output(&mut out);
					out.push(0x00);
				} else {
					// Either there's another singe-byte output after this one, or this is the last byte.
					a.output(&mut out);
				}
			}
			a @ Asm::Long(_) => {
				if idx % 4 != 0 {
					warn!("4-byte output on unaligned index {idx}");
				}
				a.output(&mut out);
			}
			a => {
				if idx % 2 != 0 {
					warn!("2-byte output on unaligned index {idx}");
				}
				a.output(&mut out);
			}
		}
	}

	out
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
				let out = super::output(&asm);
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

