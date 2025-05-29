
use std::fmt;

use crate::i4::I4;

pub(crate) type Reg = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Size { Byte, Word, Long }

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Asm {
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
		match self {
			Self::ClrMac => write!(f, "\tclrmac"),
			Self::ClrT => write!(f, "\tclrt"),
			Self::Div0U => write!(f, "\tdiv0u"),
			Self::Nop => write!(f, "\tnop"),
			Self::Rte => write!(f, "\trte"),
			Self::Rts => write!(f, "\trts"),
			Self::SetT => write!(f, "\tsett"),
			Self::Sleep => write!(f, "\tsleep"),

			Self::Bf(label) => write!(f, "\tbf #{label}"),
			Self::BfS(label) => write!(f, "\tbf/s #{label}"),
			Self::Bra(label) => write!(f, "\tbra #{label}"),
			Self::BraF(m) => write!(f, "\tbraf r{m}"),
			Self::Bsr(label) => write!(f, "\tbsr #{label}"),
			Self::BsrF(m) => write!(f, "\tbsrf r{m}"),
			Self::Bt(label) => write!(f, "\tbt #{label}"),
			Self::BtS(label) => write!(f, "\tbt/s #{label}"),
			Self::Dt(n) => write!(f, "\tdt r{n}"),
			Self::Jmp(m) => write!(f, "\tjmp @r{m}"),
			Self::Jsr(m) => write!(f, "\tjsr @r{m}"),
			Self::MovA(label) => write!(f, "\tmova @({label},pc),r0"),
			Self::MovT(n) => write!(f, "\tmovt r{n}"),
			Self::RotCL(n) => write!(f, "\trotcl r{n}"),
			Self::RotCR(n) => write!(f, "\trotcr r{n}"),
			Self::RotL(n) => write!(f, "\trotl r{n}"),
			Self::RotR(n) => write!(f, "\trotr r{n}"),
			Self::ShAL(n) => write!(f, "\tshal r{n}"),
			Self::ShAR(n) => write!(f, "\tshar r{n}"),
			Self::ShLL(n) => write!(f, "\tshll r{n}"),
			Self::ShLL2(n) => write!(f, "\tshll2 r{n}"),
			Self::ShLL8(n) => write!(f, "\tshll8 r{n}"),
			Self::ShLL16(n) => write!(f, "\tshll16 r{n}"),
			Self::ShLR(n) => write!(f, "\tshlr r{n}"),
			Self::ShLR2(n) => write!(f, "\tshlr2 r{n}"),
			Self::ShLR8(n) => write!(f, "\tshlr8 r{n}"),
			Self::ShLR16(n) => write!(f, "\tshlr16 r{n}"),
			Self::TaS(n) => write!(f, "\ttas.b @r{n}"),
			Self::TrapA(imm) => write!(f, "\ttrapa #{imm}"),

			Self::AddC(m,n) => write!(f, "\taddc r{m},r{n}"),
			Self::AddV(m,n) => write!(f, "\taddv r{m},r{n}"),
			Self::Div0S(m,n) => write!(f, "\tdiv0s r{m},r{n}"),
			Self::Div1(m,n) => write!(f, "\tdiv1 r{m},r{n}"),
			Self::ExtSByte(m,n) => write!(f, "\texts.b r{m},r{n}"),
			Self::ExtSWord(m,n) => write!(f, "\texts.w r{m},r{n}"),
			Self::ExtUByte(m,n) => write!(f, "\textu.b r{m},r{n}"),
			Self::ExtUWord(m,n) => write!(f, "\textu.w r{m},r{n}"),
			Self::MacWord(m,n) => write!(f, "\tmac.w r{m},r{n}"),
			Self::MacLong(m,n) => write!(f, "\tmac.l r{m},r{n}"),
			Self::Neg(m,n) => write!(f, "\tneg r{m},r{n}"),
			Self::NegC(m,n) => write!(f, "\tnegc r{m},r{n}"),
			Self::Not(m,n) => write!(f, "\tnot r{m},r{n}"),
			Self::Sub(m,n) => write!(f, "\tsub r{m},r{n}"),
			Self::SubC(m,n) => write!(f, "\tsubc r{m},r{n}"),
			Self::SubV(m,n) => write!(f, "\tsubv r{m},r{n}"),
			Self::SwapByte(m,n) => write!(f, "\tswap.b r{m},r{n}"),
			Self::SwapWord(m,n) => write!(f, "\tswap.w r{m},r{n}"),
			Self::Xtrct(m,n) => write!(f, "\txtrct r{m},r{n}"),

			Self::AndReg(m,n) => write!(f, "\tand r{m},r{n}"),
			Self::AndImm(imm) => write!(f, "\tand #{imm},r0"),
			Self::AndByte(imm) => write!(f, "\tand.b #{imm},@(r0,gbr)"),
			Self::OrReg(m,n) => write!(f, "\tor r{m},r{n}"),
			Self::OrImm(imm) => write!(f, "\tor #{imm},r0"),
			Self::OrByte(imm) => write!(f, "\tor.b #{imm},@(r0,gbr)"),
			Self::TstReg(m,n) => write!(f, "\ttst r{m},r{n}"),
			Self::TstImm(imm) => write!(f, "\ttst #{imm},r0"),
			Self::TstByte(imm) => write!(f, "\ttst.b #{imm},@(r0,gbr)"),
			Self::XorReg(m,n) => write!(f, "\txor r{m},r{n}"),
			Self::XorImm(imm) => write!(f, "\txor #{imm},r0"),
			Self::XorByte(imm) => write!(f, "\txor.b #{imm},@(r0,gbr)"),

			Self::CmpEqImm(imm) => write!(f, "\tcmp/eq #{imm},r0"),
			Self::CmpEqReg(m,n) => write!(f, "\tcmp/eq r{m},r{n}"),
			Self::CmpGE(m,n) => write!(f, "\tcmp/ge r{m},r{n}"),
			Self::CmpGT(m,n) => write!(f, "\tcmp/gt r{m},r{n}"),
			Self::CmpHI(m,n) => write!(f, "\tcmp/hi r{m},r{n}"),
			Self::CmpHS(m,n) => write!(f, "\tcmp/hs r{m},r{n}"),
			Self::CmpSTR(m,n) => write!(f, "\tcmp/str r{m},r{n}"),
			Self::CmpPL(n) => write!(f, "\tcmp/pl r{n}"),
			Self::CmpPZ(n) => write!(f, "\tcmp/pz r{n}"),

			Self::AddImm(imm,n) => write!(f, "\tadd #{imm},r{n}"),
			Self::AddReg(m,n) => write!(f, "\tadd r{m},r{n}"),
			Self::Mul(m,n) => write!(f, "\tmul.l r{m},r{n}"),
			Self::MulS(m,n) => write!(f, "\tmuls.w r{m},r{n}"),
			Self::MulU(m,n) => write!(f, "\tmulu.w r{m},r{n}"),
			Self::DMulS(m,n) => write!(f, "\tdmuls.l r{m},r{n}"),
			Self::DMulU(m,n) => write!(f, "\tdmulu.l r{m},r{n}"),

			Self::LdcGbr(m) => write!(f, "\tldc r{m},gbr"),
			Self::LdcSr(m) => write!(f, "\tldc r{m},sr"),
			Self::LdcVbr(m) => write!(f, "\tldc r{m},vbr"),
			Self::LdcGbrInc(m) => write!(f, "\tldc.l @r{m}+,gbr"),
			Self::LdcSrInc(m) => write!(f, "\tldc.l @r{m}+,sr"),
			Self::LdcVbrInc(m) => write!(f, "\tldc.l @r{m}+,vbr"),
			Self::LdsMach(m) => write!(f, "\tlds r{m},mach"),
			Self::LdsMacl(m) => write!(f, "\tlds r{m},macl"),
			Self::LdsPr(m) => write!(f, "\tlds r{m},pr"),
			Self::LdsMachInc(m) => write!(f, "\tlds.l @r{m}+,mach"),
			Self::LdsMaclInc(m) => write!(f, "\tlds.l @r{m}+,macl"),
			Self::LdsPrInc(m) => write!(f, "\tlds.l @r{m}+,pr"),

			Self::StcGbr(n) => write!(f, "\tstc gbr,r{n}"),
			Self::StcSr(n) => write!(f, "\tstc sr,r{n}"),
			Self::StcVbr(n) => write!(f, "\tstc vbr,r{n}"),
			Self::StcGbrDec(n) => write!(f, "\tstc.l gbr,@-r{n}"),
			Self::StcSrDec(n) => write!(f, "\tstc.l sr,@-r{n}"),
			Self::StcVbrDec(n) => write!(f, "\tstc.l vbr,@-r{n}"),
			Self::StsMach(n) => write!(f, "\tsts mach,r{n}"),
			Self::StsMacl(n) => write!(f, "\tsts macl,r{n}"),
			Self::StsPr(n) => write!(f, "\tsts pr,r{n}"),
			Self::StsMachDec(n) => write!(f, "\tsts.l mach,r{n}"),
			Self::StsMaclDec(n) => write!(f, "\tsts.l macl,r{n}"),
			Self::StsPrDec(n) => write!(f, "\tsts.l pr,r{n}"),

			Self::MovImm(imm,n) => write!(f, "\tmov #{imm},r{n}"),
			Self::MovReg(m,n) => write!(f, "\tmov r{m},r{n}"),
			Self::MovAddrToReg(Size::Byte, m,n) => write!(f, "\tmov.b @r{m},r{n}"),
			Self::MovAddrToReg(Size::Word, m,n) => write!(f, "\tmov.w @r{m},r{n}"),
			Self::MovAddrToReg(Size::Long, m,n) => write!(f, "\tmov.l @r{m},r{n}"),
			Self::MovRegToAddr(Size::Byte, m,n) => write!(f, "\tmov.b r{m},@r{n}"),
			Self::MovRegToAddr(Size::Word, m,n) => write!(f, "\tmov.w r{m},@r{n}"),
			Self::MovRegToAddr(Size::Long, m,n) => write!(f, "\tmov.l r{m},@r{n}"),
			Self::MovGbrToR0(Size::Byte, d) => write!(f, "\tmov.b @({d},gbr),r0"),
			Self::MovGbrToR0(Size::Word, d) => write!(f, "\tmov.w @({d},gbr),r0"),
			Self::MovGbrToR0(Size::Long, d) => write!(f, "\tmov.w @({d},gbr),r0"),
			Self::MovR0ToGbr(Size::Byte, d) => write!(f, "\tmov.b r0,@({d},gbr)"),
			Self::MovR0ToGbr(Size::Word, d) => write!(f, "\tmov.w r0,@({d},gbr)"),
			Self::MovR0ToGbr(Size::Long, d) => write!(f, "\tmov.l r0,@({d},gbr)"),
			Self::MovDispR0ToReg(Size::Byte, m,n) => write!(f, "\tmov.b @(r0,r{m}),r{n}"),
			Self::MovDispR0ToReg(Size::Word, m,n) => write!(f, "\tmov.w @(r0,r{m}),r{n}"),
			Self::MovDispR0ToReg(Size::Long, m,n) => write!(f, "\tmov.l @(r0,r{m}),r{n}"),
			Self::MovRegToDispR0(Size::Byte, m,n) => write!(f, "\tmov.b r{m},@(r0,r{n})"),
			Self::MovRegToDispR0(Size::Word, m,n) => write!(f, "\tmov.w r{m},@(r0,r{n})"),
			Self::MovRegToDispR0(Size::Long, m,n) => write!(f, "\tmov.l r{m},@(r0,r{n})"),
			Self::MovIncToReg(Size::Byte, m,n) => write!(f, "\tmov.b @r{m}+,r{n}"),
			Self::MovIncToReg(Size::Word, m,n) => write!(f, "\tmov.w @r{m}+,r{n}"),
			Self::MovIncToReg(Size::Long, m,n) => write!(f, "\tmov.l @r{m}+,r{n}"),
			Self::MovRegToDec(Size::Byte, m,n) => write!(f, "\tmov.b r{m},@-r{n}"),
			Self::MovRegToDec(Size::Word, m,n) => write!(f, "\tmov.w r{m},@-r{n}"),
			Self::MovRegToDec(Size::Long, m,n) => write!(f, "\tmov.l r{m},@-r{n}"),

			Self::MovByteDispRegToR0(d,m) => write!(f, "\tmov.b @({d},r{m}),r0"),
			Self::MovByteR0ToDispReg(d,n) => write!(f, "\tmov.b r0,@({d},r{n})"),
			Self::MovWordDispRegToR0(d,m) => write!(f, "\tmov.w @({d},r{m}),r0"),
			Self::MovWordR0ToDispReg(d,n) => write!(f, "\tmov.w r0,@({d},r{n})"),
			Self::MovWordDispPCToReg(d,n) => write!(f, "\tmov.w @({d},pc),r{n}"),
			Self::MovLongDispPCToReg(d,n) => write!(f, "\tmov.l @({d},pc),r{n}"),
			Self::MovLongDispRegToReg(d,m,n) => write!(f, "\tmov.l @({d},r{m}),r{n}"),
			Self::MovLongRegToDispReg(m,d,n) => write!(f, "\tmov.l r{m},@({d},r{n})"),

			Self::Byte(i) => write!(f, "${i:02X}"),
			Self::Word(i) => write!(f, "${i:04X}"),
			Self::Long(i) => write!(f, "${i:08X}"),
		}
	}
}

