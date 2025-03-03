
use pest::Parser;
use pest::iterators::Pair;
use pest::error::{Error, ErrorVariant};
use pest_derive::Parser;

use tracing::{
	instrument,
	//debug, info,
	trace, warn,
};

type Reg = u8;

type ParseResult<T> = Result<T, Error<Rule>>;

mod i4;
use i4::I4;

#[derive(Parser)]
#[grammar = "../sh2.pest"]
struct Sh2Parser;

#[derive(Debug, Default)]
struct Output(Vec<Asm>);

impl Output {
	fn push(&mut self, asm: Asm) {
		self.0.push(asm);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Size { Byte, Word, Long }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Asm {
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
	///               $C9ii | AND #imm,Rn
	AndImm(u8),
	///               $CDii | AND.B #imm,@(R0,GBR)
	AndByte(u8),
	///               $2nmB | OR Rm,Rn
	OrReg(Reg, Reg),
	///               $CBii | OR #imm,Rn
	OrImm(u8),
	///               $CFii | OR.B #imm,@(R0,GBR)
	OrByte(u8),
	///               $2nm8 | TST Rm,Rn
	TstReg(Reg, Reg),
	///               $C8ii | TST #imm,Rn
	TstImm(u8),
	///               $CCii | TST.B #imm,@(R0,GBR)
	TstByte(u8),
	///               $2nmA | XOR Rm,Rn
	XorReg(Reg, Reg),
	///               $CAii | XOR #imm,Rn
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
}

fn extra_rules(src: Pair<Rule>) {
	#[cfg(not(test))]
	tracing::error!("unexpected {src} - '{}'", src.as_str());

	#[cfg(test)]
	panic!("unexpected {src} - '{}'", src.as_str());
}

fn rename_rules(rule: &Rule) -> String {
	match rule {
		Rule::EOI => "EOF",

		Rule::bin => "a binary value",
		Rule::sp => "SP register",
		Rule::reg => "general register",
		Rule::r0 => "R0",
		Rule::gbr => "GBR",

		Rule::ins_line | Rule::ins => "instruction",
		Rule::dir_line | Rule::dir => "assembler directive",
		Rule::lbl_line => "Label",
		Rule::val_line => "Value",

		Rule::addr_reg_or_sp => "indirect register or SP (ex: '@r1')",
		Rule::reg_post_inc => "indirect register or SP w/ post-increment (ex: '@r2+')",
		Rule::reg_pre_dec => "indirect register or SP w/ pre-decrement (ex: '@-r3')",

		_ => unreachable!("{rule:?}"),
	}.to_owned()
}

#[instrument]
pub fn parser(input: &str) -> ParseResult<Output> {
	let mut output = Output::default();
	let source = Sh2Parser::parse(Rule::program, input)
		.map_err(|e| e.renamed_rules(rename_rules))?;
	for src in source {
		trace!("Parsing: {src} - '{}'", src.as_str());
		output = match src.as_rule() {
			Rule::ins_line => parse_ins_line(src, output)?,
			Rule::dir_line => todo!("{src} - '{}'", src.as_str()),
			Rule::lbl_line => todo!("{src} - '{}'", src.as_str()),
			Rule::val_line => todo!("{src} - '{}'", src.as_str()),
			Rule::EOI => continue,
			_ => {
				extra_rules(src);
				continue;
			}
		};
	}
	Ok(output)
}

#[instrument]
fn parse_ins_line(source: Pair<Rule>, mut output: Output) -> ParseResult<Output> {
	let source = source.into_inner().next().unwrap();
	for src in source.into_inner() {
		trace!("{src} - '{}'", src.as_str());

		match src.as_rule() {
			Rule::ins_clrmac => output.push(Asm::ClrMac),
			Rule::ins_clrt   => output.push(Asm::ClrT),
			Rule::ins_div0u  => output.push(Asm::Div0U),
			Rule::ins_nop    => output.push(Asm::Nop),
			Rule::ins_rte    => output.push(Asm::Rte),
			Rule::ins_rts    => output.push(Asm::Rts),
			Rule::ins_sett   => output.push(Asm::SetT),
			Rule::ins_sleep  => output.push(Asm::Sleep),
			Rule::ins_bf     => output.push(parse_ins_disp_pc(Asm::Bf, src)?),
			Rule::ins_bfs    => output.push(parse_ins_disp_pc(Asm::BfS, src)?),
			Rule::ins_bra    => output.push(parse_ins_disp_i12(Asm::Bra, src)?),
			Rule::ins_braf   => output.push(parse_ins_addr_reg_or_sp(Asm::BraF, src)?),
			Rule::ins_bsr    => output.push(parse_ins_disp_i12(Asm::Bsr, src)?),
			Rule::ins_bsrf   => output.push(parse_ins_addr_reg_or_sp(Asm::BsrF, src)?),
			Rule::ins_bt     => output.push(parse_ins_disp_pc(Asm::Bt, src)?),
			Rule::ins_bts    => output.push(parse_ins_disp_pc(Asm::BtS, src)?),
			Rule::ins_dt     => output.push(parse_ins_reg_or_sp(Asm::Dt, src)?),
			Rule::ins_jmp    => output.push(parse_ins_addr_reg_or_sp(Asm::Jmp, src)?),
			Rule::ins_jsr    => output.push(parse_ins_addr_reg_or_sp(Asm::Jsr, src)?),
			Rule::ins_movt   => output.push(parse_ins_reg_or_sp(Asm::MovT, src)?),
			Rule::ins_rotcl  => output.push(parse_ins_reg_or_sp(Asm::RotCL, src)?),
			Rule::ins_rotcr  => output.push(parse_ins_reg_or_sp(Asm::RotCR, src)?),
			Rule::ins_rotl   => output.push(parse_ins_reg_or_sp(Asm::RotL, src)?),
			Rule::ins_rotr   => output.push(parse_ins_reg_or_sp(Asm::RotR, src)?),
			Rule::ins_shal   => output.push(parse_ins_reg_or_sp(Asm::ShAL, src)?),
			Rule::ins_shar   => output.push(parse_ins_reg_or_sp(Asm::ShAR, src)?),
			Rule::ins_shll   => output.push(parse_ins_reg_or_sp(Asm::ShLL, src)?),
			Rule::ins_shll2  => output.push(parse_ins_reg_or_sp(Asm::ShLL2, src)?),
			Rule::ins_shll8  => output.push(parse_ins_reg_or_sp(Asm::ShLL8, src)?),
			Rule::ins_shll16 => output.push(parse_ins_reg_or_sp(Asm::ShLL16, src)?),
			Rule::ins_shlr   => output.push(parse_ins_reg_or_sp(Asm::ShLR, src)?),
			Rule::ins_shlr2  => output.push(parse_ins_reg_or_sp(Asm::ShLR2, src)?),
			Rule::ins_shlr8  => output.push(parse_ins_reg_or_sp(Asm::ShLR8, src)?),
			Rule::ins_shlr16 => output.push(parse_ins_reg_or_sp(Asm::ShLR16, src)?),
			Rule::ins_tas    => output.push(parse_ins_addr_reg_or_sp(Asm::TaS, src)?),
			Rule::ins_trapa  => {
				let mut args = src.into_inner();
				let imm = parse_u8(args.next().unwrap())?;
				output.push(Asm::TrapA(imm));
			}
			Rule::ins_addc  => output.push(parse_ins_rs_rs(Asm::AddC, src)?),
			Rule::ins_addv  => output.push(parse_ins_rs_rs(Asm::AddV, src)?),
			Rule::ins_div0s => output.push(parse_ins_rs_rs(Asm::Div0S, src)?),
			Rule::ins_div1  => output.push(parse_ins_rs_rs(Asm::Div1, src)?),
			Rule::ins_extsb => output.push(parse_ins_rs_rs(Asm::ExtSByte, src)?),
			Rule::ins_extsw => output.push(parse_ins_rs_rs(Asm::ExtSWord, src)?),
			Rule::ins_extub => output.push(parse_ins_rs_rs(Asm::ExtUByte, src)?),
			Rule::ins_extuw => output.push(parse_ins_rs_rs(Asm::ExtUWord, src)?),
			Rule::ins_macw  => output.push(parse_ins_pi_pi(Asm::MacWord, src)?),
			Rule::ins_macl  => output.push(parse_ins_pi_pi(Asm::MacLong, src)?),
			Rule::ins_mova  => {
				let mut args = src.into_inner();
				let disp = parse_disp_pc(args.next().unwrap())?;
				output.push(Asm::MovA(disp as u8));
			}
			Rule::ins_neg   => output.push(parse_ins_rs_rs(Asm::Neg, src)?),
			Rule::ins_negc  => output.push(parse_ins_rs_rs(Asm::NegC, src)?),
			Rule::ins_not   => output.push(parse_ins_rs_rs(Asm::Not, src)?),
			Rule::ins_sub   => output.push(parse_ins_rs_rs(Asm::Sub, src)?),
			Rule::ins_subc  => output.push(parse_ins_rs_rs(Asm::SubC, src)?),
			Rule::ins_subv  => output.push(parse_ins_rs_rs(Asm::SubV, src)?),
			Rule::ins_swapb => output.push(parse_ins_rs_rs(Asm::SwapByte, src)?),
			Rule::ins_swapw => output.push(parse_ins_rs_rs(Asm::SwapWord, src)?),
			Rule::ins_xtrct => output.push(parse_ins_rs_rs(Asm::Xtrct, src)?),

			Rule::ins_and_reg => output.push(parse_ins_rs_rs(Asm::AndReg, src)?),
			Rule::ins_and_imm => output.push(parse_ins_imm_r0(Asm::AndImm, src)?),
			Rule::ins_and_byt => output.push(parse_ins_imm_disp_r0_gbr(Asm::AndByte, src)?),
			Rule::ins_or_reg  => output.push(parse_ins_rs_rs(Asm::OrReg, src)?),
			Rule::ins_or_imm  => output.push(parse_ins_imm_r0(Asm::OrImm, src)?),
			Rule::ins_or_byt  => output.push(parse_ins_imm_disp_r0_gbr(Asm::OrByte, src)?),
			Rule::ins_tst_reg => output.push(parse_ins_rs_rs(Asm::TstReg, src)?),
			Rule::ins_tst_imm => output.push(parse_ins_imm_r0(Asm::TstImm, src)?),
			Rule::ins_tst_byt => output.push(parse_ins_imm_disp_r0_gbr(Asm::TstByte, src)?),
			Rule::ins_xor_reg => output.push(parse_ins_rs_rs(Asm::XorReg, src)?),
			Rule::ins_xor_imm => output.push(parse_ins_imm_r0(Asm::XorImm, src)?),
			Rule::ins_xor_byt => output.push(parse_ins_imm_disp_r0_gbr(Asm::XorByte, src)?),

			Rule::ins_cmp_eq_imm => output.push(parse_ins_simm_r0(Asm::CmpEqImm, src)?),
			Rule::ins_cmp_eq_reg => output.push(parse_ins_rs_rs(Asm::CmpEqReg, src)?),
			Rule::ins_cmp_ge     => output.push(parse_ins_rs_rs(Asm::CmpGE, src)?),
			Rule::ins_cmp_gt     => output.push(parse_ins_rs_rs(Asm::CmpGT, src)?),
			Rule::ins_cmp_hi     => output.push(parse_ins_rs_rs(Asm::CmpHI, src)?),
			Rule::ins_cmp_hs     => output.push(parse_ins_rs_rs(Asm::CmpHS, src)?),
			Rule::ins_cmp_str    => output.push(parse_ins_rs_rs(Asm::CmpSTR, src)?),
			Rule::ins_cmp_pl     => output.push(parse_ins_reg_or_sp(Asm::CmpPL, src)?),
			Rule::ins_cmp_pz     => output.push(parse_ins_reg_or_sp(Asm::CmpPZ, src)?),

			Rule::ins_add_reg => output.push(parse_ins_rs_rs(Asm::AddReg, src)?),
			Rule::ins_add_imm => {
				let mut args = src.into_inner();
				let src = parse_i8(args.next().unwrap())?;
				let dst = parse_reg_or_sp(args.next().unwrap())?;
				output.push(Asm::AddImm(src, dst));
			}
			Rule::ins_mul  => output.push(parse_ins_rs_rs(Asm::Mul, src)?),
			Rule::ins_muls => output.push(parse_ins_rs_rs(Asm::MulS, src)?),
			Rule::ins_mulu => output.push(parse_ins_rs_rs(Asm::MulU, src)?),
			Rule::ins_ldc  => {
				let mut args = src.into_inner();
				let src = parse_reg_or_sp(args.next().unwrap())?;
				let ins = match parse_ctrl(args.next().unwrap()) {
					Ctrl::Gbr => Asm::LdcGbr(src),
					Ctrl::Sr  => Asm::LdcSr(src),
					Ctrl::Vbr => Asm::LdcVbr(src),
				};
				output.push(ins);
			}
			Rule::ins_ldc_inc => {
				let mut args = src.into_inner();
				let src = parse_reg_post_inc(args.next().unwrap())?;
				let ins = match parse_ctrl(args.next().unwrap()) {
					Ctrl::Gbr => Asm::LdcGbrInc(src),
					Ctrl::Sr  => Asm::LdcSrInc(src),
					Ctrl::Vbr => Asm::LdcVbrInc(src),
				};
				output.push(ins);
			}
			Rule::ins_lds  => {
				let mut args = src.into_inner();
				let src = parse_reg_or_sp(args.next().unwrap())?;
				let ins = match parse_sys(args.next().unwrap()) {
					Sys::Mach => Asm::LdsMach(src),
					Sys::Macl => Asm::LdsMacl(src),
					Sys::Pr   => Asm::LdsPr(src),
				};
				output.push(ins);
			}
			Rule::ins_lds_inc => {
				let mut args = src.into_inner();
				let src = parse_reg_post_inc(args.next().unwrap())?;
				let ins = match parse_sys(args.next().unwrap()) {
					Sys::Mach => Asm::LdsMachInc(src),
					Sys::Macl => Asm::LdsMaclInc(src),
					Sys::Pr   => Asm::LdsPrInc(src),
				};
				output.push(ins);
			}
			Rule::ins_dmuls => output.push(parse_ins_rs_rs(Asm::DMulS, src)?),
			Rule::ins_dmulu => output.push(parse_ins_rs_rs(Asm::DMulU, src)?),

			Rule::ins_mov => output.push(parse_ins_mov(src)?),

			_ => {
				extra_rules(src);
				continue;
			}
		}
	}
	Ok(output)
}

#[instrument]
fn parse_ins_mov(source: Pair<Rule>) -> ParseResult<Asm> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	match src.as_rule() {
		Rule::ins_movb => parse_ins_movb(src),
		Rule::ins_movw => parse_ins_movw(src),
		Rule::ins_movl => parse_ins_movl(src),
		Rule::dec | Rule::hex | Rule::bin => {
			let src = parse_i8(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			let ins = Asm::MovImm(src, dst);
			Ok(ins)
		}
		Rule::reg | Rule::sp => {
			let src = parse_reg_or_sp(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovReg(src, dst))
		}
		_ => unreachable!("{src} - '{}'", src.as_str()),
	}
}

#[instrument]
fn parse_ins_mov_common(source: Pair<Rule>, size: Size) -> ParseResult<Asm> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	match src.as_rule() {
		Rule::disp_gbr => {
			let disp = parse_disp_gbr(src)?;
			parse_r0(args.next().unwrap())?;
			Ok(Asm::MovGbrToR0(size, disp))
		}
		Rule::disp_r0 => {
			let src = parse_disp_r0(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovDispR0ToReg(size, src, dst))
		}
		Rule::reg_post_inc => {
			let src = parse_reg_post_inc(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovIncToReg(size, src, dst))
		}
		Rule::addr_reg_or_sp => {
			let src = parse_addr_reg_or_sp(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovAddrToReg(size, src, dst))
		}
		Rule::r0 => {
			parse_r0(src)?;
			let disp = parse_disp_gbr(args.next().unwrap())?;
			Ok(Asm::MovR0ToGbr(size, disp))
		}
		Rule::reg | Rule::sp => {
			let src = parse_reg_or_sp(src)?;
			let dst = args.next().unwrap();
			match dst.as_rule() {
				Rule::disp_r0 => {
					let disp = parse_disp_r0(dst)?;
					Ok(Asm::MovRegToDispR0(size, src, disp))
				}
				Rule::reg_pre_dec => {
					let dst = parse_reg_pre_dec(dst)?;
					Ok(Asm::MovRegToDec(size, src, dst))
				}
				Rule::addr_reg_or_sp => {
					let dst = parse_addr_reg_or_sp(dst)?;
					Ok(Asm::MovRegToAddr(size, src, dst))
				}
				_ => unreachable!("{dst} - '{}'", dst.as_str()),
			}
		}
		_ => unreachable!("{src} - '{}'", src.as_str()),
	}
}

#[instrument]
fn parse_ins_movb(source: Pair<Rule>) -> ParseResult<Asm> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	match src.as_rule() {
		Rule::mov_common => parse_ins_mov_common(src, Size::Byte),
		Rule::disp_reg => {
			let (disp, src) = parse_disp_reg(src)?;
			let dst = args.next().unwrap();
			parse_r0(dst)?;
			Ok(Asm::MovByteDispRegToR0(disp, src))
		}
		Rule::r0 => {
			parse_r0(src)?;
			let (disp, dst) = parse_disp_reg(args.next().unwrap())?;
			Ok(Asm::MovByteR0ToDispReg(disp, dst))
		}
		_ => unreachable!("{src} - '{}'", src.as_str()),
	}
}

#[instrument]
fn parse_ins_movw(source: Pair<Rule>) -> ParseResult<Asm> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	match src.as_rule() {
		Rule::mov_common => parse_ins_mov_common(src, Size::Word),
		Rule::disp_reg => {
			let (disp, src) = parse_disp_reg(src)?;
			parse_r0(args.next().unwrap())?;
			Ok(Asm::MovWordDispRegToR0(disp, src))
		}
		Rule::r0 => {
			parse_r0(src)?;
			let (disp, dst) = parse_disp_reg(args.next().unwrap())?;
			Ok(Asm::MovWordR0ToDispReg(disp, dst))
		}
		Rule::disp_pc => {
			let disp = parse_disp_pc(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovWordDispPCToReg(disp, dst))
		}
		_ => unreachable!("{src} - '{}'", src.as_str()),
	}
}

#[instrument]
fn parse_ins_movl(source: Pair<Rule>) -> ParseResult<Asm> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	match src.as_rule() {
		Rule::mov_common => parse_ins_mov_common(src, Size::Long),
		Rule::disp_pc => {
			let disp = parse_disp_pc(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovLongDispPCToReg(disp, dst))
		}
		Rule::disp_reg => {
			let (disp, src) = parse_disp_reg(src)?;
			let dst = parse_reg_or_sp(args.next().unwrap())?;
			Ok(Asm::MovLongDispRegToReg(disp, src, dst))
		}
		Rule::reg | Rule::sp => {
			let src = parse_reg_or_sp(src)?;
			let (disp,dst) = parse_disp_reg(args.next().unwrap())?;
			Ok(Asm::MovLongRegToDispReg(src, disp, dst))
		}
		_ => unreachable!("{src} - '{}'", src.as_str()),
	}
}

#[instrument]
fn parse_ins_rs_rs(f: fn(Reg,Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let src = parse_reg_or_sp(args.next().unwrap())?;
	let dst = parse_reg_or_sp(args.next().unwrap())?;
	Ok(f(src, dst))
}

#[instrument]
fn parse_ins_pi_pi(f: fn(Reg,Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let src = parse_reg_post_inc(args.next().unwrap())?;
	let dst = parse_reg_post_inc(args.next().unwrap())?;
	Ok(f(src, dst))
}

#[instrument]
fn parse_ins_disp_i12(f: fn(i16) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let inner_arg = arg.into_inner().next().unwrap();
	let disp = parse_i12(inner_arg)?;
	Ok(f(disp))
}

#[instrument]
fn parse_ins_disp_pc(f: fn(i8) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let disp = parse_disp_pc(arg)?;
	Ok(f(disp))
}

#[instrument]
fn parse_ins_reg_or_sp(f: fn(Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let reg = parse_reg_or_sp(arg)?;
	Ok(f(reg))
}

#[instrument]
fn parse_ins_addr_reg_or_sp(f: fn(Reg) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let arg = source.into_inner().next().unwrap();
	let reg = parse_addr_reg_or_sp(arg)?;
	Ok(f(reg))
}

fn parse_ins_imm_r0(f: fn(u8) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let imm = parse_u8(args.next().unwrap())?;
	parse_r0(args.next().unwrap())?;
	Ok(f(imm))
}

fn parse_ins_simm_r0(f: fn(i8) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let imm = parse_i8(args.next().unwrap())?;
	parse_r0(args.next().unwrap())?;
	Ok(f(imm))
}

fn parse_ins_imm_disp_r0_gbr(f: fn(u8) -> Asm, source: Pair<Rule>) -> ParseResult<Asm> {
	let mut args = source.into_inner();
	let imm = parse_u8(args.next().unwrap())?;
	parse_disp_r0_gbr(args.next().unwrap())?;
	Ok(f(imm))
}

fn error_message(span: pest::Span, message: &str) -> Error<Rule> {
	Error::new_from_span(
		ErrorVariant::CustomError { message: message.to_string() },
		span,
	)
}

fn parse_r0(source: Pair<Rule>) -> ParseResult<()> {
	if let Rule::r0 = source.as_rule() {
		Ok(())
	} else {
		Err(error_message(source.as_span(), "expected R0"))
	}
}

fn parse_disp_r0_gbr(source: Pair<Rule>) -> ParseResult<()> {
	if let Rule::disp_r0_gbr = source.as_rule() {
		Ok(())
	} else {
		Err(error_message(source.as_span(), "expected '@(R0,GBR)'"))
	}
}

fn reg_or_sp(s: &str, err_msg: Error<Rule>) -> ParseResult<Reg> {
	if s == "sp" {
		Ok(15)
	} else if s.starts_with(['r','R']) {
		s[1..].parse::<Reg>().map_err(|_| err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(), "expected register or SP");
	reg_or_sp(source.as_str(), err_msg)
}

#[instrument]
fn parse_addr_reg_or_sp(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(), "expected indirect register or SP");
	if source.as_str().starts_with('@') {
		reg_or_sp(&source.as_str()[1..], err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_post_inc(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(),
		"expected indirect register or SP w/ post-increment");
	if source.as_str().starts_with('@')
	&& source.as_str().ends_with('+') {
		let len = source.as_str().len() - 1;
		reg_or_sp(&source.as_str()[1..len], err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_reg_pre_dec(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let err_msg = error_message(source.as_span(),
		"expected indirect register or SP w/ pre-decrement");
	if source.as_str().starts_with("@-") {
		reg_or_sp(&source.as_str()[2..], err_msg)
	} else {
		Err(err_msg)
	}
}

#[instrument]
fn parse_disp_reg(source: Pair<Rule>) -> ParseResult<(I4,Reg)> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	let src = args.next().unwrap();
	let src_span = src.as_span();
	let disp = parse_i8(src)?;
	let disp: I4 = disp.try_into()
		.map_err(|e| error_message(src_span, e))?;
	let reg = parse_reg_or_sp(args.next().unwrap())?;
	Ok((disp, reg))
}

#[instrument]
fn parse_disp_gbr(source: Pair<Rule>) -> ParseResult<i8> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	parse_i8(args.next().unwrap())
}

#[instrument]
fn parse_disp_pc(source: Pair<Rule>) -> ParseResult<i8> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	parse_i8(args.next().unwrap())
}

#[instrument]
fn parse_disp_r0(source: Pair<Rule>) -> ParseResult<Reg> {
	trace!("{source} - '{}'", source.as_str());

	let mut args = source.into_inner();
	parse_r0(args.next().unwrap())?;
	parse_reg_or_sp(args.next().unwrap())
}

#[instrument]
fn parse_u8(source: Pair<Rule>) -> ParseResult<u8> {
	trace!("{source} - '{}'", source.as_str());

	fn err_msg(base: &str) -> String {
		format!("expected a {base} value between {} and {}", u8::MIN, u8::MAX)
	}

	let s = source.as_str().replace('_', "");
	let num = match source.as_rule() {
		Rule::hex => u8::from_str_radix(&s, 16).map_err(|_| err_msg("hexadecimal")),
		Rule::bin => u8::from_str_radix(&s, 2).map_err(|_| err_msg("binary")),
		Rule::dec => s.parse::<u8>().map_err(|_| err_msg("decimal")),
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	num.map_err(|message| Error::new_from_span(
		ErrorVariant::CustomError { message },
		source.as_span(),
	))
}

#[instrument]
fn parse_i8(source: Pair<Rule>) -> ParseResult<i8> {
	trace!("{source} - '{}'", source.as_str());

	fn err_msg(base: &str) -> String {
		format!("expected a {base} value between {} and {}", i8::MIN, i8::MAX)
	}

	let s = source.as_str().replace('_', "");
	let num = match source.as_rule() {
		Rule::hex => {
			u8::from_str_radix(&s, 16)
				.map(|n| n as i8)
				.map_err(|_| err_msg("hexadecimal"))
		}
		Rule::bin => {
			u8::from_str_radix(&s, 2)
				.map(|n| n as i8)
				.map_err(|_| err_msg("binary"))
		}
		Rule::dec => {
			s.parse::<i8>()
				.map_err(|_| err_msg("decimal"))
		}
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	num.map_err(|message| Error::new_from_span(
		ErrorVariant::CustomError { message },
		source.as_span(),
	))
}

#[instrument]
fn parse_i12(source: Pair<Rule>) -> ParseResult<i16> {
	trace!("{source} - '{}'", source.as_str());

	fn fix_value(n: u16) -> i16 {
		if (n & 0x800) > 0 {
			(n | 0xF000) as i16
		} else {
			(n & 0x0FFF) as i16
		}
	}

	fn err_msg(src: &Pair<Rule>) -> String {
		format!(
			"expected a value between -2048 ($800) and 2047 ($7FF), found {}",
			src.as_str(),
		)
	}

	fn check_range(n: i16, src: &Pair<Rule>) -> Result<i16, String> {
		if (-2048..2048).contains(&n) {
			Ok(n)
		} else {
			Err(err_msg(src))
		}
	}

	let s = source.as_str().replace('_', "");
	let result = match source.as_rule() {
		Rule::hex => u16::from_str_radix(&s, 16).map(fix_value),
		Rule::bin => u16::from_str_radix(&s, 2).map(fix_value),
		Rule::dec => s.parse::<i16>(),
		_ => unreachable!("{source} - '{}'", source.as_str()),
	};

	result
		.map_err(|_| err_msg(&source))
		.and_then(|n| check_range(n, &source))
		.map_err(|message| Error::new_from_span(
			ErrorVariant::CustomError { message },
			source.as_span(),
		))
}

enum Ctrl { Gbr, Sr, Vbr }

#[instrument]
fn parse_ctrl(source: Pair<Rule>) -> Ctrl {
	match source.as_rule() {
		Rule::gbr => Ctrl::Gbr,
		Rule::sr  => Ctrl::Sr,
		Rule::vbr => Ctrl::Vbr,
		_ => unreachable!("{source} - '{}'", source.as_str()),
	}
}

enum Sys { Mach, Macl, Pr }

#[instrument]
fn parse_sys(source: Pair<Rule>) -> Sys {
	match source.as_rule() {
		Rule::mach => Sys::Mach,
		Rule::macl => Sys::Macl,
		Rule::pr   => Sys::Pr,
		_ => unreachable!("{source} - '{}'", source.as_str()),
	}
}

#[cfg(test)]
mod parser {
	use super::*;

	macro_rules! test_single {
		($name:ident, $input:expr, $val:expr, u8) => {
			test_single!($name, $input, $val, parse_u8);
		};
		($name:ident, $input:expr, $val:expr, i8) => {
			test_single!($name, $input, $val, parse_i8);
		};
		($name:ident, $input:expr, $val:expr, $parse:ident) => {
			#[test]
			fn $name() {
				let mut source = Sh2Parser::parse(Rule::num, $input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				let src = source.next().unwrap();
				let num = $parse(src)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				assert_eq!($val, num);
			}
		};
		($input:expr, $asm:expr) => {
			let out = super::parser($input)
				.map_err(|e| panic!("{e}"))
				.unwrap();
			assert_eq!(out.0, vec![$asm]);
		};
		($name:ident, $input:expr, $asm:expr) => {
			#[test]
			fn $name() {
				let out = super::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				assert_eq!(out.0, vec![$asm]);
			}
		};
	}

	test_single!(u8_hex,     "$C9",        201, u8);
	test_single!(u8_bin,     "%1100_1001", 201, u8);
	test_single!(i8_hex_pos, "$49",         73, i8);
	test_single!(i8_hex_neg, "$C9",        -55, i8);
	test_single!(i8_bin_pos, "%0100_1001",  73, i8);
	test_single!(i8_bin_neg, "%1100_1001", -55, i8);

	test_single!(clrmac, "\tclrmac", Asm::ClrMac);
	test_single!(clrt,   "\tclrt",   Asm::ClrT);
	test_single!(div0u,  "\tdiv0u",  Asm::Div0U);
	test_single!(nop,    "\tnop",    Asm::Nop);
	test_single!(rte,    "\trte",    Asm::Rte);
	test_single!(rts,    "\trts",    Asm::Rts);
	test_single!(sett,   "\tsett",   Asm::SetT);
	test_single!(sleep,  "\tsleep",  Asm::Sleep);

	test_single!(bf,     "\tbf @(34,pc)",       Asm::Bf(34));
	test_single!(bfs,    "\tbf/s @($C9,pc)",    Asm::BfS(-55));
	test_single!(bra,    "\tbra @($FFC,pc)",    Asm::Bra(-4));
	test_single!(braf,   "\tbraf @r3",          Asm::BraF(3));
	test_single!(bsr,    "\tbsr @($7FC,pc)",    Asm::Bsr(2044));
	test_single!(bsrf,   "\tbsrf @r0",          Asm::BsrF(0));
	test_single!(bt,     "\tbt @($07,pc)",      Asm::Bt(7));
	test_single!(bts,    "\tbt/s @(-12,pc)",    Asm::BtS(-12));
	test_single!(dt,     "\tdt r2",             Asm::Dt(2));
	test_single!(jmp,    "\tjmp @r7",           Asm::Jmp(7));
	test_single!(jsr,    "\tjsr @r9",           Asm::Jsr(9));
	test_single!(mova,   "\tmova @($57,pc),r0", Asm::MovA(0x57));
	test_single!(movt,   "\tmovt r0",           Asm::MovT(0));
	test_single!(rotcl,  "\trotcl r4",          Asm::RotCL(4));
	test_single!(rotcr,  "\trotcr r5",          Asm::RotCR(5));
	test_single!(rotl,   "\trotl r4",           Asm::RotL(4));
	test_single!(rotr,   "\trotr r5",           Asm::RotR(5));
	test_single!(shal,   "\tshal r10",          Asm::ShAL(10));
	test_single!(shar,   "\tshar r11",          Asm::ShAR(11));
	test_single!(shll,   "\tshll r12",          Asm::ShLL(12));
	test_single!(shll2,  "\tshll2 r15",         Asm::ShLL2(15));
	test_single!(shll8,  "\tshll8 r14",         Asm::ShLL8(14));
	test_single!(shll16, "\tshll16 r13",        Asm::ShLL16(13));
	test_single!(shlr,   "\tshlr r12",          Asm::ShLR(12));
	test_single!(shlr2,  "\tshlr2 r15",         Asm::ShLR2(15));
	test_single!(shlr8,  "\tshlr8 r14",         Asm::ShLR8(14));
	test_single!(shlr16, "\tshlr16 r13",        Asm::ShLR16(13));
	test_single!(tas,    "\ttas.b @r10",        Asm::TaS(10));
	test_single!(trapa,  "\ttrapa #$AA",        Asm::TrapA(170));

	test_single!(addc,  "\taddc r2,r7",      Asm::AddC(2, 7));
	test_single!(addv,  "\taddv r2,r7",      Asm::AddV(2, 7));
	test_single!(div0s, "\tdiv0s sp,r0",     Asm::Div0S(15, 0));
	test_single!(div1,  "\tdiv1 r1,r1",      Asm::Div1(1, 1));
	test_single!(extsb, "\texts.b sp,sp",    Asm::ExtSByte(15, 15));
	test_single!(extsw, "\texts.w sp,sp",    Asm::ExtSWord(15, 15));
	test_single!(extub, "\textu.b sp,sp",    Asm::ExtUByte(15, 15));
	test_single!(extuw, "\textu.w sp,sp",    Asm::ExtUWord(15, 15));
	test_single!(macw,  "\tmac.w @r3+,@r6+", Asm::MacWord(3, 6));
	test_single!(macl,  "\tmac.l @r6+,@r3+", Asm::MacLong(6, 3));
	test_single!(neg,   "\tneg r3,r0",       Asm::Neg(3, 0));
	test_single!(negc,  "\tnegc r7,r7",      Asm::NegC(7, 7));
	test_single!(not,   "\tnot r9,r0",       Asm::Not(9, 0));
	test_single!(sub,   "\tsub r5,r4",       Asm::Sub(5, 4));
	test_single!(subc,  "\tsubc r2,r1",      Asm::SubC(2, 1));
	test_single!(subv,  "\tsubv r3,r4",      Asm::SubV(3, 4));
	test_single!(swapb, "\tswap.b r5,r12",   Asm::SwapByte(5, 12));
	test_single!(swapw, "\tswap.w r0,r13",   Asm::SwapWord(0, 13));
	test_single!(xtrct, "\txtrct r5,r14",    Asm::Xtrct(5, 14));

	test_single!(and,  "\tand r7,r8",            Asm::AndReg(7, 8));
	test_single!(andi, "\tand #$12,r0",          Asm::AndImm(18));
	test_single!(andb, "\tand.b #250,@(r0,gbr)", Asm::AndByte(250));
	test_single!(or,   "\tor r7,r8",             Asm::OrReg(7, 8));
	test_single!(ori,  "\tor #$12,r0",           Asm::OrImm(18));
	test_single!(orb,  "\tor.b #250,@(r0,gbr)",  Asm::OrByte(250));
	test_single!(tst,  "\ttst r7,r8",            Asm::TstReg(7, 8));
	test_single!(tsti, "\ttst #$12,r0",          Asm::TstImm(18));
	test_single!(tstb, "\ttst.b #250,@(r0,gbr)", Asm::TstByte(250));
	test_single!(xor,  "\txor r7,r8",            Asm::XorReg(7, 8));
	test_single!(xori, "\txor #$12,r0",          Asm::XorImm(18));
	test_single!(xorb, "\txor.b #250,@(r0,gbr)", Asm::XorByte(250));

	test_single!(cmpeqi, "\tcmp/eq #-7,r0",  Asm::CmpEqImm(-7));
	test_single!(cmpeqr, "\tcmp/eq r5,r7",   Asm::CmpEqReg(5, 7));
	test_single!(cmpge,  "\tcmp/ge r1,r15",  Asm::CmpGE(1, 15));
	test_single!(cmpgt,  "\tcmp/gt r2,r14",  Asm::CmpGT(2, 14));
	test_single!(cmphi,  "\tcmp/hi r3,r13",  Asm::CmpHI(3, 13));
	test_single!(cmphs,  "\tcmp/hs r4,r12",  Asm::CmpHS(4, 12));
	test_single!(cmpstr, "\tcmp/str r5,r11", Asm::CmpSTR(5, 11));
	test_single!(cmppl,  "\tcmp/pl r6",      Asm::CmpPL(6));
	test_single!(cmppz,  "\tcmp/pz r10",     Asm::CmpPZ(10));

	test_single!(add,      "\tadd r2,r3",       Asm::AddReg(2, 3));
	test_single!(addi,     "\tadd #45,r5",      Asm::AddImm(45, 5));
	test_single!(mul,      "\tmul.l r0,r10",    Asm::Mul(0, 10));
	test_single!(muls,     "\tmuls.w r1,r11",   Asm::MulS(1, 11));
	test_single!(mulu,     "\tmulu.w r2,r12",   Asm::MulU(2, 12));
	test_single!(ldcgbr,   "\tldc r2,gbr",      Asm::LdcGbr(2));
	test_single!(ldcsr,    "\tldc r3,sr",       Asm::LdcSr(3));
	test_single!(ldcvbr,   "\tldc r4,vbr",      Asm::LdcVbr(4));
	test_single!(ldcgbr2,  "\tldc.l @r2+,gbr",  Asm::LdcGbrInc(2));
	test_single!(ldcsr2,   "\tldc.l @r3+,sr",   Asm::LdcSrInc(3));
	test_single!(ldcvbr2,  "\tldc.l @r4+,vbr",  Asm::LdcVbrInc(4));
	test_single!(ldsmach,  "\tlds r4,mach",     Asm::LdsMach(4));
	test_single!(ldsmacl,  "\tlds r5,macl",     Asm::LdsMacl(5));
	test_single!(ldspr,    "\tlds r6,pr",       Asm::LdsPr(6));
	test_single!(ldsmach2, "\tlds.l @r7+,mach", Asm::LdsMachInc(7));
	test_single!(ldsmacl2, "\tlds.l @r8+,macl", Asm::LdsMaclInc(8));
	test_single!(ldspr2,   "\tlds.l @r9+,pr",   Asm::LdsPrInc(9));
	test_single!(dmuls,    "\tdmuls.l r3,r2",   Asm::DMulS(3, 2));
	test_single!(dmulu,    "\tdmulu.l r1,r0",   Asm::DMulU(1, 0));

	test_single!(movi,   "\tmov #78,R3",         Asm::MovImm(78, 3));
	test_single!(mov,    "\tmov r13,r9",         Asm::MovReg(13, 9));
	test_single!(movbar, "\tmov.b @r7,r9",       Asm::MovAddrToReg(Size::Byte, 7, 9));
	test_single!(movwar, "\tmov.w @r7,r9",       Asm::MovAddrToReg(Size::Word, 7, 9));
	test_single!(movlar, "\tmov.l @r7,r9",       Asm::MovAddrToReg(Size::Long, 7, 9));
	test_single!(movbra, "\tmov.b r3,@r4",       Asm::MovRegToAddr(Size::Byte, 3, 4));
	test_single!(movwra, "\tmov.w r3,@r4",       Asm::MovRegToAddr(Size::Word, 3, 4));
	test_single!(movlra, "\tmov.l r3,@r4",       Asm::MovRegToAddr(Size::Long, 3, 4));
	test_single!(movbg0, "\tmov.b @(45,gbr),r0", Asm::MovGbrToR0(Size::Byte, 45));
	test_single!(movwg0, "\tmov.w @(45,gbr),r0", Asm::MovGbrToR0(Size::Word, 45));
	test_single!(movlg0, "\tmov.l @(45,gbr),r0", Asm::MovGbrToR0(Size::Long, 45));
	test_single!(movb0g, "\tmov.b r0,@(35,gbr)", Asm::MovR0ToGbr(Size::Byte, 35));
	test_single!(movw0g, "\tmov.w r0,@(35,gbr)", Asm::MovR0ToGbr(Size::Word, 35));
	test_single!(movl0g, "\tmov.l r0,@(35,gbr)", Asm::MovR0ToGbr(Size::Long, 35));
	test_single!(movbd0, "\tmov.b @(r0,r2),r6",  Asm::MovDispR0ToReg(Size::Byte, 2, 6));
	test_single!(movwd0, "\tmov.w @(r0,r2),r6",  Asm::MovDispR0ToReg(Size::Word, 2, 6));
	test_single!(movld0, "\tmov.l @(r0,r2),r6",  Asm::MovDispR0ToReg(Size::Long, 2, 6));
	test_single!(movb0d, "\tmov.b r7,@(r0,r5)",  Asm::MovRegToDispR0(Size::Byte, 7, 5));
	test_single!(movw0d, "\tmov.w r7,@(r0,r5)",  Asm::MovRegToDispR0(Size::Word, 7, 5));
	test_single!(movl0d, "\tmov.l r7,@(r0,r5)",  Asm::MovRegToDispR0(Size::Long, 7, 5));
	test_single!(movbir, "\tmov.b @r11+,r14",    Asm::MovIncToReg(Size::Byte, 11, 14));
	test_single!(movwir, "\tmov.w @r11+,r14",    Asm::MovIncToReg(Size::Word, 11, 14));
	test_single!(movlir, "\tmov.l @r11+,r14",    Asm::MovIncToReg(Size::Long, 11, 14));
	test_single!(movbrd, "\tmov.b r15,@-sp",     Asm::MovRegToDec(Size::Byte, 15, 15));
	test_single!(movwrd, "\tmov.w r15,@-sp",     Asm::MovRegToDec(Size::Word, 15, 15));
	test_single!(movlrd, "\tmov.l r15,@-sp",     Asm::MovRegToDec(Size::Long, 15, 15));

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bf @(243,pc)
  | 	     ^-^
  |
  = expected a decimal value between -128 and 127"]
	fn bf_too_far_forward() {
		test_single!("\tbf @(243,pc)", Asm::Bf(0));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bf @(-243,pc)
  | 	     ^--^
  |
  = expected a decimal value between -128 and 127"]
	fn bf_too_far_behind() {
		test_single!("\tbf @(-243,pc)", Asm::Bf(0));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bt @(243,pc)
  | 	     ^-^
  |
  = expected a decimal value between -128 and 127"]
	fn bt_too_far_forward() {
		test_single!("\tbt @(243,pc)", Asm::Bt(0));
	}

	#[test]
	#[should_panic = " --> 1:7
  |
1 | 	bt @(-243,pc)
  | 	     ^--^
  |
  = expected a decimal value between -128 and 127"]
	fn bt_too_far_behind() {
		test_single!("\tbt @(-243,pc)", Asm::Bt(0));
	}

	#[test]
	#[should_panic = " --> 1:11
  |
1 | 	and #$40,r3
  | 	         ^---
  |
  = expected R0"]
	fn andi_requires_r0_as_dst() {
		test_single!("\tand #$40,r3", Asm::AndImm(64));
	}

	#[test]
	#[should_panic = " --> 1:13
  |
1 | 	and.b #0,@(r9,gbr)
  | 	           ^---
  |
  = expected R0"]
	fn andb_requires_r0_as_displacement() {
		test_single!("\tand.b #0,@(r9,gbr)", Asm::AndByte(0));
	}

	#[test]
	#[should_panic = " --> 1:16
  |
1 | 	and.b #5,@(r0,vbr)
  | 	              ^---
  |
  = expected GBR"]
	fn andb_requires_gbr_as_base() {
		test_single!("\tand.b #5,@(r0,vbr)", Asm::AndByte(5));
	}

	#[test]
	#[should_panic = " --> 1:1
  |
1 | stuff
  | ^---
  |
  = expected EOF, instruction, assembler directive, Label, or Value"]
	fn failure_no_starting_tab() {
		test_single!("stuff", Asm::ClrT);
	}

	#[test]
	#[should_panic = " --> 1:2
  |
1 | 	stuff
  | 	^---
  |
  = expected instruction or assembler directive"]
	fn failure_non_command() {
		test_single!("\tstuff", Asm::ClrT);
	}
}

#[instrument]
fn output(asm: &[Asm]) -> Vec<u8> {
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

			Asm::Bf(d)      => out.push(0x8B00 | *d as u16),
			Asm::BfS(d)     => out.push(0x8F00 | *d as u16),
			Asm::Bra(d)     => out.push(0xA000 | (*d & 0xFFF) as u16),
			Asm::BraF(r)    => out.push(0x0023 | (*r as u16) << 8),
			Asm::Bsr(d)     => out.push(0xB000 | (*d & 0xFFF) as u16),
			Asm::BsrF(r)    => out.push(0x0003 | (*r as u16) << 8),
			Asm::Bt(d)      => out.push(0x8900 | *d as u16),
			Asm::BtS(d)     => out.push(0x8D00 | *d as u16),
			Asm::Dt(r)      => out.push(0x4010 | (*r as u16) << 8),
			Asm::Jmp(r)     => out.push(0x402B | (*r as u16) << 8),
			Asm::Jsr(r)     => out.push(0x400B | (*r as u16) << 8),
			Asm::MovA(d)    => out.push(0xC700 | *d as u16),
			Asm::MovT(r)    => out.push(0x0029 | (*r as u16) << 8),
			Asm::RotCL(r)   => out.push(0x4044 | (*r as u16) << 8),
			Asm::RotCR(r)   => out.push(0x4045 | (*r as u16) << 8),
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
			Asm::CmpEqImm(i) => out.push(0x8800 | *i as u16),

			Asm::AddImm(i,r)   => out.push(0x7000 | (*r as u16) << 8 | *i as u16),
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
		}
	}

	out.into_iter()
		.flat_map(|word: u16| word.to_be_bytes())
		.collect()
}

#[cfg(test)]
mod output {
	macro_rules! test_output {
		($name:ident, $input:expr, $bytes:expr) => {
			#[test]
			fn $name() {
				let asm = super::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				let out = super::output(&asm.0);
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
	test_output!(rotcl,  "\trotcl r15",         &[0x4F, 0x44]);
	test_output!(rotcr,  "\trotcr r14",         &[0x4E, 0x45]);
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

