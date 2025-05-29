
use pest::Parser;
use pest::iterators::Pair;
use pest::error::{Error, ErrorVariant};
use pest_derive::Parser;

use tracing::{instrument, trace};

use crate::i4::I4;
use crate::asm::{Asm, Reg, Size};
use crate::output::Output;

#[derive(Parser)]
#[grammar = "../sh2.pest"]
struct Sh2Parser;

type ParseResult<T> = Result<T, Error<Rule>>;

fn extra_rules(src: Pair<Rule>) {
	#[cfg(not(test))]
	tracing::error!("unexpected {src} - '{}'", src.as_str());

	#[cfg(test)]
	panic!("unexpected {src} - '{}'", src.as_str());
}

#[instrument]
pub fn parser(input: &str) -> ParseResult<Vec<Asm>> {
	let mut output = Output::default();
	let source = Sh2Parser::parse(Rule::program, input)
		.map_err(|e| e.renamed_rules(rename_rules))?;
	for src in source {
		trace!("Parsing: {src} - '{}'", src.as_str());
		output = match src.as_rule() {
			Rule::ins_line => parse_ins_line(src, output)?,
			Rule::dir_line => parse_dir_line(src, output)?,
			Rule::lbl_line => todo!("{src} - '{}'", src.as_str()),
			Rule::val_line => todo!("{src} - '{}'", src.as_str()),
			Rule::EOI => continue,
			_ => {
				extra_rules(src);
				continue;
			}
		};
	}

	if output.repeat.is_some() {
		todo!("unclosed 'repeat' directive");
	} else {
		Ok(output.asm)
	}
}

fn rename_rules(rule: &Rule) -> String {
	match rule {
		Rule::EOI => "EOF".to_string(),

		Rule::bin => "a binary value".to_string(),
		Rule::sp => "SP register".to_string(),
		Rule::reg => "general register".to_string(),
		Rule::r0 => "R0".to_string(),
		Rule::gbr => "GBR".to_string(),

		Rule::ins_line | Rule::ins => "instruction".to_string(),
		Rule::dir_line | Rule::dir => "assembler directive".to_string(),
		Rule::lbl_line => "Label".to_string(),
		Rule::val_line => "Value".to_string(),

		Rule::addr_reg_or_sp => "indirect register or SP (ex: '@r1')".to_string(),
		Rule::reg_post_inc => "indirect register or SP w/ post-increment (ex: '@r2+')".to_string(),
		Rule::reg_pre_dec => "indirect register or SP w/ pre-decrement (ex: '@-r3')".to_string(),

		_ => format!("{rule:?}"),
	}
}

#[instrument]
fn parse_dir_line(source: Pair<Rule>, mut output: Output) -> ParseResult<Output> {
	trace!("{source} - '{}'", source.as_str());

	let dir = source.into_inner().next().unwrap();
	match dir.as_rule() {
		Rule::dir => {
			let mut args = dir.into_inner();
			let arg = args.next().unwrap();
			match arg.as_rule() {
				Rule::dir_constant_l => {
					let num = parse_num(arg.into_inner().next().unwrap())?;
					output.push(Asm::Long(num as u32));
				}
				Rule::dir_constant_w => {
					let num = parse_num(arg.into_inner().next().unwrap())?;
					output.push(Asm::Word(num as u16));
				}
				Rule::dir_constant_b => {
					let num = parse_num(arg.into_inner().next().unwrap())?;
					output.push(Asm::Byte(num as u8));
				}
				Rule::dir_repeat => {
					let cnt = parse_u8(arg.into_inner().next().unwrap())?;
					output.repeat = Some((cnt as usize, Vec::default()));
				}
				Rule::dir_endr => {
					if let Some((cnt, repeat)) = output.repeat.take() {
						for _ in 0..cnt {
							output.asm.extend(&repeat);
						}
					} else {
						todo!("unexpected end-repeat directive");
					}
				}
				_ => todo!("{arg}"),
			}
		}
		_ => unreachable!("unknown directive: {}", dir.as_str()),
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
			Rule::ins_stc => {
				let mut args = src.into_inner();
				let src = parse_ctrl(args.next().unwrap());
				let dst = parse_reg_or_sp(args.next().unwrap())?;
				let ins = match src {
					Ctrl::Gbr => Asm::StcGbr(dst),
					Ctrl::Sr  => Asm::StcSr(dst),
					Ctrl::Vbr => Asm::StcVbr(dst),
				};
				output.push(ins);
			}
			Rule::ins_stc_dec => {
				let mut args = src.into_inner();
				let src = parse_ctrl(args.next().unwrap());
				let dst = parse_reg_pre_dec(args.next().unwrap())?;
				let ins = match src {
					Ctrl::Gbr => Asm::StcGbrDec(dst),
					Ctrl::Sr  => Asm::StcSrDec(dst),
					Ctrl::Vbr => Asm::StcVbrDec(dst),
				};
				output.push(ins);
			}
			Rule::ins_sts => {
				let mut args = src.into_inner();
				let src = parse_sys(args.next().unwrap());
				let dst = parse_reg_or_sp(args.next().unwrap())?;
				let ins = match src {
					Sys::Mach => Asm::StsMach(dst),
					Sys::Macl => Asm::StsMacl(dst),
					Sys::Pr   => Asm::StsPr(dst),
				};
				output.push(ins);
			}
			Rule::ins_sts_dec => {
				let mut args = src.into_inner();
				let src = parse_sys(args.next().unwrap());
				let dst = parse_reg_pre_dec(args.next().unwrap())?;
				let ins = match src {
					Sys::Mach => Asm::StsMachDec(dst),
					Sys::Macl => Asm::StsMaclDec(dst),
					Sys::Pr   => Asm::StsPrDec(dst),
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
fn parse_num(source: Pair<Rule>) -> ParseResult<i64> {
	trace!("{source} - '{}'", source.as_str());

	fn err_msg(base: &str) -> String {
		format!("expected a {base} value between {} and {}", i32::MIN, u32::MAX)
	}

	let s = source.as_str().replace('_', "");
	let num = match source.as_rule() {
		Rule::hex => u32::from_str_radix(&s, 16)
			.map(|n| n as i64)
			.map_err(|_| err_msg("hexadecimal")),
		Rule::bin => u32::from_str_radix(&s, 2)
			.map(|n| n as i64)
			.map_err(|_| err_msg("binary")),
		Rule::dec => s.parse::<i64>()
			.map_err(|_| err_msg("decimal")),
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
mod tests {
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
			assert_eq!(out, [$asm]);
		};
		($name:ident, $input:expr, $asm:expr) => {
			#[test]
			fn $name() {
				let out = super::parser($input)
					.map_err(|e| panic!("{e}"))
					.unwrap();
				assert_eq!(out, [$asm]);
			}
		};
	}

	#[test]
	fn repeat() {
		let out = super::parser("
	.repeat 16
	nop
	.endr
").unwrap();
		assert_eq!(out, [Asm::Nop; 16]);
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
	test_single!(stcgbr,   "\tstc gbr,r4",      Asm::StcGbr(4));
	test_single!(stcsr,    "\tstc sr,r4",       Asm::StcSr(4));
	test_single!(stcvbr,   "\tstc vbr,r4",      Asm::StcVbr(4));
	test_single!(stcgbr2,  "\tstc.l gbr,@-r5",  Asm::StcGbrDec(5));
	test_single!(stcsr2,   "\tstc.l sr,@-r5",   Asm::StcSrDec(5));
	test_single!(stcvbr2,  "\tstc.l vbr,@-r5",  Asm::StcVbrDec(5));
	test_single!(stsmach,  "\tsts mach,r4",     Asm::StsMach(4));
	test_single!(stsmacl,  "\tsts macl,r4",     Asm::StsMacl(4));
	test_single!(stspr,    "\tsts pr,r4",       Asm::StsPr(4));
	test_single!(stsmach2, "\tsts.l mach,@-r5", Asm::StsMachDec(5));
	test_single!(stsmacl2, "\tsts.l macl,@-r5", Asm::StsMaclDec(5));
	test_single!(stspr2,   "\tsts.l pr,@-r5",   Asm::StsPrDec(5));
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

