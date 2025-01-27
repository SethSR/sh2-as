use crate::parser::Reg;
use crate::Label;

#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Arg {
	DirReg(Reg),
	DispR0(Reg),
	DispReg(i8, Reg),
	DispPC(i8),
	DispGBR(i8),
	DispLabel(Label, Reg),
	IndReg(Reg),
	Label(Label),
	PostInc(Reg),
	PreDec(Reg),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Arg::DirReg(reg) => write!(fmt, "DirReg(R{reg})"),
			Arg::DispR0(reg) => write!(fmt, "DispR0(R{reg})"),
			Arg::DispReg(disp, reg) => write!(fmt, "DispReg({disp},R{reg})"),
			Arg::DispPC(disp) => write!(fmt, "DispPC({disp})"),
			Arg::DispGBR(disp) => write!(fmt, "DispGBR({disp})"),
			Arg::DispLabel(lbl, reg) => write!(fmt, "DispLabel({lbl},R{reg})"),
			Arg::IndReg(reg) => write!(fmt, "IndReg(R{reg})"),
			Arg::Label(lbl) => write!(fmt, "Label({lbl})"),
			Arg::PostInc(reg) => write!(fmt, "PostInc(R{reg})"),
			Arg::PreDec(reg) => write!(fmt, "PreDec(R{reg})"),
		}
	}
}
