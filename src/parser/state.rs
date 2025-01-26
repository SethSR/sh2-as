
use crate::parser::Ins;

#[derive(Clone, PartialEq, Eq)]
pub(crate) enum State {
	/// Instruction completed for output
	Complete(u16),
	/// Instruction / directive still waiting on label resolution
	Incomplete(Ins),
}

impl State {
	pub fn completed_or(&self, default: u16) -> u16 {
		match self {
			Self::Complete(inst) => *inst,
			Self::Incomplete(_) => default,
		}
	}
}

impl std::fmt::Debug for State {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Complete(inst) => write!(fmt, "Complete(${inst:04X})"),
			Self::Incomplete(inst) => write!(fmt, "Incomplete({inst:?})"),
		}
	}
}
