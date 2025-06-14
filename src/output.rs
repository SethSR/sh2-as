
use crate::asm::Asm;

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

