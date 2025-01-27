use crate::parser::{Ins, Label, State};

#[derive(Default)]
pub(crate) struct Output {
	pub(crate) sections: crate::SectionMap,
	pub(crate) labels: crate::LabelMap,
	pub(crate) values: std::collections::HashMap<Label, i8>,
}

impl Output {
	pub(super) fn add_to_section(&mut self, section_key: u64, ins: Ins) {
		self
			.sections
			.entry(section_key)
			.or_default()
			.push(State::Incomplete(ins));
	}
}

impl std::fmt::Debug for Output {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		for (address, section) in &self.sections {
			writeln!(fmt, "Address: ${address:08X}")?;
			for instr in section {
				writeln!(fmt, "\t{instr:?}")?;
			}
		}

		writeln!(fmt, "Labels:")?;
		for label in self.labels.keys() {
			writeln!(fmt, "\t{label}")?;
		}

		Ok(())
	}
}
