/*
fn main() -> miette::Result<()> {

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let target = args.next().unwrap_or("asm.out".to_string());

	// TODO - srenshaw - Change this to a CLI option.
	let is_silent = true;

	let file = std::fs::read_to_string(source).into_diagnostic()?;

	let tokens = match lexer(&file) {
		Ok(tokens) => tokens,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	for token in &tokens {
		if token.get_type() == TokenType::IdUnknown || !is_silent {
			println!("{token:?}");
		}
	}

	let mut data = match parser(&tokens) {
		Ok(tables) => tables,
		Err(errors) => {
			for error in errors {
				eprintln!("{error}");
			}
			return Ok(());
		}
	};

	if !is_silent {
		println!("State: initial");
		println!("{data:?}");
	}

	let limit = 10;
	for _ in 0..limit {
		if resolver(&mut data) {
			break;
		}
	}

	if !is_silent {
		println!("State: final");
		println!("{data:?}");
	}

	// TODO - srenshaw - This is just for debugging purposes. This is not the "real" output!
	for (_, section) in data.sections {
		let output = section
			.iter()
			.map(|state| state.completed_or(0xDEAD))
			.flat_map(|word| [(word >> 8) as u8, word as u8])
			.collect::<Vec<u8>>();
		std::fs::write(&target, output).into_diagnostic()?;
	}

	Ok(())
}
*/

use std::collections::HashMap;
use std::fs::read_to_string;

use tracing::{
	warn,
	instrument,
};

mod instructions;
use instructions::Ins;

mod arg;
use arg::Arg;

mod parser;
use parser::{Output, parser};

mod resolver;
use resolver::{Item, resolver};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Size {
	Byte,
	Word,
	Long,
}

type Reg = u8;
type Label = std::rc::Rc<str>;
#[instrument]
fn main() {
	tracing_subscriber::fmt::init();

	let mut args = std::env::args();
	args.next();

	let source = args.next().expect("missing source file");
	let input = read_to_string(&source).expect("unable to read source file");

	//let target = args.next().unwrap_or("asm.out".to_string());

	let Output { sections, mut labels, values } = parser(&input);

/*
	println!("values: {}", values.len());
	for (name, value) in &values {
		println!("  {name} = {value}");
	}
	println!("labels: {}", labels.len());
	for (name, addr) in &labels {
		println!("  {name} : ${addr:0X?}");
	}
*/
	println!("sections: {}", sections.len());
	for (addr, section) in &sections {
		println!("  addr: ${addr:0X}");
		for (i, ins) in section.iter().enumerate() {
			println!("{:0X}: {ins:0X?}", addr + i as u32 * 2);
		}
	}
	let mut sections: HashMap<u32, Vec<Item>> = sections.into_iter()
		.map(|(addr,section)| (addr, section.into_iter().map(Item::Ins).collect()))
		.collect();

	resolver(&mut sections, &mut labels, &values);

/*
	println!("labels: {}", labels.len());
	for (label, addr) in labels {
		println!("  {label} = {addr:0X?}");
	}
	println!("sections: {}", sections.len());
	for (addr, section) in &sections {
		println!("  section: {addr:0X}");
		for item in section {
			match item {
				Item::Byte(_) => {}
				Item::Word(_) => {}
				Item::Ins(ins) => println!("    {ins:?}"),
			}
		}
	}
*/
}

