
use std::{env, fs};

mod asm;
mod i4;
mod output;
mod parser;

use parser::parser;
use output::output;

fn main() {
	let mut args = env::args();
	args.next();

	let file_name = args.next()
		.expect("no source file name");
	let source = fs::read_to_string(file_name)
		.expect("unable to read source file");
	let ins = parser(&source)
		.expect("unable to parse source");
	println!("{ins:#?}");
	let out = output(&ins);
	println!("{out:02X?}");
}

