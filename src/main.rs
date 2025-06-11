
use std::{env, fs};

mod asm;
mod i4;
mod output;
mod parser;
//mod p2;

mod lexer;
mod tokens;

use parser::parser;
use output::output;

fn main() {
	let mut args = env::args();
	args.next();

	let file_name = args.next()
		.expect("no source file name");

	let use_parser2 = args.next()
		.map(|arg| arg == "p2")
		.unwrap_or(false);

	let source = fs::read_to_string(file_name)
		.expect("unable to read source file");
	let ins = if use_parser2 {
		let tokens = lexer::eval(&source)
			.expect("unable to lex source (v2)");
		println!("{tokens:?}");
		vec![]
		//p2::parser(&source)
		//	.expect("unable to parse source (v2)")
	} else {
		parser(&source)
			.expect("unable to parse source")
	};
	println!("{ins:#?}");
	let out = output(&ins);
	println!("{out:02X?}");
}

