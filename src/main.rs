
use std::{env, fs, path::PathBuf};

mod asm;
mod i4;
mod output;
mod parser;
mod a2;
//mod p2;
mod p3;

mod lexer;
mod tokens;

use parser::parser;
use asm::output;

fn main() {
	tracing_subscriber::fmt()
		.compact()
		.without_time()
		.with_max_level(tracing::Level::TRACE)
		.init();
	let mut args = env::args();
	args.next();

	let file_name = args.next()
		.expect("no source file name");
	let file_path = PathBuf::from(&file_name);
	let file_path = file_path.parent()
		.expect(&format!("unable to parse parent file-path for '{file_name}'"));

	let use_parser2 = args.next()
		.map(|arg| arg == "p2")
		.unwrap_or(false);

	let source = fs::read_to_string(file_name)
		.expect("unable to read source file");
	if use_parser2 {
		let tokens = lexer::eval(&source)
			.expect("unable to lex source (v2)");
		let out = p3::eval(&tokens, file_path.to_path_buf())
			//.expect("unable to parse source (v2)")
			;
		todo!("{out:?}");
	} else {
		let ins = parser(&source)
			.expect("unable to parse source");
		println!("{ins:#?}");
		let out = output(&ins);
		println!("{out:02X?}");
	}
}

