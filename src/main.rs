
use std::{env, fs, path::PathBuf};

mod asm;
mod lexer;
mod parser;
mod tokens;

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
		.unwrap_or_else(|| panic!("unable to parse parent file-path for '{file_name}'"));

	let source = fs::read_to_string(file_name)
		.expect("unable to read source file");

	let tokens = lexer::eval(&source)
		.expect("unable to lex source (v2)");
	let out = parser::eval(&tokens, file_path.to_path_buf());
	for asm in out {
		println!("{asm}");
	}
}

