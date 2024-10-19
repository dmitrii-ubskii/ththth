use std::{
	env,
	error::Error,
	fmt, fs,
	io::{stdin, Read},
};

fn main() -> Result<(), Box<dyn Error>> {
	let input = match env::args().nth(1) {
		Some(path) => fs::read_to_string(path)?,
		None => {
			let mut buf = String::new();
			stdin().read_to_string(&mut buf)?;
			buf
		}
	};
	let program = parse(input.trim())?;
	for expr in program {
		println!("{}", execute(expr)?);
	}
	Ok(())
}

enum Expression {
	Number(f64),
	List(Vec<Expression>),
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::Number(num) => write!(f, "{num}"),
			Expression::List(_) => todo!(),
		}
	}
}

fn parse(mut input: &str) -> Result<Vec<Expression>, Box<dyn Error>> {
	if input.is_empty() {
		return Err("Unexpected EOF".into());
	}
	Ok(parse_list(&mut input))
}

fn read_token<'a>(input: &mut &'a str) -> Option<&'a str> {
	let mut char_indices = input.char_indices().peekable();
	let (_, first_char) = char_indices.next()?;
	assert!(!first_char.is_whitespace(), "{first_char:?}");
	let token_len;
	match first_char {
		'(' | ')' => token_len = 1,
		_ => loop {
			let Some(&(idx, next_char)) = char_indices.peek() else {
				token_len = input.len();
				break;
			};
			if next_char.is_whitespace() {
				token_len = idx;
				break;
			}
			char_indices.next();
		},
	}
	let whitespace_end;
	loop {
		let Some(&(idx, next_char)) = char_indices.peek() else {
			whitespace_end = input.len();
			break;
		};
		if !next_char.is_whitespace() {
			whitespace_end = idx;
			break;
		}
		char_indices.next();
	}

	let token = &input[..token_len];
	*input = &input[whitespace_end..];
	Some(token)
}

fn parse_atom(atom: &str) -> Expression {
	if let Ok(num) = atom.parse() {
		Expression::Number(num)
	} else {
		todo!()
	}
}

fn parse_list(input: &mut &str) -> Vec<Expression> {
	let mut list = Vec::new();
	while let Some(token) = read_token(input) {
		match token {
			")" => break,
			"(" => list.push(Expression::List(parse_list(input))),
			atom => list.push(parse_atom(atom)),
		}
	}
	list
}

fn execute(expr: Expression) -> Result<Expression, Box<dyn Error>> {
	match expr {
		Expression::Number(_) => Ok(expr),
		Expression::List(_list) => {
			todo!()
		}
	}
}
