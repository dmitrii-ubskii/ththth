use std::error::Error;

use crate::Expression;

pub fn parse(mut input: &str) -> Result<Vec<Expression>, Box<dyn Error>> {
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
			if next_char.is_whitespace() || next_char == ')' {
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
		Expression::Symbol(atom.into())
	}
}

fn parse_list(input: &mut &str) -> Vec<Expression> {
	let mut list = Vec::new();
	while let Some(token) = read_token(input) {
		match token {
			")" => break,
			"(" => list.push(Expression::Application(parse_list(input))),
			atom => list.push(parse_atom(atom)),
		}
	}
	list
}
