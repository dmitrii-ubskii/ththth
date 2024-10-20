use std::{
	error::Error,
	iter::{self, Peekable},
};

use crate::{Atom, Expression};

pub fn parse(mut input: &str) -> Result<Vec<Expression>, Box<dyn Error>> {
	if input.is_empty() {
		return Err("Unexpected EOF".into());
	}
	let mut input = iter::from_fn(|| read_token(&mut input)).peekable();
	Ok(iter::from_fn(
		|| if input.peek().is_none() { None } else { Some(parse_statement(&mut input)) },
	)
	.collect())
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

fn parse_statement<'a>(input: &mut Peekable<impl Iterator<Item = &'a str>>) -> Expression {
	match input.next().unwrap() {
		"(" => parse_expr(input),
		atom => Expression::Atom(parse_atom(atom)),
	}
}

fn parse_atom(atom: &str) -> Atom {
	match atom {
		"#t" => Atom::Boolean(true),
		"#f" => Atom::Boolean(false),
		atom => {
			if let Ok(num) = atom.parse() {
				Atom::Number(num)
			} else {
				Atom::Symbol(atom.into())
			}
		}
	}
}

fn parse_expr<'a>(input: &mut Peekable<impl Iterator<Item = &'a str>>) -> Expression {
	let left = match input.next().unwrap() {
		")" => return Expression::Atom(Atom::Nil),
		"(" => parse_expr(input),
		atom => Expression::Atom(parse_atom(atom)),
	};

	let right = match *input.peek().unwrap() {
		")" => {
			input.next();
			Expression::Atom(Atom::Nil)
		}
		_ => parse_expr(input),
	};

	Expression::Expression { head: Box::new(left), tail: Box::new(right) }
}
