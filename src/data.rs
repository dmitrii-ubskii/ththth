use std::fmt;

use crate::{string::GString, Env, EvaluationError};

#[derive(Clone, Debug)]
pub enum Expression {
	Atom(Atom),
	Expression { left: Box<Expression>, right: Box<Expression> },
}

impl Expression {
	pub fn as_symbol(&self) -> Option<&GString> {
		match self {
			Self::Atom(Atom::Symbol(symbol)) => Some(symbol),
			_ => None,
		}
	}

	pub fn as_list(&self) -> Option<impl Iterator<Item = &Expression>> {
		match self {
			Self::Expression { left, right } => {
				let tail: Vec<_> = right.as_list()?.collect();
				Some(Some(left as _).into_iter().chain(tail))
			}
			Self::Atom(Atom::Nil) => Some(None.into_iter().chain(vec![])),
			Self::Atom(_) => None,
		}
	}
}

#[derive(Clone, Debug)]
pub enum Atom {
	Nil,
	Boolean(bool),
	Number(f64),
	Symbol(GString),
}

impl fmt::Display for Atom {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Atom::Nil => write!(f, "()"),
			Atom::Boolean(true) => write!(f, "#t"),
			Atom::Boolean(false) => write!(f, "#f"),
			Atom::Number(num) => write!(f, "{num}"),
			Atom::Symbol(sym) => write!(f, "{sym}"),
		}
	}
}

#[derive(Clone, Debug)]
pub enum Datum {
	Void,
	// Err,
	Atom(Atom),
	// List { head: Box<Datum>, tail: Box<Datum> },
	Expression { formals: Vec<GString>, expression: Expression },
	Builtin(fn(&Expression, &Env<'_>) -> Result<Datum, EvaluationError>),
}

impl Datum {
	pub fn as_number(&self) -> Option<f64> {
		match *self {
			Datum::Atom(Atom::Number(num)) => Some(num),
			_ => None,
		}
	}
}

impl fmt::Display for Datum {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Void => Ok(()),
			// Self::Err => write!(f, "ERR"),
			Self::Atom(atom) => write!(f, "{atom}"),
			Self::Expression { .. } => write!(f, "#expression"),
			Self::Builtin(_) => write!(f, "#builtin"),
			// Self::List { head, tail } => {
			// 	write!(f, "(")?;
			// 	write!(f, "{head}")?;
			// 	let mut tail: &Self = tail;
			// 	loop {
			// 		match tail {
			// 			Self::Void => {
			// 				write!(f, ". <void>")?;
			// 				break;
			// 			}
			// 			Self::Atom(Atom::Nil) => break,
			// 			| Self::Err
			// 			| Self::Atom(_)
			// 			| Self::Expression { .. }
			// 			| Self::Builtin(_) => {
			// 				write!(f, ". {tail}")?;
			// 				break;
			// 			}
			// 			Self::List { head, tail: next } => {
			// 				write!(f, " {head}")?;
			// 				tail = next;
			// 			}
			// 		}
			// 	}
			// 	write!(f, ")")?;
			// 	Ok(())
			// }
		}
	}
}
