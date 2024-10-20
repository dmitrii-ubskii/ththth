use std::fmt;

use crate::{string::GString, Env, EvaluationError};

#[derive(Clone, Debug)]
pub enum Expression {
	Atom(Atom),
	Expression { left: Box<Expression>, right: Box<Expression> },
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
	Atom(Atom),
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
			Self::Atom(atom) => write!(f, "{atom}"),
			Self::Builtin(_) => write!(f, "#builtin"),
		}
	}
}
