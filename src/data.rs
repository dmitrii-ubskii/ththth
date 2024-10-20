use std::fmt;

use crate::string::GString;

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
}

impl fmt::Display for Datum {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Void => Ok(()),
			Self::Atom(atom) => write!(f, "{atom}"),
		}
	}
}
