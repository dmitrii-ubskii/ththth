use std::fmt;

use crate::{string::GString, Env};

#[derive(Clone)]
pub enum Expression {
	Datum(Datum),
	Expression { head: Box<Expression>, tail: Box<Expression> },
}

impl Expression {
	pub fn as_symbol(&self) -> Option<&GString> {
		match self {
			Self::Datum(Datum::Symbol(symbol)) => Some(symbol),
			_ => None,
		}
	}
}

impl fmt::Debug for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self}")
	}
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::Datum(atom) => write!(f, "{atom}"),
			Expression::Expression { head, tail } => {
				write!(f, "(")?;
				write!(f, "{head}")?;
				let mut tail: &Self = tail;
				loop {
					match tail {
						Self::Datum(Datum::Nil) => break,
						Self::Datum(atom) => {
							write!(f, " . {atom}")?;
							break;
						}
						Self::Expression { head, tail: new_tail } => {
							write!(f, " {head}")?;
							tail = new_tail;
						}
					}
				}
				write!(f, ")")?;
				Ok(())
			}
		}
	}
}

#[derive(Clone)]
pub enum Datum {
	Void,
	Err,
	Nil,
	Boolean(bool),
	Number(f64),
	Symbol(GString),
	List { head: Box<Datum>, tail: Box<Datum> },
	Closure { formals: Box<Expression>, body: Box<Expression> },
	Builtin(fn(&mut Env<'_>, Datum) -> Datum),
}

impl fmt::Debug for Datum {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self}")
	}
}

impl fmt::Display for Datum {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Void => write!(f, "<void>"),
			Self::Err => write!(f, "ERR"),
			Self::Nil => write!(f, "()"),
			Self::Boolean(true) => write!(f, "#t"),
			Self::Boolean(false) => write!(f, "#f"),
			Self::Number(num) => write!(f, "{num}"),
			Self::Symbol(sym) => write!(f, "{sym}"),
			Self::Closure { formals, body } => write!(f, "(lambda {formals} {body})"),
			Self::Builtin(_) => write!(f, "#builtin"),
			Self::List { head, tail } => {
				write!(f, "(")?;
				write!(f, "{head}")?;
				let mut tail: &Self = tail;
				loop {
					match tail {
						Self::Nil => break,
						Self::List { head, tail: next } => {
							write!(f, " {head}")?;
							tail = next;
						}
						_ => {
							write!(f, " . {tail}")?;
							break;
						}
					}
				}
				write!(f, ")")?;
				Ok(())
			}
		}
	}
}
