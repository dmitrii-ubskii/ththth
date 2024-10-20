use std::fmt;

use crate::{string::GString, Env, EvaluationError};

#[derive(Clone)]
pub enum Expression {
	Atom(Atom),
	Expression { head: Box<Expression>, tail: Box<Expression> },
}

impl Expression {
	pub fn as_symbol(&self) -> Option<&GString> {
		match self {
			Self::Atom(Atom::Symbol(symbol)) => Some(symbol),
			_ => None,
		}
	}

	pub fn is_nil(&self) -> bool {
		matches!(self, Self::Atom(Atom::Nil))
	}

	pub fn as_list(&self) -> Option<impl Iterator<Item = &Expression>> {
		match self {
			Self::Expression { head, tail } => {
				let tail: Vec<_> = tail.as_list()?.collect();
				Some(Some(head as _).into_iter().chain(tail))
			}
			Self::Atom(Atom::Nil) => Some(None.into_iter().chain(vec![])),
			Self::Atom(_) => None,
		}
	}

	pub fn is_list(&self) -> bool {
		match self {
			Self::Atom(Atom::Nil) => true,
			Self::Expression { tail, .. } => tail.is_list(),
			_ => false,
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
			Expression::Atom(atom) => write!(f, "{atom}"),
			Expression::Expression { head, tail } => {
				write!(f, "(")?;
				write!(f, "{head}")?;
				let mut tail: &Self = tail;
				loop {
					match tail {
						Self::Atom(Atom::Nil) => break,
						Self::Atom(atom) => {
							write!(f, ". {atom}")?;
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
pub enum Atom {
	Nil,
	Boolean(bool),
	Number(f64),
	Symbol(GString),
}

impl fmt::Debug for Atom {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self}")
	}
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

#[derive(Clone)]
pub enum Datum {
	Void,
	// Err,
	Atom(Atom),
	List { head: Box<Datum>, tail: Box<Datum> },
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

	pub fn try_into_list(self) -> Result<impl Iterator<Item = Datum>, Datum> {
		if !self.is_list() {
			return Err(self);
		}
		match self {
			Self::List { head, tail } => {
				let tail: Vec<_> = tail.try_into_list().unwrap().collect();
				Ok(Some(*head).into_iter().chain(tail))
			}
			Self::Atom(Atom::Nil) => Ok(None.into_iter().chain(vec![])),
			_ => unreachable!(),
		}
	}

	/// Returns `true` if the datum is [`List`].
	///
	/// [`List`]: Datum::List
	#[must_use]
	pub fn is_list(&self) -> bool {
		match self {
			Self::Atom(Atom::Nil) => true,
			Self::List { tail, .. } => tail.is_list(),
			_ => false,
		}
	}
}

impl fmt::Debug for Datum {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self}")
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
			Self::List { head, tail } => {
				write!(f, "(")?;
				write!(f, "{head}")?;
				let mut tail: &Self = tail;
				loop {
					match tail {
						Self::Void => {
							write!(f, " . <void>")?;
							break;
						}
						Self::Atom(Atom::Nil) => break,
						| Self::Atom(_) | Self::Expression { .. } | Self::Builtin(_) => {
							write!(f, " . {tail}")?;
							break;
						}
						Self::List { head, tail: next } => {
							write!(f, " {head}")?;
							tail = next;
						}
					}
				}
				write!(f, ")")?;
				Ok(())
			}
		}
	}
}
