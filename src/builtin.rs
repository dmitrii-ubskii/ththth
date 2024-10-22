use crate::{apply, data::Datum, string::GString, Env};

pub const DEFINE: GString = GString::from_bytes(b"define");
pub const LAMBDA: GString = GString::from_bytes(b"lambda");

const APPLY: GString = GString::from_bytes(b"apply");

const CONS: GString = GString::from_bytes(b"cons");
const CAR: GString = GString::from_bytes(b"car");
const CDR: GString = GString::from_bytes(b"cdr");

const PLUS: GString = GString::from_bytes(b"+");
const MINUS: GString = GString::from_bytes(b"-");
const MULTIPLY: GString = GString::from_bytes(b"*");
const DIVIDE: GString = GString::from_bytes(b"/");

pub fn init_builtins() -> Env<'static> {
	let mut env = Env::new();
	env.insert(APPLY, Datum::Builtin(apply_f));

	env.insert(CONS, Datum::Builtin(cons));
	env.insert(CAR, Datum::Builtin(car));
	env.insert(CDR, Datum::Builtin(cdr));

	env.insert(PLUS, Datum::Builtin(plus));
	env.insert(MINUS, Datum::Builtin(minus));
	env.insert(MULTIPLY, Datum::Builtin(multiply));
	env.insert(DIVIDE, Datum::Builtin(divide));
	env
}

fn apply_f(env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: f, tail } = args else { return Datum::Err };
	let Datum::List { head: args, tail: nil } = *tail else { return Datum::Err };
	let Datum::Nil = *nil else { return Datum::Err };
	apply(env, *f, *args)
}

pub fn cons(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: fst, tail } = args else { return Datum::Err };
	let Datum::List { head: snd, tail: nil } = *tail else { return Datum::Err };
	let Datum::Nil = *nil else { return Datum::Err };
	Datum::List { head: fst, tail: snd }
}

pub fn car(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head, tail: _ } = args else { return Datum::Err };
	*head
}

pub fn cdr(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: _, tail } = args else { return Datum::Err };
	*tail
}

fn plus(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(0.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::Err };
			let Datum::Number(rhs) = plus(_env, *tail) else { return Datum::Err };
			Datum::Number(lhs + rhs)
		}
		_ => Datum::Err,
	}
}

fn minus(env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(0.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::Err };
			let Datum::Number(rhs) = plus(env, *tail) else { return Datum::Err };
			Datum::Number(lhs - rhs)
		}
		_ => Datum::Err,
	}
}

fn multiply(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(1.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::Err };
			let Datum::Number(rhs) = multiply(_env, *tail) else { return Datum::Err };
			Datum::Number(lhs * rhs)
		}
		_ => Datum::Err,
	}
}

fn divide(env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(1.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::Err };
			let Datum::Number(rhs) = multiply(env, *tail) else { return Datum::Err };
			Datum::Number(lhs / rhs)
		}
		_ => Datum::Err,
	}
}
