use crate::{apply, data::Datum, string::GString, Env};

pub const DEFINE: GString = GString::from_bytes(b"define");
pub const LAMBDA: GString = GString::from_bytes(b"lambda");
pub const IF: GString = GString::from_bytes(b"if");
pub const COND: GString = GString::from_bytes(b"cond");
pub const ELSE: GString = GString::from_bytes(b"else");
pub const ERR: GString = GString::from_bytes(b"ERR");

const APPLY: GString = GString::from_bytes(b"apply");
const DISPLAY: GString = GString::from_bytes(b"display");
const NEWLINE: GString = GString::from_bytes(b"newline");

const IS_NIL: GString = GString::from_bytes(b"nil?");
const IS_EQ: GString = GString::from_bytes(b"eq?");

const EQ: GString = GString::from_bytes(b"=");
const LT: GString = GString::from_bytes(b"<");
const LE: GString = GString::from_bytes(b"<=");
const GT: GString = GString::from_bytes(b">");
const GE: GString = GString::from_bytes(b">=");

const CONS: GString = GString::from_bytes(b"cons");
const CAR: GString = GString::from_bytes(b"car");
const CDR: GString = GString::from_bytes(b"cdr");

const PLUS: GString = GString::from_bytes(b"+");
const MINUS: GString = GString::from_bytes(b"-");
const MULTIPLY: GString = GString::from_bytes(b"*");
const DIVIDE: GString = GString::from_bytes(b"/");
const REMAINDER: GString = GString::from_bytes(b"remainder");

const TRUNC: GString = GString::from_bytes(b"trunc");

pub fn init_builtins() -> Env<'static> {
	let mut env = Env::new();

	env.insert(APPLY, Datum::Builtin(apply_f));
	env.insert(DISPLAY, Datum::Builtin(display));
	env.insert(NEWLINE, Datum::Builtin(newline));

	env.insert(CONS, Datum::Builtin(cons));
	env.insert(CAR, Datum::Builtin(car));
	env.insert(CDR, Datum::Builtin(cdr));

	env.insert(IS_NIL, Datum::Builtin(is_nil));
	env.insert(IS_EQ, Datum::Builtin(is_eq));

	env.insert(EQ, Datum::Builtin(eq));
	env.insert(LT, Datum::Builtin(lt));
	env.insert(LE, Datum::Builtin(le));
	env.insert(GT, Datum::Builtin(gt));
	env.insert(GE, Datum::Builtin(ge));

	env.insert(PLUS, Datum::Builtin(plus));
	env.insert(MINUS, Datum::Builtin(minus));
	env.insert(MULTIPLY, Datum::Builtin(multiply));
	env.insert(DIVIDE, Datum::Builtin(divide));

	env.insert(REMAINDER, Datum::Builtin(remainder));

	env.insert(TRUNC, Datum::Builtin(trunc));

	env
}

fn apply_f(env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: f, tail } = args else { return Datum::err() };
	let Datum::List { head: args, tail: nil } = *tail else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	apply(env, *f, *args)
}

fn display(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: arg, tail: nil } = args else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	print!("{arg} ");
	Datum::Void
}

fn newline(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::Nil = args else { return Datum::err() };
	println!();
	Datum::Void
}

pub fn cons(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: fst, tail } = args else { return Datum::err() };
	let Datum::List { head: snd, tail: nil } = *tail else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	Datum::List { head: fst, tail: snd }
}

pub fn car(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: list, tail: nil } = args else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	let Datum::List { head, tail: _ } = *list else { return Datum::err() };
	*head
}

pub fn cdr(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: list, tail: nil } = args else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	let Datum::List { head: _, tail } = *list else { return Datum::err() };
	*tail
}

pub fn is_nil(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: arg, tail: nil } = args else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	match *arg {
		Datum::Nil => Datum::Boolean(true),
		_ => Datum::Boolean(false),
	}
}

pub fn is_eq(_env: &mut Env<'_>, args: Datum) -> Datum {
	let Datum::List { head: lhs, tail } = args else { return Datum::err() };
	let Datum::List { head: rhs, tail: nil } = *tail else { return Datum::err() };
	let Datum::Nil = *nil else { return Datum::err() };
	Datum::Boolean(lhs == rhs)
}

macro_rules! cmp {
	($cmp:ident $op:tt) => {
		fn $cmp(_env: &mut Env<'_>, args: Datum) -> Datum {
			match args {
				Datum::Nil => Datum::Boolean(true),
				Datum::List { head, tail } => {
					let Datum::Number(lhs) = *head else { return Datum::err() };
					let Datum::Boolean(tail_eq) = $cmp(_env, *tail.clone()) else {
						return Datum::err();
					};
					let Datum::List { head: rhs, tail: _ } = *tail else {
						return Datum::Boolean(true);
					};
					let Datum::Number(rhs) = *rhs else { return Datum::err() };
					Datum::Boolean(lhs $op rhs && tail_eq)
				}
				_ => Datum::err(),
			}
		}
	};
}

cmp! { eq == }
cmp! { lt < }
cmp! { le <= }
cmp! { gt > }
cmp! { ge >= }

fn plus(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(0.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::err() };
			let Datum::Number(rhs) = plus(_env, *tail) else { return Datum::err() };
			Datum::Number(lhs + rhs)
		}
		_ => Datum::err(),
	}
}

fn minus(env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(0.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::err() };
			if let Datum::Nil = *tail {
				Datum::Number(-lhs)
			} else {
				let Datum::Number(rhs) = plus(env, *tail) else { return Datum::err() };
				Datum::Number(lhs - rhs)
			}
		}
		_ => Datum::err(),
	}
}

fn multiply(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(1.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::err() };
			let Datum::Number(rhs) = multiply(_env, *tail) else { return Datum::err() };
			Datum::Number(lhs * rhs)
		}
		_ => Datum::err(),
	}
}

fn divide(env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::Nil => Datum::Number(1.0),
		Datum::List { head, tail } => {
			let Datum::Number(lhs) = *head else { return Datum::err() };
			let Datum::Number(rhs) = multiply(env, *tail) else { return Datum::err() };
			Datum::Number(lhs / rhs)
		}
		_ => Datum::err(),
	}
}

fn remainder(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::List { head: lhs, tail } => {
			let Datum::Number(lhs) = *lhs else { return Datum::err() };
			let Datum::List { head: rhs, tail: nil } = *tail else { return Datum::err() };
			let Datum::Nil = *nil else { return Datum::err() };
			let Datum::Number(rhs) = *rhs else { return Datum::err() };
			Datum::Number(lhs % rhs)
		}
		_ => Datum::err(),
	}
}

fn trunc(_env: &mut Env<'_>, args: Datum) -> Datum {
	match args {
		Datum::List { head, tail } => {
			let Datum::Nil = *tail else { return Datum::err() };
			let Datum::Number(lhs) = *head else { return Datum::err() };
			Datum::Number(lhs.trunc())
		}
		_ => Datum::err(),
	}
}
