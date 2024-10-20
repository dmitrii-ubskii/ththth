#![deny(elided_lifetimes_in_paths)]
#![deny(clippy::semicolon_if_nothing_returned)]

use std::{
	collections::HashMap,
	env,
	error::Error,
	fmt, fs,
	io::{stdin, Read},
};

use builtin::{APPLY, CONS, DEFINE};
use data::{Atom, Datum, Expression};
use string::GString;

mod builtin;
mod data;
mod parse;
mod string;

fn main() -> Result<(), Box<dyn Error>> {
	let input = match env::args().nth(1) {
		Some(path) => fs::read_to_string(path)?,
		None => {
			let mut buf = String::new();
			stdin().read_to_string(&mut buf)?;
			buf
		}
	};

	let mut global_env = builtin::init_builtins();

	let program = parse::parse(input.trim())?;
	for expr in &program {
		let res = eval(&mut global_env, expr)?;
		if !matches!(res, Datum::Void) {
			println!("{}", res);
		}
	}
	Ok(())
}

fn eval(env: &mut Env<'_>, expr: &Expression) -> Result<Datum, EvaluationError> {
	match expr {
		Expression::Atom(atom) => eval_atom(env, atom),
		Expression::Expression { head, tail } => match &**head {
			Expression::Atom(Atom::Symbol(APPLY)) => {
				let Expression::Expression { head, tail } = &**tail else {
					return Err(EvaluationError::Application {});
				};
				let Datum::Expression { formals, expression } = eval(env, head)? else {
					return Err(EvaluationError::Application {});
				};
				let tail = eval(env, tail)?;
				apply(env, formals, expression, tail)
			}
			Expression::Atom(Atom::Symbol(DEFINE)) => {
				define(env, tail)?;
				Ok(Datum::Void)
			}
			Expression::Atom(Atom::Symbol(CONS)) => {
				let Expression::Expression { head, tail } = &**tail else {
					return Err(EvaluationError::BadSyntax);
				};
				let head = eval(env, head)?;
				let tail = eval(env, tail)?;
				Ok(Datum::List { head: Box::new(head), tail: Box::new(tail) })
			}
			Expression::Atom(atom) => {
				let left = eval_atom(env, atom)?;
				match left {
					Datum::Void => todo!(),
					atom @ Datum::Atom(_) if tail.is_nil() => Ok(atom),
					Datum::Atom(_) => todo!("{left}"),
					Datum::List { .. } => todo!(),
					Datum::Expression { formals, expression } => {
						let args = eval_list(env, tail)?;
						apply(env, formals, expression, args)
					}
					Datum::Builtin(builtin) => builtin(tail, env),
				}
			}
			Expression::Expression { .. } => {
				let head = eval(env, head)?;
				if tail.is_nil() {
					Ok(head)
				} else {
					todo!()
				}
			}
		},
	}
}

fn eval_list(env: &mut Env<'_>, list: &Expression) -> Result<Datum, EvaluationError> {
	assert!(list.is_list());
	match list {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Nil)),
		Expression::Expression { head, tail } => Ok(Datum::List {
			head: Box::new(eval(env, head)?),
			tail: Box::new(eval_list(env, tail)?),
		}),
		Expression::Atom(_) => unreachable!(),
	}
}

fn apply(
	env: &mut Env<'_>,
	formals: Vec<GString>,
	expression: Expression,
	args: Datum,
) -> Result<Datum, EvaluationError> {
	let mut local_env = Env::local(env);
	populate_formals(&mut local_env, formals, args)?;
	eval(&mut local_env, &expression)
}

fn populate_formals(
	local_env: &mut Env<'_>,
	formals: Vec<GString>,
	right: Datum,
) -> Result<(), EvaluationError> {
	let mut args = match right.try_into_list() {
		Ok(list) => list,
		Err(_) => return Err(EvaluationError::BadSyntax),
	};
	for formal in formals {
		let Some(arg) = args.next() else { return Err(EvaluationError::NotEnoughArgs { formal }) };
		local_env.insert(formal, arg);
	}
	Ok(())
}

fn eval_atom(env: &Env<'_>, atom: &Atom) -> Result<Datum, EvaluationError> {
	match atom {
		Atom::Symbol(symbol) => match env.get(symbol) {
			Some(datum) => Ok(datum.clone()),
			None => Err(EvaluationError::UnboundSymbol { symbol: symbol.clone() }),
		},
		atom => Ok(Datum::Atom(atom.clone())),
	}
}

fn define(env: &mut Env<'_>, args: &Expression) -> Result<(), EvaluationError> {
	let Expression::Expression { head: defined, tail: value } = args else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	let Expression::Expression { head: value, tail: nil } = &**value else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	match &**nil {
		Expression::Atom(Atom::Nil) => (),
		_ => return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() }),
	}
	match &**defined {
		Expression::Atom(Atom::Symbol(symbol)) => {
			let value = eval(env, value)?;
			env.insert(symbol.clone(), value);
			Ok(())
		}
		Expression::Atom(_) => Err(EvaluationError::DefineLeft { nonsymbol: (**defined).clone() }),
		Expression::Expression { head, tail: formals } => {
			let Some(symbol) = head.as_symbol() else {
				return Err(EvaluationError::DefineLeft { nonsymbol: (**head).clone() });
			};
			let formals = get_def_formals(formals)?;
			env.insert(
				symbol.clone(),
				Datum::Expression { formals, expression: (**value).clone() },
			);
			Ok(())
		}
	}
}

fn get_def_formals(formals: &Expression) -> Result<Vec<GString>, EvaluationError> {
	let Some(formal_list) = formals.as_list() else { return Err(EvaluationError::BadSyntax) };
	let formals = formal_list
		.map(|expr| match expr.as_symbol() {
			Some(formal) => Ok::<_, EvaluationError>(formal.clone()),
			None => todo!(),
		})
		.collect::<Result<_, _>>()?;
	Ok(formals)
}

#[derive(Default)]
struct Env<'a> {
	local: HashMap<GString, Datum>,
	outer: Option<&'a Env<'a>>,
}

impl Env<'static> {
	fn new() -> Self {
		Self::default()
	}
}

impl<'a> Env<'a> {
	fn local(outer: &'a Env<'_>) -> Self {
		Self { local: HashMap::default(), outer: Some(outer) }
	}
}

impl Env<'_> {
	fn insert(&mut self, key: GString, value: Datum) {
		self.local.insert(key, value);
	}

	fn get(&self, symbol: &GString) -> Option<&Datum> {
		self.local.get(symbol).or_else(|| self.outer.and_then(|outer| outer.get(symbol)))
	}
}

enum EvaluationError {
	BadSyntax,
	Application {},
	UnboundSymbol { symbol: GString },
	NotEnoughArgs { formal: GString },
	DefineWrongNumberOfArgs { args: Expression },
	DefineLeft { nonsymbol: Expression },
}

impl fmt::Debug for EvaluationError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self}")
	}
}

impl fmt::Display for EvaluationError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::BadSyntax => write!(f, "bad syntax"),
			Self::Application { .. } => todo!(),
			Self::UnboundSymbol { symbol } => write!(f, "unbound symbol {symbol}"),
			Self::NotEnoughArgs { formal } => {
				write!(f, "no value to substitute for argument `{formal}`")
			}
			Self::DefineWrongNumberOfArgs { args } => {
				write!(f, "wrong number of arguments to `define`: {args}")
			}
			Self::DefineLeft { nonsymbol } => {
				write!(f, "expected symbol in define, found: {nonsymbol}")
			}
		}
	}
}

impl Error for EvaluationError {}
