#![deny(elided_lifetimes_in_paths)]
#![deny(clippy::semicolon_if_nothing_returned)]

use std::{
	collections::HashMap,
	env,
	error::Error,
	fmt, fs,
	io::{stdin, Read},
};

use builtin::DEFINE;
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
		println!("{}", eval(expr, &mut global_env)?);
	}
	Ok(())
}

fn eval(expr: &Expression, env: &mut Env<'_>) -> Result<Datum, EvaluationError> {
	match expr {
		Expression::Atom(atom) => eval_atom(atom, env),
		Expression::Expression { left, right } => match &**left {
			Expression::Atom(Atom::Symbol(DEFINE)) => {
				define(right, env)?;
				Ok(Datum::Void)
			}
			Expression::Atom(atom) => {
				let left = eval_atom(atom, env)?;
				match left {
					Datum::Void => todo!(),
					Datum::Atom(_) => todo!(),
					Datum::Expression { formals, expression } => {
						let mut local_env = Env::local(env);
						populate_formals(formals, right, &mut local_env)?;
						eval(&expression, &mut local_env)
					}
					Datum::Builtin(builtin) => builtin(right, env),
				}
			}
			Expression::Expression { .. } => todo!(),
		},
	}
}

fn populate_formals(
	formals: Vec<GString>,
	right: &Expression,
	local_env: &mut Env<'_>,
) -> Result<(), EvaluationError> {
	let Some(mut args) = right.as_list() else { return Err(EvaluationError::BadSyntax) };
	for formal in formals {
		let Some(arg) = args.next() else { return Err(EvaluationError::NotEnoughArgs { formal }) };
		let arg = eval(arg, local_env);
		local_env.insert(formal, arg?);
	}
	Ok(())
}

fn eval_atom(atom: &Atom, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	match atom {
		Atom::Symbol(symbol) => match env.get(symbol) {
			Some(datum) => Ok(datum.clone()),
			None => Err(EvaluationError::UnboundSymbol { symbol: symbol.clone() }),
		},
		atom => Ok(Datum::Atom(atom.clone())),
	}
}

fn define(args: &Expression, env: &mut Env<'_>) -> Result<(), EvaluationError> {
	let Expression::Expression { left: defined, right: value } = args else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	let Expression::Expression { left: value, right: nil } = &**value else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	match &**nil {
		Expression::Atom(Atom::Nil) => (),
		_ => return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() }),
	}
	match &**defined {
		Expression::Atom(Atom::Symbol(symbol)) => {
			let value = eval(value, env)?;
			env.insert(symbol.clone(), value);
			Ok(())
		}
		Expression::Atom(_) => Err(EvaluationError::DefineLeft { nonsymbol: (**defined).clone() }),
		Expression::Expression { left, right: formals } => {
			let Some(symbol) = left.as_symbol() else {
				return Err(EvaluationError::DefineLeft { nonsymbol: (**left).clone() });
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

#[derive(Debug)]
enum EvaluationError {
	BadSyntax,
	UnboundSymbol { symbol: GString },
	NotEnoughArgs { formal: GString },
	DefineWrongNumberOfArgs { args: Expression },
	DefineLeft { nonsymbol: Expression },
}

impl fmt::Display for EvaluationError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::BadSyntax => write!(f, "bad syntax"),
			Self::UnboundSymbol { symbol } => write!(f, "unbound symbol {symbol}"),
			Self::NotEnoughArgs { formal } => {
				write!(f, "no value to substitute for argument `{formal}`")
			}
			Self::DefineWrongNumberOfArgs { args } => {
				write!(f, "wrong number of arguments to `define`: {args:?}")
			}
			Self::DefineLeft { nonsymbol } => {
				write!(f, "expected symbol in define, found: {nonsymbol:?}")
			}
		}
	}
}

impl Error for EvaluationError {}
