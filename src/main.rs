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
			Expression::Atom(_) => todo!(),
			Expression::Expression { .. } => todo!(),
		},
	}
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
	let Expression::Expression { left: symbol, right: value } = args else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	let Expression::Expression { left: value, right: nil } = &**value else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() });
	};
	match &**nil {
		Expression::Atom(Atom::Nil) => (),
		_ => return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.clone() }),
	}
	match &**symbol {
		Expression::Atom(Atom::Symbol(symbol)) => {
			let value = eval(value, env)?;
			env.insert(symbol.clone(), value);
			Ok(())
		}
		Expression::Atom(_) => todo!(),
		Expression::Expression { .. } => todo!(),
	}
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
	UnboundSymbol { symbol: GString },
	DefineWrongNumberOfArgs { args: Expression },
}

impl fmt::Display for EvaluationError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnboundSymbol { symbol } => write!(f, "unbound symbol {symbol}"),
			Self::DefineWrongNumberOfArgs { args } => {
				write!(f, "wrong number of arguments to `define`: {args:?}")
			}
		}
	}
}

impl Error for EvaluationError {}
