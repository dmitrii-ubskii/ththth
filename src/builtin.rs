use crate::{
	data::{Atom, Expression},
	eval,
	string::GString,
	Datum, Env, EvaluationError,
};

pub const APPLY: GString = GString::from_bytes(b"apply");
pub const DEFINE: GString = GString::from_bytes(b"define");
pub const CONS: GString = GString::from_bytes(b"cons");

const PLUS: GString = GString::from_bytes(b"+");
const MINUS: GString = GString::from_bytes(b"-");
const MULTIPLY: GString = GString::from_bytes(b"*");
const DIVIDE: GString = GString::from_bytes(b"/");

pub fn init_builtins() -> Env<'static> {
	let mut env = Env::new();
	env.insert(PLUS, Datum::Builtin(plus));
	env.insert(MINUS, Datum::Builtin(minus));
	env.insert(MULTIPLY, Datum::Builtin(multiply));
	env.insert(DIVIDE, Datum::Builtin(divide));
	env
}

fn plus(args: &Expression, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	let mut env = Env::local(env);
	match args {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Number(0.0))),
		Expression::Atom(_) => todo!(),
		Expression::Expression { head, tail } => {
			let left = eval(&mut env, head)?;
			match left.as_number() {
				Some(left) => {
					let right = plus(tail, &env)?;
					let Some(right) = right.as_number() else { unreachable!() };
					Ok(Datum::Atom(Atom::Number(left + right)))
				}
				None => todo!(),
			}
		}
	}
}

fn minus(args: &Expression, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	let mut env = Env::local(env);
	match args {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Number(0.0))),
		Expression::Atom(_) => todo!(),
		Expression::Expression { head, tail } => {
			let left = eval(&mut env, head)?;
			match left.as_number() {
				Some(left) => {
					let right = plus(tail, &env)?;
					let Some(right) = right.as_number() else { unreachable!() };
					Ok(Datum::Atom(Atom::Number(left - right)))
				}
				None => todo!(),
			}
		}
	}
}

fn multiply(args: &Expression, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	let mut env = Env::local(env);
	match args {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Number(1.0))),
		Expression::Atom(_) => todo!(),
		Expression::Expression { head, tail } => {
			let left = eval(&mut env, head)?;
			match left.as_number() {
				Some(left) => {
					let right = multiply(tail, &env)?;
					let Some(right) = right.as_number() else { unreachable!() };
					Ok(Datum::Atom(Atom::Number(left * right)))
				}
				None => todo!(),
			}
		}
	}
}

fn divide(args: &Expression, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	let mut env = Env::local(env);
	match args {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Number(1.0))),
		Expression::Atom(_) => todo!(),
		Expression::Expression { head, tail } => {
			let left = eval(&mut env, head)?;
			match left.as_number() {
				Some(left) => {
					let right = multiply(tail, &env)?;
					let Some(right) = right.as_number() else { unreachable!() };
					Ok(Datum::Atom(Atom::Number(left / right)))
				}
				None => todo!(),
			}
		}
	}
}
