use crate::{
	data::{Atom, Expression},
	eval,
	string::GString,
	Datum, Env, EvaluationError,
};

pub const DEFINE: GString = GString::from_bytes(b"define");
const PLUS: GString = GString::from_bytes(b"+");
const MINUS: GString = GString::from_bytes(b"-");

pub fn init_builtins() -> Env<'static> {
	let mut env = Env::new();
	env.insert(PLUS, Datum::Builtin(plus));
	env.insert(MINUS, Datum::Builtin(minus));
	env
}

fn plus(args: &Expression, env: &Env<'_>) -> Result<Datum, EvaluationError> {
	let mut env = Env::local(env);
	match args {
		Expression::Atom(Atom::Nil) => Ok(Datum::Atom(Atom::Number(0.0))),
		Expression::Atom(_) => todo!(),
		Expression::Expression { left, right } => {
			let left = eval(left, &mut env)?;
			match left.as_number() {
				Some(left) => {
					let right = plus(right, &env)?;
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
		Expression::Expression { left, right } => {
			let left = eval(left, &mut env)?;
			match left.as_number() {
				Some(left) => {
					let right = plus(right, &env)?;
					let Some(right) = right.as_number() else { unreachable!() };
					Ok(Datum::Atom(Atom::Number(left - right)))
				}
				None => todo!(),
			}
		}
	}
}
