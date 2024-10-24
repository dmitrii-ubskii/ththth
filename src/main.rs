#![deny(elided_lifetimes_in_paths)]
#![deny(clippy::semicolon_if_nothing_returned)]

use std::{
	collections::HashMap,
	env, fmt, fs,
	io::{stdin, Read},
};

use builtin::{AND, COND, DEFINE, ELSE, IF, LAMBDA, LET, OR, WHEN};
use data::{Datum, Expression};
use string::GString;

mod builtin;
mod data;
mod parse;
mod string;

const STD: &str = include_str!("std.th");

fn main() {
	let input = match env::args().nth(1) {
		Some(path) => fs::read_to_string(path).unwrap(),
		None => {
			let mut buf = String::new();
			stdin().read_to_string(&mut buf).unwrap();
			buf
		}
	};

	let mut global_env = builtin::init_builtins();

	for expr in parse::parse(STD.trim()) {
		eval(&mut global_env, &expr);
	}

	let program = parse::parse(input.trim());
	for expr in &program {
		let res = eval(&mut global_env, expr);
		if !matches!(res, Datum::Void) {
			println!("{}", res);
		}
	}
}

fn eval(env: &mut Env<'_>, expr: &Expression) -> Datum {
	match expr {
		Expression::Datum(atom) => eval_atom(env, atom),
		Expression::Expression { head, tail: args } => match &**head {
			Expression::Datum(Datum::Symbol(DEFINE)) => define(env, args),
			Expression::Datum(Datum::Symbol(LET)) => let_(env, args),
			Expression::Datum(Datum::Symbol(LAMBDA)) => lambda(env, args),
			Expression::Datum(Datum::Symbol(AND)) => and(env, args),
			Expression::Datum(Datum::Symbol(OR)) => or(env, args),
			Expression::Datum(Datum::Symbol(IF)) => if_then_else(env, args),
			Expression::Datum(Datum::Symbol(WHEN)) => when(env, args),
			Expression::Datum(Datum::Symbol(COND)) => cond(env, args),
			_ => {
				let head = eval(env, head);
				let args = eval_list(env, args);
				apply(env, head, args)
			}
		},
	}
}

fn eval_closure(env: &mut Env<'_>, body: &Expression) -> Datum {
	let Expression::Expression { head, tail } = body else { return Datum::err() };
	let res = eval(env, head);
	if let Datum::Void = res {
		if let Expression::Datum(Datum::Nil) = &**tail {
			Datum::Void
		} else {
			eval_closure(env, tail)
		}
	} else {
		let Expression::Datum(Datum::Nil) = &**tail else { return Datum::err() };
		res
	}
}

fn define(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: defined, tail: body } = args else { return Datum::err() };
	match &**defined {
		Expression::Datum(Datum::Symbol(symbol)) => {
			let Expression::Expression { head: value, tail: nil } = &**body else {
				return Datum::err();
			};
			let Expression::Datum(Datum::Nil) = &**nil else { return Datum::err() };
			let value = eval(env, value);
			env.insert(symbol.clone(), value);
			Datum::Void
		}
		Expression::Datum(_) => Datum::err(),
		Expression::Expression { head: symbol, tail: formals } => {
			let Some(symbol) = symbol.as_symbol() else { return Datum::err() };
			env.insert(
				symbol.clone(),
				Datum::Closure { formals: formals.clone(), body: body.clone(), env: env.flatten() },
			);
			Datum::Void
		}
	}
}

fn let_(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: bindings, tail: body } = args else { return Datum::err() };

	let mut env = Env::local(env);
	let env = &mut env;

	let mut bindings: &Expression = bindings;
	loop {
		match bindings {
			Expression::Datum(Datum::Nil) => break,
			Expression::Expression { head: binding, tail } => {
				define(env, binding);
				bindings = tail;
			}
			Expression::Datum(_) => return Datum::err(),
		}
	}

	eval_closure(env, body)
}

fn lambda(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: formals, tail: body } = args else { return Datum::err() };
	let env = env.flatten();
	Datum::Closure { formals: formals.clone(), body: body.clone(), env }
}

fn if_then_else(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: condition, tail: branches } = args else {
		return Datum::err();
	};
	let Expression::Expression { head: then_branch, tail } = &**branches else {
		return Datum::err();
	};
	let Expression::Expression { head: else_branch, tail: nil } = &**tail else {
		return Datum::err();
	};
	let Expression::Datum(Datum::Nil) = &**nil else { return Datum::err() };
	let Datum::Boolean(bool) = eval(env, condition) else { return Datum::err() };
	if bool {
		eval(env, then_branch)
	} else {
		eval(env, else_branch)
	}
}

fn and(env: &mut Env<'_>, args: &Expression) -> Datum {
	if let Expression::Datum(Datum::Nil) = args {
		return Datum::Boolean(true);
	}
	let Expression::Expression { head: first, tail: rest } = args else { return Datum::err() };
	match eval(env, first) {
		Datum::Boolean(true) => and(env, rest),
		Datum::Boolean(false) => Datum::Boolean(false),
		_ => Datum::err(),
	}
}

fn or(env: &mut Env<'_>, args: &Expression) -> Datum {
	if let Expression::Datum(Datum::Nil) = args {
		return Datum::Boolean(false);
	}
	let Expression::Expression { head: first, tail: rest } = args else { return Datum::err() };
	match eval(env, first) {
		Datum::Boolean(false) => or(env, rest),
		Datum::Boolean(true) => Datum::Boolean(true),
		_ => Datum::err(),
	}
}

fn when(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: condition, tail: body } = args else {
		return Datum::err();
	};
	let Datum::Boolean(bool) = eval(env, condition) else { return Datum::err() };
	if bool {
		eval_closure(env, body)
	} else {
		Datum::Void
	}
}

fn cond(env: &mut Env<'_>, args: &Expression) -> Datum {
	let Expression::Expression { head: branch, tail: rest } = args else { return Datum::err() };
	let Expression::Expression { head: condition, tail: body } = &**branch else {
		return Datum::err();
	};
	let Expression::Expression { head: body, tail: nil } = &**body else { return Datum::err() };
	let Expression::Datum(Datum::Nil) = &**nil else { return Datum::err() };
	match &**condition {
		Expression::Datum(Datum::Symbol(ELSE)) => eval(env, body),
		_ => {
			let Datum::Boolean(bool) = eval(env, condition) else { return Datum::err() };
			if bool {
				eval(env, body)
			} else {
				cond(env, rest)
			}
		}
	}
}

fn eval_atom(env: &Env<'_>, datum: &Datum) -> Datum {
	match datum {
		Datum::Symbol(symbol) => match env.get(symbol) {
			Some(datum) => datum.clone(),
			None => Datum::err(),
		},
		Datum::Quoted(quoted) => unquote(quoted),
		datum => datum.clone(),
	}
}

fn unquote(quoted: &Expression) -> Datum {
	match quoted {
		Expression::Datum(datum) => datum.clone(),
		Expression::Expression { head, tail } => {
			Datum::List { head: Box::new(unquote(head)), tail: Box::new(unquote(tail)) }
		}
	}
}

fn apply(env: &mut Env<'_>, head: Datum, args: Datum) -> Datum {
	match &head {
		Datum::Builtin(builtin) => builtin(env, args),
		Datum::Closure { formals, body, env: local } => {
			let env = Env { local: local.clone(), outer: Some(env) };
			closure(&env, formals, body, args)
		}
		_ => Datum::err(),
	}
}

fn eval_list(env: &mut Env<'_>, list: &Expression) -> Datum {
	if let Expression::Datum(Datum::Nil) = list {
		return Datum::Nil;
	}
	match list {
		Expression::Expression { head, tail } => {
			let head = eval(env, head);
			let tail = eval_list(env, tail);
			Datum::List { head: Box::new(head), tail: Box::new(tail) }
		}
		Expression::Datum(Datum::Quoted(expr)) => unquote(expr),
		_ => Datum::err(),
	}
}

fn closure(env: &Env<'_>, mut formals: &Expression, body: &Expression, mut args: Datum) -> Datum {
	let mut env = Env::local(env);
	loop {
		match formals {
			Expression::Datum(datum) => match datum {
				Datum::Nil => {
					let Datum::Nil = args else { return Datum::err() };
					break;
				}
				Datum::Symbol(symbol) => {
					let list = args;
					env.insert(symbol.clone(), list);
					break;
				}
				_ => return Datum::err(),
			},
			Expression::Expression { head, tail } => {
				let Expression::Datum(Datum::Symbol(binding)) = &**head else {
					return Datum::err();
				};
				let Datum::List { head: arg, tail: arg_tail } = args else {
					return Datum::err();
				};
				env.insert(binding.clone(), *arg);
				formals = tail;
				args = *arg_tail;
			}
		}
	}

	eval_closure(&mut env, body)
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
	fn flatten(&self) -> HashMap<GString, Datum> {
		if let Some(outer) = self.outer {
			let mut flat = outer.flatten();
			flat.extend(self.local.iter().map(|(k, v)| (k.clone(), v.clone())));
			flat
		} else {
			// dirty hack: don't clone global env
			Default::default()
		}
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

impl fmt::Debug for Env<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[derive(Copy, Clone)]
		struct FlatEnv<'a>(&'a Env<'a>);
		impl<'a> FlatEnv<'a> {
			fn iter(self) -> impl Iterator<Item = (&'a GString, &'a Datum)> {
				let inner = self.0;
				inner.local.iter().chain(
					inner
						.outer
						.map(|e| {
							Box::new(FlatEnv(e).iter())
								as Box<dyn Iterator<Item = (&GString, &Datum)>>
						})
						.into_iter()
						.flatten(),
				)
			}
		}

		impl fmt::Debug for FlatEnv<'_> {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				let mut map = f.debug_map();
				for (key, value) in self.iter() {
					map.entry(key, value);
				}
				map.finish()
			}
		}

		f.debug_struct("Env")
			.field("local", &self.local)
			.field("outer", &self.outer.map(FlatEnv))
			.finish()
	}
}
