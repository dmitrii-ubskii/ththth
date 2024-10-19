use std::{
	collections::HashMap,
	env,
	error::Error,
	fmt, fs,
	io::{stdin, Read},
};

use self::string::GString;

mod parse;
mod string;

type Env = HashMap<GString, Value>;

fn main() -> Result<(), Box<dyn Error>> {
	let input = match env::args().nth(1) {
		Some(path) => fs::read_to_string(path)?,
		None => {
			let mut buf = String::new();
			stdin().read_to_string(&mut buf)?;
			buf
		}
	};

	let mut env = init_env();

	let program = parse::parse(input.trim())?;
	for expr in &program {
		println!("{}", eval(expr, &mut env)?);
	}
	Ok(())
}

const DEFINE: GString = GString::from_bytes(b"define");
const PLUS: GString = GString::from_bytes(b"+");
const MINUS: GString = GString::from_bytes(b"-");

fn init_env() -> Env {
	let mut env = Env::new();
	env.insert(PLUS, Value::Builtin(plus));
	env.insert(MINUS, Value::Builtin(minus));
	env
}

fn plus(args: Vec<Value>) -> Value {
	args.into_iter().fold(Value::Number(0.0), |acc, item| {
		let Value::Number(acc) = acc else { return Value::Err };
		let Value::Number(num) = item else { return Value::Err };
		Value::Number(acc + num)
	})
}

fn minus(args: Vec<Value>) -> Value {
	let mut args = args.into_iter();
	let Some(first) = args.next() else { return Value::Err };
	args.fold(first, |acc, item| {
		let Value::Number(acc) = acc else { return Value::Err };
		let Value::Number(num) = item else { return Value::Err };
		Value::Number(acc - num)
	})
}

#[derive(Clone, Debug)]
enum Expression {
	Number(f64),
	Symbol(GString),
	Application(Vec<Expression>),
}

#[derive(Clone, Debug)]
enum Value {
	Void,
	Err,
	Number(f64),
	Symbol(GString),
	List(Vec<Value>),
	Builtin(fn(Vec<Value>) -> Value),
}

impl fmt::Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Value::Void => Ok(()),
			Value::Err => write!(f, "ERR"),
			Value::Number(num) => write!(f, "{num}"),
			Value::Symbol(sym) => write!(f, "{sym}"),
			Value::Builtin(_) => write!(f, "#builtin"),
			Value::List(list) => {
				write!(f, "(")?;
				if let [first, rest @ ..] = &list[..] {
					write!(f, "{first}")?;
					for item in rest {
						write!(f, ", {item}")?;
					}
				}
				write!(f, ")")?;
				Ok(())
			}
		}
	}
}

fn eval(expr: &Expression, env: &mut Env) -> Result<Value, EvaluationError> {
	match expr {
		&Expression::Number(num) => Ok(Value::Number(num)),
		Expression::Symbol(symbol) => match env.get(symbol) {
			Some(value) => Ok(value.clone()),
			None => Err(EvaluationError::UnboundSymbol { symbol: symbol.clone() }),
		},
		Expression::Application(list) => {
			let [symbol, args @ ..] = &list[..] else {
				return Err(EvaluationError::Application { value: Value::Void });
			};
			if let Expression::Symbol(DEFINE) = symbol {
				define(args, env)
			} else {
				match eval(symbol, env)? {
					value @ (Value::Void | Value::Err | Value::Number(_) | Value::List(_)) => {
						Err(EvaluationError::Application { value })
					}
					Value::Symbol(_) => todo!(),
					Value::Builtin(builtin) => {
						let args =
							args.iter().map(|arg| eval(arg, env)).collect::<Result<Vec<_>, _>>()?;
						Ok(builtin(args))
					}
				}
			}
		}
	}
}

fn define(args: &[Expression], env: &mut Env) -> Result<Value, EvaluationError> {
	let [symbol, value] = args else {
		return Err(EvaluationError::DefineWrongNumberOfArgs { args: args.to_owned() });
	};
	let Expression::Symbol(symbol) = symbol else {
		return Err(EvaluationError::DefineLeft { nonsymbol: symbol.clone() });
	};
	let value = eval(value, env)?;
	env.insert(symbol.clone(), value);
	Ok(Value::Void)
}

#[derive(Debug)]
enum EvaluationError {
	UnboundSymbol { symbol: GString },
	Application { value: Value },
	DefineWrongNumberOfArgs { args: Vec<Expression> },
	DefineLeft { nonsymbol: Expression },
}

impl fmt::Display for EvaluationError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnboundSymbol { symbol } => write!(f, "unbound symbol {symbol}"),
			Self::Application { value } => write!(f, "expected procedure, found {value:?}"),
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
