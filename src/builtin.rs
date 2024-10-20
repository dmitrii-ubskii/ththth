use crate::{string::GString, Env};

pub const DEFINE: GString = GString::from_bytes(b"define");

pub fn init_builtins() -> Env<'static> {
	Env::new()
}
