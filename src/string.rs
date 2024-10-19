use std::{fmt, ops::Deref};

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum GString {
	Heap(Box<str>),
	Inline { len: u8, data: [u8; Self::INLINE_LEN] },
}

impl GString {
	const INLINE_LEN: usize = 22;

	pub const fn from_bytes(bytes: &[u8]) -> Self {
		assert!(bytes.len() <= Self::INLINE_LEN);
		let mut data = [0u8; Self::INLINE_LEN];
		let mut i = 0;
		while i < bytes.len() {
			data[i] = bytes[i];
			i += 1;
		}
		Self::Inline { len: bytes.len() as u8, data }
	}
}

impl Deref for GString {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		match self {
			GString::Heap(str) => str,
			&GString::Inline { len, ref data } => unsafe {
				std::str::from_utf8_unchecked(&data[..len as usize])
			},
		}
	}
}

impl fmt::Debug for GString {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}::", std::any::type_name_of_val(self))?;
		match self {
			GString::Heap(_) => write!(f, "Heap(")?,
			GString::Inline { .. } => write!(f, "Inline(")?,
		}
		write!(f, "{:?})", self.deref())?;
		Ok(())
	}
}

impl fmt::Display for GString {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.deref().fmt(f)
	}
}

impl From<String> for GString {
	fn from(str: String) -> Self {
		if str.len() <= Self::INLINE_LEN {
			let mut inline_buf = [0u8; Self::INLINE_LEN];
			inline_buf.copy_from_slice(str.as_bytes());
			Self::Inline { len: str.len() as u8, data: inline_buf }
		} else {
			Self::Heap(str.into())
		}
	}
}

impl From<&str> for GString {
	fn from(str: &str) -> Self {
		if str.len() <= Self::INLINE_LEN {
			let mut inline_buf = [0u8; Self::INLINE_LEN];
			inline_buf[..str.len()].copy_from_slice(str.as_bytes());
			Self::Inline { len: str.len() as u8, data: inline_buf }
		} else {
			Self::Heap(str.into())
		}
	}
}
