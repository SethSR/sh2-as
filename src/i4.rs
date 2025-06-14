
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct I4(i8);

impl TryFrom<i8> for I4 {
	type Error = &'static str;
	fn try_from(i: i8) -> Result<Self, Self::Error> {
		if !(-8..8).contains(&i) {
			Err("expected value between -8 and 7")
		} else {
			Ok(Self(i))
		}
	}
}

impl std::ops::BitOr<I4> for u8 {
	type Output = u8;
	fn bitor(self, rhs: I4) -> Self::Output {
		self | (rhs.0 & 0xF) as u8
	}
}

impl std::ops::BitOr<I4> for u16 {
	type Output = u16;
	fn bitor(self, rhs: I4) -> Self::Output {
		self | (rhs.0 & 0xF) as u16
	}
}

impl std::fmt::Display for I4 {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

