
#[derive(Debug, Clone, Eq)]
pub struct Token {
	/// Token type
	pub tt: Type,

	/// Start index in the source
	pub idx: u32,
}

impl Token {
	pub fn new(tt: Type, idx: usize) -> Self {
		Self {
			tt,
			idx: idx as u32,
		}
	}
}

impl PartialEq for Token {
	fn eq(&self, rhs: &Self) -> bool {
		match (&self.tt, &rhs.tt) {
			(Type::Label(a), Type::Label(b)) |
			(Type::String(a), Type::String(b)) => a == b,
			(Type::Imm(a), Type::Imm(b)) => a == b,
			_ => self.tt == rhs.tt,
		}
	}
}

impl PartialEq<Type> for Token {
	fn eq(&self, rhs: &Type) -> bool {
		self.tt == *rhs
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	Eof,

	/* Literals */
	String(Box<str>),
	Label(Box<str>),
	Imm(Box<str>),

	/* Operators */
	Plus,    // '+'
	Dash,    // '-'
	Star,    // '*'
	Slash,   // '/'
	Percent, // '%'
	At,      // '@'
	OParen,  // '('
	CParen,  // ')'
	Dollar,  // '$'
	Colon,   // ':'
	Dot,     // '.'
	Comma,   // ','
	Eq,      // '='

	/* Sizes */
	Byte,
	Word,
	Long,

	/* Directives */
	Org,
	Include,
	BInclude,
	Align,
	Const,
	Space,
	LtOrg,

	/* Instructions */
	ClrMac,
	ClrT,
	Div0U,
	Nop,
	Rte,
	Rts,
	SetT,
	Sleep,

	Bf,
	BfS,
	Bra,
	BraF,
	Bsr,
	BsrF,
	Bt,
	BtS,
	Dt,
	Jmp,
	Jsr,
	MovA,
	MovT,
	RotCL,
	RotCR,
	RotL,
	RotR,
	ShAL,
	ShAR,
	ShLL,
	ShLL2,
	ShLL8,
	ShLL16,
	ShLR,
	ShLR2,
	ShLR8,
	ShLR16,
	Tas,
	TrapA,

	AddC,
	AddV,
	Div0S,
	Div1,
	ExtS,
	ExtU,
	Mac,
	Neg,
	NegC,
	Not,
	Sub,
	SubC,
	SubV,
	Swap,
	Xtrct,

	And,
	Or,
	Tst,
	Xor,

	CmpEq,
	CmpGe,
	CmpGt,
	CmpHi,
	CmpHs,
	CmpStr,
	CmpPl,
	CmpPz,

	Add,
	Mul,
	MulS,
	MulU,

	LdC,
	LdS,
	StC,
	StS,

	DMulS,
	DMulU,

	Mov,
}

