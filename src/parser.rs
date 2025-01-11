

use crate::lexer::{Token,TokenType};

pub(crate) type Reg = u8;

#[derive(Clone)]
pub(crate) enum Arg {
	DirReg(Reg),
	DispR0(Reg),
	DispReg(i8,Reg),
	DispPC(i8),
	DispGBR(i8),
	DispLabel(String,Reg),
	IndReg(Reg),
	Label(String),
	PostInc(Reg),
	PreDec(Reg),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self,
		fmt: &mut std::fmt::Formatter,
	) -> std::fmt::Result {
		match self {
			Arg::DirReg(reg) =>
				write!(fmt, "DirReg(R{reg})"),
			Arg::DispR0(reg) =>
				write!(fmt, "DispR0(R{reg})"),
			Arg::DispReg(disp,reg) =>
				write!(fmt, "DispReg({disp},R{reg})"),
			Arg::DispPC(disp) =>
				write!(fmt, "DispPC({disp})"),
			Arg::DispGBR(disp) =>
				write!(fmt, "DispGBR({disp})"),
			Arg::DispLabel(lbl,reg) =>
				write!(fmt, "DispLabel({lbl},R{reg})"),
			Arg::IndReg(reg) =>
				write!(fmt, "IndReg(R{reg})"),
			Arg::Label(lbl) =>
				write!(fmt, "Label({lbl})"),
			Arg::PostInc(reg) =>
				write!(fmt, "PostInc(R{reg})"),
			Arg::PreDec(reg) =>
				write!(fmt, "PreDec(R{reg})"),
		}
	}
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub(crate) enum Size {
	Byte,
	Word,
	Long,
}

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
//    | Instruction       | Bit Layout       | Operation          | Cycles  | T Bit  |
pub(crate) enum Ins {
	/// | #imm,Rn           | 0111nnnniiiiiiii | Rn+imm -> Rn       | 1       | -      |
	ADD_Imm(i8,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1100 | Rn+Rm -> Rn        | 1       | -      |
	ADD_Reg(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1110 | Rn+Rm+T -> Rn,     | 1       | Carry  |
	/// |                   |                  | Carry -> T         |         |        |
	ADDC(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1111 | Rn+Rm -> Rn,       | 1       | Over   |
	/// |                   |                  | Overflow -> T      |         |        |
	ADDV(Reg,Reg),
	/// | #imm,R0           | 11001001iiiiiiii | R0 & imm -> R0     | 1       | -      |
	AND_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1001 | Rn & Rm -> Rn      | 1       | -      |
	AND_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR)    | 11001101iiiiiiii | (R0+GBR) & imm     | 3       | -      |
	/// |                   |                  | -> (R0+GBR)        |         |        |
	AND_Byte(u8),
	/// | label             | 10011011dddddddd | if T = 0,          | 3/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 1, nop      |         |        |
	BF(String),
	/// | label             | 10001111dddddddd | if T = 0,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 1, nop      |         |        |
	BFS(String),
	/// | label             | 1010dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	BRA(String),
	/// | Rm                | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BRAF(Reg),
	/// | label             | 1011dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	BSR(String),
	/// | Rm                | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BSRF(Reg),
	/// | label             | 10001001dddddddd | if T = 1,          | 3/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	BT(String),
	/// | label             | 10001101dddddddd | if T = 1,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	BTS(String),
	/// |                   | 0000000000101000 | 0 -> MACH, MACL    | 1       | -      |
	CLRMAC,
	/// |                   | 0000000000001000 | 0 -> T             | 1       | 0      |
	CLRT,
	/// | #imm,R0           | 10001000iiiiiiii | if R0 = imm,       | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_EQ_Imm(i8),
	/// | Rm,Rn             | 0011nnnnmmmm0000 | if Rn = Rm,        | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_EQ_Reg(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0011 | if Rn >= Rm with   | 1       | Result |
	/// |                   |                  | signed data,       |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_GE(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0111 | if Rn > Rm with    | 1       | Result |
	/// |                   |                  | signed data,       |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_GT(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0110 | if Rn > Rm with    | 1       | Result |
	/// |                   |                  | unsigned data,     |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_HI(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0010 | if Rn >= Rm with   | 1       | Result |
	/// |                   |                  | unsigned data,     |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_HS(Reg,Reg),
	/// | Rn                | 0100nnnn00010101 | if Rn > 0,         | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_PL(Reg),
	/// | Rn                | 0100nnnn00010001 | if Rn > 0,         | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CMP_PZ(Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1100 | if Rn & Rm have    | 1       | Result |
	/// |                   |                  | an equivalent      |         |        |
	/// |                   |                  | byte, 1 -> T       |         |        |
	CMP_STR(Reg,Reg),
	/// | Rm,Rn             | 0010nnnnmmmm0111 | MSB of Rn -> Q,    | 1       | Result |
	/// |                   |                  | MSB of Rm -> M,    |         |        |
	/// |                   |                  | M ^ Q -> T         |         |        |
	DIV0S(Reg,Reg),
	/// |                   | 0000000000011001 | 0 -> M/Q/T         | 1       | 0      |
	DIV0U,
	/// | Rm,Rn             | 0011nnnnmmmm0100 | Single-step        | 1       | Result |
	/// |                   |                  | division (Rn/Rm)   |         |        |
	DIV1(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1101 | Signed operation   | 2 to 4  | -      |
	/// |                   |                  | of Rn x Rm ->      |         |        |
	/// |                   |                  | MACH, MACL         |         |        |
	DMULS(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0101 | Unsigned operation | 2 to 4  | -      |
	/// |                   |                  | of Rn x Rm -> MACH |         |        |
	/// |                   |                  | MACL               |         |        |
	DMULU(Reg,Reg),
	/// | Rn                | 0100nnnn00010000 | Rn - 1 -> Rn, when | 1       | Result |
	/// |                   |                  | Rn is 0, 1 -> T.   |         |        |
	/// |                   |                  | When Rn is         |         |        |
	/// |                   |                  | nonzero, 0 -> T    |         |        |
	DT(Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1110 | A byte in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1111 | A word in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	EXTS(Size,Reg,Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1100 | A byte in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1101 | A word in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	EXTU(Size,Reg,Reg),
	/// | @Rm               | 0100mmmm00101011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | Rm -> PC           |         |        |
	JMP(Reg),
	/// | @Rm               | 0100mmmm00001011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | Rm -> PC           |         |        |
	JSR(Reg),
	/// | Rm,GBR            | 0100mmmm00011110 | Rm -> GBR          | 1       | -      |
	LDC_GBR(Reg),
	/// | Rm,SR             | 0100mmmm00001110 | Rm -> SR           | 1       | LSB    |
	LDC_SR(Reg),
	/// | Rm,VBR            | 0100mmmm00101110 | Rm -> VBR          | 1       | -      |
	LDC_VBR(Reg),
	/// | @Rm+,GBR          | 0100mmmm00010111 | (Rm) -> GBR,       | 3       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDC_GBR_Inc(Reg),
	/// | @Rm+,SR           | 0100mmmm00000111 | (Rm) -> SR,        | 3       | LSB    |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDC_SR_Inc(Reg),
	/// | @Rm+,VBR          | 0100mmmm00100111 | (Rm) -> VBR,       | 3       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDC_VBR_Inc(Reg),
	/// | Rm,MACH           | 0100mmmm00001010 | Rm -> MACH         | 1       | -      |
	LDS_MACH(Reg),
	/// | Rm,MACL           | 0100mmmm00011010 | Rm -> MACL         | 1       | -      |
	LDS_MACL(Reg),
	/// | Rm,PR             | 0100mmmm00101010 | Rm -> PR           | 1       | -      |
	LDS_PR(Reg),
	/// | @Rm+,MACH         | 0100mmmm00000110 | (Rm) -> MACH,      | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDS_MACH_Inc(Reg),
	/// | @Rm+,MACL         | 0100mmmm00010110 | (Rm) -> MACL,      | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDS_MACL_Inc(Reg),
	/// | @Rm+,PR           | 0100mmmm00100110 | (Rm) -> PR,        | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LDS_PR_Inc(Reg),
	/// | @Rm+,@Rn+         | 0000nnnnmmmm1111 | Signed operation   | 3/(2-4) | -      |
	/// |                   |                  | of (Rn) x (Rm) +   |         |        |
	/// |                   |                  | MAC -> MAC         |         |        |
	MAC_Long(Reg,Reg),
	/// | @Rm+,@Rn+         | 0100nnnnmmmm1111 | Signed operation   | 3/(2)   | -      |
	/// |                   |                  | of (Rn) x (Rm) +   |         |        |
	/// |                   |                  | MAC -> MAC         |         |        |
	MAC_Word(Reg,Reg),
	/// | #imm,Rn           | 1110nnnniiiiiiii | imm -> Sign        | 1       | -      |
	/// |                   |                  | extension -> Rn    |         |        |
	MOV_Imm(i8,Reg),
	/// | Rm,Rn             | 0110nnnnmmmm0011 | Rm -> Rn           | 1       | -      |
	/// |                   |                  |                    |         |        |
	MOV_Reg(Reg,Reg),
	/// | .B @(disp,GBR),R0 | 11000100dddddddd | (disp+GBR) -> Sign | 1       | -      |
	/// |                   |                  | extension -> R0    |         |        |
	/// | .B @(disp,Rm),R0  | 10000100mmmmdddd | (disp+Rm) -> Sign  | 1       | -      |
	/// |                   |                  | extension -> R0    |         |        |
	/// | .B @(R0,Rm),Rn    | 0000nnnnmmmm1100 | (R0+Rm) -> Sign    | 1       | -      |
	/// |                   |                  | extension -> Rn    |         |        |
	/// | .B @Rm+,Rn        | 0110nnnnmmmm0100 | (Rm) -> Sign       | 1       | -      |
	/// |                   |                  | extension -> Rn,   |         |        |
	/// |                   |                  | Rm + 1 -> Rm       |         |        |
	/// | .B @Rm,Rn         | 0110nnnnmmmm0000 | (Rm) -> Sign       | 1       | -      |
	/// |                   |                  | extension -> Rn,   |         |        |
	MOV(Size,Arg,Arg),
	/// | @(disp,PC),R0     | 11000111dddddddd | disp x 4+PC -> R0  | 1       | -      |
	MOVA(i8),
	/// | Rn                | 0000nnnn00101001 | T -> Rn            | 1       | -      |
	MOVT(Reg),
	/// | Rm,Rn             | 0000nnnnmmmm0111 | Rn x Rm -> MACL    | 2 to 4  | -      |
	MUL(Reg,Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1111 | Signed operation   | 1 to 3  | -      |
	/// |                   |                  | of Rn x Rm -> MAC  |         |        |
	MULS(Reg,Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1110 | Unsigned operation | 1 to 3  | -      |
	/// |                   |                  | of Rn x Rm -> MAC  |         |        |
	MULU(Reg,Reg),
	/// | Rm,Rn             | 0110nnnnmmmm1011 | 0 - Rm -> Rn       | 1       | -      |
	NEG(Reg,Reg),
	/// | Rm,Rn             | 0110nnnnmmmm1010 | 0 - Rm - T -> Rn,  | 1       | Borrow |
	/// |                   |                  | Borrow -> T        |         |        |
	NEGC(Reg,Reg),
	/// |                   | 0000000000001001 | No operation       | 1       | -      |
	NOP,
	/// | Rm,Rn             | 0110nnnnmmmm0111 | ~Rm -> Rn          | 1       | -      |
	NOT(Reg,Reg),
	/// | #imm,R0           | 11001011iiiiiiii | R0|imm -> R0       | 1       | -      |
	OR_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1011 | Rn|Rm -> Rn        | 1       | -      |
	OR_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR)    | 11001111iiiiiiii | (R0+GBR)|imm ->    | 3       | -      |
	/// |                   |                  | (R0+GBR)           |         |        |
	OR_Byte(u8),
	/// | Rn                | 0100nnnn00100100 | T <- Rn <- T       | 1       | MSB    |
	ROTCL(Reg),
	/// | Rn                | 0100nnnn00100101 | T -> Rn -> T       | 1       | LSB    |
	ROTCR(Reg),
	/// | Rn                | 0100nnnn00000100 | T <- Rn <- MSB     | 1       | MSB    |
	ROTL(Reg),
	/// | Rn                | 0100nnnn00000101 | LSB -> Rn -> T     | 1       | LSB    |
	ROTR(Reg),
	/// |                   | 0000000000101011 | Delayed branch,    | 4       | LSB    |
	/// |                   |                  | stack area -> PC/SR |        |        |
	RTE,
	/// |                   | 0000000000001011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PR -> PC           |         |        |
	RTS,
	/// |                   | 0000000000011000 | 1 -> T             | 1       | 1      |
	SETT,
	/// | Rn                | 0100nnnn00100000 | T <- Rn <- 0       | 1       | MSB    |
	SHAL(Reg),
	/// | Rn                | 0100nnnn00100001 | MSB -> Rn -> T     | 1       | LSB    |
	SHAR(Reg),
	/// | Rn                | 0100nnnn00000000 | T <- Rn <- 0       | 1       | MSB    |
	SHLL(Reg),
	/// | Rn                | 0100nnnn00001000 | Rn << 2 -> Rn      | 1       | -      |
	SHLL2(Reg),
	/// | Rn                | 0100nnnn00011000 | Rn << 8 -> Rn      | 1       | -      |
	SHLL8(Reg),
	/// | Rn                | 0100nnnn00101000 | Rn << 16 -> Rn     | 1       | -      |
	SHLL16(Reg),
	/// | Rn                | 0100nnnn00000001 | 0 -> Rn -> T       | 1       | LSB    |
	SHLR(Reg),
	/// | Rn                | 0100nnnn00001001 | Rn >> 2 -> Rn      | 1       | -      |
	SHLR2(Reg),
	/// | Rn                | 0100nnnn00011001 | Rn >> 8 -> Rn      | 1       | -      |
	SHLR8(Reg),
	/// | Rn                | 0100nnnn00101001 | Rn >> 16 -> Rn     | 1       | -      |
	SHLR16(Reg),
	/// |                   | 0000000000011011 | Sleep              | 3       | -      |
	SLEEP,
	/// | GBR,Rn            | 0000nnnn00010010 | GBR -> Rn          | 1       | -      |
	STC_GBR(Reg),
	/// | SR,Rn             | 0000nnnn00000010 | SR -> Rn           | 1       | -      |
	STC_SR(Reg),
	/// | VBR,Rn            | 0000nnnn00100010 | VBR -> Rn          | 1       | -      |
	STC_VBR(Reg),
	/// | GBR,@-Rn          | 0100nnnn00010011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | GBR -> (Rn)        |         |        |
	STC_GBR_Dec(Reg),
	/// | SR,@-Rn           | 0100nnnn00000011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | SR -> (Rn)         |         |        |
	STC_SR_Dec(Reg),
	/// | VBR,@-Rn          | 0100nnnn00100011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | VBR -> (Rn)        |         |        |
	STC_VBR_Dec(Reg),
	/// | MACH,Rn           | 0000nnnn00001010 | MACH -> Rn         | 1       | -      |
	STS_MACH(Reg),
	/// | MACL,Rn           | 0000nnnn00011010 | MACL -> Rn         | 1       | -      |
	STS_MACL(Reg),
	/// | PR,Rn             | 0000nnnn00101010 | PR -> Rn           | 1       | -      |
	STS_PR(Reg),
	/// | MACH,@-Rn         | 0100nnnn00000010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | MACH -> (Rn)       |         |        |
	STS_MACH_Dec(Reg),
	/// | MACL,@-Rn         | 0100nnnn00010010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | MACL -> (Rn)       |         |        |
	STS_MACL_Dec(Reg),
	/// | PR,@-Rn           | 0100nnnn00100010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | PR -> (Rn)         |         |        |
	STS_PR_Dec(Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1000 | Rn - Rm -> Rn      | 1       | -      |
	SUB(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1010 | Rn - Rm - T -> Rn, | 1       | Borrow |
	/// |                   |                  | Borrow -> T        |         |        |
	SUBC(Reg,Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1011 | Rn - Rm -> Rn,     | 1       | Under  |
	/// |                   |                  | Underflow -> T     |         |        |
	SUBV(Reg,Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1000 | Rm -> Swap upper   | 1       | -      |
	/// |                   |                  | and lower 2 bytes  |         |        |
	/// |                   |                  | -> Rn              |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1001 | Rm -> Swap upper   | 1       | -      |
	/// |                   |                  | and lower word ->  |         |        |
	/// |                   |                  | Rn                 |         |        |
	SWAP(Size,Reg,Reg),
	/// | @Rn               | 0100nnnn00011011 | if (Rn) is 0, 1->T | 4       | Result |
	/// |                   |                  | 1 -> MSB of (Rn)   |         |        |
	TAS(Reg),
	/// | #imm              | 11000011iiiiiiii | PC/SR -> stack     | 8       | -      |
	/// |                   |                  | area, (imm x 4 +   |         |        |
	/// |                   |                  | VBR) -> PC         |         |        |
	TRAPA(u8),
	/// | #imm,R0           | 11001000iiiiiiii | R0 & imm; if the   | 1       | Result |
	/// |                   |                  | result is 0, 1->T  |         |        |
	TST_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1000 | Rn & Rm; if the    | 1       | Result |
	/// |                   |                  | result is 0, 1->T  |         |        |
	TST_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR)    | 11001100iiiiiiii | (R0+GBR) & imm; if | 3       | Result |
	/// |                   |                  | the result is 0,   |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	TST_Byte(u8),
	/// | #imm,R0           | 11001010iiiiiiii | R0 ^ imm -> R0     | 1       | -      |
	XOR_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1010 | Rn ^ Rm -> Rn      | 1       | -      |
	XOR_Reg(Reg,Reg),
	/// | #imm,@(R0,GBR)    | 11001110iiiiiiii | (R0+GBR) ^ imm ->  | 3       | -      |
	/// |                   |                  | (R0+GBR)           |         |        |
	XOR_Byte(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1101 | Center 32 bits of  | 1       | -      |
	/// |                   |                  | Rm and Rn -> Rn    |         |        |
	XTRCT(Reg,Reg),

	/*** Directives ***/
	Const_Imm(Size,i64),
	Const_Label(Size,String),
	Label(String),
}

#[derive(Clone)]
pub(crate) enum State {
	/// Instruction completed for output
	Complete(u16),
	/// Instruction / directive still waiting on label resolution
	Incomplete(Ins),
}

impl State {
	pub fn completed_or(&self, default: u16) -> u16 {
		match self {
			Self::Complete(inst) => *inst,
			Self::Incomplete(_) => default,
		}
	}
}

impl std::fmt::Debug for State {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Complete(inst) => write!(fmt, "Complete(${inst:04X})"),
			Self::Incomplete(inst) => write!(fmt, "Incomplete({inst:?})"),
		}
	}
}

#[derive(Default)]
pub(crate) struct Output {
	pub(crate) sections: crate::SectionMap,
	pub(crate) labels: crate::LabelMap,
}

impl Output {
	fn add_to_section(&mut self, section_key: u64, ins: Ins) {
		self.sections.entry(section_key).or_default()
			.push(State::Incomplete(ins));
	}
}

impl std::fmt::Debug for Output {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
		for (address, section) in &self.sections {
			writeln!(fmt, "Address: ${address:08X}")?;
			for instr in section {
				writeln!(fmt, "\t{instr:?}")?;
			}
		}

		writeln!(fmt, "Labels:")?;
		for (label,_) in &self.labels {
			writeln!(fmt, "\t{label}")?;
		}

		Ok(())
	}
}

struct Parser<'input, 'tok> {
	file: &'input str,
	index: usize,
	tokens: &'tok [Token],
	errors: Vec<String>,
}

impl Parser<'_,'_> {
	fn is_done(&self) -> bool {
		self.index >= self.tokens.len()
	}

	fn peek(&self, i: usize) -> &Token {
		&self.tokens[self.index + i]
	}
	fn curr(&self) -> &Token {
		self.peek(0)
	}

	fn expected(&mut self, msg: &str) {
		let tok = self.curr();
		let txt = tok.to_string(self.file);
		let (line,pos) = tok.pos();
		self.errors.push(format!("ERROR: Expected {msg}, Found '{txt}' @ ({line}:{pos})"));
	}

	fn error(&mut self, msg: &str) {
		self.expected(msg);
		while self.peek(1).get_type() != TokenType::Newline {
			self.next();
		}
		self.next();
	}

	fn next(&mut self) -> &Token {
		self.index += 1;
		self.curr()
	}

	fn number_pos(&self) -> Option<i64> {
		let txt = self.curr().to_string(self.file);
		match txt.chars().next() {
			Some('%') => {
				i64::from_str_radix(&txt[1..].replace('_',""), 2).ok()
			}
			Some('$') => {
				i64::from_str_radix(&txt[1..].replace('_',""), 16).ok()
			}
			Some(c) if c.is_numeric() => {
				i64::from_str_radix(&txt.replace('_',""), 10).ok()
			}
			_ => unreachable!("number tokens should only have valid binary, decimal, or hexadecimal values"),
		}
	}

	fn number(&mut self) -> Option<i64> {
		let mut is_neg = false;
		if self.curr().get_type() == TokenType::Dash {
			self.match_token_or_err(TokenType::Number, "Number after unary minus")?;
			is_neg = true;
		}
		let num = self.number_pos()?;
		Some(if is_neg { -num } else { num })
	}

	fn reg(&self) -> Option<Reg> {
		let txt = self.curr().to_string(self.file);
		if txt.to_lowercase() == "pc" {
			Some(15)
		} else {
			u8::from_str_radix(&txt[1..], 10).ok()
		}
	}

	fn address(&mut self) -> Option<Arg> {
		let nxt_tok = self.next();
		match nxt_tok.get_type() {
			TokenType::Dash => {
				let reg = self.match_reg()?;
				Some(Arg::PreDec(reg))
			}
			TokenType::Register => {
				let reg = self.reg()?;
				if self.try_match_token(TokenType::Plus).is_some() {
					Some(Arg::PostInc(reg))
				} else {
					Some(Arg::IndReg(reg))
				}
			}
			TokenType::OParen => {
				fn parse_num(data: &mut Parser, num: i64) -> Option<Arg> {
					data.assert_token_with_offset(0, TokenType::Comma)?;
					match data.peek(1).get_type() {
						TokenType::GBR => {
							let imm = data.assert_within_i8(num)?;
							data.next(); // Comma
							data.next(); // GBR
							data.match_token(TokenType::CParen);
							Some(Arg::DispGBR(imm))
						}
						TokenType::PC => {
							let imm = data.assert_within_i8(num)?;
							data.next(); // Comma
							data.next(); // PC
							data.match_token(TokenType::CParen);
							Some(Arg::DispPC(imm))
						}
						TokenType::Register => {
							let imm = data.assert_within_i4(num)?;
							let src = data.reg()?;
							data.next(); // Comma
							data.next(); // Register
							data.match_token(TokenType::CParen);
							Some(Arg::DispReg(imm,src))
						}
						_ => {
							data.error("GBR or Register");
							None
						}
					}
				}

				match self.next().get_type() {
					TokenType::Dash | TokenType::Number => {
						let num = self.number()?;
						parse_num(self, num)
					}
					TokenType::Register => {
						let disp = self.reg()?;
						self.assert_r0(disp);
						self.match_token(TokenType::Comma);
						let src = self.match_reg()?;
						self.match_token(TokenType::CParen);
						Some(Arg::DispR0(src))
					}
					_ => {
						self.error("Number or R0");
						None
					}
				}
			}
			_ => {
				self.error("Address specifier (@r_/ @-r_ / @r_+)");
				None
			}
		}
	}

	fn match_size(&mut self) -> Option<Size> {
		match self.next().get_type() {
			TokenType::Byte => Some(Size::Byte),
			TokenType::Word => Some(Size::Word),
			TokenType::Long => Some(Size::Long),
			_ => {
				self.error("size specifier");
				None
			}
		}
	}

	fn size(&mut self) -> Option<Size> {
		self.match_token(TokenType::Dot)?;
		self.match_size()
	}

	fn try_match_token(&mut self, tt: TokenType) -> Option<&Token> {
		if self.peek(1).get_type() == tt {
			Some(self.next())
		} else {
			None
		}
	}

	fn assert_token_with_offset_or_err(&mut self, offset: usize, tt: TokenType, msg: &str) -> Option<()> {
		if self.peek(offset).get_type() != tt {
			self.error(msg);
			None
		} else {
			Some(())
		}
	}

	fn assert_token_with_offset(&mut self, offset: usize, tt: TokenType) -> Option<()> {
		self.assert_token_with_offset_or_err(offset, tt, &tt.to_string())
	}

	fn match_token_or_err<'a>(&'a mut self, tt: TokenType, msg: &'_ str) -> Option<&'a Token> {
		self.assert_token_with_offset_or_err(1, tt, msg)?;
		Some(self.next())
	}

	fn match_token(&mut self, tt: TokenType) -> Option<&Token> {
		self.match_token_or_err(tt, &tt.to_string())
	}

	fn match_tokens(&mut self, tts: &[TokenType]) -> Option<()> {
		for tt in tts {
			self.match_token(*tt)?;
		}
		Some(())
	}

	fn match_number_pos(&mut self) -> Option<i64> {
		self.match_token(TokenType::Number)?;
		self.number_pos()
	}

	fn match_number(&mut self) -> Option<i64> {
		let mut is_neg = false;
		let num = if self.peek(1).get_type() == TokenType::Dash {
			is_neg = true;
			self.match_token_or_err(TokenType::Number, "Number after unary minus")?;
			self.number_pos()?
		} else {
			self.match_number_pos()?
		};
		Some(if is_neg { -num } else { num })
	}

	fn match_reg(&mut self) -> Option<Reg> {
		self.match_token(TokenType::Register)?;
		self.reg()
	}

	fn assert_r0(&mut self, reg: u8) -> Option<()> {
		if reg == 0 {
			Some(())
		} else {
			self.error("R0");
			None
		}
	}

	fn match_r0(&mut self) -> Option<()> {
		let reg = self.match_reg()?;
		self.assert_r0(reg);
		Some(())
	}

	fn match_reg_args(&mut self) -> Option<(Reg,Reg)> {
		let src = self.match_reg()?;
		self.match_token(TokenType::Comma);
		let dst = self.match_reg()?;
		Some((src,dst))
	}

	fn assert_within_i4(&mut self, value: i64) -> Option<i8> {
		if !(-8..7).contains(&value) {
			self.error("immediate value between -8 & 7");
			None
		} else {
			Some(((value as u8) & 0x0F) as i8)
		}
	}

	fn assert_within_i8(&mut self, value: i64) -> Option<i8> {
		if !(i8::MIN as i64..i8::MAX as i64).contains(&value) {
			self.error("immediate value between -128 & 127");
			None
		} else {
			Some(value as i8)
		}
	}

	fn assert_within_i16(&mut self, value: i64) -> Option<i16> {
		if !(i16::MIN as i64..i16::MAX as i64).contains(&value) {
			self.error("immediate value between -32768 & 32767");
			None
		} else {
			Some(value as i16)
		}
	}

	fn assert_within_i32(&mut self, value: i64) -> Option<i32> {
		if !(i32::MIN as i64..i32::MAX as i64).contains(&value) {
			self.error("immediate value between -2147483648 & 2147483647");
			None
		} else {
			Some(value as i32)
		}
	}

	fn assert_within_u8(&mut self, value: i64) -> Option<u8> {
		if !(u8::MIN as i64..u8::MAX as i64).contains(&value) {
			self.error("immediate value between 0 & 255");
			None
		} else {
			Some(value as u8)
		}
	}
}

// TODO - srenshaw - Ensure the parser actually returns what it's supposed to.

/// Parses strings of tokens into valid instructions & directives
///
/// Given a sequence of valid tokens, the parser should return either a section and label table for
/// the analysis stage, or a sequence of all errors encountered while parsing the input.
pub fn parser(
	file: &str,
	tokens: &[Token],
) -> Result<Output, Vec<String>> {
	let mut skey = 0;
	let mut output = Output::default();

	let mut data = Parser { file, tokens, index: 0, errors: Vec::new() };

	while !data.is_done() {
		use TokenType::*;
		let cur_tok = data.curr();
		match cur_tok.get_type() {
			ADD => match data.next().get_type() {
				Dash | Number => data.number()
					.and_then(|num| data.assert_within_i8(num))
					.and_then(|imm| data.match_token(Comma).map(|_| imm))
					.zip(data.match_reg())
					.map(|(imm,reg)| Ins::ADD_Imm(imm,reg)),
				Register => data.reg()
					.and_then(|src| data.match_token(Comma).map(|_| src))
					.zip(data.match_reg())
					.map(|(src,dst)| Ins::ADD_Reg(src,dst)),
				_ => {
					data.error("Valid ADD source argument: Number or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			ADDC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::ADDC(src,dst)))
				.unwrap_or_default(),
			ADDV => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::ADDV(src,dst)))
				.unwrap_or_default(),
			AND => match data.next().get_type() {
				Number => data.number_pos()
					.and_then(|num| data.assert_within_u8(num))
					.and_then(|imm| data.match_token(Comma).map(|_| imm))
					.and_then(|imm| data.match_r0().map(|_| imm))
					.map(Ins::AND_Imm),
				Register => data.reg()
					.and_then(|src| data.match_token(Comma).map(|_| src))
					.zip(data.match_reg())
					.map(|(src,dst)| Ins::AND_Reg(src,dst)),
				Dot => || -> Option<Ins> {
					data.match_token(Byte)?;
					let num = data.match_number()?;
					let imm = data.assert_within_u8(num)?;
					data.match_tokens(&[Comma,Address,OParen])?;
					data.match_r0()?;
					data.match_tokens(&[Comma,GBR,CParen])?;
					Some(Ins::AND_Byte(imm))
				}(),
				_ => {
					data.error("Valid AND source argument: Number or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			BF => match data.next().get_type() {
				Identifier => {
					let lbl = data.curr().to_string(&file);
					Some(Ins::BF(lbl))
				}
				Slash => || -> Option<Ins> {
					data.match_token(Delay)?;
					let lbl_tok = data.match_token_or_err(Identifier, "Label")?;
					let lbl = lbl_tok.to_string(&file);
					Some(Ins::BFS(lbl))
				}(),
				_ => {
					data.error("Label");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			BRA => data.match_token_or_err(Identifier, "Label")
				.map(|lbl_tok| lbl_tok.to_string(&file))
				.map(|lbl| output.add_to_section(skey, Ins::BRA(lbl)))
				.unwrap_or_default(),
			BRAF => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BRAF(reg)))
				.unwrap_or_default(),
			BSR => data.match_token_or_err(Identifier, "Label")
				.map(|lbl_tok| lbl_tok.to_string(&file))
				.map(|lbl| output.add_to_section(skey, Ins::BSR(lbl)))
				.unwrap_or_default(),
			BSRF => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BSRF(reg)))
				.unwrap_or_default(),
			BT => match data.next().get_type() {
				Identifier => {
					let lbl = data.curr().to_string(&file);
					Some(Ins::BT(lbl))
				}
				Slash => || -> Option<Ins> {
					data.match_token(Delay)?;
					let lbl_tok = data.match_token_or_err(Identifier, "Label")?;
					let lbl = lbl_tok.to_string(&file);
					Some(Ins::BTS(lbl))
				}(),
				_ => {
					data.error("Label");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			CLRMAC => output.add_to_section(skey, Ins::CLRMAC),
			CLRT => output.add_to_section(skey, Ins::CLRT),
			CMP => {
				let mut func = || -> Option<Ins> {
					data.match_token(Slash)?;
					match data.next().get_type() {
						EQ => if data.next().get_type() == Number {
							let num = data.number()?;
							let imm = data.assert_within_i8(num)?;
							data.match_token(Comma)?;
							data.match_r0()?;
							Some(Ins::CMP_EQ_Imm(imm))
						} else {
							data.match_reg_args().map(|(src,dst)| Ins::CMP_EQ_Reg(src,dst))
						},
						GE => data.match_reg_args().map(|(src,dst)| Ins::CMP_GE(src,dst)),
						GT => data.match_reg_args().map(|(src,dst)| Ins::CMP_GT(src,dst)),
						HI => data.match_reg_args().map(|(src,dst)| Ins::CMP_HI(src,dst)),
						HS => data.match_reg_args().map(|(src,dst)| Ins::CMP_HS(src,dst)),
						PL => data.match_reg().map(Ins::CMP_PL),
						PZ => data.match_reg().map(Ins::CMP_PZ),
						STR => data.match_reg_args().map(|(src,dst)| Ins::CMP_STR(src,dst)),
						_ => {
							data.error("Comparator type (EQ,GT,GE,HI,HS,PL,PZ,STR)");
							None
						}
					}
				};
				if let Some(ins) = func() {
					output.add_to_section(skey, ins);
				}
			}
			Comment => {} // skip comments
			Const => data.size()
				.and_then(|sz| match data.next().get_type() {
					Number => data.number()
						.and_then(|num| match sz {
							Size::Byte => data.assert_within_i8(num).map(|n| n as i64),
							Size::Word => data.assert_within_i16(num).map(|n| n as i64),
							Size::Long => data.assert_within_i32(num).map(|n| n as i64),
						})
						.map(|imm| Ins::Const_Imm(sz, imm)),
					Identifier => {
						let txt = data.curr().to_string(&file);
						Some(Ins::Const_Label(sz, txt))
					}
					_ => {
						data.error("integer literal or label");
						None
					}
				})
				.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			DIV0S => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::DIV0S(src,dst)))
				.unwrap_or_default(),
			DIV0U => output.add_to_section(skey, Ins::DIV0U),
			DIV1 => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::DIV1(src,dst)))
				.unwrap_or_default(),
			DMULS => data.size()
				.and_then(|sz| if sz != Size::Long {
					data.error("Size specifier Long('l')");
					None
				} else {
					data.match_reg_args()
				})
				.map(|(src,dst)| output.add_to_section(skey, Ins::DMULS(src,dst)))
				.unwrap_or_default(),
			DMULU => data.size()
				.and_then(|sz| if sz != Size::Long {
					data.error("Size specifier Long('l')");
					None
				} else {
					data.match_reg_args()
				})
				.map(|(src,dst)| output.add_to_section(skey, Ins::DMULU(src,dst)))
				.unwrap_or_default(),
			DT => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::DT(reg)))
				.unwrap_or_default(),
			EXTS => data.size()
				.and_then(|sz| if sz == Size::Long {
					data.error("Size specifier Byte('b') or Word('w')");
					None
				} else {
					data.match_reg_args()
						.map(|(src,dst)| output.add_to_section(skey, Ins::EXTS(sz,src,dst)))
				})
				.unwrap_or_default(),
			EXTU => data.size()
				.and_then(|sz| if sz == Size::Long {
					data.error("Size specifier Byte('b') or Word('w')");
					None
				} else {
					data.match_reg_args()
						.map(|(src,dst)| output.add_to_section(skey, Ins::EXTU(sz,src,dst)))
				})
				.unwrap_or_default(),
			Identifier => {
				let lbl = cur_tok.to_string(&file);
				if data.match_token_or_err(Colon, "End of label declaration (':')").is_none() {
					continue;
				}
				if output.labels.contains_key(&lbl) {
					eprintln!("ERROR: Label '{lbl}' already defined");
					while data.peek(1).get_type() != Newline {
						data.next();
					}
					data.next();
					continue;
				}
				output.labels.insert(lbl.clone(), None);
				output.add_to_section(skey, Ins::Label(lbl));
			}
			JMP => {
				// TODO - srenshaw - Add label handling for JMP
				let mut func = || -> Option<Ins> {
					data.match_token(Address)?;
					let reg = data.match_reg()?;
					Some(Ins::JMP(reg))
				};
				if let Some(ins) = func() {
					output.add_to_section(skey, ins);
				}
			}
			JSR => {
				// TODO - srenshaw - Add label handling for JSR
				let mut func = || -> Option<Ins> {
					data.match_token(Address)?;
					let reg = data.match_reg()?;
					Some(Ins::JSR(reg))
				};
				if let Some(ins) = func() {
					output.add_to_section(skey, ins);
				}
			}
			LDC => match data.next().get_type() {
				Register => || -> Option<Ins> {
					let reg = data.reg()?;
					data.match_token(Comma)?;
					match data.next().get_type() {
						GBR => Some(Ins::LDC_GBR(reg)),
						SR => Some(Ins::LDC_SR(reg)),
						VBR => Some(Ins::LDC_VBR(reg)),
						_ => {
							data.error("Control Register (GBR,SR,VBR)");
							None
						}
					}
				}(),
				Dot => || -> Option<Ins> {
					data.match_tokens(&[Long,Address])?;
					let reg = data.match_reg()?;
					data.match_token(Plus)?;
					match data.next().get_type() {
						GBR => Some(Ins::LDC_GBR_Inc(reg)),
						SR => Some(Ins::LDC_SR_Inc(reg)),
						VBR => Some(Ins::LDC_VBR_Inc(reg)),
						_ => {
							data.error("Control Register (GBR,SR,VBR)");
							None
						}
					}
				}(),
				_ => {
					data.error("Valid LDC instruction");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			LDS => match data.next().get_type() {
					Register => data.reg()
						.and_then(|reg| data.match_token(Comma).map(|_| reg))
						.and_then(|reg| match data.next().get_type() {
							MACH => Some(Ins::LDS_MACH(reg)),
							MACL => Some(Ins::LDS_MACL(reg)),
							PR => Some(Ins::LDS_PR(reg)),
							_ => {
								data.error("Special Register (MACH,MACL,PR)");
								None
							}
						}),
					Dot => || -> Option<Ins> {
						data.match_tokens(&[Long,Address])?;
						let reg = data.match_reg()?;
						data.match_token(Plus)?;
						match data.next().get_type() {
							MACH => Some(Ins::LDS_MACH_Inc(reg)),
							MACL => Some(Ins::LDS_MACL_Inc(reg)),
							PR => Some(Ins::LDS_PR_Inc(reg)),
							_ => {
								data.error("Special Register (MACH,MACL,PR)");
								None
							}
						}
					}(),
					_ => {
						data.error("Valid LDS instruction");
						None
					}
				}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			MAC => {
				let ins_func = match data.size() {
					Some(Size::Byte) => {
						data.error("Size specifier Word('w') or Long('l')");
						continue;
					}
					Some(Size::Word) => Ins::MAC_Word,
					Some(Size::Long) => Ins::MAC_Long,
					None => continue,
				};
				let ins = || -> Option<Ins> {
					data.match_token(Address)?;
					let src = data.match_reg()?;
					data.match_tokens(&[Plus,Comma,Address])?;
					let dst = data.match_reg()?;
					data.match_token(Plus)?;
					Some(ins_func(src,dst))
				}();
				if let Some(ins) = ins {
					output.add_to_section(skey, ins);
				}
			}
			MOV => match data.next().get_type() {
				Dash | Number => {
					data.match_number()
						.and_then(|num| data.assert_within_i8(num))
						.and_then(|imm| data.match_token(Comma).map(|_| imm))
						.zip(data.match_reg())
						.map(|(imm,reg)| Ins::MOV_Imm(imm,reg))
				}
				Register => {
					data.reg()
						.and_then(|src| data.match_token(Comma).map(|_| src))
						.zip(data.match_reg())
						.map(|(src,dst)| Ins::MOV_Reg(src,dst))
				}
				Dot => || -> Option<Ins> {
					let size = data.match_size()?;

					let src = match data.next().get_type() {
						Address => data.address(),
						Register => data.reg().map(Arg::DirReg),
						_ => {
							data.error("Register, Displacement, or Address");
							None
						}
					}?;

					data.match_token(Comma)?;

					let dst = match data.next().get_type() {
						Address => data.address(),
						Register => data.reg().map(Arg::DirReg),
						_ => {
							data.error("Register, Displacement, or Address");
							None
						}
					}?;

					Some(Ins::MOV(size,src,dst))
				}(),
				_ => {
					data.error("size specifier, 8-bit immediate, or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			MOVA => data.match_tokens(&[Address, OParen])
				.and_then(|_| data.match_number())
				.and_then(|num| data.assert_within_i8(num))
				.and_then(|num| data.match_tokens(&[Comma,PC,CParen,Comma]).map(|_| num))
				.and_then(|num| data.match_r0().map(|_| num))
				.map(|num| output.add_to_section(skey, Ins::MOVA(num)))
				.unwrap_or_default(),
			MOVT => data.match_reg()
				.map(|dst| output.add_to_section(skey, Ins::MOVT(dst)))
				.unwrap_or_default(),
			MUL => data.match_tokens(&[Dot,Long])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MUL(src,dst)))
				.unwrap_or_default(),
			MULS => data.match_tokens(&[Dot,Word])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MULS(src,dst)))
				.unwrap_or_default(),
			MULU => data.match_tokens(&[Dot,Word])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MULU(src,dst)))
				.unwrap_or_default(),
			NEG => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NEG(src,dst)))
				.unwrap_or_default(),
			NEGC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NEGC(src,dst)))
				.unwrap_or_default(),
			Newline => {} // skip newlines
			NOP => output.add_to_section(skey, Ins::NOP),
			NOT => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NOT(src,dst)))
				.unwrap_or_default(),
			OR => match data.next().get_type() {
				Number => data.number_pos()
					.and_then(|num| data.assert_within_u8(num))
					.and_then(|imm| data.match_token(Comma).map(|_| imm))
					.and_then(|imm| data.match_r0().map(|_| imm))
					.map(Ins::OR_Imm),
				Register => data.reg()
					.and_then(|src| data.match_token(Comma).map(|_| src))
					.zip(data.match_reg())
					.map(|(src,dst)| Ins::OR_Reg(src,dst)),
				Dot => || -> Option<Ins> {
					data.match_token(Byte)?;
					let num = data.match_number()?;
					let imm = data.assert_within_u8(num)?;
					data.match_tokens(&[Comma,Address,OParen])?;
					data.match_r0()?;
					data.match_tokens(&[Comma,GBR,CParen])?;
					Some(Ins::OR_Byte(imm))
				}(),
				_ => {
					data.error("Valid OR source argument: Number or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			Org => data.match_number_pos()
				.map(|addr| skey = addr as u64)
				.unwrap_or_default(),
			ROTCL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTCL(reg)))
				.unwrap_or_default(),
			ROTCR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTCR(reg)))
				.unwrap_or_default(),
			ROTL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTL(reg)))
				.unwrap_or_default(),
			ROTR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTR(reg)))
				.unwrap_or_default(),
			RTE => output.add_to_section(skey, Ins::RTE),
			RTS => output.add_to_section(skey, Ins::RTS),
			SETT => output.add_to_section(skey, Ins::SETT),
			SHAL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHAL(reg)))
				.unwrap_or_default(),
			SHAR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHAR(reg)))
				.unwrap_or_default(),
			SHLL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL(reg)))
				.unwrap_or_default(),
			SHLL2 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL2(reg)))
				.unwrap_or_default(),
			SHLL8 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL8(reg)))
				.unwrap_or_default(),
			SHLL16 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL16(reg)))
				.unwrap_or_default(),
			SHLR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR(reg)))
				.unwrap_or_default(),
			SHLR2 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR2(reg)))
				.unwrap_or_default(),
			SHLR8 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR8(reg)))
				.unwrap_or_default(),
			SHLR16 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR16(reg)))
				.unwrap_or_default(),
			SLEEP => output.add_to_section(skey, Ins::SLEEP),
			STC => || -> Option<()> {
				fn comma_reg(p: &mut Parser) -> Option<Reg> {
					p.match_token(Comma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Option<Reg> {
					p.match_tokens(&[Comma,Address,Dash])?;
					p.match_reg()
				}
				let ins = match data.next().get_type() {
					GBR => comma_reg(&mut data).map(Ins::STC_GBR)?,
					SR => comma_reg(&mut data).map(Ins::STC_SR)?,
					VBR => comma_reg(&mut data).map(Ins::STC_VBR)?,
					Dot => {
						data.match_token(Long)?;
						match data.next().get_type() {
							GBR => pre_dec(&mut data).map(Ins::STC_GBR_Dec)?,
							SR => pre_dec(&mut data).map(Ins::STC_SR_Dec)?,
							VBR => pre_dec(&mut data).map(Ins::STC_VBR_Dec)?,
							_ => {
								data.error("Control Register (GBR,SR,VBR)");
								return None;
							}
						}
					}
					_ => {
						data.error("Valid STC instruction");
						return None;
					}
				};
				Some(output.add_to_section(skey, ins))
			}().unwrap_or_default(),
			STS => || -> Option<()> {
				fn comma_reg(p: &mut Parser) -> Option<Reg> {
					p.match_token(Comma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Option<Reg> {
					p.match_tokens(&[Comma,Address,Dash])?;
					p.match_reg()
				}
				let ins = match data.next().get_type() {
					MACH => comma_reg(&mut data).map(Ins::STS_MACH)?,
					MACL => comma_reg(&mut data).map(Ins::STS_MACL)?,
					PR => comma_reg(&mut data).map(Ins::STS_PR)?,
					Dot => {
						data.match_token(Long)?;
						match data.next().get_type() {
							MACH => pre_dec(&mut data).map(Ins::STS_MACH_Dec)?,
							MACL => pre_dec(&mut data).map(Ins::STS_MACL_Dec)?,
							PR => pre_dec(&mut data).map(Ins::STS_PR_Dec)?,
							_ => {
								data.error("Special Register (MACH,MACL,PR)");
								return None;
							}
						}
					}
					_ => {
						data.error("Valid STS instruction");
						return None;
					}
				};
				Some(output.add_to_section(skey, ins))
			}().unwrap_or_default(),
			SUB => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUB(src,dst)))
				.unwrap_or_default(),
			SUBC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUBC(src,dst)))
				.unwrap_or_default(),
			SUBV => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUBV(src,dst)))
				.unwrap_or_default(),
			SWAP => || -> Option<()> {
				data.match_token(Dot)?;
				let sz = data.match_size()?;
				let (src,dst) = data.match_reg_args()?;
				Some(output.add_to_section(skey, Ins::SWAP(sz,src,dst)))
			}().unwrap_or_default(),
			TAS => data.match_tokens(&[Dot, Byte, Address])
				.and_then(|_| data.match_reg())
				.map(Ins::TAS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TRAPA => data.match_number()
				.and_then(|num| data.assert_within_u8(num))
				.map(Ins::TRAPA)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TST => match data.next().get_type() {
				Number => data.number_pos()
					.and_then(|num| data.assert_within_u8(num))
					.and_then(|imm| data.match_token(Comma).map(|_| imm))
					.and_then(|imm| data.match_r0().map(|_| imm))
					.map(Ins::TST_Imm),
				Register => data.reg()
					.and_then(|src| data.match_token(Comma).map(|_| src))
					.zip(data.match_reg())
					.map(|(src,dst)| Ins::TST_Reg(src,dst)),
				Dot => || -> Option<Ins> {
					data.match_token(Byte)?;
					let num = data.match_number()?;
					let imm = data.assert_within_u8(num)?;
					data.match_tokens(&[Comma,Address,OParen])?;
					data.match_r0()?;
					data.match_tokens(&[Comma,GBR,CParen])?;
					Some(Ins::TST_Byte(imm))
				}(),
				_ => {
					data.error("Valid TST source argument: Number or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			XOR => match data.next().get_type() {
				Number => data.number_pos()
					.and_then(|num| data.assert_within_u8(num))
					.and_then(|imm| data.match_token(Comma).map(|_| imm))
					.and_then(|imm| data.match_r0().map(|_| imm))
					.map(Ins::XOR_Imm),
				Register => data.reg()
					.and_then(|src| data.match_token(Comma).map(|_| src))
					.zip(data.match_reg())
					.map(|(src,dst)| Ins::XOR_Reg(src,dst)),
				Dot => || -> Option<Ins> {
					data.match_token(Byte)?;
					let num = data.match_number()?;
					let imm = data.assert_within_u8(num)?;
					data.match_tokens(&[Comma,Address,OParen])?;
					data.match_r0()?;
					data.match_tokens(&[Comma,GBR,CParen])?;
					Some(Ins::XOR_Byte(imm))
				}(),
				_ => {
					data.error("Valid XOR source argument: Number or Register");
					None
				}
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			XTRCT => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::XTRCT(src,dst)))
				.unwrap_or_default(),
			Unknown => {
				let txt = cur_tok.to_string(&file);
				let (line,pos) = cur_tok.pos();
				eprintln!("unknown item '{txt}' @ ({line}:{pos})");
			}
			_ => {
				let txt = cur_tok.to_string(&file);
				let (line,pos) = cur_tok.pos();
				eprintln!("unexpected {txt} @ ({line}:{pos})");
			}
		}
		data.index += 1;
	}

	Ok(output)
}

