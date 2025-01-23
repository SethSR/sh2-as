
use crate::Label;
use crate::lexer::{Token,TokenType};

pub(crate) type Reg = u8;

#[derive(Clone)]
pub(crate) enum Arg {
	DirReg(Reg),
	DispR0(Reg),
	DispReg(i8,Reg),
	DispPC(i8),
	DispGBR(i8),
	DispLabel(Label,Reg),
	IndReg(Reg),
	Label(Label),
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
	BF(Label),
	/// | label             | 10001111dddddddd | if T = 0,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 1, nop      |         |        |
	BFS(Label),
	/// | label             | 1010dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	BRA(Label),
	/// | Rm                | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BRAF(Reg),
	/// | label             | 1011dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	BSR(Label),
	/// | Rm                | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BSRF(Reg),
	/// | label             | 10001001dddddddd | if T = 1,          | 3/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	BT(Label),
	/// | label             | 10001101dddddddd | if T = 1,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	BTS(Label),
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
	Const_Label(Size,Label),
	Label(Label),
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
	pub(crate) values: std::collections::HashMap<Label, i8>,
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

struct Parser<'tok> {
	index: usize,
	tokens: &'tok [Token],
	errors: Vec<String>,
}

macro_rules! error {
	($t:ty, $data:expr, $msg:expr) => {{
		$data.next_line();
		Err::<$t, std::string::String>($data.expected($msg))
	}}
}

impl Parser<'_> {
	fn is_done(&self) -> bool {
		self.index >= self.tokens.len()
	}

	fn peek(&self, i: usize) -> &Token {
		&self.tokens[self.index + i]
	}
	fn curr(&self) -> &Token {
		self.peek(0)
	}

	fn expected(&mut self, msg: &str) -> String {
		let tok = self.curr().clone();
		let (line,pos) = tok.pos();
		match tok.get_type() {
			TokenType::IdNumber => {
				let num = self.signed(Size::Long);
				let num = num.expect("unable to parse number");
				format!("ERROR: Expected '{msg}', Found '{tok}' ({num}) @ [{line}:{pos}]")
			}
			_ => format!("ERROR: Expected '{msg}', Found '{tok}' @ [{line}:{pos}]"),
		}
	}

	fn next(&mut self) -> &Token {
		self.index += 1;
		self.curr()
	}

	fn next_line(&mut self) {
		while self.peek(1).get_type() != TokenType::SymNewline {
			self.next();
		}
		self.next();
	}

	fn to_i64(txt: &str, radix: u32, sz: Size, is_signed: bool) -> Result<i64, String> {
		let txt = txt.replace('_',"");
		if is_signed {
			match sz {
				Size::Byte => i8::from_str_radix(&txt, radix).map(|n| n as i64),
				Size::Word => i16::from_str_radix(&txt, radix).map(|n| n as i64),
				Size::Long => i32::from_str_radix(&txt, radix).map(|n| n as i64),
			}.map_err(|e| format!("{e}"))
		} else {
			match sz {
				Size::Byte => u8::from_str_radix(&txt, radix).map(|n| n as i64),
				Size::Word => u16::from_str_radix(&txt, radix).map(|n| n as i64),
				Size::Long => u32::from_str_radix(&txt, radix).map(|n| n as i64),
			}.map_err(|e| format!("{e}"))
		}
	}

	fn i64_from_dec(txt: &str, sz: Size, is_signed: bool) -> Result<i64, String> {
		Self::to_i64(txt, 10, sz, is_signed)
	}
	fn i64_from_bin(txt: &str, sz: Size) -> Result<i64, String> {
		Self::to_i64(&txt[1..], 2, sz, false)
	}
	fn i64_from_hex(txt: &str, sz: Size) -> Result<i64, String> {
		Self::to_i64(&txt[1..], 16, sz, false)
	}

	fn unsigned(&mut self, sz: Size) -> Result<i64, String> {
		let txt = self.curr().to_string();
		match txt.chars().next() {
			Some('%') => Self::i64_from_bin(&txt, sz),
			Some('$') => Self::i64_from_hex(&txt, sz),
			Some(c) if c.is_numeric() => Self::i64_from_dec(&txt, sz, false),
			_ => error!(i64, self, "parsing non-numeric"),
		}
	}

	fn signed(&mut self, sz: Size) -> Result<i64, String> {
		let txt = self.curr().to_string();
		match txt.chars().next() {
			Some('%') => Self::i64_from_bin(&txt, sz),
			Some('$') => Self::i64_from_hex(&txt, sz),
			Some('-') => Self::i64_from_dec(&txt, sz, true),
			Some(c) if c.is_numeric() => Self::i64_from_dec(&txt, sz, false),
			_ => error!(i64, self, "parsing non-numeric"),
		}
	}

	fn reg(&self) -> Result<Reg, String> {
		let txt = self.curr().to_string();
		if txt.to_lowercase() == "pc" {
			Ok(15)
		} else {
			u8::from_str_radix(&txt[1..], 10).map_err(|e| format!("{e}"))
		}
	}

	fn address(&mut self) -> Result<Arg, String> {
		use TokenType as TT;

		match self.next().get_type() {
			TT::SymDash => self.match_reg()
				.map(Arg::PreDec),
			TT::IdRegister => {
				let reg = self.reg()?;
				if self.match_token(TT::SymPlus).is_ok() {
					Ok(Arg::PostInc(reg))
				} else {
					Ok(Arg::IndReg(reg))
				}
			}
			TT::SymOParen => match self.next().get_type() {
				TT::IdLabel => {
					let Some(lbl) = self.curr().get_id() else {
						return error!(Arg, self, "Unknown Value");
					};
					self.match_token(TT::SymComma)?;
					let reg = self.match_reg()?;
					self.match_token(TT::SymCParen)?;
					Ok(Arg::DispLabel(lbl,reg))
				}
				TT::IdNumber => {
					let num = self.signed(Size::Byte)?;
					self.match_token(TT::SymComma)?;
					match self.next().get_type() {
						TT::SymGBR => self.assert_within_i8(num)
							.and_then(|imm| self.match_token(TT::SymCParen).map(|_| imm))
							.map(Arg::DispGBR),
						TT::SymPC => self.assert_within_i8(num)
							.and_then(|imm| self.match_token(TT::SymCParen).map(|_| imm))
							.map(Arg::DispPC),
						TT::IdRegister => self.assert_within_i4(num)
							.and_then(|imm| self.reg().map(|r| (imm,r)))
							.and_then(|pair| self.match_token(TT::SymCParen).map(|_| pair))
							.map(|(disp,reg)| Arg::DispReg(disp,reg)),
						_ => error!(Arg, self, "GBR, PC, or Register"),
					}
				}
				TT::IdRegister => self.reg_args()
					.and_then(|(r0,reg)| self.assert_r0(r0).map(|_| reg))
					.and_then(|reg| self.match_token(TT::SymCParen).map(|_| reg))
					.map(Arg::DispR0),
				_ => error!(Arg, self, "Number or R0"),
			}
			_ => error!(Arg, self, "Address specifier (@r_/ @-r_ / @r_+)"),
		}
	}

	fn match_size(&mut self) -> Result<Size, String> {
		match self.next().get_type() {
			TokenType::SymByte => Ok(Size::Byte),
			TokenType::SymWord => Ok(Size::Word),
			TokenType::SymLong => Ok(Size::Long),
			_ => error!(Size, self, "size specifier"),
		}
	}

	fn size(&mut self) -> Result<Size, String> {
		self.match_token(TokenType::SymDot)?;
		self.match_size()
	}

	fn assert_token_with_offset_or_err<'a>(&mut self,
		offset: usize,
		tt: TokenType,
		msg: &'a str,
	) -> Result<(), String> {
		if self.peek(offset).get_type() != tt {
			error!((), self, msg)
		} else {
			Ok(())
		}
	}

	fn match_token_or_err<'a>(&'a mut self,
		tt: TokenType,
		msg: &str,
	) -> Result<&'a Token, String> {
		self.assert_token_with_offset_or_err(1, tt, msg)?;
		Ok(self.next())
	}

	fn match_ident_or_err<'a>(&mut self, msg: &str) -> Result<Label, String> {
		self.assert_token_with_offset_or_err(1, TokenType::IdLabel, msg)?;
		Ok(self.next().get_id().unwrap())
	}

	fn match_token(&mut self, tt: TokenType) -> Result<&Token, String> {
		self.match_token_or_err(tt, &tt.to_string())
			.map_err(|s| s.to_string())
	}

	fn match_tokens(&mut self, tts: &[TokenType]) -> Result<(),String> {
		for tt in tts {
			self.match_token(*tt)?;
		}
		Ok(())
	}

	fn match_unsigned(&mut self, sz: Size) -> Result<i64, String> {
		self.match_token(TokenType::IdNumber)?;
		self.unsigned(sz)
	}

	fn match_signed(&mut self, sz: Size) -> Result<i64, String> {
		self.match_token(TokenType::IdNumber)?;
		self.signed(sz)
	}

	fn match_reg(&mut self) -> Result<Reg, String> {
		self.match_token(TokenType::IdRegister)?;
		self.reg()
	}

	fn assert_r0(&mut self, reg: u8) -> Result<(), String> {
		if reg == 0 {
			Ok(())
		} else {
			error!((), self, "R0")
		}
	}

	fn match_r0(&mut self) -> Result<(), String> {
		let reg = self.match_reg()?;
		self.assert_r0(reg)?;
		Ok(())
	}

	fn reg_args(&mut self) -> Result<(Reg,Reg), String> {
		let src = self.reg()?;
		self.match_token(TokenType::SymComma)?;
		let dst = self.match_reg()?;
		Ok((src,dst))
	}

	fn match_reg_args(&mut self) -> Result<(Reg,Reg), String> {
		self.assert_token_with_offset_or_err(1,
			TokenType::IdRegister,
			&TokenType::IdRegister.to_string())?;
		self.reg_args()
	}

	fn assert_within_i4(&mut self, value: i64) -> Result<i8, String> {
		if (-8..7).contains(&value) {
			Ok(((value as u8) & 0x0F) as i8)
		} else {
			error!(i8, self, "immediate value between -8 & 7")
		}
	}

	fn assert_within_i8(&mut self, value: i64) -> Result<i8, String> {
		if (i8::MIN as i64..i8::MAX as i64).contains(&value) {
			Ok(value as i8)
		} else {
			error!(i8, self, &format!("immediate value between {} & {}", i8::MIN, i8::MAX))
		}
	}

	fn assert_within_i16(&mut self, value: i64) -> Result<i16, String> {
		if (i16::MIN as i64..i16::MAX as i64).contains(&value) {
			Ok(value as i16)
		} else {
			error!(i16, self, &format!("immediate value between {} & {}", i16::MIN, i16::MAX))
		}
	}

	fn assert_within_i32(&mut self, value: i64) -> Result<i32, String> {
		if (i32::MIN as i64..i32::MAX as i64).contains(&value) {
			Ok(value as i32)
		} else {
			error!(i32, self, &format!("immediate value between {} & {}", i32::MIN, i32::MAX))
		}
	}

	fn assert_within_u8(&mut self, value: i64) -> Result<u8, String> {
		if (u8::MIN as i64..u8::MAX as i64).contains(&value) {
			Ok(value as u8)
		} else {
			error!(u8, self, "immediate value between 0 & 255")
		}
	}

	fn simple_branch_ins(
		&mut self,
		fn_fast: impl Fn(Label) -> Ins,
		fn_delay: impl Fn(Label) -> Ins,
	) -> Result<Ins, String> {
		use TokenType as TT;
		match self.next().get_type() {
			TT::IdLabel => self.curr().get_id()
				.map(fn_fast)
				.ok_or_else(|| {
					self.next_line();
					self.expected("Label")
				}),
			TT::SymSlash => self.match_token(TT::SymDelay).map(|_| ())
				.and_then(|_| self.match_ident_or_err("Label"))
				.map_err(|e| self.expected(&e))
				.map(fn_delay),
			_ => error!(Ins, self, "Label"),
		}
	}

	fn logic_ins(
		&mut self,
		fn_imm: impl Fn(u8) -> Ins,
		fn_reg: impl Fn(Reg,Reg) -> Ins,
		fn_byte: impl Fn(u8) -> Ins,
		ins_name: &str,
	) -> Result<Ins, String> {
		use TokenType as TT;
		match self.next().get_type() {
			// INS r#,r#
			TT::IdRegister => self.reg_args()
				.map(|(src,dst)| fn_reg(src,dst)),
			// INS.B #imm,@(r#,GBR)
			TT::SymDot => self.match_tokens(&[TT::SymByte,TT::SymImmediate])
				.and_then(|_| self.match_signed(Size::Byte))
				.and_then(|num| self.assert_within_u8(num))
				.and_then(|imm| self.match_tokens(&[TT::SymComma,TT::SymAddress,TT::SymOParen]).map(|_| imm))
				.and_then(|imm| self.match_r0().map(|_| imm))
				.and_then(|imm| self.match_tokens(&[TT::SymComma,TT::SymGBR,TT::SymCParen]).map(|_| imm))
				.map(fn_byte),
			// INS #imm,r0
			TT::SymImmediate => self.match_unsigned(Size::Byte)
				.and_then(|num| self.assert_within_u8(num))
				.and_then(|imm| self.match_token(TT::SymComma).map(|_| imm))
				.and_then(|imm| self.match_r0().map(|_| imm))
				.map(fn_imm),
			_ => error!(Ins, self, &format!("Valid {ins_name} source argument: Number or Register")),
		}
	}

	fn dmul_ins(&mut self,
		fn_ins: impl Fn(Reg,Reg) -> Ins,
	) -> Result<Ins, String> {
		self.size()
			.and_then(|sz| if sz == Size::Long {
				self.match_reg_args()
					.map(|(src,dst)| fn_ins(src,dst))
			} else {
				error!(Ins, self, "Size specifier Long('l')")
			})
	}

	fn ext_ins(&mut self,
		fn_ins: impl Fn(Size,Reg,Reg) -> Ins,
	) -> Result<Ins, String> {
		self.size()
			.and_then(|sz| if sz == Size::Long {
				error!(Ins, self, "Size specifier Byte('b') or Word('w')")
			} else {
				self.match_reg_args()
					.map(|(src,dst)| fn_ins(sz,src,dst))
			})
	}
}

// TODO - srenshaw - Ensure the parser actually returns what it's supposed to.

/// Parses strings of tokens into valid instructions & directives
///
/// Given a sequence of valid tokens, the parser should return either a section and label table for
/// the analysis stage, or a sequence of all errors encountered while parsing the input.
pub fn parser(tokens: &[Token]) -> Result<Output, Vec<String>> {
	let mut skey = 0;
	let mut output = Output::default();

	let mut data = Parser { tokens, index: 0, errors: Vec::new() };

	while !data.is_done() {
		use TokenType as TT;
		let cur_tok = data.curr();
		eprintln!("{cur_tok:?}");
		match cur_tok.get_type() {
			TT::IdComment => {} // skip comments
			TT::IdLabel => {
				let lbl = cur_tok.get_id()
					.expect("identifier without referent");
				match data.next().get_type() {
					TT::SymEqual => data.match_signed(Size::Byte)
						.and_then(|num| data.assert_within_i8(num))
						.map(|imm| output.values.insert(lbl.clone(), imm))
						.map(|_| ())
						.unwrap_or_default(),
					TT::SymColon => {
						if output.labels.contains_key(&lbl) {
							data.errors.push(format!("ERROR: Label '{lbl}' already defined"));
							data.next_line();
							continue;
						}
						output.labels.insert(lbl.clone(), None);
						output.add_to_section(skey, Ins::Label(lbl))
					}
					_ => {
						let msg = data.expected(": or \"= <number>\" to declare a new label or value");
						data.errors.push(msg);
						data.next_line();
						continue;
					}
				}
			}
			TT::IdUnknown => {
				let (line,pos) = cur_tok.pos();
				data.errors.push(format!("ERROR: unknown item '{cur_tok}' @ [{line}:{pos}]"));
			}
			TT::InsADD => match data.next().get_type() {
				TT::SymImmediate => data.match_signed(Size::Byte)
					.and_then(|num| data.assert_within_i8(num))
					.and_then(|imm| data.match_token(TT::SymComma).map(|_| imm))
					.and_then(|imm| data.match_reg().map(|r| (imm,r)))
					.map(|(imm,reg)| Ins::ADD_Imm(imm,reg)),
				TT::IdRegister => data.reg_args()
					.map(|(src,dst)| Ins::ADD_Reg(src,dst)),
				_ => error!(Ins, data, "Valid ADD source argument: Number or Register"),
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsADDC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::ADDC(src,dst)))
				.unwrap_or_default(),
			TT::InsADDV => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::ADDV(src,dst)))
				.unwrap_or_default(),
			TT::InsAND => data.logic_ins(Ins::AND_Imm, Ins::AND_Reg, Ins::AND_Byte, "AND")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsBF => data.simple_branch_ins(Ins::BF, Ins::BFS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsBRA => data.match_ident_or_err("Label")
				.map(|lbl| output.add_to_section(skey, Ins::BRA(lbl)))
				.unwrap_or_default(),
			TT::InsBRAF => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BRAF(reg)))
				.unwrap_or_default(),
			TT::InsBSR => data.match_ident_or_err("Label")
				.map(|lbl| output.add_to_section(skey, Ins::BSR(lbl)))
				.unwrap_or_default(),
			TT::InsBSRF => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::BSRF(reg)))
				.unwrap_or_default(),
			TT::InsBT => data.simple_branch_ins(Ins::BT, Ins::BTS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsCLRMAC => output.add_to_section(skey, Ins::CLRMAC),
			TT::InsCLRT => output.add_to_section(skey, Ins::CLRT),
			TT::InsCMP => {
				let mut func = || -> Result<Ins, String> {
					data.match_token(TT::SymSlash)?;
					match data.next().get_type() {
						TT::SymEQ => if data.next().get_type() == TT::SymImmediate {
							let num = data.match_signed(Size::Byte)?;
							let imm = data.assert_within_i8(num)?;
							data.match_token(TT::SymComma)?;
							data.match_r0()?;
							Ok(Ins::CMP_EQ_Imm(imm))
						} else {
							data.match_reg_args().map(|(src,dst)| Ins::CMP_EQ_Reg(src,dst))
						},
						TT::SymGE => data.match_reg_args().map(|(src,dst)| Ins::CMP_GE(src,dst)),
						TT::SymGT => data.match_reg_args().map(|(src,dst)| Ins::CMP_GT(src,dst)),
						TT::SymHI => data.match_reg_args().map(|(src,dst)| Ins::CMP_HI(src,dst)),
						TT::SymHS => data.match_reg_args().map(|(src,dst)| Ins::CMP_HS(src,dst)),
						TT::SymPL => data.match_reg().map(Ins::CMP_PL),
						TT::SymPZ => data.match_reg().map(Ins::CMP_PZ),
						TT::SymSTR => data.match_reg_args().map(|(src,dst)| Ins::CMP_STR(src,dst)),
						_ => error!(Ins, data, "Comparator type (EQ,GT,GE,HI,HS,PL,PZ,STR)"),
					}
				};
				match func() {
					Ok(ins) => output.add_to_section(skey, ins),
					Err(e) => data.errors.push(e),
				}
			}
			TT::InsDIV0S => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::DIV0S(src,dst)))
				.unwrap_or_default(),
			TT::InsDIV0U => output.add_to_section(skey, Ins::DIV0U),
			TT::InsDIV1 => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::DIV1(src,dst)))
				.unwrap_or_default(),
			TT::InsDMULS => data.dmul_ins(Ins::DMULS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsDMULU => data.dmul_ins(Ins::DMULU)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsDT => data.match_reg().map(Ins::DT)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsEXTS => data.ext_ins(Ins::EXTS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsEXTU => data.ext_ins(Ins::EXTU)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsJMP => {
				// TODO - srenshaw - Add label handling for JMP
				data.match_token(TT::SymAddress).map(|_| ())
					.and_then(|_| data.match_reg())
					.map(Ins::JMP)
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			TT::InsJSR => {
				// TODO - srenshaw - Add label handling for JSR
				data.match_token(TT::SymAddress).map(|_| ())
					.and_then(|_| data.match_reg())
					.map(Ins::JSR)
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			TT::InsLDC => match data.next().get_type() {
				// LDC r#,GBR
				// LDC r#,SR
				// LDC r#,VBR
				TT::IdRegister => data.reg()
					.and_then(|reg| data.match_token(TT::SymComma).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymGBR => Ok(Ins::LDC_GBR(reg)),
						TT::SymSR => Ok(Ins::LDC_SR(reg)),
						TT::SymVBR => Ok(Ins::LDC_VBR(reg)),
						_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
					}),
				// LDC.L @r#+,GBR
				// LDC.L @r#+,SR
				// LDC.L @r#+,VBR
				TT::SymDot => data.match_tokens(&[TT::SymLong,TT::SymAddress])
					.and_then(|_| data.match_reg())
					.and_then(|reg| data.match_tokens(&[TT::SymPlus,TT::SymComma]).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymGBR => Ok(Ins::LDC_GBR_Inc(reg)),
						TT::SymSR => Ok(Ins::LDC_SR_Inc(reg)),
						TT::SymVBR => Ok(Ins::LDC_VBR_Inc(reg)),
						_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
					}),
				_ => error!(Ins, data, "Valid LDC instruction"),
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsLDS => match data.next().get_type() {
				// LDS r#,MACH
				// LDS r#,MACL
				// LDS r#,PR
				TT::IdRegister => data.reg()
					.and_then(|reg| data.match_token(TT::SymComma).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymMACH => Ok(Ins::LDS_MACH(reg)),
						TT::SymMACL => Ok(Ins::LDS_MACL(reg)),
						TT::SymPR => Ok(Ins::LDS_PR(reg)),
						_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
					}),
				// LDS.L @r#+,MACH
				// LDS.L @r#+,MACL
				// LDS.L @r#+,PR
				TT::SymDot => data.match_tokens(&[TT::SymLong,TT::SymAddress])
					.and_then(|_| data.match_reg())
					.and_then(|reg| data.match_tokens(&[TT::SymPlus,TT::SymComma]).map(|_| reg))
					.and_then(|reg| match data.next().get_type() {
						TT::SymMACH => Ok(Ins::LDS_MACH_Inc(reg)),
						TT::SymMACL => Ok(Ins::LDS_MACL_Inc(reg)),
						TT::SymPR => Ok(Ins::LDS_PR_Inc(reg)),
						_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
					}),
				_ => error!(Ins, data, "Valid LDS instruction"),
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsMAC => {
				let ins_func = match data.size() {
					Ok(Size::Byte) => {
						let msg = data.expected("Size specifier Word('w') or Long('l')");
						data.errors.push(msg);
						data.next_line();
						continue;
					}
					Ok(Size::Word) => Ins::MAC_Word,
					Ok(Size::Long) => Ins::MAC_Long,
					Err(_) => continue,
				};
				data.match_token(TT::SymAddress).map(|_| ())
					.and_then(|_| data.match_reg())
					.and_then(|reg| data.match_tokens(&[TT::SymPlus,TT::SymComma,TT::SymAddress]).map(|_| reg))
					.and_then(|reg| data.match_reg().map(|r| (reg,r)))
					.and_then(|pair| data.match_token(TT::SymPlus).map(|_| pair))
					.map(|(src,dst)| ins_func(src,dst))
					.map(|ins| output.add_to_section(skey, ins))
					.unwrap_or_default()
			}
			// MOV #imm,r#
			// MOV r#,r#
			//
			// MOV.B @(disp,GBR),r0
			// MOV.W @(disp,GBR),r0
			// MOV.L @(disp,GBR),r0
			// MOV_FromGBR(Size,i8)
			//
			// MOV.B @(disp,r#),r0
			// MOV.W @(disp,r#),r0
			// MOV.L @(disp,r#),r#
			// MOV_FromDisp(Size,i4,Reg,Reg) // assert!(Dst == r0 || Size == Long)
			//
			// MOV.B @(r0,r#),r#
			// MOV.W @(r0,r#),r#
			// MOV.L @(r0,r#),r#
			// MOV_FromR0(Size,Reg,Reg)
			//
			// MOV.W @(disp,PC),r#
			// MOV.L @(disp,PC),r#
			// MOV_FromPC(Size,i8,Reg) // !Byte
			// MOV_FromLabel(Size,Label,Reg) // !Byte
			//
			// MOV.B @r#+,r#
			// MOV.W @r#+,r#
			// MOV.L @r#+,r#
			// MOV_FromInc(Size,Reg,Reg)
			//
			// MOV.B @r#,r#
			// MOV.W @r#,r#
			// MOV.L @r#,r#
			// MOV_FromReg(Size,Reg,Reg)
			//
			// MOV.B r0,@(disp,GBR)
			// MOV.W r0,@(disp,GBR)
			// MOV.L r0,@(disp,GBR)
			// MOV_ToGBR(Size,i8)
			//
			// MOV.B r0,@(disp,r#)
			// MOV.W r0,@(disp,r#)
			// MOV.L r#,@(disp,r#)
			// MOV_ToDisp(Size,i4,Reg,Reg) // assert!(Src == r0 || Size == Long)
			//
			// MOV.B r#,@(r0,r#)
			// MOV.W r#,@(r0,r#)
			// MOV.L r#,@(r0,r#)
			// MOV_ToR0(Size,Reg,Reg)
			//
			// MOV.B r#,@-r#
			// MOV.W r#,@-r#
			// MOV.L r#,@-r#
			// MOV_ToDec(Size,Reg,Reg)
			//
			// MOV.B r#,@r#
			// MOV.W r#,@r#
			// MOV.L r#,@r#
			// MOV_ToReg(Size,Reg,Reg)
			TT::InsMOV => match data.next().get_type() {
				// MOV Rm,Rn
				TT::IdRegister => data.reg_args()
					.map(|(src,dst)| Ins::MOV_Reg(src,dst)),
				TT::SymDot => || -> Result<Ins, String> {
					let size = data.match_size()?;

					let src = match data.next().get_type() {
						TT::IdLabel => data.curr().get_id()
							.map(Arg::Label)
							.ok_or_else(|| {
								data.next_line();
								data.expected("Label")
							}),
						TT::IdRegister => data.reg().map(Arg::DirReg),
						TT::SymAddress => data.address(),
						_ => error!(Arg, data, "Register, Displacement, or Address"),
					}?;

					data.match_token(TT::SymComma)?;

					let dst = match data.next().get_type() {
						TT::IdRegister => data.reg().map(Arg::DirReg),
						TT::SymAddress => data.address(),
						_ => error!(Arg, data, "Register, Displacement, or Address"),
					}?;

					Ok(Ins::MOV(size,src,dst))
				}(),
				// MOV #imm,Rn
				TT::SymImmediate => data.match_signed(Size::Byte)
					.and_then(|num| data.assert_within_i8(num))
					.and_then(|imm| data.match_token(TT::SymComma).map(|_| imm))
					.and_then(|imm| data.match_reg().map(|r| (imm,r)))
					.map(|(imm,reg)| Ins::MOV_Imm(imm,reg)),
				_ => error!(Ins, data, "size specifier, 8-bit immediate, or Register")
			}.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsMOVA => data.match_tokens(&[TT::SymAddress, TT::SymOParen])
				.and_then(|_| data.match_signed(Size::Byte))
				.and_then(|num| data.assert_within_i8(num))
				.and_then(|imm| data.match_tokens(&[TT::SymComma,TT::SymPC,TT::SymCParen,TT::SymComma]).map(|_| imm))
				.and_then(|imm| data.match_r0().map(|_| imm))
				.map(|num| output.add_to_section(skey, Ins::MOVA(num)))
				.unwrap_or_default(),
			TT::InsMOVT => data.match_reg()
				.map(|dst| output.add_to_section(skey, Ins::MOVT(dst)))
				.unwrap_or_default(),
			TT::InsMUL => data.match_tokens(&[TT::SymDot,TT::SymLong])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MUL(src,dst)))
				.unwrap_or_default(),
			TT::InsMULS => data.match_tokens(&[TT::SymDot,TT::SymWord])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MULS(src,dst)))
				.unwrap_or_default(),
			TT::InsMULU => data.match_tokens(&[TT::SymDot,TT::SymWord])
				.and_then(|_| data.match_reg_args())
				.map(|(src,dst)| output.add_to_section(skey, Ins::MULU(src,dst)))
				.unwrap_or_default(),
			TT::InsNEG => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NEG(src,dst)))
				.unwrap_or_default(),
			TT::InsNEGC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NEGC(src,dst)))
				.unwrap_or_default(),
			TT::InsNOP => output.add_to_section(skey, Ins::NOP),
			TT::InsNOT => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::NOT(src,dst)))
				.unwrap_or_default(),
			TT::InsOR => data.logic_ins(Ins::OR_Imm, Ins::OR_Reg, Ins::OR_Byte, "OR")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsROTCL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTCL(reg)))
				.unwrap_or_default(),
			TT::InsROTCR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTCR(reg)))
				.unwrap_or_default(),
			TT::InsROTL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTL(reg)))
				.unwrap_or_default(),
			TT::InsROTR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::ROTR(reg)))
				.unwrap_or_default(),
			TT::InsRTE => output.add_to_section(skey, Ins::RTE),
			TT::InsRTS => output.add_to_section(skey, Ins::RTS),
			TT::InsSETT => output.add_to_section(skey, Ins::SETT),
			TT::InsSHAL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHAL(reg)))
				.unwrap_or_default(),
			TT::InsSHAR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHAR(reg)))
				.unwrap_or_default(),
			TT::InsSHLL => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL(reg)))
				.unwrap_or_default(),
			TT::InsSHLL2 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL2(reg)))
				.unwrap_or_default(),
			TT::InsSHLL8 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL8(reg)))
				.unwrap_or_default(),
			TT::InsSHLL16 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLL16(reg)))
				.unwrap_or_default(),
			TT::InsSHLR => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR(reg)))
				.unwrap_or_default(),
			TT::InsSHLR2 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR2(reg)))
				.unwrap_or_default(),
			TT::InsSHLR8 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR8(reg)))
				.unwrap_or_default(),
			TT::InsSHLR16 => data.match_reg()
				.map(|reg| output.add_to_section(skey, Ins::SHLR16(reg)))
				.unwrap_or_default(),
			TT::InsSLEEP => output.add_to_section(skey, Ins::SLEEP),
			TT::InsSTC => || -> Result<Ins, String> {
				fn comma_reg(p: &mut Parser) -> Result<Reg, String> {
					p.match_token(TT::SymComma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Result<Reg, String> {
					p.match_tokens(&[TT::SymComma,TT::SymAddress,TT::SymDash])?;
					p.match_reg()
				}
				match data.next().get_type() {
					TT::SymGBR => comma_reg(&mut data).map(Ins::STC_GBR),
					TT::SymSR => comma_reg(&mut data).map(Ins::STC_SR),
					TT::SymVBR => comma_reg(&mut data).map(Ins::STC_VBR),
					TT::SymDot => {
						data.match_token(TT::SymLong)?;
						match data.next().get_type() {
							TT::SymGBR => pre_dec(&mut data).map(Ins::STC_GBR_Dec),
							TT::SymSR => pre_dec(&mut data).map(Ins::STC_SR_Dec),
							TT::SymVBR => pre_dec(&mut data).map(Ins::STC_VBR_Dec),
							_ => error!(Ins, data, "Control Register (GBR,SR,VBR)"),
						}
					}
					_ => error!(Ins, data, "Valid STC instruction"),
				}
			}().map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsSTS => || -> Result<Ins, String> {
				fn comma_reg(p: &mut Parser) -> Result<Reg, String> {
					p.match_token(TT::SymComma)?;
					p.match_reg()
				}
				fn pre_dec(p: &mut Parser) -> Result<Reg, String> {
					p.match_tokens(&[TT::SymComma,TT::SymAddress,TT::SymDash])?;
					p.match_reg()
				}
				match data.next().get_type() {
					TT::SymMACH => comma_reg(&mut data).map(Ins::STS_MACH),
					TT::SymMACL => comma_reg(&mut data).map(Ins::STS_MACL),
					TT::SymPR => comma_reg(&mut data).map(Ins::STS_PR),
					TT::SymDot => {
						data.match_token(TT::SymLong)?;
						match data.next().get_type() {
							TT::SymMACH => pre_dec(&mut data).map(Ins::STS_MACH_Dec),
							TT::SymMACL => pre_dec(&mut data).map(Ins::STS_MACL_Dec),
							TT::SymPR => pre_dec(&mut data).map(Ins::STS_PR_Dec),
							_ => error!(Ins, data, "Special Register (MACH,MACL,PR)"),
						}
					}
					_ => error!(Ins, data, "Valid STS instruction"),
				}
			}().map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::InsSUB => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUB(src,dst)))
				.unwrap_or_default(),
			TT::InsSUBC => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUBC(src,dst)))
				.unwrap_or_default(),
			TT::InsSUBV => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::SUBV(src,dst)))
				.unwrap_or_default(),
			TT::InsSWAP => data.match_token(TT::SymDot).map(|_| ())
				.and_then(|_| data.match_size())
				.and_then(|sz| data.match_reg_args().map(|pair| (sz, pair)))
				.map(|(sz,(src,dst))| Ins::SWAP(sz,src,dst))
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTAS => data.match_tokens(&[TT::SymDot, TT::SymByte, TT::SymAddress])
				.and_then(|_| data.match_reg())
				.map(Ins::TAS)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTRAPA => data.match_token(TT::SymImmediate).map(|_| ())
				.and_then(|_| data.match_signed(Size::Byte))
				.and_then(|num| data.assert_within_u8(num))
				.map(Ins::TRAPA)
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsTST => data.logic_ins(Ins::TST_Imm, Ins::TST_Reg, Ins::TST_Byte, "TST")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsXOR => data.logic_ins(Ins::XOR_Imm, Ins::XOR_Reg, Ins::XOR_Byte, "XOR")
				.map(|ins| output.add_to_section(skey, ins))
				.unwrap_or_default(),
			TT::InsXTRCT => data.match_reg_args()
				.map(|(src,dst)| output.add_to_section(skey, Ins::XTRCT(src,dst)))
				.unwrap_or_default(),
			TT::SymConst => data.size()
				.and_then(|sz| match data.next().get_type() {
					TT::IdNumber => data.signed(sz)
						.and_then(|num| match sz {
							Size::Byte => data.assert_within_i8(num).map(|n| n as i64),
							Size::Word => data.assert_within_i16(num).map(|n| n as i64),
							Size::Long => data.assert_within_i32(num).map(|n| n as i64),
						})
						.map(|imm| Ins::Const_Imm(sz,imm)),
					TT::IdLabel => data.curr().get_id()
						.map(|label| Ins::Const_Label(sz,label))
						.ok_or_else(|| {
							data.next_line();
							data.expected("identifier with label")
						}),
					_ => error!(Ins, data, "integer literal or label"),
				})
				.map(|ins| output.add_to_section(skey, ins)).unwrap_or_default(),
			TT::SymNewline => {} // skip newlines
			TT::SymOrg => data.match_unsigned(Size::Long)
				.map(|addr| skey = addr as u64)
				.unwrap_or_default(),
			_ => {
				let (line,pos) = cur_tok.pos();
				data.errors.push(format!("unexpected {cur_tok} @ [{line}:{pos}]"));
			}
		}
		data.index += 1;
	}

	if data.errors.is_empty() {
		Ok(output)
	} else {
		Err(data.errors)
	}
}

