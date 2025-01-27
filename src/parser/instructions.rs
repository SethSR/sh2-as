use crate::parser::{Arg, Label, Reg, Size};

#[allow(non_camel_case_types)]
//#[allow(upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, Eq)]
//    | Instruction       | Bit Layout       | Operation          | Cycles  | T Bit  |
pub(crate) enum Ins {
	/// | #imm,Rn           | 0111nnnniiiiiiii | Rn+imm -> Rn       | 1       | -      |
	AddImm(i8, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1100 | Rn+Rm -> Rn        | 1       | -      |
	AddReg(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1110 | Rn+Rm+T -> Rn,     | 1       | Carry  |
	/// |                   |                  | Carry -> T         |         |        |
	AddC(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1111 | Rn+Rm -> Rn,       | 1       | Over   |
	/// |                   |                  | Overflow -> T      |         |        |
	AddV(Reg, Reg),
	/// | #imm,R0           | 11001001iiiiiiii | R0 & imm -> R0     | 1       | -      |
	AndImm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1001 | Rn & Rm -> Rn      | 1       | -      |
	AndReg(Reg, Reg),
	/// | #imm,@(R0,GBR)    | 11001101iiiiiiii | (R0+GBR) & imm     | 3       | -      |
	/// |                   |                  | -> (R0+GBR)        |         |        |
	AndByte(u8),
	/// | label             | 10011011dddddddd | if T = 0,          | 3/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 1, nop      |         |        |
	Bf(Label),
	/// | label             | 10001111dddddddd | if T = 0,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 1, nop      |         |        |
	BfS(Label),
	/// | label             | 1010dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	Bra(Label),
	/// | Rm                | 0000mmmm00100011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BraF(Reg),
	/// | label             | 1011dddddddddddd | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | dispx2+PC -> PC    |         |        |
	Bsr(Label),
	/// | Rm                | 0000mmmm00000011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | Rm+PC -> PC        |         |        |
	BsrF(Reg),
	/// | label             | 10001001dddddddd | if T = 1,          | 3/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	Bt(Label),
	/// | label             | 10001101dddddddd | if T = 1,          | 2/1     | -      |
	/// |                   |                  | dispx2+PC -> PC;   |         |        |
	/// |                   |                  | if T = 0, nop      |         |        |
	BtS(Label),
	/// |                   | 0000000000101000 | 0 -> MACH, MACL    | 1       | -      |
	ClrMac,
	/// |                   | 0000000000001000 | 0 -> T             | 1       | 0      |
	ClrT,
	/// | #imm,R0           | 10001000iiiiiiii | if R0 = imm,       | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CmpEqImm(i8),
	/// | Rm,Rn             | 0011nnnnmmmm0000 | if Rn = Rm,        | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CmpEqReg(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0011 | if Rn >= Rm with   | 1       | Result |
	/// |                   |                  | signed data,       |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CmpGE(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0111 | if Rn > Rm with    | 1       | Result |
	/// |                   |                  | signed data,       |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CmpGT(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0110 | if Rn > Rm with    | 1       | Result |
	/// |                   |                  | unsigned data,     |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CmpHI(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0010 | if Rn >= Rm with   | 1       | Result |
	/// |                   |                  | unsigned data,     |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	CmpHS(Reg, Reg),
	/// | Rn                | 0100nnnn00010101 | if Rn > 0,         | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CmpPL(Reg),
	/// | Rn                | 0100nnnn00010001 | if Rn > 0,         | 1       | Result |
	/// |                   |                  | 1 -> T             |         |        |
	CmpPZ(Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1100 | if Rn & Rm have    | 1       | Result |
	/// |                   |                  | an equivalent      |         |        |
	/// |                   |                  | byte, 1 -> T       |         |        |
	CmpStr(Reg, Reg),
	/// | Rm,Rn             | 0010nnnnmmmm0111 | MSB of Rn -> Q,    | 1       | Result |
	/// |                   |                  | MSB of Rm -> M,    |         |        |
	/// |                   |                  | M ^ Q -> T         |         |        |
	Div0S(Reg, Reg),
	/// |                   | 0000000000011001 | 0 -> M/Q/T         | 1       | 0      |
	Div0U,
	/// | Rm,Rn             | 0011nnnnmmmm0100 | Single-step        | 1       | Result |
	/// |                   |                  | division (Rn/Rm)   |         |        |
	Div1(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1101 | Signed operation   | 2 to 4  | -      |
	/// |                   |                  | of Rn x Rm ->      |         |        |
	/// |                   |                  | MACH, MACL         |         |        |
	DMulS(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm0101 | Unsigned operation | 2 to 4  | -      |
	/// |                   |                  | of Rn x Rm -> MACH |         |        |
	/// |                   |                  | MACL               |         |        |
	DMulU(Reg, Reg),
	/// | Rn                | 0100nnnn00010000 | Rn - 1 -> Rn, when | 1       | Result |
	/// |                   |                  | Rn is 0, 1 -> T.   |         |        |
	/// |                   |                  | When Rn is         |         |        |
	/// |                   |                  | nonzero, 0 -> T    |         |        |
	Dt(Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1110 | A byte in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1111 | A word in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	ExtS(Size, Reg, Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1100 | A byte in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1101 | A word in Rm is    | 1       | -      |
	/// |                   |                  | sign-extended ->   |         |        |
	/// |                   |                  | Rn                 |         |        |
	ExtU(Size, Reg, Reg),
	/// | @Rm               | 0100mmmm00101011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | Rm -> PC           |         |        |
	Jmp(Reg),
	/// | @Rm               | 0100mmmm00001011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PC -> PR,          |         |        |
	/// |                   |                  | Rm -> PC           |         |        |
	Jsr(Reg),
	/// | Rm,GBR            | 0100mmmm00011110 | Rm -> GBR          | 1       | -      |
	LdcGBR(Reg),
	/// | Rm,SR             | 0100mmmm00001110 | Rm -> SR           | 1       | LSB    |
	LdcSR(Reg),
	/// | Rm,VBR            | 0100mmmm00101110 | Rm -> VBR          | 1       | -      |
	LdcVBR(Reg),
	/// | @Rm+,GBR          | 0100mmmm00010111 | (Rm) -> GBR,       | 3       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdcGBR_Inc(Reg),
	/// | @Rm+,SR           | 0100mmmm00000111 | (Rm) -> SR,        | 3       | LSB    |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdcSR_Inc(Reg),
	/// | @Rm+,VBR          | 0100mmmm00100111 | (Rm) -> VBR,       | 3       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdcVBR_Inc(Reg),
	/// | Rm,MACH           | 0100mmmm00001010 | Rm -> MACH         | 1       | -      |
	LdsMACH(Reg),
	/// | Rm,MACL           | 0100mmmm00011010 | Rm -> MACL         | 1       | -      |
	LdsMACL(Reg),
	/// | Rm,PR             | 0100mmmm00101010 | Rm -> PR           | 1       | -      |
	LdsPR(Reg),
	/// | @Rm+,MACH         | 0100mmmm00000110 | (Rm) -> MACH,      | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdsMACH_Inc(Reg),
	/// | @Rm+,MACL         | 0100mmmm00010110 | (Rm) -> MACL,      | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdsMACL_Inc(Reg),
	/// | @Rm+,PR           | 0100mmmm00100110 | (Rm) -> PR,        | 1       | -      |
	/// |                   |                  | Rm + 4 -> Rm       |         |        |
	LdsPR_Inc(Reg),
	/// | @Rm+,@Rn+         | 0000nnnnmmmm1111 | Signed operation   | 3/(2-4) | -      |
	/// |                   |                  | of (Rn) x (Rm) +   |         |        |
	/// |                   |                  | MAC -> MAC         |         |        |
	MacLong(Reg, Reg),
	/// | @Rm+,@Rn+         | 0100nnnnmmmm1111 | Signed operation   | 3/(2)   | -      |
	/// |                   |                  | of (Rn) x (Rm) +   |         |        |
	/// |                   |                  | MAC -> MAC         |         |        |
	MacWord(Reg, Reg),
	/// | #imm,Rn           | 1110nnnniiiiiiii | imm -> Sign        | 1       | -      |
	/// |                   |                  | extension -> Rn    |         |        |
	MovImm(i8, Reg),
	/// | Rm,Rn             | 0110nnnnmmmm0011 | Rm -> Rn           | 1       | -      |
	/// |                   |                  |                    |         |        |
	MovReg(Reg, Reg),
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
	Mov(Size, Arg, Arg),
	/// | @(disp,PC),R0     | 11000111dddddddd | disp x 4+PC -> R0  | 1       | -      |
	MovA(i8),
	/// | Rn                | 0000nnnn00101001 | T -> Rn            | 1       | -      |
	MovT(Reg),
	/// | Rm,Rn             | 0000nnnnmmmm0111 | Rn x Rm -> MACL    | 2 to 4  | -      |
	Mul(Reg, Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1111 | Signed operation   | 1 to 3  | -      |
	/// |                   |                  | of Rn x Rm -> MAC  |         |        |
	MulS(Reg, Reg),
	/// | Rm,Rn             | 0010nnnnmmmm1110 | Unsigned operation | 1 to 3  | -      |
	/// |                   |                  | of Rn x Rm -> MAC  |         |        |
	MulU(Reg, Reg),
	/// | Rm,Rn             | 0110nnnnmmmm1011 | 0 - Rm -> Rn       | 1       | -      |
	Neg(Reg, Reg),
	/// | Rm,Rn             | 0110nnnnmmmm1010 | 0 - Rm - T -> Rn,  | 1       | Borrow |
	/// |                   |                  | Borrow -> T        |         |        |
	NegC(Reg, Reg),
	/// |                   | 0000000000001001 | No operation       | 1       | -      |
	Nop,
	/// | Rm,Rn             | 0110nnnnmmmm0111 | ~Rm -> Rn          | 1       | -      |
	Not(Reg, Reg),
	/// | #imm,R0           | 11001011iiiiiiii | R0|imm -> R0       | 1       | -      |
	OrImm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1011 | Rn|Rm -> Rn        | 1       | -      |
	OrReg(Reg, Reg),
	/// | #imm,@(R0,GBR)    | 11001111iiiiiiii | (R0+GBR)|imm ->    | 3       | -      |
	/// |                   |                  | (R0+GBR)           |         |        |
	OrByte(u8),
	/// | Rn                | 0100nnnn00100100 | T <- Rn <- T       | 1       | MSB    |
	RotCL(Reg),
	/// | Rn                | 0100nnnn00100101 | T -> Rn -> T       | 1       | LSB    |
	RotCR(Reg),
	/// | Rn                | 0100nnnn00000100 | T <- Rn <- MSB     | 1       | MSB    |
	RotL(Reg),
	/// | Rn                | 0100nnnn00000101 | LSB -> Rn -> T     | 1       | LSB    |
	RotR(Reg),
	/// |                   | 0000000000101011 | Delayed branch,    | 4       | LSB    |
	/// |                   |                  | stack area -> PC/SR |        |        |
	Rte,
	/// |                   | 0000000000001011 | Delayed branch,    | 2       | -      |
	/// |                   |                  | PR -> PC           |         |        |
	Rts,
	/// |                   | 0000000000011000 | 1 -> T             | 1       | 1      |
	SetT,
	/// | Rn                | 0100nnnn00100000 | T <- Rn <- 0       | 1       | MSB    |
	ShAL(Reg),
	/// | Rn                | 0100nnnn00100001 | MSB -> Rn -> T     | 1       | LSB    |
	ShAR(Reg),
	/// | Rn                | 0100nnnn00000000 | T <- Rn <- 0       | 1       | MSB    |
	ShLL(Reg),
	/// | Rn                | 0100nnnn00001000 | Rn << 2 -> Rn      | 1       | -      |
	ShLL2(Reg),
	/// | Rn                | 0100nnnn00011000 | Rn << 8 -> Rn      | 1       | -      |
	ShLL8(Reg),
	/// | Rn                | 0100nnnn00101000 | Rn << 16 -> Rn     | 1       | -      |
	ShLL16(Reg),
	/// | Rn                | 0100nnnn00000001 | 0 -> Rn -> T       | 1       | LSB    |
	ShLR(Reg),
	/// | Rn                | 0100nnnn00001001 | Rn >> 2 -> Rn      | 1       | -      |
	ShLR2(Reg),
	/// | Rn                | 0100nnnn00011001 | Rn >> 8 -> Rn      | 1       | -      |
	ShLR8(Reg),
	/// | Rn                | 0100nnnn00101001 | Rn >> 16 -> Rn     | 1       | -      |
	ShLR16(Reg),
	/// |                   | 0000000000011011 | Sleep              | 3       | -      |
	Sleep,
	/// | GBR,Rn            | 0000nnnn00010010 | GBR -> Rn          | 1       | -      |
	StcGBR(Reg),
	/// | SR,Rn             | 0000nnnn00000010 | SR -> Rn           | 1       | -      |
	StcSR(Reg),
	/// | VBR,Rn            | 0000nnnn00100010 | VBR -> Rn          | 1       | -      |
	StcVBR(Reg),
	/// | GBR,@-Rn          | 0100nnnn00010011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | GBR -> (Rn)        |         |        |
	StcGBR_Dec(Reg),
	/// | SR,@-Rn           | 0100nnnn00000011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | SR -> (Rn)         |         |        |
	StcSR_Dec(Reg),
	/// | VBR,@-Rn          | 0100nnnn00100011 | Rn - 4 -> Rn,      | 2       | -      |
	/// |                   |                  | VBR -> (Rn)        |         |        |
	StcVBR_Dec(Reg),
	/// | MACH,Rn           | 0000nnnn00001010 | MACH -> Rn         | 1       | -      |
	StsMACH(Reg),
	/// | MACL,Rn           | 0000nnnn00011010 | MACL -> Rn         | 1       | -      |
	StsMACL(Reg),
	/// | PR,Rn             | 0000nnnn00101010 | PR -> Rn           | 1       | -      |
	StsPR(Reg),
	/// | MACH,@-Rn         | 0100nnnn00000010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | MACH -> (Rn)       |         |        |
	StsMACH_Dec(Reg),
	/// | MACL,@-Rn         | 0100nnnn00010010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | MACL -> (Rn)       |         |        |
	StsMACL_Dec(Reg),
	/// | PR,@-Rn           | 0100nnnn00100010 | Rn - 4 -> Rn,      | 1       | -      |
	/// |                   |                  | PR -> (Rn)         |         |        |
	StsPR_Dec(Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1000 | Rn - Rm -> Rn      | 1       | -      |
	Sub(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1010 | Rn - Rm - T -> Rn, | 1       | Borrow |
	/// |                   |                  | Borrow -> T        |         |        |
	SubC(Reg, Reg),
	/// | Rm,Rn             | 0011nnnnmmmm1011 | Rn - Rm -> Rn,     | 1       | Under  |
	/// |                   |                  | Underflow -> T     |         |        |
	SubV(Reg, Reg),
	/// | .B Rm,Rn          | 0110nnnnmmmm1000 | Rm -> Swap upper   | 1       | -      |
	/// |                   |                  | and lower 2 bytes  |         |        |
	/// |                   |                  | -> Rn              |         |        |
	/// | .W Rm,Rn          | 0110nnnnmmmm1001 | Rm -> Swap upper   | 1       | -      |
	/// |                   |                  | and lower word ->  |         |        |
	/// |                   |                  | Rn                 |         |        |
	Swap(Size, Reg, Reg),
	/// | @Rn               | 0100nnnn00011011 | if (Rn) is 0, 1->T | 4       | Result |
	/// |                   |                  | 1 -> MSB of (Rn)   |         |        |
	Tas(Reg),
	/// | #imm              | 11000011iiiiiiii | PC/SR -> stack     | 8       | -      |
	/// |                   |                  | area, (imm x 4 +   |         |        |
	/// |                   |                  | VBR) -> PC         |         |        |
	TrapA(u8),
	/// | #imm,R0           | 11001000iiiiiiii | R0 & imm; if the   | 1       | Result |
	/// |                   |                  | result is 0, 1->T  |         |        |
	Tst_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1000 | Rn & Rm; if the    | 1       | Result |
	/// |                   |                  | result is 0, 1->T  |         |        |
	Tst_Reg(Reg, Reg),
	/// | #imm,@(R0,GBR)    | 11001100iiiiiiii | (R0+GBR) & imm; if | 3       | Result |
	/// |                   |                  | the result is 0,   |         |        |
	/// |                   |                  | 1 -> T             |         |        |
	Tst_Byte(u8),
	/// | #imm,R0           | 11001010iiiiiiii | R0 ^ imm -> R0     | 1       | -      |
	Xor_Imm(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1010 | Rn ^ Rm -> Rn      | 1       | -      |
	Xor_Reg(Reg, Reg),
	/// | #imm,@(R0,GBR)    | 11001110iiiiiiii | (R0+GBR) ^ imm ->  | 3       | -      |
	/// |                   |                  | (R0+GBR)           |         |        |
	Xor_Byte(u8),
	/// | Rm,Rn             | 0010nnnnmmmm1101 | Center 32 bits of  | 1       | -      |
	/// |                   |                  | Rm and Rn -> Rn    |         |        |
	Xtrct(Reg, Reg),

	/*** Directives ***/
	Const_Imm(Size, i64),
	Const_Label(Size, Label),
	Label(Label),
}
