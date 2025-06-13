
Monitor:
	pushall
		mov sp,r3
		stc sr,r6
		add #60,r3	;Point to first pushed reg
		mov #0,r1 	;Reg label

		mov #3,r5	;Reg per line

		mov #15,r4	;Reg count
MonitorAgain:
		mov.l @r3,r2	;Get reg
		add #-4,r3		;Move to next reg

		bsr ShowReg
		nop

		add #1,r1		;Reg label

		dt r5
		bf MonitorNoNL

		mov #3,r5	;Reg per line
		bsr NewLine
		nop
MonitorNoNL:
		dt r4
		bf MonitorAgain

		mov sp,r2
		mov #'S',r0
		mov #'P',r1
		add #64,r2		;Correct SP
		bsr ShowRegSpec
		nop

		mov.l @r3,r2		;Return addr
		mov #'P',r0
		mov #'C',r1
		bsr ShowRegSpec
		nop

		mov r6,r2 ;stc SR,r2
		mov #'S',r0
		mov #'R',r1
		bsr ShowRegSpec
		nop

		bsr NewLine
		nop

		stc gbr,r2
		mov #'G',r0
		mov #'B',r1
		bsr ShowRegSpec
		nop

		stc vbr,r2
		mov #'V',r0
		mov #'B',r1
		bsr ShowRegSpec
		nop

		bsr NewLine
		nop

		sts MACH,r2
		mov #'M',r0
		mov #'H',r1
		bsr ShowRegSpec
		nop

		sts MACL,r2
		mov #'M',r0
		mov #'L',r1
		bsr ShowRegSpec
		nop

		bsr NewLine
		nop
	popall
	rts
	nop

ShowRegSpec:	;R0R1:R2
	sts.l pr,@-sp
		bsr PrintChar
		nop

		mov r1,r0
		bsr PrintChar
		nop

		bra ShowRegB
		nop

ShowReg:	;R1:R2
	sts.l pr,@-sp
		mov #'R',r0
		bsr PrintChar
		nop

		mov r1,r0
		bsr ShowHexChar
		nop
ShowRegB:
		mov #':',r0
		bsr PrintChar
		nop

		mov r2,r0
		bsr ShowHex32
		nop

		mov #' ',r0
		bsr PrintChar
		nop

	lds.l @sp+,pr
	rts
	nop

ShowHex32:			;Show R0
	sts.l pr,@-sp
	mov.l r0,@-sp
	mov.l r1,@-sp
	mov.l r2,@-sp
		mov #8,r2
ShowHexAgain:
		rotl r0				;Move top nibble to bottom
		rotl r0
		rotl r0
		rotl r0

		bsr ShowHexChar
		nop

		dt r2
		bf ShowHexAgain

	mov.l @sp+,r2
	mov.l @sp+,r1
	mov.l @sp+,r0
	lds.l @sp+,pr
	rts
	nop

ShowHexChar:		;Show R0
	sts.l pr,@-sp
		mov.l r0,@-sp
		mov.l r1,@-sp
			and #$0f,r0
			add #'0',r0
			mov #'9',r1				;Char valid?
			cmp/ge r0,r1
			bt ShowHexDigit
			add #7,r0				;Fix A-F
ShowHexDigit:
			bsr PrintChar
			nop
		mov.l @sp+,r1
		mov.l @sp+,r0
	lds.l @sp+,pr
	rts
	nop

MemDump:
	pushall
		mov r1,r0
		bsr ShowHex32
		nop

		mov #':',r0
		bsr PrintChar
		nop

		bsr NewLine
		nop
MemDumpNextLine:
		mov #8,r3			;8 bytes per line
MemDumpByteAgain:
		mov.b @r1+,r0
		bsr ShowHex8
		nop

		mov #' ',r0
		bsr PrintChar
		nop

		dt r3				;Decrement and Test
		bf MemDumpByteAgain ;Branch if False

		mov #8,r3			;8 bytes per line
		sub r3,r1			;Back 8 chars
MemDumpCharAgain:
		mov.b @r1+,r0

		mov #32,r4
		CMP/GE r4,r0
		bt MemDumpCharOK1
		mov #'.',r0
MemDumpCharOK1:

		mov #127,r4
		CMP/GE r4,r0
		bf MemDumpCharOK2
		mov #'.',r0
MemDumpCharOK2:

		bsr PrintChar
		nop

		dt r3				;Decrement and Test
		bf MemDumpCharAgain ;Branch if False

		bsr NewLine
		nop

		add #-8,r2

		mov #0,r3
		CMP/EQ r3,r2
		bf MemDumpNextLine

	popall 
	rts
	nop

ShowHex8:
	sts.l pr,@-sp		;Back up PR (used by BSR/RTS)
	mov.l r1,@-sp
	mov.l r2,@-sp

		mov r0,r1
		rotr r0				;Move top nibble to bottom
		rotr r0
		rotr r0
		rotr r0
		bsr ShowHexChar
		nop

		mov r1,r0
		bsr ShowHexChar
		nop

	mov.l @sp+,r2
	mov.l @sp+,r1
	lds.l @sp+,pr		;Restore PR (used by BSR/RTS)
	rts 				;Return to the caller
	nop

