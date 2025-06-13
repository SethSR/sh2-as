
PrintString:
	sts.l pr,@-sp
PrintStringAgain:
		mov.b @r13+,r0
		mov #-1,r1				;Compare to 255
		cmp/eq r0,r1

		bt PrintStringDone
		nop

		bsr PrintChar
		nop

		bra PrintStringAgain
		nop
PrintStringDone:
	lds.l @sp+,pr
	rts
	nop

PrintChar:	;PrintChar R0
	sts.l mach,@-sp
	sts.l macl,@-sp
	PushAll

		mov.l AdrUserRam,r14		;R14 points to our work ram

		mov.l AdrScreenbuffer,r6		;Screen VRAM

		mov.l AdrFont,r5

		add #-32,r0		;No Char below space

		shll r0
		shll r0
		shll r0		;8 bytes per char
		add r0,r5

		mov.l @(UserRam_CursorX,r14),r0

		shll r0
		shll r0
		shll r0		;8 bytes per Horiz char
		add r0,r6

		mov.l @(UserRam_CursorY,r14),r0

		shll r0
		shll r0
		shll r0		;8 lines per vert char

		mov.l OneLineSize,r1	;320 bytes per line
		mulu r0,r1

		sts macl,r0		;Get L result of multiplication

		add r0,r6		;Add Y offset

		mov #7,r1
		add r1,r6		;Move 7 pixels right (work right->left)

		mov #8,r1		;Lines

	NextYLine:
		mov.l r6,@-sp
			mov #8,r0		;Bits per line

			mov.b @r5+,r3	;Get a byte
	NextXPixel:
			mov #1,r2		;Fill pixel

			rotcr r3	;Get a bit
			bt CharPixelSet
			mov #0,r2		;Clear pixel
	CharPixelSet:
			mov.b r2,@r6	;Write pixel

			add #-1,r6		;Left one pixel

			dt r0
			bf NextXPixel

		mov.l @sp+,r6
		mov.l OneLineSize,r0
		add r0,r6		;Down a line

		dt r1
		bf NextYLine

		mov.l @(UserRam_CursorX,r14),r0
		add #1,r0
		mov.l r0,@(UserRam_CursorX,r14)
	PopAll
	lds.l @sp+,macl
	lds.l @sp+,mach
	rts
	nop

NewLine:
	mov.l r0,@-sp
	mov.l r14,@-sp

		mov.l AdrUserRam,r14		;R14 points to our work ram
		mov #0,r0
		mov.l r0,@(UserRam_CursorX,r14)

		mov.l @(UserRam_CursorY,r14),r0
		add #1,r0
		mov.l r0,@(UserRam_CursorY,r14)
	mov.l @sp+,r14
	mov.l @sp+,r0
	rts
	nop

