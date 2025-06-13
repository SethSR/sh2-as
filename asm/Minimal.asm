
	include "BasicMacros.asm"
	include "BasicHeader.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov.l AdrHelloWorld,r13
	bsr PrintString			;Show string R13
	nop

	bsr NewLine				;Down a line
	nop

	mov #64,r2				;Bytecount
	mov #AdrHelloWorld,r1	;SourceAddr

	bsr Monitor				;Show Registers
	nop

	bsr MemDump				;Dump R2 bytes from addr R1 to screen
	nop

	bsr PageFlip			;Flip Buffer (32x)
	nop

InfLoop:

	bra	InfLoop				;Branch to INFLOOP

	nop						;NOP in the BRA delay slot

	align 4
AdrHelloWorld: dc.l TxtHelloWorld
TxtHelloWorld: dc.b "Hello World 12345 !?",255
	align 4
	ltorg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	align 4
	include "BasicFooter.asm"

