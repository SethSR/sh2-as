
	org $6004000 		;Base address for a BIN
	
;Set up the VDP2 registers
	mov.l VDP2_Regs,r11
	mov.l ScreenMode,r6
	add #$E,r11				;$25f8000E RAMCTL - Ram Control Register
	mov.w	r6,@r11			;Set Screen Mode

	
	add #$2e,r11			;$25f8003C MPOFN - Map Offset Register
	mov	#0,r2
	mov.w r2,@r11
	
	add #$3c,r11	;$25f80078 - ZMXIN0 Co-ord Increment register
	mov.w r2,@-r11	;$25f80076 - SCYDN0 Screen Scroll Value (Vert Fraction)
	mov.w r2,@-r11	;$25f80074 - SCYIN0 Screen Scroll Value (Vert Integer)
	mov.w r2,@-r11	;$25f80072 - SCXDN0 Screen Scroll Value (Horiz Fraction)
	mov.w r2,@-r11	;$25f80070 - SCXIN0 Screen Scroll Value (Horiz Integer)
	
	add #-72,r11	;$25f80028 CHCTLA - Character control (NBG0, NBG1)
	mov	#$12,r2
	mov.w r2,@r11
	
	
	add	#-8,r11		;$25f80020 BGON - Screen Display Enable
	mov	#$101,r8
	mov.w r8,@r11
	
	
	add	#-32,r11	;$25f80020 TVMD - TV Mode
	mov.w bit15,r10	;DISP bit=1
	mov.w r10,@r11
	
	


;Set Palette		
	mov.l VDP2_CRAM,r6		;
	mov.l AdrPalette,r8		;Palette
	mov	#4,r0				;PaletteEntries
	
PaletteLoop:
	mov.w @r8+,r1
	mov.w r1,@r6
	add	#2,r6
	
	dt r0		;Dec R0
	bf PaletteLoop 
	
	mov.l #UserRam,r14

	mov #0,r0
	mov.l r0,@(UserRam_CursorX,r14)
	mov.l r0,@(UserRam_CursorY,r14)

	bra ProgramStart
	
	align 4
bit15:		dc.w -32768
deltaainc:	dc.w 258
AdrPalette:	dc.l Palette

VDP2_Regs:	dc.l $25f80000
VDP2_CRAM:	dc.l $25F00000

ScreenMode:	dc.l %1100111111111111	;RGB 5 bits

	ltorg
	
	align 4
ProgramStart:
