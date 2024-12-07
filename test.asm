
	org $06004000  ;Base address for a BIN file

;Set up the VDP2 registers
	mov.l #$25F8_0000,r11   ;VDP2_Regs
	add #$E,r11
	      ;%C-cc--VVRRRRRRRR     cc=0=5bit RGB
	mov.w #%1000001111111111,r6 ;Set RAM partitioning / screen mode
	mov.w r6,@r11  ;$25F8_000E RAMCTL - RAM Control Register

	add #$2E,r11
	      ;%-NNN-nnn-NNN-nnn
	mov.w #%0000000000000000,r2
	mov.w r2,@r11  ;$25F8_003C MPOFN - Map Offset Register

	add #$3C,r11
	      ;%NNNNNNNN--------
	mov.w r2,@-r11 ;$25F8_0076 SCYDNO - Scr Scroll (Vert Fraction)
	      ;%-----NNNNNNNNNNN
	mov.w r2,@-r11 ;$25F8_0074 SCYINO - Scr Scroll (Vert Integer)
	      ;%NNNNNNNN--------
	mov.w r2,@-r11 ;$25F8_0072 SCXDNO - Scr Scroll (Horz Fraction)
	      ;%-----NNNNNNNNNNN
	mov.w r2,@-r11 ;$25F8_0070 SCXINO - Scr Scroll (Horz Integer)

	add #-72,r11
	      ;%--CCNNES-cccnnes     c=colors (16/256/2k/32k/16m) e=1=bmp format
	mov.w #%0000000000010010,r2 ;nn=bmp size(512x256/512x512/1024x256/1024x512)
	mov.w r2,@r11  ;$25F8_0028 CHTCLA - Character ctrl (NBG0, NBG1)

	add #-8,r11
	      ;%---RNNNN--rrnnnn RN=Transparency / rn=enabled
	mov.w #%0000000100000001,r8
	mov.w r8,@r11  ;$25F8_0020 BGON - Screen Display Enable

	add #-32,r11
	      ;%D------BLLVV-HHH      D=1=Screen on B=1=Blackout
	mov.l #%1000000000000000,r10 ;L=interlace V=Vres H=Hres
	mov.w r10,@r11 ;$25F8_0000 TVMD - TV Mode

;Set Palette
	mov.l #$25F0_0000,r6   ;VDP2_CRAM
	mov.l #Palette,r8      ;Palette
	mov.w #4,r0            ;PaletteEntries
PaletteLoop:
	mov.w @r8+,r1          ;-BBBBBGGGGGRRRRR
	mov.w r1,@r6           ;Set a color
	add #2,r6

	dt r0                  ;Dec R0
	bf PaletteLoop


Palette: ;-BBBBBGGGGGRRRRR
	dc.w   %0011110000000000   ;Our 4 colors!
	dc.w   %0000001111111111
	dc.w   %0111111111100000
	dc.w   %0000000000011111
