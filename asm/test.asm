
	.org $06004000  ;Base address for a BIN file

;Set up the VDP2 registers
	mov.l #VDP2_Reg_Base,r11
	add #$E,r11
	mov.w #RAM_n_Screen_Config,r6 ;Set RAM partitioning / screen mode
	mov.w r6,@r11  ;$25F8_000E RAMCTL - RAM Control Register

	add #$2E,r11
	mov.w #Map_Offset_Register,r2
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
	mov.w #Character_CTRL,r2 ;nn=bmp size(512x256/512x512/1024x256/1024x512)
	mov.w r2,@r11  ;$25F8_0028 CHTCLA - Character ctrl (NBG0, NBG1)

	add #-8,r11
	mov.w #Screen_Display_Enable,r8
	mov.w r8,@r11  ;$25F8_0020 BGON - Screen Display Enable

	add #-32,r11
	mov.l #TV_Mode,r10 ;L=interlace V=Vres H=Hres
	mov.w r10,@r11 ;$25F8_0000 TVMD - TV Mode

;Set Palette
	mov.l #VDP2_CRAM_Base,r6
	mov.l #Palette,r8    ;Palette
	mov.w #PaletteEntries,r0          ;PaletteEntries
PaletteLoop:
	mov.w @r8+,r1       ;-BBBBBGGGGGRRRRR
	mov.w r1,@r6        ;Set a color
	add #2,r6

	dt r0               ;Dec R0
	bf PaletteLoop

	;TODO - srenshaw - Jump to the actual start of the program


VDP2_CRAM_Base:
	.dc.l $25F0_0000

VDP2_Reg_Base:
	.dc.l $25F8_0000

RAM_n_Screen_Config:
	     ;%C-cc--VVRRRRRRRR     cc=0=5bit RGB
	.dc.w %1000001111111111

Map_Offset_Register:
	     ;%-NNN-nnn-NNN-nnn
	.dc.w %0000000000000000

Character_CTRL:
	     ;%--CCNNES-cccnnes     c=colors (16/256/2k/32k/16m) e=1=bmp format
	.dc.w %0000000000010010

Screen_Display_Enable:
	     ;%---RNNNN--rrnnnn RN=Transparency / rn=enabled
	.dc.w %0000000100000001

TV_Mode:
	     ;%D------BLLVV-HHH      D=1=Screen on B=1=Blackout
	.dc.w %1000000000000000

PaletteEntries:
	.dc.w 4

Palette:  ;-BBBBBGGGGGRRRRR
	.dc.w   %0011110000000000   ;Our 4 colors!
	.dc.w   %0000001111111111
	.dc.w   %0111111111100000
	.dc.w   %0000000000011111

ShowSpriteXY:    ;R11=Width R10=Height
	mov.l #UserRam,r9      ;R14 points to our work RAM

	mov.l #$25E0_0000,r12  ;Cache through Addr (Data)

	mov.l @(UserRam_PlayerX,r9),r0 ;+Xpos
	add r0,r12

	mov.l @(UserRam_PlayerY,r9),r0 ;+Ypos*512
	mov.l #512,r1    ;512 bytes per line
	mulu r0,r1
	sts macl,r0     ;Get L result of multiplication
	add r0,r12


ShowSprite:     ;Show sprite @R13 to @R12
	              ;R11=Width R10=Height

	mov.l #512,r3         ;Line Width in bytes
ShowSpriteY:
	mov r11,r1
	mov r12,r2
ShowSpriteX:
		mov.l @r13+,r0     ;Get a source word
		mov.l @r12,r4      ;Get screen word
		xor r4,r0          ;XOR with current screen data
		mov.l r0,@r12      ;Store back to screen
		add #4,r12          ;Across screen 4 pixels

		add #-3,r1          ;We did 4 pixels
		dt r1              ;We did 4 pixels
		bf ShowSpriteX

	mov r2,r12    ;Reset Ypos
	add r3,r12    ;Down a line

	dt r10        ;R10=Height
	bf ShowSpriteY
	rts
	nop

UserRam_VdpStat = 0
UserRam_PlayerX = 12
UserRam_PlayerY = 16

UserRam:
	.dc.l 0   ;VDP Stat cache
	.dc.l 0   ;Cursor Xpos
	.dc.l 0   ;Cursor Ypos
	.dc.l 0   ;Player X
	.dc.l 0   ;Player Y
	.dc.l 0

ChibikoBmp:     ;1 byte per pixel - same as DOS VGA format
	;TODO - srenshaw - Will need to implement this at some point.
	.binclude "ResAll/Sprites/SpriteTestVGA.RAW"


GetJoy:
	sts.l pr,@-sp

;Set up the ports
		mov #%0000_0000,r0   ;All ports to input
		mov #$2010_0079,r1   ;DDR1 Data Dir %-DDDDDDD 1=output 0=input
		mov.b r0,@r1

		mov #0,r0           ;0=SMPC / 1=SH2 direct
		mov #$2010_007D,r1   ;% - - - - - - IOSEL2 IOSEL1
		mov.b r0,@r1

		mov #0,r0           ;1=VDP Latch
		mov #$2010_007F,r1   ;% - - - - - - EXLE2 EXLE1
		mov.b r0,@r1

;Send our command
		mov.b #1,r0         ;Flag busy before our command
		mov #$2010_0063,r13 ;Status flag
		mov.b r0,@r13

		mov #$2010_0001,r1  ;SMPC Status Acquisition Switch
		mov #$00,r0         ;1=Get extra info (time reset etc.)
		mov.b r0,@r1

		mov #$2010_0003,r1
		   ;BBbbP-O-    bb/BB=15/255/?/0 bytes.. p=peripheral data
		mov #%0000_1000,r0  ;0=Acquisition Time Optimization
		mov.b r0,@r1       ;GetPeripheral data 15 byte

		mov #$2010_0005,r1
		mov #$F0,r0         ;Fixed value
		mov.b r0,@r1

		mov #$2010_001F,r1  ;command
		mov #$10,r0         ;INTBACK - Interrupt Back
		mov.b r0,@r1       ; (SMPC Status Acquisition)

;Wait for results

GetJoyWait:
		mov #$2010_0063,r13 ;Status flag
		mov.b @r13,r0
		and #1,r0           ;Only bottom bit is defined
		cmp/eq #1,r0        ;Wait for command completion
		bt GetJoyWait


		mov #$2010_0020,r13

		mov.b @(5,r13),r0  ;%RLDUSABC
		shlr2 r0
		shlr2 r0           ;Get %----RLDU
		and #%0000_1111,r0
		mov r0,r1

		mov.b @(5,r13),r0  ;%RLDUSABC
		shll2 r0
		shll2 r0           ;Get %SABC----
		and #%1111_0000,r0
		or r1,r0
		mov r0,r1          ;Now we have %SABCRLDU

		mov.b @(7,r13),r0  ;%rXYZl---
		shll2 r0
		shll2 r0
		shll r0            ;Get %---rXYZl --------
		mov #%00011111_00000000,r2
		and r2,r0
		or r1,r0
		mov r0,r1          ;Now we have %rXYZlSABCRLDU

		mov #$FFFFE000,r2   ;Fill unused bits
		or r2,r0

	lds.l @sp+,pr        ;Joy state in r0
	rts
	nop
