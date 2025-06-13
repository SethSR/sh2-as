
	align 4

	include "Monitor.asm"
	include "PrintChar.asm"
	
WaitVblank:
	mov.l VDP2_Reg_TVSTAT,r11	;$25f80004 TVSTAT - Screen Status 
VblankWait1:
	mov.w @r11,r0
	tst	#8,r0			;%------LS----VHOP V=Vblank H=Hblank O=Odd field P=Pal
	bf	VblankWait1		;V=1=In Vblank
VblankWait2:
	mov.w @r11,r0
	tst	#8,r0			;%------LS----VHOP V=Vblank H=Hblank O=Odd field P=Pal
	bt	VblankWait2		;V=1=In Vblank
	rts
	
PageFlip:
	rts
	

	align 4
	
	ltorg
			
Palette:
		 ;-BBBBBGGGGGRRRRR
	dc.w %0011110000000000
	dc.w %0000001111111111
	dc.w %0111111111100000
	dc.w %0000000000011111
	align 4	

VDP2_Reg_TVSTAT:	dc.l $25f80004
	
AdrUserRam: 	dc.l UserRam
AdrFont:		dc.l Font


;LineSkip:	dc.l 192		;Vram is actually 512x256
OneLineSize:	dc.l 512
;LineCount:		dc.l 224
;LineStartAddr:	dc.l $100


	
AdrScreenbuffer:	dc.l $25e00000 ;Screenbuffer
VDP2_VRAM:	dc.l $25e00000

		
	
UserRam_VdpStat = 0	
UserRam_CursorX = 4
UserRam_CursorY = 8
	
UserRam:
	dc.l 0	;VDP Stat cache
	dc.l 0	;Xpos
	dc.l 0	;Ypos
	dc.l 0
	dc.l 0
	dc.l 0
	
Font:							;1bpp font - 8x8 96 characters 
	binclude "Font96.FNT"
