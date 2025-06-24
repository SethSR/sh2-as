	macro PushAll
	mov.l r0,@-sp
	mov.l r1,@-sp
	mov.l r2,@-sp
	mov.l r3,@-sp
	mov.l r4,@-sp
	mov.l r5,@-sp
	mov.l r6,@-sp
	mov.l r7,@-sp
	mov.l r8,@-sp
	mov.l r9,@-sp
	mov.l r10,@-sp
	mov.l r11,@-sp
	mov.l r12,@-sp
	mov.l r13,@-sp
	mov.l r14,@-sp
	sts.l pr,@-sp	
	endm

	macro PopAll
	lds.l @sp+,pr
	mov.l @sp+,r14
	mov.l @sp+,r13
	mov.l @sp+,r12
	mov.l @sp+,r11
	mov.l @sp+,r10
	mov.l @sp+,r9
	mov.l @sp+,r8
	mov.l @sp+,r7
	mov.l @sp+,r6
	mov.l @sp+,r5
	mov.l @sp+,r4
	mov.l @sp+,r3
	mov.l @sp+,r2
	mov.l @sp+,r1
	mov.l @sp+,r0
	endm
