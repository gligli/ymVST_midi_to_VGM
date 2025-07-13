	SECTION TEXT

	bra.w	sndh_init
	bra.w	sndh_exit
	bra.w	sndh_tick
	
	dc.b	'SNDH'

	rorg	$100
	
saved_regs:
	ds.b	16
track_ptr:
	dc.l	$deadbeef
waits_left
	dc.b	1
waits:	
	dc.b	1
	
sndh_init:
	movem.l a0/a3-a4/d7,-(sp)

	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4

	lea	saved_regs(PC),a0
	moveq	#15,d7
save_regs_lp:
		move.b	d7,(a3)
		move.b	(a4),0(a0,d7.w)
		dbra.w	d7,save_regs_lp

	; init state
	lea	track_data(PC),a3
	move.l	a3,16(a0)
	move.w	#$0101,20(a0)
	
	movem.l  (sp)+,a0/a3-a4/d7
	rts
	
sndh_exit:
	movem.l a0/a3-a4/d7,-(sp)

	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4

	lea	saved_regs(PC),a0
	moveq	#15,d7
restore_regs_lp:
		move.b	d7,(a3)
		move.b	0(a0,d7.w),(a4)
		dbra.w	d7,restore_regs_lp
	
	movem.l  (sp)+,a0/a3-a4/d7
	rts	
	
sndh_tick:
	movem.l a2-a5/d0-d1,-(sp)
	
	lea	track_ptr(PC),a5

	tst.b	4(a5)
	beq.s	just_wait	
	subq.b	#1,4(a5)
	bne.s	just_wait	

	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4

	; track_ptr (current regs addr) in a2
	move.l	(a5),a2
	
	; load unaligned word indicator (reg present?)
	move.b	(a2),-(sp)
	move.w	(sp)+,d0
	move.b	1(a2),d0
	
	addq.l	#2,a2
	moveq	#-1,d1

	; loop on present regs
	reg_lp:
		addq.w	#1,d1
		
		add.w	d0,d0
		bcc.s	reg_lp
		
		move.b	d1,(a3)
		move.b	(a2)+,(a4)

		tst.w	d0
		bne.s	reg_lp
	
	; update waits_left (in timer ticks, "virtually" in YM reg 15)
	cmp.b	#15,d1
	bne.s	no_new_waits_left
		move.b	-1(a2),5(a5)
no_new_waits_left:
	move.b	5(a5),4(a5)
	
	; update track_ptr
	move.l	a2,(a5)

just_wait:
	movem.l (sp)+,a2-a5/d0-d1
	rts
	
track_data:
	