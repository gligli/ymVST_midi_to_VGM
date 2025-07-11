	SECTION TEXT

	bra.w	sndh_init
	bra.w	sndh_exit
	bra.w	sndh_vbl
	
	dc.b	'SNDH'

	rorg	$100
	
saved_regs:
	ds.b	16
track_offset:
	dc.l	0

	
sndh_init:
	movem.l a0/a3-a4/d7,-(sp)

	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4

	lea	(saved_regs),a0
	moveq	#15,d7
save_regs_lp:
		move.b	d7,(a3)
		move.b	(a4),0(a0,d7.w)
		dbra.w	d7,save_regs_lp
	
	move.b	#15,(a3)
	move.b	#1,(a4)

	movem.l  (sp)+,a0/a3-a4/d7
	rts
	
sndh_exit:
	movem.l a0/a3-a4/d7,-(sp)

	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4

	lea	(saved_regs),a0
	moveq	#15,d7
restore_regs_lp:
		move.b	d7,(a3)
		move.b	0(a0,d7.w),(a4)
		dbra.w	d7,restore_regs_lp
	
	movem.l  (sp)+,a0/a3-a4/d7
	rts	
	
sndh_vbl:
	movem.l a3-a6/d0,-(sp)
	
	lea	$ffff8800.w,a3
	lea	$ffff8802.w,a4
	
	move.b	#15,(a3)
	subq.b	#1,(a4)
	bne.s	just_wait	

	lea	(track_data),a6
	move.l	(track_offset),d0
	lea	0(a6,d0.l),a5
	
	move.b	(a5),-(sp)
	move.w	(sp)+,d0
	move.b	1(a5),d0
	
	addq.l	#1,a5
	moveq	#-1,d0

	reg_lp:
		addq.w	#1,d0
		
		add.w	d0,d0
		bge.s	out_of_regs
		bcc.s	reg_lp
		
		move.b	d0,(a3)
		move.b	(a5)+,(a4)
		
		bra.s	reg_lp
	
out_of_regs:
	suba.l	a6,a5
	move.l	a5,(track_offset)

just_wait:
	movem.l (sp)+,a3-a6/d0
	rts
	
track_data:
	