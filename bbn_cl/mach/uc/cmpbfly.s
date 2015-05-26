#                                 ********
#
# Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
# Beranek and Newman Inc.
#
# Permission to use, copy, modify and distribute this software and its
# documentation is hereby granted without fee, provided that the above
# copyright notice and this permission appear in all copies and in
# supporting documentation, and that the name Bolt, Beranek and Newman
# Inc. not be used in advertising or publicity pertaining to distribution
# of the software without specific, written prior permission.  In
# addition, BBN makes no respresentation about the suitability of this
# software for any purposes.  It is provided "AS IS" without express or
# implied warranties including (but not limited to) all implied warranties
# of merchantability and fitness.  In no event shall BBN be liable for any
# special, indirect or consequential damages whatsoever resulting from
# loss of use, data or profits, whether in an action of contract,
# negligence or other tortuous action, arising out of or in connection
# with the use or performance of this software.
# 
#                                 ********
# 
##	This code provides runtime support for compiled
#	code on the Butterfly

	global	_Registers
	global	_Ext_Stack_Pointer
	global	_Free
	global	Btran
	global	bxfers
	global	return_to_interpreter
	global	c_save_stack
	global	_Who_Am_I
	global	_Current_State_Point
	global	_Fluid_Bindings
	global	_Fixed_Objects
	global	_Prim_Await_Future
	global	_Propagate_Variable_Cache

	set	tc_null,0x00
	set	tc_return_code,0x1b
	set	tc_future,0x2e
	set	tc_return_address,0x39
	set	tc_fixnum,0x1a
	set	tc_vector,0x0a
	set	tc_pair,0x01

	set	pc_vector_ref,0x2e

	set	prim_await_future,-8

	set	future_is_determined,4
	set	future_lock,8
	set	future_value,12

	set	rc_compiler_future_restart,0x51

	set	apply_primitive,0x138

	set	regblock_value,8
	set	regblock_expr,20
	set	regblock_base,1600

	set	System_Scheduler,0x15+0x15+0x15+0x15
	
	set	regblock_bfly_pnc_load,regblock_base
	set	regblock_bfly_pnc_store,regblock_base+6
	set	regblock_bfly_pnc_copy,regblock_base+12
	set	regblock_bfly_pnc_push,regblock_base+18
	set	regblock_bfly_touch_op,regblock_base+24

	set	regblock_bfly_one_plus,regblock_base+30
	set	regblock_bfly_one_minus,regblock_base+36
	set	regblock_bfly_plus,regblock_base+42
	set	regblock_bfly_minus,regblock_base+48
	set	regblock_bfly_eql,regblock_base+54
	set	regblock_bfly_lt,regblock_base+60
	set	regblock_bfly_gt,regblock_base+66
	set	regblock_bfly_eq,regblock_base+72
	set	regblock_bfly_prim_type_q,regblock_base+78
	set	regblock_bfly_cons,regblock_base+84
	set	regblock_bfly_vector_length,regblock_base+90
	set	regblock_bfly_vector_ref,regblock_base+96
	set	regblock_bfly_car,regblock_base+102
	set	regblock_bfly_cdr,regblock_base+108
	set	regblock_bfly_future_ref,regblock_base+114
	set	regblock_bfly_my_int_num,regblock_base+120
	set	regblock_bfly_get_fluid,regblock_base+126
	set	regblock_bfly_set_fluid,regblock_base+132
	set	regblock_bfly_get_dynamic,regblock_base+138
	set	regblock_bfly_set_dynamic,regblock_base+144
	set	regblock_bfly_touch,regblock_base+150
	set	regblock_bfly_gen_car_cdr,regblock_base+156

	set	regblock_bfly_temp,2000

;	struct btrctrl                  /* block transfer control block */
;	{
;	    char  *bt_to;		/* virtual address of destination */
;	    short unsigned bt_len;	/* length of transfer in bytes [minus 1] */
;	    char  *bt_from;		/* virtual address of source */
;	};

	set	regblock_bfly_pnc_block,1004
	set	regblock_bfly_target,1004
	set	regblock_bfly_length,1008
	set	regblock_bfly_source,1010

	set	regblock_cache_variable,1014
	set	regblock_cache_start,1020
	set	regblock_cache_length,1024
	set	regblock_cache_block,1028

	text

	global	_butterfly_compiler_init
	global	_return_to_interpreter

_butterfly_compiler_init:
	mov.l	%a0,-(%sp)

	lea	_return_instruction,%a0
	mov.l	%a0,_return_to_interpreter
	mov.b	&tc_return_address,_return_to_interpreter

	lea	_Registers,%a0
	mov.w	&0x4ef9,regblock_bfly_pnc_load(%a0)
	mov.l	&bfly_pnc_load,regblock_bfly_pnc_load+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_pnc_load(%a0)
	mov.l	&bfly_pnc_load,regblock_bfly_pnc_load+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_pnc_store(%a0)
	mov.l	&bfly_pnc_store,regblock_bfly_pnc_store+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_pnc_copy(%a0)
	mov.l	&bfly_pnc_copy,regblock_bfly_pnc_copy+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_pnc_push(%a0)
	mov.l	&bfly_pnc_push,regblock_bfly_pnc_push+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_touch_op(%a0)
	mov.l	&bfly_touch,regblock_bfly_touch_op+2(%a0)

	mov.w	&3,regblock_bfly_length(%a0)

	mov.w	&0x4ef9,regblock_bfly_one_plus(%a0)
	mov.l	&bfly_prim_one_plus,regblock_bfly_one_plus+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_one_minus(%a0)
	mov.l	&bfly_prim_one_minus,regblock_bfly_one_minus+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_plus(%a0)
	mov.l	&bfly_prim_plus,regblock_bfly_plus+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_minus(%a0)
	mov.l	&bfly_prim_minus,regblock_bfly_minus+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_eql(%a0)
	mov.l	&bfly_prim_eql,regblock_bfly_eql+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_lt(%a0)
	mov.l	&bfly_prim_lt,regblock_bfly_lt+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_gt(%a0)
	mov.l	&bfly_prim_gt,regblock_bfly_gt+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_eq(%a0)
	mov.l	&bfly_prim_eq,regblock_bfly_eq+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_prim_type_q(%a0)
	mov.l	&bfly_prim_prim_type_q,regblock_bfly_prim_type_q+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_cons(%a0)
	mov.l	&bfly_prim_cons,regblock_bfly_cons+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_vector_length(%a0)
	mov.l	&bfly_prim_vector_length,regblock_bfly_vector_length+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_vector_ref(%a0)
	mov.l	&bfly_prim_vector_ref,regblock_bfly_vector_ref+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_car(%a0)
	mov.l	&bfly_prim_car,regblock_bfly_car+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_cdr(%a0)
	mov.l	&bfly_prim_cdr,regblock_bfly_cdr+2(%a0)

	mov.w	&0x4ef9,regblock_bfly_future_ref(%a0)
	mov.l	&bfly_prim_future_ref,regblock_bfly_future_ref+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_my_int_num(%a0)
	mov.l	&bfly_prim_my_int_num,regblock_bfly_my_int_num+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_get_fluid(%a0)
	mov.l	&bfly_prim_get_fluid,regblock_bfly_get_fluid+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_set_fluid(%a0)
	mov.l	&bfly_prim_set_fluid,regblock_bfly_set_fluid+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_get_dynamic(%a0)
	mov.l	&bfly_prim_get_dynamic,regblock_bfly_get_dynamic+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_set_dynamic(%a0)
	mov.l	&bfly_prim_set_dynamic,regblock_bfly_set_dynamic+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_touch(%a0)
	mov.l	&bfly_prim_touch,regblock_bfly_touch+2(%a0)
	mov.w	&0x4ef9,regblock_bfly_gen_car_cdr(%a0)
	mov.l	&bfly_prim_gen_car_cdr,regblock_bfly_gen_car_cdr+2(%a0)

	jsr	setup_cache_hook

	mov.l	(%sp)+,%a0
	rts

#	A0 -> the source, result into D0

	global	bfly_pnc_load
bfly_pnc_load:
	mov.l	%a0,regblock_bfly_source(%a6)
	lea	regblock_bfly_temp(%a6),%a0
	mov.l	%a0,regblock_bfly_target(%a6)
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
bfly_pnc_wait_loop:
	tst.w	bxfers
	bne.b	bfly_pnc_wait_loop
	mov.l	regblock_bfly_temp(%a6),%d0	# get the result
	rts

#	A0 -> the target, store value in D0

	global	bfly_pnc_store
bfly_pnc_store:
	mov.l	%d0,regblock_bfly_temp(%a6)
	mov.l	%a0,regblock_bfly_target(%a6)
	lea	regblock_bfly_temp(%a6),%a0
	mov.l	%a0,regblock_bfly_source(%a6)
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
bfly_pnc_store_loop:
	tst.w	bxfers
	bne.b	bfly_pnc_store_loop
	rts

#	%sp -> source 	(long)
#	       3 	(word)
#	       target	(long)

	global	bfly_pnc_copy
bfly_pnc_copy:
	mov.l	(%sp)+,%a0
	mov.l	%sp,Btran		# start the PNC
bfly_pnc_copy_loop:
	tst.w	bxfers			# wait for finish
	bne.b	bfly_pnc_copy_loop
	lea	10(%sp),%sp		# clean the stack
	jmp	(%a0)			# done

#	A0 -> source, result is pushed onto the stack

bfly_pnc_push:
	mov.l	%a0,regblock_bfly_source(%a6)
	mov.l	(%sp),%d0		# save the return address
	mov.l	%sp,regblock_bfly_target(%a6)
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
bfly_push_loop:
	tst.w	bxfers
	bne.b	bfly_push_loop
	mov.l	%d0,%a0			# restore the return address
	jmp	(%a0)			# done

#	Touch an object which might be a future
#
#	The object starts and winds up in regblock_value.
#	We KNOW it is of type FUTURE.

	global	bfly_touch
bfly_touch:
	mov.l	regblock_value(%a6),%a0	# get the future
	tst.l	future_is_determined(%a0)
	beq.w	bfly_must_wait		# if it is determined, get the value
	mov.l	future_value(%a0),%d0
	mov.l	%d0,regblock_value(%a6) # result goes into regblock:value
	rol.l	&8,%d0			# is this a future too?
	cmp.b	%d0,&tc_future
	beq.b	bfly_touch		#	if so -> refetch
	rts				# otherwise continue

#	If we must wait, we turn this into a call to the default scheduler
#	Some of this code has been stolen from compiler_primitive_apply

bfly_must_wait:
	mov.b	&tc_return_address,(%sp)# for the GC
	mov.l	%a0,-(%sp)		# push the future for the restart code
	pea	bfly_touch_restart	# return to bfly_touch_restart
	mov.b	&tc_return_address,(%sp)
	mov.l	%a0,-(%sp)		# push the future as an argument
	mov.l	_Fixed_Objects+System_Scheduler,-(%sp)
	movq	&2,%d0
	jmp	compiler_apply

#	We return with the value in register:value

bfly_touch_restart:
	mov.l	(%sp)+,regblock_value(%a6) # restore the future as the result
	cmp.b	regblock_value(%a6),&tc_future
	beq.w	bfly_touch
	clr.b	(%sp)
	rts

#	handle 1+
bfly_prim_one_plus:
	mov.w	&0xf1,%d6	# primitive 1+
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	add.l	&1,%d0
	rol.l	&8,%d0
	cmp.b	%d0,&tc_fixnum
	bne.w	do_it_the_hard_way
	ror.l	&8,%d0
	mov.l	%d0,regblock_value(%a6)
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

#	handle -1+
bfly_prim_one_minus:
	mov.w	&0xf2,%d6	# primitive -1+
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	sub.l	&1,%d0
	rol.l	&8,%d0
	cmp.b	%d0,&tc_fixnum
	bne.w	do_it_the_hard_way
	ror.l	&8,%d0
	mov.l	%d0,regblock_value(%a6)
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

#	handle +
bfly_prim_plus:
	mov.w	&0xec,%d6	# primitive +
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	add.l	4(%sp),%d0
	rol.l	&8,%d0
	cmp.b	%d0,&tc_fixnum
	bne.w	do_it_the_hard_way
	ror.l	&8,%d0
	mov.l	%d0,regblock_value(%a6)
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

#	handle -
bfly_prim_minus:
	mov.w	&0xed,%d6	# primitive -
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	4(%sp),%d0
	sub.l	(%sp),%d0
	rol.l	&8,%d0
	cmp.b	%d0,&tc_fixnum
	bne.w	do_it_the_hard_way
	ror.l	&8,%d0
	mov.l	%d0,regblock_value(%a6)
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

#	handle =
bfly_prim_eql:
	mov.w	&0xe9,%d6	# primitive =
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	cmp.l	%d0,4(%sp)
	bne.b	return_false

return_true:
	mov.l	&0x08000000,regblock_value(%a6)
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

return_false:
	clr.l	regblock_value(%a6)
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

#	handle <
bfly_prim_lt:
	mov.w	&0xea,%d6	# primitive <
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	asr.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	asr.l	&8,%d1
	cmp.l	%d0,%d1
	blt.w	return_true
	bra.w	return_false

#	handle >
bfly_prim_gt:
	mov.w	&0xeb,%d6	# primitive >
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	asr.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	asr.l	&8,%d1
	cmp.l	%d0,%d1
	bgt.w	return_true
	bra.w	return_false

#	handle eq?
bfly_prim_eq:
	mov.w	&0x0d,%d6	# primitive eq?
	cmp.b	(%sp),&tc_future
	beq.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_future
	beq.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	cmp.l	%d0,4(%sp)
	beq.w	return_true
	bra.w	return_false

#
do_it_the_hard_way:
	bra.w	0x138(%a6)

#	handle prim-type?
bfly_prim_prim_type_q:
	mov.w	&0x0f,%d6	# primitive prim-type?
	cmp.b	(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	cmp.b	4(%sp),&tc_future
	beq.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	cmp.b	%d0,4(%sp)
	bne.w	return_false
	bra.w	return_true

#	handle cons - OBSELETE
bfly_prim_cons:
	mov.w	&0x20,%d6	# primitive cons
	mov.l	%a5,regblock_value(%a6)
	mov.l	(%sp),(%a5)+
	mov.l	4(%sp),(%a5)+
	mov.b	&tc_pair,regblock_value(%a6)
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

#	handle vector-length
bfly_prim_vector_length:
	mov.w	&0x2d,%d6	# primitive vector-length
	cmp.b	(%sp),&tc_vector
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%a0
	mov.l	(%a0),regblock_value(%a6)
	mov.b	&tc_fixnum,regblock_value(%a6)
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

#	handle vector-ref
bfly_prim_vector_ref:
	mov.w	&0x2e,%d6	# primitive vector-ref
	cmp.b	(%sp),&tc_vector
	bne.w	do_it_the_hard_way
common_vector_ref:
	cmp.b	4(%sp),&tc_fixnum
	bne.w	do_it_the_hard_way
	mov.l	4(%sp),%d0
	lsl.l	&8,%d0
	asr.l	&8,%d0
	bmi.w	do_it_the_hard_way
	mov.l	(%sp),%a0
	mov.l	(%a0),%d1
	cmp.l	%d0,%d1
	blt.w	do_it_the_hard_way
	lsl.l	&2,%d0
	add.l	%d0,%a0
	add.l	&4,%a0
	mov.l	%a0,regblock_bfly_source(%a6)
	lea	regblock_value(%a6),%a0
	mov.l	%a0,regblock_bfly_target(%a6)
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
vector_wait_loop:
	tst.w	bxfers
	bne.b	vector_wait_loop
	lea	8(%sp),%sp
	clr.b	(%sp)
	rts

#	handle car
bfly_prim_car:
	mov.w	&0x21,%d6	# primitive car
	cmp.b	(%sp),&tc_pair
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%a0
bfly_car_cdr_common:
	tst.l	%a0
	beq.b	car_cdr_return_nil	
	mov.l	%a0,regblock_bfly_source(%a6)
	lea	regblock_value(%a6),%a0
	mov.l	%a0,regblock_bfly_target(%a6)
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
car_cdr_wait_loop:
	tst.w	bxfers
	bne.b	car_cdr_wait_loop
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

car_cdr_return_nil:
	clr.l	regblock_value(%a6)
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

#	handle cdr
bfly_prim_cdr:
	mov.w	&0x22,%d6	# primitive cdr
	cmp.b	(%sp),&tc_pair
	bne.w	do_it_the_hard_way
	mov.l	(%sp),%d0
	add.l	&4,%d0
	mov.l	%d0,regblock_bfly_source(%a6)
	bra.w	bfly_car_cdr_common

#	handle future-ref
bfly_prim_future_ref:
	mov.l	pc_vector_ref,%d6
	bra.w	common_vector_ref

#	handle my-interpreter-number
bfly_prim_my_int_num:
	mov.l	_Who_Am_I,regblock_value(%a6)
	mov.b	&tc_fixnum,regblock_value(%a6)
	clr.b	(%sp)
	rts

#	handle get-fluid-bindings
bfly_prim_get_fluid:
	mov.l	_Fluid_Bindings,regblock_value(%a6)
	clr.b	(%sp)
	rts

#	handle set-fluid-bindings!
bfly_prim_set_fluid:
	mov.l	_Fluid_Bindings,regblock_value(%a6)
	mov.l	(%sp),_Fluid_Bindings
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts

#	handle %get-dynamic-state
bfly_prim_get_dynamic:
	mov.l	_Current_State_Point,regblock_value(%a6)
	clr.b	(%sp)
	rts

	
#	handle %set-dynamic-state!
bfly_prim_set_dynamic:
	mov.l	_Current_State_Point,regblock_value(%a6)
	mov.l	(%sp),_Current_State_Point
	lea	4(%sp),%sp
	clr.b	(%sp)
	rts


#	handle general-car-cdr
bfly_prim_gen_car_cdr:
	mov.w	&0x27,%d6			; in case we hit futures
	lea	regblock_bfly_temp(%a6),%a0	; setup for PNC
	mov.l	%a0,regblock_bfly_target(%a6)	
	mov.l	(%sp),regblock_bfly_source(%a6)	; original pointer
	mov.l	4(%sp),%d0			; get the path into D0
	and.l	%d7,%d0				; turn off FIXNUM bits

gen_car_cdr_loop:
	cmp.l	%d0,&1				; if 1, we are done
	beq.b	gen_car_cdr_done

	cmp.b	regblock_bfly_source(%a6),&tc_pair
	bne.w	do_it_the_hard_way		; check for futures

	tst.l	regblock_bfly_source(%a6)	; check for NIL
	beq.b	gen_car_cdr_done

	lsr.l	&1,%d0				; check the bottom bit
	bcs.b	gen_car_cdr_do_car		; 0 means take CDR (else CAR)
	addq	&4,regblock_bfly_source(%a6)
gen_car_cdr_do_car:				; start the PNC
	lea	regblock_bfly_pnc_block(%a6),%a0
	mov.l	%a0,Btran
gen_car_cdr_wait:				; wait for PNC done
	tst.w	bxfers
	bne.b	gen_car_cdr_wait
	mov.l	regblock_bfly_temp(%a6),regblock_bfly_source(%a6)
	bra.b	gen_car_cdr_loop		; copy fetched into source and loop

gen_car_cdr_done:
	mov.l	regblock_bfly_source(%a6),regblock_value(%a6)
	lea	8(%sp),%sp			; adjust the stack
	clr.b	(%sp)				; and return
	rts

#	handle touch
bfly_prim_touch:
	cmp.b	(%sp),&tc_future
	beq.b	bfly_test_future
	mov.l	(%sp)+,regblock_value(%a6)
	clr.b	(%sp)
	rts

bfly_test_future:
	mov.l	(%sp),%a0
	tst.l	future_is_determined(%a0)
	beq.b	bfly_call_scheduler
	mov.l	future_value(%a0),(%sp)
	bra.b	bfly_prim_touch

;	Redo the stack:
;
;	BEFORE			AFTER			AFTER AWAIT
;
;	<return>		<return>		<return>
;	<future>		<future>		<future>
;				bfly_prim_touch
;				<future>

bfly_call_scheduler:
	mov.l	(%sp),%d0			;; Save the future
	pea	bfly_prim_touch
	mov.b	&tc_return_address,(%sp)
	mov.l	%d0,-(%sp)			;; Now push the future again
	mov.l	_Fixed_Objects+System_Scheduler,-(%sp)
	movq	&2,%d0
	jmp	compiler_apply

;
;	We need this so the return instruction has the right access mode!
;

retoffset	=	0x014a			;; Adjust this early and often!

		.globl	_return_instruction

_return_instruction:
		jmp	retoffset(%a6)

;
;	Two way hook to propagate the variable cache into each code space.
;

cache_variable	=	0x246			;; comentry_cache_variable_multiple

setup_cache_hook:
		cmp.l	cache_variable+2(%a0),&bfly_cache_hook
		beq.b	dont_setup_cache_hook
		mov.w	&0x4ef9,regblock_cache_variable(%a0)
		mov.l	cache_variable+2(%a0),regblock_cache_variable+2(%a0)
		mov.l	&bfly_cache_hook,cache_variable+2(%a0)
dont_setup_cache_hook:		;; Make sure we don't do this twice!
		rts

bfly_cache_hook:
		mov.l	%a1,regblock_cache_start(%a6)
		ext.l	%d1
		mov.l	%d1,regblock_cache_length(%a6)
		mov.l	%a0,regblock_cache_block(%a6)

		addq.l	&2,(%sp)		;; bump the return pointer
		mov.b	&tc_return_address,(%sp);; make stack safe for GC
		jsr	regblock_cache_variable(%a6)

		short	0			;; bogus code relocation

		mov.l	%a5,_Free
		mov.l	%sp,_Ext_Stack_Pointer
		mov.l	c_save_stack,%sp

		mov.l	regblock_cache_length(%a6),-(%sp)
		mov.l	regblock_cache_start(%a6),-(%sp)
		mov.l	regblock_cache_block(%a6),-(%sp)
		jsr	_Propagate_Variable_Cache
		lea	12(%sp),%sp

		mov.l	%sp,c_save_stack
		mov.l	_Ext_Stack_Pointer,%sp
		mov.l	_Free,%a5
		lea	_Registers,%a6

		clr.b	(%sp)
		rts
