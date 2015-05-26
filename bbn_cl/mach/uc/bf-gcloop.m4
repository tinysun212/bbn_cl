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
#### -*- Midas -*-

#
#	$Header: bf-gcloop.s,v 10.0 88/12/07 13:03:47 las Exp $
#
	text
	lalign	4
#
gcloop_dispatch_table:





	set	TC_NULL,0x00
	long	gcloop_non_pointer
	set	TC_LIST,0x01
	long	gcloop_pair
	set	TC_CHARACTER,0x02
	long	gcloop_non_pointer
	set	TC_SCODE_QUOTE,0x03
	long	gcloop_pair
	set	TC_PCOMB2,0x04
	long	gcloop_triple
	set	TC_UNINTERNED_SYMBOL,0x05
	long	gcloop_pair
	set	TC_BIG_FLONUM,0x06
	long	gcloop_vector
	set	TC_COMBINATION_1,0x07
	long	gcloop_pair
	set	TC_TRUE,0x08
	long	gcloop_non_pointer
	set	TC_EXTENDED_PROCEDURE,0x09
	long	gcloop_pair
	set	TC_VECTOR,0x0A
	long	gcloop_vector
	set	TC_RETURN_CODE,0x0B
	long	gcloop_non_pointer
	set	TC_COMBINATION_2,0x0C
	long	gcloop_triple
	set	TC_MANIFEST_CLOSURE,0x0D
	long	gcloop_closure
	set	TC_BIG_FIXNUM,0x0E
	long	gcloop_vector
	set	TC_PROCEDURE,0x0F
	long	gcloop_pair

	set	TC_ENTITY,0x10
	long	gcloop_pair
	set	TC_DELAY,0x11
	long	gcloop_pair
	set	TC_ENVIRONMENT,0x12
	long	gcloop_vector
	set	TC_DELAYED,0x13
	long	gcloop_pair
	set	TC_EXTENDED_LAMBDA,0x14
	long	gcloop_triple
	set	TC_COMMENT,0x15
	long	gcloop_pair
	set	TC_NON_MARKED_VECTOR,0x16
	long	gcloop_vector
	set	TC_LAMBDA,0x17
	long	gcloop_pair
	set	TC_PRIMITIVE,0x18
	long	gcloop_non_pointer
	set	TC_SEQUENCE_2,0x19
	long	gcloop_pair
	set	TC_FIXNUM,0x1A
	long	gcloop_non_pointer
	set	TC_PCOMB1,0x1B
	long	gcloop_pair
	set	TC_CONTROL_POINT,0x1C
	long	gcloop_vector
	set	TC_INTERNED_SYMBOL,0x1D
	long	gcloop_pair
	set	TC_CHARACTER_STRING,0x1E
	long	gcloop_vector
	set	TC_ACCESS,0x1F
	long	gcloop_pair
	set	TC_HUNK3_A,0x20
	long	gcloop_triple
	set	TC_DEFINITION,0x21
	long	gcloop_pair
	set	TC_BROKEN_HEART,0x22
	long	gcloop_broken_heart
	set	TC_ASSIGNMENT,0x23
	long	gcloop_pair
	set	TC_HUNK3_B,0x24
	long	gcloop_triple
	set	TC_IN_PACKAGE,0x25
	long	gcloop_pair
	set	TC_COMBINATION,0x26
	long	gcloop_vector
	set	TC_MANIFEST_NM_VECTOR,0x27
	long	gcloop_nm_header
	set	TC_COMPILED_ENTRY,0x28
	long	gcloop_compiled
	set	TC_LEXPR,0x29
	long	gcloop_pair
	set	TC_PCOMB3,0x2A
	long	gcloop_vector
	set	TC_MANIFEST_SPECIAL_NM_VECTOR,0x2B
	long	gcloop_nm_header
	set	TC_VARIABLE,0x2C
	long	gcloop_triple
	set	TC_THE_ENVIRONMENT,0x2D
	long	gcloop_non_pointer
	set	TC_FUTURE,0x2E
	long	gcloop_future
	set	TC_VECTOR_1B,0x2F
	long	gcloop_vector
	set	TC_PCOMB0,0x30
	long	gcloop_non_pointer
	set	TC_VECTOR_16B,0x31
	long	gcloop_vector
	set	TC_REFERENCE_TRAP,0x32
	long	gcloop_trap
	set	TC_SEQUENCE_3,0x33
	long	gcloop_triple
	set	TC_CONDITIONAL,0x34
	long	gcloop_triple
	set	TC_DISJUNCTION,0x35
	long	gcloop_pair
	set	TC_CELL,0x36
	long	gcloop_cell
	set	TC_WEAK_CONS,0x37
	long	gcloop_weak_pair
	set	TC_QUAD,0x38
	long	gcloop_quad
	set	TC_LINKAGE_SECTION,0x39
	long	gcloop_linkage
	set	TC_RATNUM,0x3A
	long	gcloop_pair
	set	TC_STACK_ENVIRONMENT,0x3B
	long	gcloop_non_pointer
	set	TC_COMPLEX,0x3C
	long	gcloop_pair
	set	TC_COMPILED_CODE_BLOCK,0x3D
	long	gcloop_vector
	set	TC_EMPTY_1,0x3E
	long	gcloop_non_pointer
	set	TC_EMPTY_2,0x3F
	long	gcloop_non_pointer

	set	TC_G_VECTOR,0x40
	long	gcloop_vector
	set	TC_IO_ERROR_CODE,0x41
	long	gcloop_non_pointer
	set	TC_CL_PACKAGE,0x42
	long	gcloop_vector
	set	TC_CLSAV,0x43
	long	gcloop_vector
	set	TC_RATIO,0x44
	long	gcloop_pair
	set	TC_CL_STREAM,0x45
	long	gcloop_vector
	set	TC_VECTOR_32B,0x46
	long	gcloop_vector
	set	TC_CL_ARRAY,0x47
	long	gcloop_vector
	set	TC_CL_IVECTOR,0x48
	long	gcloop_vector
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type

	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type

	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type

	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type
	long	gcloop_bad_type

###	***START***
###
###	Registers and locations:
###
###
###				is a0		Old_Value		is d0
###				is a1		vector counter		is d1
###	Old			is a2		Old/New object		is d2
###	To			is a3		Transport_Delta		is d3
###	Scan 			is a4		&scan_heaplet_end[-1]	is d4
###	fake_to			is a5		Transport_Bound		is d5
###	fp			is a6		cons_heaplet_bound	is d6
###	sp			is a7		0x00ffffff		is d7
###
###	To_ptr			is 12(fp)
###	Scan (parameter)	is 8(fp)
###	return address		is 4(fp)
###	saved a6		is (fp)
###	bump amount		is -4(fp)
###	entry return address	is -8(fp)
###	operator linkage count	is -12(fp)
















###
###	In various places there is an implicit assumption that
###	tc_null = 0, and furthermore, NIL = 0.
###
###	It is also assumed that scan_heaplet_end does not vary
###	while GCLoop is active.
###

	set	term_broken_heart,0x02
	set	term_exit,0x0c
	set	term_invalid_type_code,0x12
	set	term_no_space,0x15

	set	trap_max_immediate,0x9

#
#	Multiplied by sizeof(Pointer)
#
	set	future_determined_slot,0x4
	set	future_lock_slot,0x8
	set	future_value_slot,0xc
###
###	vector_delta is an offset to adjust a vector's address and length
###	and compare it to cons_heaplet_bound.  It depends on the fact
###	that the distance between cons_heaplet_bound and cons_heaplet_end
###	is fixed (heaplet_delta).  vector_delta takes into account the
###	the vector header (1 word), and the fact that there MUST be 1 word
###	left at the end of the heaplet for the broken heart that will make
###	GCLoop stop scanning.
###
	set	heaplet_buffer_size,5
	set	heaplet_delta,(4*heaplet_buffer_size)
	set	vector_delta,(8-heaplet_delta)






###
###	Transport_Bound and Transport_Delta are computed by Start_Flip
###	in order to allow GCLoop to remain blissfully ignorant of the
###	structure of SHARED_DATA.
###

	data
	global	_Transport_Bound
_Transport_Bound:
	space	4
	global	_Transport_Delta
_Transport_Delta:
	space	4

	text

	global	_GCLoop
_GCLoop:
	link	%a6,&-12			# For the debugger
	movm.l	%d2-%d7/%a2-%a5,-(%sp)

	mov.l	&0x00ffffff,%d7
	clr.l	-4(%a6)			# Scan bump amount
	movq	&-1,%d0
	mov.l	%d0,-(%sp)	# Mask for atomior32
	mov.l	%d0,-(%sp)	# allocate space for pointer
	mov.l	_Transport_Bound,%d5
	mov.l	_Transport_Delta,%d3
#
	mov.l	_cons_heaplet_bound,%d6
	mov.l	12(%a6),%a0
	mov.l	(%a0),%a5		# fake_to = *To_ptr
	mov.l	%a5,%a3
	add.l	%d3,%a3		# To = fake_to + Transport_Delta
	add.l	%d3,%d6
#
	mov.l	_scan_heaplet_end,%d4
	subq.l	&4,%d4		# Comparison excludes header
	mov.l	8(%a6),%a4		# Scan
	cmp.l	%a4,%d5
	bhs	gcloop_test		# Are we scanning a fixed area?
	mov.l	%d3,-4(%a6)	# Scan bump amount
	add.l	%d3,%a4	# We are really scanning the other heap
	add.l	%d3,%d4
	bra	gcloop_test
#
gcloop_end:
	mov.l	%a5,%d0	# Consistency check
	add.l	%d3,%d0
	cmp.l	%d0,%a3
	bne	gcloop_phase_error
#
	mov.l	12(%a6),%a0
	mov.l	%a5,(%a0)		# *To_ptr = fake_to
	mov.l	%a4,%d0		# Bump Scan back and return it
	sub.l	-4(%a6),%d0
#
	lea	8(%sp),%sp		# Pop space for atomior32
	movm.l	(%sp)+,%d2-%d7/%a2-%a5
	unlk	%a6
	rts

gcloop_nm_header:
	and.l	%d7,%d2
	lea	(0,%a4,%d2.l*4),%a4	# The 4 (header) will be added below
	cmp.l	%a4,%d4			# Have we exceeded scan_heaplet_end?
	bhs	gcloop_scan_overflow
#
#	fall	through
#
gcloop_dont_relocate:
gcloop_non_pointer:
	addq.l	&4,%a4			# bump scan and fall through
#
#	fall	through
#
gcloop_test:
	cmp.l	%a3,%a4			# compare Scan and To
	beq	gcloop_end
#
#	fall	through
#	
gcloop_dispatch:
	mov.l	(%a4),%d2		# Temp
	mov.l	%d2,(%sp)
	clr.l	%d0
	mov.b	(%sp),%d0	# Type_Code(Temp)
#
#	I don't know what addressing mode to use to get this to work
#
#	mov.l	(0,gcloop_dispatch_table,first_word.w*4),%a0
#
#	This is a kludge, but it works.
#
dispatch_instr:
#
	set	dispatch_dot,dispatch_instr+2
#
	mov.l	(gcloop_dispatch_table - dispatch_dot,%pc,%d0.w*4),%a0
	jmp	(%a0)

#
#	Shared macros
#








	




###
###	Pointer utilities
###

gcloop_locked_relocated:
	mov.l	%d0,(%a2)	# Unlock
#
#	fall	through
#
gcloop_relocated:
	mov.l	(%sp),%d0
#
gcloop_relocated_common:
	and.l	%d7,%d0	# Get address
	add.l	%d0,%d2	# compute new object
	mov.l	%d2,(%a4)+		# relocate
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_locked:
	mov.l	(%a2),%d0	# wait until unlocked
	cmp.l	%d0,4(%sp)
	beq	gcloop_locked
	bra	gcloop_relocated_common

###
###	Small fixed length objects.
###
gcloop_trap:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	movq	&trap_max_immediate,%d1	# Is it an immediate trap?
	cmp.l	%d1,%d0
	bhs	gcloop_non_pointer
#
#	fall	through
#
gcloop_pair:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	2*4(%a5),%a5
	mov.l	(%sp),(%a2)+	# Forward and unlock
	mov.l	%d0,(%a3)+	# Transport
	mov.l	(%a2),(%a3)+
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_triple:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	3*4(%a5),%a5
	mov.l	(%sp),(%a2)+	# Forward and unlock
	mov.l	%d0,(%a3)+	# Transport
	mov.l	(%a2)+,(%a3)+
	mov.l	(%a2),(%a3)+
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_cell:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	1*4(%a5),%a5
	mov.l	(%sp),(%a2)	# Forward and unlock
	mov.l	%d0,(%a3)+	# Transport
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_quad:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	4*4(%a5),%a5
	mov.l	(%sp),(%a2)+	# Forward and unlock
	mov.l	%d0,(%a3)+	# Transport
	mov.l	(%a2)+,(%a3)+
	mov.l	(%a2)+,(%a3)+
	mov.l	(%a2),(%a3)+
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

#
#	We can see whether it is spliceable before we consider
#	moving it.  This can be done because the slots examined
#	are not the header, where the lock or broken heart would
#	be stored, and thus these fields contain the same value
#	whether the object is in old space, new space, or in the
#	process of being moved by another processor.
#
gcloop_future_splice:
	mov.l	future_value_slot(%a0),(%a4) # value -> *Scan
	bra	gcloop_dispatch		# look at it again.

gcloop_future:
	mov.l	%d2,%d0	# compute vector address
	and.l	%d7,%d0
	mov.l	%d0,%a0
	cmpi.l	future_determined_slot(%a0),&TC_TRUE*0x1000000+0 # is it determined?
	bne.b	gcloop_vector
	tst.l	future_lock_slot(%a0)	# is it locked?
	beq	gcloop_future_splice
#
#	fall	through
#
gcloop_vector:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
#
gcloop_vector_lock:
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	mov.l	%d0,%d1
	and.l	%d7,%d1 # Get Length
	lea	(vector_delta,%a3,%d1.l*4),%a0 # compute end of copy
	cmp.l	%a0,%d6
	bhi	vector_get_more_heap
#
gcloop_vector_continue:
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	(4,%a5,%d1.l*4),%a5
	mov.l	(%sp),(%a2)+		# Forward and unlock
	mov.l	%d0,(%a3)+		# Move header
	bra.b	copy_vector_inner_test
#
copy_vector_outer_loop:
	swap	%d1
copy_vector_inner_loop:
	mov.l	(%a2)+,(%a3)+
copy_vector_inner_test:
	dbf	%d1,copy_vector_inner_loop
	swap	%d1
	dbf	%d1,copy_vector_outer_loop
#
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end
#
#	Compiled entries are like vectors except that the
#	vector address is computed differently.
#
gcloop_compiled:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	bra.b	enter_compiled_entry_to_block_loop

compiled_entry_to_block_loop:
	lea	1(%a2),%a2

enter_compiled_entry_to_block_loop:
	mov.w	-2(%a2),%d1 # compute vector address
	sub.w	%d1,%a2	# vector_count is sign extended first
	lsr.w	&1,%d1		# is this an extension word?
	bcs.b	compiled_entry_to_block_loop
	bra	gcloop_vector_lock

###	Linkage sections contain jump quads without type codes or
###	jmp/jsr instructions to absolute addresses.
###
gcloop_linkage:
	tst.b	1(%sp)			# Is it operator
	beq	gcloop_linkage_operator
#
#	All the objects are just quads without their type codes.
#	The count is in object.w
#
	mov.w	%d2,%d1	# Use vector_count as counter
	bra.b	linkage_quads_constant
#
linkage_quads_loop:
	mov.l	(%a4),%d2
	cmp.l	%d2,%d5
	blo.b	linkage_quads_relocate
#
linkage_quads_constant:
	addq.l	&4,%a4			# Bump scan to next word
#
linkage_quads_test:
	dbf	%d1,linkage_quads_loop
	bra	gcloop_test
#
linkage_quads_relocate:
	mov.l	%d2,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	quad_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	quad_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	quad_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	16(%a5),%a5	# Bump fake_to to end
	mov.l	(%sp),(%a2)+	# Forward and unlock
	mov.l	%d0,(%a3)+	# Transport
	mov.l	(%a2)+,(%a3)+
	mov.l	(%a2)+,(%a3)+
	mov.l	(%a2),(%a3)+
	mov.l	%d2,(%a4)+
#
	cmp.l	%a3,%d6		# Do we need more space?
	blo	linkage_quads_test

#
#	Get more heap and continue the loop
#
	mov.l	%d1,-(%sp)	# Save the counter
	pea	linkage_quads_test	# tail recurse
	bra	common_get_more_heap
#
quad_locked_relocated:
	mov.l	%d0,(%a2)	# Unlock
#
quad_relocated:
	mov.l	(%sp),%d0
#
quad_reloacted_common:
	and.l	%d7,%d0
	mov.l	%d0,(%a4)+	# relocate
	bra	linkage_quads_test
#
quad_locked:
	mov.l	(%a2),%d0	# wait until unlocked
	cmp.l	%d0,4(%sp)
	beq	quad_locked
	bra	quad_reloacted_common

###	Each entry is an absolute jmp/jsr instruction followed by a
###	word (2 bytes) of  argument count.
###	The number of entries times 2 is in object.w
###
gcloop_linkage_operator:
	mov.w	%d2,%d1
	lsr.w	&1,%d1		# Divide by 2
	addq.l	&2,%a4			# Bump scan to first entry
	lea	linkage_transport_continue,%a0
	mov.l	%a0,-8(%a6)		# Save return address
	bra	enter_linkage_operator_loop

linkage_operator_loop:
	addq.l	&8,%a4			# Bump past this entry
	mov.l	-4(%a4),%a2	# Extract address
	cmp.l	%a2,%d5
	blo.b	linkage_transport_entry
#
enter_linkage_operator_loop:
	dbf	%d1,linkage_operator_loop
#
	addq.l	&2,%a4
	bra	gcloop_test		# or gcloop_dont_relocate
#
linkage_transport_entry;
	mov.w	%d1,-12(%a6)	# Save count
	mov.l	%a2,%d2		# Copy entry point to save offset
	bra	enter_entry_to_block_loop
#
linkage_transport_continue:
	mov.w	-12(%a6),%d1	# Restore count
	bra	enter_linkage_operator_loop

###	Manifest closures are a header followed by cc-entry headers and
###	jsr instructions to absolute addresses.
###
gcloop_closure:
	lea	closure_loop_continue,%a0
	mov.l	%a0,-8(%a6)		# return address for entry transport
	addq.l	&4,%a4
	bra.b	closure_loop_continue

closure_loop:
	addq.l	&6,%a4			# Bump to next entry point
	mov.l	(%a4)+,%a2	# Get entry point
	cmp.l	%a2,%d5	# in constant or code space?
	blo.b	closure_transport_entry
#
closure_loop_continue:
	tst.w	(%a4)			# Are we done?
	bne	closure_loop
#
	mov.l	%a4,%d2		# Round down to previous longword
	and.b	&0xfc,%d2
	mov.l	%d2,%a4
	bra	gcloop_dont_relocate	# The rest are normal objects
#
#	This will be exercised extremely rarely since cc lives almost
#	exclusively in code space.
#
closure_transport_entry:
	mov.l	%a2,%d2
	bra.b	enter_entry_to_block_loop

###	Shared utility for manifest_closure and operator linkage_section.
###	It expects a return address (of the loop) in -8(fp).
###	It relocates the entry in object,old_addr (2 copies).
###	It stores the new version into -4(scan).
###	scan is maintained.
###
entry_to_block_loop:
	lea	1(%a2),%a2

enter_entry_to_block_loop:
	mov.w	-2(%a2),%d1 # compute vector address
	sub.w	%d1,%a2	# vector_count is sign extended first
	lsr.w	&1,%d1		# is this an extension word?
	bcs.b	entry_to_block_loop
#
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	entry_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	entry_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	entry_locked_relocated
	mov.l	%d0,%d1
	and.l	%d7,%d1 # Get Length
	lea	(vector_delta,%a3,%d1.l*4),%a0 # compute end of copy
	cmp.l	%a0,%d6
	bhi	entry_get_more_heap_1
#
copy_entry_continue:
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	(4,%a5,%d1.l*4),%a5
	mov.l	(%sp),(%a2)+		# Forward and unlock
	mov.l	%d0,(%a3)+		# Move header
	bra.b	copy_entry_inner_test
#
copy_entry_outer_loop:
	swap	%d1
copy_entry_inner_loop:
	mov.l	(%a2)+,(%a3)+
copy_entry_inner_test:
	dbf	%d1,copy_entry_inner_loop
	swap	%d1
	dbf	%d1,copy_entry_outer_loop
#
entry_end:
	mov.l	%d2,-4(%a4)		# into Scan
	cmp.l	%a3,%d6		# Do we need more space?
	bhs.b	entry_get_more_heap_2
	mov.l	-8(%a6),%a0		# return address
	jmp	(%a0)
	
#
#	Random entry points for entry relocation.
#
entry_locked_relocated:
	mov.l	%d0,(%a2)	# Unlock
#
entry_relocated:
	mov.l	(%sp),%d0
#
entry_relocated_common:
	and.l	%d7,%d0	# Get address
	add.l	%d0,%d2	# compute new object
	bra	entry_end

entry_locked:
	mov.l	(%a2),%d0	# wait until unlocked
	cmp.l	%d0,4(%sp)
	beq	entry_locked
	bra	entry_relocated_common
#
entry_get_more_heap_1:
	pea	copy_entry_continue
	bra	common_vector_get_more_heap
#
entry_get_more_heap_2:
	mov.l	-8(%a6),-(%sp)		# Tail recurse
	bra	common_get_more_heap

#
#	This is currently not used, but in case it ever is.
#
gcloop_weak_pair:
	mov.l	%d2,%d0
	and.l	%d7,%d0
	cmp.l	%d0,%d5	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	%d0,%a2
	sub.l	%a2,%d2		# Save type code and offset (if cc)
	mov.l	(%a2),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_relocated
	mov.l	%a2,(%sp)
	jsr	_atomior32
	cmp.l	%d0,4(%sp)	# Locked?
	beq	gcloop_locked
	mov.l	%d0,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	gcloop_locked_relocated
	add.l	%a5,%d2		# Compute new object
	mov.l	%a5,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)
	lea	2*4(%a5),%a5
	mov.l	(%sp),(%a2)+	# Forward and unlock
	mov.l	%d0,%d1	#
	and.l	%d7,%d1 # Datum of car
	mov.l	%d1,(%a3)+	# in new space with NULL type
	mov.l	(%a2),(%a3)+	# cdr
	sub.l	%d1,%d0	# Type of old car in high 24 bits.
	mov.l	_Weak_Chain,%d1
	mov.l	(%a4),_Weak_Chain	# Weak_Chain = Temp
	and.l	%d7,%d1 # Datum of old Weak_Chain
	add.l	%d1,%d0	# with type code of old car
	mov.l	%d0,(%a2)	# into old cdr.
	mov.l	%d2,(%a4)+		# into Scan
	cmp.l	%a3,%d6
	bhs	gcloop_get_more_heap
	cmp.l	%a3,%a4
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_broken_heart:
	and.l	%d7,%d2	# Get_Pointer(Temp)
	add.l	-4(%a6),%d2		# Bump by same amount as scan
	cmp.l	%a4,%d2		# == Scan ?
	beq	gcloop_end
	mov.l	(%a4),-(%sp)		# *Scan
	pea	broken_heart_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	12(%sp),%sp
	moveq	&term_broken_heart,%d0
	bra	gcloop_death

gcloop_get_more_heap:
	pea	gcloop_test		# Tail recurse
#	bra	common_get_more_heap
#
common_get_more_heap:	
	mov.l	%a5,%d0	# Consistency check
	add.l	%d3,%d0
	cmp.l	%d0,%a3
	bne	gcloop_phase_error
#
	clr.l	-(%sp)			# get_new_consing_partition(fake_to, 0)
	pea	(%a5)
	jsr	_get_new_consing_partition
	lea	8(%sp),%sp		# Pop args
	mov.l	_cons_heaplet_bound,%d6
	add.l	%d3,%d6
	mov.l	%d0,%a5		# result is new fake_to
	mov.l	%a5,%a3
	add.l	%d3,%a3		# To = fake_to + Transport_Delta
	rts

vector_get_more_heap:
	pea	gcloop_vector_continue	# Tail recurse
#	bra	common_vector_get_more_heap
#
#	Fall through
#
common_vector_get_more_heap:
	mov.l	%d0,-(%sp)	# save header
	mov.l	%a5,%d0	# Consistency check
	add.l	%d3,%d0
	cmp.l	%d0,%a3
	bne	gcloop_phase_error
#
	mov.l	%d1,-(%sp)	# get_new_consing_partition(fake_to, count)
	pea	(%a5)
	jsr	_get_new_consing_partition
	lea	8(%sp),%sp		# Pop args
	mov.l	_cons_heaplet_bound,%d6
	add.l	%d3,%d6
	mov.l	%d0,%a5		# result is new fake_to
	mov.l	%a5,%a3
	add.l	%d3,%a3		# To = fake_to + Transport_Delta
	mov.l	(%sp)+,%d0	# restore header
	mov.l	%d0,%d1
	and.l	%d7,%d1 # Get Length
	lea	(vector_delta,%a3,%d1.l*4),%a0
	cmp.l	%a0,%d6
	bhi.b	common_vector_out_of_space
	rts				# Return
#
common_vector_out_of_space:
	mov.l	_cons_heaplet_end,%d1
	sub.l	_cons_heaplet,%d1
	subq.l	&1,%d1
	lsr.l	&2,%d1		# length of heaplet in pointers
	mov.l	%d1,-(%sp)	# effective heaplet length
	and.l	%d7,%d0
	addq.l	&1,%d0
	mov.l	%d0,-(%sp)	# effective vector length
	pea	vector_too_long_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp
	moveq	&term_no_space,%d0
	bra	gcloop_death

gcloop_phase_error:
	mov.l	%d3,-(%sp)
	mov.l	%a5,-(%sp)
	pea	phase_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_exit,%d0
	bra	gcloop_death

gcloop_bad_type:
	mov.l	%d2,-(%sp)		# Temp
	mov.l	%d0,-(%sp)	# Type_Code(Temp)
	pea	bad_type_code_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_invalid_type_code,%d0
	bra	gcloop_death

gcloop_scan_overflow:
	mov.l	%d4,-(%sp)
	addq.l	&4,(%sp)		# Compute effective scan_heaplet_end
	pea	(%a4)
	pea	nmv_overflow_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_exit,%d0
#
#	fall	through
#
gcloop_death:
	pea	(%a3)
	pea	(%a4)
	pea	_gc_death_message_buffer
	mov.l	%d0,-(%sp)	# Termination code
	jsr	_gc_death
#
#	We should NEVER get here.
#
real_death:
	lea	16(%sp),%sp

real_death_loop:
	mov.l	&1,-(%sp)
	jsr	_exit
	lea	4(%sp),%sp
	bra	real_death_loop

#
#	These are in text space since they are read only.
#
	lalign	4

broken_heart_error_string:
	asciz	"gcloop: broken heart (0x%lx) in Scan"
	lalign	4

nmv_overflow_error_string:
	asciz	"gcloop: NMV at 0x%x overflows heaplet ending at 0x%x"
	lalign	4

vector_too_long_error_string:
	asciz	"gcloop: vector is too long (0d%d) for partition (0d%d)"
	lalign	4

bad_type_code_error_string:
	asciz	"gcloop: bad type code (0x%02x), pointer = 0x%08x"
	lalign	4

phase_error_string
	asciz	"gcloop: phase error: To = 0x%x, delta = 0x%x"
	lalign	4
