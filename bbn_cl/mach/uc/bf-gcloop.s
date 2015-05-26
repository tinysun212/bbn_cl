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

define(define_type_code,
	`set	$1,$2
	long	$3')

define(define_bad_type_code,
	`long	gcloop_bad_type')

	define_type_code(TC_NULL,			0x00,	gcloop_non_pointer)
	define_type_code(TC_LIST,			0x01,	gcloop_pair)
	define_type_code(TC_CHARACTER,			0x02,	gcloop_non_pointer)
	define_type_code(TC_SCODE_QUOTE,		0x03,	gcloop_pair)
	define_type_code(TC_PCOMB2,			0x04,	gcloop_triple)
	define_type_code(TC_UNINTERNED_SYMBOL,		0x05,	gcloop_pair)
	define_type_code(TC_BIG_FLONUM,			0x06,	gcloop_vector)
	define_type_code(TC_COMBINATION_1,		0x07,	gcloop_pair)
	define_type_code(TC_TRUE,			0x08,	gcloop_non_pointer)
	define_type_code(TC_EXTENDED_PROCEDURE,		0x09,	gcloop_pair)
	define_type_code(TC_VECTOR,			0x0A,	gcloop_vector)
	define_type_code(TC_RETURN_CODE,		0x0B,	gcloop_non_pointer)
	define_type_code(TC_COMBINATION_2,		0x0C,	gcloop_triple)
	define_type_code(TC_MANIFEST_CLOSURE,		0x0D,	gcloop_closure)
	define_type_code(TC_BIG_FIXNUM,			0x0E,	gcloop_vector)
	define_type_code(TC_PROCEDURE,			0x0F,	gcloop_pair)

	define_type_code(TC_ENTITY,			0x10,	gcloop_pair)
	define_type_code(TC_DELAY,			0x11,	gcloop_pair)
	define_type_code(TC_ENVIRONMENT,		0x12,	gcloop_vector)
	define_type_code(TC_DELAYED,			0x13,	gcloop_pair)
	define_type_code(TC_EXTENDED_LAMBDA,		0x14,	gcloop_triple)
	define_type_code(TC_COMMENT,			0x15,	gcloop_pair)
	define_type_code(TC_NON_MARKED_VECTOR,		0x16,	gcloop_vector)
	define_type_code(TC_LAMBDA,			0x17,	gcloop_pair)
	define_type_code(TC_PRIMITIVE,			0x18,	gcloop_non_pointer)
	define_type_code(TC_SEQUENCE_2,			0x19,	gcloop_pair)
	define_type_code(TC_FIXNUM,			0x1A,	gcloop_non_pointer)
	define_type_code(TC_PCOMB1,			0x1B,	gcloop_pair)
	define_type_code(TC_CONTROL_POINT,		0x1C,	gcloop_vector)
	define_type_code(TC_INTERNED_SYMBOL,		0x1D,	gcloop_pair)
	define_type_code(TC_CHARACTER_STRING,		0x1E,	gcloop_vector)
	define_type_code(TC_ACCESS,			0x1F,	gcloop_pair)
	define_type_code(TC_HUNK3_A,			0x20,	gcloop_triple)
	define_type_code(TC_DEFINITION,			0x21,	gcloop_pair)
	define_type_code(TC_BROKEN_HEART,		0x22,	gcloop_broken_heart)
	define_type_code(TC_ASSIGNMENT,			0x23,	gcloop_pair)
	define_type_code(TC_HUNK3_B,			0x24,	gcloop_triple)
	define_type_code(TC_IN_PACKAGE,			0x25,	gcloop_pair)
	define_type_code(TC_COMBINATION,		0x26,	gcloop_vector)
	define_type_code(TC_MANIFEST_NM_VECTOR,		0x27,	gcloop_nm_header)
	define_type_code(TC_COMPILED_ENTRY,		0x28,	gcloop_compiled)
	define_type_code(TC_LEXPR,			0x29,	gcloop_pair)
	define_type_code(TC_PCOMB3,			0x2A,	gcloop_vector)
	define_type_code(TC_MANIFEST_SPECIAL_NM_VECTOR,	0x2B,	gcloop_nm_header)
	define_type_code(TC_VARIABLE,			0x2C,	gcloop_triple)
	define_type_code(TC_THE_ENVIRONMENT,		0x2D,	gcloop_non_pointer)
	define_type_code(TC_FUTURE,			0x2E,	gcloop_future)
	define_type_code(TC_VECTOR_1B,			0x2F,	gcloop_vector)
	define_type_code(TC_PCOMB0,			0x30,	gcloop_non_pointer)
	define_type_code(TC_VECTOR_16B,			0x31,	gcloop_vector)
	define_type_code(TC_REFERENCE_TRAP,		0x32,	gcloop_trap)
	define_type_code(TC_SEQUENCE_3,			0x33,	gcloop_triple)
	define_type_code(TC_CONDITIONAL,		0x34,	gcloop_triple)
	define_type_code(TC_DISJUNCTION,		0x35,	gcloop_pair)
	define_type_code(TC_CELL,			0x36,	gcloop_cell)
	define_type_code(TC_WEAK_CONS,			0x37,	gcloop_weak_pair)
	define_type_code(TC_QUAD,			0x38,	gcloop_quad)
	define_type_code(TC_LINKAGE_SECTION,		0x39,	gcloop_linkage)
	define_type_code(TC_RATNUM,			0x3A,	gcloop_pair)
	define_type_code(TC_STACK_ENVIRONMENT,		0x3B,	gcloop_non_pointer)
	define_type_code(TC_COMPLEX,			0x3C,	gcloop_pair)
	define_type_code(TC_COMPILED_CODE_BLOCK,	0x3D,	gcloop_vector)
	define_type_code(TC_EMPTY_1,			0x3E,	gcloop_non_pointer)
	define_type_code(TC_EMPTY_2,			0x3F,	gcloop_non_pointer)

	define_type_code(TC_G_VECTOR,			0x40,	gcloop_vector)
	define_type_code(TC_IO_ERROR_CODE,		0x41,	gcloop_non_pointer)
	define_type_code(TC_CL_PACKAGE,			0x42,	gcloop_vector)
	define_type_code(TC_CLSAV,			0x43,	gcloop_vector)
	define_type_code(TC_RATIO,			0x44,	gcloop_pair)
	define_type_code(TC_CL_STREAM,			0x45,	gcloop_vector)
	define_type_code(TC_VECTOR_32B,			0x46,	gcloop_vector)
	define_type_code(TC_CL_ARRAY,			0x47,	gcloop_vector)
	define_type_code(TC_CL_IVECTOR,			0x48,	gcloop_vector)
	define_bad_type_code(0x49)
	define_bad_type_code(0x4A)
	define_bad_type_code(0x4B)
	define_bad_type_code(0x4C)
	define_bad_type_code(0x4D)
	define_bad_type_code(0x4E)
	define_bad_type_code(0x4F)
	define_bad_type_code(0x50)
	define_bad_type_code(0x51)
	define_bad_type_code(0x52)
	define_bad_type_code(0x53)
	define_bad_type_code(0x54)
	define_bad_type_code(0x55)
	define_bad_type_code(0x56)
	define_bad_type_code(0x57)
	define_bad_type_code(0x58)
	define_bad_type_code(0x59)
	define_bad_type_code(0x5A)
	define_bad_type_code(0x5B)
	define_bad_type_code(0x5C)
	define_bad_type_code(0x5D)
	define_bad_type_code(0x5E)
	define_bad_type_code(0x5F)
	define_bad_type_code(0x60)
	define_bad_type_code(0x61)
	define_bad_type_code(0x62)
	define_bad_type_code(0x63)
	define_bad_type_code(0x64)
	define_bad_type_code(0x65)
	define_bad_type_code(0x66)
	define_bad_type_code(0x67)
	define_bad_type_code(0x68)
	define_bad_type_code(0x69)
	define_bad_type_code(0x6A)
	define_bad_type_code(0x6B)
	define_bad_type_code(0x6C)
	define_bad_type_code(0x6D)
	define_bad_type_code(0x6E)
	define_bad_type_code(0x6F)

	define_bad_type_code(0x70)
	define_bad_type_code(0x71)
	define_bad_type_code(0x72)
	define_bad_type_code(0x73)
	define_bad_type_code(0x74)
	define_bad_type_code(0x75)
	define_bad_type_code(0x76)
	define_bad_type_code(0x77)
	define_bad_type_code(0x78)
	define_bad_type_code(0x79)
	define_bad_type_code(0x7A)
	define_bad_type_code(0x7B)
	define_bad_type_code(0x7C)
	define_bad_type_code(0x7D)
	define_bad_type_code(0x7E)
	define_bad_type_code(0x7F)
	define_bad_type_code(0x80)
	define_bad_type_code(0x81)
	define_bad_type_code(0x82)
	define_bad_type_code(0x83)
	define_bad_type_code(0x84)
	define_bad_type_code(0x85)
	define_bad_type_code(0x86)
	define_bad_type_code(0x87)
	define_bad_type_code(0x88)
	define_bad_type_code(0x89)
	define_bad_type_code(0x8A)
	define_bad_type_code(0x8B)
	define_bad_type_code(0x8C)
	define_bad_type_code(0x8D)
	define_bad_type_code(0x8E)
	define_bad_type_code(0x8F)
	define_bad_type_code(0x90)
	define_bad_type_code(0x91)
	define_bad_type_code(0x92)
	define_bad_type_code(0x93)
	define_bad_type_code(0x94)
	define_bad_type_code(0x95)
	define_bad_type_code(0x96)
	define_bad_type_code(0x97)
	define_bad_type_code(0x98)
	define_bad_type_code(0x99)
	define_bad_type_code(0x9A)
	define_bad_type_code(0x9B)
	define_bad_type_code(0x9C)
	define_bad_type_code(0x9D)
	define_bad_type_code(0x9E)
	define_bad_type_code(0x9F)

	define_bad_type_code(0xA0)
	define_bad_type_code(0xA1)
	define_bad_type_code(0xA2)
	define_bad_type_code(0xA3)
	define_bad_type_code(0xA4)
	define_bad_type_code(0xA5)
	define_bad_type_code(0xA6)
	define_bad_type_code(0xA7)
	define_bad_type_code(0xA8)
	define_bad_type_code(0xA9)
	define_bad_type_code(0xAA)
	define_bad_type_code(0xAB)
	define_bad_type_code(0xAC)
	define_bad_type_code(0xAD)
	define_bad_type_code(0xAE)
	define_bad_type_code(0xAF)
	define_bad_type_code(0xB0)
	define_bad_type_code(0xB1)
	define_bad_type_code(0xB2)
	define_bad_type_code(0xB3)
	define_bad_type_code(0xB4)
	define_bad_type_code(0xB5)
	define_bad_type_code(0xB6)
	define_bad_type_code(0xB7)
	define_bad_type_code(0xB8)
	define_bad_type_code(0xB9)
	define_bad_type_code(0xBA)
	define_bad_type_code(0xBB)
	define_bad_type_code(0xBC)
	define_bad_type_code(0xBD)
	define_bad_type_code(0xBE)
	define_bad_type_code(0xBF)
	define_bad_type_code(0xC0)
	define_bad_type_code(0xC1)
	define_bad_type_code(0xC2)
	define_bad_type_code(0xC3)
	define_bad_type_code(0xC4)
	define_bad_type_code(0xC5)
	define_bad_type_code(0xC6)
	define_bad_type_code(0xC7)
	define_bad_type_code(0xC8)
	define_bad_type_code(0xC9)
	define_bad_type_code(0xCA)
	define_bad_type_code(0xCB)
	define_bad_type_code(0xCC)
	define_bad_type_code(0xCD)
	define_bad_type_code(0xCE)
	define_bad_type_code(0xCF)

	define_bad_type_code(0xD0)
	define_bad_type_code(0xD1)
	define_bad_type_code(0xD2)
	define_bad_type_code(0xD3)
	define_bad_type_code(0xD4)
	define_bad_type_code(0xD5)
	define_bad_type_code(0xD6)
	define_bad_type_code(0xD7)
	define_bad_type_code(0xD8)
	define_bad_type_code(0xD9)
	define_bad_type_code(0xDA)
	define_bad_type_code(0xDB)
	define_bad_type_code(0xDC)
	define_bad_type_code(0xDD)
	define_bad_type_code(0xDE)
	define_bad_type_code(0xDF)
	define_bad_type_code(0xE0)
	define_bad_type_code(0xE1)
	define_bad_type_code(0xE2)
	define_bad_type_code(0xE3)
	define_bad_type_code(0xE4)
	define_bad_type_code(0xE5)
	define_bad_type_code(0xE6)
	define_bad_type_code(0xE7)
	define_bad_type_code(0xE8)
	define_bad_type_code(0xE9)
	define_bad_type_code(0xEA)
	define_bad_type_code(0xEB)
	define_bad_type_code(0xEC)
	define_bad_type_code(0xED)
	define_bad_type_code(0xEE)
	define_bad_type_code(0xEF)
	define_bad_type_code(0xF0)
	define_bad_type_code(0xF1)
	define_bad_type_code(0xF2)
	define_bad_type_code(0xF3)
	define_bad_type_code(0xF4)
	define_bad_type_code(0xF5)
	define_bad_type_code(0xF6)
	define_bad_type_code(0xF7)
	define_bad_type_code(0xF8)
	define_bad_type_code(0xF9)
	define_bad_type_code(0xFA)
	define_bad_type_code(0xFB)
	define_bad_type_code(0xFC)
	define_bad_type_code(0xFD)
	define_bad_type_code(0xFE)
	define_bad_type_code(0xFF)

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

define(old_addr, %a2)
define(to, %a3)
define(scan, %a4)
define(fake_to,%a5)
define(fp, %a6)

define(first_word, %d0)
define(vector_count, %d1)
define(object, %d2)
define(trans_delta, %d3)
define(scan_end, %d4)
define(heap_top, %d5)
define(cons_bound, %d6)
define(pointer_mask, %d7)

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

define(make_object, `$1*0x1000000+$2')

define(null_object, make_object(TC_NULL, 0))
define(true_object, make_object(TC_TRUE, 0))

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
	link	fp,&-12			# For the debugger
	movm.l	%d2-%d7/%a2-%a5,-(%sp)

	mov.l	&0x00ffffff,pointer_mask
	clr.l	-4(fp)			# Scan bump amount
	movq	&-1,first_word
	mov.l	first_word,-(%sp)	# Mask for atomior32
	mov.l	first_word,-(%sp)	# allocate space for pointer
	mov.l	_Transport_Bound,heap_top
	mov.l	_Transport_Delta,trans_delta
#
	mov.l	_cons_heaplet_bound,cons_bound
	mov.l	12(fp),%a0
	mov.l	(%a0),fake_to		# fake_to = *To_ptr
	mov.l	fake_to,to
	add.l	trans_delta,to		# To = fake_to + Transport_Delta
	add.l	trans_delta,cons_bound
#
	mov.l	_scan_heaplet_end,scan_end
	subq.l	&4,scan_end		# Comparison excludes header
	mov.l	8(fp),scan		# Scan
	cmp.l	scan,heap_top
	bhs	gcloop_test		# Are we scanning a fixed area?
	mov.l	trans_delta,-4(fp)	# Scan bump amount
	add.l	trans_delta,scan	# We are really scanning the other heap
	add.l	trans_delta,scan_end
	bra	gcloop_test
#
gcloop_end:
	mov.l	fake_to,first_word	# Consistency check
	add.l	trans_delta,first_word
	cmp.l	first_word,to
	bne	gcloop_phase_error
#
	mov.l	12(fp),%a0
	mov.l	fake_to,(%a0)		# *To_ptr = fake_to
	mov.l	scan,%d0		# Bump Scan back and return it
	sub.l	-4(fp),%d0
#
	lea	8(%sp),%sp		# Pop space for atomior32
	movm.l	(%sp)+,%d2-%d7/%a2-%a5
	unlk	fp
	rts

gcloop_nm_header:
	and.l	pointer_mask,object
	lea	(0,scan,object.l*4),scan	# The 4 (header) will be added below
	cmp.l	scan,scan_end			# Have we exceeded scan_heaplet_end?
	bhs	gcloop_scan_overflow
#
#	fall	through
#
gcloop_dont_relocate:
gcloop_non_pointer:
	addq.l	&4,scan			# bump scan and fall through
#
#	fall	through
#
gcloop_test:
	cmp.l	to,scan			# compare Scan and To
	beq	gcloop_end
#
#	fall	through
#	
gcloop_dispatch:
	mov.l	(scan),object		# Temp
	mov.l	object,(%sp)
	clr.l	first_word
	mov.b	(%sp),first_word	# Type_Code(Temp)
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
	mov.l	(gcloop_dispatch_table - dispatch_dot,%pc,first_word.w*4),%a0
	jmp	(%a0)

#
#	Shared macros
#

define(pointer_constant_test,
	`mov.l	object,first_word
	and.l	pointer_mask,first_word
	cmp.l	first_word,heap_top	# in constant or code space?
	bhs	gcloop_dont_relocate
	mov.l	first_word,old_addr')

define(object_lock_test,
	`sub.l	old_addr,object		# Save type code and offset (if cc)
	mov.l	(old_addr),(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	$1_relocated
	mov.l	old_addr,(%sp)
	jsr	_atomior32
	cmp.l	first_word,4(%sp)	# Locked?
	beq	$1_locked
	mov.l	first_word,(%sp)
	cmpi.b	(%sp),&TC_BROKEN_HEART	# Has it been relocated?
	beq	$1_locked_relocated')

define(pointer_lock_test,`object_lock_test(gcloop)')

define(pointer_transport_setup,
	`add.l	fake_to,object		# Compute new object
	mov.l	fake_to,(%sp)
	mov.b	&TC_BROKEN_HEART,(%sp)')
	
define(pointer_end,
	`mov.l	object,(scan)+		# into Scan
	cmp.l	to,cons_bound
	bhs	gcloop_get_more_heap
	cmp.l	to,scan
	bne	gcloop_dispatch
	bra	gcloop_end')

define(pointer_start,
	`pointer_constant_test()
	pointer_lock_test()
	pointer_transport_setup()
	lea	$1*4(fake_to),fake_to')

###
###	Pointer utilities
###

gcloop_locked_relocated:
	mov.l	first_word,(old_addr)	# Unlock
#
#	fall	through
#
gcloop_relocated:
	mov.l	(%sp),first_word
#
gcloop_relocated_common:
	and.l	pointer_mask,first_word	# Get address
	add.l	first_word,object	# compute new object
	mov.l	object,(scan)+		# relocate
	cmp.l	to,scan
	bne	gcloop_dispatch
	bra	gcloop_end

gcloop_locked:
	mov.l	(old_addr),first_word	# wait until unlocked
	cmp.l	first_word,4(%sp)
	beq	gcloop_locked
	bra	gcloop_relocated_common

###
###	Small fixed length objects.
###
gcloop_trap:
	mov.l	object,first_word
	and.l	pointer_mask,first_word
	movq	&trap_max_immediate,vector_count	# Is it an immediate trap?
	cmp.l	vector_count,first_word
	bhs	gcloop_non_pointer
#
#	fall	through
#
gcloop_pair:
	pointer_start(2)
	mov.l	(%sp),(old_addr)+	# Forward and unlock
	mov.l	first_word,(to)+	# Transport
	mov.l	(old_addr),(to)+
	pointer_end()

gcloop_triple:
	pointer_start(3)
	mov.l	(%sp),(old_addr)+	# Forward and unlock
	mov.l	first_word,(to)+	# Transport
	mov.l	(old_addr)+,(to)+
	mov.l	(old_addr),(to)+
	pointer_end()

gcloop_cell:
	pointer_start(1)
	mov.l	(%sp),(old_addr)	# Forward and unlock
	mov.l	first_word,(to)+	# Transport
	pointer_end()

gcloop_quad:
	pointer_start(4)
	mov.l	(%sp),(old_addr)+	# Forward and unlock
	mov.l	first_word,(to)+	# Transport
	mov.l	(old_addr)+,(to)+
	mov.l	(old_addr)+,(to)+
	mov.l	(old_addr),(to)+
	pointer_end()

#
#	We can see whether it is spliceable before we consider
#	moving it.  This can be done because the slots examined
#	are not the header, where the lock or broken heart would
#	be stored, and thus these fields contain the same value
#	whether the object is in old space, new space, or in the
#	process of being moved by another processor.
#
gcloop_future_splice:
	mov.l	future_value_slot(%a0),(scan) # value -> *Scan
	bra	gcloop_dispatch		# look at it again.

gcloop_future:
	mov.l	object,first_word	# compute vector address
	and.l	pointer_mask,first_word
	mov.l	first_word,%a0
	cmpi.l	future_determined_slot(%a0),&true_object # is it determined?
	bne.b	gcloop_vector
	tst.l	future_lock_slot(%a0)	# is it locked?
	beq	gcloop_future_splice
#
#	fall	through
#
gcloop_vector:
	pointer_constant_test()
#
gcloop_vector_lock:
	pointer_lock_test()
	mov.l	first_word,vector_count
	and.l	pointer_mask,vector_count # Get Length
	lea	(vector_delta,to,vector_count.l*4),%a0 # compute end of copy
	cmp.l	%a0,cons_bound
	bhi	vector_get_more_heap
#
gcloop_vector_continue:
	pointer_transport_setup()
	lea	(4,fake_to,vector_count.l*4),fake_to
	mov.l	(%sp),(old_addr)+		# Forward and unlock
	mov.l	first_word,(to)+		# Move header
	bra.b	copy_vector_inner_test
#
copy_vector_outer_loop:
	swap	vector_count
copy_vector_inner_loop:
	mov.l	(old_addr)+,(to)+
copy_vector_inner_test:
	dbf	vector_count,copy_vector_inner_loop
	swap	vector_count
	dbf	vector_count,copy_vector_outer_loop
#
	pointer_end()
#
#	Compiled entries are like vectors except that the
#	vector address is computed differently.
#
gcloop_compiled:
	pointer_constant_test()
	bra.b	enter_compiled_entry_to_block_loop

compiled_entry_to_block_loop:
	lea	1(old_addr),old_addr

enter_compiled_entry_to_block_loop:
	mov.w	-2(old_addr),vector_count # compute vector address
	sub.w	vector_count,old_addr	# vector_count is sign extended first
	lsr.w	&1,vector_count		# is this an extension word?
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
	mov.w	object,vector_count	# Use vector_count as counter
	bra.b	linkage_quads_constant
#
linkage_quads_loop:
	mov.l	(scan),object
	cmp.l	object,heap_top
	blo.b	linkage_quads_relocate
#
linkage_quads_constant:
	addq.l	&4,scan			# Bump scan to next word
#
linkage_quads_test:
	dbf	vector_count,linkage_quads_loop
	bra	gcloop_test
#
linkage_quads_relocate:
	mov.l	object,old_addr
	object_lock_test(quad)
	pointer_transport_setup()
	lea	16(fake_to),fake_to	# Bump fake_to to end
	mov.l	(%sp),(old_addr)+	# Forward and unlock
	mov.l	first_word,(to)+	# Transport
	mov.l	(old_addr)+,(to)+
	mov.l	(old_addr)+,(to)+
	mov.l	(old_addr),(to)+
	mov.l	object,(scan)+
#
	cmp.l	to,cons_bound		# Do we need more space?
	blo	linkage_quads_test

#
#	Get more heap and continue the loop
#
	mov.l	vector_count,-(%sp)	# Save the counter
	pea	linkage_quads_test	# tail recurse
	bra	common_get_more_heap
#
quad_locked_relocated:
	mov.l	first_word,(old_addr)	# Unlock
#
quad_relocated:
	mov.l	(%sp),first_word
#
quad_reloacted_common:
	and.l	pointer_mask,first_word
	mov.l	first_word,(scan)+	# relocate
	bra	linkage_quads_test
#
quad_locked:
	mov.l	(old_addr),first_word	# wait until unlocked
	cmp.l	first_word,4(%sp)
	beq	quad_locked
	bra	quad_reloacted_common

###	Each entry is an absolute jmp/jsr instruction followed by a
###	word (2 bytes) of  argument count.
###	The number of entries times 2 is in object.w
###
gcloop_linkage_operator:
	mov.w	object,vector_count
	lsr.w	&1,vector_count		# Divide by 2
	addq.l	&2,scan			# Bump scan to first entry
	lea	linkage_transport_continue,%a0
	mov.l	%a0,-8(fp)		# Save return address
	bra	enter_linkage_operator_loop

linkage_operator_loop:
	addq.l	&8,scan			# Bump past this entry
	mov.l	-4(scan),old_addr	# Extract address
	cmp.l	old_addr,heap_top
	blo.b	linkage_transport_entry
#
enter_linkage_operator_loop:
	dbf	vector_count,linkage_operator_loop
#
	addq.l	&2,scan
	bra	gcloop_test		# or gcloop_dont_relocate
#
linkage_transport_entry;
	mov.w	vector_count,-12(fp)	# Save count
	mov.l	old_addr,object		# Copy entry point to save offset
	bra	enter_entry_to_block_loop
#
linkage_transport_continue:
	mov.w	-12(fp),vector_count	# Restore count
	bra	enter_linkage_operator_loop

###	Manifest closures are a header followed by cc-entry headers and
###	jsr instructions to absolute addresses.
###
gcloop_closure:
	lea	closure_loop_continue,%a0
	mov.l	%a0,-8(fp)		# return address for entry transport
	addq.l	&4,scan
	bra.b	closure_loop_continue

closure_loop:
	addq.l	&6,scan			# Bump to next entry point
	mov.l	(scan)+,old_addr	# Get entry point
	cmp.l	old_addr,heap_top	# in constant or code space?
	blo.b	closure_transport_entry
#
closure_loop_continue:
	tst.w	(scan)			# Are we done?
	bne	closure_loop
#
	mov.l	scan,object		# Round down to previous longword
	and.b	&0xfc,object
	mov.l	object,scan
	bra	gcloop_dont_relocate	# The rest are normal objects
#
#	This will be exercised extremely rarely since cc lives almost
#	exclusively in code space.
#
closure_transport_entry:
	mov.l	old_addr,object
	bra.b	enter_entry_to_block_loop

###	Shared utility for manifest_closure and operator linkage_section.
###	It expects a return address (of the loop) in -8(fp).
###	It relocates the entry in object,old_addr (2 copies).
###	It stores the new version into -4(scan).
###	scan is maintained.
###
entry_to_block_loop:
	lea	1(old_addr),old_addr

enter_entry_to_block_loop:
	mov.w	-2(old_addr),vector_count # compute vector address
	sub.w	vector_count,old_addr	# vector_count is sign extended first
	lsr.w	&1,vector_count		# is this an extension word?
	bcs.b	entry_to_block_loop
#
	object_lock_test(entry)
	mov.l	first_word,vector_count
	and.l	pointer_mask,vector_count # Get Length
	lea	(vector_delta,to,vector_count.l*4),%a0 # compute end of copy
	cmp.l	%a0,cons_bound
	bhi	entry_get_more_heap_1
#
copy_entry_continue:
	pointer_transport_setup()
	lea	(4,fake_to,vector_count.l*4),fake_to
	mov.l	(%sp),(old_addr)+		# Forward and unlock
	mov.l	first_word,(to)+		# Move header
	bra.b	copy_entry_inner_test
#
copy_entry_outer_loop:
	swap	vector_count
copy_entry_inner_loop:
	mov.l	(old_addr)+,(to)+
copy_entry_inner_test:
	dbf	vector_count,copy_entry_inner_loop
	swap	vector_count
	dbf	vector_count,copy_entry_outer_loop
#
entry_end:
	mov.l	object,-4(scan)		# into Scan
	cmp.l	to,cons_bound		# Do we need more space?
	bhs.b	entry_get_more_heap_2
	mov.l	-8(fp),%a0		# return address
	jmp	(%a0)
	
#
#	Random entry points for entry relocation.
#
entry_locked_relocated:
	mov.l	first_word,(old_addr)	# Unlock
#
entry_relocated:
	mov.l	(%sp),first_word
#
entry_relocated_common:
	and.l	pointer_mask,first_word	# Get address
	add.l	first_word,object	# compute new object
	bra	entry_end

entry_locked:
	mov.l	(old_addr),first_word	# wait until unlocked
	cmp.l	first_word,4(%sp)
	beq	entry_locked
	bra	entry_relocated_common
#
entry_get_more_heap_1:
	pea	copy_entry_continue
	bra	common_vector_get_more_heap
#
entry_get_more_heap_2:
	mov.l	-8(fp),-(%sp)		# Tail recurse
	bra	common_get_more_heap

#
#	This is currently not used, but in case it ever is.
#
gcloop_weak_pair:
	pointer_start(2)
	mov.l	(%sp),(old_addr)+	# Forward and unlock
	mov.l	first_word,vector_count	#
	and.l	pointer_mask,vector_count # Datum of car
	mov.l	vector_count,(to)+	# in new space with NULL type
	mov.l	(old_addr),(to)+	# cdr
	sub.l	vector_count,first_word	# Type of old car in high 24 bits.
	mov.l	_Weak_Chain,vector_count
	mov.l	(scan),_Weak_Chain	# Weak_Chain = Temp
	and.l	pointer_mask,vector_count # Datum of old Weak_Chain
	add.l	vector_count,first_word	# with type code of old car
	mov.l	first_word,(old_addr)	# into old cdr.
	pointer_end()

gcloop_broken_heart:
	and.l	pointer_mask,object	# Get_Pointer(Temp)
	add.l	-4(fp),object		# Bump by same amount as scan
	cmp.l	scan,object		# == Scan ?
	beq	gcloop_end
	mov.l	(scan),-(%sp)		# *Scan
	pea	broken_heart_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	12(%sp),%sp
	moveq	&term_broken_heart,first_word
	bra	gcloop_death

gcloop_get_more_heap:
	pea	gcloop_test		# Tail recurse
#	bra	common_get_more_heap
#
common_get_more_heap:	
	mov.l	fake_to,first_word	# Consistency check
	add.l	trans_delta,first_word
	cmp.l	first_word,to
	bne	gcloop_phase_error
#
	clr.l	-(%sp)			# get_new_consing_partition(fake_to, 0)
	pea	(fake_to)
	jsr	_get_new_consing_partition
	lea	8(%sp),%sp		# Pop args
	mov.l	_cons_heaplet_bound,cons_bound
	add.l	trans_delta,cons_bound
	mov.l	%d0,fake_to		# result is new fake_to
	mov.l	fake_to,to
	add.l	trans_delta,to		# To = fake_to + Transport_Delta
	rts

vector_get_more_heap:
	pea	gcloop_vector_continue	# Tail recurse
#	bra	common_vector_get_more_heap
#
#	Fall through
#
common_vector_get_more_heap:
	mov.l	first_word,-(%sp)	# save header
	mov.l	fake_to,first_word	# Consistency check
	add.l	trans_delta,first_word
	cmp.l	first_word,to
	bne	gcloop_phase_error
#
	mov.l	vector_count,-(%sp)	# get_new_consing_partition(fake_to, count)
	pea	(fake_to)
	jsr	_get_new_consing_partition
	lea	8(%sp),%sp		# Pop args
	mov.l	_cons_heaplet_bound,cons_bound
	add.l	trans_delta,cons_bound
	mov.l	%d0,fake_to		# result is new fake_to
	mov.l	fake_to,to
	add.l	trans_delta,to		# To = fake_to + Transport_Delta
	mov.l	(%sp)+,first_word	# restore header
	mov.l	first_word,vector_count
	and.l	pointer_mask,vector_count # Get Length
	lea	(vector_delta,to,vector_count.l*4),%a0
	cmp.l	%a0,cons_bound
	bhi.b	common_vector_out_of_space
	rts				# Return
#
common_vector_out_of_space:
	mov.l	_cons_heaplet_end,vector_count
	sub.l	_cons_heaplet,vector_count
	subq.l	&1,vector_count
	lsr.l	&2,vector_count		# length of heaplet in pointers
	mov.l	vector_count,-(%sp)	# effective heaplet length
	and.l	pointer_mask,first_word
	addq.l	&1,first_word
	mov.l	first_word,-(%sp)	# effective vector length
	pea	vector_too_long_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp
	moveq	&term_no_space,first_word
	bra	gcloop_death

gcloop_phase_error:
	mov.l	trans_delta,-(%sp)
	mov.l	fake_to,-(%sp)
	pea	phase_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_exit,first_word
	bra	gcloop_death

gcloop_bad_type:
	mov.l	object,-(%sp)		# Temp
	mov.l	first_word,-(%sp)	# Type_Code(Temp)
	pea	bad_type_code_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_invalid_type_code,first_word
	bra	gcloop_death

gcloop_scan_overflow:
	mov.l	scan_end,-(%sp)
	addq.l	&4,(%sp)		# Compute effective scan_heaplet_end
	pea	(scan)
	pea	nmv_overflow_error_string
	pea	_gc_death_message_buffer
	jsr	_sprintf
	lea	16(%sp),%sp		# Pop args
	moveq	&term_exit,first_word
#
#	fall	through
#
gcloop_death:
	pea	(to)
	pea	(scan)
	pea	_gc_death_message_buffer
	mov.l	first_word,-(%sp)	# Termination code
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
