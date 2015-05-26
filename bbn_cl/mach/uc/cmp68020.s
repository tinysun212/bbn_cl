### -*-Midas-*-
###
###	Copyright (c) 1987 Massachusetts Institute of Technology
###
###	This material was developed by the Scheme project at the
###	Massachusetts Institute of Technology, Department of
###	Electrical Engineering and Computer Science.  Permission to
###	copy this software, to redistribute it, and to use it for any
###	purpose is granted, subject to the following restrictions and
###	understandings.
###
###	1. Any copy made of this software must include this copyright
###	notice in full.
###
###	2. Users of this software agree to make their best efforts (a)
###	to return to the MIT Scheme project any improvements or
###	extensions that they make, so that these may be included in
###	future releases; and (b) to inform MIT of noteworthy uses of
###	this software.
###
###	3. All materials developed as a consequence of the use of this
###	software shall duly acknowledge such use, in accordance with
###	the usual standards of acknowledging credit in academic
###	research.
###
###	4. MIT has made no warrantee or representation that the
###	operation of this software will be error-free, and MIT is
###	under no obligation to provide any services, by way of
###	maintenance, update, or otherwise.
###
###	5. In conjunction with products arising from the use of this
###	material, there shall be no use of the name of the
###	Massachusetts Institute of Technology nor of any adaptation
###	thereof in any advertising, promotional, or sales literature
###	without prior written consent from MIT in each case.
###

#### $Header: cmp68020.s,v 10.0 88/12/07 13:06:00 las Exp $
#### $MIT-Header: cmp68020.s,v 9.72 88/04/27 01:09:56 GMT jinx Exp $
####
#### Compiled Code Interface for HP9000 series 300
####
#### NOTE:
####       The following code is written assuming the C calling convention
####       for the HP9000s300 compiler in HP-UX release 6.0.  Scheme does NOT
####       use the same calling convention.  Note that the C calling convention
####       on 68k Suns and butterflies is the same.
####
####       - The JSR instruction is used to invoke procedures.  RTS is used
####       to return.  Procedures will save used registers on entry, and
####       restore them prior to returning, thus using a callee saves
####       convention.
####       - Arguments are popped by the caller, and are accessible
####       immediately above the procedure's return address, in left to
####       right order for increasing stack addresses in memory.
####       - a0, a1, d0, and d1 are super temporaries which need not be saved
####       on entry.  All other registers must be saved if used.

define(PROCESSOR_TYPE, 1)		# cmp68020
define(INTERFACE_VERSION, 2)		# new closures
define(dlink, %a4)
define(rfree, %a5)
define(regs, %a6)
define(rmask, %d7)

define(switch_to_compiled_code_registers,
	`mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,rfree
	lea	_Registers,regs')

define(switch_to_interpreter_registers,
	`mov.l	rfree,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp')

define(switch_registers_before_primitive,
	`mov.l	%sp,_Ext_Stack_Pointer
	mov.l	rfree,_Free')

define(switch_registers_after_primitive,
	`mov.l	_Free,rfree')

define(compiler_invoke_continuation,
	`clr.b	(%sp)
	rts')

define(push_one,
	`mov.l	$1,-(%sp)')

define(pop_one,
	`mov.l	(%sp)+,$1')

define(pop_discard,
	`ifelse(eval($1 > 2), 1,
	`lea	eval($1 * 4)(%sp),%sp',
	eval($1 > 0), 1,
	`addq.l	&eval($1 * 4),%sp')')

define(perform_c_call,
	`jsr	$2
	pop_discard($1)')

define(extern_c_label,`_$1')	

#### use include(macros.sysV.m4) if m4 is smart

include(macros.bsd.m4)

define(push_frame_count,
	`ext.l	$1
	push($1)')

define(make_object, `$1*0x1000000+$2')

define(false_object, make_object(tc_null, 0))
define(null_object, make_object(tc_null, 0))
define(true_object, make_object(tc_true, 0))

define(push_return_code,
	`push(&false_object)
	push(&make_object(tc_return_code,$1))')

define(push_fixnum_word,
	`mov.w	$1,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)')

define(push_fixnum_long,
	`mov.l	$1,-(%sp)
	mov.b	&tc_fixnum,(%sp)')

define(push_compiled_code_block,
	`mov.l	$1,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)')

define(define_c_label,
`	global	_$1
_$1:')

define(define_debugging_label,
`	global	$1
$1:')

#
# This is used to define utility procedures called by C.
#

define(define_simple_c_procedure,
`define_c_label($1)')

#
# This is used to define procedures called by C that switch the state
# into that required to run Scheme compiled code rather than C.
#

define(define_interface_entry,
`define_c_label($1)
	movm.l	%d2-%d7/%a2-%a6,-(%sp)
	mov.l	&0x00ffffff,rmask
	switch_to_compiled_code_registers()')

define(adjust_return_address,
	`addq.l	&4,(%sp)
	mov.b	&tc_compiled_entry,(%sp)')

# The first few (/4) must match const.h

# 10 registers for the compiled code interface.
	set	regblock_memtop,0
	set	regblock_stackguard,4
	set	regblock_val,8
	set	regblock_env,12
	set	regblock_temp,16
	set	regblock_expr,20
	set	regblock_return,24
	set	regblock_lexpr_actuals,28
#	set	regblock_spare,32
#	set	regblock_spare,36

# 50 registers for compiled code temporaries.
	set	regblock_temporaries,40
	set	regblock_ntemps,50

# 10 special entry points (internal to the compiled code interface)
	set	regblock_hooks,(regblock_temporaries + (regblock_ntemps * 4))
	set	regblock_nhooks,10
	set	offset_return_to_interpreter,regblock_hooks
	set	offset_operator_lookup_trap,(offset_return_to_interpreter + 6)
	set	offset_operator_interpreted_trap,(offset_operator_lookup_trap + 6)
	set	offset_operator_arity_trap,(offset_operator_interpreted_trap + 6)
	set	offset_operator_apply_trap,(offset_operator_arity_trap + 6)

# 50 6-byte entry points called as utilities by compiled code.
	set	regblock_entries,(regblock_hooks + (regblock_nhooks * 6))
	set	regblock_nentries,50
	set	regblock_length,(regblock_entries + (regblock_nentries * 6))

# types and other constants required.

	set	prim_done,-1
	set	prim_do_expression,-2
	set	prim_apply,-3
	set	prim_interrupt,-4
	set	prim_no_trap_eval,-5
	set	prim_no_trap_apply,-6
	set	prim_pop_return,-7
	set	prim_touch,-8
	set	prim_apply_interrupt,-9

	set	primitive_index_mask,0x00000fff

	set	tc_null,0x00
	set	tc_manifest_vector,0x00
	set	tc_list,0x01
	set	tc_flonum,0x06
	set	tc_true,0x08
	set	tc_vector,0x0a
	set	tc_return_code,0x0b
	set	tc_manifest_closure,0x0d
	set	tc_environment,0x12
	set	tc_fixnum,0x1a
	set	tc_hunk3,0x24
	set	tc_manifest_nm_vector,0x27
	set	tc_compiled_entry,0x28
	set	tc_reference_trap,0x32
	set	tc_quad,0x38
	set	tc_linkage_section,0x39
	set	tc_stack_environment,0x3b
	set	tc_compiled_code_block,0x3d

	set	err_unbound_variable,0x01
	set	err_unassigned_variable,0x02
	set	err_inapplicable_object,0x03
	set	err_wrong_number_of_arguments,0x0c
	set	err_compiled_code_error,0x31
	set	err_unimplemented_primitive,0x33

	set	int_gc_bit,2

	set	fobject_compiler_error_procedure,0x21*4

	set	rc_comp_reference_restart,0x1f
	set	rc_comp_assignment_restart,0x28
	set	rc_comp_interrupt_restart,0x43
	set	rc_comp_lookup_apply_restart,0x4b
	set	rc_comp_access_restart,0x4c
	set	rc_comp_unassigned_p_restart,0x4d
	set	rc_comp_unbound_p_restart,0x4e
	set	rc_comp_definition_restart,0x4f
	set	rc_comp_safe_reference_restart,0x51
	set	rc_comp_lookup_trap_restart,0x53
	set	rc_comp_assignment_trap_restart,0x54
	set	rc_comp_op_lookup_trap_restart,0x56
	set	rc_comp_cache_lookup_apply_restart,0x57
	set	rc_comp_safe_lookup_trap_restart,0x58
	set	rc_comp_unassigned_p_trap_restart,0x59
	set	rc_comp_link_caches_restart,0x5b

	set	trap_unassigned,0
	set	trap_max_immediate,9
	set	trap_fluid,12

###
### Global data
###

	data
define_c_label(Registers)
	space	regblock_length
define_c_label(compiler_utilities)
	space	4
define_c_label(compiler_interface_version)
	space	4
define_c_label(compiler_processor_type)
	space	4
define_c_label(return_to_interpreter)
	space	4
define_debugging_label(c_save_stack)
	space	4
define_debugging_label(data_patch_area)
	space	64

	text

define_debugging_label(code_patch_area)
	space	256	
###
### Entries from the interpreter
###

### Compiled code entry points:
###
### There are 4 kinds of pointers to compiled code:
###
### - compiled expressions.  They correspond to the top level of
### files, SCODE-QUOTEs and IN-PACKAGEs.  The only meaningful
### action is to EVAL them.
###
### - compiled code return addresses.  They are the entry points
### of continuations created by compiled code.  The only
### meaningful action is to "return" to them.
###
### - compiled procedures.  They are the entry points of
### procedures created by compiled code.  The only meaningful
### action is to APPLY them to arguments.
###
### - compiled entries.  They are the entry points of internal
### procedures which for some reason (ie. interrupt) have been
### exported.  There is no inherent meaningful action.  The code
### that exported them will set up a return address/code to use
### them in the appropriate way.

### Format:
###
### All of the objects described above are represented as pointers
### to the appropriate instruction (labelled LABEL below)with a
### TC_COMPILED_ENTRY type code.  The two words (each word is 2
### bytes) preceding the instruction must have the following
### format:
###
### 	dc.w	<format word>
### 	dc.w	<gc word>
### label:
### 	<instructions>

### The gc word contains the offset to the beginning of the object
### to the middle of which the address points.  It is used by all
### relocators (including the garbage collector) to move the
### storage as a unit.  See compiled_entry_to_block for its
### interpretation.

### The format word varies with the type of object:
###
### - For compiled expressions it is always 0xffff (-1).
###
### - For compiled code return addresses it is currently 0x8080.
### Eventually the spare bits will be used to describe where to find
### the previous return address on the stack.  The only constraints
### are that it must not be 0xfff[d-f], and that the most
### significant bit of BOTH bytes MUST BE 1.
###
### - For compiled procedures, the format word describes the
### arity and the format of the frame on the stack:
###
### The high order byte is (1+ REQ) where REQ is the number of
### required arguments.  Note that REQ must be less than 127!
###
### The low order byte is given by the expression
### (* (EXPT -1 TAIL?) FRAME-SIZE)
### where FRAME-SIZE is (+ 1 REQ OPT TAIL?), REQ is as above, OPT
### is the number of named optional arguments, and TAIL? is 1 if
### the procedure has a tail parameter (ie. it is a "lexpr"), or 0
### otherwise.  Note that FRAME-SIZE must be less than 127!
###
### - For compiled entries it is always 0xfff[d-e] (- 3 or - 2).
### It is 0xfffe for compiler generated entries, 0xfffd for
### compiler-interface generated entries.

### RETURN_TO_COMPILED_CODE assumes that the object on the stack
### IS a valid return address.
### It also copies VAL to d0 since some routines expect it there.

define_interface_entry(return_to_compiled_code)
	mov.l	regblock_val(regs),%d0
	compiler_invoke_continuation()

### ENTER_COMPILED_EXPRESSION must first determine whether the
### expression is a compiled expression or anything else.  If it is
### NOT a compiled expression, it is self evaluating.  It assumes that
### it has the correct type code (currently this entry point is
### invoked only from EVAL, which dispatches on type).

define_interface_entry(enter_compiled_expression)
	mov.l	regblock_expr(regs),%d0
	and.l	rmask,%d0
	mov.l	%d0,%a0
	cmpi.w	-4(%a0),&0xffff
	bne.b	self_evaluate
	jmp	(%a0)

self_evaluate:
	mov.l	regblock_expr(regs),%d0
	mov.l	%d0,regblock_val(regs)
	compiler_invoke_continuation()

define_debugging_label(comentry_return_to_interpreter)
	movq	&prim_done,%d0

define_debugging_label(return_to_interpreter)
	switch_to_interpreter_registers()
	movm.l	(%sp)+,%d2-%d7/%a2-%a6
	rts

define_debugging_label(return_to_interpreter_error)
	mov.l	_compiled_code_error_code,%d0
	tst.l	%d0
	blt.b	return_to_interpreter
	mov.l	&err_compiled_code_error,%d0
	bra.b	return_to_interpreter

### comentry_error
### comentry_apply
###
### comentry_error is used by compiled code to signal an error.  It
### expects the arguments to be pushed on the stack, and the count
### (arguments + 1) to be in d0.w (no type code needed).
###
### comentry_apply is used by compiled code when calling unknown
### procedures. It expects the procedure and arguments to be pushed on
### the stack, and the count to be in d0.w (no type code needed).

define_debugging_label(comentry_error)
	mov.l	%d0,-(%sp)		# Save the count
	jsr     _Compiler_Get_Fixed_Objects
	mov.l   %d0,%d1			# FOV
	mov.l	(%sp)+,%d0		# Restore the count
	and.l	rmask,%d1
	mov.l	%d1,%a0
	push(fobject_compiler_error_procedure(%a0))
### ...fall through and apply the error procedure

define_debugging_label(comentry_apply)
	cmp.b	(%sp),&tc_compiled_entry
	beq.b	apply_compiled_procedure_1
comentry_apply_interpreted:
	push_frame_count(%d0)
	movq	&prim_apply,%d0
	bra	return_to_interpreter	

### comentry_lexpr_apply
###
### This entry point is invoked when compiled code calls a known
### lexpr, and the frame must be reformatted.  a0 contains the label
### to invoke, and d0.w contains the (apply) frame size.
### Important: This assumes that it is always invoked with a valid
### number of arguments (the compiler checked it), and will not check.

define_debugging_label(comentry_lexpr_apply)
	mov.b	-3(%a0),%d1
	ext.w	%d1
	bra.w	invoke_lexpr

### apply_compiled_procedure
###
### apply_compiled_procedure must determine whether the object is
### a procedure or a different kind of entry point.  It must also
### check the number of arguments, and set the frame up.  The type
### check and arity check are combined, since the representation
### for expressions and return addresses is designed to make them
### look like lexprs to the code below.

define_interface_entry(apply_compiled_procedure)
	pop(%d0)			# Get frame length in d0.

define_debugging_label(apply_compiled_procedure_1)
	mov.l	(%sp)+,%d1		# Get procedure.

define_debugging_label(apply_compiled_procedure_2)
	and.l	rmask,%d1
	mov.l	%d1,%a0			# Entry point address

define_debugging_label(apply_compiled_procedure_3)
	mov.b	-3(%a0),%d1		# FRAME-SIZE
	ext.w	%d1
	cmp.w	%d0,%d1			# eq => not lexpr and frame
	bne.b	max_argument_mismatch	# already set up.
	jmp	(%a0)			# Invoke

not_a_procedure:
	pea	(%a0)
	mov.b	&tc_compiled_entry,(%sp)
	push_frame_count(%d0)
	mov.l	&err_inapplicable_object,%d0
	bra.w	return_to_interpreter

too_many_arguments:
too_few_arguments:
	pea	(%a0)
	mov.b	&tc_compiled_entry,(%sp)
	push_frame_count(%d0)
	mov.l	&err_wrong_number_of_arguments,%d0
	bra.w	return_to_interpreter

max_argument_mismatch:
	mov.b	-4(%a0),%d2		# (1+ REQ)
	bmi.b	not_a_procedure		# Expression, ret. add., local lexpr.
	ext.w	%d2
	cmp.w	%d0,%d2
	blt.b	too_few_arguments

define_debugging_label(invoke_lexpr)
	mov.w	%d0,%d2
	sub.w	%d1,%d2
	bgt.b	too_many_arguments_or_lexpr
	pea	(%a0)			# Make open_gap tail_recurse
					# into the procedure.

### Open a gap for the missing optionals, and fill them in.
### Note that MAX_ARGUMENT_MISMATCH falls through to this code!

define_debugging_label(open_gap)
	lea	(0,%sp,%d2.w*4),%a1
	exg	%a1,%sp
	mov.l	%sp,%a2
	subq.w	&1,%d0

open_optionals_gap_loop:
	mov.l	(%a1)+,(%a2)+
	dbf	%d0,open_optionals_gap_loop

	mov.l	&make_object(tc_reference_trap, 0),%d0
	neg.w	%d2
	subq.w	&1,%d2
	
fill_optionals_loop:
	mov.l	%d0,(%a2)+
	dbf	%d2,fill_optionals_loop

	rts

### Dispatch according to whether not a lexpr, not enough
### arguments supplied, or a list must be consed.

too_many_arguments_or_lexpr:
	neg.w	%d1			# Frame size
	bmi.b	too_many_arguments
	mov.w	%d0,%d2
	sub.w	%d1,%d2
	bgt.b	cons_up_tail
	beq.b	cons_singleton
	bsr.b	open_gap
	mov.l	&null_object,-4(%a2)	# Smash last "optional"
	jmp	(%a0)			# Invoke

### Trivial case: The frame has the right size, we must make a
### list of a single element, the last argument.  This code
### assumes there is enough space to cons one pair.

cons_singleton:
	lea	(-8,%sp,%d0.w*4),%a1
	mov.l	rfree,%d1		# Cons
	mov.l	(%a1),(rfree)+
	mov.l	&null_object,(rfree)+
	mov.l	%d1,(%a1)
	mov.b	&tc_list,(%a1)
	jmp	(%a0)			# Invoke

### Hair squared, a rest argument must be consed up, and the frame
### shifted so that the procedure can find all the arguments in
### the right place.

cons_up_tail:
	lea	(8,rfree,%d2.w*8),%a1
	cmp.l	%a1,regblock_memtop(regs)
	bge.b	cons_tail_interrupt

cons_up_continue:
	mov.l	%a1,rfree		# Cons
	lea	(-4,%sp,%d0.w*4),%a2	# End of frame
	mov.l	%a2,%a3			# Save for CLOSE_GAP below
	mov.l	&null_object,-(%a1)	# End of list
	subq.w	&1,%d2

cons_tail_loop:
	mov.l	-(%a2),-(%a1)		# Arguments to the list
	mov.l	%a1,-(%a1)
	mov.b	&tc_list,(%a1)
	dbf	%d2,cons_tail_loop

	mov.l	-(%a2),-(%a1)		# First tail argument
	mov.l	%a1,(%a2)		# Clobber first tail argument
	mov.b	&tc_list,(%a2)		# with list
	addq.l	&4,%a2			# End of frame
	subq.w	&2,%d1

close_gap_loop:
	mov.l	-(%a2),-(%a3)		# Shift frame
	dbf	%d1,close_gap_loop

	mov.l	%a3,%sp			# Adjust stack pointer
	jmp	(%a0)			# Finally, invoke!
	
### Determine whether interrupts/gc allowed, continue if not.

cons_tail_interrupt:
	tst.b	regblock_memtop(regs)	# Interrupt or GC?
	bmi.b	cons_tail_backout
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	beq.b	cons_up_continue

cons_tail_backout:
	pea	(%a0)
	mov.b	&tc_compiled_entry,(%sp)
	push_frame_count(%d0)
	mov.l	&prim_apply_interrupt,%d0
	bra.w	return_to_interpreter

### compiled_entry_to_block
###
### Expects a Scheme object representing a compiled code entry point in d1.
### Returns the address of the block to which it belongs in a0.

define_debugging_label(compiled_entry_to_block)
	and.l	&0xffffff,%d1
	mov.l	%d1,%a0
	bra.b	enter_compiled_entry_to_block_loop

compiled_entry_to_block_loop:
	lea	1(%a0),%a0

enter_compiled_entry_to_block_loop:
	mov.w	-2(%a0),%d1
	sub.w	%d1,%a0
	lsr.w	&1,%d1
	bcs.b	compiled_entry_to_block_loop
	rts

### compiled_block_environment
###
### Given a compiled code block, it extracts the environment where
### the block was "loaded".

define_simple_c_procedure(compiled_block_environment)
	mov.l	4(%sp),%d0
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d0
	and.l	&0xffffff,%d0
	mov.l	(0,%a0,%d0.l*4),%d0
	rts

### compiled_entry_to_block_address
###
### C entry point to compiled_entry_to_block

define_simple_c_procedure(compiled_entry_to_block_address)
	mov.l	4(%sp),%d1
	bsr	compiled_entry_to_block
	mov.l	%a0,%d0
	rts

### compiled_entry_to_block_offset
###
### Computes the offset from the block to the entry point
	
define_simple_c_procedure(compiled_entry_to_block_offset)
	mov.l	4(%sp),%d1
	bsr	compiled_entry_to_block
	mov.l	4(%sp),%d0
	and.l	&0xffffff,%d0
	sub.l	%a0,%d0
	rts

### compiled_entry_type
###
### Stores the entry information into a buffer specified

define_simple_c_procedure(compiled_entry_type)
	mov.l	%d2,-(%sp)		# Save old d2
	mov.l	12(%sp),%a0		# buffer
	mov.l	8(%sp),%d0		# entry
	and.l	&0xffffff,%d0
	mov.l	%d0,%a1
	clr.l	%d0			# type
	mov.b	-3(%a1),%d2		# max
	extb.l	%d2
	mov.b	-4(%a1),%d1		# min
	extb.l	%d1
	bge.b	kind_computed		# it is a procedure
	addq.l	&1,%d0
	cmp.b	%d1,&0xff
	bne.b	kind_computed		# it is a return address
	clr.l	%d1			# min is void
	addq.l	&1,%d0
	cmp.b	%d2,&0xff
	beq.b	kind_trivial		# it is an expression
	addq.l	&1,%d0
kind_trivial:
	clr.l	%d2			# max is void
kind_computed:
	mov.l	%d0,(%a0)+
	mov.l	%d1,(%a0)+
	mov.l	%d2,(%a0)
	mov.l	(%sp)+,%d2
	rts

### extract_variable_cache
###
### Given a compiled code block, and an offset, it extracts the
### variable cache at that location.

define_simple_c_procedure(extract_variable_cache)
	mov.l	4(%sp),%d0		# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	8(%sp),%d0		# offset
	mov.l	(0,%a0,%d0.l*4),-(%sp)
	mov.b	&tc_quad,(%sp)
	mov.l	(%sp)+,%d0
	rts

### store_variable_cache
###
### Given a variable cache, a compiled code block, and an offset,
### it stores the variable cache at that location.

define_simple_c_procedure(real_store_variable_cache)
	mov.l	8(%sp),%d0		# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	12(%sp),%d0		# offset
	mov.l	4(%sp),%d1
	and.l	&0x00ffffff,%d1
	mov.l	%d1,(0,%a0,%d0.l*4)	# Store!
	rts

### extract_uuo_link
###
### Get a compiled procedure from a cached operator reference.

define_simple_c_procedure(extract_uuo_link)
	mov.l	4(%sp),%d0			# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	8(%sp),%d0			# offset
	mov.l	(2,%a0,%d0.l*4),-(%sp)		# address
	mov.b	&tc_compiled_entry,(%sp)	# procedure
	mov.l	(%sp)+,%d0
	rts

### comentry_operator_apply_trap
###
### Called when an object "returned" by coerce_to_compiled is invoked
### and it was not a compiled procedure or there was a number of
### arguments mismatch

define_debugging_label(comentry_operator_apply_trap)
	mov.l	(%sp),%a0		# Fake procedure
	mov.l	(%a0),(%sp)		# Real procedure
	clr.w	%d0
	mov.b	-8(%a0),%d0		# Number of arguments
	bra	comentry_apply

### comentry_operator_arity_trap
###
### Invoked when compiled code calls a cached procedure which expects
### a different number of arguments from that supplied by the caller.
### An error may be generated or the frame may be reformatted.  It
### expects the stack to contain the fake procedure.

define_debugging_label(comentry_operator_arity_trap)
	mov.l	(%sp)+,%a0		# Fake procedure
	mov.l	(%sp)+,%a1		# Fake continuation
	mov.w	(%a1),%d0		# Number of arguments
	mov.l	(%a0),%d1		# Real procedure
	bra	apply_compiled_procedure_2

### comentry_operator_interpreted_trap
###
### Called when compiled code caches a procedure which is not compiled.
### Typically the call must be performed by apply.

define_debugging_label(comentry_operator_interpreted_trap)
	mov.l	(%sp)+,%a0		# Fake procedure
	mov.l	(%sp),%a1		# Fake continuation
	mov.w	(%a1),%d0		# Number of arguments
	mov.l	(%a0),(%sp)		# Real procedure
	push_frame_count(%d0)
	movq	&prim_apply,%d0
	bra	return_to_interpreter	

### coerce_to_compiled
###
### This is given an object, a number, and a location.  It stores in
### the location a compiled procedure which expects that number of
### arguments and invokes the object.  If the object is already a
### compiled procedure which expects that number of arguments, the
### object is stored.  Otherwise a fake compiled procedure (which
### invokes comentry_operator_apply_trap) is created (like with uuo
### links below) and stored.  It is always safe to invoke the stored
### object with the specified number arguments on the stack.  It will 
### take care of signalling an error when appropriate.

define_simple_c_procedure(coerce_to_compiled)
	mov.l	12(%sp),%a0		# block
	cmp.b	4(%sp),&tc_compiled_entry
	bne.b	coerce_to_compiled_invoke_apply
	mov.l	4(%sp),%d0
	and.l	&0x00ffffff,%d0
	mov.l	%d0,%a1
	mov.b	-3(%a1),%d0
	extb.l	%d0
	subq.l	&1,%d0			# Procedure itself
	cmp.l	%d0,8(%sp)
	bne.b	coerce_to_compiled_invoke_apply
	mov.l	4(%sp),(%a0)		# Store
	movq	&prim_done,%d0
	rts

coerce_to_compiled_too_large:
	movq	&err_wrong_number_of_arguments,%d0
	rts

coerce_to_compiled_invoke_apply:
	moveq	&5,%d0
	bsr.w	allocate_heap_fake_operator
	tst.l	%d0
	beq.b	coerce_to_compiled_interrupt
	mov.l	8(%sp),%d0
	addq.l	&1,%d0
	mov.b	%d0,%d1
	bmi.b	coerce_to_compiled_too_large # overflow ?
	lsl.l	&8,%d1
	mov.b	%d0,%d1
	mov.l	&make_object(tc_manifest_vector,4),(%a1)+
	mov.l	&make_object(tc_manifest_nm_vector,2),(%a1)+
	mov.w	%d1,(%a1)+
	mov.w	&0x000c,(%a1)+
	mov.l	%a1,(%a0)		# store fake operator
	mov.b	&tc_compiled_entry,(%a0)
	mov.w	&0x4eae,(%a1)+		# JSR <offset>(a6) opcode
	mov.w	&offset_operator_apply_trap,(%a1)+
	mov.l	4(%sp),(%a1)		# real operator
	movq	&prim_done,%d0
	rts

coerce_to_compiled_interrupt:
	movq	&prim_interrupt,%d0
	rts

###	allocate_constant_fake_operator
###
###	- Expects: The number of words (n) to allocate in %d0.
###	- Returns: a block of memory n words long in constant
###	space and a boolean success value in %d0.
###	- Preserves: %a0, %a2-%a7, %d2-%d7.

define_debugging_label(allocate_constant_fake_operator)
	mov.l	%a0,-(%sp)		# Preserve a0
	c_call_c(Atomic_Allocate,%d0)
	mov.l	(%sp)+,%a0		# Restore a0
	tst.l	%d0
	bne.b	allocate_constant_continue
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	lea	_Registers,%a1		# Canonicalize interrupt
	mov.l	&-1,regblock_memtop(%a1)
	clr.l	%d0
	rts
#
allocate_constant_continue:
	mov.l	%d0,%a1			# Chunk of memory
	movq	&1,%d0
	rts

###	allocate_heap_fake_operator
###
###	- Expects: The number of words (n) to allocate in %d0.
###	- Returns: a block of memory n words long in the heap
###	and a boolean success value in %d0.
###	- Preserves: %a0, %a2-%a7, %d2-%d7.

define_debugging_label(allocate_heap_fake_operator)
	mov.l	_Free,%a1
	cmp.l	%a1,_MemTop
	blt.b	allocate_heap_succeed
#
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	bne.b	allocate_heap_succeed
	lea	_Registers,%a1		# Canonicalize interrupt
	mov.l	&-1,regblock_memtop(%a1)
	clr.l	%d0
	rts
#
allocate_heap_succeed:
	pea	(0,%a1,%d0.l*4)		# End of area
	mov.l	(%sp)+,_Free		# Allocate
	movq	&1,%d0			# Success
	rts

###	allocate_fake_operator:
###
###	- Expects: the operator cache address in %a0,
###	and the number of words (n) to allocate in %d0.
###	- Returns: a block of memory n words long in %a1,
###	and a boolean success value in %d0.
###	The block of memory may be in the heap or in constant
###	space, depending on whether it is safe to link into
###	the cache address.
###	- Preserves: %a0, %a2-%a7, %d2-%d7.

define_debugging_label(allocate_fake_operator)
	mov.l	%d0,-(%sp)		# Preserve n
	mov.l	%a0,-(%sp)		# Preserve cache address
	c_call_c(unsafe_to_link_test,%a0) # can we link?
	mov.l	%d0,%d1
	mov.l	(%sp)+,%a0		# Restore cache address
	mov.l	(%sp)+,%d0		# Restore n
	tst.l	%d1
	bne	allocate_constant_fake_operator
	bra	allocate_heap_fake_operator

### fake_uuo_link_p
###
### Given a procedure and an extension object, it returns true if the
### procedure is a fake uuo link to the value cell contained in the
### extension object, false otherwise.  It depends on the precise
### format laid by make_fake_uuo_link.

define_simple_c_procedure(fake_uuo_link_p)
	mov.l	4(%sp),%d0		# procedure
	and.l	&0x00ffffff,%d0		# address
	mov.l	%d0,%a0
	mov.l	&0xfffd000c,%d0
	cmp.l	%d0,-4(%a0)
	bne.b	not_fake_uuo_link_p
	mov.l	8(%sp),%d0
	cmp.l	%d0,4(%a0)		# extension is eq?
	bne.b	not_fake_uuo_link_p
	movq	&1,%d0
	rts

not_fake_uuo_link_p:	
	clr.l	%d0
	rts

### make_uuo_link
###
### This is called by C and initializes a compiled procedure cache at
### a location given by a block and an offset.
###
### 	make_uuo_link checks its operator argument, and:
###
### - If it is not a compiled procedure, it stores a fake compiled
### procedure which will invoke comentry_operator_interpreted_trap
### when invoked.
###
### - If its argument is a compiled procedure that expects more or
### less arguments than those provided, it stores a fake compiled
### procedure which will invoke comentry_operator_arity_trap when
### invoked.
###
### - If the compiled procedure lies in the heap, but the cache cell is
### in pure space (or it is otherwise unsafe to link), it allocates a
### fake procedure in constant space and stores it in the cache cell.
### This fake compiled procedure which will invoke the actual operator
### when called, but will avoid potential GC problems.
###
### - Otherwise, the actual (compatible) operator is stored.

define_simple_c_procedure(real_make_uuo_link)
	mov.l	12(%sp),%d0		# block
	and.l	&0x00ffffff,%d0
	mov.l	%d0,%a0
	mov.l	16(%sp),%d0		# offset
	lea	(0,%a0,%d0.l*4),%a0	# Execute cache cell address
	cmp.b	4(%sp),&tc_compiled_entry
	bne.w	make_uuo_link_1
	mov.l	4(%sp),%d0		# operator
	and.l	&0x00ffffff,%d0
	mov.l	%d0,%a1
	mov.b	-3(%a1),%d0		# FRAME-SIZE expected
	ext.w	%d0
	cmp.w	%d0,6(%a0)		# Number of arguments supplied
	bne.b	make_uuo_link_2
	pea	(%a0)			# preserve cell address
	pea	(%a1)			# preserve address of operator
	c_call_c(constant_test,%a1)	# is it in constant space?
	tst.l	%d0
	beq.b	make_uuo_link_3		# branch if it is not

define_debugging_label(make_uuo_link_continue)
	mov.l	(%sp)+,%a1		# address of operator
	mov.l	(%sp)+,%a0		# cell address
	mov.w	&0x4ef9,(%a0)+		# JMP <value>.L opcode
	mov.l	%a1,(%a0)		# operator address
	movq	&prim_done,%d0
	rts

define_debugging_label(make_uuo_link_interrupt)
	movq	&prim_interrupt,%d0
	rts

define_debugging_label(make_uuo_link_3)
	mov.l	4(%sp),%a0		# cell address
	c_call_c(unsafe_to_link_test,%a0) # can we link anyway?
	tst.l	%d0
	beq.b	make_uuo_link_continue	# branch if it is not
	mov.l	(%sp)+,%a1		# address of operator
	mov.l	(%sp)+,%a0		# cell address
	movq	&4,%d0
	bsr	allocate_constant_fake_operator
	tst.l	%d0			# Did we succeed?
	beq	make_uuo_link_interrupt
	mov.w	6(%a0),%d0		# number of args
	mov.w	%d0,%d1			# construct code word
	lsl.l	&8,%d1
	mov.b	%d0,%d1
	mov.w	&0x4ef9,(%a0)+		# JMP <fake>.L opcode
	mov.l	&make_object(tc_manifest_closure,3),(%a1)+
	mov.w	%d1,(%a1)+		# code word
	mov.w	&8,(%a1)+
	mov.l	%a1,(%a0)		# fake operator address
	mov.w	&0x4ef9,(%a1)+		# JMP <value>.L opcode
	mov.l	4(%sp),(%a1)+		# real operator
	clr.b	-4(%a1)			# Remove type code
	clr.w	(%a1)			# End of closure
	movq	&prim_done,%d0
	rts

define_debugging_label(make_uuo_link_2)
	mov.w	&offset_operator_arity_trap,%d1
	bra.b	make_simple_fake_operator

define_debugging_label(make_uuo_link_1)
	mov.w	&offset_operator_interpreted_trap,%d1

define_debugging_label(make_simple_fake_operator)
	mov.l	%d1,-(%sp)		# Preserve trap offset
	movq	&5,%d0
	bsr	allocate_fake_operator
	mov.l	(%sp)+,%d1		# Restore trap offset
	tst.l	%d0			# Did we succeed?
	beq	make_uuo_link_interrupt
	mov.w	&0x4eb9,(%a0)+		# JSR <value>.L opcode
	mov.l	&make_object(tc_manifest_vector,4),(%a1)+
	mov.l	&make_object(tc_manifest_nm_vector,2),(%a1)+
	mov.l	&0xfffd000c,(%a1)+
	mov.l	%a1,(%a0)		# fake operator address
	mov.w	&0x4eae,(%a1)+		# JSR <offset>(a6) opcode
	mov.w	%d1,(%a1)+
	mov.l	4(%sp),(%a1)		# real operator
	movq	&prim_done,%d0
	rts

### make_fake_uuo_link
###
### Makes a fake compiled procedure which calls
### comentry_operator_lookup_trap when invoked.

define_simple_c_procedure(real_make_fake_uuo_link)
	mov.l	8(%sp),%d0		# block
	and.l	&0x00ffffff,%d0
	mov.l	%d0,%a0
	mov.l	12(%sp),%d0		# offset
	lea	(0,%a0,%d0.l*4),%a0	# Execute cache cell address
	movq	&7,%d0
	bsr	allocate_fake_operator
	tst.l	%d0			# Did we succeed?
	beq.b	make_fake_uuo_link_interrupt
	mov.w	&0x4eb9,(%a0)+		# JSR <value>.L opcode
	mov.l	&make_object(tc_manifest_vector,6),(%a1)+
	mov.l	&make_object(tc_manifest_nm_vector,2),(%a1)+
	mov.l	&0xfffd000c,(%a1)+
	mov.l	%a1,(%a0)		# fake operator address
	mov.w	&0x4eae,(%a1)+		# JSR <offset>(a6) opcode
	mov.w	&offset_operator_lookup_trap,(%a1)+
	mov.l	4(%sp),(%a1)+		# extension
	mov.l	8(%sp),(%a1)+		# block
	mov.l	12(%sp),(%a1)		# offset
	mov.b	&tc_fixnum,(%a1)
	movq	&prim_done,%d0
	rts

define_debugging_label(make_fake_uuo_link_interrupt)
	movq	&prim_interrupt,%d0
	rts

### comentry_interrupt_closure
###
### In all of the following it is assumed that regblock_memtop = 0,
### and that the compiler makes use of this when generating an addressing
### mode.
###
### We are expecting the compiler to generate the following code at
### a closure entry point:
###
###	label1:
###		jmp	regblock_comentry_interrupt_closure(regs)
###		dc.w	<format word>
###		dc.w	<offset to block start for gc of tc_compiled_entry>
###	entry_label:
###		add.l	&magic_constant,(%sp)
###		cmp.l	rfree,regblock_memtop(regs)
###		bge.b	label1
###
### comentry_interrupt_procedure
### comentry_interrupt_continuation
### comentry_interrupt_ic_procedure
###
### We are expecting the compiler to generate the following code at
### a procedure or continuation entry point:
###
###	label1:
###		jsr	regblock_comentry_interrupt_procedure(regs)
###		dc.w	<format word>
###		dc.w	<offset to block start for gc of tc_compiled_entry>
###	entry_label:
###		cmp.l	rfree,regblock_memtop(regs)
###		bge.b	label1

### Hack: This handles returns from all interrupts.  Masking val into
### dlink is harmless when it is not a dynamic link.  It is the right
### thing when it is, thus it is OK to do it always.  Same for moving
### it into regblock_env(regs)

define_interface_entry(comp_interrupt_restart)
	pop(%d0)
	mov.l	%d0,regblock_val(regs)	# Restore VAL.
	mov.l	%d0,regblock_env(regs)	# Restore ENV.
	and.l	rmask,%d0
	mov.l	%d0,dlink		# Restore dlink
	compiler_invoke_continuation()

define_debugging_label(ignore_interrupt_normal)
	pop_discard(1)
	clr.b	(%sp)			# Resume, skipping past entry test.
	pop(%a0)
	jmp	4(%a0)

define_debugging_label(ignore_interrupt_closure)
	pop_discard(1)
	mov.l	(%sp),%d0		# Load closure without popping
	and.l	rmask,%d0
	mov.l	%d0,%a0
	mov.l	2(%a0),%a0		# Real entry point
	jmp	10(%a0)			# Resume, skipping past entry test.

### Hack: Instead of pushing a continuation, we use the closure itself
### to restart the code.  When the interrupt proceeds, the closure
### will be "reinvoked", and it will be pushed on the stack automatically.

define_debugging_label(comentry_interrupt_closure)
	push(&false_object)
	lea	ignore_interrupt_closure,%a0
	bra.b	comentry_interrupt_common

define_debugging_label(comentry_interrupt_continuation)
	adjust_return_address()
	push(regblock_val(regs))	# Save VAL.
	lea	ignore_interrupt_normal,%a0
	bra.b	comentry_interrupt_common

define_debugging_label(comentry_interrupt_ic_procedure)
	adjust_return_address()
	push(regblock_env(regs))
	lea	ignore_interrupt_normal,%a0
	bra.b	comentry_interrupt_common	

### Procedures require the dynamic link, if there is one.
### This piece of code does not know whether the interrupted procedure
### uses a dynamic link or no, so it tries to guess that information,
### and save it if it decides there was in fact a dynamic link.  The
### restart code does the right thing anyway.  This code assumes that
### the dynamic link is always in the dlink register. 
###
### Continuations require VAL, so it is saved.
### Note that continuations never need the dynamic link, since it was
### saved by the caller.

### The heuristic used to determine whether dlink contains a dynamic
### link is as follows:
### - If the contents of dlink have a type code, there is no dynamic
###   link.
### - If the contents of dlink do not have the same (longword)
###   alignment as the Stack Pointer, there is no dynamic link.
### - If the contents of dlink point outside the interval
###   (Stack_Pointer, Stack_Top), there is no dynamic link.
###
### This should be fixed in the future by having a separate entry
### point for proce

define_debugging_label(comentry_interrupt_procedure)
	adjust_return_address()
	push(dlink)
	lea	ignore_interrupt_normal,%a0
	tst.b	(%sp)
	bne.b	interrupt_no_dlink
	mov.b	&tc_stack_environment,(%sp)
	cmp.l	dlink,%sp
	bls.b	interrupt_no_dlink
	cmp.l	dlink,_Stack_Top
	bhs.b	interrupt_no_dlink
	mov.l	dlink,%d1
	sub.l	%sp,%d1
	and.l	&0x00000003,%d1
	beq.b	comentry_interrupt_common
	
interrupt_no_dlink:
	mov.l	&false_object,(%sp)

define_debugging_label(comentry_interrupt_common)
	tst.b	regblock_memtop(regs)	# Interrupt or GC?
	bmi.b	comentry_interrupt
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	bne.b	comentry_interrupt_common_1
	jmp	(%a0)			# interrupt ignore code

comentry_interrupt_common_1:
	mov.l	&-1,regblock_memtop(regs) # Set interrupt mark in register.

define_debugging_label(comentry_interrupt)
	push_return_code(rc_comp_interrupt_restart)
	movq	&prim_interrupt,%d0
	bra.w	return_to_interpreter

define(compiler_lookup_error,
	`push_return_code(rc_comp_$1_restart)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error')

### comentry_lookup_apply
###
### Expects the arguments to be pushed on the stack, the environment
### in d4, the variable in d5, and the frame count in d0.w.

define_debugging_label(comentry_lookup_apply)
	push_frame_count(%d0)
	push(%d5, %d4)

comentry_lookup_apply_1:
	c_call(Lex_Ref, %d4, %d5)
	cmp.l	%d0,&prim_done
	bne.b	comentry_lookup_apply_error
	pop_discard(2)
	mov.l	(%sp),%d0
	mov.l	regblock_val(regs),(%sp)
	bra.w	comentry_apply

comentry_lookup_apply_error:
	compiler_lookup_error(lookup_apply)

define_interface_entry(comp_lookup_apply_restart)
	mov.l	(%sp),%d4
	mov.l	4(%sp),%d5
	bra.w	comentry_lookup_apply_1

### comentry_reference
### comentry_safe_reference
### comentry_access
### comentry_unassigned_p

### comentry_unbound_p
###
### Expects an environment in a0, and a name in a1.
### Returns the value in d0.
### Temporarily it is assumed that regblock_env(regs) contains the
### same value as a0, and this value is preserved accross the call.

define(define_compiler_reference,
`define_debugging_label(comentry_$1)
	adjust_return_address()
	push(%a1, %a0)

comentry_$1_1:
	c_call($2, %a0, %a1)
	cmp.l	%d0,&prim_done
	bne.b	comentry_$1_error
	pop_discard(2)
	mov.l	regblock_val(regs),%d0
	compiler_invoke_continuation()

comentry_$1_error:
	compiler_lookup_error($1)

define_interface_entry(comp_$1_restart)
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(regs)
	bra.w	comentry_$1_1')

define_compiler_reference(reference, Lex_Ref)
define_compiler_reference(safe_reference, safe_lex_ref)
define_compiler_reference(access, Symbol_Lex_Ref)
define_compiler_reference(unassigned_p, Symbol_Lex_unassigned_p)
define_compiler_reference(unbound_p, Symbol_Lex_unbound_p)

### comentry_assignment
### comentry_definition
###
### Expects an environment in a0, a name in a1, and a value in a2.
### Returns the old value in d0.
### Temporarily it is assumed that regblock_env(regs) contains the
### same value as a0, and this value is preserved accross the call.

define(define_compiler_assignment,
`define_debugging_label(comentry_$1)
	adjust_return_address()
	push(%a2, %a1, %a0)

comentry_$1_1:
	c_call($2, %a0, %a1, %a2)
	cmp.l	%d0,&prim_done
	bne.b	comentry_$1_error
	pop_discard(3)
	mov.l	regblock_val(regs),%d0
	compiler_invoke_continuation()

comentry_$1_error:
	compiler_lookup_error($1)

define_interface_entry(comp_$1_restart)
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	8(%sp),%a2		# value
	mov.l	%a0,regblock_env(regs)
	bra.w	comentry_$1_1')

define_compiler_assignment(assignment, Lex_Set)
define_compiler_assignment(definition, Local_Set)

### comentry_lookup_trap
### comentry_safe_lookup_trap
### comentry_unassigned_p_trap
###
### Expects a cached-variable extension object in a0 (this is what is
### left in the constant area slot by comentry_cache_mumble).
### Returns the value of the variable in d0.

define(define_reference_trap,
`define_debugging_label(comentry_$1)
	mov.l	(%a0),%d0		# Reference trap object
	and.l	rmask,%d0
	cmp.l	%d0,&trap_max_immediate
	ble.b	comentry_$1_immediate
	mov.l	%d0,%a2
	cmp.l	(%a2),&make_object(tc_fixnum,trap_fluid)
	bne.b	comentry_$1_continue
#
define_debugging_label(comentry_$1_fluid)
	mov.l	%a0,-(%sp)		# Preserve extension address
	c_call_c(lookup_fluid,(%a0))	# Should be c_call, but it works
	mov.l	(%sp)+,%a0		# Restore extension address
	mov.l	%d0,%a2			# Result is a value cell address
	cmp.b	(%a2),&tc_reference_trap
	bne	comentry_$1_valid
	mov.l	(%a2),%d0		# Fall through to unassigned code
	and.l	rmask,%d0
#
comentry_$1_immediate:
	cmp.l	%d0,&trap_unassigned
	beq	comentry_$1_unassigned
#	
comentry_$1_continue:
	adjust_return_address()
	push(%a0)
	mov.b	&tc_quad,(%sp)
	mov.l	(%sp),%a0
	c_call(compiler_$1, %a0)
	cmp.l	%d0,&prim_done
	bne.b	comentry_$1_error
	pop_discard(1)
	mov.l	regblock_val(regs),%d0
	compiler_invoke_continuation()

comentry_$1_error:
	mov.l	4(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	compiler_reference_trap_error($1)

define_interface_entry(comp_$1_restart)
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	c_call($2, %d1, %d0)
	cmp.l	%d0,&prim_done
	beq.b	comentry_$1_restart_continue
	mov.l	%d0,_compiled_code_error_code
	push_return_code(rc_comp_$1_restart)
	bra.w	return_to_interpreter_error

comentry_$1_restart_continue:
	pop_discard(2)
	mov.l	regblock_val(regs),%d0
	compiler_invoke_continuation()')

define(compiler_reference_trap_error,
	`mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	rmask,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	c_call(compiler_var_error, %a1, %a0)
	push(%d0)			# Save name
	push_return_code(rc_comp_$1_restart)
	bra.w	return_to_interpreter_error')

###
### comentry_lookup_trap
###

define_debugging_label(comentry_lookup_trap_valid)
	mov.l	(%a2),%d0		# Value expected there
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)
	
define_debugging_label(comentry_lookup_trap_unassigned)
	adjust_return_address()
	push(%a0)			# extension object
	mov.b	&tc_quad,(%sp)
	movq	&err_unassigned_variable,%d0
	bra	comentry_lookup_trap_error

define_reference_trap(lookup_trap, Symbol_Lex_Ref)

###
### comentry_safe_lookup_trap
###

define_debugging_label(comentry_safe_lookup_trap_valid)
	mov.l	(%a2),%d0		# Value expected there
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)

define_debugging_label(comentry_safe_lookup_trap_unassigned)
	mov.l	&make_object(tc_reference_trap,trap_unassigned),%d0
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)

define_reference_trap(safe_lookup_trap, safe_symbol_lex_ref)

###
### comentry_unassigned_p_trap
###

define_debugging_label(comentry_unassigned_p_trap_valid)
	mov.l	&false_object,%d0
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)

define_debugging_label(comentry_unassigned_p_trap_unassigned)
	mov.l	&true_object,%d0
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)

define_reference_trap(unassigned_p_trap, Symbol_Lex_unassigned_p)

### comentry_assignment_trap
###
### Expects a cached-variable extension object in a0, and the assignment
### value in a1.

define_debugging_label(comentry_assignment_trap)
	mov.l	(%a0),%d0		# trap object
	and.l	rmask,%d0
	cmp.l	%d0,&trap_max_immediate
	ble.b	comentry_assignment_trap_immediate
	mov.l	%d0,%a2
	cmp.l	(%a2),&make_object(tc_fixnum,trap_fluid)
	bne.b	comentry_assignment_trap_continue
#
define_debugging_label(comentry_assignment_trap_fluid)
	mov.l	%a0,-(%sp)		# Preserve extension address
	mov.l	%a1,-(%sp)		# Preserve value
	c_call_c(lookup_fluid,(%a0))	# Should be c_call, but it works
	mov.l	(%sp)+,%a1		# Restore value
	mov.l	(%sp)+,%a0		# Restore extension address
	mov.l	%d0,%a2			# Result is a value cell address
	cmp.b	(%a2),&tc_reference_trap
	beq.b	comentry_assignment_trap_fluid_trap
	mov.l	%a1,(%a2)		# Store
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)
#
comentry_assignment_trap_fluid_trap:
	mov.l	(%a2),%d0		# Reference trap object
	and.l	rmask,%d0
	cmp.l	%d0,&trap_unassigned
	bne.b	comentry_assignment_trap_continue
	mov.l	%a1,(%a2)		# Store
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)
#
comentry_assignment_trap_immediate:
	cmp.l	%d0,&trap_unassigned
	bne.b	comentry_assignment_trap_continue
	mov.l	%a1,(%a0)		# Store
	mov.l	(%sp)+,%a0		# Pseudo return address
	jmp	4(%a0)

comentry_assignment_trap_continue:
	adjust_return_address()
	push(%a1)			# value
	push(%a0)			# extension object
	mov.b	&tc_quad,(%sp)
	mov.l	(%sp),%a0

comentry_assignment_trap_1:
	c_call(compiler_assignment_trap, %a0, %a1)
	cmp.l	%d0,&prim_done
	bne.b	comentry_assignment_trap_error
	pop_discard(2)
	compiler_invoke_continuation()

comentry_assignment_trap_error:
	mov.l	8(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	compiler_reference_trap_error(assignment_trap)

define_interface_entry(comp_assignment_trap_restart)
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	8(%sp),%d2		# value
	c_call(Symbol_Lex_Set, %d1, %d0, %d2)
	cmp.l	%d0,&prim_done
	beq.b	comentry_assignment_trap_restart_continue
	mov.l	%d0,_compiled_code_error_code
	push_return_code(rc_comp_assignment_trap_restart)
	bra.w	return_to_interpreter_error

comentry_assignment_trap_restart_continue:
	pop_discard(3)
	compiler_invoke_continuation()

### comentry_cache_lookup_apply
###
### Expects the arguments on the stack, frame count in d0.w,
### a cached-variable extension object in a3, and the address of the
### compiled-code block in a1.

define_debugging_label(comentry_cache_lookup_apply)
	push_compiled_code_block(%a1)
	push_frame_count(%d0)
	push(%a3)
	mov.b	&tc_quad,(%sp)
	mov.l	(%sp),%a3

comentry_cache_lookup_apply_1:
	c_call(compiler_lookup_trap, %a3)
	cmp.l	%d0,&prim_done
	bne.b	comentry_cache_lookup_apply_error
	pop_discard(1)
	pop(%d0)
	mov.l	regblock_val(regs),(%sp)
	bra.w	comentry_apply

#
# The masking here is necessary because the block was pushed
# with push_compiled_code_block, which puts a tag on the
# address.
#

comentry_cache_lookup_apply_error:
	mov.l	8(%sp),%d1		# Block
	and.l	rmask,%d1
	mov.l	%d1,%a0
	compiler_reference_trap_error(cache_lookup_apply)

define_interface_entry(comp_cache_lookup_apply_restart)
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	c_call(Symbol_Lex_Ref, %d1, %d0)
	cmp.l	%d0,&prim_done
	beq.b	comentry_cache_lookup_apply_continue
	mov.l	%d0,_compiled_code_error_code
	push_return_code(rc_comp_cache_lookup_apply_restart)
	bra.w	return_to_interpreter_error

comentry_cache_lookup_apply_continue:
	pop_discard(2)
	pop(%d0)
	mov.l	regblock_val(regs),(%sp)
	bra.w	comentry_apply

### comentry_operator_lookup_trap
###
### Called when a cached operator references a variable which needs special
### attention.

define_debugging_label(comentry_operator_lookup_trap)
	mov.l	(%sp)+,%a0		# Fake procedure
	mov.l	(%sp)+,%a1		# Fake continuation (with nargs)
	pea	-4(%a0)			# Bump procedure back to JSR instruction
	mov.b	&tc_compiled_entry,(%sp)
	pea	(%a1)			# Save the fake continuation
	mov.l	(%a0),%d0		# Extract extension object
	mov.l	%d0,%d1
	and.l	rmask,%d1		# Compute extension address
	mov.l	%d1,%a2
	cmp.b	(%a2),&tc_reference_trap # Is it a trap?
	beq.b	comentry_operator_lookup_1
	mov.l	(%a2),4(%sp)		# Replace fake with real procedure
	bra.b	comentry_operator_lookup_invoke
#
comentry_operator_lookup_1:
	mov.l	(%a2),%d1		# trap object
	mov.l	%d1,%d2
	and.l	rmask,%d2
	cmp.l	%d2,&trap_max_immediate
	ble.b	comentry_operator_lookup_2
	mov.l	%d2,%a1
	cmp.l	(%a1),&make_object(tc_fixnum,trap_fluid)
	bne.b	comentry_operator_lookup_2
	mov.l	%d0,-(%sp)		# Preserve extension object
	c_call_c(lookup_fluid,%d1)	# Should be c_call, but it works
	mov.l	%d0,%a2			# Value cell address
	mov.l	(%sp)+,%d0		# Restore extension object
	cmp.b	(%a2),&tc_reference_trap
	beq.b	comentry_operator_lookup_2
	mov.l	(%a2),4(%sp)		# Replace fake with real procedure
	bra.b	comentry_operator_lookup_invoke
#
comentry_operator_lookup_2:
	lea	4(%sp),%a0		# Stack slot
	c_call(complr_operator_reference_trap, %a0, %d0)
	cmp.l	%d0,&prim_done
	bne.b	comentry_operator_lookup_trap_error
#
comentry_operator_lookup_invoke:
	cmp.b	4(%sp),&tc_compiled_entry # Are we lucky?
	bne.b	comentry_operator_trap_interpreted
	mov.l	4(%sp),%d0
	and.l	rmask,%d0
	mov.l	%d0,%a0
	cmp.w	-4(%a0),&0xfffd		# Is it still a fake procedure?
	bne.b	comentry_operator_trap_compiled
	mov.l	(%sp)+,(%sp)		# Move fake continuation to the
					# correct location, and pop the
					# fake procedure
	jmp	(%a0)			# Tail recurse into the fake procedure

comentry_operator_trap_compiled:
	mov.l	(%sp)+,%a1		# Fake continuation
	mov.w	(%a1),%d0		# Number of arguments supplied
	addq.l	&4,%sp			# Pop procedure
	bra.w	apply_compiled_procedure_3	

comentry_operator_trap_interpreted:
	mov.l	(%sp)+,%a0		# Fake continuation
	mov.w	(%a0),%d0		# Number of arguments supplied
	bra.w	comentry_apply_interpreted

comentry_operator_lookup_trap_error:
	mov.l	(%sp)+,%a0		# Fake continuation
	mov.w	(%a0),%d1		# Number of arguments supplied
	push_frame_count(%d1)		# For apply
	mov.l	4(%sp),%d1		# Fake procedure
	and.l	rmask,%d1
	mov.l	%d1,%a0
	mov.l	4(%a0),-(%sp)		# extension
	mov.l	8(%a0),%d1		# block
	and.l	rmask,%d1
	mov.l	%d1,%a0			# block address
	compiler_reference_trap_error(op_lookup_trap)

### comp_op_lookup_trap_restart
###
### This "re-fetches" the cached operator from the compiled code block
### because the location may have been clobbered in the interim with a
### different object.

define_interface_entry(comp_op_lookup_trap_restart)
	lea	12(%sp),%sp		# Ignore the number of arguments,
					# the environment and the name.
	clr.b	(%sp)
	mov.l	(%sp)+,%a0		# Fake procedure
	mov.l	12(%a0),%d0		# offset
	and.l	rmask,%d0
	mov.l	8(%a0),%d1		# block
	and.l	rmask,%d1
	mov.l	%d1,%a0
	jmp	(0,%a0,%d0.l*4)		# Invoke contents of slot

### comentry_primitive_apply
### comentry_primitive_lexpr_apply
###
### Both expect the primitive object to be in d6.  d6 is expected to
### be preserved by the primitive so we can use it on return; this is
### consistent with this C compiler's calling conventions.
###
### The primitive is also saved in the interpreter's expression register
### so that the system can back out in case of an error/interrupt during
### the primitive.
###
### comentry_primitive_lexpr_apply is used for "lexpr" primitives
### (those whose arity is not fixed).  In addition, it expects
### regblock_lexpr_actuals to contain the actual number of arguments
### passed.

define_debugging_label(comentry_primitive_apply)
	mov.l	%d6,regblock_expr(regs)
	and.l	&primitive_index_mask,%d6
	switch_registers_before_primitive()
	jsr	([_Primitive_Procedure_Table,%za0,%d6.w*4],0) # call
	switch_registers_after_primitive()
	mov.l	%d0,regblock_val(regs)
	adda.l	(_Primitive_Count_Table,%za0,%d6.w*4),%sp # pop primitive arguments.
	compiler_invoke_continuation()

define_debugging_label(comentry_primitive_lexpr_apply)
	mov.l	%d6,regblock_expr(regs)
	and.l	&primitive_index_mask,%d6
	switch_registers_before_primitive()
	jsr	([_Primitive_Procedure_Table,%za0,%d6.w*4],0) # call
	switch_registers_after_primitive()
	mov.l	%d0,regblock_val(regs)
	mov.l	regblock_lexpr_actuals(regs),%d0 # number of arguments
	lea	(0,%sp,%d0.w*4),%sp		# pop primitive arguments.
	compiler_invoke_continuation()

#### Special primitive linkage tables
###

	data

###
###	Names of primitives
###

pc_zero_string:
	asciz	"ZERO?"
pc_positive_string:
	asciz	"POSITIVE?"
pc_negative_string:
	asciz	"NEGATIVE?"
pc_equal_string:
	asciz	"&="
pc_less_string:
	asciz	"&<"
pc_greater_string:
	asciz	"&>"
pc_plus_string:
	asciz	"&+"
pc_minus_string:
	asciz	"&-"
pc_multiply_string:
	asciz	"&*"
pc_divide_string:
	asciz	"&/"
pc_increment_string:
	asciz	"1+"
pc_decrement_string:
	asciz	"-1+"
	lalign	4

###
###	Pointers to names of primitives
###

first_pc_name:
	long	pc_zero_string
	long	pc_positive_string
	long	pc_negative_string
	long	pc_equal_string
	long	pc_less_string
	long	pc_greater_string
	long	pc_plus_string
	long	pc_minus_string
	long	pc_multiply_string
	long	pc_divide_string
	long	pc_increment_string
	long	pc_decrement_string
last_pc_name:
	space	4

###
###	Primitive codes
###

define_debugging_label(first_pc_code)
pc_zero:
	space	4
pc_positive:
	space	4
pc_negative:
	space	4
pc_equal:
	space	4
pc_less:
	space	4
pc_greater:
	space	4
pc_plus:
	space	4
pc_minus:
	space	4
pc_multiply:
	space	4
pc_divide:
	space	4
pc_increment:
	space	4
pc_decrement:
	space	4
	text

#### Special coded arithmetic
###
### Arguments on the stack.  Compiler return address above.
###

define(define_unary_generic,
	`define_debugging_label(comentry_$1)
	cmp.b	(%sp),&tc_fixnum
	bne.b	$1_generic
	$2
	pop_discard(1)
	compiler_invoke_continuation()
$1_generic:
	mov.l	pc_$1,%d6
	bra	comentry_primitive_apply')

define(generic_fixnum_result,
	`bvs.b	$1_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(regs)
	mov.b	&tc_fixnum,regblock_val(regs)')

unary_return_true:
	mov.l	&true_object,regblock_val(regs)
	pop_discard(1)
	compiler_invoke_continuation()	

define(define_unary_generic_predicate,
	`define_unary_generic($1,
	`mov.l	(%sp),%d0
	lsl.l	&8,%d0
	b$2	unary_return_true
	mov.l	&false_object,regblock_val(regs)')')
	
define_unary_generic_predicate(zero, eq)
define_unary_generic_predicate(positive, gt)
define_unary_generic_predicate(negative, lt)

define(define_unary_generic_operator,
	`define_unary_generic($1,
	`mov.l	(%sp),%d0
	lsl.l	&8,%d0
	$2.l	&256,%d0
	generic_fixnum_result($1)')')

define_unary_generic_operator(increment, add)
define_unary_generic_operator(decrement, sub)

#### Binary procedures

define(define_binary_generic,
	`define_debugging_label(comentry_$1)
	cmp.b	(%sp),&tc_fixnum
	bne.b	$1_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	$1_generic
	$2
	pop_discard(2)
	compiler_invoke_continuation()
$1_generic:
	mov.l	pc_$1,%d6
	bra	comentry_primitive_apply')

binary_return_true:
	mov.l	&true_object,regblock_val(regs)
	pop_discard(2)
	compiler_invoke_continuation()	

define(define_binary_generic_predicate,
	`define_binary_generic($1,
	`mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	cmp.l	%d0,%d1
	b$2	binary_return_true
	mov.l	&false_object,regblock_val(regs)')')

define_binary_generic_predicate(equal, eq)
define_binary_generic_predicate(less, lt)
define_binary_generic_predicate(greater, gt)

define(define_binary_generic_operator,
	`define_binary_generic($1,
	`mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	$2.l	%d1,%d0
	generic_fixnum_result($1)')')

define_binary_generic_operator(plus, add)
define_binary_generic_operator(minus, sub)

define_binary_generic(multiply,
	`mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	asr.l	&8,%d1
	muls.l	%d1,%d0
	generic_fixnum_result(multiply)')

# This one is not optimized yet.

define_debugging_label(comentry_divide)
	mov.l	pc_divide,%d6
	bra	comentry_primitive_apply

### comentry_link
###
### Initialize all the variable cache slots for a compiled code block.
### It is called at load time, by the compiled code itself.
### It expects a block address in a0, the address of the constant section
### in a1, and a count of special blocks in d0.w

define_debugging_label(comentry_link)
	adjust_return_address()
###
### First, propagate the environment stored in the compiled code block.
###
	mov.l	%a0,-(%sp)		# Preserve a0, a1, and d0.
	mov.l	%a1,-(%sp)
	mov.l	%d0,-(%sp)
	mov.l	(%a0),%d0
	and.l	%d7,%d0
	c_call(propagate_variable_cache,%a0,%d0)
	mov.l	(%sp)+,%d0		# Restore d0, a1, and a0.
	mov.l	(%sp)+,%a1
	mov.l	(%sp)+,%a0
#
	mov.l	%a1,%d6
	sub.l	%a0,%d6
	lsr.l	&2,%d6
	push_fixnum_word(%d0)				# code
	mov.l	%a0,%a4					# a4 is preserved
	push_compiled_code_block(%a0)
	mov.l	(%sp)+,%d4
	bra.w	comentry_outer_loop_enter

comentry_outer_loop:
	mov.b	&tc_linkage_section,(0,%a4,%d6.l*4)	# New header
	c_call(propagate_variable_cache,%a4,%d6) 	# Propagate header
	push_fixnum_long(%d6)				# last offset
	mov.w	(2,%a4,%d6.l*4),%d5			# count
	mov.b	(1,%a4,%d6.l*4),%d0			# kind of object
	bne.b	comentry_link_caches
	lsr.w	&1,%d5					# count is twice the iterations
	subq.l	&1,%d6					# Adjust for add.
	subq.w	&1,%d5					# dbcc, assumed >= 1.
comentry_link_operators_restart:
	movq	&2,%d3					# each iteration bumps by 2
	mov.l	&_compiler_cache_operator,%a3		# C handler
	bra.b	comentry_link_caches_loop

comentry_link_caches:
	subq.w	&1,%d5					# dbcc, assumed >= 1.
comentry_link_caches_restart:
	movq	&1,%d3					# each iteration bumps by 1
	mov.l	&_compiler_cache_lookup,%a3
	cmp.b	%d0,&1
	beq.b	comentry_link_caches_loop
	mov.l	&_compiler_cache_assignment,%a3

comentry_link_caches_loop:
	add.l	%d3,%d6					# bump offset
	mov.l	(0,%a4,%d6.l*4),%d0			# symbol
	computed_c_call((%a3), %d0, %d4, %d6)
	cmp.l	%d0,&prim_done				# completed?
	dbne	%d5,comentry_link_caches_loop
	bne.b	comentry_link_caches_error
	add.l	%d3,%d6
	lea	4(%sp),%sp				# ignore last offset

comentry_outer_loop_enter:
	subq.w	&1,2(%sp)				# code
	bge.w	comentry_outer_loop
	lea	4(%sp),%sp				# pop code
	compiler_invoke_continuation()

###
### Recovery from errors/interrupt in linking code.
###

comentry_link_caches_error:
	push_fixnum_long(%d6)				# offset
	push(%d4)					# block
	push_fixnum_word(%d5)				# count
	mov.l	12(%sp),%d5				# last offset
	and.l	rmask,%d5
	sub.l	%d5,%d6
	subq.l	&1,%d6
	mov.l	%d0,%d2					# Preserve accross call
	mov.w	(2,%a4,%d5.l*4),%d3			# Real count
	mov.w	%d6,(2,%a4,%d5.l*4)			# Smash count in header
	push_fixnum_word(%d3)				# Save the real count
	c_call(propagate_variable_cache,%a4,%d5)	# Propagate header
	mov.l	%d2,%d0					# Restore
	cmp.l	%d0,&prim_interrupt			# Interrupt or error?
	beq.b	comentry_link_caches_interrupt
	compiler_lookup_error(link_caches)

comentry_link_caches_interrupt:
	push_return_code(rc_comp_link_caches_restart)
	bra.w	return_to_interpreter

define_interface_entry(comp_link_caches_restart)
	mov.l	(%sp)+,%d0				# header count
	mov.l	(%sp)+,%d5				# loop count
	mov.l	(%sp),%d4				# cc block
	clr.b	(%sp)
	mov.l	(%sp)+,%a4				# cc block address
	mov.l	(%a4),%d6				# block length
	and.l	rmask,%d6
	mov.l	(0,%a4,%d6.l*4),regblock_env(regs)	# restore env
	mov.l	(%sp)+,%d6				# offset
	and.l	rmask,%d6
	mov.l	(%sp),%d2				# last offset
	and.l	rmask,%d2
	mov.w	%d0,(2,%a4,%d2.l*4)			# Smash count in header
	c_call(propagate_variable_cache,%a4,%d2)	# Propagate header
	mov.b	(1,%a4,%d2.l*4),%d0			# Kind of linkage section
	beq.b	comp_link_operator_restart
	subq.l	&1,%d6					# adjust for next iteration
	bra	comentry_link_caches_restart

comp_link_operator_restart:
	subq.l	&2,%d6					# adjust for next iteration
	bra	comentry_link_operators_restart

### compiler_initialize
###
### Sets up the compiler section of the register block.

define(setup_register,
	`mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_$1,(%a0)+')

define_simple_c_procedure(compiler_initialize)
	lea	_Registers,%a1
	lea	regblock_hooks(%a1),%a0

#
#	setup_register(<name>)				# index	offset
#

	setup_register(return_to_interpreter) 		#  0	 f0
	setup_register(operator_lookup_trap)		#  1	 f6
	setup_register(operator_interpreted_trap)	#  2	 fc
	setup_register(operator_arity_trap)		#  3	102
	setup_register(operator_apply_trap) 		#  4    108

	lea	regblock_entries(%a1),%a0

	setup_register(link)				#  0	12c
	setup_register(error)				#  1	132
	setup_register(apply)				#  2	138
	setup_register(lexpr_apply)			#  3	13e
	setup_register(primitive_apply)			#  4	144
	setup_register(primitive_lexpr_apply)		#  5	14a
	setup_register(cache_lookup_apply)		#  6	150
	setup_register(lookup_apply)			#  7	156
	setup_register(interrupt_continuation)		#  8	15c
	setup_register(interrupt_ic_procedure)		#  9	162
	setup_register(interrupt_procedure)		#  a	168
	setup_register(interrupt_closure)		#  b	16e
	setup_register(reference)			#  c	174
	setup_register(safe_reference)			#  d	17a
	setup_register(assignment)			#  e	180
	setup_register(access)				#  f	186
	setup_register(unassigned_p)			# 10	18c
	setup_register(unbound_p)			# 11	192
	setup_register(definition)			# 12	198
	setup_register(lookup_trap)			# 13	19e
	setup_register(safe_lookup_trap)		# 14	1a4
	setup_register(assignment_trap)			# 15	1aa
	setup_register(unassigned_p_trap)		# 16	1b0
	setup_register(plus)				# 17	1b6
	setup_register(minus)				# 18	1bc
	setup_register(multiply)			# 19	1c2
	setup_register(divide)				# 1a	1c8
	setup_register(equal)				# 1b	1ce
	setup_register(less)				# 1c	1d4
	setup_register(greater)				# 1d	1da
	setup_register(increment)			# 1e	1e0
	setup_register(decrement)			# 1f	1e6
	setup_register(zero)				# 20	1ec
	setup_register(positive)			# 21	1f2
	setup_register(negative)			# 22	1f8

	mov.l	&INTERFACE_VERSION,_compiler_interface_version
	mov.l	&PROCESSOR_TYPE,_compiler_processor_type
	mov.l	&null_object,_compiler_utilities

	tst.l	4(%sp)					# FASL_It
	bne.b	init_constant_space
	rts

### Create a compiled code block with special entry points.

init_constant_space:
	lea	constant_end,%a0
	mov.l	%a0,%d0
	lea	constant_start,%a0
	sub.l	%a0,%d0
	lsr.l	&2,%d0
	mov.l	%d0,-(%sp)
	c_call_c(copy_to_constant_space, %a0, %d0)
	mov.l	%d0,%a0
	mov.l	(%sp)+,%d0

	subq.l	&1,%d0				# init vector header
	mov.l	%d0,(%a0)
	mov.b	&tc_manifest_vector,(%a0)
	subq.l	&1,%d0				# init NM vector header
	mov.l	%d0,4(%a0)
	mov.b	&tc_manifest_nm_vector,4(%a0)

### Cache the special entry points

define_debugging_label(compiler_reset)
	mov.l	%a0,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	mov.l	(%sp),_compiler_utilities

	mov.b	&tc_compiled_entry,(%sp)
	mov.l	(%sp)+,%d1
	add.l	&constant_return_to_interpreter-constant_start,%d1
	mov.l	%d1,_return_to_interpreter

### Cache the relevant primitives

	lea	first_pc_code,%a0	# Setup pointers
	mov.l	%a0,-(%sp)
	lea	first_pc_name,%a0
	mov.l	%a0,-(%sp)
	bra.b	enter_primitive_loop

primitive_loop:
	mov.l	(%sp),%a0
	c_call_c(make_primitive,(%a0))	# Name string pointer
	mov.l	4(%sp),%a0
	mov.l	%d0,(%a0)+		# Primitive object
	mov.l	%a0,4(%sp)
	addq.l	&4,(%sp)		# Bump string pointer

enter_primitive_loop:
	mov.l	(%sp),%d0
	lea	last_pc_name,%a0
	cmp.l	%d0,%a0
	blt.b	primitive_loop
	lea	8(%sp),%sp		# Pop pointers
	rts

### The following is called after a disk-restore.
###
### It receives the new compiled block with fake entry points.
### The processor/version information has already been checked by fasload.

define_simple_c_procedure(compiler_reset)
	cmp.b	4(%sp),&tc_compiled_code_block
	bne.w	compiler_reset_error

	mov.l	4(%sp),%d0
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	bra.w	compiler_reset	

compiler_reset_error:
	c_call_c(compiler_reset_error)
	rts

### The following block is copied to constant space, and the
### interpreter variable return_to_interpreter is made to point
### to the entry point.

	data
	lalign	4
constant_start:
	long	0				# Vector header
	long	0				# NM header
###
	short	0x8080
	short	constant_return_to_interpreter-constant_start
constant_return_to_interpreter:
	jmp	offset_return_to_interpreter(regs)

	lalign	4
constant_end:
	long	0
