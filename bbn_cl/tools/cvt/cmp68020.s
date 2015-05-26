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

#### $Header: cmp68020.s,v 9.64 87/12/04 22:14:27 GMT jinx Exp $
####
#### Compiled Code Interface for HP9000 series 300















### push the arguments in order, left to right.



### push the arguments in order, right to left.



### pop the arguments in order, right to left.



































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

# 20 6-byte entry points.
	set	regblock_entries,(regblock_temporaries + (regblock_ntemps * 4))
	set	regblock_nentries,20
	set	offset_return_to_interpreter,(regblock_entries + (15 * 6))

# An area for popper code.
	set	regblock_messages,(regblock_entries + (regblock_nentries * 6))

# 40 more 6-byte entry points.
	set	regblock_entries_2,(regblock_messages + 192)
	set	regblock_nentries_2,40
	set	regblock_length,(regblock_entries_2 + (regblock_nentries_2 * 6))
	set	offset_fake_uuo_link_trap,(regblock_entries_2 + 6)
	set	offset_uuo_link_trap,(regblock_entries_2 + (21 * 6))

# types and other constants required.

	set	prim_done,-1
	set	prim_do_expression,-2
	set	prim_apply,-3
	set	prim_interrupt,-4
	set	prim_no_trap_eval,-5
	set	prim_no_trap_apply,-6
	set	prim_pop_return,-7

	set	primitive_index_mask,0x00000fff

	set	tc_null,0x00
	set	tc_manifest_vector,0x00
	set	tc_list,0x01
	set	tc_flonum,0x06
	set	tc_true,0x08
	set	tc_vector,0x0a
	set	tc_return_code,0x0b
	set	tc_compiled_procedure,0x0d
	set	tc_environment,0x12
	set	tc_fixnum,0x1a
	set	tc_hunk3,0x24
	set	tc_manifest_nm_vector,0x27
	set	tc_reference_trap,0x32
	set	tc_return_address,0x39
	set	tc_compiled_code_block,0x3d

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
	set	rc_comp_lexpr_interrupt_restart,0x50
	set	rc_comp_safe_reference_restart,0x51
	set	rc_comp_cache_lookup_restart,0x52
	set	rc_comp_lookup_trap_restart,0x53
	set	rc_comp_assignment_trap_restart,0x54
	set	rc_comp_cache_operator_restart,0x55
	set	rc_comp_op_ref_trap_restart,0x56
	set	rc_comp_cache_reference_apply_restart,0x57
	set	rc_comp_safe_ref_trap_restart,0x58
	set	rc_comp_unassigned_p_trap_restart,0x59
	set	rc_comp_cache_assignment_restart,0x5A

###
### Global data
###

	data
	global	_Registers
_Registers:
	space	regblock_length
	global	_compiler_utilities
_compiler_utilities:
	space	4
	global	_return_to_interpreter
_return_to_interpreter:
	space	4
	global	_fake_uuo_link_trap
_fake_uuo_link_trap:
	space	4
	global	_uuo_link_trap
_uuo_link_trap:
	space	4
	global	c_save_stack
c_save_stack:
	space	4

###
### Entries from the interpreter
###

	text
	global	_enter_compiled_expression
_enter_compiled_expression:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	regblock_expr(%a6),%d0	# Mask and branch on the expression.
	and.l	%d7,%d0
	mov.l	%d0,%a0
	jmp	(%a0)

	global	_apply_compiled_procedure
_apply_compiled_procedure:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp)+,%d0			# Get frame length in d0.

	global	apply_compiled_procedure_common
apply_compiled_procedure_common:
	mov.l	(%sp),%d1		# Get procedure.
	and.l	%d7,%d1
	mov.l	%d1,%a1
	mov.l	(%a1),%d1		# Take car to get entry address.
	and.l	%d7,%d1
	mov.l	%d1,%a0
	jmp	(%a0)			# Apply.

	global	return_to_interpreter_error
return_to_interpreter_error:
	mov.l	_compiled_code_error_code,%d0
	tst.l	%d0
	blt.b	return_to_interpreter
	mov.l	&err_compiled_code_error,%d0
	bra.b	return_to_interpreter

	global	compiler_return_to_interpreter
compiler_return_to_interpreter:
	movq	&prim_done,%d0

	global	return_to_interpreter
return_to_interpreter:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	movm.l	(%sp)+,%d2-%d7/%a0-%a6
	rts

### compiler_apply
### compiler_error
###
### Expects the procedure and arguments to be pushed on the stack, and
### the count to be in D0.W (no type code needed).

	global	compiler_error
compiler_error:
	mov.l	_Fixed_Objects,%d1
	and.l	%d7,%d1
	mov.l	%d1,%a0
	mov.l	fobject_compiler_error_procedure(%a0),-(%sp)

### macroize the application code so that other places can call it
### without speed penalty.

	global	compiler_apply
compiler_apply:
	cmp.b	(%sp),&tc_compiled_procedure
	beq.b	apply_compiled_procedure_common
	ext.l	%d0
	mov.l	%d0,-(%sp)
	movq	&prim_apply,%d0
	bra.b	return_to_interpreter

	global	_return_to_compiled_code
_return_to_compiled_code:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	clr.b	(%sp)
	rts

### compiler_primitive_apply
### comentry_primitive_lexpr_apply
###
### Both expect the primitive object to be in D6.  D6 is expected to
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

	global	compiler_primitive_apply
compiler_primitive_apply:
	mov.l	%d6,regblock_expr(%a6)
	and.l	&primitive_index_mask,%d6
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	%a5,_Free
	jsr	([_Primitive_Procedure_Table,%za0,%d6.w*4],0) # call
	mov.l	_Free,%a5
	mov.l	%d0,regblock_val(%a6)
	adda.l	(_Primitive_Count_Table,%za0,%d6.w*4),%sp # pop primitive arguments.
	clr.b	(%sp)
	rts

	global	comentry_primitive_lexpr_apply
comentry_primitive_lexpr_apply:
	mov.l	%d6,regblock_expr(%a6)
	and.l	&primitive_index_mask,%d6
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	%a5,_Free
	jsr	([_Primitive_Procedure_Table,%za0,%d6.w*4],0) # call
	mov.l	_Free,%a5
	mov.l	%d0,regblock_val(%a6)
	mov.l	regblock_lexpr_actuals(%a6),%d0 # number of arguments
	lea	(0,%sp,%d0.w*4),%sp		# pop primitive arguments.
	clr.b	(%sp)
	rts

### compiler_lookup_apply
###
### Expects the arguments to be pushed on the stack, the environment
### in D4, the variable in D5, and the frame count in D0.W.



	global	compiler_lookup_apply
compiler_lookup_apply:
	ext.l	%d0
	mov.l	%d0,-(%sp)
	mov.l	%d5,-(%sp)
	mov.l	%d4,-(%sp)

compiler_lookup_apply_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d5,-(%sp)
	mov.l	%d4,-(%sp)
	jsr	_Lex_Ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_lookup_apply_error
	addq.l	&8,%sp
	mov.l	(%sp),%d0
	mov.l	regblock_val(%a6),(%sp)
	bra.w	compiler_apply

compiler_lookup_apply_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_lookup_apply_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_lookup_apply_restart
_comp_lookup_apply_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d4
	mov.l	4(%sp),%d5
	bra.w	compiler_lookup_apply_1

### compiler_reference
### compiler_safe_reference
### compiler_access
### compiler_unassigned_p
### compiler_unbound_p
###
### Expects an environment in A0, and a name in A1.
### Returns the value in D0.
### Temporarily it is assumed that regblock_env(regs) contains the
### same value as A0, and this value is preserved accross the call.



	global	compiler_reference
compiler_reference:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_reference_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Lex_Ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_reference_error
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_reference_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_reference_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_reference_restart
_comp_reference_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_reference_1

	global	compiler_safe_reference
compiler_safe_reference:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_safe_reference_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_safe_lex_ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_safe_reference_error
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_safe_reference_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_safe_reference_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_safe_reference_restart
_comp_safe_reference_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_safe_reference_1

	global	compiler_access
compiler_access:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_access_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Symbol_Lex_Ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_access_error
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_access_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_access_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_access_restart
_comp_access_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_access_1

	global	compiler_unassigned_p
compiler_unassigned_p:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_unassigned_p_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Symbol_Lex_unassigned_p
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_unassigned_p_error
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_unassigned_p_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_unassigned_p_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_unassigned_p_restart
_comp_unassigned_p_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_unassigned_p_1

	global	compiler_unbound_p
compiler_unbound_p:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_unbound_p_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Symbol_Lex_unbound_p
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_unbound_p_error
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_unbound_p_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_unbound_p_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_unbound_p_restart
_comp_unbound_p_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_unbound_p_1

### compiler_assignment
### compiler_definition
###
### Expects an environment in A0, a name in A1, and a value in A2.
### Returns the old value in D0.
### Temporarily it is assumed that regblock_env(regs) contains the
### same value as A0, and this value is preserved accross the call.



	global	compiler_assignment
compiler_assignment:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a2,-(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_assignment_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a2,-(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Lex_Set
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_assignment_error
	lea	12(%sp),%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_assignment_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_assignment_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_assignment_restart
_comp_assignment_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	8(%sp),%a2		# value
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_assignment_1

	global	compiler_definition
compiler_definition:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a2,-(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

compiler_definition_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a2,-(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_Local_Set
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	compiler_definition_error
	lea	12(%sp),%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

compiler_definition_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_definition_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_definition_restart
_comp_definition_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%a0		# environment
	mov.l	4(%sp),%a1		# name
	mov.l	8(%sp),%a2		# value
	mov.l	%a0,regblock_env(%a6)
	bra.w	compiler_definition_1

### compiler_cache_lookup{_multiple}
### compiler_cache_assignment{_multiple}
### compiler_cache_operator{_multiple}
###
### Expects a compiled-code block address in A0 and the address of a
### constant area offset in A1.  The "_multiple" entry points expect
### D1.W to contain a count indicating how many caches to initialize.
### Temporarily, these entry points are supposed to preserve
### environment, so the restart routines extract it from the block.



	global	comentry_cache_lookup
comentry_cache_lookup:
	movq	&1,%d1

	global	comentry_cache_lookup_multiple
comentry_cache_lookup_multiple:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,%d0
	sub.l	%a0,%d0
	lsr.l	&2,%d0
	mov.w	%d1,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)
	mov.l	%d0,-(%sp)
	mov.b	&tc_fixnum,(%sp)
	mov.l	%a0,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	dbf	%d1,comentry_cache_lookup_retry
	bra.b	comentry_cache_lookup_done

comentry_cache_lookup_loop:
	mov.l	4(%sp),%d0
	addq.l	&1,%d0
	mov.l	%d0,4(%sp)

comentry_cache_lookup_retry:
	and.l	%d7,%d0		# Offset
	mov.w	%d1,10(%sp)
	mov.l	(%sp),%d2
	and.l	%d7,%d2
	mov.l	%d2,%a0
	mov.l	(0,%a0,%d0.l*4),%d2	# Symbol
	mov.l	(%sp),%a0		# Block
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%d2,-(%sp)
	jsr	_compiler_cache_lookup
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_cache_lookup_error
	mov.w	10(%sp),%d1
	dbf	%d1,comentry_cache_lookup_loop

comentry_cache_lookup_done:
	clr.b	(%sp)			# preserve block argument.
	mov.l	(%sp)+,%a0			#
	addq.l	&8,%sp
	clr.b	(%sp)
	rts

comentry_cache_lookup_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_cache_lookup_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_cache_lookup_restart
_comp_cache_lookup_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# cc block
	and.l	%d7,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d0		# length
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),regblock_env(%a6)
	mov.w	10(%sp),%d1		# count
	mov.l	4(%sp),%d0		# offset
	bra.w	comentry_cache_lookup_retry
	global	comentry_cache_assignment
comentry_cache_assignment:
	movq	&1,%d1

	global	comentry_cache_assignment_multiple
comentry_cache_assignment_multiple:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,%d0
	sub.l	%a0,%d0
	lsr.l	&2,%d0
	mov.w	%d1,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)
	mov.l	%d0,-(%sp)
	mov.b	&tc_fixnum,(%sp)
	mov.l	%a0,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	dbf	%d1,comentry_cache_assignment_retry
	bra.b	comentry_cache_assignment_done

comentry_cache_assignment_loop:
	mov.l	4(%sp),%d0
	addq.l	&1,%d0
	mov.l	%d0,4(%sp)

comentry_cache_assignment_retry:
	and.l	%d7,%d0		# Offset
	mov.w	%d1,10(%sp)
	mov.l	(%sp),%d2
	and.l	%d7,%d2
	mov.l	%d2,%a0
	mov.l	(0,%a0,%d0.l*4),%d2	# Symbol
	mov.l	(%sp),%a0		# Block
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%d2,-(%sp)
	jsr	_compiler_cache_assignment
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_cache_assignment_error
	mov.w	10(%sp),%d1
	dbf	%d1,comentry_cache_assignment_loop

comentry_cache_assignment_done:
	clr.b	(%sp)			# preserve block argument.
	mov.l	(%sp)+,%a0			#
	addq.l	&8,%sp
	clr.b	(%sp)
	rts

comentry_cache_assignment_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_cache_assignment_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_cache_assignment_restart
_comp_cache_assignment_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# cc block
	and.l	%d7,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d0		# length
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),regblock_env(%a6)
	mov.w	10(%sp),%d1		# count
	mov.l	4(%sp),%d0		# offset
	bra.w	comentry_cache_assignment_retry
	global	comentry_cache_operator
comentry_cache_operator:
	movq	&1,%d1

	global	comentry_cache_operator_multiple
comentry_cache_operator_multiple:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,%d0
	sub.l	%a0,%d0
	lsr.l	&2,%d0
	mov.w	%d1,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)
	mov.l	%d0,-(%sp)
	mov.b	&tc_fixnum,(%sp)
	mov.l	%a0,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	dbf	%d1,comentry_cache_operator_retry
	bra.b	comentry_cache_operator_done

comentry_cache_operator_loop:
	mov.l	4(%sp),%d0
	addq.l	&1,%d0
	mov.l	%d0,4(%sp)

comentry_cache_operator_retry:
	and.l	%d7,%d0		# Offset
	mov.w	%d1,10(%sp)
	mov.l	(%sp),%d2
	and.l	%d7,%d2
	mov.l	%d2,%a0
	mov.l	(0,%a0,%d0.l*4),%d2	# Symbol
	mov.l	(%sp),%a0		# Block
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%d2,-(%sp)
	jsr	_compiler_cache_operator
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_cache_operator_error
	mov.w	10(%sp),%d1
	dbf	%d1,comentry_cache_operator_loop

comentry_cache_operator_done:
	clr.b	(%sp)			# preserve block argument.
	mov.l	(%sp)+,%a0			#
	addq.l	&8,%sp
	clr.b	(%sp)
	rts

comentry_cache_operator_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_cache_operator_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

	global	_comp_cache_operator_restart
_comp_cache_operator_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# cc block
	and.l	%d7,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d0		# length
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),regblock_env(%a6)
	mov.w	10(%sp),%d1		# count
	mov.l	4(%sp),%d0		# offset
	bra.w	comentry_cache_operator_retry

### compiled_entry_to_block
###
### Expects a Scheme object representing a compiled code entry point in D1.
### Returns the address of the block to which it belongs in A0.

	global	compiled_entry_to_block
compiled_entry_to_block:
	and.l	%d7,%d1
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

### compiler_lookup_trap
### compiler_safe_reference_trap
### compiler_unassigned_p_trap
###
### Expects a cached-variable extension object in A0 (this is what is
### left in the constant area slot by compiler_cache_mumble).
### Returns the value of the variable in D0.





	global	comentry_lookup_trap
comentry_lookup_trap:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	jsr	_compiler_lookup_trap
	addq.l	&4,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_lookup_trap_error
	addq.l	&4,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

comentry_lookup_trap_error:
	mov.l	4(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	mov.l	%a1,-(%sp)
	jsr	_compiler_var_error
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	%d0,-(%sp)			# Save name
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_lookup_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

	global	_comp_lookup_trap_restart
_comp_lookup_trap_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%d1,-(%sp)
	jsr	_Symbol_Lex_Ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	beq.b	comentry_lookup_trap_restart_continue
	mov.l	%d0,_compiled_code_error_code
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_lookup_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

comentry_lookup_trap_restart_continue:
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts
	global	comentry_safe_ref_trap
comentry_safe_ref_trap:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	jsr	_compiler_safe_lookup_trap
	addq.l	&4,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_safe_ref_trap_error
	addq.l	&4,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

comentry_safe_ref_trap_error:
	mov.l	4(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	mov.l	%a1,-(%sp)
	jsr	_compiler_var_error
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	%d0,-(%sp)			# Save name
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_safe_ref_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

	global	_comp_safe_ref_trap_restart
_comp_safe_ref_trap_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%d1,-(%sp)
	jsr	_safe_symbol_lex_ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	beq.b	comentry_safe_ref_trap_restart_continue
	mov.l	%d0,_compiled_code_error_code
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_safe_ref_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

comentry_safe_ref_trap_restart_continue:
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts
	global	comentry_unassigned_p_trap
comentry_unassigned_p_trap:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a0,-(%sp)
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	jsr	_compiler_unassigned_p_trap
	addq.l	&4,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_unassigned_p_trap_error
	addq.l	&4,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

comentry_unassigned_p_trap_error:
	mov.l	4(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	mov.l	%a1,-(%sp)
	jsr	_compiler_var_error
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	%d0,-(%sp)			# Save name
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_unassigned_p_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

	global	_comp_unassigned_p_trap_restart
_comp_unassigned_p_trap_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%d1,-(%sp)
	jsr	_Symbol_Lex_unassigned_p
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	beq.b	comentry_unassigned_p_trap_restart_continue
	mov.l	%d0,_compiled_code_error_code
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_unassigned_p_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

comentry_unassigned_p_trap_restart_continue:
	addq.l	&8,%sp
	mov.l	regblock_val(%a6),%d0
	clr.b	(%sp)
	rts

### compiler_cache_reference_apply
###
### Expects the arguments on the stack, frame count in D0.W,
### a cached-variable extension object in A3, and the address of the
### compiled-code block in A1.

	global	comentry_cache_reference_apply
comentry_cache_reference_apply:
	mov.l	%a1,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	ext.l	%d0
	mov.l	%d0,-(%sp)
	mov.l	%a3,-(%sp)

comentry_cache_reference_apply_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a3,-(%sp)
	jsr	_compiler_lookup_trap
	addq.l	&4,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_cache_reference_apply_error
	addq.l	&4,%sp
	mov.l	(%sp)+,%d0
	mov.l	regblock_val(%a6),(%sp)
	bra.w	compiler_apply

comentry_cache_reference_apply_error:
	mov.l	8(%sp),%d1		# Block
	and.l	%d7,%d1
	mov.l	%d1,%a0
	mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	mov.l	%a1,-(%sp)
	jsr	_compiler_var_error
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	%d0,-(%sp)			# Save name
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_cache_reference_apply_restart,-(%sp)
	bra.w	return_to_interpreter_error

	global	_comp_cache_ref_apply_restart
_comp_cache_ref_apply_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%d1,-(%sp)
	jsr	_Symbol_Lex_Ref
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	beq.b	comentry_cache_ref_restart_continue
	mov.l	%d0,_compiled_code_error_code
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_cache_reference_apply_restart,-(%sp)
	bra.w	return_to_interpreter_error

comentry_cache_ref_restart_continue:
	addq.l	&8,%sp
	mov.l	(%sp)+,%d0
	mov.l	regblock_val(%a6),(%sp)
	bra.w	compiler_apply

### compiler_assignment_trap
###
### Expects a cached-variable extension object in A0, and the assignment
### value in A1.

	global	comentry_assignment_trap
comentry_assignment_trap:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)

comentry_assignment_trap_1:
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a1,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_compiler_assignment_trap
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_assignment_trap_error
	addq.l	&8,%sp
	clr.b	(%sp)
	rts

comentry_assignment_trap_error:
	mov.l	8(%sp),%d1		# Get return address
	bsr	compiled_entry_to_block
	mov.l	%d0,_compiled_code_error_code
	mov.l	(%a0),%d0		# Extract environment
	and.l	%d7,%d0
	mov.l	(0,%a0,%d0.l*4),%a0
	mov.l	(%sp),%a1		# Get extension
	mov.l	%a0,(%sp)		# Save environment
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%a0,-(%sp)
	mov.l	%a1,-(%sp)
	jsr	_compiler_var_error
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	%d0,-(%sp)			# Save name
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_assignment_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

	global	_comp_assignment_trap_restart
_comp_assignment_trap_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp),%d0		# name
	mov.l	4(%sp),%d1		# environment
	mov.l	8(%sp),%d2		# value
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d2,-(%sp)
	mov.l	%d0,-(%sp)
	mov.l	%d1,-(%sp)
	jsr	_Symbol_Lex_Set
	lea	12(%sp),%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	beq.b	comentry_assignment_trap_restart_continue
	mov.l	%d0,_compiled_code_error_code
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_assignment_trap_restart,-(%sp)
	bra.w	return_to_interpreter_error

comentry_assignment_trap_restart_continue:
	lea	12(%sp),%sp
	clr.b	(%sp)
	rts

### compiler_operator_reference_trap
###
### Called when a uuo-link references a variable which needs special attention.
### The conditions are the same as for COMPILER_APPLY.

	global	comentry_operator_reference_trap
comentry_operator_reference_trap:
	ext.l	%d0
	mov.l	%d0,-(%sp)
	lea	4(%sp),%a0			# Stack slot
	mov.l	(%a0),%d0
	and.l	%d7,%d0
	mov.l	%d0,%a1
	mov.l	4(%a1),%d0			# hunk3
	and.l	%d7,%d0
	mov.l	%d0,%a1
	mov.l	(%a1),%d0			# extension
	mov.l	%a5,_Free
	mov.l	%sp,_Ext_Stack_Pointer
	mov.l	c_save_stack,%sp
	mov.l	%d0,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_complr_operator_reference_trap
	addq.l	&8,%sp
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	cmp.l	%d0,&prim_done
	bne.b	comentry_operator_reference_trap_error
	mov.l	(%sp)+,%d0
	bra.w	compiler_apply

comentry_operator_reference_trap_error:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_op_ref_trap_restart,-(%sp)
	mov.l	%d0,_compiled_code_error_code
	bra.w	return_to_interpreter_error

### comp_op_ref_trap_restart
###
### This "re-fetches" the uuo-link from the compiled code block because
### the location may have been clobbered in the interim with a different
### object.

	global	_comp_op_ref_trap_restart
_comp_op_ref_trap_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp)+,%d0
	mov.l	(%sp),%d1			# procedure
	and.l	%d7,%d1
	mov.l	%d1,%a0
	mov.l	4(%a0),%d1			# hunk3
	and.l	%d7,%d1
	mov.l	%d1,%a0
	mov.l	4(%a0),%d1			# block
	and.l	%d7,%d1
	mov.l	8(%a0),%d2			# offset
	and.l	%d7,%d2
	mov.l	%d1,%a0
	mov.l	(0,%a0,%d2.l*4),(%sp)		# procedure
	bra.w	compiler_apply

### compiler_operator_trap
###
### Called when a uuo-link has been established to a value which is not
### a compiled procedure.  The call must be performed by apply.
### The conditions are the same as for COMPILER_APPLY.

	global	comentry_operator_trap
comentry_operator_trap:
	mov.l	(%sp),%d1			# uuo link
	and.l	%d7,%d1
	mov.l	%d1,%a0
	mov.l	4(%a0),(%sp)			# real operator
	ext.l	%d0
	mov.l	%d0,-(%sp)
	movq	&prim_apply,%d0
	bra.w	return_to_interpreter

### extract_uuo_link
###
### Get a compiled procedure from a cached operator reference.

	global	_extract_uuo_link
_extract_uuo_link:
	mov.l	4(%sp),%d0			# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	8(%sp),%d0			# offset
	mov.l	(0,%a0,%d0.l*4),%d0		# uuo link
	rts

### make_uuo_link
###
### Check its argument, and if it is a compiled procedure, store it in the
### destination.  Otherwise store a fake compiled procedure which will use
### apply (by way of comentry_operator_trap) when invoked.

	global	_make_uuo_link
_make_uuo_link:
	cmp.b	4(%sp),&tc_compiled_procedure	# operator
	bne.b	make_uuo_link_1
	mov.l	12(%sp),%d0			# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	16(%sp),%d0			# offset
	mov.l	4(%sp),(0,%a0,%d0.l*4)		# Store!
	movq	&prim_done,%d0
	rts

make_uuo_link_1:
	bsr.b	uuo_link_gc_check
	mov.l	_uuo_link_trap,%d0		# comentry_operator_trap
	lea	12(%sp),%a1			# args
	bra.w	make_fake_uuo_link_common

uuo_link_gc_check:
	mov.l	_Free,%a0
	cmp.l	%a0,_MemTop
	bge.b	uuo_link_gc_check_fail
uuo_link_gc_check_succeed:
	rts

uuo_link_gc_check_fail:
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	bne.b	uuo_link_gc_check_succeed
	lea	4(%sp),%sp		# Pop immediate return address
	lea	_Registers,%a0		# Canonicalize interrupt
	mov.l	&-1,regblock_memtop(%a0)
	movq	&prim_interrupt,%d0
	rts

### make_fake_uuo_link
###
### Makes a fake compiled procedure which calls
### comentry_operator_reference_trap when invoked.

	global	_make_fake_uuo_link
_make_fake_uuo_link:
	bsr.b	uuo_link_gc_check
	pea	(%a0)
	mov.b	&tc_hunk3,(%sp)
	mov.l	8(%sp),(%a0)+			# extension
	mov.l	12(%sp),(%a0)+			# block
	mov.l	16(%sp),(%a0)+			# offset
	mov.b	&tc_fixnum,-4(%a0)
	mov.l	(%sp)+,%d0
	mov.l	%d0,4(%sp)			# replace extension
	mov.l	_fake_uuo_link_trap,%d0		# entry point
	lea	8(%sp),%a1			# args

make_fake_uuo_link_common:
	pea	(%a0)
	mov.b	&tc_compiled_procedure,(%sp)	# fake procedure
	mov.l	%d0,(%a0)+			# entry point
	mov.l	8(%sp),(%a0)+			# environment
	mov.l	%a0,_Free			# finish consing
	mov.l	(%a1)+,%d0			# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	(%a1),%d0			# offset
	mov.l	(%sp)+,(0,%a0,%d0.l*4)		# Store!
	movq	&prim_done,%d0
	rts

### compiled_block_environment
###
### Given a compiled code block, it extracts the environment where
### the block was "loaded".

	global	_compiled_block_environment
_compiled_block_environment:
	mov.l	4(%sp),%d0
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d0
	and.l	&0xffffff,%d0
	mov.l	(0,%a0,%d0.l*4),%d0
	rts

### extract_variable_cache
###
### Given a compiled code block, and an offset, it extracts the
### variable cache at that location.

	global	_extract_variable_cache
_extract_variable_cache:
	mov.l	4(%sp),%d0		# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	8(%sp),%d0		# offset
	mov.l	(0,%a0,%d0.l*4),%d0
	rts

### store_variable_cache
###
### Given a variable cache, a compiled code block, and an offset,
### it stores the variable cache at that location.

	global	_store_variable_cache
_store_variable_cache:
	mov.l	8(%sp),%d0		# block
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	12(%sp),%d0		# offset
	mov.l	4(%sp),(0,%a0,%d0.l*4)	# Store!
	rts

### compiler_enclose
###
### inputs:	size of vector in D0.W
### outputs:	vector in A0
### used:	A0, A1, D0
###
### **** This probably doesn't work anymore.  No one uses it. ****
###
### Optimized for size of the calling sequence rather than speed.  It
### is assumed that this is inline coded when speed is required.
### Warning!  This does not check for GC overflow.

	global	compiler_enclose
compiler_enclose:
	mov.l	%a5,%a0		# Allocate space.
	lea	(4,%a5,%d0.w*4),%a5
	mov.l	%a0,regblock_temp(%a6)	# Save result.
	mov.b	&tc_vector,regblock_temp(%a6)
	mov.w	&tc_manifest_vector*0x10000,(%a0)+ # Write vector header.
	mov.w	%d0,(%a0)+
	mov.l	(%sp)+,%a1			# Get return address off stack.
	bra.b	compiler_enclose_loop_entry

compiler_enclose_loop:
	mov.l	(%sp)+,(%a0)+
compiler_enclose_loop_entry:
	dbf	%d0,compiler_enclose_loop

	mov.l	regblock_temp(%a6),%a0
	jmp	(%a1)

### compiler_wrong_number_of_arguments
###
### Expects to be used just after entering a compiled closure, so the
### conditions should be the same as for COMPILER_APPLY.

	global	compiler_wrong_number_of_arguments
compiler_wrong_number_of_arguments:
	ext.l	%d0
	mov.l	%d0,-(%sp)
	mov.l	&err_wrong_number_of_arguments,%d0
	bra.w	return_to_interpreter

### compiler_interrupt_procedure
### compiler_interrupt_continuation
###
### We are expecting the compiler to generate the following code at
### a procedure or continuation entry point:
###
###	label1:
###		jsr	regblock_compiler_interrupt_procedure(regs)
###		dc.w	<offset to block start for gc of tc_return_address>
###	entry_label:
###		cmp.l	rfree,regblock_memtop(regs)
###		bge.b	label1

	global	compiler_interrupt_procedure
compiler_interrupt_procedure:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	&tc_null*0x1000000+0,-(%sp)		# Dummy value.

compiler_interrupt_common:
	tst.b	regblock_memtop(%a6)	# Interrupt or GC?
	bmi.b	compiler_interrupt
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	bne.b	compiler_interrupt_common_1
	addq.l	&4,%sp
	clr.b	(%sp)			# Resume, skipping past entry test.
	mov.l	(%sp)+,%a0
	jmp	4(%a0)

compiler_interrupt_common_1:
	mov.l	&-1,regblock_memtop(%a6) # Set interrupt mark in register.

	global	compiler_interrupt
compiler_interrupt:
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_interrupt_restart,-(%sp)
	movq	&prim_interrupt,%d0
	bra.w	return_to_interpreter

	global	compiler_interrupt_continuation
compiler_interrupt_continuation:
	addq.l	&2,(%sp)
	mov.b	&tc_return_address,(%sp)
	mov.l	regblock_val(%a6),-(%sp)	# Save VAL.
	bra.b	compiler_interrupt_common

	global	_comp_interrupt_restart
_comp_interrupt_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp)+,regblock_val(%a6)		# Restore VAL.
	clr.b	(%sp)
	rts

### compiler_setup_lexpr
###
### inputs:	number of supplied arguments in D0.W
###		number of required+optional arguments in D1.W
###		rest argument (1 = yes, 0 = no) in D2.W
###
### Adjusts the stack frame by the appropriate amount; also performs a
### GC check.  Expects to be called with the following sequence so
### that it can compute the GC address to push if needed:
###
###		mov.w	&n,%d1
###		movq	&r,%d2
###		jsr	<compiler_setup_lexpr offset>(regs)

	global	compiler_setup_lexpr
compiler_setup_lexpr:
	mov.w	%d1,%d3			# Compute number of missing optionals.
	sub.w	%d0,%d3
	blt.b	setup_lexpr_5

### Simple case.  Shift the frame up enough to make room for the
### missing optionals and the rest variable, then clobber them with
### appropriate values.  Since this case does no consing, we do an
### explicit GC check here.

	cmp.l	%a5,regblock_memtop(%a6)
	blt.b	setup_lexpr_10
	bsr.b	setup_lexpr_interrupt
setup_lexpr_10:

	mov.l	%sp,%a0			# Source pointer for loop.
	mov.w	%d3,%d1			# Allocate extra space on stack.
	neg.w	%d1
	sub.w	%d2,%d1
	lea	(0,%sp,%d1.w*4),%sp
	mov.l	%sp,%a1			# Target pointer for loop.

### Because of the return address, we must loop d0+1 times.

setup_lexpr_1:
	mov.l	(%a0)+,(%a1)+
	dbf	%d0,setup_lexpr_1

	bra.b	setup_lexpr_3

setup_lexpr_2:
	mov.l	&tc_reference_trap*0x1000000+0,(%a1)+
setup_lexpr_3:
	dbf	%d3,setup_lexpr_2

	tst.w	%d2			# Write the rest argument if any.
	beq.b	setup_lexpr_4
	mov.l	&tc_null*0x1000000+0,(%a1)
setup_lexpr_4:
	rts

### In all subsequent cases, it is assumed that D2.W = 1, since the
### number of supplied arguments exceeds the number of fixed ones.
### This is because we assume that the procedure has been passed a
### legal number of arguments.

setup_lexpr_5:
	cmp.w	%d3,&-1
	bne.b	setup_lexpr_6

### Another easy case: just one extra argument needs to be made into a
### list, no stack frame shifting is required.

	cmp.l	%a5,regblock_memtop(%a6)
	blt.b	setup_lexpr_11
	bsr.b	setup_lexpr_interrupt
setup_lexpr_11:

	lea	(0,%sp,%d0.w*4),%a0	# Get the argument.
	mov.l	(%a0),%d3
	mov.l	%a5,(%a0)		# Cons it into a list.
	mov.b	&tc_list,(%a0)
	mov.l	%d3,(%a5)+
	mov.l	&tc_null*0x1000000+0,(%a5)+
	rts

### The tough case.  We must cons a list, then shift the stack frame
### down over the (now listified) extra arguments.

setup_lexpr_6:
	lea	(4,%sp,%d1.w*4),%a0	# Compute pointer to rest arguments.
	mov.l	%a0,%a2			# Save it for shift-down.
	neg.w	%d3			# Get length of list.
	mov.l	%d3,%d4			# Save for compiler_list_unconditional

	bsr.w	compiler_list		# Cons the list.
	tst.w	%d3
	bne.b	setup_lexpr_12
	bsr.b	setup_lexpr_interrupt
	mov.l   %d4,%d3
	bsr.w	compiler_list_unconditional	# Cons the list.

setup_lexpr_12:

	mov.l	%a1,-(%a0)		# Save the rest argument in place.

setup_lexpr_7:
	mov.l	-(%a2),-(%a0)		# Shift the stack frame down.
	dbf	%d1,setup_lexpr_7

	mov.l	%a0,%sp
	rts

	global	setup_lexpr_interrupt
setup_lexpr_interrupt:
	tst.b	regblock_memtop(%a6)	# Interrupt or GC?
	bmi.b	compiler_lexpr_interrupt
	bset	&int_gc_bit,_IntCode+3	# Set the GC interrupt bit.
	btst	&int_gc_bit,_IntEnb+3	# If GC not enabled, ignore it.
	bne.b	compiler_lexpr_interrupt
	rts

	global	compiler_lexpr_interrupt
compiler_lexpr_interrupt:
	mov.l	&-1,regblock_memtop(%a6) # Canonicalize
	addq.l	&4,%sp			# Don't need to continue caller.
	sub.l	&10,(%sp)		# Create GC-able return address.
	mov.b	&tc_return_address,(%sp)
	mov.w	%d0,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)		# Save arguments.
	mov.w	%d1,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)
	mov.w	%d2,-(%sp)
	mov.w	&tc_fixnum*0x100,-(%sp)
	mov.l	&tc_null*0x1000000+0,-(%sp)
	mov.l	&tc_return_code*0x1000000+rc_comp_lexpr_interrupt_restart,-(%sp)
	mov.l	&prim_interrupt,%d0
	bra.w	return_to_interpreter

	global	_comp_lexpr_interrupt_restart
_comp_lexpr_interrupt_restart:
	movm.l	%d2-%d7/%a0-%a6,-(%sp)
	mov.l	&0x00ffffff,%d7
	mov.l	%sp,c_save_stack
	mov.l	_Ext_Stack_Pointer,%sp
	mov.l	_Free,%a5
	lea	_Registers,%a6
	mov.l	(%sp)+,%d2			# Restore arguments.
	mov.l	(%sp)+,%d1
	mov.l	(%sp)+,%d0
	clr.b	(%sp)			# Restore return address.
	add.l	&10,(%sp)
	bra.w	compiler_setup_lexpr

### compiler_list
###
### inputs:	pointer to block of objects in A0
###		number of objects in block in D3.W
### outputs:	pointer past block of objects in A0
###		pointer to list in A1
###		GC needed in D3.W (0 = true)

### Assumes that D3 is at least 1.

compiler_list:
	movm.l	%d1/%a2,-(%sp)
	lea	(0,%a5,%d3.w*8),%a2	# Allocate the free space needed.
	cmp.l	%a2,regblock_memtop(%a6)
	blt.b	compiler_list_1
	clr.w	%d3
	movm.l	(%sp)+,%d1/%a2
	rts

### Create list regardless of whether Free has collided with MemTop

compiler_list_unconditional:
	movm.l	%d1/%a2,-(%sp)
	lea	(0,%a5,%d3.w*8),%a2	# Allocate the free space needed.

### Clever loop to create list.  Create a pointer to the head of the
### list which has pair type code.  Bump this in the loop to write the
### cdr of each of the pairs in the list.  The loop counter is
### decremented by two, causing the main loop to stop just before the
### last pair, which is handled specially.

compiler_list_1:
	exg	%a2,%a5
	mov.l	%a2,%d1			# Create a pointer to the first pair.
	or.l	&tc_list*0x1000000+0,%d1
	mov.l	%d1,%a1			# Save this as the return value.

	subq.w	&1,%d3			# Adjust to stop loop before last arg.
	bra.b	compiler_list_3

compiler_list_2:
	mov.l	(%a0)+,(%a2)+		# Copy argument to car of pair.
	addq.l	&8,%d1			# Adjust pointer to next pair.
	mov.l	%d1,(%a2)+		# Put it in this pair's cdr.
compiler_list_3:
	dbf	%d3,compiler_list_2

	mov.l	(%a0)+,(%a2)+		# Copy last argument to last pair.
	mov.l	&tc_null*0x1000000+0,(%a2)	# Put '() in last pair's cdr.
	movm.l	(%sp)+,%d1/%a2
	rts

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

	global	first_pc_code
first_pc_code:
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





unary_return_true:
	mov.l	&tc_true*0x1000000+0,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts	


	
	global	comentry_zero
comentry_zero:
	cmp.b	(%sp),&tc_fixnum
	bne.b	zero_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	beq	unary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts
zero_generic:
	mov.l	pc_zero,%d6
	bra	compiler_primitive_apply
	global	comentry_positive
comentry_positive:
	cmp.b	(%sp),&tc_fixnum
	bne.b	positive_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	bgt	unary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts
positive_generic:
	mov.l	pc_positive,%d6
	bra	compiler_primitive_apply
	global	comentry_negative
comentry_negative:
	cmp.b	(%sp),&tc_fixnum
	bne.b	negative_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	blt	unary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts
negative_generic:
	mov.l	pc_negative,%d6
	bra	compiler_primitive_apply



	global	comentry_increment
comentry_increment:
	cmp.b	(%sp),&tc_fixnum
	bne.b	increment_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	add.l	&256,%d0
	bvs.b	increment_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(%a6)
	mov.b	&tc_fixnum,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts
increment_generic:
	mov.l	pc_increment,%d6
	bra	compiler_primitive_apply
	global	comentry_decrement
comentry_decrement:
	cmp.b	(%sp),&tc_fixnum
	bne.b	decrement_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	sub.l	&256,%d0
	bvs.b	decrement_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(%a6)
	mov.b	&tc_fixnum,regblock_val(%a6)
	addq.l	&4,%sp
	clr.b	(%sp)
	rts
decrement_generic:
	mov.l	pc_decrement,%d6
	bra	compiler_primitive_apply

#### Binary procedures



binary_return_true:
	mov.l	&tc_true*0x1000000+0,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts	



	global	comentry_equal
comentry_equal:
	cmp.b	(%sp),&tc_fixnum
	bne.b	equal_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	equal_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	cmp.l	%d0,%d1
	beq	binary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
equal_generic:
	mov.l	pc_equal,%d6
	bra	compiler_primitive_apply
	global	comentry_less
comentry_less:
	cmp.b	(%sp),&tc_fixnum
	bne.b	less_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	less_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	cmp.l	%d0,%d1
	blt	binary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
less_generic:
	mov.l	pc_less,%d6
	bra	compiler_primitive_apply
	global	comentry_greater
comentry_greater:
	cmp.b	(%sp),&tc_fixnum
	bne.b	greater_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	greater_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	cmp.l	%d0,%d1
	bgt	binary_return_true
	mov.l	&tc_null*0x1000000+0,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
greater_generic:
	mov.l	pc_greater,%d6
	bra	compiler_primitive_apply



	global	comentry_plus
comentry_plus:
	cmp.b	(%sp),&tc_fixnum
	bne.b	plus_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	plus_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	add.l	%d1,%d0
	bvs.b	plus_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(%a6)
	mov.b	&tc_fixnum,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
plus_generic:
	mov.l	pc_plus,%d6
	bra	compiler_primitive_apply
	global	comentry_minus
comentry_minus:
	cmp.b	(%sp),&tc_fixnum
	bne.b	minus_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	minus_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	sub.l	%d1,%d0
	bvs.b	minus_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(%a6)
	mov.b	&tc_fixnum,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
minus_generic:
	mov.l	pc_minus,%d6
	bra	compiler_primitive_apply

	global	comentry_multiply
comentry_multiply:
	cmp.b	(%sp),&tc_fixnum
	bne.b	multiply_generic
	cmp.b	4(%sp),&tc_fixnum
	bne.b	multiply_generic
	mov.l	(%sp),%d0
	lsl.l	&8,%d0
	mov.l	4(%sp),%d1
	lsl.l	&8,%d1
	asr.l	&8,%d1
	muls.l	%d1,%d0
	bvs.b	multiply_generic
	lsr.l	&8,%d0
	mov.l	%d0,regblock_val(%a6)
	mov.b	&tc_fixnum,regblock_val(%a6)
	addq.l	&8,%sp
	clr.b	(%sp)
	rts
multiply_generic:
	mov.l	pc_multiply,%d6
	bra	compiler_primitive_apply

# This one is not optimized yet.

	global	comentry_divide
comentry_divide:
	mov.l	pc_divide,%d6
	bra	compiler_primitive_apply

#### Popper Entries

### Here are the three message receivers which are pushed on the stack
### by compiled code:

###	closure:
###		short	0x0000			# Type = closure receiver
###		short	<frame-size>		# Stack frame size
###		space	<frame-size>		# Stack frame
###		long	???			# Return address

###	stack:
###		short	0x0010			# Type = stack receiver
###		short	<frame-size>		# Stack frame size
###		space	<frame-size>		# Stack frame

###	subproblem:
###		short	0x0020			# Type = subproblem receiver
###		short	0x0000			# Stack frame size (always 0)
###		long	???			# Return address

### Here is the code for each of the message senders.  In each case,
### `frame_size' is the size of the stack frame about to be applied
### (in longwords), and `receiver_offset' is the distance to the first
### receiver (in bytes).

###	apply_closure:
###		mov.w	&frame_size,%d1
###		lea	receiver_offset(%sp),%a0
###		lea	<application>,%a1
###		jmp	apply_closure_offset(regs)

###	apply_stack:
###		movq	&n_levels,%d0
###		mov.w	&frame_size,%d1
###		lea	receiver_offset(%sp),%a0
###		lea	<application>,%a1
###		jmp	apply_stack_offset(regs)

###	value:
###		lea	receiver_offset(%sp),%sp
###		jmp	value_offset(regs)

message_block_start:

### Numbers in brackets are the sizes of the corresponding
### instructions in bytes.  If the entries exceed 16 bytes, then the
### compiler must be changed to have the new offsets into the array.

### If the total size of the message block exceeds 192 bytes, update
### the value of `regblock_length' accordingly.







### ------------------------------------------------------------------

message_apply_closure:
	mov.w	(%a0),%d2
	jmp	(((message_apply_closure + 14) - .),%pc,%d2.w) # [6]
	space	10			# Pad to 16 bytes.

### closure:
	addq.l	&2,%a0
	add.w	(%a0)+,%a0	# [4] Pointer to frame start.
	lea	(0,%sp,%d1.w*4),%a2
	bra.b	copy_invoke_loop_entry			# [6]
	space	6			# Pad to 16 bytes.

### stack:
	addq.l	&2,%a0
	add.w	(%a0)+,%a0	# [4] Move to next receiver.
	mov.w	(%a0),%d2
	jmp	(((message_apply_closure + 14) - .),%pc,%d2.w) # [6]
	space	6			# Pad to 16 bytes.

### subproblem:
	addq.l	&4,%a0			# [2] Discard message receiver.
	lea	(0,%sp,%d1.w*4),%a2
	bra.b	copy_invoke_loop_entry			# [6]
	space	8			# Pad to 16 bytes.

message_apply_stack:
	mov.w	(%a0),%d2
	jmp	(((message_apply_stack + 14) - .),%pc,%d2.w) # [6]

copy_invoke_loop:
	mov.l	-(%a2),-(%a0)		# [2] Copy the frame down.
copy_invoke_loop_entry:
	dbf	%d1,copy_invoke_loop	# [4]

	mov.l	%a0,%sp			# [2]
	jmp	(%a1)			# [2] Perform invocation.

### closure:
	lea	(0,%sp,%d1.w*4),%a2
	bra.b	copy_invoke_loop_entry			# [6]

message_apply_stack_continue:
	addq.l	&2,%a0
	add.w	(%a0)+,%a0	# [4] Move to next receiver.
	mov.w	(%a0),%d2
	jmp	(((message_apply_stack + 14) - .),%pc,%d2.w) # [6]

### stack:
	dbf	%d0,message_apply_stack_continue # [4]
	lea	(0,%sp,%d1.w*4),%a2
	bra.b	copy_invoke_loop_entry			# [6]
	space	6			# Pad to 16 bytes.

### subproblem:
	lea	(0,%sp,%d1.w*4),%a2
	bra.b	copy_invoke_loop_entry			# [6]
	space	10			# Pad to 16 bytes.

### ------------------------------------------------------------------

message_value:
	mov.w	(%sp),%d2
	jmp	(((message_value + 14) - .),%pc,%d2.w) # [6]
	space	10			# Pad to 16 bytes.

### closure:
	addq.l	&2,%sp
	add.w	(%sp)+,%sp	# [4] Move to return address.
	clr.b	(%sp)
	rts	# [4] Return to caller.
	space	8			# Pad to 16 bytes.

### stack:
	addq.l	&2,%sp
	add.w	(%sp)+,%sp	# [4] Move to next receiver.
	mov.w	(%sp),%d2
	jmp	(((message_value + 14) - .),%pc,%d2.w) # [6]
	space	6			# Pad to 16 bytes.

### subproblem:
	addq.l	&2,%sp
	add.w	(%sp)+,%sp	# [4] Move to return address.
	clr.b	(%sp)
	rts	# [4] Return to caller.
	space	8			# Pad to 16 bytes.

### compiler_initialize



	global	_compiler_initialize
_compiler_initialize:
	lea	_Registers,%a0		# first 20 entry points
	lea	regblock_entries(%a0),%a0
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_apply,(%a0)+				#  0
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_error,(%a0)+				#  1
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_wrong_number_of_arguments,(%a0)+	#  2
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_interrupt_procedure,(%a0)+		#  3
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_interrupt_continuation,(%a0)+		#  4
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_lookup_apply,(%a0)+			#  5
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_reference,(%a0)+			#  6
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_access,(%a0)+				#  7
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_unassigned_p,(%a0)+			#  8
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_unbound_p,(%a0)+			#  9
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_assignment,(%a0)+			# 10
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_definition,(%a0)+			# 11
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_primitive_apply,(%a0)+		# 12
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_enclose,(%a0)+			# 13
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_setup_lexpr,(%a0)+			# 14
###
### The definition of `offset_return_to_interpreter' depends on this.
###
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_return_to_interpreter,(%a0)+		# 15
###
	mov.w	&0x4ef9,(%a0)+
	mov.l	&compiler_safe_reference,(%a0)+			# 16
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_lookup,(%a0)+			# 17
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_lookup_trap,(%a0)+			# 18
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_assignment_trap,(%a0)+		# 19

	lea	_Registers,%a0		# second 20 entry points
	lea	regblock_entries_2(%a0),%a0
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_operator,(%a0)+			# 20
###
### The definition of `offset_fake_uuo_link_trap' depends on this.
###
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_operator_reference_trap,(%a0)+	# 21
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_reference_apply,(%a0)+		# 22
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_safe_ref_trap,(%a0)+			# 23
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_unassigned_p_trap,(%a0)+		# 24
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_lookup_multiple,(%a0)+		# 25
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_operator_multiple,(%a0)+	# 26
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_plus,(%a0)+				# 27
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_minus,(%a0)+				# 28
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_multiply,(%a0)+			# 29
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_divide,(%a0)+				# 30
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_equal,(%a0)+				# 31
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_less,(%a0)+				# 32
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_greater,(%a0)+			# 33
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_increment,(%a0)+			# 34
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_decrement,(%a0)+			# 35
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_zero,(%a0)+				# 36
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_positive,(%a0)+			# 37
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_negative,(%a0)+			# 38
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_assignment,(%a0)+ 		# 39
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_cache_assignment_multiple,(%a0)+	# 40
###
### The definition of `offset_uuo_link_trap' depends on this.
###
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_operator_trap,(%a0)+			# 41
	mov.w	&0x4ef9,(%a0)+
	mov.l	&comentry_primitive_lexpr_apply,(%a0)+		# 42

### Initialize message block.

	lea	_Registers,%a0
	lea	regblock_messages(%a0),%a0
	lea	message_block_start,%a1
	movq	&47,%d0

copy_message_loop:
	mov.l	(%a1)+,(%a0)+
	dbf	%d0,copy_message_loop

	tst.l	4(%sp)				# FASL_It
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
	mov.l	%d0,-(%sp)
	mov.l	%a0,-(%sp)
	jsr	_copy_to_constant_space
	addq.l	&8,%sp
	mov.l	%d0,%a0
	mov.l	(%sp)+,%d0

	subq.l	&1,%d0				# init vector header
	mov.l	%d0,(%a0)
	mov.b	&tc_manifest_vector,(%a0)
	subq.l	&1,%d0				# init NM vector header
	mov.l	%d0,4(%a0)
	mov.b	&tc_manifest_nm_vector,4(%a0)

### Cache the special entry points

	global	compiler_reset
compiler_reset:
	mov.l	%a0,-(%sp)
	mov.b	&tc_compiled_code_block,(%sp)
	mov.l	(%sp),_compiler_utilities

	mov.b	&tc_return_address,(%sp)
	mov.l	(%sp)+,%d0

	mov.l	%d0,%d1
	add.l	&constant_return_to_interpreter-constant_start,%d1
	mov.l	%d1,_return_to_interpreter

	mov.l	%d0,%d1
	add.l	&constant_uuo_link_trap-constant_start,%d1
	mov.l	%d1,_uuo_link_trap

	add.l	&constant_fake_uuo_link_trap-constant_start,%d0
	mov.l	%d0,_fake_uuo_link_trap

### Cache the relevant primitives

	lea	first_pc_code,%a0	# Setup pointers
	mov.l	%a0,-(%sp)
	lea	first_pc_name,%a0
	mov.l	%a0,-(%sp)
	bra.b	enter_primitive_loop

primitive_loop:
	mov.l	(%sp),%a0
	mov.l	(%a0),-(%sp)
	jsr	_make_primitive
	addq.l	&4,%sp	# Name string pointer
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
### The only consistency check performed is that the blocks
### have the same length.

	global	_compiler_reset
_compiler_reset:
	cmp.b	4(%sp),&tc_compiled_code_block
	bne.w	compiler_reset_error

	mov.l	4(%sp),%d0
	and.l	&0xffffff,%d0
	mov.l	%d0,%a0
	mov.l	(%a0),%d1
	and.l	&0xffffff,%d1
	lea	constant_end,%a1
	mov.l	%a1,%d0
	lea	constant_start,%a1
	sub.l	%a1,%d0
	lsr.l	&2,%d0
	subq.l	&1,%d0
	cmp.l	%d0,%d1
	bne.w	compiler_reset_error
	subq.l	&1,%d0
	bra.w	compiler_reset	

compiler_reset_error:
	
	jsr	_compiler_reset_error
	
	rts

### The following block is copied to constant space, and the
### interpreter variable return_to_interpreter is made to point
### to the entry point.

	lalign	4
constant_start:
	long	0				# Vector header
	long	0				# NM header
###
	short	constant_return_to_interpreter-constant_start
constant_return_to_interpreter:
	jmp	offset_return_to_interpreter(%a6)
###
	short	constant_fake_uuo_link_trap-constant_start
constant_fake_uuo_link_trap:
	jmp	offset_fake_uuo_link_trap(%a6)
###
	short	constant_uuo_link_trap-constant_start
constant_uuo_link_trap:
	jmp	offset_uuo_link_trap(%a6)
###
	lalign	4
constant_end:
	long	0
