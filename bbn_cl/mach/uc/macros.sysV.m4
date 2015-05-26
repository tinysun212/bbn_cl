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

#### $Header: macros.sysV.m4,v 10.0 88/12/07 13:09:06 las Exp $
####
#### M4 macros for cmp68020.s file.  Version for smart m4.
####

### push the arguments in order, left to right.

define(push,
	`ifelse(eval($# > 1), 1,
	`mov.l	$1,-(%sp)
	push(shift($*))',
	eval((len($1)) > 0), 1,
	`mov.l	$1,-(%sp)')')

### push the arguments in order, right to left.

define(push_reversed,
	`ifelse(eval($# > 1), 1,
	`push_reversed(shift($*))
	mov.l	$1,-(%sp)',
	eval((len($1)) > 0), 1,
	`mov.l	$1,-(%sp)')')

### pop the arguments in order, right to left.

define(pop,
	`ifelse(eval($# > 1), 1,
	`pop(shift($*))
	mov.l	(%sp)+,$1',
	eval((len($1)) > 0), 1,
	`mov.l	(%sp)+,$1')')

define(c_call_c,
	`push_reversed(shift($*))
	perform_c_call(eval($# - 1),extern_c_label($1))')

define(c_call,
	`switch_to_interpreter_registers()
	c_call_c($*)
	switch_to_compiled_code_registers()')

define(computed_c_call,
	`switch_to_interpreter_registers()
	push_reversed(shift($*))
	perform_c_call(eval($# - 1),$1)
	switch_to_compiled_code_registers()')

