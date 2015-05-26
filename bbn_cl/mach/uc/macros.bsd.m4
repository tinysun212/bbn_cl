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

#### $Header: macros.bsd.m4,v 10.0 88/12/07 13:09:04 las Exp $
####
#### M4 macros for cmp68020.s file.  Version for dumb m4.
####

###
###	BSD 4.2 M4 does not have shift, $#, $@, or $*
###
###	These will not work in general (ie, more than 9 arguments).
###

define(count_args,
	`ifelse(len($1), 0, 0,
	`incr(count_args($2,$3,$4,$5,$6,$7,$8,$9))')')

define(shift_collect,
	`ifelse(eval($1 < 2),1,$2,
		`$2,shift_collect(($1-1),$3,$4,$5,$6,$7,$8,$9)')')

define(m4_shift,
	`shift_collect(count_args($2,$3,$4,$5,$6,$7,$8,$9),
		       $2,$3,$4,$5,$6,$7,$8,$9)')

### push the arguments in order, left to right.

define(push,
	`ifelse(eval(count_args($1,$2,$3,$4,$5,$6,$7,$8,$9) > 1), 1,
	`push_one($1)
	push(m4_shift($1,$2,$3,$4,$5,$6,$7,$8,$9))',
	eval((len($1)) > 0), 1,
	`push_one($1)')')

### push the arguments in order, right to left.

define(push_reversed,
	`ifelse(eval(count_args($1,$2,$3,$4,$5,$6,$7,$8,$9) > 1), 1,
	`push_reversed(m4_shift($1,$2,$3,$4,$5,$6,$7,$8,$9))
	push_one($1)',
	eval((len($1)) > 0), 1,
	`push_one($1)')')

### pop the arguments in order, right to left.

define(pop,
	`ifelse(eval(count_args($1,$2,$3,$4,$5,$6,$7,$8,$9) > 1), 1,
	`pop(m4_shift($1,$2,$3,$4,$5,$6,$7,$8,$9))
	pop_one($1)',
	eval((len($1)) > 0), 1,
	`pop_one($1)')')

define(c_call_c,
	`push_reversed(m4_shift($1,$2,$3,$4,$5,$6,$7,$8,$9))
	perform_c_call(eval(count_args($2,$3,$4,$5,$6,$7,$8,$9)),extern_c_label($1))')

define(c_call,
	`switch_to_interpreter_registers()
	c_call_c($1,$2,$3,$4,$5,$6,$7,$8,$9)
	switch_to_compiled_code_registers()')

define(computed_c_call,
	`switch_to_interpreter_registers()
	push_reversed(m4_shift($1,$2,$3,$4,$5,$6,$7,$8,$9))
	perform_c_call(eval(count_args($2,$3,$4,$5,$6,$7,$8,$9)),$1)
	switch_to_compiled_code_registers()')
