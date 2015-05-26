/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* $Header: compiler.c,v 10.0 88/12/07 13:06:29 las Exp $
 * $MIT-Header: compiler.c,v 9.32 88/03/21 21:15:21 GMT jinx Exp $
 *
 * Stubs for compiler utilities.  
 * All entries error out or kill the microcode.
 *
 */

#include "config.h"	/* Pointer type declaration */
#include "object.h"	/* Making pointers */
#include "sdata.h"	/* Needed by const.h */
#include "types.h"	/* Needed by const.h */
#include "errors.h"	/* Error codes and Termination codes */
#include "const.h"	/* REGBLOCK_MINIMUM_LENGTH */
#include "returns.h"	/* RC_POP_FROM_COMPILED_CODE */

extern long
  compiler_interface_version,
  compiler_processor_type;

extern Pointer
  Registers[],
  compiler_utilities,
  return_to_interpreter;

extern long
  enter_compiled_expression(), 
  apply_compiled_procedure(),
  return_to_compiled_code(),
  make_fake_uuo_link(),
  make_uuo_link(),
  compiled_entry_to_block_offset();

extern Pointer
  extract_uuo_link(),
  extract_variable_cache(),
  compiled_block_environment(),
  *compiled_entry_to_block_address();

extern void
  store_variable_cache(),
  compiled_entry_type(),
  Microcode_Termination();

Pointer
  Registers[REGBLOCK_MINIMUM_LENGTH],
  compiler_utilities,
  return_to_interpreter;

long
  compiler_interface_version,
  compiler_processor_type;

long
enter_compiled_expression ()
{
  return (ERR_EXECUTE_MANIFEST_VECTOR);
}

long
apply_compiled_procedure ()
{
  return (ERR_BAD_COMBINATION);
}

long
return_to_compiled_code ()
{
  return (ERR_INAPPLICABLE_CONTINUATION);
}

/* Bad entry points. */

long
make_fake_uuo_link(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
make_uuo_link(value, extension, block, offset)
     Pointer value, extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
extract_uuo_link(block, offset)
     Pointer block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
store_variable_cache(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
extract_variable_cache(block, offset)
     Pointer block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
compiled_block_environment(block)
     Pointer block;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer *
compiled_entry_to_block_address(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
compiled_entry_to_block_offset(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
compiled_entry_type(entry, buffer)
     Pointer entry, *buffer;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

#define losing_return_address(name)					\
extern long name();							\
long									\
name()									\
{									\
  Microcode_Termination (TERM_COMPILER_DEATH);				\
  /*NOTREACHED*/							\
}

losing_return_address (comp_interrupt_restart)
losing_return_address (comp_lookup_apply_restart)
losing_return_address (comp_reference_restart)
losing_return_address (comp_access_restart)
losing_return_address (comp_unassigned_p_restart)
losing_return_address (comp_unbound_p_restart)
losing_return_address (comp_assignment_restart)
losing_return_address (comp_definition_restart)
losing_return_address (comp_safe_reference_restart)
losing_return_address (comp_lookup_trap_restart)
losing_return_address (comp_assignment_trap_restart)
losing_return_address (comp_op_lookup_trap_restart)
losing_return_address (comp_cache_lookup_apply_restart)
losing_return_address (comp_safe_lookup_trap_restart)
losing_return_address (comp_unassigned_p_trap_restart)
losing_return_address (comp_link_caches_restart)

/* NOP entry points */

extern void
  compiler_reset(),
  compiler_initialize();

extern long
  coerce_to_compiled();

void
compiler_reset (new_block)
     Pointer new_block;
     long version;
{
  extern void compiler_reset_error();

  if (new_block != NIL)
  {
    compiler_reset_error();
  }
  return;
}

void
compiler_initialize ()
{
  compiler_processor_type = 0;
  compiler_interface_version = 0;
  compiler_utilities = NIL;
  return_to_interpreter =
    (Make_Non_Pointer (TC_RETURN_CODE, RC_POP_FROM_COMPILED_CODE));
  return;
}

/* Identity procedure */

long
coerce_to_compiled(object, arity, location)
     Pointer object, *location;
     long arity;
{
  *location = object;
  return (PRIM_DONE);
}
