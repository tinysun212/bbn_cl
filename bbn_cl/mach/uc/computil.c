/*
                                ********

Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
Beranek and Newman Inc.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice and this permission appear in all copies and in
supporting documentation, and that the name Bolt, Beranek and Newman
Inc. not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.  In
addition, BBN makes no respresentation about the suitability of this
software for any purposes.  It is provided "AS IS" without express or
implied warranties including (but not limited to) all implied warranties
of merchantability and fitness.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from
loss of use, data or profits, whether in an action of contract,
negligence or other tortuous action, arising out of or in connection
with the use or performance of this software.

                                ********
*/

#include "scheme.h"
#include "primitive.h"
#include "cmp68kgc.h"

DEFINE_PRIMITIVE("SHORT-FETCH", Prim_Short_Fetch, 1)
{
  short int *address;
  PRIMITIVE_HEADER(1);

  address = ((short int *) Get_Pointer(ARG_REF(1)));
  PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM, *address));
}

/*
  Given a compiled procedure, return the number of arguments it takes.
 */

#define COMPILED_CODE_ADDRESS_P(object)			\
   ((OBJECT_TYPE (object)) == TC_COMPILED_ENTRY)

DEFINE_PRIMITIVE("COMPILED-GET-NARGS", Prim_Get_Nargs, 1)
{
  extern void compiled_entry_type();
  long reqs, opts, rest, buffer[3];
  Pointer result;
  PRIMITIVE_HEADER(1);

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  Primitive_GC_If_Needed(6);

  compiled_entry_type(ARG_REF(1), &buffer[0]);

  if (buffer[0] != 0)
  {
    /* It's not a procedure */

    error_bad_range_arg(1);
  }

  reqs = buffer[1] - 1;
  if (buffer[2] < 0)
  {
    rest = 1;
    opts = (- ((buffer[2]) + (1 + (buffer[1]))));
  }
  else
  {
    rest = 0;
    opts = ((buffer[2]) - (buffer[1]));
  }

  result = Make_Pointer(TC_LIST, Free);

  *Free++ = MAKE_UNSIGNED_FIXNUM(reqs);
  Free += 1;
  Free[-1] = Make_Pointer(TC_LIST, Free);
  *Free++ = MAKE_UNSINGED_FIXNUM(opts);
  Free += 1;
  Free[-1] = Make_Pointer(TC_LIST, Free);
  *Free++ = MAKE_UNSINGED_FIXNUM(rest);
  *Free++ = NIL;

  PRIMITIVE_RETURN(result);
}

/*
  Given a return address, try and back up to the PEA, MOVEB or
  LEA, MOVEL, MOVEB sequence that pushed it.  We return a list:
    ((#T -> LEA, #F -> PEA . FIXNUM) ... )

*/

DEFINE_PRIMITIVE("COMPILED-GET-USERS", Prim_Get_Pushes, 2)
{
  Pointer entry, result, cons, new;
  long range, target, delta;
  struct pea_sequence_like
  {
    short unsigned int pea_opcode;
    short int pea_offset;
    short unsigned int moveb_opcode;
    short unsigned int moveb_datum;
  } *pea_sequence;
  struct lea_sequence_like
  {
    short unsigned int lea_opcode;
    short int lea_offset;
    short unsigned int allocate_op;
    short unsigned int moveb_opcode;
    short unsigned int moveb_datum;
  } *lea_sequence;
  PRIMITIVE_HEADER(2);

  entry = ARG_REF(1);
  if ((!FIXNUM_P(entry)) && (!COMPILED_CODE_ADDRESS_P(entry)))
    error_wrong_type_arg(1);
  range = FIXNUM_ARG(2);

  if ((range >= 65536) || (range < -65536))
    error_bad_range_arg(2);
    
  if (range == 0)
    PRIMITIVE_RETURN(NIL);

  Primitive_GC_If_Needed(10);

  result = NIL;
  pea_sequence = ((struct pea_sequence_like *) Get_Integer(entry));
  lea_sequence = ((struct lea_sequence_like *) pea_sequence);
  target = ((long) pea_sequence);
  delta = 2;

  if (range < 0)
  {
    range = - range;
    delta = -2;
  }

  while (range-- > 0)
  {
    if (((pea_sequence->pea_opcode & 0xffc0) == 0x4840) &&
	((pea_sequence->pea_offset + ((long) pea_sequence) + 2) == target) &&
	((pea_sequence->moveb_opcode & 0xf03f) == 0x103c) &&
	(pea_sequence->moveb_datum == 0x39))
    {
      cons = Make_Pointer(TC_LIST, Free);
      *Free++ = NIL;
      *Free++ = Make_Non_Pointer(TC_FIXNUM, pea_sequence);
      new = Make_Pointer(TC_LIST, Free);
      *Free++ = cons;
      *Free++ = result;
      result = new;
    }
    
    if (((lea_sequence->lea_opcode & 0xf1ff) == 0x41fa) &&
	((lea_sequence->lea_offset + ((long) lea_sequence) + 2) == target) &&
	(lea_sequence->allocate_op == 0x2ac8) &&
	((lea_sequence->moveb_opcode & 0xf03f) == 0x103c) &&
	(lea_sequence->moveb_datum == 0x39))
    {
      cons = Make_Pointer(TC_LIST, Free);
      *Free++ = TRUTH;
      *Free++ = Make_Non_Pointer(TC_FIXNUM, lea_sequence);
      new = Make_Pointer(TC_LIST, Free);
      *Free++ = cons;
      *Free++ = result;
      result = new;
    }
    
    pea_sequence = ((struct pea_sequence_like *) ((long) pea_sequence) + delta);
    lea_sequence = ((struct lea_sequence_like *) pea_sequence);
  }

  PRIMITIVE_RETURN(result);
}

/*
  Given a pointer to a PEA continuation sequence, scan backwards
  until we find the CMPL BGES sequence and return that address.  
*/

DEFINE_PRIMITIVE("COMPILED-GET-HEAD", Prim_Find_Sequence_Header, 2)
{
  long range;
  struct heap_check_like
  {
    short unsigned int cmpl_inst;
    short unsigned int bges_inst;
  } *heap_check;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);

  Range_Check(range, Arg2, 1, 65536, ERR_ARG_2_BAD_RANGE);

  heap_check = (struct heap_check_like *) Get_Integer(Arg1);
  while (range-- > 0)
  {
    if (heap_check->cmpl_inst == 0xbbd6 &&
	heap_check->bges_inst == 0x6cf6)
    {
      PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM, ((long) heap_check)));
    }
    heap_check = ((struct heap_check_like *) ((long) heap_check) - 2);
  }

  PRIMITIVE_RETURN(NIL);
}

/*
  Given a pointer to an LEA sequence, skip down, to the JSR #x132(A6)
  and then back up to get the offset of the quad pointer.

  Given a pointer to an LEA sequence, skip down and see if we are storing
  through a D register and if so, can backwards for a PC relative MOVE
  which should get us the quad pointer.
*/

DEFINE_PRIMITIVE("COMPILED-GET-LEA-SYMBOL", Prim_Lea_To_Symbol, 2)
{
  short unsigned int *overlay;
  int offset, index, range, found, word, rcheck;
  Pointer result;
  Primitive_2_Args();
  
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(range, Arg2, 1, 65536, ERR_ARG_2_BAD_RANGE);
  overlay = ((short unsigned int *) Get_Integer(Arg1));

  found = false;

  if ((overlay[7] & 0xf1ff) == 0x21ae)
  {
    /* move this value through Dn */

    for (index = -1; index > -100; index--)
    {
      if ((overlay[index] & 0xf1ff) == 0x203a)
      {
	index++;
	found = true;
	break;
      }
    }
  }

  if (!found) for (index = 1; index < range; index++)
  {
    word = overlay[index];
    if (word == 0x0132)
    {
      /* call to compiler definition */

      if (overlay[index-1] == 0x4eae)
      {
	index -= 2;
	found = true;
	break;
      }
    }
    if (word == 0x012c)
    {
      /* call to compiler assignment */

      if (overlay[index-1] == 0x4eae)
      {
	index -= 2;
	found = true;
	break;
      }
    }
    if (word == 0x203a)
    {
      /* move l the symbol into a register */

      index++;
      found = true;
      break;
    }
  }

  if (!found)
    PRIMITIVE_RETURN(NIL);

  for (rcheck = 0; rcheck < index; rcheck++)
    if (overlay[rcheck] == 0x4e75)
    {
      /* RTS before the reference? */

      PRIMITIVE_RETURN(NIL);
    }

  offset = overlay[index];
  result = *((Pointer *) (Get_Integer(Arg1) + offset + index + index));

  if (OBJECT_TYPE(result) == TC_QUAD)
    PRIMITIVE_RETURN(Fast_Vector_Ref(result, TRAP_EXTENSION_NAME));

  if (OBJECT_TYPE(result) == TC_VARIABLE)
    PRIMITIVE_RETURN(Fast_Vector_Ref(result, VARIABLE_SYMBOL));

  PRIMITIVE_RETURN(result);
}

/*
  Given a pointer to a PEA continuation sequence, scan forward until
  we do a compiler apply, then find the target of that compiler apply's
  LEA instruction so we can get our procedure name.
*/

/*
  Given a return address, return the environment it must be enclosed
  in by finding the last item in the code block.
*/

/*
  Given a pointer to an entry point, peek back a bit and see if there
  is an offset which gets us to a symbol.
*/

DEFINE_PRIMITIVE("GET-ENTRY-SYMBOL", Prim_Get_Entry_Symbol, 1)
{
  short int *offset;
  Pointer *code_block;
  Pointer *symbol_pointer, symbol;
  Primitive_1_Arg();

  offset = ((short int *) Get_Pointer(Arg1));
  offset -= 2;			/* Skip past the block offset. */

  if (*offset <= 0)
    PRIMITIVE_RETURN(NIL);

  Get_Compiled_Block(code_block, Get_Pointer(Arg1));

  symbol_pointer = (Pointer *) ((char *)code_block + (*offset));
  symbol = *symbol_pointer;

  if (OBJECT_TYPE(symbol) != TC_INTERNED_SYMBOL)
    PRIMITIVE_RETURN(NIL);

  PRIMITIVE_RETURN(symbol);
}
