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

/*          Hey EMACS, this is -*- C -*- code!                 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "zones.h"
#ifdef TC_CLSAV
#include "cl-symbol.h"
#endif TC_CLSAV

extern int Debug_Flags[];

/* This is a copy of GCLoop, with GC_Mode handling added, and
   debugging printout removed.
*/

/* Purify modes */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

#if false

/* Why is the CONSTANT_COPY test needed?  It seems redundant with
   the test in Setup_Pointer_Internal.

   If it can go away (as in the version below this one), the code
   becomes identical to GC_Pointer in gcloop.c, CONSTANT_COPY becomes
   identical to NORMAL_GC.
 */

#define Purify_Pointer(Code)						\
Old = Get_Pointer(Temp);						\
if ((GC_Mode == CONSTANT_COPY) &&					\
    (Old > Low_Constant))						\
  continue;								\
Code

#else

#define Purify_Pointer(Code)						\
Old = Get_Pointer(Temp);						\
Code

#endif

#define Setup_Pointer_for_Purify(Extra_Code)				\
Purify_Pointer(Setup_Pointer(false, Extra_Code))

#define Indirect_BH(In_GC)						\
if (OBJECT_TYPE(*Old) == TC_BROKEN_HEART)				\
  continue;

#define Transport_Vector_Indirect()					\
Real_Transport_Vector();						\
*Get_Pointer(Temp) = New_Address

#define Purify_Compiled_Entry()						\
{									\
  Purify_Pointer(Setup_Internal(false,					\
				Transport_Compiled(),			\
				Compiled_BH(false, continue)));		\
}

Pointer *
PurifyLoop(Scan, To_Pointer, GC_Mode)
     fast Pointer *Scan;
     Pointer **To_Pointer;
     int GC_Mode;
{
  fast Pointer *To, *Old, Temp, *Low_Constant, New_Address;

  To = *To_Pointer;
  Low_Constant = ((Pointer *) SHARED_DATA->Code_Base);
  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan == (Get_Pointer(Temp)))
	{
	  *To_Pointer = To;
	  return (Scan);
	}
	sprintf(gc_death_message_buffer,
		"purifyloop: broken heart (0x%lx) in scan",
		Temp);
	gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	/*NOTREACHED*/

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += Get_Integer(Temp);
	break;

      case_Non_Pointer:
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	if (GC_Mode == PURE_COPY)
	{
	  /* Unlike the normal purifier, finding a linkage
	     section in pure space is not lethal.
	     Most of code space is pure, and thus there will in
	     fact be such sections in (conceptual) pure space.
	     The reason it is not harmful is because quads
	     and uuo-links are always allocated in constant space,
	     thus they are safe to store in pure space.
	   */

	  if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	     */

	    Scan += READ_CACHE_LINKAGE_COUNT(Temp);
	  }
	  else
	  {
	    Scan = END_OPERATOR_LINKAGE_AREA(Scan,
					     READ_OPERATOR_LINKAGE_COUNT(Temp));
	  }
	}
	else

	{
	  if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	     */

	    fast long count;

	    Scan++;
	    for (count = READ_CACHE_LINKAGE_COUNT(Temp);
		 --count >= 0;
		 Scan += 1)
	    {
	      Temp = *Scan;
	      Setup_Pointer_for_Purify(Transport_Quadruple());
	    }
	    Scan -= 1;
	  }
	  else
	  {
	    fast long count;
	    fast machine_word *word_ptr;
	    Pointer *end_scan;

	    count = READ_OPERATOR_LINKAGE_COUNT(Temp);
	    word_ptr = FIRST_OPERATOR_LINKAGE_ENTRY(Scan);
	    end_scan = END_OPERATOR_LINKAGE_AREA(Scan, count);

	    while(--count >= 0)
	    {
	      Scan = OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr);
	      word_ptr = NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr);
	      Temp = *Scan;
	      Purify_Compiled_Entry();
	    }
	    Scan = end_scan;
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	machine_word *start_ptr;
	fast machine_word *word_ptr;

	if (GC_Mode == PURE_COPY)
	{
	  gc_death(TERM_COMPILER_DEATH,
		   "purifyloop: manifest closure in pure area",
		   Scan, To);
	  /*NOTREACHED*/
	}

	Scan += 1;
	word_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Scan);
	start_ptr = word_ptr;

	while (VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  Scan = MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr);
	  word_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	  Temp = *Scan;
	  Purify_Pointer(Setup_Internal(false,
					Transport_Compiled(),
					Compiled_BH(false, continue)));
	}
	Scan = MANIFEST_CLOSURE_END(word_ptr, start_ptr);
	break;
      }

      case_compiled_entry_point:
	if (GC_Mode != PURE_COPY)
	{
	  Purify_Compiled_Entry();
	}
	break;

      case_Cell:
	/* Cells are typically used to contain the values of
	   "settable" variables in compiled code.
	   As such, their contents can change, and they should
	   therefore not be purified.
	 */
	if (GC_Mode != PURE_COPY)
	{
	  Setup_Pointer_for_Purify(Transport_Cell());
	}
	break;

      /*
	Symbols, cells, environments, and reference traps cannot be
	put into pure space.  The strings contained in symbols can, on
	the other hand.
       */

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM(Temp) <= TRAP_MAX_IMMEDIATE) ||
	    (GC_Mode == PURE_COPY))
	{
	  /* It is a non pointer. */
	  break;
	}
	goto purify_pair;

      case TC_INTERNED_SYMBOL:
      case TC_UNINTERNED_SYMBOL:
	if (GC_Mode == PURE_COPY)
        {
	  Pointer Orig_Temp = Temp;

	  /* Move the print name into pure space. */

	  Temp = Vector_Ref(Temp, SYMBOL_NAME);

#ifdef TC_CLSAV

	  if ((OBJECT_TYPE(Temp) == TC_INTERNED_SYMBOL) ||
	      (OBJECT_TYPE(Temp) == TC_UNINTERNED_SYMBOL))
	  {
	    Temp = Vector_Ref(Temp, SYMBOL_NAME);
	  }

	  if (OBJECT_TYPE(Temp) == TC_CLSAV)
	  {
	    Temp = User_Vector_Ref(Temp, CLSAV_NAME);
	  }
	    
#endif TC_CLSAV

	  if (OBJECT_TYPE(Temp) != TC_CHARACTER_STRING)
	  {
	    printf("Warning: symbol print name did not resolve to a string.\n");
	    printf("Orig_Temp: 0x%x, Temp: 0x%x\n", Orig_Temp, Temp);
	    break;
	  }
	  else
	  {
	    Purify_Pointer(Setup_Internal(false,
					  Transport_Vector_Indirect(),
					  Indirect_BH(false)));
	    break;
	  }
	}

	/* Fall through */

      case_Fasdump_Pair:
      purify_pair:
	Setup_Pointer_for_Purify(Transport_Pair());
	break;

      case TC_WEAK_CONS:
	Setup_Pointer_for_Purify(Transport_Weak_Cons());
	break;

      case TC_VARIABLE:
      case_Triple:
	Setup_Pointer_for_Purify(Transport_Triple());
	break;

/* PurifyLoop continues on the next page */

/* PurifyLoop, continued */

      case_Quadruple:
	if (GC_Mode != PURE_COPY)
	{
	  Setup_Pointer_for_Purify(Transport_Quadruple());
	}
	break;

      case TC_FUTURE:
	Setup_Pointer_for_Purify(Transport_Future());
	break;

      case TC_ENVIRONMENT:
      case TC_COMPILED_CODE_BLOCK:
	if (GC_Mode == PURE_COPY)
	{
	  /* This should actually do an indirect pair transport of
	     the procedure, at least.
	   */
	  break;
	}

	/* Fall through */

      case_Purify_Vector:
      purify_vector:
	Setup_Pointer_for_Purify(Transport_Vector());
	break;

      case TC_BIG_FLONUM:
        Setup_Pointer_for_Purify({
	  Transport_Flonum();
	  break;
	});

      default:
	sprintf(gc_death_message_buffer,
		"purifyloop: bad type code (0x%02x)",
		OBJECT_TYPE(Temp));
	gc_death(TERM_INVALID_TYPE_CODE, gc_death_message_buffer,
		 Scan, To);
	/*NOTREACHED*/
      } /* Switch_by_GC_Type */
  } /* For loop */

  *To_Pointer = To;
  return (To);

} /* PurifyLoop */

/* purify_area manages the purification process.

   It invokes PurifyLoop on various areas and with varying values
   of GC_mode.

   area_start and area_end delimit the area to be purified.
   area_start is the first (lowest addressed) word in the area,
   area_end is the first word above (not in) the area.

   It is assumed that the word at area_end can be overwritten,
   and it's contents are garbage.

   params is the number of arguments which the primitive invoking
   purify area received.  It is needed because purify_area ends by
   performing a garbage collection, and in order to invoke the GC daemons,
   the primitive's frame must be popped.

   really_purify signals whether the objects pointed at by the area
   should be moved to constant space or pure space.

   code_space signals whether we are purifying a section of code space,
   which must be marked and propagated.
 */

extern void purify_area();

void
purify_area(name, area_start, area_end, params, really_purify, code_space)
     char *name;
     Pointer *area_start, *area_end;
     int params;
     Boolean really_purify, code_space;
{
  long Saved_Zone;

  Save_Time_Zone(Zone_Purify);
  if ((Free_Constant + 6) > Constant_Top)
  {
    fprintf(stderr,
	    "%s: Not enough room to purify object - %x\n",
	    name, Free_Constant);
  }
  else
  { extern Pointer Weak_Chain;
    Pointer *Original_Pure, *Original_Constant, *Result;
    long Pure_Length, Total_Length, Code_Length;
    int purify_kind;

    Weak_Chain = NIL;
    Original_Pure = Free_Constant;
    Free_Constant += 2;		/* Leave room for the header. */

    /* Signal end of Scan. */

    *area_end = Make_Pointer(TC_BROKEN_HEART, area_end);

    if (really_purify)
    {
      Result = PurifyLoop(area_start, &Free_Constant, PURE_COPY);
      if (Result != area_end)
      {
	fprintf(stderr, "%s: Pure Copy 1 ended too early.\n", name);
	Microcode_Termination(TERM_BROKEN_HEART);
      }

      Result = PurifyLoop((Original_Pure + 2), &Free_Constant, PURE_COPY);
      if (Result != Free_Constant)
      {
	fprintf(stderr, "%s: Pure Copy 2 ended too early.\n", name);
	Microcode_Termination(TERM_BROKEN_HEART);
      }
      Pure_Length = ((Free_Constant - Original_Pure) + 1);
    }
    else
    {
      Pure_Length = 3;
    }

    if (Debug_Flags[3])
    {
      printf("Finished first round of purify, FC=%x\n", Free_Constant);
      fflush(stdout);
    }

    if ((Free_Constant + 2) > Constant_Top)
    {
      fprintf(stderr,
	      "%s: Not enough room to purify object - %x\n",
	      name, Free_Constant);
      Microcode_Termination(TERM_NO_SPACE);
    }

    *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *Free_Constant++ = Make_Non_Pointer(CONSTANT_PART, Pure_Length);

    purify_kind = (really_purify ? CONSTANT_COPY : NORMAL_GC);

    Original_Constant = Free_Constant;
    Result = PurifyLoop(area_start, &Free_Constant, purify_kind);
    if (Result != area_end)
    {
      fprintf(stderr, "%s: Constant Copy 1 ended too early.\n", name);
      Microcode_Termination(TERM_BROKEN_HEART);
    }

    Result = PurifyLoop((Original_Pure + 2), &Free_Constant, purify_kind);
    if (Result != Free_Constant)
    {
      fprintf(stderr, "%s: Constant Copy 2 ended too early.\n", name);
      Microcode_Termination(TERM_BROKEN_HEART);
    }

    if ((Free_Constant + 2) > Constant_Top)
    {
      fprintf(stderr,
	      "%s: Not enough room to purify object - %x\n",
	     name, Free_Constant);
      Microcode_Termination(TERM_NO_SPACE);
    }

    Total_Length = ((Free_Constant - Original_Pure) + 1);
    *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *Free_Constant++ = Make_Non_Pointer(END_OF_BLOCK, Total_Length);

    *Original_Pure++ =
      Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, Pure_Length);
    *Original_Pure = Make_Non_Pointer(PURE_PART, Total_Length);

    if (code_space)
    {
      extern void end_code_space_purification();

      end_code_space_purification(area_start, area_end);
    }
  }

  Restore_Time_Zone();			
  if (N_Interps == 1)
  {
    /* This is a kludge needed to boot the system.
       It's here to take care of the first purification (of bf-gc.bin)
       which is performed with the "sequential" memory setup.
     */
    SHARED_DATA->Flip_Count = 1;
    SHARED_DATA->Slave_Done_Count = 1;
  }
  Finish_GC(params);
  /*NOTREACHED*/
}

/* (PRIMITIVE_PURIFY OBJECT PURE?)

   To purify an object we just copy it into Pure Space in two
   parts with the appropriate headers and footers.  The actual
   copying is done by PurifyLoop above.  If we run out of room
   SCHEME crashes.

   Once the copy is complete we run a full GC which handles the
   broken hearts which now point into pure space.  Since this
   primitive uses the master-gc-loop it should only be used
   as one would use master-gc-loop i.e. with everyone else halted.

   It does not return directly since there may be GC-DEMONS to run,
   etc.  Purification works by side effect.
*/

DEFINE_PRIMITIVE("PRIMITIVE-PURIFY", Prim_Primitive_Purify, 2)
{
  Boolean Purify_Object;
  Primitive_2_Args();

  if (Arg2 == TRUTH)
    Purify_Object = true;
  else if (Arg2 == NIL)
    Purify_Object = false;
  else
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  if (Debug_Flags[3])
  {
    printf("Purifying %x, Free_Constant is %x\n",
	   Arg1, Free_Constant);
    fflush(stdout);
  }

  Free[0] = Arg1;
  purify_area("Purify", &Free[0], &Free[1], 2, Purify_Object, false);
  /*NOTREACHED*/
}

/*
	The following are useful Pure and Constant Space
	primitives and subroutines.

	We assume that TC_MANIFEST_SPECIAL_NM_VECTOR only appears
	in pure/constant space to indicate the start of a pure region.
	We have to step backwards over the traps and whatnot moved in
	here by LOOKUP.C by checking type codes.
*/

Boolean
Pure_Test(Obj_Address)
     fast Pointer *Obj_Address;
{
  fast Pointer *Where;

  if ((Obj_Address >= Constant_Space) &&
      (Obj_Address < Free_Constant))
  {
    Where = Free_Constant - 1;
    while (Where >= Constant_Space)
    {
      Where -= 1 + Get_Integer(*Where);
      if (Where <= Obj_Address)
	return
	  ((Boolean) (Obj_Address <= (Where + 1 + Get_Integer(*(Where + 1)))));
    }
  }
  else if ((Obj_Address >= ((Pointer *) SHARED_DATA->Code_Base)) &&
	   (Obj_Address < ((Pointer *) SHARED_DATA->Code_Free)))
	   
  {
    Pointer *first_impure;

    first_impure = ((Pointer *) SHARED_DATA->Code_Impure);
    while (OBJECT_TYPE(*first_impure) == TC_MANIFEST_SPECIAL_NM_VECTOR)
      first_impure += (1 + Get_Integer(*first_impure));
    SHARED_DATA->Code_Impure = ((long *) first_impure);

    return (Obj_Address < first_impure);
  }
  return ((Boolean) false);
}

/* (PURE_P OBJECT)
   Returns #!TRUE if the object is pure (ie it doesn't point to any
   other object, or it is in a pure section of the constant space).
*/

DEFINE_PRIMITIVE("PURE?", Prim_Pure_P, 1)
{
  Pointer Object, *Obj_Address;
  Primitive_1_Arg();

  if (GC_Type(Arg1) == GC_Non_Pointer)
    PRIMITIVE_RETURN(TRUTH);

  Obj_Address = Get_Pointer(Arg1);
  if (Pure_Test(Obj_Address))
    PRIMITIVE_RETURN(TRUTH);
  else
    PRIMITIVE_RETURN(NIL);
}

extern Boolean
  constant_test(),
  unsafe_to_link_test();

Boolean
constant_test(addr)
     Pointer *addr;
{
  return (((addr >= Constant_Space) &&
	   (addr < Free_Constant)) ||
	  ((addr >= ((Pointer *) SHARED_DATA->Code_Base)) &&
	   (addr < ((Pointer *) SHARED_DATA->Code_Free))));
}

Boolean
unsafe_to_link_test(addr)
     Pointer *addr;
{
  if (constant_test(addr))
    return (Pure_Test(addr));
  else
    return (false);
}

/* (CONSTANT_P OBJECT)
   Returns #!TRUE if the object is in constant space or isn't a
   pointer.
*/

DEFINE_PRIMITIVE("CONSTANT?", Prim_Constant_P, 1)
{
  Primitive_1_Arg();

  PRIMITIVE_RETURN (((GC_Type(Arg1) == GC_Non_Pointer) ||
		     constant_test(Get_Pointer(Arg1))) ?
		    TRUTH : NIL);
}

Pointer
Purify_Pass_2(Ignored)
     Pointer Ignored;
{
  fprintf(stderr, "Purify_Pass_2 invoked on a butterfly!\n");
  Microcode_Termination(TERM_HALT);
  /*NOTREACHED*/
}

/* (GET_NEXT_CONSTANT)
   Returns the next free address in constant space.
*/

DEFINE_PRIMITIVE("GET-NEXT-CONSTANT", Prim_Get_Next_Constant, 0)
{
  Pointer *Next_Address;
  Primitive_0_Args();

  Next_Address = (Free_Constant + 1);
  PRIMITIVE_RETURN(Make_Pointer(TC_ADDRESS, Next_Address));
}

/* copy_to_constant_space is a microcode utility procedure.
   It moves a specified block into code space (not constant space).
   The microcode kills itself if there is not enough code
   space left.
 */

extern Pointer *copy_to_constant_space();

Pointer *
copy_to_code_space(source, nobjects)
     fast Pointer *source;
     long nobjects;
{
  fast Pointer *dest;
  fast long i;
  Pointer *result;

  dest = (Pointer *) Allocate_Code(nobjects + 1);
  *dest++ = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, nobjects);
  result = dest;
  for (i = nobjects; --i >= 0; )
  {
    *dest++ = *source++;
  }
  return result;
}

Pointer *
copy_to_constant_space(source, nobjects)
     fast Pointer *source;
     long nobjects;
{
  fast Pointer *dest;
  fast long i;
  Pointer *result;

  dest = Free_Constant;
  if (!Test_Pure_Space_Top(dest+nobjects+6))
  {
    fprintf(stderr,
	    "copy_to_constant_space: Not enough constant space!\n");
    Microcode_Termination(TERM_NO_SPACE);
  }
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 3);
  *dest++ = Make_Non_Pointer(PURE_PART, (nobjects + 5));
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = Make_Non_Pointer(CONSTANT_PART, 3);
  result = dest;
  for (i = nobjects; --i >= 0; )
  {
    *dest++ = *source++;
  }
  *dest++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = Make_Non_Pointer(END_OF_BLOCK, (nobjects + 5));
  Free_Constant = dest;

  return (result);
}

/* Some functions which are referenced by the interpreter shared
 * code but which the butterfly does not support.
 */

DEFINE_PRIMITIVE("GARBAGE-COLLECT", Prim_Garbage_Collect, 1)
{
  PRIMITIVE_HEADER(1);

  fprintf(stderr,
	  "Butterflies don't use a standard GC primitive.\n");
  PRIMITIVE_RETURN(NIL);
}

DEFINE_PRIMITIVE("PRIMITIVE-IMPURIFY", Prim_Impurify, 1)
{
  PRIMITIVE_HEADER(1);

  fprintf(stderr, "Butterflies don't support IMPURIFY.\n");
  PRIMITIVE_RETURN(NIL);
}
