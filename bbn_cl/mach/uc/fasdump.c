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

/*
 * This file contains code for fasdump and dump-band.
 */

#include "scheme.h"
#include "primitive.h"
#define In_Fasdump
#include "gccode.h"
#include "trap.h"
#include "lookup.h"
#include "fasl.h"
#include "cl-symbol.h"
#include "dump.c"

extern int Debug_Flags[];

extern Pointer
  dump_renumber_primitive(),
  *initialize_primitive_table(),
  *cons_primitive_table(),
  *cons_whole_primitive_table();

/* Some statics used freely in this file */

static Pointer *NewFree, *NewMemTop, *Fixup, *Orig_New_Free;
static Boolean compiled_code_present_p;

/* FASDUMP:

   Hair squared! ... in order to dump an object it must be traced (as
   in a garbage collection), but with some significant differences.
   First, the copy must have the global value cell of symbols set to
   UNBOUND and variables uncompiled.  Second, and worse, all the
   broken hearts created during the process must be restored to their
   original values.  This last is done by growing the copy of the
   object in the bottom of spare heap, keeping track of the locations
   of broken hearts and original contents at the top of the spare
   heap.

   FASDUMP is called with three arguments:
   Argument 1: Object to dump.
   Argument 2: File name.
   Argument 3: Flag.
               where the flag is #!true for a dump into constant
               space at reload time, () for a dump into heap.

   Currently flag is ignored.	       
*/

#ifdef USE_SINGLE_HEAP

extern long Half_Heap_Size;

#define MAP_HIGH_TO_LOW(X) ((Pointer) (((Pointer *) X) - Half_Heap_Size))
#define MAP_LOW_TO_HIGH(X) ((Pointer) (((Pointer *) X) + Half_Heap_Size))

#undef Fasdump_Setup_Pointer

#define Fasdump_Setup_Pointer(Extra_Code, BH_Code)			\
BH_Code;								\
									\
/* It must be transported to New Space */				\
									\
New_Address = (Make_Broken_Heart(MAP_HIGH_TO_LOW(C_To_Scheme(To))));    \
if ((Fixes - To) < FASDUMP_FIX_BUFFER)					\
{									\
  NewFree = To;								\
  Fixup = Fixes;							\
  return (PRIM_INTERRUPT);						\
}									\
*--Fixes = *Old;							\
*--Fixes = C_To_Scheme(Old);						\
Extra_Code;								\
continue

#else

#define MAP_HIGH_TO_LOW(X) (X)
#define MAP_LOW_TO_HIGH(X) (X)

#endif

/* 
   Copy of GCLoop, except (a) copies out of constant space into the
   object to be dumped; (b) changes symbols and variables as
   described; (c) keeps track of broken hearts and their original
   contents (e) To_Pointer is now NewFree.
*/

#define Setup_Pointer_for_Dump(Extra_Code)			\
Dump_Pointer(Fasdump_Setup_Pointer(Extra_Code, Normal_BH(false, continue)))

#define Dump_Pointer(Code)					\
Old = Get_Pointer(Temp);					\
Code

#define Dump_Compiled_Entry()						\
{									\
  Dump_Pointer(Fasdump_Setup_Pointer(Transport_Compiled(),		\
				     Compiled_BH(false, continue)));	\
}

/* Dump_Mode is currently a fossil.  It should be resurrected. */

/* Should be big enough for the largest fixed size object (a Quad) 
   and 2 for the Fixup.
 */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

#define FASDUMP_FIX_BUFFER 10

long
DumpLoop(Scan, Dump_Mode)
     fast Pointer *Scan;
     int Dump_Mode;
{
  fast Pointer *To, *Old, Temp, New_Address, *Fixes;

  To = NewFree;
  Fixes = Fixup;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;

    if (OBJECT_TYPE(Temp) == TC_CLSAV)
    { if (OBJECT_TYPE(User_Vector_Ref(Temp, CLSAV_FUNCTION_SYMBOL)) ==
	  TC_INTERNED_SYMBOL)
      {
	printf("Error: Function symbol of wrong type: %s\n",
	       Scheme_String_To_C_String
	        (cl_get_symbol_name(User_Vector_Ref(Temp,
						    CLSAV_FUNCTION_SYMBOL))));
	Primitive_Error(ERR_EXTERNAL_RETURN);
      }
      Setup_Pointer_for_Dump(Transport_Clsav());
    }
    else
      Switch_by_GC_Type(Temp)
    {
      case TC_PRIMITIVE:
      case TC_PCOMB0:
        *Scan = dump_renumber_primitive(*Scan);
	break;

      case TC_BROKEN_HEART:
        if (OBJECT_DATUM(Temp) != 0)
	{
	  sprintf(gc_death_message_buffer,
		  "dumploop: broken heart (0x%lx) in scan",
		  Temp);
	  gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += Get_Integer(Temp);
	break;

      case TC_STACK_ENVIRONMENT:
      case_Fasload_Non_Pointer:
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	compiled_code_present_p = true;
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
	    Setup_Pointer_for_Dump(Transport_Quadruple());
	  }
	  Scan -= 1;
	  break;
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
	    Dump_Compiled_Entry();
	  }
	  Scan = end_scan;
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	machine_word *start_ptr;
	fast machine_word *word_ptr;

	Scan += 1;
	word_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Scan);
	start_ptr = word_ptr;

	while (VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  Scan = MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr);
	  word_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	  Temp = *Scan;
	  Dump_Compiled_Entry();
	}
	Scan = MANIFEST_CLOSURE_END(word_ptr, start_ptr);
	break;
      }

      case_compiled_entry_point:
	compiled_code_present_p = true;
	Dump_Compiled_Entry();
	break;

      case_Cell:
	Setup_Pointer_for_Dump(Transport_Cell());
	break;

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall through. */

      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	Setup_Pointer_for_Dump(Transport_Pair());
	break;

      case TC_INTERNED_SYMBOL:
	Setup_Pointer_for_Dump(Fasdump_Symbol(Make_Broken_Heart(0)));
	break;

      case TC_UNINTERNED_SYMBOL:
	Setup_Pointer_for_Dump(Fasdump_Symbol(UNBOUND_OBJECT));
	break;

      case_Triple:
	Setup_Pointer_for_Dump(Transport_Triple());
	break;

      case TC_VARIABLE:
	Setup_Pointer_for_Dump(Fasdump_Variable());
	break;

/* DumpLoop continues on the next page */

/* DumpLoop, continued */

      case_Quadruple:
	Setup_Pointer_for_Dump(Transport_Quadruple());
	break;

      case TC_BIG_FLONUM:
	Setup_Pointer_for_Dump({
	  Transport_Flonum();
	  break;
	});

      case TC_COMPILED_CODE_BLOCK:
      case_Purify_Vector:
	Setup_Pointer_for_Dump(Transport_Vector());
	break;

      case TC_ENVIRONMENT:
	/* Make fasdump fail */
	return (ERR_FASDUMP_ENVIRONMENT);

      case TC_FUTURE:
	Setup_Pointer_for_Dump(Transport_Future());
	break;

      default:
	sprintf(gc_death_message_buffer,
		"dumploop: bad type code (0x%02x)",
		OBJECT_TYPE(Temp));
	gc_death(TERM_INVALID_TYPE_CODE, gc_death_message_buffer,
		 Scan, To);
	/*NOTREACHED*/
      }
  }
  NewFree = To;
  Fixup = Fixes;
  return (PRIM_DONE);
}

#define DUMPLOOP(obj, code)						\
{									\
  long value;								\
									\
  value = DumpLoop(obj, code);						\
  if (value != PRIM_DONE)						\
  {									\
    PRIMITIVE_RETURN(Fasdump_Exit(value));				\
  }									\
}

#define FASDUMP_INTERRUPT()						\
{									\
  PRIMITIVE_RETURN(Fasdump_Exit(PRIM_INTERRUPT));			\
}

Pointer
Fasdump_Exit(code)
     long code;
{
  Boolean result = true;
  fast Pointer *Fixes;

  Fixes = Fixup;
  while (Fixes != NewMemTop)
  {
    fast Pointer *Fix_Address;

    Fix_Address = Get_Pointer(*Fixes++); /* Where it goes. */
    *Fix_Address = *Fixes++;             /* Put it there. */
  }
  Fixup = Fixes;
  Fasdump_Exit_Hook();
  if (!result)
  {
    Primitive_Error(ERR_IO_ERROR);
    /*NOTREACHED*/
  }
  if (code == PRIM_DONE)
  {
    return (TRUTH);
  }
  else if (code == PRIM_INTERRUPT)
  {
    return (NIL);
  }
  else
  {
    Primitive_Error(code);
    /*NOTREACHED*/
  }
}


Pointer fasdump_fd(Object, Fd, Flag, Current_Pkg)
Pointer Object;
long Fd;
Pointer Flag;
Pointer Current_Pkg;
{
  Pointer *New_Object,
          *Addr_Of_New_Object, Prim_Exts;
  Pointer *table_start, *table_end;
  long Pure_Length, Length, table_length;
  Boolean result;

  set_file_des(Fd);
  set_current_pkg(Current_Pkg);
  compiled_code_present_p = false;
#if false
  if ((Flag != NIL) && (Flag != TRUTH))
#else
  if (Flag != NIL)
#endif
    Primitive_Error(ERR_ARG_3_WRONG_TYPE);

  table_end = &Free[Space_Before_GC()];
  table_start = initialize_primitive_table(Free, table_end);
  if (table_start >= table_end)
  {
    Primitive_GC(table_start - Free);
  }

  Fasdump_Free_Calc(NewFree, NewMemTop, Orig_New_Free);

  Fixup = NewMemTop;
  New_Object = NewFree;
  *NewFree++ = Object;

  DUMPLOOP(New_Object, NORMAL_GC);

  Length = (NewFree - New_Object);
  table_start = NewFree;
  table_end = cons_primitive_table(NewFree, Fixup, &table_length);
  if (table_end >= Fixup)
    {
      FASDUMP_INTERRUPT();
    }
  result = Write_File(MAP_HIGH_TO_LOW(New_Object),
		      Length, New_Object,
		      MAP_HIGH_TO_LOW(New_Object),
		      0, Constant_Space,
		      0, 0x000000,
		      table_start, table_length,
		      ((long) (table_end - table_start)),
		      compiled_code_present_p, false);
  /* The and is short-circuit, so it must be done in this order. */
  return (Fasdump_Exit(result ? PRIM_DONE : PRIM_INTERRUPT));
}

/* (PRIMITIVE-FASDUMP object-to-dump file-name flag)
   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.
   The first argument is the object to be dumped.  The second is
   the filename and the third a flag.  The flag, if #!TRUE, means
   that the object is to be dumped for reloading into constant
   space.  This is currently disabled. If the flag is NIL, it means
   that it will be reloaded into the heap.  The primitive returns
   #!TRUE or NIL indicating whether it successfully dumped the
   object (it can fail on an object that is too large).

   The code for dumping pure is severely broken and conditionalized out.
*/

DEFINE_PRIMITIVE("PRIMITIVE-FASDUMP", Prim_Prim_Fasdump, 3)
{
  Pointer Object, File_Name, Flag;
  Pointer status;

  Primitive_3_Args();

  Object = Arg1;
  File_Name = Arg2;
  Flag = Arg3;
  if (Type_Code(File_Name) != TC_CHARACTER_STRING)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (!Open_Dump_File(File_Name, WRITE_FLAG))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  status = fasdump_fd(Object,get_file_des(),Flag,NIL);
  Close_Dump_File();
  PRIMITIVE_RETURN (status);
}

DEFINE_PRIMITIVE("PRIMITIVE-FASDUMP-FD", Prim_Prim_Fasdump_Fd, 4)
{
  Pointer Object, Fd, Flag, Current_Pkg;
  Pointer status;

  Primitive_4_Args();

  Object = Arg1;
  Fd = Arg2;
  Flag = Arg3;
  Current_Pkg = Arg4;
  if (Type_Code(Fd) != TC_FIXNUM)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  status = fasdump_fd(Object,Get_Integer(Fd),Flag,Current_Pkg);
  PRIMITIVE_RETURN (status);
}


/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of NIL.
*/

DEFINE_PRIMITIVE("DUMP-BAND", Prim_Band_Dump, 2)
{
  Pointer Combination, *table_start, *table_end, *saved_free;
  long Arg1Type, table_length;
  Boolean result;
  Primitive_2_Args();

  Band_Dump_Permitted();
  Arg1Type = Type_Code(Arg1);
  if ((Arg1Type != TC_CONTROL_POINT) &&
      (Arg1Type != TC_EXTENDED_PROCEDURE) &&
      (Arg1Type != TC_PRIMITIVE))
  {
    Arg_1_Type(TC_PROCEDURE);
  }
  Arg_2_Type(TC_CHARACTER_STRING);

#ifdef butterfly
  if (Local_Heap_Base != SHARED_DATA->Heap_Base)
  {
    /* Cause the image to be in the low heap, to increase
       the probability that no relocation is needed on reload.
     */
    Primitive_GC(0);
  }
  Start_Direct_IO();
#define Cleanup_For_Exit() Finish_Direct_IO();
#else
#define Cleanup_For_Exit()
#endif

  if (!Open_Dump_File(Arg2, WRITE_FLAG))
  {
    Cleanup_For_Exit();
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  }
  Primitive_GC_If_Needed(5);
  saved_free = Free;
  Combination = Make_Pointer(TC_COMBINATION_1, Free);
  Free[COMB_1_FN] = Arg1;
  Free[COMB_1_ARG_1] = NIL;
  Free += 2;
  *Free++ = Combination;
  *Free++ = compiler_utilities;
  *Free = Make_Pointer(TC_LIST, (Free - 2));
  Free++;  /* C doesn't say which side of = gets done first */
  table_start = Free;
  table_end = cons_whole_primitive_table(Free, Heap_Top, &table_length);
  if (Debug_Flags[13])
    printf("table_end = %x, Heap_Top = %x, table_length = %x\n",
	   table_end, Heap_Top, table_length);
  if (table_end >= Heap_Top)
  {
    result = false;
  }
  else
  {
    result = Write_File((Free - 1),
			((long) (Free - Heap_Bottom)), Heap_Bottom,
			Heap_Bottom,
			((long) (Free_Constant - Constant_Space)),
			Constant_Space,
			Code_Space_Size(),
			Code_Space_Base(),
			table_start, table_length,
			((long) (table_end - table_start)),
			(compiler_utilities != NIL), true);
  }
  /* The and is short-circuit, so it must be done in this order. */
  result = (Close_Dump_File() && result);
  Band_Dump_Exit_Hook();
  Free = saved_free;
  Cleanup_For_Exit();
  PRIMITIVE_RETURN(result ? TRUTH : NIL);
}
