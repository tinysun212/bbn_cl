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

/* $Header: cmp68kgc.h,v 10.0 88/12/07 13:06:06 las Exp $
   $MIT-Header: cmp68kgc.h,v 9.28 88/03/21 21:15:07 GMT jinx Rel $

Utilities to relocate compiled code in garbage collection-like processes. 

This file is conditionally included by gccode.h.
*/

/* The following is a kludge which is used to get
   return_to_interpreter and uuo_link_trap to work.
   The return to interpreter block is never dumped on normal
   bin files, but is dumped in complete bands.
   As long as it does not change in position with respect to the
   beginning of constant space, it will be relocated correctly on
   reload.
 */

#ifndef In_Fasdump

#define Compiled_Code_Pre_Test(then_what)

#else

extern Pointer compiler_utilities;

#define Compiled_Code_Pre_Test(then_what)				\
if (Old == Get_Pointer(compiler_utilities))				\
  then_what;								\
else

#endif

/* The following code handles compiled entry points, where the addresses
   point to the "middle" of the code vector.  The word before the one
   pointed at contains an offset to the beginning of the block so it
   can be found and copied as a whole.  The broken heart for the whole
   block lives in its usual place (first word in the vector).
   If the offset is odd, then it actually points to another offset.
 */

#define Get_Compiled_Block_Step(var, address)				\
{									\
  var = ((Pointer *) (((char *) (address)) -				\
		      ((unsigned long)					\
		       (((unsigned short *) (address))[-1]))));		\
}

#define Get_Compiled_Block(var, address)				\
{									\
  Get_Compiled_Block_Step(var, address);				\
  while (((unsigned long) var) & 1)					\
  {									\
    var = ((Pointer *) (((char *) (var)) + 1));				\
    Get_Compiled_Block_Step(var, var);					\
  }									\
}

/* Pointers to char are used here because compiled entry points do not
   in general point to Pointer boundaries.
 */

#define RELOCATE_COMPILED_ADDRESS(object, new_block, old_block)		\
((Pointer *) (((char *) new_block) +					\
	      (((char *) Get_Pointer(object)) -				\
	       ((char *) old_block))))

#define Relocate_Compiled(object, new_block, old_block)			\
Make_Pointer(OBJECT_TYPE(object),					\
	     RELOCATE_COMPILED_ADDRESS(object, new_block, old_block))

#define Compiled_BH(In_GC, then_what)					\
{									\
  /* Has it already been relocated? */					\
									\
  Get_Compiled_Block(Old, Old);						\
  Compiled_Code_Pre_Test(then_what)					\
  if (OBJECT_TYPE(*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = Relocate_Compiled(Temp, Get_Pointer(*Old), Old);		\
    then_what;								\
  }									\
}

#define Transport_Compiled()						\
{									\
  Pointer *Saved_Old = Old;						\
									\
  Real_Transport_Vector();						\
  *Saved_Old = New_Address;						\
  *Scan = Relocate_Compiled(Temp,					\
			    Get_Pointer(New_Address),			\
			    Saved_Old);					\
}

/* Manifest and implied types */

/* Manifest closures */

typedef unsigned short machine_word;

#define FIRST_MANIFEST_CLOSURE_ENTRY(scan)		\
  ((machine_word *) scan)

#define VALID_MANIFEST_CLOSURE_ENTRY(word_ptr)		\
  ((*(word_ptr)) != 0)

#define MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr)	\
  ((Pointer *) &word_ptr[3])

#define NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr)		\
  (&(word_ptr)[5])

#define MANIFEST_CLOSURE_SIZE(end_ptr, start_ptr)	\
  ((end_ptr - start_ptr) /				\
   ((sizeof(Pointer)) / (sizeof(machine_word))))

/* This takes into account the fact that the relocation loop increments
   by 1 on each major iteration.
 */

#define MANIFEST_CLOSURE_END(end_ptr, start_ptr)			\
  (((Pointer *) start_ptr) + MANIFEST_CLOSURE_SIZE(end_ptr, start_ptr))

#define MANIFEST_CLOSURE_VALID_FITS_P(word_ptr, top)			\
  (word_ptr < ((machine_word *) top))

/* Linkage sections */

#define OPERATOR_LINKAGE_KIND			0x000000
#define REFERENCE_LINKAGE_KIND			0x010000
#define ASSIGNMENT_LINKAGE_KIND			0x020000

#define READ_LINKAGE_KIND(header)			\
  ((header) & 0xff0000)

#define READ_CACHE_LINKAGE_COUNT(header)		\
  ((header) & 0xffff)

#define READ_OPERATOR_LINKAGE_COUNT(header)		\
  (((header) & 0xffff) >> 1)
  
#define END_OPERATOR_LINKAGE_AREA(scan, count)		\
  (&scan[count + count])

#define FIRST_OPERATOR_LINKAGE_ENTRY(scan)		\
  (&(((machine_word *) scan)[2]))

#define OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr)	\
  ((Pointer *) (&(word_ptr)[1]))

#define NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr)		\
  (&(word_ptr)[4])
