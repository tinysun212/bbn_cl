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

/* $Header: random.c,v 10.0 88/12/07 13:10:05 las Exp $
 * $MIT-Header: random.c,v 9.38 87/12/04 22:19:13 GMT jinx Exp $
 *
 * Totally random primitives.
 * They are fossils of days gone by and should go away.
 *
 */

#include "scheme.h"
#include "primitive.h"

/* Playing with non marked vectors. */
 
/* (NON-MARKED-VECTOR-CONS LENGTH)
   Creates a non-marked vector of the specified LENGTH.  The
   contents of such a vector are not seen by the garbage collector.
   There are no ordinary operations which can be performed on
   non-marked vectors, but the SYS_VECTOR operations can be used
   with care.
*/
DEFINE_PRIMITIVE("NON-MARKED-VECTOR-CONS", Prim_Non_Marked_Vector_Cons, 1)
{
  long Length;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  Length = Get_Integer(Arg1);
  Primitive_GC_If_Needed(Length + 1);
  *Free = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Length);
  Free += (Length + 1);
  PRIMITIVE_RETURN(Make_Pointer(TC_NON_MARKED_VECTOR,
				(Free - (Length + 1))));
}

/* (INSERT-NON-MARKED-VECTOR TO-GC-VECTOR N FROM-GC-VECTOR)
   This primitive performs a side-effect on the TO-GC-VECTOR.  Both
   TO- and FROM-GC-VECTOR must be of the garbage collector type
   vector (i.e. vectors, strings, non-marked vectors, bignums,
   etc.).  The FROM-GC-VECTOR is inserted in the middle of
   TO-GC-VECTOR, preceded by a non-marked vector header.  The
   insertion begins at the Nth position of the vector with the
   non-marked header.  Notice that this is really an "overwrite"
   rather than an insertion, since the length of the TO-GC-VECTOR
   does not change and the data which was formerly in the part of
   the vector now occupied by FROM-GC-VECTOR and its header has
   been lost.  This primitive was added for the use of certain
   parts of the compiler and runtime system which need to make
   objects that have an internal part which is "hidden" from the
   garbage collector. The value returned is TO-GC-VECTOR.
*/
DEFINE_PRIMITIVE("INSERT-NON-MARKED-VECTOR!", Prim_Insert_Non_Marked_Vector, 3)
{
  Pointer *To, *From;
  long Index, NM_Length, Length, i;
  Primitive_3_Args();

  Arg_1_GC_Type(GC_Vector);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_GC_Type(GC_Vector);
  Length = Vector_Length(Arg1);
  NM_Length = Vector_Length(Arg3);
  Range_Check(Index, Arg2, 0, (Length - 1), ERR_ARG_2_BAD_RANGE);
  if ((Length - Index) <= NM_Length)
  {
    Primitive_Error(ERR_ARG_3_BAD_RANGE);
  }
  From = Nth_Vector_Loc(Arg3, VECTOR_TYPE);
  To = Nth_Vector_Loc(Arg1, VECTOR_DATA + Index);
  for (i = 0; i <= NM_Length; i++)
  {
    *To++ = *From++;
  }
  PRIMITIVE_RETURN(Arg1);
}
