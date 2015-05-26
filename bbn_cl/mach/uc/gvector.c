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

/****************************************************************
*                                                               *
*                         Copyright (c) 1984                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: GVECTOR.C
 *
 * This file contains procedures for handling vectors and conversion
 * back and forth to lists.
 */

#include "scheme.h"
#include "primitive.h"


#define GV_LENGTH_SLOT	0
#define GV_SUBTYPE_SLOT	1
#define HEADER_LENGTH	2
#define DEFAULT_SUBTYPE	0	/* 0 is normal vector */
#define STRUCTURE_SUBTYPE 1

#define G_Vector_Length(gv)	(Fast_Vector_Ref(gv, GV_LENGTH_SLOT) - (HEADER_LENGTH - 1))

/* This is sorta weird, so I will try to explain the format of G-vectors.
   The same goes for HEADERS, too.  The first word contains the number of
   words allocated to the vector.  That is, it's one plus the number of
   locations the user can reference.  You might wonder why the header isn't
   just two words long, and the length that gets stored is the length of
   user accessible words.  The reason is that G-vectors and Headers, and
   I-vectors are all treated the same as (SCHEME) Vectors by the garbage
   collector.  And the garbage collector expects the first word of a vector
   to contain the total number of entries that need copying, etc. */

Define_Primitive(Prim_G_Vector_Alloc, 1, "G-VECTOR-ALLOC")
{ 
	long	Length, 
		i;
	Pointer	*ReturnAddr = Free;
	Primitive_1_Args();

	Arg_1_Type(TC_FIXNUM);
	Length = Get_Integer(Arg1);
	Primitive_GC_If_Needed(Length+HEADER_LENGTH);
	Free[GV_LENGTH_SLOT] = Make_Non_Pointer(TC_MANIFEST_VECTOR, Length + HEADER_LENGTH - 1);
	Free[GV_SUBTYPE_SLOT] = Make_Non_Pointer(TC_FIXNUM, DEFAULT_SUBTYPE);
	bzero((char *) (Free + HEADER_LENGTH), Length * sizeof (Pointer));	
	Free += Length + HEADER_LENGTH;
	return Make_Pointer(TC_G_VECTOR, ReturnAddr);
}

Define_Primitive(Prim_G_Vector_Ref, 2, "G-VECTOR-REF")
{ 
	long Offset;
	Primitive_2_Args();
	Arg_1_Type(TC_G_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	Range_Check(Offset, Arg2,
		0, G_Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
	return Fast_Vector_Ref(Arg1, Offset+HEADER_LENGTH);
}

Define_Primitive(Prim_G_Vector_Set, 3, "G-VECTOR-SET!")
{ 
	long Offset;
	Primitive_3_Args();

	Arg_1_Type(TC_G_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	Range_Check(Offset, Arg2,
		0, G_Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
	Fast_Vector_Set(Arg1, Offset + HEADER_LENGTH, Arg3);
	return Arg3;
}

Define_Primitive(Prim_G_Vector_Length, 1, "G-VECTOR-LENGTH")
{ 
	Primitive_1_Arg();
	Arg_1_Type(TC_G_VECTOR);
	return Make_Unsigned_Fixnum(G_Vector_Length(Arg1));
}

Define_Primitive(Prim_Get_G_Vector_Subtype, 1, "GET-G-VECTOR-SUBTYPE")
{
	Primitive_1_Arg();

	Arg_1_Type(TC_G_VECTOR);
	return Fast_Vector_Ref(Arg1, GV_SUBTYPE_SLOT);
}

Define_Primitive(Prim_Set_G_Vector_Subtype, 2, "SET-G-VECTOR-SUBTYPE!")
{
	Primitive_2_Args();

	Arg_1_Type(TC_G_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	return Fast_Vector_Set(Arg1, GV_SUBTYPE_SLOT, Arg2);
}


Define_Primitive(Prim_Make_Structure_G_Vector, 2, "MAKE-STRUCTURE-G-VECTOR")
{ 
  long len;
  Pointer *res, data, *x;
	
  Primitive_2_Args();

  len = Get_Integer(Arg1);     /* Length of vector */
  data = Arg2;                 /* List */
  Primitive_GC_If_Needed(len+HEADER_LENGTH);
  Free[GV_LENGTH_SLOT] = Make_Non_Pointer(TC_MANIFEST_VECTOR, len + HEADER_LENGTH - 1);
  Free[GV_SUBTYPE_SLOT] = Make_Non_Pointer(TC_FIXNUM, STRUCTURE_SUBTYPE);
  res = Free;
  x = res + HEADER_LENGTH;
  while (data != NIL)
    {
      *x++ = Fast_Vector_Ref(data,CONS_CAR);
      data = Fast_Vector_Ref(data,CONS_CDR);
    }
  Free += len + HEADER_LENGTH;
  return Make_Pointer(TC_G_VECTOR, res);
}

Define_Primitive(prim_svref, 2, "SVREF")
{
  long i;
  Primitive_2_Args();

  Arg_1_Type(TC_G_VECTOR);
  if (Fast_Vector_Ref(Arg1, GV_SUBTYPE_SLOT) != 0)
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  Arg_2_Type(TC_FIXNUM);
  Range_Check(i, Arg2, 0, G_Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  return Fast_Vector_Ref(Arg1, i + HEADER_LENGTH);
}

Define_Primitive(prim_svset, 3, "%SVSET")
{
  long i;
  Primitive_3_Args();

  Arg_1_Type(TC_G_VECTOR);
  if (Fast_Vector_Ref(Arg1, GV_SUBTYPE_SLOT) != 0)
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  Arg_2_Type(TC_FIXNUM);
  Range_Check(i, Arg2, 0, G_Vector_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Fast_Vector_Set(Arg1, i + HEADER_LENGTH, Arg3);
  return Arg3;
}

