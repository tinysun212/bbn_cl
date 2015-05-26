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

/* File: HEADER.C
 *
 * This file contains procedures for handling headers, which are
 * just like vectors, except they are of type TC_HEADER.  Provided
 * primitives are HEADER-CONS, HEADER-SET, HEADER-REF, HEADER-LENGTH.
 */

#include "scheme.h"
#include "primitive.h"

#define H_LENGTH_SLOT	0
#define H_SUBTYPE_SLOT	1
#define HEADER_LENGTH	2
#define DEFAULT_SUBTYPE	0

#define Header_Length(h)	(Fast_Vector_Ref(h, H_LENGTH_SLOT) - (HEADER_LENGTH - 1))

/* This is sorta weird, so I will try to explain the format of Headers.
   The same goes for G-vectors, too.  The first word contains the number of
   words allocated to the header.  That is, it's one plus the number of
   locations the user can reference.  You might wonder why the header isn't
   just two words long, and the length that gets stored is the length of
   user accessible words.  The reason is that G-vectors and Headers and
   I-vectors are all treated the same as (SCHEME) Vectors by the garbage
   collector.  And the garbage collector expects the first word of a vector
   to contain the total number of entries that need copying. */

Define_Primitive(Prim_Header_Cons, 1, "HEADER-ALLOC")
{
	long	Length,
		i;
	Pointer	*ReturnAddr = Free;
	Primitive_1_Args();

	Arg_1_Type(TC_FIXNUM);
	Length = Get_Integer(Arg1);
	Primitive_GC_If_Needed(Length+HEADER_LENGTH);
	Free[H_LENGTH_SLOT] = Make_Non_Pointer(TC_MANIFEST_VECTOR, Length + HEADER_LENGTH - 1);
	Free[H_SUBTYPE_SLOT] = Make_Non_Pointer(TC_FIXNUM, DEFAULT_SUBTYPE);
	bzero((char *) (Free + HEADER_LENGTH), Length * sizeof (Pointer));
	Free += (HEADER_LENGTH + Length);
	return Make_Pointer(TC_HEADER, ReturnAddr);
}

Define_Primitive(Prim_Header_Set, 3, "HEADER-SET!")
{
	long	Offset;

	Primitive_3_Args();
	Arg_1_Type(TC_HEADER);
	Arg_2_Type(TC_FIXNUM);
	Range_Check(Offset, Arg2,
              0, Header_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
	Side_Effect_Impurify(Arg1, Arg3);
	return Swap_Pointers(Nth_Vector_Loc(Arg1, Offset+HEADER_LENGTH), Arg3);
}

Define_Primitive(Prim_Header_Ref, 2, "HEADER-REF")
{
	long	Offset;

	Primitive_2_Args();
	Arg_1_Type(TC_HEADER);
	Arg_2_Type(TC_FIXNUM);
	Range_Check(Offset, Arg2,
              0, Header_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
	return Vector_Ref(Arg1, Offset + HEADER_LENGTH);
}

Define_Primitive(Prim_Header_Length, 1, "HEADER-LENGTH")
{
	Primitive_1_Arg();
	Arg_1_Type(TC_HEADER);
	return Make_Unsigned_Fixnum(Header_Length(Arg1));
}

Define_Primitive(Prim_Set_Header_Subtype, 2, "SET-HEADER-SUBTYPE!")
{
	Primitive_2_Args();

	Arg_1_Type(TC_HEADER);
	Arg_2_Type(TC_FIXNUM);
	return Fast_Vector_Set(Arg1, H_SUBTYPE_SLOT, Arg2);
}

Define_Primitive(Prim_Get_Header_Subtype, 1, "GET-HEADER-SUBTYPE")
{
	Primitive_1_Arg();
	Arg_1_Type(TC_HEADER);
	return Fast_Vector_Ref(Arg1, H_SUBTYPE_SLOT);
}
