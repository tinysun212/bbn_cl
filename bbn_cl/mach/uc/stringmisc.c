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

/* File: STRINGMISC.C
 *
 * Support for strings and conversion to and from lists of characters.
 */

#include "scheme.h"
#include "primitive.h"

/* This is for Spice Lisp routines.  This could have been written in
   SCHEME but I decided to put it in the micro-code for speed.  Here's
   the comment from the spice lisp code.

   Compares the substrings specified by String1 and String2 and returns
   NIL if the strings are String=, or the lowest index of String1 in
   which the two differ. If one string is longer than the other and the
   shorter is a prefix of the longer, the length of the shorter + start1
   is returned. This would be done on the Vax with CMPC3. The arguments
   must be simple strings. */

Define_Primitive(Prim_SubString_Equal, 6, "SUBSTRING-EQUAL?")
{ 
	register char	*string1,
			*string2;
	register int	index1,
			index2;
	int	end1,
		end2,
		len1,
		len2;
	Primitive_6_Args();

	Arg_1_Type(TC_CHARACTER_STRING);
	Arg_2_Type(TC_FIXNUM);
	Arg_3_Type(TC_FIXNUM);
	Arg_4_Type(TC_CHARACTER_STRING);
	Arg_5_Type(TC_FIXNUM);
	Arg_6_Type(TC_FIXNUM);

	len1 = Get_Integer(Fast_Vector_Ref(Arg1, STRING_LENGTH));
	len2 = Get_Integer(Fast_Vector_Ref(Arg4, STRING_LENGTH));
	index1 = Get_Integer(Arg2);
	index2 = Get_Integer(Arg5);
	end1 = Get_Integer(Arg3);
	end2 = Get_Integer(Arg6);
	if (index1 < 0)
		Primitive_Error(ERR_ARG_2_BAD_RANGE);
	if (end1 > len1 || end1 < index1)
		Primitive_Error(ERR_ARG_3_BAD_RANGE);
	if (index2 < 0)
		Primitive_Error(ERR_ARG_5_BAD_RANGE);
	if (end2 > len2 || end2 < index2)
		Primitive_Error(ERR_ARG_6_BAD_RANGE);
	string1 = (char *) Nth_Vector_Loc(Arg1, STRING_CHARS);
	string2 = (char *) Nth_Vector_Loc(Arg4, STRING_CHARS);

	/* len1 and len2 now become the number of bytes to compare in
	   each string */
	len1 = (end1 - index1);
	len2 = (end2 - index2);
	if (len1 == len2) {
		while (index1 < end1) {
			if (string1[index1] != string2[index2])
				return Make_Unsigned_Fixnum(index1);
			index1++;
			index2++;
		}
		return NIL;
	} else if (len1 > len2) {
		while (index2 < end2) {
			if (string1[index1] != string2[index2])
				break;
			index1++;
			index2++;
		}
		return Make_Unsigned_Fixnum(index1);
	} else {
		while (index1 < end1) {
			if (string1[index1] != string2[index2])
				break;
			index1++;
			index2++;
		}
		return Make_Unsigned_Fixnum(index1);
	}
}

/*
  Used in cl io system. All type-checking is done there.
*/

Define_Primitive(prim_cl_overwrite_string, 5, "CL-OVERWRITE-STRING")
{
  char *src, *dst;
  long pos, start, end, i;
  Primitive_5_Args();

  dst = Scheme_String_To_C_String(Arg1);
  src = Scheme_String_To_C_String(Arg3);
  pos = Get_Integer(Arg2);
  start = Get_Integer(Arg4);
  end = Get_Integer(Arg5);
  dst += pos;
  src += start;
  for (i = 0; i < (end - start); i++) *dst++ = *src++;
  return Arg1;
}

