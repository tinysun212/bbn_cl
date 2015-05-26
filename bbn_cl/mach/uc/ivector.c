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

/* File: IVECTOR.C
 *
 * This file contains procedures for handling ivectors.
 */

#include "scheme.h"
#include "primitive.h"

/* i-vectors are stored in much the same way as in cmu spice lisp.  The
   header is three words long.  The first word is exactly the same as
   scheme vectors.  It contains the length in 32-bit words allocated to
   the array (it includes the two other header bytes).  The second word
   contains the array subtype, which is normally 0, which means this is
   a normal integer array.  The third word contains the access type and
   the number of entries.  The access type can be from 0 to 4; this says
   how many bits are in each entry.
	0	1-bit
	1	2-bit
	2	4-bit
	3	8-bit
	4	16-bit
   This should not be called with code == 0 since we want to allocate
   a real bit-vector in that case.  I don't know of a way to call a
   scheme micro-code primitive from another primitive. */

#define HEADER_LENGTH		3
#define IV_LENGTH_SLOT		0	/* don't change this! */
#define IV_SUBTYPE_SLOT		1
#define IV_ACCESS_CODE_SLOT	2
#define DEFAULT_SUBTYPE		0

#define Get_Access_Code(iv)	Type_Code(Fast_Vector_Ref(iv, IV_ACCESS_CODE_SLOT))
#define I_Vector_Length(iv)	Get_Integer(Fast_Vector_Ref(iv, IV_ACCESS_CODE_SLOT))

Define_Primitive(Prim_I_Vector_Alloc, 2, "I-VECTOR-ALLOC")
{ 
	long	code,
		words_needed,
		bpe,	/* bit per entry */
		nentries;

	Primitive_2_Args();
	Arg_1_Type(TC_FIXNUM);
	Arg_2_Type(TC_FIXNUM);
	nentries = Get_Integer(Arg1);
	code = Get_Integer(Arg2);
	if (code <= 0 || code > 4)
		Primitive_Error(ERR_ARG_2_BAD_RANGE);
	bpe = (1 << code);
	/* convert size in n-bit bytes to 32-bit words */
	words_needed = (((nentries * bpe) + POINTER_LENGTH - 1) / POINTER_LENGTH);

	Primitive_GC_If_Needed(words_needed+HEADER_LENGTH);
	Free[IV_LENGTH_SLOT] = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, words_needed + HEADER_LENGTH - 1);
	Free[IV_SUBTYPE_SLOT] = Make_Non_Pointer(TC_FIXNUM, DEFAULT_SUBTYPE);
	Free[IV_ACCESS_CODE_SLOT] = Make_Non_Pointer(code, nentries);
	Free += HEADER_LENGTH;
	bzero((char *) Free, words_needed * sizeof(Pointer));
	Free += words_needed;
	return Make_Pointer(TC_I_VECTOR, Free-(words_needed+HEADER_LENGTH));
}

Define_Primitive(Prim_I_Vector_Length, 1, "I-VECTOR-LENGTH")
{
	Primitive_1_Arg();
	Arg_1_Type(TC_I_VECTOR);
	return Make_Unsigned_Fixnum(I_Vector_Length(Arg1));
}

Define_Primitive(Prim_I_Vector_Ref, 2, "I-VECTOR-REF")
{
	long	offset,
		shift,
		bpe,	/* bits per entry */
		epw,	/* entries per word */
		access_code,
		word_no;

	Primitive_2_Args();
	Arg_1_Type(TC_I_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	Range_Check(offset, Arg2, 0, I_Vector_Length(Arg1) - 1,
		    ERR_ARG_2_BAD_RANGE);
	access_code = Type_Code(Fast_Vector_Ref(Arg1, IV_ACCESS_CODE_SLOT));
	bpe = (1 << access_code);
	epw = POINTER_LENGTH / bpe;
	word_no = offset / epw;
	shift = (offset % epw) * bpe;
	return Make_Unsigned_Fixnum(((Fast_Vector_Ref(Arg1, HEADER_LENGTH + word_no)
				      ) >> shift) &
				    ((1 << bpe) - 1));
}

Define_Primitive(Prim_I_Vector_Set, 3, "I-VECTOR-SET!")
{
	long	offset,
		shift,
		mask,
		bpe,	/* bits per entry */
		epw,	/* entries per word */
		newval,
		oldval,
		wordval,
		access_code,
		word_no;

	Primitive_3_Args();
	Arg_1_Type(TC_I_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	Arg_3_Type(TC_FIXNUM);
	Range_Check(offset, Arg2, 0, I_Vector_Length(Arg1) - 1,
		    ERR_ARG_2_BAD_RANGE);
	newval = Get_Integer(Arg3);
	access_code = Get_Access_Code(Arg1);
	bpe = (1 << access_code);
	if (newval < 0 || newval > ((1 << bpe) - 1))
		Primitive_Error(ERR_ARG_3_WRONG_TYPE);
	epw = POINTER_LENGTH / bpe;
	word_no = offset / epw;
	shift = (offset % epw) * bpe;
	wordval = (Fast_Vector_Ref(Arg1, HEADER_LENGTH + word_no));
	mask = ((1 << bpe) - 1);
	oldval = ((wordval >> shift) & mask);
	Fast_Vector_Set(Arg1, HEADER_LENGTH + word_no,
		((wordval & ~(mask << shift)) | (newval << shift)));
		
	return Make_Unsigned_Fixnum(oldval);
}

Define_Primitive(Prim_I_Vector_Access_Code, 1, "I-VECTOR-ACCESS-CODE")
{
	Primitive_1_Arg();
	Arg_1_Type(TC_I_VECTOR);
	return Make_Unsigned_Fixnum(Get_Access_Code(Arg1));
}

Define_Primitive(Prim_Get_I_Vector_Subtype, 1, "GET-I-VECTOR-SUBTYPE")
{
	Primitive_1_Arg();

	Arg_1_Type(TC_I_VECTOR);
	return Fast_Vector_Ref(Arg1, IV_SUBTYPE_SLOT);
}

Define_Primitive(Prim_Set_I_Vector_Subtype, 2, "SET-I-VECTOR-SUBTYPE!")
{
	Primitive_2_Args();

	Arg_1_Type(TC_I_VECTOR);
	Arg_2_Type(TC_FIXNUM);
	return Fast_Vector_Set(Arg1, IV_SUBTYPE_SLOT, Arg2);
}
