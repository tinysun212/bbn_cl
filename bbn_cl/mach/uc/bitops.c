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

/* $Header: bitops.c,v 10.0 88/12/07 13:04:24 las Exp $
 * $MIT-Header: bitops.c,v 9.24 87/08/14 15:36:15 GMT kwh Exp $
 *  
 * Additional bit string support
 */

/* This file extends C-SCHEME's bit string representation to represent
    infinite length bit strings; in this representation, virtual bits
    off the edge of the actual bit string are treated as zero or false.
    Logical operations can be performed on bit strings; one may take 
    bitwise AND or OR of two bit strings.   These procedures return new
    bit strings.
   All operations on bit strings are functional; they construct new bit 
    strings rather than modifying existing bit strings.  
*/

#include "scheme.h"
#include "primitive.h"
#include "bitstr.h"

#define size_in_words(bitstr)    ((Vector_Length(bitstr))-1)


Pointer
cons_bit_string(length)
     long length;
{fast Pointer string;
 string=allocate_bit_string(length);
 clear_bit_string(string);
 return string;}


/*(BIT-STRING-CHECK bitstring index)
   Treats <bitstring> as an infinite bitstring and
   extracts the <index>th bit of it; this will be either
   TRUE or FALSE;  if <index> is off the edge of the
   implemented bitstring, FALSE is returned.
*/

/* Assumes bits off the end of the bit string are zero. */
Define_Primitive(Prim_BSt_Check, 2, "BIT-STRING-CHECK")
{ long index, mask;
  Pointer *word;
  Primitive_2_Args();
  CHECK_ARG(1,BIT_STRING_P);
  index=(arg_nonnegative_integer(2));
  if (index >= bit_string_length(Arg1)) PRIMITIVE_RETURN(NIL);
     else {word = Nth_Vector_Loc(Arg1, index_to_word(Arg1,index));
	   mask = 1 << (index % POINTER_LENGTH);
	   if (((bit_string_word(word)) & mask) == 0) 
	     PRIMITIVE_RETURN(NIL);
	   else PRIMITIVE_RETURN(TRUTH);
	 }
}


/*(BIT-STRING-AND bitstring1 bitstring2)
   Returns a new bitstring which is the logical 
   bitwise AND of <bitstring1> and <bitstring2>. 
   The length of this bitstrings actual representation
   is the shorter of the lengths of the two input 
   bitstrings.
*/

Define_Primitive(Prim_BSt_And, 2, "BIT-STRING-AND")
{ fast Pointer *short_scan, *long_scan, *new_scan;
  Pointer short_string, long_string, new_string;
  long long_length, short_length, i, temp;
  Primitive_2_Args();
  CHECK_ARG(1,BIT_STRING_P);
  CHECK_ARG(2,BIT_STRING_P);
  /* Now we sort out the longer bit string */
  long_length=bit_string_length(Arg1);
  short_length=bit_string_length(Arg2);
  if (long_length >= short_length)
    {long_string=Arg1;short_string=Arg2;}
  else {long_string=Arg2; short_string=Arg1;
        temp=long_length;long_length=short_length;short_length=temp;}
  /* We make a new bit string as long as the shorter one */
  new_string=(cons_bit_string(short_length));
  long_scan=(bit_string_low_ptr(long_string));
  short_scan=(bit_string_low_ptr(short_string));
  new_scan=(bit_string_low_ptr(new_string));
  for (i=(size_in_words(new_string));(i > 0); i= i-1)
    *inc_bit_string_ptr(new_scan) =
      *inc_bit_string_ptr(long_scan) &
	*inc_bit_string_ptr(short_scan);
  PRIMITIVE_RETURN(new_string); 
}


/*(BIT-STRING-OR bitstring1 bitstring2)
   Returns a new bitstring which is the logical 
   bitwise OR of <bitstring1> and <bitstring2>. 
   The length of this bitstrings actual representation
   is the shorter of the lengths of the two input 
   bitstrings.
*/

Define_Primitive(Prim_BSt_Or, 2, "BIT-STRING-OR")
{ fast Pointer *short_scan, *long_scan, *new_scan;
  Pointer short_string, long_string, new_string;
  long long_length, short_length, i, temp;
  Primitive_2_Args();
  CHECK_ARG(1,BIT_STRING_P);
  CHECK_ARG(2,BIT_STRING_P);
  long_length=(bit_string_length(Arg1));
  short_length=(bit_string_length(Arg2));
  if (long_length >= short_length)
    {long_string=Arg1;short_string=Arg2;}
  else {long_string=Arg2; short_string=Arg1;
        temp=long_length;long_length=short_length;short_length=temp;}
  new_string=(cons_bit_string(long_length));
  short_scan=(bit_string_low_ptr(short_string));
  long_scan=(bit_string_low_ptr(long_string));
  new_scan=(bit_string_low_ptr(new_string));
  for (i=size_in_words(short_string);i > 0; i-= 1)
     *inc_bit_string_ptr(new_scan) =
       *inc_bit_string_ptr(long_scan) |
	 *inc_bit_string_ptr(short_scan);
  for (i=(size_in_words(long_string)-size_in_words(short_string));i >= 0; i-= 1)
    *inc_bit_string_ptr(new_scan) = *inc_bit_string_ptr(long_scan);
  PRIMITIVE_RETURN(new_string); 
}



/*(BIT-STRING-MODIFY bitstring index value)
  Returns a bitstring which is a copy of <bitstring>
  with the <index>th element set to <value> (which
  must be boolean).
*/
 
Define_Primitive(Prim_BSt_Mod, 3, "BIT-STRING-MODIFY")
{ Boolean value_to_set;
  long index, length, mask, i;
  fast Pointer *word, result, *result_scan, *source_scan;
  Primitive_3_Args();
  CHECK_ARG(1,BIT_STRING_P);
  index=(arg_nonnegative_integer(2));
  if (Arg3 == NIL) value_to_set=false;
  else value_to_set=true;
  length=bit_string_length(Arg1);
  word=Nth_Vector_Loc(Arg1,index_to_word(Arg1,index));
  mask=(1 << (index % POINTER_LENGTH));
  /* In some cases, no change is neccessary:
       if you are storing zero, and the index is past the string's end.
       or if you are storing a value which is already there. */
  if ((~value_to_set & (index >= length)) |
      (~value_to_set & ((bit_string_word(word) & mask) == 0)) |
      ((value_to_set & (index < length))
       & ((bit_string_word(word) & mask) != 0)))
    PRIMITIVE_RETURN(Arg1);
  /* Otherwise, cons a new long-enough bit string to side effect. */
  if (index >= length) result=cons_bit_string(index+1);
  else result=cons_bit_string(length);
  source_scan=bit_string_low_ptr(Arg1);
  result_scan=bit_string_low_ptr(result);
  /* Do the copying .... */
  for (i=(size_in_words(Arg1));i>0;i -= 1)
    *(inc_bit_string_ptr(result_scan)) = *(inc_bit_string_ptr(source_scan));
  /* And finally, actually do the side effect. */
  bit_string_word(Nth_Vector_Loc(result,index_to_word(result,index))) |= mask;
  PRIMITIVE_RETURN(result);
}


/*(BIT-STRING-MODIFY! bitstring index value)
  Returns a bitstring which is a copy of <bitstring>
  with the <index>th element set to <value> (which
  must be boolean).
*/
 
Define_Primitive(Prim_BSt_Mod_X, 3, "BIT-STRING-MODIFY!")
{ Boolean value_to_set;
  long index, length, mask, i;
  fast Pointer *word, result, *result_scan, *source_scan;
  Primitive_3_Args();
  CHECK_ARG(1,BIT_STRING_P);
  index=(arg_nonnegative_integer(2));
  if (Arg3 == NIL) value_to_set=false;
  else value_to_set=true;
  length=bit_string_length(Arg1);
  word=Nth_Vector_Loc(Arg1,index_to_word(Arg1,index));
  mask=(1 << (index % POINTER_LENGTH));
  /* In some cases, no change is neccessary:
       if you are storing zero, and the index is past the string's end.
       or if you are storing a value which is already there. */
  if ((~value_to_set & (index >= length)) |
      (~value_to_set & ((bit_string_word(word) & mask) == 0)) |
      ((value_to_set & (index < length))
       & ((bit_string_word(word) & mask) != 0)))
    PRIMITIVE_RETURN(Arg1);
  /* Otherwise, we have to do some side effecting.... */
  if (index >= length)
    /* If the place we have to effect is past the end of the string,
       we have to cons a new bit string.  */ 
    {
      /* To avoid such things in the future, we make a
	new bit string which is twice as long. */
      result=cons_bit_string(index*2+1);
      /* We copy the old string into the new string */
      source_scan=bit_string_low_ptr(Arg1);
      result_scan=bit_string_low_ptr(result);
      /* Do the copying .... */
      for (i=(size_in_words(Arg1));i>0;i -= 1)
	*(inc_bit_string_ptr(result_scan)) = *(inc_bit_string_ptr(source_scan));
    }
  else result=Arg1;
  bit_string_word(Nth_Vector_Loc(result,index_to_word(result,index))) |= mask;
  PRIMITIVE_RETURN(result);
}








