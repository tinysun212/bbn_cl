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

/*
  NOTE: There are no primitives defined in the file.  This file was
  created soley for commonlisp make-array. 
  CORRECTION: I added one, cl-ivector-equal --las
*/

#define private	static
#define debug 1

#define Abs(X) (X < 0 ? (- X) : X)

/* The header is three words long.  The first word is exactly the same as
   scheme vectors.  It contains the length in 32-bit words allocated to
   the array (it includes the two other header bytes).  The second word
   contains the number of entries.  The third word contains the array
   subtype. The absolute value of this field is the number of bits, 
   positive implies unsigned and negative implies signed. Note that
   an unsigned field of 1 bit is equivalent to the type BIT but 
   a signed field of 1 bit is the range [-1,0]. Fields up to 
   16 bits are supported.

   (Eventually we may want to define this for 32-bit quantities.) 

   NOTE:
   We call make_cl_ivector with a bit size of 1 only for binary streams.
   In making an array, scheme bit vectors are used.
*/

#define LENGTH_SLOT		0	/* don't change this! */
#define ENTRIES_SLOT		1
#define SUBTYPE_SLOT		2
#define HEADER_LENGTH		3

tc_cl_ivector_overhead()
{
	return HEADER_LENGTH;
}

char *cl_ivector_to_c_string(ivector)
Pointer ivector;
{
  return (char *) Nth_Vector_Loc(ivector, HEADER_LENGTH);
}

long cl_ivector_subtype(ivector)
Pointer ivector;
{
  long result;

  Sign_Extend(Fast_Vector_Ref(ivector, SUBTYPE_SLOT), result);
  return result;
}

ivector_error(str, a1, a2, a3)
{
	if (debug)
		printf(str, a1, a2, a3);
	Primitive_Error(ERR_EXTERNAL_RETURN);
}

ivector_warning(str, a1, a2, a3)
{
  if (debug) 
    {
      printf("WARNING: ");
      printf(str, a1, a2, a3);
    }
}

Pointer
make_cl_ivector(entries, subtype, init)
long	entries,
	subtype,
	init;
{ 
	long	fill_word,
		i,
		words_needed,
	        min_init,
                max_init,
		bpe;			/* bits per entry */
	Pointer	ptr;

	bpe = Abs(subtype);
	if (bpe < 1 || bpe > 16)
		ivector_error("subtype out of range");
	if (subtype < 0)
	  {
	    min_init = - (1 << (bpe - 1));
	    max_init = (1 << (bpe - 1)) - 1;
	  }
	else
	  {
	    min_init = 0;
	    max_init = (1 << bpe) - 1;
	  }
	if (init < min_init || init > max_init)
		ivector_error("initial value %d value out of range", init);
	/* convert size in n-bit bytes to POINTER_LENGTH words */
	words_needed = (((entries * bpe) + POINTER_LENGTH - 1) / POINTER_LENGTH);

	Primitive_GC_If_Needed(words_needed + HEADER_LENGTH);
	ptr = Make_Pointer(TC_CL_IVECTOR, Free);
	Free[LENGTH_SLOT] = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, words_needed + HEADER_LENGTH - 1);
	Free[ENTRIES_SLOT] = C_Integer_To_Scheme_Integer(entries);
	Free[SUBTYPE_SLOT] = C_Integer_To_Scheme_Integer(subtype);
	Free += HEADER_LENGTH;

	/* given the initial value, figure out what the mask for each
	   POINTER_LENGTH entry will look like */
	fill_word = 0;
	for (i = 0; i < POINTER_LENGTH; i += bpe)
		fill_word |= (init << i);

	for (i = words_needed; --i >= 0; )
		*Free++ = (Pointer) fill_word;

	return ptr;
}

long
cl_ivector_length(iv)
Pointer	iv;
{
	return scheme_to_c_integer(Vector_Ref(iv, ENTRIES_SLOT));
}

Pointer
cl_ivector_ref(iv, offset)
Pointer	iv;
long	offset;
{
	long	shift,
		bpe,			/* bits per entry */
		epw,			/* entries per word */
		word_no,
		subtype,
	        result;

	Sign_Extend(Fast_Vector_Ref(iv, SUBTYPE_SLOT), subtype);
	bpe = Abs(subtype);
	epw = POINTER_LENGTH / bpe;
	word_no = offset / epw;
	shift = ((epw - 1) - (offset % epw)) * bpe;
	result = (((Fast_Vector_Ref(iv, HEADER_LENGTH + word_no)) >> shift) &
		  ((1 << bpe) - 1));
	if (subtype < 0)
	  {
	    long n_upper_bits = POINTER_LENGTH - bpe;
	    result = (result << n_upper_bits) >> n_upper_bits;
	  }
	return C_Integer_To_Scheme_Integer(result);
}

Pointer
cl_ivector_set(iv, offset, value)
Pointer	iv;
long	offset,
	value;
{
	long	shift,
		mask,
		bpe,	/* bits per entry */
		epw,	/* entries per word */
		oldval,
		wordval,
		subtype,
		word_no,
	        min_val,
	        max_val;

	Sign_Extend(Vector_Ref(iv, SUBTYPE_SLOT), subtype);
	bpe = Abs(subtype);
	if (subtype < 0)
	  {
	    min_val = - (1 << (bpe - 1));
	    max_val = (1 << (bpe - 1)) - 1;
	  }
	else
	  {
	    min_val = 0;
	    max_val = (1 << bpe) - 1;
	  }
	if (value < min_val || value > max_val)
		ivector_error("%d value out of range", value);
	epw = POINTER_LENGTH / bpe;
	word_no = offset / epw;
	shift = ((epw - 1) - (offset % epw)) * bpe; /* Big-endian byte sex so can use as file buffers */
	wordval = (Fast_Vector_Ref(iv, HEADER_LENGTH + word_no));
	mask = ((1 << bpe) - 1);
	oldval = ((wordval >> shift) & mask);
	Fast_Vector_Set(iv, HEADER_LENGTH + word_no,
		((wordval & ~(mask << shift)) | ((value & mask) << shift)));

	return C_Integer_To_Scheme_Integer(value);
}

Pointer
cl_ivector_subtype_indicator(iv)
Pointer	iv;
{
	switch (scheme_to_c_integer(Vector_Ref(iv, SUBTYPE_SLOT))) {
	case   1:   return List_unsigned_byte_1;
	case   2:   return List_unsigned_byte_2;
	case   4:   return List_unsigned_byte_4;
	case   8:   return List_unsigned_byte_8;
	case  16:   return List_unsigned_byte_16;
	case  -1:   return List_signed_byte_1;
	case  -2:   return List_signed_byte_2;
	case  -4:   return List_signed_byte_4;
	case  -8:   return List_signed_byte_8;
	case -16:   return List_signed_byte_16;

	default:
		ivector_error("ivector subtype %d: out of range", Vector_Ref(iv, SUBTYPE_SLOT));
	}
}

/* Is INDICATOR of the form

     ([signed-byte | unsigned-byte]  [1 | 2 | 4 | 8 | 16])

   ?  If so, return true and store the corresponding subtype code in
   SUBTYPE.  Therefore, we assume that INDICATOR is a list, that the
   first element is Sym_signed_byte or Sym_unsigned_byte, 
   and that the second (and last) element is
   a number >= 1 and <= 16. */

Boolean
is_cl_ivector_spec(indicator, subtype)
Pointer	indicator;
long	*subtype;
{
	long	type,
		number,
	        sign,
	        nbits;
	Pointer	second_cons,
		second,
	        type_symbol;

	type = Type_Code(indicator);
	if (type != TC_LIST)
		return false;
	type_symbol = Vector_Ref(indicator, CONS_CAR);
	if (type_symbol == Sym_unsigned_byte)
	  sign = 1;
	else if (type_symbol == Sym_signed_byte)
	  sign = -1;
	else return false;
	second_cons = Vector_Ref(indicator, CONS_CDR);
	if (Vector_Ref(second_cons, CONS_CDR) != NIL)
		return false;
	second = Vector_Ref(second_cons, CONS_CAR);
	if (Type_Code(second) != TC_FIXNUM)
		return false;
	/* gettin' there, folks! */
	(void) Scheme_Integer_To_C_Integer(second, &number);
	/* Since the bit sizes we can represent are all powers
	   of two, we round up by taking 2**ceiling(log2(number)) */
	number--;
	for (nbits = 0; number != 0; nbits++) number >>= 1;
	nbits = 1 << nbits;
	if (nbits <= 0 || nbits > 16)
	  return false;
	else
	  *subtype = nbits * sign;
	return true;
}

DEFINE_PRIMITIVE("CL-IVECTOR-EQUAL", prim_cl_ivector_equal, 2)
{
  long len, i;
  Primitive_2_Args();
  Arg_1_Type(TC_CL_IVECTOR);
  Arg_2_Type(TC_CL_IVECTOR);  

  len = cl_ivector_length(Arg1);
  if (len != cl_ivector_length(Arg2))
    return NIL;
  for (i = 0; i < len; i++)
    if (cl_ivector_ref(Arg1, i) != cl_ivector_ref(Arg2, i))
      return NIL;
  return TRUTH;
}
