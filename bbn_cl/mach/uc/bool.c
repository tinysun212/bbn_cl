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
/* File: BOOL.C 
 *  
 * Logical operations on numbers. 
 */

#include "scheme.h"
#include "primitive.h"
#include "bignum.h"

#define BOOLE_CLR	0
#define BOOLE_SET	1
#define BOOLE_1		2
#define BOOLE_2		3
#define BOOLE_C1	4
#define BOOLE_C2	5
#define BOOLE_AND	6
#define BOOLE_IOR	7
#define BOOLE_XOR	8
#define BOOLE_EQV	9
#define BOOLE_NAND     10
#define BOOLE_NOR      11
#define BOOLE_ANDC1    12
#define BOOLE_ANDC2    13
#define BOOLE_ORC1     14
#define BOOLE_ORC2     15


/* Compute a specified Boolean operation on a pair of longs.  Be careful
 * to convert arguments and answer properly to and from longs. */

long do_boole(op, a1, a2)
    int op;
    long a1, a2;
{

    switch (op)
	{
        case BOOLE_CLR:
     	    return 0L;

        case BOOLE_SET:
	    return -1L;

        case BOOLE_1:
	    return a1;

        case BOOLE_2:
	    return a2;

        case BOOLE_C1:
	    return (~a1);

        case BOOLE_C2:
	    return (~a2);

        case BOOLE_AND:
	    return (a1 & a2);

        case BOOLE_IOR:
	    return (a1 | a2);

        case BOOLE_XOR:
	    return (a1 ^ a2);

        case BOOLE_EQV:
	    return (~(a1 ^ a2));

        case BOOLE_NAND:
	    return (~(a1 & a2));

        case BOOLE_NOR:
	    return (~(a1 | a2));

        case BOOLE_ANDC1:
	    return ((~a1) & a2);

        case BOOLE_ANDC2:
	    return (a1 & (~a2));

        case BOOLE_ORC1:
	    return ((~a1) | a2);

        case BOOLE_ORC2:
	    return (a1 | (~a2));

	default:
	    /* op pre-checked, so should have some "impossible error" here */
	    printf("Bad op argument to do_boole");
	}
}


big_logop(op, Big1, Big2)
	int op;
	Pointer Big1, Big2;
	{
	fast bigdigit *bd1, *bd2;
        fast bigdigit *top1, *bottom1, *top2, *bottom2;
        fast bigdigit *Answer;
	long length, length1, length2;
        long Size;
	int sign1, sign2, sign;
	long proxy1, proxy2, proxy;
	int carry1, carry2, carry;
	bigdouble digit1, digit2, digit;
	int i;

	bd1 = BIGNUM(Get_Pointer(Big1));
	bd2 = BIGNUM(Get_Pointer(Big2));

	length1 = LEN(bd1);
	length2 = LEN(bd2);

	length = (length1 > length2) ? length1 : length2;

        /* Allocate Storage and do GC if needed */
        Size = Align(length + 1);
        Primitive_GC_If_Needed(Size);
        Answer = BIGNUM(Free);

	/* do a proxy computation with integers to get sign of answer */
	sign1 = SIGN(bd1);
	sign2 = SIGN(bd2);
	proxy1 = (sign1 == POSITIVE) ? 1L : -1L;
	proxy2 = (sign2 == POSITIVE) ? 1L : -1L;
	proxy = do_boole(op, proxy1, proxy2);
	sign = (proxy < 0) ? NEGATIVE : POSITIVE;
        Prepare_Header(Answer, length + 1, sign);

        /* Prepare Scanning Pointers and delimiters */
        top1 = Bignum_Top(bd1);
        top2 = Bignum_Top(bd2);
        bottom1 = Bignum_Bottom(bd1);
        bottom2 = Bignum_Bottom(bd2);
        Answer = Bignum_Bottom(Answer);  /* note redefinition of Answer */

	/* Set up for in-place two's complement, if needed */
	/* (the whole computation is messy because BIGNUM's are */
	/* stored as sign+magnitude and the operations are defined */
	/* as two's complement) */
	carry = (sign == NEGATIVE) ? 1 : 0;
	carry1 = (sign1 == NEGATIVE) ? 1 : 0;
	carry2 = (sign2 == NEGATIVE) ? 1 : 0;

        /* Starts Looping */
	for (i = 0; i < length; i++)
	    {
	    if (top1 >= bottom1)
		{
		digit1 = *bottom1++;
		if (sign1 == NEGATIVE)
		    {
		    digit1 = ((~digit1) & DIGIT_MASK) + carry1;
		    if ((digit1 & CARRY_MASK) == 0)
			carry1 = 0;
		    digit1 &= DIGIT_MASK;
		    }
		}
	    else
		if (sign1 == POSITIVE)
		    digit1 = 0;
		else
		    digit1 = DIGIT_MASK;

	    if (top2 >= bottom2)
		{
		digit2 = *bottom2++;
		if (sign2 == NEGATIVE)
		    {
		    digit2 = ((~digit2) & DIGIT_MASK) + carry2;
		    if ((digit2 & CARRY_MASK) == 0)
			carry2 = 0;
		    digit2 &= DIGIT_MASK;
		    }
		}
	    else
		if (sign2 == POSITIVE)
		    digit2 = 0;
		else
		    digit2 = DIGIT_MASK;

	    /* The big moment! */
	    digit = (do_boole(op, (long)digit1, (long)digit2) & DIGIT_MASK);

	    if (sign == NEGATIVE)
		{
		digit = ((~digit) & DIGIT_MASK) + carry;
		if ((digit & CARRY_MASK) == 0)
		    carry = 0;
		digit &= DIGIT_MASK;
		}

	    *Answer++ = digit;
	    }

	*Answer = carry;

	/* Trim off leading zeros from answer */
        trim_bignum((bigdigit *) Free);
        Free += Size;   /* Size isn't necessarily exact... */
        return Big_To_Fix(Make_Pointer(TC_BIG_FIXNUM, Free - Size));
	}


/*
  	String Hashing Function

	WARNING: Possibly Machine Dependent
*/

Define_Primitive(Prim_Hash_A_String, 3, "SX-HASH-SIMPLE-STRING")
{ fast char *string;
  fast long i, hash, ch, temp, first, last;
  long length;

  Primitive_3_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  if (Arg2!=NIL) Arg_2_Type(TC_FIXNUM);
  if (Arg3!=NIL) Arg_3_Type(TC_FIXNUM);

  length=Get_Integer(Vector_Ref(Arg1,STRING_LENGTH));
  string=(char *) Nth_Vector_Loc(Arg1,STRING_CHARS);
  if (Arg2==NIL) 
    first=0;
  else Range_Check(first,Arg2,0,length-1,ERR_ARG_2_BAD_RANGE);
  if (Arg3==NIL)
    last=length;
  else Range_Check(last,Arg3,first+1,length,ERR_ARG_3_BAD_RANGE);

  hash=0;
  for (i=first; i<last; i++) {

    ch = string[i] & 0x1f;	/* So that 'A' and 'a' are the same. */
    hash = (((~hash) & ch) | (hash & (~ch)));
    temp = (hash&0x0003ffff) << 5;
    hash = temp | ((hash&0x007c0000) >> 18); }


  return Make_Non_Pointer(TC_FIXNUM,hash);
}

Define_Primitive(prim_boole, 3, "BOOLE")
{
  long tcx, tcy, op;
  Pointer x, y;
  Primitive_3_Args();

  x = Arg2;
  y = Arg3;
  Arg_1_Type(TC_FIXNUM);
  op = Get_Integer(Arg1);
  if ((op < 0) || (op > 15))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  tcx = Type_Code(x);
  if ((tcx != TC_FIXNUM) && (tcx != TC_BIG_FIXNUM))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  tcy = Type_Code(y);
  if ((tcy != TC_FIXNUM) && (tcy != TC_BIG_FIXNUM))
    Primitive_Error(ERR_ARG_3_BAD_RANGE);

  /* Future-touch code here ... */

  switch (tcx)
    {
    case TC_FIXNUM:
      switch (tcy)
	{
	case TC_FIXNUM:
	  return Make_Non_Pointer(TC_FIXNUM,do_boole(op,Get_Integer(x),Get_Integer(y)));
	case TC_BIG_FIXNUM:
	  return big_logop(op,Fix_To_Big(x),y);
	}
    case TC_BIG_FIXNUM:
      switch (tcy)
	{
	case TC_FIXNUM:
	  	  return big_logop(op,x,Fix_To_Big(y));
	case TC_BIG_FIXNUM:
		  return big_logop(op,x,y);
		}
    }
}
