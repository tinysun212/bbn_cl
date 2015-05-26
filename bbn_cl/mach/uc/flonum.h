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

/* $Header: flonum.h,v 10.0 88/12/07 13:07:38 las Exp $
 * $MIT-Header: flonum.h,v 9.23 87/07/07 20:00:47 GMT cph Rel $
 *
 * Header file for flonums. Shared by various arithmetic primitive
 * files.  Some additional (configuration dependent) information
 * defined in CONFIG.H
 */

#define FLONUM_SIZE							\
  ((sizeof(Pointer)+sizeof(double)-1)/ sizeof(Pointer))
				 
#define flonum_exceeds_fixnum(temp)					\
  ((temp >= (((double) BIGGEST_FIXNUM) + 0.5))	||			\
   (temp <= (((double) SMALLEST_FIXNUM) - 0.5)))

/* Flonum_Result is a macro used at the end of the operations which
   return floating point results.  It checks that there is sufficient
   space in the heap for the result, creates the appropriate header,
   and returns the result. Store_Flonum_Result is similar but stores
   the result instead of returning.  Both are implemented in terms
   of Generic_Flonum_Result.
*/

#define Generic_Flonum_Result(Ans, How_To_Return, Value_Cell) 		\
  How_To_Return(Allocate_Float(Ans), Value_Cell)

#define Flonum_Result(Ans)						\
Generic_Flonum_Result(Ans, Return_Coerced_Value, Null_Value_Cell);

#define Store_Flonum_Result(Ans, Value_Cell)            		\
Generic_Flonum_Result(Ans, Store_Coerced_Value, Value_Cell);

/* Reduced_Flonum_Result is same as Flonum_Result except that it
   tries to coerce down, if possible.  Store_Reduced_Flonum_Result
   likewise is the same as Store_Flonum_Result except that it
   tries to coerce down.  Both are implemented in terms of 
   Generic_Reduced_Flonum_Result.
*/

#define Generic_Reduced_Flonum_Result(Ans, How_To_Return, Value_Cell)	\
{									\
  double Number = (Ans);						\
  double floor();							\
  Pointer result;							\
									\
  if (DOWNWARD_COERCE_FLONUM_P (Number))				\
  {									\
    Generic_Flonum_Result(Number, How_To_Return, Value_Cell);		\
  }									\
  else if (Number == 0)							\
    How_To_Return(Make_Unsigned_Fixnum(0), Value_Cell);			\
  {									\
    int exponent;							\
    double frexp();							\
									\
    frexp(Number, &exponent);						\
    if (exponent <= FIXNUM_LENGTH)					\
    {									\
      double_into_fixnum(Number, result);				\
      How_To_Return(result, Value_Cell);				\
    }									\
    /* Since the float has no fraction, we will not gain		\
       precision if its mantissa has enough bits to support		\
       the exponent. */							\
    else if (exponent <= FLONUM_MANTISSA_BITS)				\
    {									\
      result = Float_To_Big(Number);					\
      How_To_Return(result, Value_Cell);				\
    }									\
    else								\
      Generic_Flonum_Result(Number, How_To_Return, Value_Cell);		\
  }									\
}

#define Store_Reduced_Flonum_Result(Ans, Value_Cell)			\
Generic_Reduced_Flonum_Result(Ans, Store_Coerced_Value, Value_Cell);

#define Reduced_Flonum_Result(Ans)					\
Generic_Reduced_Flonum_Result(Ans, Return_Coerced_Value, Null_Value_Cell);

#define Store_Coerced_Value(Answer, Value_Cell) Value_Cell = Answer;

#define Return_Coerced_Value(Answer, Value_Cell) return Answer;

#define Null_Value_Cell
