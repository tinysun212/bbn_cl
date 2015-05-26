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
#include "complex.h"
#include "ratio.h"
#include "flonum.h"
#include "generic-error.h"

/**********************************************************/
/*                                                        */
/*     UTILITIES                                          */
/*                                                        */
/**********************************************************/

static Pointer FIXNUM_0 = Make_Non_Pointer(TC_FIXNUM, 0);
static Pointer FIXNUM_1 = Make_Non_Pointer(TC_FIXNUM, 1);
static Pointer FIXNUM_2 = Make_Non_Pointer(TC_FIXNUM, 2);
static Pointer FIXNUM_3 = Make_Non_Pointer(TC_FIXNUM, 3);
static Pointer FIXNUM_4 = Make_Non_Pointer(TC_FIXNUM, 4);
static Pointer FIXNUM_MINUS_1 =   Make_Signed_Fixnum(-1);

static double pi        = 3.14159265358979323846264338329750288419716939937511;
static double pi_by_2   = 1.57079632679489661923132169164875144209858469968755;

long Generic_Status;

#define truncate(arg1, arg2) scheme_truncate(scheme_divide(arg1, arg2))
#define integer_divide(arg1, arg2) Vector_Ref(scheme_integer_divide(arg1, arg2), \
					      CONS_CAR)
#define remainder(arg1, arg2) Vector_Ref(scheme_integer_divide(arg1, arg2),      \
					 CONS_CDR)

#define reduce_ratio(rat) scheme_divide(ratio_numer(rat), ratio_denom(rat))

#define scheme_evenp(arg1) scheme_zerop(remainder(arg1, FIXNUM_2))
#define scheme_oddp(arg1) (!(scheme_evenp(arg1)))

#define REALP(x)                          \
((Type_Code(x) == TC_FIXNUM) ||           \
 (Type_Code(x) == TC_BIG_FIXNUM) ||       \
 (Type_Code(x) == TC_BIG_FLONUM) ||       \
 (Type_Code(x) == TC_RATIO))

#define GENERIC_NUMBERP(x)                \
  (REALP(x) || (Type_Code(x) == TC_COMPLEX))

#define INTEGERP(x)                       \
((Type_Code(x) == TC_FIXNUM) ||           \
 (Type_Code(x) == TC_BIG_FIXNUM))

#define RATIONALP(x)                      \
(INTEGERP(x) || (Type_Code(x) == TC_RATIO))

#define COMPLEXP(x)                       \
(Type_Code(x) == TC_COMPLEX)

Pointer scheme_abs(Arg1)
Pointer Arg1;
{ if (scheme_negativep(Arg1))
    return(scheme_minus(FIXNUM_0, Arg1));
  else
    return Arg1;
}

Pointer scheme_gcd(Arg1, Arg2)
Pointer Arg1, Arg2;
{ if (scheme_zerop(Arg2))
    return(scheme_abs(Arg1));
  else
    return(scheme_gcd(Arg2, remainder(Arg1, Arg2)));
}

Pointer scheme_lcm(Arg1, Arg2)
Pointer Arg1, Arg2;
{ if (scheme_zerop(Arg2))
    return FIXNUM_0;
  else if (scheme_zerop(Arg1))
    return FIXNUM_0;
  else
    return(scheme_divide(scheme_abs(scheme_multiply(Arg1, Arg2)),
			 scheme_gcd(Arg1, Arg2)));
}

#define Variable_Two_Arg_Check(arg1, arg2)                      \
  if (generic_errorp())                                         \
  { if (is_generic_error(GENERIC_ERROR_2_ARG2_DISPATCH))        \
    { error_wrong_type_arg(arg2);                               \
    }                                                           \
    else if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH) || \
	     is_generic_error(GENERIC_ERROR_2_ARG1_DISPATCH))   \
    { error_wrong_type_arg(arg1);                               \
    }                                                           \
    else                                                        \
    { Primitive_Error(ERR_EXTERNAL_RETURN);                     \
    }                                                           \
  }

/* We wan't to check flonums being returned from primitives
   to see if they are NANs or Infinites.  We specify an error
   if the proper mode is set.
*/

Boolean Check_NANp()
{ return true;
}

Pointer NAN_Or_Infinity_P();

NAN_check(x)
Pointer x;
{ if (Check_NANp())
  { if (Type_Code(x) == TC_BIG_FLONUM)
    { if (NAN_Or_Infinity_P(x))
      { CL_Error("Floating-point overflow error with ~f", 1, x);
      }
    }
    else if (Type_Code(x) == TC_COMPLEX)
    { NAN_check(complex_real_part(x));
      NAN_check(complex_imag_part(x));
    }
  }
}

/* So that the C compiler knows */
Pointer generic_equal(), generic_lessthan(), generic_plus(), generic_multiply();
Pointer generic_lessequalthan(), generic_greaterthan(), generic_greaterequalthan();
Pointer generic_minus(), generic_divide(), generic_one_minus();
Pointer generic_one_plus(), generic_zerop(), generic_positive(), generic_negative();
Pointer generic_expt(), generic_log(), complex_magnitude(), generic_abs();
Pointer generic_phase(), generic_sin(), generic_cos(), generic_tan();
Pointer generic_sinh(), generic_cosh(), generic_tanh();
Pointer generic_cis(), generic_atan(), complex_phase();


/**********************************************************/
/*                                                        */
/*     RATIOS and COMPLEX                                 */
/*                                                        */
/**********************************************************/
Pointer p_make_complex(real, imag)
Pointer real, imag;
{ Pointer result;
  Primitive_GC_If_Needed(2);
  result = Make_Pointer(TC_COMPLEX, Free);
  *Free++ = real;
  *Free++ = imag;
  return result;
}

Pointer coerce_to_float(arg)
Pointer arg;
{ Pointer result, intvalue;
  if (Type_Code(arg) == TC_FIXNUM)                   
  { Sign_Extend(arg, intvalue);
    Store_Flonum_Result(((double) intvalue), result);
    return result;
  }                                                  
  else if (Type_Code(arg) == TC_BIG_FIXNUM)          
  { result = Big_To_Float(arg);                           
    if (Type_Code(result) != TC_BIG_FLONUM)               
    { declare_error(GENERIC_ERROR_ARG1_COERCION);    
    }                                                
    else                                             
      return result;
  }
  else return(arg);
}

#define contagious(arg1, arg2)                       \
  if (Type_Code(arg1) == TC_BIG_FLONUM)              \
  { arg2 = coerce_to_float(arg2);                    \
  }                                                  \
  else if (Type_Code(arg2) == TC_BIG_FLONUM)         \
  { arg1 = coerce_to_float(arg1);                    \
  }


Pointer make_complex(real, imag)
Pointer real, imag;
{ if (RATIONALP(real) && INTEGERP(imag) && scheme_zerop(imag))
    return real;
  else 
  { contagious(real, imag);
    return p_make_complex(real, imag);
  }
}

Pointer p_make_ratio(numer, denom)
Pointer numer, denom;
{ Pointer result;
  Primitive_GC_If_Needed(2);
  result = Make_Pointer(TC_RATIO, Free);
  *Free++ = numer;
  *Free++ = denom;
  return result;
}

Pointer make_ratio(numer, denom)
Pointer numer, denom;
{ if (scheme_zerop(denom))
  { declare_error(GENERIC_ERROR_0_DENOM);
    return FIXNUM_0;
  }
  else if (scheme_zerop(numer))
    return FIXNUM_0;
  else
  { Pointer g, reduced_denom, reduced_numer;
    if (scheme_negativep(denom))
    { numer = scheme_minus(FIXNUM_0, numer);
      denom = scheme_minus(FIXNUM_0, denom);
    }
    g = scheme_gcd(numer, denom);
    reduced_numer = integer_divide(numer, g);
    reduced_denom = integer_divide(denom, g);
    if (scheme_equal(reduced_denom, FIXNUM_1))
      return reduced_numer;
    else
      return p_make_ratio(reduced_numer, reduced_denom);
  }
}

Define_Primitive(Prim_Make_Complex, -1, "MAKE-COMPLEX")
{ Primitive_Variable_Args();
  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else if (COMPLEXP(Primitive_Variable_Arg(1)))
  { error_wrong_type_arg(1);
  }
  else if (Number_Of_Args == 1)
  { Pointer Arg1 = Primitive_Variable_Arg(1);
    return(make_complex(Arg1, FIXNUM_0));
  }
  else if (!(REALP(Primitive_Variable_Arg(2))))
  { error_wrong_type_arg(2);
  }
  else
  { Pointer Arg1 = Primitive_Variable_Arg(1);
    Pointer Arg2 = Primitive_Variable_Arg(2);
    return(make_complex(Arg1, Arg2));
  }
}

Define_Primitive(Prim_Complex_Real, 1, "REALPART")
{ Primitive_1_Arg();
  if (Type_Code(Arg1) != TC_COMPLEX)
  { if (GENERIC_NUMBERP(Arg1))
      return(Arg1);
    else
      { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
      }
  }
  else
    return complex_real_part(Arg1);
}

Define_Primitive(Prim_Complex_Imag, 1, "IMAGPART")
{ Primitive_1_Arg();
  if (Type_Code(Arg1) != TC_COMPLEX)
  { if (GENERIC_NUMBERP(Arg1))
      return FIXNUM_0;
    else
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
  }
  else
    return complex_imag_part(Arg1);
}

Define_Primitive(Prim_Make_Ratio, 2, "MAKE-RATIO")
{ Pointer result;
  Primitive_2_Args();
  
  if (!(INTEGERP(Arg1)))
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  else if (!(INTEGERP(Arg2)))
  { Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  }
  else
  { result = make_ratio(Arg1, Arg2);
    if (generic_errorp())
    { if (is_generic_error(GENERIC_ERROR_0_DENOM))
      { Primitive_Error(ERR_ARG_2_WRONG_TYPE);
      }
      else
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    else
      return result;
  }
}

Define_Primitive(Prim_Ratio_Numerator, 1, "RATIO-NUMERATOR")
{ Primitive_1_Arg();
  if (Type_Code(Arg1) != TC_RATIO)
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  else
    return ratio_numer(Arg1);
}

Define_Primitive(Prim_Ratio_Denominator, 1, "RATIO-DENOMINATOR")
{ Primitive_1_Arg();
  if (Type_Code(Arg1) != TC_RATIO)
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  else
    return ratio_denom(Arg1);
}

/**********************************************************/
/*                                                        */
/*     TYPE TABLES                                        */
/*                                                        */
/**********************************************************/

#define INTEGER_CODE 0
#define SHIFTED_INTEGER_CODE 0
#define FLOAT_CODE 1
#define SHIFTED_FLOAT_CODE 4
#define RATIO_CODE 2
#define SHIFTED_RATIO_CODE 8
#define COMPLEX_CODE 3
#define SHIFTED_COMPLEX_CODE 12
#define ERROR_CODE_ARG1 50
#define ERROR_CODE_ARG2 100

static int arith_arg1_table[256] = {
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  SHIFTED_FLOAT_CODE,         /* 0x06 TC_BIG_FLONUM */
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  SHIFTED_INTEGER_CODE,       /* 0x0E TC_BIG_FIXNUM */
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  SHIFTED_INTEGER_CODE,       /* 0x1A TC_FIXNUM */
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  SHIFTED_COMPLEX_CODE,       /* 0x3C TC_COMPLEX */
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  SHIFTED_RATIO_CODE,         /*0x44 TC_RATIO */
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,

  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
  ERROR_CODE_ARG1,
};

static int arith_arg2_table[256] = {
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  FLOAT_CODE,         /* 0x06 TC_BIG_FLONUM */
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  INTEGER_CODE,       /* 0x0E TC_BIG_FIXNUM */
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  INTEGER_CODE,       /* 0x1A TC_FIXNUM */
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  COMPLEX_CODE,       /* 0x3C TC_COMPLEX */
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  RATIO_CODE,         /*0x44 TC_RATIO */
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,

  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
  ERROR_CODE_ARG2,
};

/**********************************************************/
/*                                                        */
/*     The Dispatcher for 2 Argument generic operators    */
/*                                                        */
/**********************************************************/

#define get_two_d_index(obj1, obj2)           \
  (arith_arg1_table[Type_Code(obj1)] | arith_arg2_table[Type_Code(obj2)])

#define defop2_semantics(GENERIC_NAME,                                                                                              int_int_body,     int_float_body,     int_ratio_body,     int_complex_body,                                float_int_body,   float_float_body,   float_ratio_body,   float_complex_body,                              ratio_int_body,   ratio_float_body,   ratio_ratio_body,   ratio_complex_body,                              complex_int_body, complex_float_body, complex_ratio_body, complex_complex_body) \
Pointer GENERIC_NAME(arg1, arg2)          \
Pointer arg1, arg2;                       \
{ switch (get_two_d_index(arg1, arg2))    \
  { case 0:  int_int_body;                \
    case 1:  int_float_body;              \
    case 2:  int_ratio_body;              \
    case 3:  int_complex_body;            \
    case 4:  float_int_body;              \
    case 5:  float_float_body;            \
    case 6:  float_ratio_body;            \
    case 7:  float_complex_body;          \
    case 8:  ratio_int_body;              \
    case 9:  ratio_float_body;            \
    case 10: ratio_ratio_body;            \
    case 11: ratio_complex_body;          \
    case 12: complex_int_body;            \
    case 13: complex_float_body;          \
    case 14: complex_ratio_body;          \
    case 15: complex_complex_body;        \
    default: break;                       \
  }                                       \
    if (get_two_d_index(arg1, arg2) >= ERROR_CODE_ARG2) \
    { declare_error(GENERIC_ERROR_2_ARG2_DISPATCH);     \
      return (FIXNUM_0);                                \
    }                                                   \
  else { declare_error(GENERIC_ERROR_2_ARG1_DISPATCH);  \
	 return (FIXNUM_0);                             \
       }                                                \
}

/**********************************************************/
/*                                                        */
/*     The Dispatcher for 1 Argument generic operators    */
/*                                                        */
/**********************************************************/

#define defop1_semantics(GENERIC_NAME, int_body, float_body, ratio_body, complex_body) \
Pointer GENERIC_NAME(arg1)                       \
Pointer arg1;                                    \
{ switch (arith_arg2_table[Type_Code(arg1)])     \
  { case 0:  int_body;                           \
    case 1:  float_body;                         \
    case 2:  ratio_body;                         \
    case 3:  complex_body;                       \
    default: break;                              \
  }                                              \
  declare_error(GENERIC_ERROR_1_ARG1_DISPATCH);  \
  return FIXNUM_0;                               \
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ZEROP                                      */
/*                                                        */
/**********************************************************/
defop1_semantics(generic_zerop,
		 
		 return(scheme_zerop(arg1)),
		 return(scheme_zerop(arg1)),
		 return(NIL),
		 { if (scheme_zerop(complex_real_part(arg1)) &&
		       scheme_zerop(complex_imag_part(arg1)))
		     return TRUTH;
		   else 
		     return NIL;
		 });

Define_Primitive(Prim_generic_zerop, 1, "GENERIC-ZEROP")
{ Pointer result;
  Primitive_1_Arg();

  clear_errors();
  result = generic_zerop(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
    return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-PLUSP                                      */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_plusp,
		 
		 return(scheme_positivep(arg1)),
		 return(scheme_positivep(arg1)),
		 return(scheme_positivep(ratio_numer(arg1))),
	         { declare_error(GENERIC_ERROR_COMPLEX_SIGN);
		   return NIL;
		 });

Define_Primitive(Prim_generic_plusp, 1, "GENERIC-PLUSP")
{ Pointer result;
  Primitive_1_Arg();

  clear_errors();
  result = generic_plusp(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
    return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-MINUSP                                     */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_minusp,
		 
		 return(scheme_negativep(arg1)),
		 return(scheme_negativep(arg1)),
		 return(scheme_negativep(ratio_numer(arg1))),
	         { declare_error(GENERIC_ERROR_COMPLEX_SIGN);
		   return NIL;
		 });

Define_Primitive(Prim_generic_minusp, 1, "GENERIC-MINUSP")
{ Pointer result;
  Primitive_1_Arg();

  clear_errors();
  result = generic_minusp(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
    return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ODDP                                       */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_oddp, 1, "GENERIC-ODDP")
{ Pointer result;
  Primitive_1_Arg();

  clear_errors();
  if (!(INTEGERP(Arg1)))
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  else
  { if (scheme_oddp(Arg1))
      result = TRUTH;
    else
      result = NIL;
    if (generic_errorp())
    { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
      { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
      }
      else
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    else
      return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-EVENP                                      */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_evenp, 1, "GENERIC-EVENP")
{ Pointer result;
  Primitive_1_Arg();

  clear_errors();
  if (!(INTEGERP(Arg1)))
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  else
  { result = scheme_evenp(Arg1);
    if (generic_errorp())
    { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
      { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
      }
      else
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    else
      return result;
  }
}

/* Body constructor for comparison operators */

#define Zero_Arg_Check()                                       \
   Primitive_Variable_Args();                                  \
   if (Number_Of_Args == 0)                                    \
   { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);           \
   }                                                           \
   clear_errors();

#define Make_Check(arg1, arg2)                                        \
     if(generic_errorp())                                             \
     { if (is_generic_error(GENERIC_ERROR_2_ARG2_DISPATCH))           \
       { error_wrong_type_arg(arg2);                                  \
       }                                                              \
     else if (is_generic_error(GENERIC_ERROR_2_ARG1_DISPATCH))        \
       { error_wrong_type_arg(arg1);                                  \
       }                                                              \
     else if (is_generic_error(GENERIC_ERROR_ARG2_COMPLEX_COMPARE))   \
       { error_wrong_type_arg(arg2);                                  \
       }                                                              \
     else if (is_generic_error(GENERIC_ERROR_ARG1_COMPLEX_COMPARE))   \
       { error_wrong_type_arg(arg1);                                  \
       }                                                              \
     else                                                             \
       { Primitive_Error(ERR_EXTERNAL_RETURN);                        \
       }                                                              \
     }

#define Comparison_Error_Check() Make_Check(i, i+1)
#define Max_Min_Error_Check()    Make_Check(i+1, i)

#define make_multiarg_comparison_body(op)                      \
   Pointer result = TRUTH;                                     \
   int i;                                                      \
   Zero_Arg_Check();                                           \
   for (i = 1; i < Number_Of_Args; i++)                        \
   { result = result & op(Primitive_Variable_Arg(i),           \
			  Primitive_Variable_Arg(i+1));        \
     Comparison_Error_Check();                                 \
     if (!result) break;                                       \
   }                                                           \
   return(result)

/* Body constructor for max/min operators */

#define make_multiarg_max_min_body(op)                         \
   Pointer result;                                             \
   int i;                                                      \
   Zero_Arg_Check();                                           \
   result = Primitive_Variable_Arg(1);                         \
   for (i = 1; i < Number_Of_Args; i++)                        \
   { if (op(Primitive_Variable_Arg(i+1), result))              \
       result = Primitive_Variable_Arg(i+1);                   \
     Max_Min_Error_Check();                                    \
   }                                                           \
   return(result)

/**********************************************************/
/*                                                        */
/*     GENERIC-=                                          */
/*                                                        */
/**********************************************************/

#define normal_complex_equal(A1, A2)                       \
{ if (generic_equal(complex_imag_part(A2), FIXNUM_0) &&    \
      generic_equal(complex_real_part(A2), A1))            \
	return TRUTH;                                      \
  else                                                     \
  return NIL;                                              \
}

defop2_semantics(generic_equal,

		 return(scheme_equal(arg1, arg2)),
		 return(scheme_equal(arg1, arg2)),
		 return(NIL),
		 normal_complex_equal(arg1, arg2),
		 
		 return(scheme_equal(arg1, arg2)),
		 return(scheme_equal(arg1, arg2)),
		 return(scheme_equal(arg1, reduce_ratio(arg2))),
		 normal_complex_equal(arg1, arg2),

		 return(NIL),
		 return(scheme_equal(reduce_ratio(arg1), arg2)),
  	         { if (generic_equal(ratio_numer(arg1),
				     ratio_numer(arg2)) &&
		       generic_equal(ratio_denom(arg1), ratio_denom(arg2)))
		     return TRUTH;
		   else return NIL;
		 },
		 normal_complex_equal(arg1, arg2),

		 normal_complex_equal(arg2, arg1),
		 normal_complex_equal(arg2, arg1),
		 normal_complex_equal(arg2, arg1),
	         { if (generic_equal(complex_real_part(arg1),
				     complex_real_part(arg2)) &&
		       generic_equal(complex_imag_part(arg1), 
				     complex_imag_part(arg2)))
		     return TRUTH;
		 else return NIL;
		 }
);

Define_Primitive(Prim_generic_equal, -1, "GENERIC-=")
{ make_multiarg_comparison_body(generic_equal);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-/=                                         */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_notequal, -1, "GENERIC-/=")
{  Pointer result = NIL;
   int i, j;
   Primitive_Variable_Args();
   
   clear_errors();
   for (i = 1; i < Number_Of_Args; i++)
     { for (j = i+1; j <= Number_Of_Args; j++)
       { result = result | generic_equal(Primitive_Variable_Arg(i),
					 Primitive_Variable_Arg(j));
	 Variable_Two_Arg_Check(i, j);
	 if (result)
	   break;
       }
       if (result)
	 break;
     }

   if (result)
   { return(NIL);
   }
   else 
   { return(TRUTH);
   }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-<                                          */
/*                                                        */
/**********************************************************/
#define complex_compare_arg2()                             \
{ declare_error(GENERIC_ERROR_ARG2_COMPLEX_COMPARE);       \
  return NIL;                                              \
}

#define complex_compare_arg1()                             \
{ declare_error(GENERIC_ERROR_ARG1_COMPLEX_COMPARE);       \
  return NIL;                                              \
}

defop2_semantics(generic_lessthan,
		 return(scheme_lessthan(arg1, arg2)),
		 return(scheme_lessthan(arg1, arg2)),
		 return (generic_lessthan(generic_multiply(arg1,
							   ratio_denom(arg2)),
					  ratio_numer(arg2))),
	         complex_compare_arg2(),
		 
		 return(scheme_lessthan(arg1, arg2)),
		 return(scheme_lessthan(arg1, arg2)),
		 return(generic_lessthan(arg1, reduce_ratio(arg2))),
	         complex_compare_arg2(),

		 return(generic_lessthan(ratio_numer(arg1),
					 generic_multiply(arg2,
							  ratio_denom(arg1)))),
		 return(generic_lessthan(reduce_ratio(arg1), arg2)),
		 return(generic_lessthan(generic_multiply(ratio_numer(arg1),
							  ratio_denom(arg2)),
					 generic_multiply(ratio_numer(arg2), 
							  ratio_denom(arg1)))),
		 complex_compare_arg2(),
		 
		 complex_compare_arg1(),
		 complex_compare_arg1(),
		 complex_compare_arg1(),
		 complex_compare_arg1());

Define_Primitive(Prim_generic_lessthan, -1, "GENERIC-<")
{ make_multiarg_comparison_body(generic_lessthan);
}

/**********************************************************/
/*                                                        */
/*     GENERIC->                                          */
/*                                                        */
/**********************************************************/

defop2_semantics(generic_greaterthan,
		 return(scheme_greaterthan(arg1, arg2)),
		 return(scheme_greaterthan(arg1, arg2)),
		 return (generic_greaterthan(generic_multiply(arg1,
							      ratio_denom(arg2)),
					     ratio_numer(arg2))),
		 complex_compare_arg2(),
		 
		 return(scheme_greaterthan(arg1, arg2)),
		 return(scheme_greaterthan(arg1, arg2)),
		 return(generic_greaterthan(arg1, reduce_ratio(arg2))),
		 complex_compare_arg2(),

		 return(generic_greaterthan(ratio_numer(arg1),
					    generic_multiply(arg2,
							     ratio_denom(arg1)))),
		 return(generic_greaterthan(reduce_ratio(arg1), arg2)),
		 return(generic_greaterthan(generic_multiply(ratio_numer(arg1),
							     ratio_denom(arg2)),
					    generic_multiply(ratio_numer(arg2), 
							     ratio_denom(arg1)))),
		 complex_compare_arg2(),
		 
		 complex_compare_arg1(),
		 complex_compare_arg1(),
		 complex_compare_arg1(),
		 complex_compare_arg1());

Define_Primitive(Prim_generic_greaterthan, -1, "GENERIC->")
{ make_multiarg_comparison_body(generic_greaterthan);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-<=                                         */
/*                                                        */
/**********************************************************/
Pointer generic_lessequalthan(arg1, arg2)
Pointer arg1, arg2;
{ if (generic_greaterthan(arg1, arg2))
    return(NIL);
  else 
    return(TRUTH);
}

Define_Primitive(Prim_generic_lessequalthan, -1, "GENERIC-<=")
{ make_multiarg_comparison_body(generic_lessequalthan);
}

/**********************************************************/
/*                                                        */
/*     GENERIC->=                                         */
/*                                                        */
/**********************************************************/
Pointer generic_greaterequalthan(arg1, arg2)
Pointer arg1, arg2;
{ if (generic_lessthan(arg1, arg2))
    return(NIL);
  else 
    return(TRUTH);
}

Define_Primitive(Prim_generic_greaterequalthan, -1, "GENERIC->=")
{ make_multiarg_comparison_body(generic_greaterequalthan);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-MAX                                        */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_max, -1, "GENERIC-MAX")
{ make_multiarg_max_min_body(generic_greaterthan);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-MIN                                        */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_min, -1, "GENERIC-MIN")
{ make_multiarg_max_min_body(generic_lessthan);
}

/* Body constructor for combining numeric operators */

#define make_multiarg_combiner_body(op, default)                   \
  Pointer result;                                                  \
  int i;                                                           \
  Primitive_Variable_Args();                                       \
  if (Number_Of_Args == 0)                                         \
  {  return(default);                                              \
  }                                                                \
  else if (Number_Of_Args == 1)                                    \
  {  if (GENERIC_NUMBERP(Primitive_Variable_Arg(1)))               \
     { result = op(default, Primitive_Variable_Arg(1));            \
       NAN_check(result);                                          \
       return(result);                                             \
     }                                                             \
     else                                                          \
     { error_wrong_type_arg(1);                                    \
     }                                                             \
  }                                                                \
  else                                                             \
  {  clear_errors();                                               \
     result = Primitive_Variable_Arg(1);                           \
     for (i = 2; i <= Number_Of_Args; i++)                         \
     { result = op(result, Primitive_Variable_Arg(i));             \
       Variable_Two_Arg_Check(i-1, i);                             \
     }                                                             \
     NAN_check(result);                                            \
     return(result);                                               \
  }

/**********************************************************/
/*                                                        */
/*     GENERIC-+                                          */
/*                                                        */
/**********************************************************/

#define normal_complex_plus(a1, a2)                          \
  (make_complex(generic_plus(a1, complex_real_part(a2)),     \
		complex_imag_part(a2)))

#define integer_ratio_plus(A1, A2)                             \
{ Pointer darg, n;                                             \
  darg = ratio_denom(A2);                                      \
  n = scheme_plus(scheme_multiply(A1, darg), ratio_numer(A2)); \
  return (p_make_ratio(n, darg));                              \
}

#define make_ratio_ratio_plus_minus(NAME, COMBINE_OP)                   \
Pointer NAME(arg1, arg2)                                                \
Pointer arg1, arg2;                                                     \
{ Pointer nn, nd;                                                       \
  Pointer narg1 = ratio_numer(arg1);                                    \
  Pointer narg2 = ratio_numer(arg2);                                    \
  Pointer darg1 = ratio_denom(arg1);                                    \
  Pointer darg2 = ratio_denom(arg2);                                    \
  Pointer g1 = scheme_gcd(darg1, darg2);                                \
  if (g1 == FIXNUM_1)                                                   \
    return(p_make_ratio(COMBINE_OP(scheme_multiply(narg1, darg2),       \
				 scheme_multiply(darg1, narg2)),        \
		      scheme_multiply(darg1, darg2)));                  \
  else                                                                  \
  { Pointer t1 = COMBINE_OP(scheme_multiply(narg1, truncate(darg2, g1)), \
			    scheme_multiply(truncate(darg1, g1),         \
					    narg2));                   \
    Pointer g2 = scheme_gcd(t1, g1);                                   \
    Pointer t2 = truncate(darg1, g1);                                  \
    if (g2 == FIXNUM_1)                                                \
    return (p_make_ratio(t1, scheme_multiply(t2, darg2)));             \
    else                                                               \
    { if (g2 == FIXNUM_0)                                              \
	return (FIXNUM_0);                                             \
      else                                                             \
      { Pointer nn = truncate(t1, g2);                                 \
	Pointer t3 = truncate(darg2, g2);                              \
	Pointer nd;                                                    \
	if (t2 == FIXNUM_1)                                            \
	  nd = t3;                                                     \
	else                                                           \
	  nd = scheme_multiply(t2, t3);                                \
	if (nd == FIXNUM_1)                                            \
	  return(nn);                                                  \
	else                                                           \
	  return (p_make_ratio(nn, nd));                               \
      }                                                                \
    }                                                                  \
  }                                                                    \
}

make_ratio_ratio_plus_minus(ratio_ratio_plus, scheme_plus);
make_ratio_ratio_plus_minus(ratio_ratio_minus, scheme_minus);

defop2_semantics(generic_plus,
		 
	         return(scheme_plus(arg1, arg2)),
		 return(scheme_plus(arg1, arg2)),
		 integer_ratio_plus(arg1, arg2),
		 return(normal_complex_plus(arg1, arg2)),
		
		 return(scheme_plus(arg1, arg2)),
		 return(scheme_plus(arg1, arg2)),
		 return(scheme_plus(arg1, reduce_ratio(arg2))),
		 return(normal_complex_plus(arg1, arg2)),
		
		 integer_ratio_plus(arg2, arg1),
		 return(scheme_plus(reduce_ratio(arg1), arg2)),
		 return(ratio_ratio_plus(arg1, arg2)),
		 return(normal_complex_plus(arg1, arg2)),

		 return(normal_complex_plus(arg2, arg1)),
		 return(normal_complex_plus(arg2, arg1)),
		 return(normal_complex_plus(arg2, arg1)),
		 return (make_complex(generic_plus(complex_real_part(arg1),
						   complex_real_part(arg2)),
				      generic_plus(complex_imag_part(arg1),
						   complex_imag_part(arg2))))
		 );

Define_Primitive(Prim_generic_plus, -1, "GENERIC-+")
{  make_multiarg_combiner_body(generic_plus, FIXNUM_0);
}

/**********************************************************/
/*                                                        */
/*     GENERIC--                                          */
/*                                                        */
/**********************************************************/

#define normal_complex_minus(a1, a2)                         \
  (make_complex(generic_minus(a1, complex_real_part(a2)),    \
		generic_minus(FIXNUM_0, complex_imag_part(a2))))

#define complex_normal_minus(a1, a2)                         \
  (make_complex(generic_minus(complex_real_part(a1), a2),    \
		complex_imag_part(a1)))

#define integer_ratio_minus()                                \
{ Pointer darg, n;                                           \
  darg = ratio_denom(arg2);                                  \
  n = scheme_minus(scheme_multiply(arg1, darg),              \
		   ratio_numer(arg2));                       \
  return (p_make_ratio(n, darg));                            \
}

#define ratio_integer_minus()                                \
{ Pointer darg, n;                                           \
  darg = ratio_denom(arg1);                                  \
  n = scheme_minus(ratio_numer(arg1),                        \
		   scheme_multiply(arg2, darg));             \
  return (p_make_ratio(n, darg));                            \
}

defop2_semantics(generic_minus,

		 return(scheme_minus(arg1, arg2)),
		 return(scheme_minus(arg1, arg2)),
		 integer_ratio_minus(),
		 return(normal_complex_minus(arg1, arg2)),
		 
		 return(scheme_minus(arg1, arg2)),
		 return(scheme_minus(arg1, arg2)),
		 return(scheme_minus(arg1, reduce_ratio(arg2))),
		 return(normal_complex_minus(arg1, arg2)),
		
		 ratio_integer_minus(),
		 return(scheme_minus(reduce_ratio(arg1), arg2)),
		 return(ratio_ratio_minus(arg1, arg2)),
		 return(normal_complex_minus(arg1, arg2)),

		 return(complex_normal_minus(arg1, arg2)),
		 return(complex_normal_minus(arg1, arg2)),
		 return(complex_normal_minus(arg1, arg2)),
		 return (make_complex(generic_minus(complex_real_part(arg1),
						    complex_real_part(arg2)),
				      generic_minus(complex_imag_part(arg1),
						    complex_imag_part(arg2))))
		 );

Define_Primitive(Prim_generic_minus, -1, "GENERIC--")
{  make_multiarg_combiner_body(generic_minus, FIXNUM_0);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-*                                          */
/*                                                        */
/**********************************************************/

#define normal_complex_multiply(a1, a2)                        \
  (make_complex(generic_multiply(a1, complex_real_part(a2)),   \
		generic_multiply(a1, complex_imag_part(a2))))

Pointer integer_ratio_multiply(arg1, arg2)
Pointer arg1, arg2;
{ if (scheme_zerop(arg1))
    return(arg1);
  else
  { Pointer narg, darg, gcdarg;
    narg = ratio_numer(arg2);
    darg = ratio_denom(arg2);
    gcdarg = scheme_gcd(arg1, darg);
    if (generic_equal(gcdarg, FIXNUM_1))
      return(p_make_ratio(generic_multiply(arg1, narg), darg));
    else
    { Pointer nn, nd;
      nn = generic_multiply(truncate(arg1, gcdarg), narg);
      nd = truncate(darg, gcdarg);
      if (generic_equal(nd, FIXNUM_1))
	return (nn);
      else
	return (p_make_ratio(nn, nd));
    }
  }
}

Pointer ratio_ratio_multiply(arg1, arg2)
Pointer arg1, arg2;
{ Pointer nn, nd;
  Pointer narg1 = ratio_numer(arg1);
  Pointer narg2 = ratio_numer(arg2);
  Pointer darg1 = ratio_denom(arg1);
  Pointer darg2 = ratio_denom(arg2);
  Pointer g1 = scheme_gcd(narg1, darg2);
  Pointer g2 = scheme_gcd(darg1, narg2);
  Pointer temp1, temp2;
  
  if (g1 == FIXNUM_1)
    temp1 = narg1;
  else temp1 = truncate(narg1, g1);
  if (g2 == FIXNUM_1)
    temp2 = narg2;
  else temp2 = truncate(narg2, g2);
  nn = scheme_multiply(temp1, temp2);
  
  if (g2 == FIXNUM_1)
    temp1 = darg1;
  else temp1 = truncate(darg1, g2);
  if (g1 == FIXNUM_1)
    temp2 = darg2;
  else temp2 = truncate(darg2, g1);
  nd = scheme_multiply(temp1, temp2);
  if (nd == FIXNUM_1)
    return(nn);
  else return(p_make_ratio(nn, nd));
}


Pointer complex_complex_multiply(arg1, arg2)
Pointer arg1, arg2;
{ Pointer rarg1 = complex_real_part(arg1);
  Pointer iarg1 = complex_imag_part(arg1);
  Pointer rarg2 = complex_real_part(arg2);
  Pointer iarg2 = complex_imag_part(arg2);
  return(make_complex(generic_minus(generic_multiply(rarg1, rarg2),
				    generic_multiply(iarg1, iarg2)),
		      generic_plus(generic_multiply(rarg1, iarg2),
				   generic_multiply(iarg1, rarg2))));
}

defop2_semantics(generic_multiply,

		 return(scheme_multiply(arg1, arg2)),
		 return(scheme_multiply(arg1, arg2)),
		 return(integer_ratio_multiply(arg1, arg2)),
		 return(normal_complex_multiply(arg1, arg2)),
		
		 return(scheme_multiply(arg1, arg2)),
		 return(scheme_multiply(arg1, arg2)),
		 return(scheme_multiply(arg1, reduce_ratio(arg2))),
		 return(normal_complex_multiply(arg1, arg2)),
		 
		 return(integer_ratio_multiply(arg2, arg1)),
		 return(scheme_multiply(reduce_ratio(arg1), arg2)),
		 return(ratio_ratio_multiply(arg1, arg2)),
		 return(normal_complex_multiply(arg1, arg2)),

		 return(normal_complex_multiply(arg2, arg1)),
		 return(normal_complex_multiply(arg2, arg1)),
		 return(normal_complex_multiply(arg2, arg1)),
		 return(complex_complex_multiply(arg1, arg2)));

Define_Primitive(Prim_generic_multiply, -1, "GENERIC-*")
{  make_multiarg_combiner_body(generic_multiply, FIXNUM_1);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-/                                          */
/*                                                        */
/**********************************************************/

Pointer normal_complex_divide(arg1, arg2)
Pointer arg1, arg2;
{ Pointer rarg2, iarg2, dn;
  rarg2 = complex_real_part(arg2);
  iarg2 = complex_imag_part(arg2);
  dn = generic_plus(generic_multiply(rarg2, rarg2),
		    generic_multiply(iarg2, iarg2));
  return(make_complex(generic_divide(generic_multiply(arg1, rarg2),
				     dn),
		      generic_divide(generic_minus(FIXNUM_0,
						   generic_multiply(arg1, iarg2)),
				     dn)));
}

Pointer complex_normal_divide(arg1, arg2)
Pointer arg1, arg2;
{ return(make_complex(generic_divide(complex_real_part(arg1),
				     arg2),
		      generic_divide(complex_imag_part(arg1),
				     arg2)));
}

Pointer complex_complex_divide(arg1, arg2)
Pointer arg1, arg2;
{ Pointer rarg1, iarg1, rarg2, iarg2, dn;
  rarg1 = complex_real_part(arg1);
  iarg1 = complex_imag_part(arg1);
  rarg2 = complex_real_part(arg2);
  iarg2 = complex_imag_part(arg2);
  dn = generic_plus(generic_multiply(rarg2, rarg2),
		    generic_multiply(iarg2, iarg2));
  return(make_complex(generic_divide(generic_plus(generic_multiply(rarg1, rarg2),
						  generic_multiply(iarg1, iarg2)),
				     dn),
		      generic_divide(generic_minus(generic_multiply(iarg1, rarg2),
						   generic_multiply(rarg1, iarg2)),
				     dn)));
}

Pointer integer_ratio_divide(arg1, arg2)
Pointer arg1, arg2;
{ if (scheme_zerop(arg1))
    return(arg1);
  else
  { Pointer narg2, darg2, gcdarg;
    narg2 = ratio_numer(arg2);
    darg2 = ratio_denom(arg2);
    gcdarg = scheme_gcd(arg1, narg2);
    if (generic_equal(gcdarg, FIXNUM_1))
    { Pointer nn = generic_multiply(arg1, darg2);
      if (generic_equal(narg2, FIXNUM_1))
	return nn;
      else
	return(p_make_ratio(nn, narg2));
    }
    else
    { Pointer nn, nd;
      nn = generic_multiply(truncate(arg1, gcdarg), darg2);
      nd = truncate(narg2, gcdarg);
      if (generic_equal(nd, FIXNUM_1))
	return (nn);
      else
	return (p_make_ratio(nn, nd));
    }
  }
}

Pointer ratio_integer_divide(arg1, arg2)
Pointer arg1, arg2;
{ Pointer narg1, darg1, gcdarg;
  narg1 = ratio_numer(arg1);
  darg1 = ratio_denom(arg1);
  gcdarg = scheme_gcd(narg1, arg2);
  if (scheme_zerop(arg2))
  { declare_error(GENERIC_ERROR_0_DIVIDE);
    return FIXNUM_0;
  }
  else if (scheme_equal(gcdarg, FIXNUM_1))
    return(p_make_ratio(narg1, generic_multiply(arg2, darg1)));
  else
    return(p_make_ratio(truncate(narg1, gcdarg),
			generic_multiply(truncate(arg2, gcdarg),
					 darg1)));
}

Pointer ratio_ratio_divide(arg1, arg2)
Pointer arg1, arg2;
{ Pointer nn, nd;
  Pointer narg1 = ratio_numer(arg1);
  Pointer narg2 = ratio_numer(arg2);
  Pointer darg1 = ratio_denom(arg1);
  Pointer darg2 = ratio_denom(arg2);
  Pointer g1 = scheme_gcd(narg1, narg2);
  Pointer g2 = scheme_gcd(darg1, darg2);
  Pointer temp1, temp2;
  
  if (g1 == FIXNUM_1)
    temp1 = narg1;
  else temp1 = truncate(narg1, g1);
  if (g2 == FIXNUM_1)
    temp2 = darg2;
  else temp2 = truncate(darg2, g2);
  nn = scheme_multiply(temp1, temp2);
  
  if (g2 == FIXNUM_1)
    temp1 = darg1;
  else temp1 = truncate(darg1, g2);
  if (g1 == FIXNUM_1)
    temp2 = narg2;
  else temp2 = truncate(darg2, g1);
  nd = scheme_multiply(temp1, temp2);
  if (nd == FIXNUM_1)
    return(nn);
  else return(p_make_ratio(nn, nd));
}

defop2_semantics(generic_divide,

		 return(make_ratio(arg1, arg2)),
		 return(scheme_divide(arg1, arg2)),
		 return(integer_ratio_divide(arg1, arg2)),
		 return(normal_complex_divide(arg1, arg2)),
		
		 return(scheme_divide(arg1, arg2)),
		 return(scheme_divide(arg1, arg2)),
		 return(scheme_divide(arg1, reduce_ratio(arg2))),
		 return(normal_complex_divide(arg1, arg2)),
		
		 return(ratio_integer_divide(arg1, arg2)),
		 return(scheme_divide(reduce_ratio(arg1), arg2)),
		 return(ratio_ratio_divide(arg1, arg2)),
		 return(normal_complex_divide(arg1, arg2)),

		 return(complex_normal_divide(arg1, arg2)),
		 return(complex_normal_divide(arg1, arg2)),
		 return(complex_normal_divide(arg1, arg2)),
		 return(complex_complex_divide(arg1, arg2)));

Define_Primitive(Prim_generic_divide, -1, "GENERIC-/")
{ Pointer result;
  int i;
  Primitive_Variable_Args();
  clear_errors();
  if (Number_Of_Args == 0)
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else if (Number_Of_Args == 1)
  { result = generic_divide(FIXNUM_1,
			    Primitive_Variable_Arg(1));
    if (generic_errorp())
    { if (is_generic_error(GENERIC_ERROR_2_ARG2_DISPATCH))
      { error_wrong_type_arg(1);
      }
      else if ((is_generic_error(GENERIC_ERROR_0_DIVIDE)) ||
	       (is_generic_error(GENERIC_ERROR_0_DENOM)))
      { error_bad_range_arg(1);
      }
      else
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    NAN_check(result);
    return(result);
  }
  else
  { result = Primitive_Variable_Arg(1);
    for (i = 2; i <= Number_Of_Args; i++)
    { result = generic_divide(result, Primitive_Variable_Arg(i));
      if (generic_errorp())
      { if (is_generic_error(GENERIC_ERROR_2_ARG2_DISPATCH))
	{ error_wrong_type_arg(i);
	}
        else if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
	{ error_wrong_type_arg(i-1);
	}
	else if ((is_generic_error(GENERIC_ERROR_0_DIVIDE)) ||
		 (is_generic_error(GENERIC_ERROR_0_DENOM)))
	{ error_bad_range_arg(i);
	}
	else
	{ Primitive_Error(ERR_EXTERNAL_RETURN);
	}
      }
    }
    NAN_check(result);
    return(result);
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-1+                                         */
/*                                                        */
/**********************************************************/
defop1_semantics(generic_one_plus,
		 
		 return(scheme_one_plus(arg1)),
		 return(scheme_one_plus(arg1)),
		 { Pointer darg = ratio_denom(arg1);
		   return(make_ratio(scheme_plus(ratio_numer(arg1),
						 darg),
				     darg));
		 },
	         return(make_complex(generic_plus(complex_real_part(arg1),
						  FIXNUM_1),
				     complex_imag_part(arg1))));

Define_Primitive(Prim_generic_one_plus, 1, "GENERIC-1+")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_one_plus(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-1-                                         */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_one_minus,
		 
		 return(scheme_minus_one_plus(arg1)),
		 return(scheme_minus_one_plus(arg1)),
		 { Pointer darg = ratio_denom(arg1);
		   return(make_ratio(scheme_minus(ratio_numer(arg1),
						  darg),
				     darg));
		 },
	         return(make_complex(generic_minus(complex_real_part(arg1),
						   FIXNUM_1),
				     complex_imag_part(arg1))));

Define_Primitive(Prim_generic_one_minus, 1, "GENERIC-1-")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_one_minus(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-CONJUGATE                                  */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_conjugate,
		 
		 return(arg1),
		 return(arg1),
		 return(arg1),
	         return(make_complex(complex_real_part(arg1),
				     generic_minus(FIXNUM_0,
						   complex_imag_part(arg1)))));

Define_Primitive(Prim_generic_conjugate, 1, "GENERIC-CONJUGATE")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_conjugate(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-GCD                                        */
/*                                                        */
/**********************************************************/

#define arg1_error()                                   \
  { declare_error(GENERIC_ERROR_2_ARG1_DISPATCH);      \
    return FIXNUM_0;                                   \
  }

#define arg2_error()                                   \
  { declare_error(GENERIC_ERROR_2_ARG2_DISPATCH);      \
    return FIXNUM_0;                                   \
  }

defop2_semantics(generic_gcd,
		 
		 return(scheme_gcd(arg1, arg2)),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error());

Define_Primitive(Prim_generic_gcd, -1, "GENERIC-GCD")
{ Pointer result;
  int i;
  Primitive_Variable_Args();
  clear_errors();
  if (Number_Of_Args == 0)
  { return(FIXNUM_0);
  }
  else if (Number_Of_Args == 1)
  { if (INTEGERP(Primitive_Variable_Arg(1)))
    { result = generic_abs(Primitive_Variable_Arg(1));
      if (generic_errorp())
      { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
	{ Primitive_Error(ERR_ARG_1_WRONG_TYPE);
	}
        else
	{ Primitive_Error(ERR_EXTERNAL_RETURN);
	}
      }
      else
      { return(result);
      }
    }
    else
    { error_wrong_type_arg(1);
    }
  }
  else
  { result = Primitive_Variable_Arg(1);
    for (i = 2; i <= Number_Of_Args; i++)
    { result = generic_gcd(result, Primitive_Variable_Arg(i));
      Variable_Two_Arg_Check(i-1, i);
    }
    return(result);
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-LCM                                        */
/*                                                        */
/**********************************************************/

defop2_semantics(generic_lcm,
		 
		 return(scheme_lcm(arg1, arg2)),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error(),

		 arg1_error(),
		 arg2_error(),
		 arg2_error(),
		 arg2_error());

Define_Primitive(Prim_generic_lcm, -1, "GENERIC-LCM")
{ Pointer result;
  int i;
  Primitive_Variable_Args();
  clear_errors();
  if (Number_Of_Args == 0)
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else if (Number_Of_Args == 1)
  { if (INTEGERP(Primitive_Variable_Arg(1)))
    { return(Primitive_Variable_Arg(1));
    }
    else
    { error_wrong_type_arg(1);
    }
  }
  else
  { result = Primitive_Variable_Arg(1);
    for (i = 2; i <= Number_Of_Args; i++)
    { result = generic_lcm(result, Primitive_Variable_Arg(i));
      Variable_Two_Arg_Check(i-1, i);
    }
    return(result);
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-EXP                                        */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_exp,
		 
		 return(scheme_exp(arg1)),
		 return(scheme_exp(arg1)),
		 return(scheme_exp(reduce_ratio(arg1))),
	         return(generic_multiply(generic_exp(complex_real_part(arg1)),
					 generic_cis(complex_imag_part(arg1)))));

Define_Primitive(Prim_generic_exp, 1, "GENERIC-EXP")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_exp(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-EXPT                                       */
/*                                                        */
/**********************************************************/

/* Raise a rational base to an integer power */

Pointer intexpt(base, power)
Pointer base, power;
{ if (generic_minusp(power))
    return(generic_divide(FIXNUM_1,
			  intexpt(base, 
				  generic_minus(FIXNUM_0, power))));
  else if (Type_Code(base) == TC_RATIO)
    return(generic_divide(intexpt(ratio_numer(base), power),
			  intexpt(ratio_denom(base), power)));
  else
  { Pointer total;
    Pointer nextn;

    if (generic_zerop(power))
      return FIXNUM_1;
    else if (generic_zerop(base))
    { declare_error(GENERIC_ERROR_ARG1);
      return FIXNUM_1;
    }
    else
    { nextn = integer_divide(power, FIXNUM_2);
      if (scheme_oddp(power))
	total = base;
      else
	total = FIXNUM_1;
    
      while(!(scheme_zerop(nextn)))
      { base = scheme_multiply(base, base);
	power = nextn;
	if (scheme_oddp(power))
	  total = scheme_multiply(base, total);
	nextn = integer_divide(power, FIXNUM_2);
      }
      return total;
    }
  }
}

/* Raise a real-non-ratio base to a real-non-ratio power */

Pointer nonintexpt(base, power)
{ Pointer result;
  if (generic_zerop(power))
  { result = FIXNUM_1;
    contagious(result, base);
    return result;
  }
  else if (generic_zerop(base))
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_1;
  }
  else
    return(scheme_exp(scheme_multiply(scheme_ln(base), power)));
}

/* Raise a complex base to a complex power */

Pointer c_complex_expt(base, power)
Pointer base, power;
{ Pointer result;
  if (generic_zerop(base))
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_1;
  }
  else
    return(generic_exp(generic_multiply(power, generic_log(base))));
}

/* Raise a complex base to a real power */

Pointer complex_expt(base, power)
Pointer base, power;
{ Pointer result;
  if (generic_zerop(power))
  { result = FIXNUM_1;
    contagious(result, base);
    return result;
  }
  else if (generic_zerop(base))
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_1;
  }
  else
    return(generic_multiply(generic_expt(complex_magnitude(base), power),
			    generic_cis(generic_multiply(generic_phase(base), power))));
}

defop2_semantics(generic_expt,
		 
		 return(intexpt(arg1, arg2)),
		 return(nonintexpt(arg1, arg2)),
		 return(intexpt(arg1, arg2)),
		 return(c_complex_expt(arg1, arg2)),

		 return(nonintexpt(arg1, arg2)),
		 return(nonintexpt(arg1, arg2)),
		 return(nonintexpt(arg1, reduce_ratio(arg2))),
		 return(c_complex_expt(arg1, arg2)),

		 return(intexpt(arg1, arg2)),
		 return(nonintexpt(reduce_ratio(arg1), arg2)),
		 return(intexpt(arg1, arg2)),
		 return(c_complex_expt(arg1, arg2)),

		 return(complex_expt(arg1, arg2)),		 
		 return(complex_expt(arg1, arg2)),
		 return(complex_expt(arg1, arg2)),
		 return(c_complex_expt(arg1, arg2)));

Define_Primitive(Prim_generic_expt, 2, "GENERIC-EXPT")
{ Pointer result;
  Primitive_2_Args();
  result = generic_expt(Arg1, Arg2);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_2_ARG2_DISPATCH))
    { Primitive_Error(ERR_ARG_2_WRONG_TYPE);
    }
    else if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  NAN_check(result);
  return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-LOG                                        */
/*                                                        */
/**********************************************************/

Pointer extended_log(arg1)
Pointer arg1;
{ if ((REALP(arg1)) && generic_minusp(arg1))
  { Pointer Pi;
    Store_Flonum_Result(pi, Pi);
    return(make_complex(scheme_ln(generic_minus(FIXNUM_0, arg1)),
			Pi));
  }
  else
    return scheme_ln(arg1);
}

defop1_semantics(generic_log,
		 
		 return(extended_log(arg1)),
		 return(extended_log(arg1)),
		 return(extended_log(reduce_ratio(arg1))),
	         return(make_complex(generic_log(complex_magnitude(arg1)),
				     complex_phase(arg1))));

Define_Primitive(Prim_generic_log, -1, "GENERIC-LOG")
{ Pointer result;
  Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else if (Number_Of_Args == 1)
    result = generic_log(Primitive_Variable_Arg(1));
  else
    result = generic_divide(generic_log(Primitive_Variable_Arg(1)),
			    generic_log(Primitive_Variable_Arg(2)));
  Variable_Two_Arg_Check(1, 2);
  NAN_check(result);
  return(result);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-SQRT                                       */
/*                                                        */
/**********************************************************/

Pointer extended_sqrt(arg1)
Pointer arg1;
{ if ((REALP(arg1)) && generic_minusp(arg1))
    return(make_complex(FIXNUM_0, scheme_sqrt(generic_minus(FIXNUM_0, arg1))));
  else
    return scheme_sqrt(arg1);
}

defop1_semantics(generic_sqrt,
		 
		 return(extended_sqrt(arg1)),
		 return(extended_sqrt(arg1)),
		 return(extended_sqrt(reduce_ratio(arg1))),
	         return(generic_multiply(generic_exp(generic_divide(generic_multiply(complex_phase(arg1),
										     make_complex(FIXNUM_0,
												  FIXNUM_1)),
								    FIXNUM_2)),
					 generic_sqrt(complex_magnitude(arg1)))));

Define_Primitive(Prim_generic_sqrt, 1, "GENERIC-SQRT")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_sqrt(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ISQRT                                      */
/*                                                        */
/**********************************************************/

/*++++ This should be redone someday ++++++*/

Define_Primitive(Prim_generic_isqrt, 1, "GENERIC-ISQRT")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  if (!((INTEGERP(Arg1)) && (scheme_positivep(Arg1))))
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  result = scheme_floor(generic_sqrt(Arg1));
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ABS                                        */
/*                                                        */
/**********************************************************/

Pointer complex_magnitude(z)
Pointer z;
{ Pointer r = complex_real_part(z);
  Pointer i = complex_imag_part(z);
  if (generic_zerop(r))
    return(generic_abs(i));
  else if (RATIONALP(r))
    return(generic_sqrt(generic_plus(generic_multiply(r, r),
				     generic_multiply(i, i))));
  else /* float case, optimize later */
    return(generic_sqrt(generic_plus(generic_multiply(r, r),
				     generic_multiply(i, i))));
}

Pointer default_abs(arg1)
Pointer arg1;
{ if (generic_minusp(arg1))
    return(generic_minus(FIXNUM_0, arg1));
  else return(arg1);
}

defop1_semantics(generic_abs,
		 
		 return(default_abs(arg1)),
		 return(default_abs(arg1)),
		 return(default_abs(arg1)),
	         return(complex_magnitude(arg1)));

Define_Primitive(Prim_generic_abs, 1, "GENERIC-ABS")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_abs(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
    return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-PHASE                                      */
/*                                                        */
/**********************************************************/

Pointer phase_rational(r)
Pointer r;
{ Pointer scheme_pi;
  Store_Flonum_Result(pi, scheme_pi);

  if (generic_minusp(r))
  { return scheme_pi;
  }
  else
  { Pointer result = FIXNUM_0;
    contagious(result, scheme_pi);
    return result;
  }
}

Pointer complex_phase(arg1)
Pointer arg1;
{ return(generic_atan(complex_imag_part(arg1),
		      complex_real_part(arg1)));
}

defop1_semantics(generic_phase,
		 
		 return(phase_rational(arg1)),
		 return(phase_rational(arg1)),
		 return(phase_rational(arg1)),
		 return(complex_phase(arg1)));

Define_Primitive(Prim_generic_phase, 1, "GENERIC-PHASE")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_phase(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-SIGNUM                                     */
/*                                                        */
/**********************************************************/

Pointer sig_check(r)
Pointer r;
{ if (generic_zerop(r))
    return FIXNUM_0;
  else if (generic_minusp(r))
    return FIXNUM_MINUS_1;
  else
    return FIXNUM_1;
}

defop1_semantics(generic_signum,
		 
		 return(sig_check(arg1)),
		 return(sig_check(arg1)),
		 return(sig_check(arg1)),
		 return(generic_divide(arg1, complex_magnitude(arg1))));

Define_Primitive(Prim_generic_signum, 1, "GENERIC-SIGNUM")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_signum(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
    return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-SIN                                        */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_sin,
		 
		 return(scheme_sin(arg1)),
		 return(scheme_sin(arg1)),
		 return(scheme_sin(reduce_ratio(arg1))),
	         { Pointer x = complex_real_part(arg1);
		   Pointer y = complex_imag_part(arg1);
		   return(make_complex(generic_multiply(generic_sin(x),
							generic_cosh(y)),
				       generic_multiply(generic_cos(x),
							generic_sinh(y))));
		 });

Define_Primitive(Prim_generic_sin, 1, "GENERIC-SIN")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_sin(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-COS                                        */
/*                                                        */
/**********************************************************/

defop1_semantics(generic_cos,
		 
		 return(scheme_cos(arg1)),
		 return(scheme_cos(arg1)),
		 return(scheme_cos(reduce_ratio(arg1))),
	         { Pointer x = complex_real_part(arg1);
		   Pointer y = complex_imag_part(arg1);
		   return(make_complex(generic_multiply(generic_cos(x),
							generic_cosh(y)),
				       generic_multiply(generic_sin(x),
							generic_sinh(y))));
		 });

Define_Primitive(Prim_generic_cos, 1, "GENERIC-COS")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_cos(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-TAN                                        */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_tan, 1, "GENERIC-TAN")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_divide(generic_sin(Arg1),
			  generic_cos(Arg1));
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-CIS                                        */
/*                                                        */
/**********************************************************/

Pointer generic_cis(theta)
Pointer theta;
{ if (REALP(theta))
  { return(make_complex(generic_cos(theta), generic_sin(theta)));
  }
  else
  { declare_error(GENERIC_ERROR_1_ARG1_DISPATCH);
    return FIXNUM_0;
  }
}

Define_Primitive(Prim_generic_cis, 1, "GENERIC-CIS")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_cis(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ASIN                                       */
/*                                                        */
/**********************************************************/

Pointer scheme_asin(x)
Pointer x;
{ Pointer result;
  Pointer sq = generic_multiply(x, x);
  if (generic_equal(sq, FIXNUM_1))
  { Store_Flonum_Result(pi_by_2, result);
    if (generic_plusp(x))
      return result;
    else
      return(generic_minus(FIXNUM_0, result));
  }
  else if (generic_greaterthan(sq, FIXNUM_1))
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
  else if (generic_plusp(x))
  { result = generic_divide(x, 
			    generic_sqrt(generic_minus(FIXNUM_1,
						       sq)));
    if (Type_Code(result) == TC_RATIO)
      result = reduce_ratio(result);
    return scheme_atan(result);
  }
  else
  { result = generic_divide(generic_minus(FIXNUM_0, x),
			    generic_sqrt(generic_minus(FIXNUM_1, sq)));
    if (Type_Code(result) == TC_RATIO)
      result = reduce_ratio(result);
    return generic_minus(FIXNUM_0, scheme_atan(result));
  }
}

Boolean in_asin_domain(z)
Pointer z;
{ Pointer scheme_pi_by_2;
  Store_Flonum_Result(pi_by_2, scheme_pi_by_2);
  if ((generic_lessthan(generic_minus(FIXNUM_0, scheme_pi_by_2),
			complex_real_part(z))) &&
      (generic_lessthan(complex_real_part(z),
			scheme_pi_by_2)))
  { return true;
  }
  else if (generic_equal(complex_real_part(z),
			 generic_minus(FIXNUM_0, scheme_pi_by_2)))
  { if (generic_greaterequalthan(complex_imag_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else if (generic_equal(complex_real_part(z), scheme_pi_by_2))
  { if (generic_lessequalthan(complex_imag_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else return false;
}

#define I() p_make_complex(FIXNUM_0, FIXNUM_1)

Pointer multbyi(z)
Pointer z;
{ return(generic_multiply(z, I()));
}

Pointer complex_asin(z)
Pointer z;
{ if (in_asin_domain(z))
  { Pointer i = multbyi(z);
    return (generic_minus(FIXNUM_0,
			  multbyi(generic_log(generic_plus(multbyi(z),
							   generic_sqrt(generic_minus(FIXNUM_1,
										      generic_multiply(z, z))))))));
  }
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

defop1_semantics(generic_asin,
		 
		 return(scheme_asin(arg1)),
		 return(scheme_asin(arg1)),
		 return(scheme_asin(reduce_ratio(arg1))),
		 return(complex_asin(arg1)));

Define_Primitive(Prim_generic_asin, 1, "GENERIC-ASIN")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_asin(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else if (is_generic_error(GENERIC_ERROR_ARG1))
    { Primitive_Error(ERR_ARG_1_BAD_RANGE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ACOS                                       */
/*                                                        */
/**********************************************************/

Pointer scheme_acos(x)
Pointer x;
{ Pointer result;
  Store_Flonum_Result(pi_by_2, result);
  if (generic_zerop(x))
  { return result;
  }
  else 
  { Pointer sq = generic_multiply(x, x);
    if (generic_greaterthan(sq, FIXNUM_1))
    { declare_error(GENERIC_ERROR_ARG1);
      return FIXNUM_0;
    }
    else if (generic_plusp(x))
    { result = generic_divide(generic_sqrt(generic_minus(FIXNUM_1, sq)),
			      x);
      if (Type_Code(result) == TC_RATIO)
	result = reduce_ratio(result);
      return scheme_atan(result);
    }
    else
    { Pointer scheme_pi;
      Store_Flonum_Result(pi, scheme_pi);
      result = generic_divide(generic_sqrt(generic_minus(FIXNUM_1, sq)),
			      generic_minus(FIXNUM_0, x));
      if (Type_Code(result) == TC_RATIO)
	result = reduce_ratio(result);
      return(generic_minus(scheme_pi, scheme_atan(result)));
    }
  }
}

Boolean in_acos_domain(z)
Pointer z;
{ Pointer scheme_pi;
  Store_Flonum_Result(pi, scheme_pi);
  if ((generic_lessthan(FIXNUM_0, complex_real_part(z))) &&
      (generic_lessthan(complex_real_part(z), scheme_pi)))
  { return true;
  }
  else if (generic_equal(complex_real_part(z), FIXNUM_0))
  { if (generic_greaterequalthan(complex_imag_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else if (generic_equal(complex_real_part(z), scheme_pi))
  { if (generic_lessequalthan(complex_imag_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else return false;
}

Pointer complex_acos(z)
Pointer z;
{ if (in_acos_domain(z))
  { return(generic_minus(FIXNUM_0,
			 multbyi(generic_log(generic_plus(z,
							  multbyi(generic_sqrt(generic_minus(FIXNUM_1,
											     generic_multiply(z, z)))))))));
  }
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

defop1_semantics(generic_acos,
		 
		 return(scheme_acos(arg1)),
		 return(scheme_acos(arg1)),
		 return(scheme_acos(reduce_ratio(arg1))),
		 return(complex_acos(arg1)));

Define_Primitive(Prim_generic_acos, 1, "GENERIC-ACOS")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  result = generic_acos(Arg1);
  if (generic_errorp())
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    else if (is_generic_error(GENERIC_ERROR_ARG1))
    { Primitive_Error(ERR_ARG_1_BAD_RANGE);
    }
    else
    { Primitive_Error(ERR_EXTERNAL_RETURN);
    }
  }
  else
  { NAN_check(result);
    return result;
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ATAN                                       */
/*                                                        */
/**********************************************************/

Pointer scheme_atan2(Y, X)
Pointer Y, X;
{ if (generic_zerop(X))
  { Pointer scheme_pi_by_2;
    Store_Flonum_Result(pi_by_2, scheme_pi_by_2);
    if (generic_zerop(Y))
    { declare_error(GENERIC_ERROR_ARG1);
      return FIXNUM_1;
    }
    else if (generic_minusp(Y))
      return(generic_minus(FIXNUM_0, scheme_pi_by_2));
    else return scheme_pi_by_2;
  }
  else 
  { Pointer scheme_pi;
    Pointer atan1 = generic_divide(Y, X);
    if (Type_Code(atan1) == TC_RATIO)
      atan1 = reduce_ratio(atan1);
    atan1 = scheme_atan(atan1);
    Store_Flonum_Result(pi, scheme_pi);
    if (generic_plusp(X))
      return(atan1);
    else if (generic_minusp(Y))
      return (generic_minus(atan1, scheme_pi));
    else
      return (generic_plus(atan1, scheme_pi));
  }
}

Boolean in_atan_domain(z)
Pointer z;
{ Pointer scheme_pi_by_2;
  Store_Flonum_Result(pi_by_2, scheme_pi_by_2);
  if ((generic_lessthan(generic_minus(FIXNUM_0, scheme_pi_by_2),
			complex_real_part(z))) &&
      (generic_lessthan(complex_real_part(z),
			scheme_pi_by_2)))
  { return true;
  }
  else if (generic_equal(complex_real_part(z),
			 generic_minus(FIXNUM_0, scheme_pi_by_2)))
  { if (generic_plusp(complex_imag_part(z)))
      return true;
    else return false;
  }
  else if (generic_equal(complex_real_part(z), scheme_pi_by_2))
  { if (generic_minusp(complex_imag_part(z)))
      return true;
    else return false;
  }
  else return false;
}

Pointer complex_atan(z)
Pointer z;
{ if (in_atan_domain(z))
  { Pointer i = multbyi(z);
    return(generic_minus(FIXNUM_0,
			 multbyi(generic_multiply(p_make_ratio(FIXNUM_1, FIXNUM_2),
						  generic_log(generic_divide(generic_plus(FIXNUM_1, i),
									     generic_minus(FIXNUM_1, i)))))));
  }
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

#define complex_atan_error(X) { declare_error(X);   \
				return FIXNUM_0;    \
			      }

defop2_semantics(generic_atan,
		 
		 return(scheme_atan2(arg1, arg2)),
		 return(scheme_atan2(arg1, arg2)),
		 return(scheme_atan2(arg1, reduce_ratio(arg2))),
		 complex_atan_error(GENERIC_ERROR_2_ARG2_DISPATCH),

		 return(scheme_atan2(arg1, arg2)),
		 return(scheme_atan2(arg1, arg2)),
		 return(scheme_atan2(arg1, reduce_ratio(arg2))),
		 complex_atan_error(GENERIC_ERROR_2_ARG2_DISPATCH),

		 return(scheme_atan2(reduce_ratio(arg1), arg2)),
		 return(scheme_atan2(reduce_ratio(arg1), arg2)),
		 return(scheme_atan2(reduce_ratio(arg1), reduce_ratio(arg2))),
		 complex_atan_error(GENERIC_ERROR_2_ARG2_DISPATCH),

		 complex_atan_error(GENERIC_ERROR_2_ARG1_DISPATCH),
		 complex_atan_error(GENERIC_ERROR_2_ARG1_DISPATCH),
		 complex_atan_error(GENERIC_ERROR_2_ARG1_DISPATCH),
		 complex_atan_error(GENERIC_ERROR_2_ARG1_DISPATCH));

#define atan_error_check()                                \
  if (generic_errorp())                                   \
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))  \
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);              \
    }                                                     \
    else if (is_generic_error(GENERIC_ERROR_ARG1))        \
    { Primitive_Error(ERR_ARG_1_BAD_RANGE);               \
    }                                                     \
    else                                                  \
    { Primitive_Error(ERR_EXTERNAL_RETURN);               \
    }                                                     \
  }

Define_Primitive(Prim_generic_atan, -1, "GENERIC-ATAN")
{ Pointer result;
  Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else
  { clear_errors();
    if (Number_Of_Args == 1)
    { if (COMPLEXP(Primitive_Variable_Arg(1)))
	result = complex_atan(Primitive_Variable_Arg(1));
      else
	result = scheme_atan(Primitive_Variable_Arg(1));
      atan_error_check();
      NAN_check(result);
      return(result);
    }
    else
    { result = generic_atan(Primitive_Variable_Arg(1),
			    Primitive_Variable_Arg(2));
      atan_error_check();
      NAN_check(result);
      return(result);
    }
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-PI                                         */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_pi, 0, "GENERIC-PI")
{ Pointer result;

  Store_Flonum_Result(pi, result);
  return(result);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-SINH                                       */
/*                                                        */
/**********************************************************/

#define hyperbolic_error_check()                          \
  if (generic_errorp())                                   \
  { if (is_generic_error(GENERIC_ERROR_1_ARG1_DISPATCH))  \
    { Primitive_Error(ERR_ARG_1_WRONG_TYPE);              \
    }                                                     \
    else if (is_generic_error(GENERIC_ERROR_ARG1))        \
    { Primitive_Error(ERR_ARG_1_BAD_RANGE);               \
    }                                                     \
    else                                                  \
    { Primitive_Error(ERR_EXTERNAL_RETURN);               \
    }                                                     \
  }

Pointer generic_sinh(Arg1)
Pointer Arg1;
{ Pointer z = generic_exp(Arg1);
  return(generic_divide(generic_minus(z, generic_divide(FIXNUM_1, z)),
			FIXNUM_2));
}

Define_Primitive(Prim_generic_sinh, 1, "GENERIC-SINH")
{ Pointer result;
  Primitive_1_Arg();

  result = generic_sinh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return(result);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-COSH                                       */
/*                                                        */
/**********************************************************/

Pointer generic_cosh(Arg1)
Pointer Arg1;
{ Pointer z = generic_exp(Arg1);
  return(generic_divide(generic_plus(z, generic_divide(FIXNUM_1, z)),
			FIXNUM_2));
}

Define_Primitive(Prim_generic_cosh, 1, "GENERIC-COSH")
{ Pointer result;
  Primitive_1_Arg();

  result = generic_cosh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return(result);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-TANH                                       */
/*                                                        */
/**********************************************************/

Pointer generic_tanh(Arg1)
Pointer Arg1;
{ Pointer z, y;
  z = generic_exp(generic_multiply(Arg1, FIXNUM_2));
  y = generic_divide(FIXNUM_1, z);
  return(generic_minus(generic_divide(FIXNUM_1, generic_plus(FIXNUM_1, y)),
		       generic_divide(FIXNUM_1, generic_plus(FIXNUM_1, z))));
}

Define_Primitive(Prim_generic_tanh, 1, "GENERIC-TANH")
{ Pointer result;
  Primitive_1_Arg();

  result = generic_tanh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return(result);
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ASINH                                      */
/*                                                        */
/**********************************************************/
Boolean in_asinh_domain(z)
Pointer z;
{ Pointer scheme_pi_by_2;
  Store_Flonum_Result(pi_by_2, scheme_pi_by_2);
  if ((generic_lessthan(generic_minus(FIXNUM_0, scheme_pi_by_2),
			complex_imag_part(z))) &&
      (generic_lessthan(complex_imag_part(z),
			scheme_pi_by_2)))
  { return true;
  }
  else if (generic_equal(complex_imag_part(z),
			 generic_minus(FIXNUM_0, scheme_pi_by_2)))
  { if (generic_lessequalthan(complex_real_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else if (generic_equal(complex_imag_part(z), scheme_pi_by_2))
  { if (generic_greaterequalthan(complex_real_part(z), FIXNUM_0))
      return true;
    else return false;
  }
  else return false;
}

Pointer asinh(x)
Pointer x;
{ return(generic_log(generic_plus(x,
				  generic_sqrt(generic_plus(generic_multiply(x, x), 
							    FIXNUM_1)))));
}

Pointer complex_asinh(x)
Pointer x;
{ if (in_asinh_domain(x))
    return(asinh(x));
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

Define_Primitive(Prim_generic_asinh, 1, "GENERIC-ASINH")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  if (COMPLEXP(Arg1))
    result = complex_asinh(Arg1);
  else
    result = asinh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ACOSH                                      */
/*                                                        */
/**********************************************************/

Boolean in_acosh_domain(z)
Pointer z;
{ Pointer scheme_pi;
  Store_Flonum_Result(pi, scheme_pi);
  if (generic_plusp(complex_imag_part(z)))
  { if ((generic_lessequalthan(generic_minus(FIXNUM_0, scheme_pi),
			       z)) &&
	(generic_lessthan(z, scheme_pi)))
      return true;
   else return false;
  }
  else if (generic_equal(complex_real_part(z), FIXNUM_0))
  { if (generic_lessequalthan(complex_imag_part(z), scheme_pi))
      return true;
    else return false;
  }
  else return false;
}

Pointer acosh(x)
Pointer x;
{ return(generic_log(generic_plus(x, generic_sqrt(generic_minus(generic_multiply(x, x),
								FIXNUM_1)))));
}

Pointer real_acosh(x)
Pointer x;
{ if (generic_plusp(x))
    return(acosh(x));
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

Pointer complex_acosh(x)
Pointer x;
{ if (in_acosh_domain(x))
    return(acosh(x));
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}


Define_Primitive(Prim_generic_acosh, 1, "GENERIC-ACOSH")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  if (COMPLEXP(Arg1))
    result = complex_acosh(Arg1);
  else
    result = real_acosh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ATANH                                      */
/*                                                        */
/**********************************************************/

Boolean in_atanh_domain(z)
Pointer z;
{ Pointer scheme_pi_by_2;
  Store_Flonum_Result(pi_by_2, scheme_pi_by_2);
  if ((generic_lessthan(generic_minus(FIXNUM_0, scheme_pi_by_2),
			complex_imag_part(z))) &&
      (generic_lessthan(complex_imag_part(z),
			scheme_pi_by_2)))
  { return true;
  }
  else if (generic_equal(complex_imag_part(z),
			 generic_minus(FIXNUM_0, scheme_pi_by_2)))
  { if (generic_minusp(complex_real_part(z)))
      return true;
    else return false;
  }
  else if (generic_equal(complex_imag_part(z), scheme_pi_by_2))
  { if (generic_plusp(complex_real_part(z)))
      return true;
    else return false;
  }
  else return false;
}

Pointer atanh(x)
Pointer x;
{ return(generic_divide(generic_log(generic_divide(generic_plus(x, FIXNUM_1),
						   generic_minus(FIXNUM_1, x))),
			FIXNUM_2));
}

Pointer real_atanh(x)
Pointer x;
{ if ((generic_lessthan(FIXNUM_MINUS_1, x)) &&
      (generic_lessthan(x, FIXNUM_1)))
    return(atanh(x));
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

Pointer complex_atanh(x)
Pointer x;
{ if (in_atanh_domain(x))
    return(acosh(x));
  else
  { declare_error(GENERIC_ERROR_ARG1);
    return FIXNUM_0;
  }
}

Define_Primitive(Prim_generic_atanh, 1, "GENERIC-ATANH")
{ Pointer result;
  Primitive_1_Arg();
  clear_errors();
  if (COMPLEXP(Arg1))
    result = complex_atanh(Arg1);
  else
    result = real_atanh(Arg1);
  hyperbolic_error_check();
  NAN_check(result);
  return result;
}

/**********************************************************/
/*                                                        */
/*     GENERIC-FLOAT                                      */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_float, -1, "GENERIC-FLOAT")
{ Pointer result;
  Primitive_Variable_Args();
  
  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else if ((COMPLEXP(Primitive_Variable_Arg(1))) ||
	   !(GENERIC_NUMBERP(Primitive_Variable_Arg(1))))
  { error_wrong_type_arg(1);
  }
  else if ((Number_Of_Args == 2) &&
	   (Type_Code(Primitive_Variable_Arg(2)) != TC_BIG_FLONUM))
  { error_wrong_type_arg(2);
  }
  else
  { result = coerce_to_float(Primitive_Variable_Arg(1));
    if (generic_errorp())
    { if (is_generic_error(GENERIC_ERROR_ARG1_COERCION))
      { Primitive_Error(ERR_ARG_1_FAILED_COERCION);
      }
      else
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    else
    { return(result);
    }
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-RATIONAL                                   */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_rational, 1, "GENERIC-RATIONAL")
{ Primitive_1_Arg();
  
  if (Type_Code(Arg1) == TC_BIG_FLONUM)
  { Pointer f, e, s;
    f = integer_decode_mantissa(Arg1);
    e = integer_decode_exponent(Arg1);
    s = integer_decode_sign(Arg1);
    if (scheme_positivep(e))
      return(scheme_multiply(s,
			     scheme_multiply(f,
					     generic_expt(FIXNUM_2,
							  e))));
    else
      return(make_ratio(scheme_multiply(s, f),
			generic_expt(FIXNUM_2,
				     scheme_minus(FIXNUM_0, e))));
  }
  else if (REALP(Arg1))
    return Arg1;
  else
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-RATIONALIZE                                */
/*                                                        */
/**********************************************************/

/* This Gosper code was not meant to be understood */

Pointer rationalize(x)
Pointer x;
{ if (scheme_negativep(x))
    return(generic_minus(FIXNUM_0, 
			 rationalize(scheme_minus(FIXNUM_0, x))));
  else if (scheme_zerop(x))
    return FIXNUM_0;
  else
  { Pointer y, a;
    Pointer oden, onum, den, num, xx;
    Pointer epsilon = get_float_parameter(FIXNUM_3, FIXNUM_4);
    
    clear_errors();
    xx = x;
    a = scheme_truncate(x);
    num = a;
    den = FIXNUM_1;
    onum = FIXNUM_1;
    oden = FIXNUM_0;
    
    while((scheme_zerop(den)) ||
	  (scheme_greaterthan(generic_abs(scheme_divide(scheme_minus(x,
								     scheme_divide(coerce_to_float(num),
										   coerce_to_float(den))),
							  x)),
			       epsilon)))
    { Pointer tempn = num;
      Pointer tempd = den;
      y = scheme_divide(coerce_to_float(FIXNUM_1),
			scheme_minus(xx, coerce_to_float(a)));
      xx = y;
      a = scheme_truncate(y);
      num = scheme_plus(scheme_multiply(a, num), onum);
      den = scheme_plus(scheme_multiply(a, den), oden);
      onum = tempn;
      oden = tempd;
      if (generic_errorp())
      { Primitive_Error(ERR_EXTERNAL_RETURN);
      }
    }
    return(make_ratio(num, den));
  }
}

Define_Primitive(Prim_generic_rationalize, 1, "GENERIC-RATIONALIZE")
{ Primitive_1_Arg();
  
  if (Type_Code(Arg1) == TC_BIG_FLONUM)
    return rationalize(Arg1);
  else if (REALP(Arg1))
    return Arg1;
  else
  { Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-FLOOR                                      */
/*                                                        */
/**********************************************************/

Pointer reduce_if_necessary(div)
Pointer div;
{ if (Type_Code(div) == TC_RATIO)
  { return(scheme_integer_divide(ratio_numer(div),
				 ratio_denom(div)));
  }
  else if (GENERIC_NUMBERP(div))
    return div;
  else
  { declare_error(GENERIC_ERROR_2_ARG1_DISPATCH);
    return FIXNUM_0;
  }
}

Pointer generic_floor_divide(Arg1, Arg2)
Pointer Arg1, Arg2;
{ return(reduce_if_necessary(generic_divide(Arg1, Arg2)));
}

#define Floor_Check()                                        \
if ((!(generic_zerop(rem))) && generic_minusp(num))          \
{ num = generic_minus(num, FIXNUM_1);                        \
}                                                            \
else if (generic_minusp(rem) && generic_zerop(num))          \
{ num = FIXNUM_MINUS_1;                                      \
}                                                            \
rem = generic_minus(Arg1,  generic_multiply(num, Arg2));

Define_Primitive(Prim_generic_floor, -1, "GENERIC-FLOOR")
{ Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else 
  { Pointer num, rem, div, Arg1, Arg2;
    clear_errors();
    if (Number_Of_Args == 2)
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = Primitive_Variable_Arg(2);
      if (COMPLEXP(Arg2))
      { error_wrong_type_arg(2);
      }
      div = generic_floor_divide(Arg1, Arg2);
    }
    else 
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = FIXNUM_1;
      div = reduce_if_necessary(Arg1);
    }
    if (COMPLEXP(div))
    { error_wrong_type_arg(1);
    }
    if (Type_Code(div) == TC_LIST)
    { num = Vector_Ref(div, CONS_CAR);
      rem = Vector_Ref(div, CONS_CDR);
      Floor_Check();
    }
    else
    { num = scheme_floor(div); 
      rem = generic_minus(Arg1,  generic_multiply(num, Arg2));
    }
    Variable_Two_Arg_Check(1, 2);
    Multiple_Value_Return(Number_Of_Args,
			  2,
			  num,
			  { Push(rem);
			    Push(num);
			  });
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-CEILING                                    */
/*                                                        */
/**********************************************************/

#define Ceiling_Check()                                      \
if ((!(generic_zerop(rem))) && generic_plusp(num))           \
{ num = generic_plus(num, FIXNUM_1);                         \
}                                                            \
rem = generic_minus(Arg1,  generic_multiply(num, Arg2));

Define_Primitive(Prim_generic_ceiling, -1, "GENERIC-CEILING")
{ Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else 
  { Pointer num, rem, div, Arg1, Arg2;
    clear_errors();
    if (Number_Of_Args == 2)
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = Primitive_Variable_Arg(2);
      if (COMPLEXP(Arg2))
      { error_wrong_type_arg(2);
      }
      div = generic_floor_divide(Arg1, Arg2);
    }
    else 
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = FIXNUM_1;
      div = reduce_if_necessary(Arg1);
    }
    if (COMPLEXP(div))
    { error_wrong_type_arg(1);
    }
    if (Type_Code(div) == TC_LIST)
    { num = Vector_Ref(div, CONS_CAR);
      rem = Vector_Ref(div, CONS_CDR);
      Ceiling_Check();
    }
    else
    { num = scheme_ceiling(div); 
      rem = generic_minus(div, num);
    }
    Variable_Two_Arg_Check(1, 2);
    Multiple_Value_Return(Number_Of_Args,
			  2,
			  num,
			  { Push(rem);
			    Push(num);
			  });
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-TRUNCATE                                   */
/*                                                        */
/**********************************************************/

#define Truncate_Check()                                   \
rem = generic_minus(Arg1,  generic_multiply(num, Arg2));

Define_Primitive(Prim_generic_truncate, -1, "GENERIC-TRUNCATE")
{ Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else 
  { Pointer num, rem, div, Arg1, Arg2;
    clear_errors();
    if (Number_Of_Args == 2)
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = Primitive_Variable_Arg(2);
      if (COMPLEXP(Arg2))
      { error_wrong_type_arg(2);
      }
      div = generic_floor_divide(Arg1, Arg2);
    }
    else 
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = FIXNUM_1;
      div = reduce_if_necessary(Arg1);
    }
    if (COMPLEXP(div))
    { error_wrong_type_arg(1);
    }
    if (Type_Code(div) == TC_LIST)
    { num = Vector_Ref(div, CONS_CAR);
      rem = Vector_Ref(div, CONS_CDR);
      Truncate_Check();
    }
    else
    { num = scheme_truncate(div); 
      rem = generic_minus(div, num);
    }
    Variable_Two_Arg_Check(1, 2);
    Multiple_Value_Return(Number_Of_Args,
			  2,
			  num,
			  { Push(rem);
			    Push(num);
			  });
  }
}

/**********************************************************/
/*                                                        */
/*     GENERIC-ROUND                                      */
/*                                                        */
/**********************************************************/

Define_Primitive(Prim_generic_round, -1, "GENERIC-ROUND")
{ Primitive_Variable_Args();

  if ((Number_Of_Args == 0) || (Number_Of_Args > 2))
  { Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  else 
  { Pointer num, rem, div, Arg1, Arg2;
    clear_errors();
    if (Number_Of_Args == 2)
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = Primitive_Variable_Arg(2);
      if (COMPLEXP(Arg2))
      { error_wrong_type_arg(2);
      }
      div = generic_floor_divide(Arg1, Arg2);
    }
    else 
    { Arg1 = Primitive_Variable_Arg(1);
      Arg2 = FIXNUM_1;
      div = reduce_if_necessary(Arg1);
    }
    if (COMPLEXP(div))
    { error_wrong_type_arg(1);
    }
    if (Type_Code(div) == TC_LIST)
    { num = Vector_Ref(div, CONS_CAR);
      rem = Vector_Ref(div, CONS_CDR);
    }
    else
    { num = scheme_round(div); 
      rem = generic_minus(div, num);
    }
    if ((generic_equal(generic_abs(rem), p_make_ratio(FIXNUM_1, FIXNUM_2))) &&
	(generic_equal(generic_abs(remainder(num, FIXNUM_2)),
		       FIXNUM_1)))
    { if (generic_plusp(num))
	num = generic_minus(num, FIXNUM_1);
      else num = generic_plus(num, FIXNUM_1);
    }
    rem = generic_minus(Arg1,  generic_multiply(num, Arg2));
    Variable_Two_Arg_Check(1, 2);
    Multiple_Value_Return(Number_Of_Args,
			  2,
			  num,
			  { Push(rem);
			    Push(num);
			  });
  }
}
