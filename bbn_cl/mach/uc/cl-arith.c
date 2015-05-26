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

/*
  See kcl source num_co.c for some good hints on implementing this stuff with various
  floating formats.

  Also see Motorola 68881 Manual.
*/

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "zones.h"
#include "bignum.h"
#include "stringprim.h"
#include "character.h"
#include <math.h>

#ifdef sun
#define FLOAT_BITS                                  64
#define FLOAT_BIGDIGITS                             (FLOAT_BITS / SHIFT)    /* Assumed a multiple of bigdigit size */
#define FRACTION_BITS                               52
#define FRACTION_BIGDIGITS                          ((FRACTION_BITS + SHIFT - 1) / SHIFT)
#define LEFTOVER_BITS                               (FRACTION_BITS % SHIFT)
#define LEFTOVER_BIT_MASK                           ((1 << LEFTOVER_BITS) - 1)
#define defined_stuff
#endif

#ifdef vax
#define FLOAT_BITS                                  64
#define FLOAT_BIGDIGITS                             (FLOAT_BITS / SHIFT)    /* Assumed a multiple of bigdigit size */
#define FRACTION_BITS                               55
#define FRACTION_BIGDIGITS                          ((FRACTION_BITS + SHIFT - 1) / SHIFT)
#define LEFTOVER_BITS                               (FRACTION_BITS % SHIFT)
#define LEFTOVER_BIT_MASK                           ((1 << LEFTOVER_BITS) - 1)
#define defined_stuff
#endif

#ifdef butterfly
#define FLOAT_BITS                                  64
#define FLOAT_BIGDIGITS                             (FLOAT_BITS / SHIFT)    /* Assumed a multiple of bigdigit size */
#define FRACTION_BITS                               52
#define FRACTION_BIGDIGITS                          ((FRACTION_BITS + SHIFT - 1) / SHIFT)
#define LEFTOVER_BITS                               (FRACTION_BITS % SHIFT)
#define LEFTOVER_BIT_MASK                           ((1 << LEFTOVER_BITS) - 1)
#define defined_stuff
#undef max
#undef min
#define abs(x) (((x) >= 0) ? (x) : (-(x)))
#endif

#ifndef defined_stuff
#include "You forgot to define stuff."
#endif

#define MOST_POS               0
#define MOST_NEG               1
#define LEAST_POS              2
#define LEAST_NEG              3
#define EPSILON                4
#define NEG_EPSILON            5

#define SHORT_FLOAT_TYPE       0
#define SINGLE_FLOAT_TYPE      1
#define DOUBLE_FLOAT_TYPE      2
#define LONG_FLOAT_TYPE        3

/*
  Returns a fixnum or bignum given an array of len bigdigits.
  Array is assumed properly zeroed at the top.
*/

Pointer blk_to_num(block,len)
bigdigit *block;
long len;

{
  bigdigit *res, *p;
  long length;
  long fixans;
  long blen;

  fixans = block_fixnum_range(block,len*sizeof(bigdigit));   
  if (fixans != -1) return Make_Non_Pointer(TC_FIXNUM,fixans);
  p = block;
  blen = len;
  while (*p++ == 0) blen--;
  length = Align(blen);
  block += len - 1;
  Primitive_GC_If_Needed(length);
  res = BIGNUM(Free);
  Prepare_Header(res,blen,POSITIVE);
  for (p = Bignum_Bottom(res); blen != 0; blen--) *p++ = *block--;
  Free += length;
  return (Make_Pointer(TC_BIG_FIXNUM, Free - length));
}

/*
  Given a block and its byte len, returns untagged fixnum result
  if block is in fixnum range, else -1
*/

Pointer integer_decode_mantissa(Arg1)
Pointer Arg1;
{ double x; 
  bigdigit block[FRACTION_BIGDIGITS], *p;
  long i;
  
  x = Get_Float(Arg1);
  p = (bigdigit *) &x;
  p += FRACTION_BIGDIGITS - 1;
  for (i=FRACTION_BIGDIGITS-1 ; i >= 0 ; i--) block[i] = *p--;
  i += 1;
  block[i] &= LEFTOVER_BIT_MASK;
  block[i] |= (1 << LEFTOVER_BITS);             /* to set the upper, implicit 1 in 1.f */
  return blk_to_num(block,FRACTION_BIGDIGITS);
}

Define_Primitive(prim_idf_mant, 1, "INTEGER-DECODE-FLOAT-MANTISSA")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  return integer_decode_mantissa(Arg1);
}

block_fixnum_range(block,byte_len)
unsigned char *block;
long byte_len;

{
  long i, non_fixnum_part_len, r;

  non_fixnum_part_len = byte_len - sizeof(Pointer);
  for (i=0 ; i<non_fixnum_part_len ; i++) if (*block++ != 0) return(-1);
  r = 0;
  for (i=non_fixnum_part_len ; i < sizeof(Pointer) ; i++)
    {
      r <<= CHAR_SIZE;
      r += *block++;
    }
  if ((r & SIGN_MASK) != 0) return(-1);
  return (r & BIGGEST_FIXNUM);
}

/* 
  Given a long int x, returns either a fixnum or a bignum.
*/

Pointer c_int_to_scheme_int(x)
long x;
{
  bigdigit *b;

  b = (bigdigit *)&x;
  return blk_to_num(b,sizeof(long)/sizeof(bigdigit));
}

Pointer integer_decode_exponent(Arg1)
{ double x; 
  long high;

  x = Get_Float(Arg1);
  high = *((int *) &x);

#ifdef sun
  return Make_Non_Pointer(TC_FIXNUM,((high >> 20) & 0x7ff) - 1022 - 53);
#endif
#ifdef vax
  return Make_Non_Pointer(TC_FIXNUM,((high >> 7) & 0xff) - 128 - 56);
#endif
}

Define_Primitive(prim_idf_exp, 1, "INTEGER-DECODE-FLOAT-EXPONENT")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  return integer_decode_exponent(Arg1);
}

Pointer integer_decode_sign(Arg1)
{ double x; 
  long high;
  long res;

  x = Get_Float(Arg1);
  high = *((int *) &x);
  if (high < 0) res = -1;
  else res = 1;

  return Make_Non_Pointer(TC_FIXNUM,(res & ADDRESS_MASK));
}

Define_Primitive(prim_decode_sign, 1, "DECODE-FLOAT-SIGN")
{ Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  return integer_decode_sign(Arg1);
}

Define_Primitive(prim_df_mant, 1, "DECODE-FLOAT-MANTISSA")
{ 
  double x; 
  long *h;
  
  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  x = Get_Float(Arg1);
  h = (long *) &x;
#ifdef sun
  *h = *h & ~(0x7ff << 20) & 0x7fffffff | (1022 << 20);
#endif
#ifdef vax
  *h = *h & ~(0xff << 7) & 0x7fffffff | (128 << 7);
#endif
  Flonum_Result(x);
}

Define_Primitive(prim_df_exp, 1, "DECODE-FLOAT-EXPONENT")
{ 
  double x; 
  long high;

  Primitive_1_Arg();
  Arg_1_Type(TC_BIG_FLONUM);
  x = Get_Float(Arg1);
  high = *((int *) &x);

#ifdef sun
  return Make_Non_Pointer(TC_FIXNUM,((high >> 20) & 0x7ff) - 1022);
#endif
#ifdef vax
  return Make_Non_Pointer(TC_FIXNUM,((high >> 7) & 0xff) - 128);
#endif
}

Pointer get_float_parameter(Arg1, Arg2)
Pointer Arg1, Arg2;
{ long typ, param;
  double r;
  long *p;

  typ = Get_Integer(Arg1);
  param = Get_Integer(Arg2);
  p = (long *)&r;
  switch (param)
    {
    case MOST_POS:
      {
/*               6666 5555 5555 5544 4444 4444 3333 3333     */
/*               3210 9876 5432 1098 7654 3210 9876 5432     */
/*	p[0] = 0b0111 1111 1110 1111 1111 1111 1111 1111;    */
/*	p[1] = 0b1111 1111 1111 1111 1111 1111 1111 1111;    */
	p[0] = 0x7fefffff;
	p[1] = 0xffffffff;
	break;
      }
    case LEAST_POS:
      {
/*	p[0] = 0b0000 0000 0000 0000 0000 0000 0000 0000;    */
/*	p[1] = 0b0000 0000 0000 0000 0000 0000 0000 0001;    */
	p[0] = 0x00000000;
	p[1] = 0x00000001;
	break;
      }
    case LEAST_NEG:
      {
/*	p[0] = 0b1000 0000 0000 0000 0000 0000 0000 0000;    */
/*	p[1] = 0b0000 0000 0000 0000 0000 0000 0000 0001;    */
	p[0] = 0x80000000;
	p[1] = 0x00000001;
	break;
      }
    case MOST_NEG:
      {
/*	p[0] = 0b1111 1111 1110 1111 1111 1111 1111 1111;    */
/*	p[1] = 0b1111 1111 1111 1111 1111 1111 1111 1111;    */
	p[0] = 0xffefffff;
	p[1] = 0xffffffff;
	break;
      }
    case EPSILON:
      {
	/* Stolen straight from KCL */
	for (r = 1.0; (1.0 + r) != 1.0; r /= 2.0)
	  {};
	break;
      }
    case NEG_EPSILON:
      {
	/* Stolen straight from KCL */
	for (r = 1.0; (1.0 - r) != 1.0; r /= 2.0)
	  {};
	break;
      }
    }
  Flonum_Result(r);
}


Define_Primitive(prim_float_parameter, 2, "FLOAT-PARAMETER")
{ Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  return(get_float_parameter(Arg1, Arg2));
}

Pointer NAN_Or_Infinity_P(f)
Pointer f;
{ long *p;

  p = (long *)&Get_Float(f);
  if ((p[0] & 0x7ff00000) == 0x7ff00000)
    return TRUTH;
  else
    return NIL;
}

Pointer Infinity_P(f)
Pointer f;
{ long *p;

  p = (long *)&Get_Float(f);
  if (((p[0] & 0x7ff00000) == 0x7ff00000) && 
      ((p[0] & 0x000fffff) == 0) &&
      (p[1] == 0)) return TRUTH;
  else return NIL;
}

Pointer NAN_P(f)
Pointer f;
{ long *p;

  p = (long *)&Get_Float(f);
  if (((p[0] & 0x7ff00000) == 0x7ff00000) && 
      (((p[0] & 0x000fffff) != 0) ||
       ((p[1] != 0)))) return TRUTH;
  else return NIL;
}

Define_Primitive(prim_float_infinity_p, 1, "FLOAT-INFINITY?")
{
  long *p;
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  return Infinity_P(Arg1);
}

Define_Primitive(prim_float_nan_p, 1, "FLOAT-NAN?")
{
  long *p;
  Primitive_1_Arg();

  Arg_1_Type(TC_BIG_FLONUM);
  return NAN_P(Arg1);
}

Define_Primitive(prim_hash_float, 1, "SXHASH-FLOAT")
{
  double x;
  short *p;
  long h, i;

  Primitive_1_Arg();

  x = Get_Float(Arg1);
  p = (short *) &x;
  h = 0;
  for (i = 0; i < 4; i++)
    h += *p++;
  return Make_Non_Pointer(TC_FIXNUM,h);
}

Define_Primitive(prim_ash, 2, "ASH")
{
  long n, i, tc_arg1, tc_arg2, fixans, mask, arg1_sign;
  Primitive_2_Args();

  tc_arg1 = Type_Code(Arg1);
  tc_arg2 = Type_Code(Arg2);
  if ((tc_arg1 != TC_FIXNUM) && (tc_arg1 != TC_BIG_FIXNUM)) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if ((tc_arg2 != TC_FIXNUM) && (tc_arg2 != TC_BIG_FIXNUM)) Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  if (tc_arg2==TC_BIG_FIXNUM)
    {
      if (Type_Code(Arg1)==TC_FIXNUM)
	{
	  Sign_Extend(Arg1,n);
	  arg1_sign = n >= 0;
	}
      else arg1_sign = SIGN(BIGNUM(Get_Pointer(Arg1)));

      switch ((arg1_sign << 1) | SIGN(BIGNUM(Get_Pointer(Arg2))))
	{
	case BOTH_POSITIVE:
	case ARG1_NEGATIVE:
	  {
	    Primitive_Error(ERR_ARG_2_BAD_RANGE); /* We assume that memory can't hold a number this big */
	  }
	case ARG2_NEGATIVE:
	  {
	    return Make_Unsigned_Fixnum(0);
	  }
	case BOTH_NEGATIVE:
	  {
	    return Make_Non_Pointer(TC_FIXNUM,-1);
	  }
	}
    }

  switch (tc_arg1)
    {
    case TC_BIG_FIXNUM:
      {
	Sign_Extend(Arg2,i);
	return Big_To_Fix(big_ash(BIGNUM(Get_Pointer(Arg1)),i));
      }
    case TC_FIXNUM:
      {
	Sign_Extend(Arg1,n);
	if (n==0) return Make_Unsigned_Fixnum(0);
	Sign_Extend(Arg2,i);
	if (i<0)  /* RIGHT -- easy */
	  {
	    i = -i;
	    if (i > FIXNUM_LENGTH) 
	      {
		if (n<0) fixans = -1;
		else fixans = 0;
	      }
	    else fixans = n >> i;
	    return Make_Non_Pointer(TC_FIXNUM,fixans);
	  }
	else  /* LEFT */
	  {
	    if (i < FIXNUM_LENGTH)
	      {
		mask = ((1 << (i + 1)) - 1) << ((FIXNUM_LENGTH + 1) - (i + 1)); /* Gives us i+1 upper bits set */
		if (n>0)
		  {
		    if ((n & mask) == 0) return Make_Non_Pointer(TC_FIXNUM,n << i);
		    else return big_ash(BIGNUM(Get_Pointer(Fix_To_Big(Arg1))),i);
		  }
		else
		  {
		    if ((n & mask) == mask) return Make_Non_Pointer(TC_FIXNUM,n << i);
		    else return big_ash(BIGNUM(Get_Pointer(Fix_To_Big(Arg1))),i);
		  }
	      }
	    else return big_ash(BIGNUM(Get_Pointer(Fix_To_Big(Arg1))),i);
	  }
      }
    }
}

big_ash(arg,shift_cnt_in)
bigdigit *arg;
long shift_cnt_in;

{
  bigdigit *top, *ans, mask, *x, *y, *z, *anstop;
  bigdouble sum;
  bigdigit sign;
  long size, shift_cnt, rem_bit_shift_cnt, 
       arg_bit_len, result_len, word_shift_cnt,
       bit_shift_cnt, dir;

  sign = SIGN(arg);

  if (shift_cnt_in<0) shift_cnt = -shift_cnt_in;
  else shift_cnt = shift_cnt_in;

  x = arg;
  word_shift_cnt = shift_cnt / SHIFT;
  bit_shift_cnt = imod(shift_cnt,SHIFT);
  rem_bit_shift_cnt = SHIFT - bit_shift_cnt;
  arg_bit_len = bignum_bit_len(arg);   /* assumes bignums no longer than 2**long-bit-len bits -- a good assumption */
  result_len = iceil(arg_bit_len + shift_cnt_in,SHIFT);  /* uses fact that shift_cnt_in may be negative */
  size = Align(result_len);
  Primitive_GC_If_Needed(size);
  ans = BIGNUM(Free);
  Prepare_Header(ans,result_len,sign);  
  anstop = Bignum_Top(ans);

  if (shift_cnt_in<0) /* RIGHT */
    {
      if (shift_cnt>=arg_bit_len)
	{
	  if (sign==NEGATIVE) return Make_Non_Pointer(TC_FIXNUM,-1);
	  else return Make_Unsigned_Fixnum(0);
	}
      mask = (1 << bit_shift_cnt) - 1;
      x = Bignum_Bottom(arg);
      top = Bignum_Top(arg);
      z = Bignum_Bottom(ans);
      sum = 0;
      y = x + word_shift_cnt;
      while (x != y)		/* Doing this loop even if arg positive insures that we zero the storage */
	{
	  sum |= (*x++ != 0);
	}
      sum |= ((*x & mask) != 0);
      sum <<= SHIFT;		/* gets low bit into carry position */
      while (x != top)
	{
	  *z = *x++ >> bit_shift_cnt;
	  *z |= *x << rem_bit_shift_cnt;
	  if (sign==NEGATIVE)
	    {
	      sum = *z + Get_Carry(sum);
	      *z++ = Get_Digit(sum);
	    }
	  else 
	    z++;
	}
      *z = (*x >> bit_shift_cnt);
      if (sign==NEGATIVE) *z += Get_Carry(sum);
    }
  else  /* LEFT */
    {
      bigdigit high_save = 0;
      long i;

      mask = ((1 << bit_shift_cnt) - 1) << rem_bit_shift_cnt;
      x = Bignum_Bottom(arg);
      top = Bignum_Top(arg);
      z = Bignum_Bottom(ans);
      for (i=0;i<word_shift_cnt;i++)
	{
	  *z++ = 0;
	}
      while (x <= top)
	{
	  *z = high_save >> rem_bit_shift_cnt;
	  high_save = (*x & mask);
	  *z++ |= *x++ << bit_shift_cnt;
	}
      if (z == anstop) *z = high_save >> rem_bit_shift_cnt;
    }
  Free += size;
  return Make_Pointer(TC_BIG_FIXNUM,Free-size);
}

bignum_bit_len(bignum)
bigdigit *bignum;
{
  short x;
  long i;

  x = (short)(*Bignum_Top(bignum));
  for (i=0; x>0; i++) x <<= 1;
  return (LEN(bignum) * SHIFT) - i;
}

iceil(x,y)
long x,y;
{
  return (x + y - 1) / y;
}

imod(x,y)
long x,y;
{
  return (x - y * (x / y));
}

/*
  (format-fixed-flonum-to-string  x        ; number
                                  result   ; string to stuff
				  width    ; total field width
				  fdigits  ; fraction width
				  scale    ; scale power
				  )

  Returns the length of what it stuffed into the string.  If width is
  ommitted (nil), than enough digits are printed to represent the
  number, constrained by fdigits.  If fdigits is ommitted (nil), then
  enough fraction digits are printed to represent the number,
  constrained by width.  A leading 0 before the decimal point is added
  if the integer part is zero and the width constraint allows this.  A
  trailing 0 after the decimal point is added if the fractional part
  to be printed is 0 and the width and fdigits constraint allow this.

*/

Define_Primitive(prim_format_fixed_flonum_to_string, 5, "CL-FORMAT-FIXED-FLONUM-TO-STRING")
{ double x;
  char *result;
  long width, fdigits, scale, result_len;
  Primitive_5_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  x = Get_Float(Arg1);
  Arg_2_Type(TC_CHARACTER_STRING);
  result = string_pointer(Arg2,0);
  if (Arg3 != NIL)
  { Arg_3_Type(TC_FIXNUM);
    Sign_Extend(Arg3,width);
  }
  else width = -1;
  if (Arg4 != NIL)
  { Arg_4_Type(TC_FIXNUM);
    Sign_Extend(Arg4,fdigits);
  }
  else fdigits = -1;
  if (Arg5 != NIL)
  { Arg_5_Type(TC_FIXNUM);
    Sign_Extend(Arg5,scale);
    x = x * pow(10.0,(double)scale);
  }

  if (NAN_P(Arg1))
  { sprintf(result, "FLOATING-NAN");
    result_len = s_len(result);
    return Make_Non_Pointer(TC_FIXNUM, result_len);
  }
  else if (Infinity_P(Arg1))
  { sprintf(result, "INFINITY");
    result_len = s_len(result);
    return Make_Non_Pointer(TC_FIXNUM, result_len);
  }

  if (width == -1)
    { if (fdigits == -1)
      { sprintf(result,"%f",x);
	result_len = s_len(result);
	suppress_trailing_zeroes(result,&result_len);
      }
      else
      { sprintf(result,"%.*f", fdigits, x);
	result_len = s_len(result);
      }
    }
  else if (fdigits == -1)
  { sprintf(result,"%*f", width, x);
    if (s_len(result) > width)
    { if (i_len(result) >= width)
	sprintf(result,"%*.0f.", width, x);
      else
	sprintf(result,"%*.*f", width, (width - i_len(result)), x);
    }
    result_len = s_len(result);
    suppress_trailing_zeroes(result,&result_len);
  }
  else
  { if (fdigits != 0)
      sprintf(result,"%*.*f",width,fdigits,x);
    else
      sprintf(result,"%*.0f.",width,x);
    result_len = s_len(result);
  }
  /* Do decimal point processing */
  suppress_leading_zeroes_and_spaces(result, &result_len);
  /* The result is now in a cannonical form of either ".xxx" or "yyy.xxx" or "yyy."
     Now add a leading zero and/or trailing zeros, if necessary */
  if ((result[0] == '.') && 
      (!((width != -1) && (fdigits != -1) && (width == fdigits+1))))
    { long i;
      char save2, save1 = result[0];
      result[0] = '0';
      for (i = 1; i <= result_len; i++)
      { save2 = result[i];
	result[i] = save1;
	save1 = save2;
      }
      result_len += 1;
    }
  if ((result[result_len - 1] == '.') &&
      (fdigits != 0) &&
      (!((width != -1) && (result_len >= width))))
  { result[result_len] = '0';
    result_len++;
  }

  return(Make_Non_Pointer(TC_FIXNUM,result_len));
}

/*
  (format-exponential-flonum-to-string  x        ; number
                                        result   ; string to stuff
				        width    ; total field width
				        fdigits  ; fraction width
					scale    ; scale power
					e        ; exponent width
					expchar  ; exponent character
				  )

  Returns the length of what it stuffed into the string.  If width is
  ommitted (nil), than enough digits are printed to represent the
  number, constrained by fdigits.  If fdigits is ommitted (nil), then
  enough fraction digits are printed to represent the number,
  constrained by width.  A leading 0 before the decimal point is added
  if the integer part is zero and the width constraint allows this.  A
  trailing 0 after the decimal point is added if the fractional part
  to be printed is 0 and the width and fdigits constraint allow this.

*/

/* Helper routines */

long min(a, b)
long a, b;
{ if (a < b)
    return a;
  else
    return b;
}

long expt(x, n)
long x, n;
{ int i, p;
  p = 1;
  for (i = 1; i <= n; ++i)
    p = p * x;
  return p;
}

long round2(x, y)
long x, y;
{ return (long) ((((double) x) / ((double) y)) + 0.5);
}

copy_n_digits(source, source_index, dest, dest_index, n)
char *source, *dest;
long *source_index, *dest_index, n;
{ while (n > 0)
  { if (source[*source_index])
      dest[(*dest_index)++] = source[(*source_index)++];
    else
      dest[(*dest_index)++] = '0';
    n--;
  }
}

copy_last_digit(source, dest, dest_index)
char *source, *dest;
long *dest_index;
{ long rem;
  if (*source)
  { sscanf(source, "%ld", &rem);
    if (rem == 0)
      dest[(*dest_index)++] = '0';
    else
      dest[(*dest_index)++] = 
	'0' + round2(rem, expt(10, ((long) floor(log10((double) rem)))));
  }
  else
    dest[(*dest_index)++] = '0';
}

long add_exponent(str, index, exp, n)
char *str;
long index, exp, n;
{   str+= index;

    if (n == -1)
    { sprintf(str, "%d", exp);
      while (*str)
      { str++;
	index++;
      }
    } 
    else
    { sprintf(str, "%0*d", n, exp);
      index += n;
    }
    return index;
}

add_leading_zero(str)
char *str;
{ long i;
  char save1, save2;
  
  save2 = *str;
  *str++ = '0';

  while (*str)
  { save1 = *str;
    *str++ = save2;
    save2 = save1;
  }
  *str++ = save2;
  *str++ = '\0';
}

Define_Primitive(prim_format_exponential_flonum_to_string, 7, "CL-FORMAT-EXPONENTIAL-FLONUM-TO-STRING")
{ double x;
  char *result, expchar, dummy;
  char temp1[100], *temp;
  long w, d, e, ee, k, len, i, j, num_digits, frac, exponent;
  Primitive_7_Args();

  Arg_1_Type(TC_BIG_FLONUM);
  x = Get_Float(Arg1);
  Arg_2_Type(TC_CHARACTER_STRING);
  result = string_pointer(Arg2,0);
  if (Arg3 != NIL)
  { Arg_3_Type(TC_FIXNUM);
    Sign_Extend(Arg3,w);
  }
  else w = -1;
  if (Arg4 != NIL)
  { Arg_4_Type(TC_FIXNUM);
    Sign_Extend(Arg4,d);
  }
  else d = -1;
  if (Arg5 != NIL)
  { Arg_5_Type(TC_FIXNUM);
    Sign_Extend(Arg5,k);
  }
  else k = 1;
  if (Arg6 != NIL)
  { Arg_6_Type(TC_FIXNUM);
    Sign_Extend(Arg6,e);
  }
  else e = -1;
  if (Arg7 != NIL)
  { Arg_7_Type(TC_CHARACTER);
    expchar = scheme_char_to_c_char(Arg7);
  }
  else expchar = 'E';

  if (NAN_P(Arg1))
  { sprintf(result, "FLOATING-NAN");
    len = s_len(result);
    return Make_Non_Pointer(TC_FIXNUM, len);
  }
  else if (Infinity_P(Arg1))
  { sprintf(result, "INFINITY");
    len = s_len(result);
    return Make_Non_Pointer(TC_FIXNUM, len);
  }

  temp = temp1;
  if (w == -1)
  { if (d == -1)
      sprintf(temp, "%15.15e", x);
    else
      sprintf(temp, "%.*e", d, x);
  }
  else
  { if (d == -1)
      sprintf(temp, "%*.15e", w, x);
    else
      sprintf(temp, "%*.*e", w, d, x);
  }
  len = s_len(temp);
  suppress_leading_zeroes_and_spaces(temp, &len);
  temp[len] = '\0';
  if (d == -1)
    computerized_suppress_trailing_zeroes(temp, &len);
  
  /* Remove leading '.' */ 
  temp[1] = temp[0];
  temp++;

  /* Get the exponent */
  sscanf(temp, "%ld%c%ld", &frac, &dummy, &exponent);
  exponent = exponent + 1 - k;
  /* ee is the number of digits that will be used to print the exponent */
  if (exponent == 0)
    ee = max(1, e);
  else if (exponent < 0)
    ee = max((((long) floor(log10(0 - ((double) exponent)))) + 1),
	     e);
  else
    ee = max((((long) floor(log10((double) exponent))) + 1),
	     e);

  /* Figure out the number of digits */
  for (num_digits = 0; 
       ((temp[num_digits] != 'E') && (temp[num_digits] != 'e'));
       num_digits++)
    ;

  /* Make the temp into a proper C string of digits */
  temp[num_digits] = '\0';

  /* The temp is now a digit string in a cannonical form of "nnnn".  Now
     process the options. There are num_digits digits available */
  
  /* First do scaling */
  j = 0;
  i = 0;
  if (k == 0)
  { result[i++] = '.';
    if (d == -1)
    { if (w == -1)
	copy_n_digits(temp, &j, result, &i, max(num_digits, 1));
      else if (num_digits >= (w - (4 + ee)))
      { copy_n_digits(temp, &j, result, &i, (w - (5 + ee)));
	copy_last_digit(temp+j, result, &i);
      }
      else if (num_digits > 0)
	copy_n_digits(temp, &j, result, &i, num_digits);
      else if (w > (k + 4 + ee))
	copy_n_digits(temp, &j, result, &i, 1); /* Add trailing zero after . */
    }
    else
      copy_n_digits(temp, &j, result, &i, d);
  }
  else if (k > 0)
  { copy_n_digits(temp, &j, result, &i, k);
    result[i++] = '.';
    if (d == -1)
    { if (w == -1)
	copy_n_digits(temp, &j, result, &i, max(num_digits-k, 1));
      else if (num_digits >= (w - (3 + ee)))
      { copy_n_digits(temp, &j, result, &i, (w - (k + 4 + ee)));
	copy_last_digit(temp+j, result, &i);
      }
      else if (num_digits > k)
	copy_n_digits(temp, &j, result, &i, num_digits - k);
      else if (w > (k + 3 + ee))
	copy_n_digits(temp, &j, result, &i, 1); /* Add trailing zero after . */
    }
    else
    { copy_n_digits(temp, &j, result, &i, (d - k));
      if ((d + 1) != k)
	copy_last_digit(temp+j, result, &i);
    }
  }
  else /* k < 0 */
  { k *= -1;
    result[i++] = '.';
    for (; i < k + 1; i++)
    { result[i] = '0';
    }
    if (d == -1)
    { if (w == -1)
	copy_n_digits(temp, &j, result, &i, num_digits);
      else if (num_digits > (w - (k + 4 + ee)))
      { copy_n_digits(temp, &j, result, &i, (w - (k + 5 + ee)));
	copy_last_digit(temp+j, result, &i);
      }
      else
	copy_n_digits(temp, &j, result, &i, num_digits - k);
    }
    else
    { copy_n_digits(temp, &j, result, &i, ((d - k) - 1));
      if (d != k)
	copy_last_digit(temp+j, result, &i);
    }
  }

  /* Print the exponent marker */
  if (expchar)
    result[i++] = expchar;
  else
    result[i++] = 'E';

  /* Print the sign */
  if (exponent < 0)
    result[i++] = '-';
  else
    result[i++] = '+';
  
  /* Print the exponent */
  i = add_exponent(result, i, abs(exponent), e);

  /* Add leading zero if needed and it fits */
  if ((result[0] == '.') &&
      ((w == -1) || (w > i)))
    { add_leading_zero(result);
      i++;
    }

  /* Check for exponent overflow and return */
  if ((e == -1) || (ee <= e))
    return(Make_Non_Pointer(TC_FIXNUM, i));
  else /* exponent overflow, return negative length */
    return(Make_Non_Pointer(TC_FIXNUM, (0 - i)));
}

/* Prints a flonum in computerized scientific notation */

Define_Primitive(prim_cl_computerized_flonum_to_string, 2, "CL-COMPUTERIZED-FLONUM-TO-STRING")
{
  double x;
  char *result;
  long result_len;
  Primitive_2_Args();
  Arg_1_Type(TC_BIG_FLONUM);
  Arg_2_Type(TC_CHARACTER_STRING);

  x = Get_Float(Arg1);
  result = string_pointer(Arg2, 0);
  sprintf(result, "%-.13e", x);
  result_len = s_len(result);
  computerized_suppress_trailing_zeroes(result, &result_len);
  computerized_contract_exponent(result, &result_len);
  return Make_Non_Pointer(TC_FIXNUM, result_len);
}

computerized_suppress_trailing_zeroes(s, len)
char *s;
long *len;
{  long marker, unneeded, i, j, dif;
   
   for (marker = 0; s[marker] != 'e'; marker++)
   { }
   s[marker] = 'E';
   
   for (unneeded = marker-1; s[unneeded] == '0'; unneeded--)
   { }
   if (s[unneeded] == '.')  /* We need at least one digit */
     unneeded++;

   unneeded++;
   dif = marker - unneeded;

   for (i = unneeded, j = marker; j <= *len; i++, j++)
   { s[i] = s[j];
   }

   *len = *len - dif;
   s[*len] = '\0';
}

computerized_contract_exponent(s, len)
char *s;
long *len;
{ long old_e_len, new_e_len, exponent_start, i;

  for (i = 0; s[i] != 'E'; i++)
    ;
  i++;

  if (s[i] == '+')      /* Clear + in exponent */
    s[i] = '0';
  else if (s[i] == '-') /* Skip over the sign */
    i++;
  
  exponent_start = i;
  for (old_e_len = 0; s[i] != '\0'; i++)
    old_e_len++;
  new_e_len = old_e_len;
  suppress_leading_zeroes_and_spaces((s+exponent_start), &new_e_len);
  *len = *len - (old_e_len - new_e_len);
}

/*
  length of string, not including the end null char
*/

s_len(s)
char *s;
{
  long i;

  for (i=0; s[i]!='\0'; i++);
  return i;
}

/*
 Length of integer part of string, including point.
*/

i_len(s)
char *s;
{
  long i;

  for (i=1; *s++!='.'; i++);
  return i;
}

suppress_trailing_zeroes(s,plen)
char *s;
long *plen;
{
  s += *plen - 1;
  while (*s-- == '0') (*plen)--;
}

suppress_leading_zeroes_and_spaces(s,plen)
char *s;
long *plen;
{
  long i,j,len;

  len = *plen;
  for (i=0; (s[i]=='0') || (s[i]==' '); i++) len--;
  j = 0;
  for (; i < *plen; i++)
    {
      s[j++] = s[i];
    }
  *plen = len;
}

/*
  Assumes, as code above, that we will have bit lengths no larger than a fixnum.
*/

Define_Primitive(prim_integer_length, 1, "INTEGER-LENGTH")
{
  long n, i, tc, l, fix_ceiling_carry;
  bigdigit *big;
  Primitive_1_Arg();

  tc = Type_Code(Arg1);
  if ((tc != TC_FIXNUM) && (tc != TC_BIG_FIXNUM)) Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  if (tc == TC_FIXNUM)
    {
      Sign_Extend(Arg1,n);
      if (n==BIGGEST_FIXNUM) return Make_Non_Pointer(TC_FIXNUM,FIXNUM_LENGTH + 1);
      if (n==SMALLEST_FIXNUM) return Make_Non_Pointer(TC_FIXNUM,FIXNUM_LENGTH);
      if (n<0) n = -n;
      else n += 1;
      fix_ceiling_carry = 0;
      for (i = -1; n != 0; i++)   /* n can never start at zero */
	{
	  if ((n != 1) && ((n & 1) != 0)) fix_ceiling_carry = 1;
	  n >>= 1;
	}
      return Make_Non_Pointer(TC_FIXNUM,(i + fix_ceiling_carry));
    }
  else
    {
      big = BIGNUM(Get_Pointer(Arg1));
      return Make_Non_Pointer(TC_FIXNUM,(bignum_bit_len(big) + add_1_carry(big) + ceiling_carry(big)));
    }
}

add_1_carry(big)
bigdigit *big;
{
  bigdigit *x, *top, n;
  bigdigit biggest_bigdigit = (1 << SHIFT) - 1;

  if (SIGN(big)==NEGATIVE) return 0;
  x = Bignum_Bottom(big);
  top = Bignum_Top(big);
  while (x != top)
    if (*x++ != biggest_bigdigit) return 0;
  n = *x;
  while (n != 0)
    {
      if ((n & 1) == 0) return 0;
      n >>= 1;
    }
  return 1;
}

ceiling_carry(big)
bigdigit *big;
{
  bigdigit *x, *top, n;

  x = Bignum_Bottom(big);
  top = Bignum_Top(big);
  while (x != top)
    if (*x++ != 0) return 1;
  n = *x;
  while (n != 1)
    {
      if ((n & 1) != 0) return 1;
      n >>= 1;
    }
  return 0;
}

#define s0 0
#define s1 1
#define s2 2
#define s3 3
#define s4 4
#define s5 5
#define s6 6
#define s7 7
#define s8 8
#define s9 9
#define s10 10
#define s1_5 11

#define PARSED_FLOAT  0
#define PARSED_INTEGER  1
#define PARSED_RATIO  2

#define Digit(Char)  digitp(Char, base)

Boolean digitp(c, base)
char c;
long base;
{
  if ((c >= 'a') && (c <= 'z'))
    c = c - ('a' - 'A');
  if (base == 10)
    return ((c >= '0') && (c <= '9'));
  else if (base < 10)
    return ((c >= '0') && ((c - '0') < base));
  else
    return ((c >= '0') && c < ((base + '0') + ('A' - '9')));
}

Boolean exp_marker(c)
char c;
{
  char *p;

  for (p = "EeFfSsLlDd"; *p != '\0'; p++)
    if (*p == c) return true;
  return false;
}

/* 
  Returns NIL if not a number.
  Returns T if base illegal.
  Returns multiple values otherwise.

  Note below, when testing for an exp marker and a digit,
  digit must be tested for first, since it takes precedence
  in ambiguous cases.
*/

Define_Primitive(prim_cl_parse_number, 2, "CL-PARSE-NUMBER")
{
  char *string;
  long i, state, base, parsed_kind;
  long digit_start[3], digit_end[3];
  long digit_index;
  char c;
  Boolean negative, point1, point2, exp, ratio, exp_negative;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  string = Scheme_String_To_C_String(Arg1);
  base = Get_Integer(Arg2);
  if ((base < 2) || (base > 36)) return TRUTH;
  negative = false;
  point1 = false;
  point2 = false;
  exp = false;
  exp_negative = false;
  ratio = false;
  for (i = 0; i < 3; i++)
    {
      digit_start[i] = 0;
      digit_end[i] = 0;
    }
  digit_index = 0;
  state = s0;
  for (i = 0;;i++)
    {
      c = *string++;
      if (c == '\0') break;
      switch (state)
	{
	case s0:
	  {
	    if ((c == '+') || (c == '-'))
	      {
		negative = (c == '-');
		state = s1;
		break;
	      }
	    else if (Digit(c))
	      {
		digit_start[digit_index] = i;
		digit_end[digit_index] = i + 1;
		state = s2;
		break;
	      }
	    else if ((c == '.') && (base == 10))
	      {
		state = s1_5;
		point1 = true;
		digit_index++;
		break;
	      }
	    else return NIL;
	  }
	case s1:
	  {
	    if (Digit(c))
	      {
		digit_start[digit_index] = i;
		digit_end[digit_index] = i + 1;
		state = s2;
		break;
	      }
	    else if ((c == '.') && (base == 10))
	      {
		point1 = true;
		state = s2;
		digit_index++;
		break;
	      }
	    else return NIL;
	  }
	case s1_5:
	  {
	    if (Digit(c))
	      {
		digit_start[digit_index] = 1;
		digit_end[digit_index] = i + 1;
		state = s2;
		break;
	      }
	    else return NIL;
	  }
	case s2:
	  {
	    if (Digit(c))
	      {
		digit_end[digit_index]++;
		state = s2;
		break;
	      }
	    else if ((exp_marker(c)) && (base == 10))
	      {
		exp = true;
		state = s5;
		break;
	      }
	    else if ((c == '.') && (base == 10))
	      {
		point2 = true;
		state = s3;
		digit_index++;
		break;
	      }
	    else if (c == '/')
	      {
		ratio = true;
		digit_index++;
		state = s7;
		break;
	      }
	    else return NIL;
	  }
	case s3:
	  {
	    if (Digit(c))
	      {
		digit_start[digit_index] = i;
		digit_end[digit_index] = i + 1;
		state = s4;
		break;
	      }
	    else if ((exp_marker(c)) && (base == 10))
	      {
		exp = true;
		state = s5;
		break;
	      }
	    else return NIL;
	  }
	case s4:
	  {
	    if (Digit(c))
	      {
		digit_end[digit_index]++;
		state = s4;
		break;
	      }
	    else if ((exp_marker(c)) && (base == 10))
	      {
		exp = true;
		state = s5;
		break;
	      }
	    else return NIL;
	  }
	case s5:
	  {
	    if ((c == '+') || (c == '-'))
	      {
		exp_negative = (c == '-');
		state = s6;
		break;
	      }
	    else if (Digit(c))
	      {
		digit_start[2] = i;
		digit_end[2] = i + 1;
		state = s9;
		break;
	      }
	    else return NIL;
	  }
	case s6:
	  {
	    if (Digit(c))
	      {
		digit_start[2] = i;
		digit_end[2] = i + 1;
		state = s9;
		break;
	      }
	    else return NIL;
	  }
	case s7:
	  {
	    if (Digit(c))
	      {
		digit_start[1] = i;
		digit_end[1] = i + 1;
		state = s10;
		break;
	      }
	    else return NIL;
	  }
	case s8:   /* should never get here */
	  {
	    break;
	  }
	case s9:
	  {
	    if (Digit(c))
	      {
		digit_end[2]++;
		state = s9;
		break;
	      }
	    else return NIL;
	  }
	case s10:
	  {
	    if (Digit(c))
	      {
		digit_end[1]++;
		state = s10;
		break;
	      }
	    else return NIL;
	  }
	}
    }
  if ((point1 && point2) ||
      (exp && ratio) ||
      ((point1 || point2) && ratio) ||
      ((digit_start[0] == digit_end[0]) &&
       (digit_start[1] == digit_end[1]) &&
       (digit_start[2] == digit_end[2])) ||
      (exp && (digit_start[2] == digit_end[2])))
    return NIL;
  if (exp ||
      ((point1 || point2) &&
       (digit_start[1] != digit_end[1])))
    parsed_kind = PARSED_FLOAT;
  else if (ratio)
    parsed_kind = PARSED_RATIO;
  else
    parsed_kind = PARSED_INTEGER;
  Multiple_Value_Return(2, 9, parsed_kind,
			{
			  Push(Make_Unsigned_Fixnum(digit_end[2]));
			  Push(Make_Unsigned_Fixnum(digit_start[2]));
			  Push(Make_Unsigned_Fixnum(digit_end[1]));
			  Push(Make_Unsigned_Fixnum(digit_start[1]));
			  Push(Make_Unsigned_Fixnum(digit_end[0]));
			  Push(Make_Unsigned_Fixnum(digit_start[0]));
			  Push((exp_negative ? TRUTH : NIL));
			  Push((negative ? TRUTH : NIL));
			  Push(Make_Unsigned_Fixnum(parsed_kind));
			});
}
