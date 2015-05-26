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

/* $Header: character.h,v 10.0 88/12/07 13:05:07 las Exp $
 * $MIT-Header: char.h,v 9.22 87/05/14 13:48:04 GMT cph Rel $
 */

/* Macros for characters. */

#define character_p(object)					\
  ((pointer_type (object)) == TC_CHARACTER)

#define NOT_ASCII -1

#define ASCII_LENGTH CHAR_SIZE     /* CHAR_SIZE in config.h - 8 for unix  */
#define CODE_LENGTH 7
#define BITS_LENGTH 5
#define EXTNDD_CHAR_LENGTH 12

#define CHAR_BITS_CONTROL 01
#define CHAR_BITS_META 02
#define CHAR_BITS_CONTROL_META (CHAR_BITS_CONTROL | CHAR_BITS_META)

#define MAX_ASCII (1 << ASCII_LENGTH)
#define MAX_CODE (1 << CODE_LENGTH)
#define MAX_BITS (1 << BITS_LENGTH)
#define MAX_EXTNDD_CHAR (1 << EXTNDD_CHAR_LENGTH)

#define MASK_ASCII (MAX_ASCII - 1)
#define CHAR_MASK_CODE (MAX_CODE - 1)
#define CHAR_MASK_BITS (MAX_BITS - 1)
#define MASK_EXTNDD_CHAR (MAX_EXTNDD_CHAR - 1)

extern long char_downcase();
extern long char_upcase();

#define char_to_long(expression)				\
  (((long) (expression)) & MASK_ASCII)

#define c_char_to_scheme_char(ascii)				\
  (Make_Non_Pointer (TC_CHARACTER, (ascii_to_mit_ascii (ascii))))

#define scheme_char_to_c_char(mit_ascii)			\
  (mit_ascii_to_ascii (pointer_datum (mit_ascii)))

#define make_char(bucky_bits, code)				\
  (Make_Non_Pointer (TC_CHARACTER,				\
		     (((bucky_bits) << (CODE_LENGTH)) | (code))))

#define char_bits(chr)						\
  (((pointer_datum (chr)) >> CODE_LENGTH) & CHAR_MASK_BITS)

#define char_code(chr)						\
  ((pointer_datum (chr)) & CHAR_MASK_CODE)

extern long mit_ascii_to_ascii(), ascii_to_mit_ascii();
