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

/* $Header: stringprim.h,v 10.0 88/12/07 13:11:05 las Exp $
 * $MIT-Header: string.h,v 9.23 87/11/17 08:17:59 GMT jinx Exp $
 */

/* Macros for string primitives. */

#define string_length(string_object)				\
  ((long) (Vector_Ref (string_object, STRING_LENGTH)))

#define set_string_length(string_object, length)		\
{								\
  Vector_Set ((string_object), STRING_LENGTH, (length));	\
  *(string_pointer ((string_object), (length))) = '\0';		\
}

/* Subtract 1 to account for the fact that we maintain a '\0'
   at the end of the string. */

#define maximum_string_length(string_object)			\
  ((((Vector_Ref (string_object, STRING_HEADER)) - 1)		\
    * (sizeof (Pointer)))					\
   - 1)

#define string_pointer(string_object, index)			\
  (((char *) (Nth_Vector_Loc (string_object, STRING_CHARS))) + (index))

#define string_ref(string_object, index)			\
  (char_to_long (* (string_pointer (string_object, index))))

extern Pointer allocate_string ();
extern Pointer memory_to_string ();
