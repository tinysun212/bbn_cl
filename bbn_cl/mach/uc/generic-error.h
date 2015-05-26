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

/**********************************************************/
/*                                                        */
/*          Generic Error System                          */
/*                                                        */
/**********************************************************/

/* The Common Lisp generic arithmetic has many mutually recursive
   calls to C routines.  Errors could happen in these routines, and
   the argument that caused the error may have no relation to the 
   arguments that were passed to the Scheme primitive.  Hence, a more
   flexible error system is needed to signal errors.  The following 
   mechanism uses a global status word to store error messages into.
   This word is cleared upon entry to a primitive, and is checked when
   the primitive is about to return a result.  The word is set by the C
   routines that make up the generic arithmetic.
*/

#define declare_error(error_code) Generic_Status = Generic_Status | error_code

#define GENERIC_ERROR_ARG1                         1
#define GENERIC_ERROR_ARG2                         2
#define GENERIC_ERROR_0_DENOM                      4
#define GENERIC_ERROR_2_ARG1_DISPATCH              8
#define GENERIC_ERROR_2_ARG2_DISPATCH              16
#define GENERIC_ERROR_1_ARG1_DISPATCH              32
#define GENERIC_ERROR_COMPLEX_SIGN                 64
#define GENERIC_ERROR_WRONG_NUMBER_OF_ARGS         128
#define GENERIC_ERROR_ARG1_COMPLEX_COMPARE         256
#define GENERIC_ERROR_ARG2_COMPLEX_COMPARE         512
#define GENERIC_ERROR_ARG1_COERCION                1024
#define GENERIC_ERROR_ARG2_COERCION                2048
#define GENERIC_ERROR_0_DIVIDE                     4096

#define generic_errorp() Generic_Status
#define clear_errors()   Generic_Status = 0

#define is_generic_error(error_code) (Generic_Status & error_code)

extern long Generic_Status;
