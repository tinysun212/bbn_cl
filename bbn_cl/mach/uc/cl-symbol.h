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

/* -*-C-*-

   Common Lisp Symbols Definitions  for Butterfly Lisp


   A Common Lisp Symbol is a Scheme interned symbol that is expanded.
   Instead of a string in the SYMBOL_NAME slot, there is a 
   CL_SYMBOL_ATTRIBUTE_VECTOR (CLSAV). This has slots for the CLSAV_NAME,
   CLSAV_PROPERTY_LIST, CLSAV_PACKAGE_CELL, and a special
   CLSAV_FUNCTION_SYMBOL.  The CL_SYMBOL_FUNCTION_SYMBOL is a normal 
   Scheme interned symbol which is used by the Common Lisp syntaxer for 
   function references.  This symbol stores a Common Lisp's function cell
   procedure in its SYMBOL_GLOBAL_VALUE cell and a pointer back to the
   interned symbol in its SYMBOL_NAME cell.  This allows code with uninterned
   symbols for operators in combinations to find the same 
   CL_SYMBOL_ATTRIBUTE_VECTOR.

*/

#define CLSAV_NAME            0
#define CLSAV_FUNCTION_SYMBOL 1
#define CLSAV_PROPERTY_LIST   2
#define CLSAV_PACKAGE_CELL    3
#define CLSAV_HASH_CELL       4
#define CLSAV_SYSTEM_INFO_CELL 5

#define CLSAV_SIZE            6
