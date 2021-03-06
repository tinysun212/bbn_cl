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

/* $Header: bltdef.h,v 10.0 88/12/07 13:04:44 las Exp $
 * $MIT-Header: bltdef.h,v 1.1 87/11/17 07:56:57 GMT jinx Exp $
 *
 * Names and arity's of old "built-in" primitives.
 * The tables here are used by Bintopsb to upgrade binaries.
 */

#define MAX_BUILTIN_PRIMITIVE 431

int builtin_prim_arity_table[] = {
  3,
  2,
  3,
  1,
  2,
  2,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  2,
  2,
  2,
  1,
  2,
  2,
  2,
  2,
  1,
  0,
  1,
  2,
  3,
  0,
  1,
  2,
  3,
  1,
  1,
  2,
  1,
  1,
  2,
  2,
  0,
  0,
  2,
  3,
  2,
  3,
  3,
  2,
  1,
  2,
  1,
  3,
  1,
  0,
  2,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  2,
  2,
  2,
  2,
  2,
  2,
  1,
  1,
  1,
  2,
  3,
  1,
  0,
  0,
  0,
  3,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  1,
  2,
  1,
  3,
  1,
  3,
  2,
  0,
  0,
  2,
  1,
  2,
  1,
  2,
  1,
  1,
  1,
  1,
  1,
  2,
  1,
  1,
  2,
  2,
  2,
  2,
  2,
  2,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  1,
  1,
  3,
  1,
  1,
  1,
  2,
  2,
  1,
  3,
  1,
  1,
  1,
  2,
  2,
  2,
  0,
  2,
  2,
  1,
  2,
  2,
  1,
  2,
  2,
  1,
  2,
  1,
  2,
  3,
  1,
  2,
  3,
  1,
  5,
  5,
  4,
  0,
  0,
  0,
  1,
  1,
  2,
  3,
  1,
  1,
  1,
  2,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  2,
  1,
  0,
  2,
  2,
  1,
  1,
  1,
  1,
  1,
  2,
  2,
  1,
  1,
  1,
  2,
  1,
  2,
  2,
  1,
  0,
  2,
  3,
  3,
  2,
  1,
  0,
  0,
  0,
  1,
  2,
  1,
  1,
  2,
  5,
  2,
  2,
  1,
  3,
  0,
  2,
  1,
  0,
  3,
  3,
  1,
  4,
  1,
  0,
  0,
  1,
  1,
  1,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  0,
  1,
  2,
  0,
  0,
  0,
  2,
  0,
  0,
  1,
  0,
  2,
  0,
  0,
  0,
  0,
  2,
  2,
  1,
  3,
  1,
  0,
  1,
  7,
  7,
  7,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  2,
  1,
  1,
  2,
  3,
  5,
  5,
  1,
  1,
  2,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  6,
  6,
  6,
  3,
  3,
  6,
  6,
  6,
  6,
  1,
  0,
  2,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  1,
  4,
  4,
  7,
  7,
  7,
  4,
  4,
  2,
  4,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  3,
  2,
  2,
  4,
  7,
  7,
  7,
  2,
  3,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  0,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
};

static char No_Name[] = "";

char *builtin_prim_name_table[] = {
  "LEXICAL-ASSIGNMENT",
  "LOCAL-REFERENCE",
  "LOCAL-ASSIGNMENT",
  "CALL-WITH-CURRENT-CONTINUATION",
  "SCODE-EVAL",
  "APPLY",
  "SET-INTERRUPT-ENABLES!",
  "STRING->SYMBOL",
  "GET-WORK",
  "NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION",
  "CURRENT-DYNAMIC-STATE",
  "SET-CURRENT-DYNAMIC-STATE!",
  "NULL?",
  "EQ?",
  "STRING-EQUAL?",
  "PRIMITIVE-TYPE?",
  "PRIMITIVE-TYPE",
  "PRIMITIVE-SET-TYPE",
  "LEXICAL-REFERENCE",
  "LEXICAL-UNREFERENCEABLE?",
  "MAKE-CHAR",
  "CHAR-BITS",
  "EXIT",
  "CHAR-CODE",
  "LEXICAL-UNASSIGNED?",
  "INSERT-NON-MARKED-VECTOR!",
  "HALT",
  "CHAR->INTEGER",
  "MEMQ",
  "INSERT-STRING",
  "ENABLE-INTERRUPTS!",
  "MAKE-EMPTY-STRING",
  "CONS",
  "CAR",
  "CDR",
  "SET-CAR!",
  "SET-CDR!",
  "GET-COMMAND-LINE",
  "TTY-GET-CURSOR",
  "GENERAL-CAR-CDR",
  "HUNK3-CONS",
  "HUNK3-CXR",
  "HUNK3-SET-CXR!",
  "INSERT-STRING!",
  "VECTOR-CONS",
  "VECTOR-LENGTH",
  "VECTOR-REF",
  "SET-CURRENT-HISTORY!",
  "VECTOR-SET!",
  "NON-MARKED-VECTOR-CONS",
  No_Name,
  "LEXICAL-UNBOUND?",
  "INTEGER->CHAR",
  "CHAR-DOWNCASE",
  "CHAR-UPCASE",
  "ASCII->CHAR",
  "CHAR-ASCII?",
  "CHAR->ASCII",
  "GARBAGE-COLLECT",
  "PLUS-FIXNUM",
  "MINUS-FIXNUM",
  "MULTIPLY-FIXNUM",
  "DIVIDE-FIXNUM",
  "EQUAL-FIXNUM?",
  "LESS-THAN-FIXNUM?",
  "POSITIVE-FIXNUM?",
  "ONE-PLUS-FIXNUM",
  "MINUS-ONE-PLUS-FIXNUM",
  "TRUNCATE-STRING!",
  "SUBSTRING",
  "ZERO-FIXNUM?",
  No_Name,
  No_Name,
  No_Name,
  "SUBSTRING->LIST",
  "MAKE-FILLED-STRING",
  "PLUS-BIGNUM",
  "MINUS-BIGNUM",
  "MULTIPLY-BIGNUM",
  "DIVIDE-BIGNUM",
  "LISTIFY-BIGNUM",
  "EQUAL-BIGNUM?",
  "LESS-THAN-BIGNUM?",
  "POSITIVE-BIGNUM?",
  "FILE-OPEN-CHANNEL",
  "FILE-CLOSE-CHANNEL",
  "PRIMITIVE-FASDUMP",
  "BINARY-FASLOAD",
  "STRING-POSITION",
  "STRING-LESS?",
  No_Name,
  No_Name,
  "REHASH",
  "LENGTH",
  "ASSQ",
  "LIST->STRING",
  "EQUAL-STRING-TO-LIST?",
  "MAKE-CELL",
  "CELL-CONTENTS",
  "CELL?",
  "CHARACTER-UPCASE",
  "CHARACTER-LIST-HASH",
  "GCD-FIXNUM",
  "COERCE-FIXNUM-TO-BIGNUM",
  "COERCE-BIGNUM-TO-FIXNUM",
  "PLUS-FLONUM",
  "MINUS-FLONUM",
  "MULTIPLY-FLONUM",
  "DIVIDE-FLONUM",
  "EQUAL-FLONUM?",
  "LESS-THAN-FLONUM?",
  "ZERO-BIGNUM?",
  "TRUNCATE-FLONUM",
  "ROUND-FLONUM",
  "COERCE-INTEGER-TO-FLONUM",
  "SINE-FLONUM",
  "COSINE-FLONUM",
  "ARCTAN-FLONUM",
  "EXP-FLONUM",
  "LN-FLONUM",
  "SQRT-FLONUM",
  No_Name,
  "GET-FIXED-OBJECTS-VECTOR",
  "SET-FIXED-OBJECTS-VECTOR!",
  "LIST->VECTOR",
  "SUBVECTOR->LIST",
  "PAIR?",
  "NEGATIVE-FIXNUM?",
  "NEGATIVE-BIGNUM?",
  "GREATER-THAN-FIXNUM?",
  "GREATER-THAN-BIGNUM?",
  "STRING-HASH",
  "SYSTEM-PAIR-CONS",
  "SYSTEM-PAIR?",
  "SYSTEM-PAIR-CAR",
  "SYSTEM-PAIR-CDR",
  "SYSTEM-PAIR-SET-CAR!",
  "SYSTEM-PAIR-SET-CDR!",
  "STRING-HASH-MOD",
  No_Name,
  "SET-CELL-CONTENTS!",
  "&MAKE-OBJECT",
  "SYSTEM-HUNK3-CXR0",
  "SYSTEM-HUNK3-SET-CXR0!",
  "MAP-MACHINE-ADDRESS-TO-CODE",
  "SYSTEM-HUNK3-CXR1",
  "SYSTEM-HUNK3-SET-CXR1!",
  "MAP-CODE-TO-MACHINE-ADDRESS",
  "SYSTEM-HUNK3-CXR2",
  "SYSTEM-HUNK3-SET-CXR2!",
  "PRIMITIVE-PROCEDURE-ARITY",
  "SYSTEM-LIST-TO-VECTOR",
  "SYSTEM-SUBVECTOR-TO-LIST",
  "SYSTEM-VECTOR?",
  "SYSTEM-VECTOR-REF",
  "SYSTEM-VECTOR-SET!",
  "WITH-HISTORY-DISABLED",
  "SUBVECTOR-MOVE-RIGHT!",
  "SUBVECTOR-MOVE-LEFT!",
  "SUBVECTOR-FILL!",
  No_Name,
  No_Name,
  No_Name,
  "VECTOR-8B-CONS",
  "VECTOR-8B?",
  "VECTOR-8B-REF",
  "VECTOR-8B-SET!",
  "ZERO-FLONUM?",
  "POSITIVE-FLONUM?",
  "NEGATIVE-FLONUM?",
  "GREATER-THAN-FLONUM?",
  "INTERN-CHARACTER-LIST",
  "COMPILED-CODE-ADDRESS->OFFSET",
  "VECTOR-8B-SIZE",
  "SYSTEM-VECTOR-SIZE",
  "FORCE",
  "PRIMITIVE-DATUM",
  "MAKE-NON-POINTER-OBJECT",
  "DEBUGGING-PRINTER",
  "STRING-UPCASE",
  "PRIMITIVE-PURIFY",
  "COMPILED-CODE-ADDRESS->BLOCK",
  No_Name,
  "DUMP-BAND",
  "SUBSTRING-SEARCH",
  "LOAD-BAND",
  "CONSTANT?",
  "PURE?",
  "PRIMITIVE-GC-TYPE",
  "PRIMITIVE-IMPURIFY",
  "WITH-THREADED-CONTINUATION",
  "WITHIN-CONTROL-POINT",
  "SET-RUN-LIGHT!",
  "FILE-EOF?",
  "FILE-READ-CHAR",
  "FILE-FILL-INPUT-BUFFER",
  "FILE-LENGTH",
  "FILE-WRITE-CHAR",
  "FILE-WRITE-STRING",
  "CLOSE-LOST-OPEN-FILES",
  No_Name,
  "WITH-INTERRUPTS-REDUCED",
  "PRIMITIVE-EVAL-STEP",
  "PRIMITIVE-APPLY-STEP",
  "PRIMITIVE-RETURN-STEP",
  "TTY-READ-CHAR-READY?",
  "TTY-READ-CHAR",
  "TTY-READ-CHAR-IMMEDIATE",
  "TTY-READ-FINISH",
  "BIT-STRING-ALLOCATE",
  "MAKE-BIT-STRING",
  "BIT-STRING?",
  "BIT-STRING-LENGTH",
  "BIT-STRING-REF",
  "BIT-SUBSTRING-MOVE-RIGHT!",
  "BIT-STRING-SET!",
  "BIT-STRING-CLEAR!",
  "BIT-STRING-ZERO?",
  "BIT-SUBSTRING-FIND-NEXT-SET-BIT",
  No_Name,
  "UNSIGNED-INTEGER->BIT-STRING",
  "BIT-STRING->UNSIGNED-INTEGER",
  No_Name,
  "READ-BITS!",
  "WRITE-BITS!",
  "MAKE-STATE-SPACE",
  "EXECUTE-AT-NEW-POINT",
  "TRANSLATE-TO-STATE-POINT",
  "GET-NEXT-CONSTANT",
  "MICROCODE-IDENTIFY",
  "ZERO?",
  "POSITIVE?",
  "NEGATIVE?",
  "&=",
  "&<",
  "&>",
  "&+",
  "&-",
  "&*",
  "&/",
  "INTEGER-DIVIDE",
  "1+",
  "-1+",
  "TRUNCATE",
  "ROUND",
  "FLOOR",
  "CEILING",
  "SQRT",
  "EXP",
  "LOG",
  "SIN",
  "COS",
  "&ATAN",
  "TTY-WRITE-CHAR",
  "TTY-WRITE-STRING",
  "TTY-BEEP",
  "TTY-CLEAR",
  "GET-PRIMITIVE-COUNTS",
  "GET-PRIMITIVE-NAME",
  "GET-PRIMITIVE-ADDRESS",
  No_Name,
  No_Name,
  "GET-NEXT-INTERRUPT-CHARACTER",
  "CHECK-AND-CLEAN-UP-INPUT-CHANNEL",
  No_Name,
  "SYSTEM-CLOCK",
  "FILE-EXISTS?",
  No_Name,
  "TTY-MOVE-CURSOR",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "COPY-FILE",
  "RENAME-FILE",
  "REMOVE-FILE",
  "LINK-FILE",
  "MAKE-DIRECTORY",
  No_Name,
  "SET-WORKING-DIRECTORY-PATHNAME!",
  "RE-MATCH-SUBSTRING",
  "RE-SEARCH-SUBSTRING-FORWARD",
  "RE-SEARCH-SUBSTRING-BACKWARD",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "CURRENT-YEAR",
  "CURRENT-MONTH",
  "CURRENT-DAY",
  "CURRENT-HOUR",
  "CURRENT-MINUTE",
  "CURRENT-SECOND",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "CLEAR-TO-END-OF-LINE",
  No_Name,
  No_Name,
  "WITH-INTERRUPT-MASK",
  "STRING?",
  "STRING-LENGTH",
  "STRING-REF",
  "STRING-SET!",
  "SUBSTRING-MOVE-RIGHT!",
  "SUBSTRING-MOVE-LEFT!",
  "STRING-ALLOCATE",
  "STRING-MAXIMUM-LENGTH",
  "SET-STRING-LENGTH!",
  "VECTOR-8B-FILL!",
  "VECTOR-8B-FIND-NEXT-CHAR",
  "VECTOR-8B-FIND-PREVIOUS-CHAR",
  "VECTOR-8B-FIND-NEXT-CHAR-CI",
  "VECTOR-8B-FIND-PREVIOUS-CHAR-CI",
  "SUBSTRING-FIND-NEXT-CHAR-IN-SET",
  "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET",
  "SUBSTRING=?",
  "SUBSTRING-CI=?",
  "SUBSTRING<?",
  "SUBSTRING-UPCASE!",
  "SUBSTRING-DOWNCASE!",
  "SUBSTRING-MATCH-FORWARD",
  "SUBSTRING-MATCH-BACKWARD",
  "SUBSTRING-MATCH-FORWARD-CI",
  "SUBSTRING-MATCH-BACKWARD-CI",
  "PHOTO-OPEN",
  "PHOTO-CLOSE",
  "SETUP-TIMER-INTERRUPT",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "SCREEN-X-SIZE",
  "SCREEN-Y-SIZE",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "STRING->SYNTAX-ENTRY",
  "SCAN-WORD-FORWARD",
  "SCAN-WORD-BACKWARD",
  "SCAN-LIST-FORWARD",
  "SCAN-LIST-BACKWARD",
  "SCAN-SEXPS-FORWARD",
  "SCAN-FORWARD-TO-WORD",
  "SCAN-BACKWARD-PREFIX-CHARS",
  "CHAR->SYNTAX-CODE",
  "QUOTED-CHAR?",
  "MICROCODE-TABLES-FILENAME",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  "ERROR-PROCEDURE",
  "BIT-STRING-XOR!",
  "RE-CHAR-SET-ADJOIN!",
  "RE-COMPILE-FASTMAP",
  "RE-MATCH-BUFFER",
  "RE-SEARCH-BUFFER-FORWARD",
  "RE-SEARCH-BUFFER-BACKWARD",
  "SYSTEM-MEMORY-REF",
  "SYSTEM-MEMORY-SET!",
  "BIT-STRING-FILL!",
  "BIT-STRING-MOVE!",
  "BIT-STRING-MOVEC!",
  "BIT-STRING-OR!",
  "BIT-STRING-AND!",
  "BIT-STRING-ANDC!",
  "BIT-STRING=?",
  "WORKING-DIRECTORY-PATHNAME",
  "OPEN-DIRECTORY",
  "DIRECTORY-READ",
  "UNDER-EMACS?",
  "TTY-FLUSH-OUTPUT",
  "RELOAD-BAND-NAME",
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name,
  No_Name
};

