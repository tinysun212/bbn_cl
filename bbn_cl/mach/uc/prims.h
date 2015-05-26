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


/* $Header: prims.h,v 10.0 88/12/07 13:09:49 las Exp $

This file contains the information needed for the basic primitives. */

/* Information on primitives:
 *
 * Primitives are routines written in the interpreter implementation
 * language (C in this case) which accept Scheme objects as arguments
 * and return a Scheme object as their value.  Some of them (APPLY,
 * SCODE-EVAL, CATCH) are really entry points into the interpreter,
 * and in other languages or implementations would be special forms.
 * These depend on the way the interpreter works and should not be
 * modified.  Most (all?) of these "interpreter hooks" live in the
 * file HOOKS.C
 *
 * Most primitives are basic in some sense.  Their functionality
 * cannot easily be achieved with a Scheme procedure unless they are
 * defined (eg, CAR, PRIMITIVE-TYPE, GET-CHARACTER, etc).  Some exist
 * only for efficiency, they are very common and easily written in
 * the interpreter implementation language, and this extra work is
 * worth the speed that they provide (eg LENGTH, MEMQ, etc).
 *
 * The runtime system depends on many primitives (the others are
 * fossils or used by specialized subsystems only).  Users may,
 * however, add new primitives to suit their applications (graphics,
 * matrix multiplication, etc.), and are encouraged to do so.
 *
 * The system provides some facilities for declaring new primitives,
 * which can then be accessed from Scheme.  The linking of the C code
 * which implements these user-supplied (we call them "external")
 * primitives is done at system (microcode) generation time (e.g.
 * when you use "make" to link a new version of scheme under Unix).
 * These externals are handled in such a way that Scheme programs
 * which use them can be loaded on any system, including
 * systems that do not have those primitives.  If you run a program
 * which tries to apply one of these undefined externals to arguments,
 * you will, of course, get an error.



 * The original primitives (provided with Cscheme) are not linked in
 * the same way for historical reasons, and must always exist.  They
 * will eventually be linked in the same or similar way that user
 * defined primitives are linked, and then there will be no difference
 * between both kinds.  Currently, there are tables in STORAGE.C with
 * information on these original, or "built in" primitives.  These
 * tables are needed for the old linking mechanism, but need not be
 * extended when adding user defined primitives, since the equivalent
 * tables are constructed at microcode generation time.  If you are
 * curious, look at the files USRDEF.C, created at microcode generation
 * time, and FINDPRIM.C, the program which creates it.
 *
 * Primitives have 2 fixed names: The C name (the name of the C
 * routine that implements them), and the Scheme linking name.  The
 * latter is the name which when given to MAKE-PRIMITIVE-PROCEDURE
 * will return the appropriate procedure object.  Note that this name
 * is not usually initially bound in Scheme, and it may be bound to
 * anything at all, though for some of the most common primitives it
 * is bound to the primitive object.  In other words, you must choose
 * names for your primitives so that neither the C name nor the
 * linking name have conflicts with other linked primitives, but you
 * need not worry about name conflicts with the Scheme environment.
 * Note also that this name is currently ignored for "built in",
 * required primitives.
 *
 * Define_Primitive is a macro used to declare a primitive to the
 * linking mechanism.  It is both a C macro (defined in primitive.h)
 * and a special name used by the program Findprim to generate a
 * special file used when a new scheme microcode is linked together.
 * Notice that you must be sure to run Findprim ONLY on the files
 * containing user defined primitives, and NOT the system supplied
 * primitives.  For UNIX users, you should look at the top few lines
 * of the Makefile and add the names of your files.  In particular,
 * this means that you cannot have both kinds of primitives in one
 * file.  Don't add primitives to "system" files, add your own files
 * instead.
 *
 * Define_Primitive expects 3 arguments which are the C name, the
 * number of arguments that the primitive expects (currently
 * primitives cannot receive variable numbers of arguments), and the
 * Scheme linking name.  Original primitives make use of
 * Built_In_Primitive and the primitive numbers defined below for
 * linking purposes instead of using this symbolic linking mechanism.
 *
 * One last note: Unless very special care is taken (and we do not
 * recommend it), primitives cannot call other primitives.  If you
 * want to share code between two primitives, write a third procedure
 * which implements the common work, and have both primitives call it
 * as appropriate.  The reason for this is that the primitive calling
 * convention may change without notice, and the way arguments
 * are accessed may therefore change.  As long as you only use
 * Primitive_N_Args to access them, your code will continue to work.
 * In particular, if you write primitives which do not expect any
 * arguments, make sure to invoke Primitive_0_Args anyway since it will
 * take care of any initialization needed.  Define_Primitive,
 * Built_In_Primitive, Primitive_N_Args, and other utilities for
 * primitives are defined in the file PRIMITIVE.H.
 *
 * For examples of user defined primitives, look at the files SAMPLE.C
 * and XDEBUG.C.  For an example on how to access these primitives
 * from scheme look at sample.scm and xdebug.scm.
 */

                      /************************/
                      /* PRIMITIVE OPERATIONS */
                      /************************/

#define PC_LEXICAL_ASSIGNMENT           0x00
#define PC_LOCAL_REFERENCE              0x01
#define PC_LOCAL_ASSIGNMENT             0x02
#define PC_CATCH                        0x03
#define PC_SCODE_EVAL                   0x04
#define PC_APPLY                        0x05
#define PC_SET_INTERRUPT_ENABLES        0x06
#define PC_STRING_TO_SYMBOL		0x07
#define PC_GET_WORK                     0x08	/* Futures only */
#define PC_NON_REENTRANT_CATCH	        0x09
#define PC_GET_DYNAMIC_STATE		0x0A
#define PC_SET_DYNAMIC_STATE		0x0B
#define PC_NOT                          0x0C
#define PC_EQ                           0x0D
#define PC_STRING_EQUAL                 0x0E
#define PC_PRIM_TYPE_QM                 0x0F
#define PC_PRIM_TYPE                    0x10
#define PC_PRIMITIVE_SET_TYPE           0x11
#define PC_LEXICAL_REFERENCE            0x12
#define PC_UNREFERENCEABLE_TEST         0x13
#define PC_MAKE_CHAR			0x14
#define PC_CHAR_BITS			0x15
#define PC_NON_RESTARTABLE_EXIT         0x16
#define PC_CHAR_CODE			0x17
#define PC_UNASSIGNED_TEST              0x18
#define PC_INSERT_NON_MARKED_VECTOR     0x19
#define PC_RESTARTABLE_EXIT             0x1A
#define PC_CHAR_TO_INTEGER		0x1B
#define PC_MEMQ                         0x1C
#define PC_INSERT_STRING                0x1D
#define PC_ENABLE_INTERRUPTS            0x1E
#define PC_MAKE_EMPTY_STRING            0x1F
#define PC_CONS                         0x20
#define PC_CAR                          0x21
#define PC_CDR                          0x22
#define PC_SET_CAR                      0x23
#define PC_SET_CDR                      0x24

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_PRINT_STRING                 0x25
#define PC_TTY_GET_CURSOR               0x26
#define PC_GENERAL_CAR_CDR              0x27
#define PC_HUNK3_CONS                   0x28
#define PC_HUNK3_CXR                    0x29
#define PC_HUNK3_SET_CXR                0x2A
#define PC_OVERWRITE_STRING             0x2B
#define PC_VECTOR_CONS                  0x2C
#define PC_VECTOR_SIZE                  0x2D
#define PC_VECTOR_REF                   0x2E
#define PC_SET_CURRENT_HISTORY          0x2F
#define PC_VECTOR_SET                   0x30
#define PC_NON_MARKED_VECTOR_CONS       0x31
#define PC_GET_CHAR                     0x32
#define PC_UNBOUND_TEST                 0x33
#define PC_INTEGER_TO_CHAR		0x34
#define PC_CHAR_DOWNCASE		0x35
#define PC_CHAR_UPCASE			0x36
#define PC_ASCII_TO_CHAR		0x37
#define PC_CHAR_ASCII_P			0x38
#define PC_CHAR_TO_ASCII		0x39
#define PC_GARBAGE_COLLECT              0x3A
#define PC_PLUS_FIXNUM                  0x3B
#define PC_MINUS_FIXNUM                 0x3C
#define PC_MULTIPLY_FIXNUM              0x3D
#define PC_DIVIDE_FIXNUM                0x3E
#define PC_EQUAL_FIXNUM                 0x3F
#define PC_LESS_FIXNUM                  0x40
#define PC_POSITIVE_FIXNUM              0x41
#define PC_ONE_PLUS_FIXNUM              0x42
#define PC_M_ONE_PLUS_FIXNUM	        0x43
#define PC_TRUNCATE_STRING              0x44
#define PC_SUBSTRING                    0x45
#define PC_ZERO_FIXNUM                  0x46
#define PC_UNDANGERIZE                  0x47
#define PC_DANGERIZE                    0x48
#define PC_DANGEROUS_QM                 0x49
#define PC_SUBSTRING_TO_LIST            0x4A
#define PC_MAKE_FILLED_STRING           0x4B
#define PC_PLUS_BIGNUM                  0x4C
#define PC_MINUS_BIGNUM                 0x4D
#define PC_MULTIPLY_BIGNUM              0x4E
#define PC_DIVIDE_BIGNUM                0x4F

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_LISTIFY_BIGNUM               0x50
#define PC_EQUAL_BIGNUM                 0x51
#define PC_LESS_BIGNUM                  0x52
#define PC_POSITIVE_BIGNUM              0x53
#define PC_FILE_OPEN_CHANNEL            0x54
#define PC_FILE_CLOSE_CHANNEL 		0x55
#define PC_PRIMITIVE_FASDUMP            0x56
#define PC_FASLOAD               	0x57
#define PC_STRING_POSITION              0x58
#define PC_STRING_LESS                  0x59
/* #define PC_OBJECT_HASH                  0x5A */
/* #define PC_OBJECT_UNHASH                0x5B */
#define PC_REHASH                       0x5C
#define PC_LENGTH                       0x5D
#define PC_ASSQ                         0x5E
#define PC_BUILD_STRING_FROM_LIST       0x5F
#define PC_EQUAL_STRING_TO_LIST         0x60
#define PC_MAKE_CELL                    0x61
#define PC_CELL_CONTENTS                0x62
#define PC_CELL 	                0x63
#define PC_RAISE_CHAR                   0x64
#define PC_CHARACTER_LIST_HASH          0x65
#define PC_GCD_FIXNUM                   0x66
#define PC_FIX_TO_BIG                   0x67
#define PC_BIG_TO_FIX                   0x68
#define PC_PLUS_FLONUM                  0x69
#define PC_MINUS_FLONUM                 0x6A
#define PC_MULTIPLY_FLONUM              0x6B
#define PC_DIVIDE_FLONUM                0x6C
#define PC_EQUAL_FLONUM                 0x6D
#define PC_LESS_FLONUM                  0x6E
#define PC_ZERO_BIGNUM                  0x6F
#define PC_TRUNCATE_FLONUM              0x70
#define PC_ROUND_FLONUM                 0x71
#define PC_INT_TO_FLOAT                 0x72
#define PC_SINE_FLONUM                  0x73
#define PC_COSINE_FLONUM                0x74
#define PC_ARCTAN_FLONUM                0x75
#define PC_EXP_FLONUM                   0x76
#define PC_LN_FLONUM                    0x77

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_SQRT_FLONUM                  0x78
#define PC_ASCII_FASLOAD                0x79
#define PC_GET_FIXED_OBJECTS_VECTOR     0x7A
#define PC_SET_FIXED_OBJECTS_VECTOR     0x7B
#define PC_LIST_TO_VECTOR               0x7C
#define PC_SUBVECTOR_TO_LIST            0x7D
#define PC_PAIR                         0x7E
#define PC_NEGATIVE_FIXNUM              0x7F
#define PC_NEGATIVE_BIGNUM              0x80
#define PC_GREATER_FIXNUM               0x81
#define PC_GREATER_BIGNUM               0x82
#define PC_STRING_HASH                  0x83
#define PC_SYS_PAIR_CONS                0x84
#define PC_SYS_PAIR                     0x85
#define PC_SYS_PAIR_CAR                 0x86
#define PC_SYS_PAIR_CDR                 0x87
#define PC_SYS_SET_CAR                  0x88
#define PC_SYS_SET_CDR                  0x89
/* #define PC_INITIALIZE_OBJECT_HASH       0x8A */
#define PC_GET_CHAR_IMMEDIATE           0x8B
#define PC_SET_CELL_CONTENTS            0x8C
#define PC_AND_MAKE_OBJECT		0x8D
#define PC_SYS_H3_0                     0x8E
#define PC_SYS_H3_SET_0                 0x8F
#define PC_MAP_ADDRESS_TO_CODE          0x90
#define PC_SYS_H3_1                     0x91
#define PC_SYS_H3_SET_1                 0x92
#define PC_MAP_CODE_TO_ADDRESS          0x93
#define PC_SYS_H3_2                     0x94
#define PC_SYS_H3_SET_2                 0x95
#define PC_MAP_PRIM_ADDRESS_TO_ARITY    0x96
#define PC_SYS_LIST_TO_VECTOR           0x97
#define PC_SYS_SUBVECTOR_TO_LIST        0x98
#define PC_SYS_VECTOR                   0x99
#define PC_SYS_VECTOR_REF               0x9A
#define PC_SYS_VECTOR_SET               0x9B
#define PC_WITH_HISTORY_DISABLED        0x9C

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

/* #define PC_VECTOR_1B_CONS               0x9D */
/* #define PC_VECTOR_1B                    0x9E */
/* #define PC_VECTOR_1B_REF                0x9F */
/* #define PC_VECTOR_1B_SET                0xA0 */
/* #define PC_VEC_1B_SET_FALSE             0xA1 */
/* #define PC_VEC_1B_SET_TRUE              0xA2 */
#define PC_VECTOR_8B_CONS               0xA3
#define PC_VECTOR_8B                    0xA4
#define PC_VECTOR_8B_REF                0xA5
#define PC_VECTOR_8B_SET                0xA6
#define PC_ZERO_FLONUM                  0xA7
#define PC_POSITIVE_FLONUM              0xA8
#define PC_NEGATIVE_FLONUM              0xA9
#define PC_GREATER_FLONUM               0xAA
#define PC_INTERN_CHARACTER_LIST        0xAB
/* #define PC_VECTOR_1B_SIZE               0xAC */
#define PC_VECTOR_8B_SIZE               0xAD
#define PC_SYS_VECTOR_SIZE              0xAE
#define PC_FORCE                        0xAF
#define PC_PRIMITIVE_DATUM              0xB0
#define PC_MAKE_NON_POINTER             0xB1
#define PC_TEMP_PRINTER                 0xB2
#define PC_RAISE_STRING                 0xB3
#define PC_PRIMITIVE_PURIFY             0xB4
/*	UNUSED				0xB5 */
#define PC_COMPLETE_GARBAGE_COLLECT     0xB6
#define PC_BAND_DUMP                    0xB7
#define PC_SUBSTRING_SEARCH             0xB8
#define PC_BAND_LOAD                    0xB9
#define PC_CONSTANT_P                   0xBA
#define PC_PURE_P                       0xBB
#define PC_GC_TYPE                      0xBC
#define PC_IMPURIFY                     0xBD
#define PC_WITH_THREADED_STACK          0xBE
#define PC_WITHIN_CONTROL_POINT         0xBF
#define PC_SET_RUN_LIGHT                0xC0
#define PC_FILE_EOF_P			0xC1
#define PC_FILE_READ_CHAR		0xC2
#define PC_FILE_FILL_INPUT_BUFFER	0xC3
#define PC_FILE_LENGTH			0xC4
#define PC_FILE_WRITE_CHAR		0xC5
#define PC_FILE_WRITE_STRING		0xC6

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_CLOSE_LOST_OPEN_FILES        0xC7
#define PC_PUT_CHAR_TO_OUTPUT_CHANNEL   0xC8
#define PC_WITH_INTERRUPTS_REDUCED      0xC9
#define PC_EVAL_STEP                    0xCA
#define PC_APPLY_STEP                   0xCB
#define PC_RETURN_STEP                  0xCC
#define PC_TTY_READ_CHAR_READY_P	0xCD
#define PC_TTY_READ_CHAR		0xCE
#define PC_TTY_READ_CHAR_IMMEDIATE	0xCF
#define PC_TTY_READ_FINISH		0xD0
#define PC_BIT_STRING_ALLOCATE          0xD1
#define PC_MAKE_BIT_STRING              0xD2
#define PC_BIT_STRING_P                 0xD3
#define PC_BIT_STRING_LENGTH            0xD4
#define PC_BIT_STRING_REF               0xD5
#define PC_BIT_SUBSTRING_MOVE_RIGHT_X   0xD6
#define PC_BIT_STRING_SET_X             0xD7
#define PC_BIT_STRING_CLEAR_X           0xD8
/* #define PC_BIT_SUBSTRING                0xD9 */
/* #define PC_INST_BIT_STR                 0xDA */
/* #define PC_INST_BIT_STR_EXCL	        0xDB */
#define PC_UNSIGNED_INTEGER_TO_BIT_STRING 0xDC
#define PC_BIT_STRING_TO_UNSIGNED_INTEGER 0xDD
/* #define PC_REVERSE_BIT_STRING           0xDE */
#define PC_READ_BITS_X                  0xDF
#define PC_WRITE_BITS_X                 0xE0
#define PC_MAKE_STATE_SPACE             0xE1
#define PC_EXECUTE_AT_NEW_POINT         0xE2
#define PC_TRANSLATE_TO_POINT           0xE3
#define PC_GET_NEXT_CONSTANT		0xE4

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_MICROCODE_IDENTIFY		0xE5
#define PC_ZERO                         0xE6
#define PC_POSITIVE                     0xE7
#define PC_NEGATIVE                     0xE8
#define PC_EQUAL_NUMBER			0xE9
#define PC_LESS       			0xEA
#define PC_GREATER    		        0xEB
#define PC_PLUS			        0xEC
#define PC_MINUS		        0xED
#define PC_MULTIPLY			0xEE
#define PC_DIVIDE		       	0xEF
#define PC_INTEGER_DIVIDE           	0xF0
#define PC_ONE_PLUS			0xF1
#define PC_M_ONE_PLUS			0xF2
#define PC_TRUNCATE		        0xF3
#define PC_ROUND		        0xF4
#define PC_FLOOR		        0xF5
#define PC_CEILING		        0xF6
#define PC_SQRT			        0xF7
#define PC_EXP				0xF8
#define PC_LN				0xF9
#define PC_SINE				0xFA
#define PC_COSINE			0xFB
#define PC_ARCTAN		        0xFC
#define PC_TTY_WRITE_CHAR		0xFD
#define PC_TTY_WRITE_STRING		0xFE
#define PC_TTY_BEEP			0xFF
#define PC_TTY_CLEAR			0x100
#define PC_GET_EXTERNAL_COUNTS		0x101
#define PC_GET_EXT_NAME			0x102
#define PC_GET_EXT_NUMBER		0x103
/* #define PC_OPEN_CHANNEL			0x104 */
/* #define PC_CLOSE_PHYSICAL_CHANNEL	0x105 */
#define PC_GET_NEXT_INTERRUPT_CHAR	0x106
#define PC_CHK_AND_CLN_INPUT_CHANNEL	0x107
#define PC_INITIALIZE_MICROCODE_DEBUG	0x108
#define PC_SYSTEM_CLOCK			0x109
#define PC_FILE_EXISTS			0x10A
/* #define PC_DELETE_FILE			0x10B */
#define PC_TTY_MOVE_CURSOR		0x10C
/* #define PC_INTERNAL_PHOTO		0x10D */
#define PC_CURRENT_DATE			0x10E

/* Primitive operations continue on the next page */

/* Primitive operations, continued */

#define PC_CURRENT_TIME			0x10F
#define PC_TRANSLATE_FILE		0x110
#define PC_COPY_FILE			0x111
#define PC_RENAME_FILE			0x112
#define PC_REMOVE_FILE			0x113
#define PC_LINK_FILE			0x114
#define PC_MAKE_DIRECTORY		0x115
#define PC_VOLUME_NAME			0x116
#define PC_SET_WORKING_DIRECTORY_PATHNAME_X	0x117
#define PC_OPEN_CATALOG			0x118
#define PC_CLOSE_CATALOG		0x119
#define PC_NEXT_FILE			0x11A
#define PC_CAT_NAME			0x11B
#define PC_CAT_KIND			0x11C
#define PC_CAT_PSIZE			0x11D
#define PC_CAT_LSIZE			0x11E
#define PC_CAT_INFO			0x11F
#define PC_CAT_BLOCK			0x120
#define PC_CAT_CREATE_DATE		0x121
#define PC_CAT_CREATE_TIME		0x122
#define PC_CAT_LAST_DATE		0x123
#define PC_CAT_LAST_TIME		0x124
#define PC_ERROR_MESSAGE		0x125
#define PC_CURRENT_YEAR			0x126
#define PC_CURRENT_MONTH		0x127
#define PC_CURRENT_DAY			0x128
#define PC_CURRENT_HOUR			0x129
#define PC_CURRENT_MINUTE		0x12A
#define PC_CURRENT_SECOND		0x12B
#define PC_INIT_FLOPPY			0x12C
#define PC_ZERO_FLOPPY			0x12D
#define PC_PACK_VOLUME			0x12E
#define PC_LOAD_PICTURE			0x12F
#define PC_STORE_PICTURE		0x130
#define PC_LOOKUP_SYSTEM_SYMBOL		0x131

/* Unix specialized primitives start here */

/* Unused		       		0x132 */
#define PC_CLEAR_SCREEN			0x133
#define PC_CLEAR_TO_END_OF_LINE		0x134
#define PC_NUMBER_OF_COLUMNS		0x135
#define PC_NUMBER_OF_LINES		0x136

#define PC_WITH_INTERRUPT_MASK		0x137


/* New String Primitives */

#define PC_STRING_P			0x138
#define PC_STRING_LENGTH		0x139
#define PC_STRING_REF			0x13A
#define PC_STRING_SET			0x13B
#define PC_SUBSTRING_MOVE_RIGHT		0x13C
#define PC_SUBSTRING_MOVE_LEFT		0x13D
#define PC_STRING_ALLOCATE		0x13E
#define PC_STRING_MAXIMUM_LENGTH	0x13F
#define PC_SET_STRING_LENGTH		0x140

/* 8b vector primitives */

/* These have been in the system for a while.
   PC_VECTOR_8B_CONS 		        0xA3
   PC_VECTOR_8B                         0xA4
   PC_VECTOR_8B_REF                     0xA5
   PC_VECTOR_8B_SET                     0xA6
   PC_VECTOR_8B_SIZE                    0xAD
 */

#define PC_VECTOR_8B_FILL		0x141
#define PC_VECTOR_8B_FIND_NEXT_CHAR	0x142
#define PC_VECTOR_8B_FIND_PREVIOUS_CHAR 0x143
#define PC_VECTOR_8B_FIND_NEXT_CHAR_CI  0x144
#define PC_VECTOR_8B_FIND_PREVIOUS_CHAR_CI 0x145

/* Substring primitives */

#define PC_SUBSTRING_FIND_NEXT_CHAR_IN_SET     0x146
#define PC_SUBSTRING_FIND_PREVIOUS_CHAR_IN_SET 0x147
#define PC_SUBSTRING_EQUAL              0x148
#define PC_SUBSTRING_CI_EQUAL		0x149
#define PC_SUBSTRING_LESS		0x14A
#define PC_SUBSTRING_UPCASE		0x14B
#define PC_SUBSTRING_DOWNCASE		0x14C
#define PC_SUBSTRING_MATCH_FORWARD	0x14D
#define PC_SUBSTRING_MATCH_BACKWARD	0x14E
#define PC_SUBSTRING_MATCH_FORWARD_CI	0x14F
#define PC_SUBSTRING_MATCH_BACKWARD_CI  0x150

#define PC_PHOTO_OPEN			0x151
#define PC_PHOTO_CLOSE			0x152
#define PC_SETUP_TIMER_INTERRUPT        0x153

/* Primitives that are not yet implemented */

/* Unused                               0x154 */
/* Unused                               0x155 */
/* Unused			        0x156 */
/* Unused			        0x157 */
/* Unused			        0x158 */
/* Unused			        0x159 */
/* Unused			        0x15A */
/* Unused			        0x15B */
/* Unused			        0x15C */
/* Unused			        0x15D */
/* Unused			        0x15E */
/* Unused			        0x15F */
/* Unused			        0x160 */

#define PC_EXTRACT_NON_MARKED_VECTOR    0x161
#define PC_UNSNAP_LINKS		        0x162
#define PC_SAFE_PRIMITIVE_P	        0x163
#define PC_SUBSTRING_READ	        0x164
#define PC_SUBSTRING_WRITE	        0x165
#define PC_SCREEN_X_SIZE	        0x166
#define PC_SCREEN_Y_SIZE	        0x167
#define PC_SCREEN_WRITE_CURSOR	        0x168
#define PC_SCREEN_WRITE_CHARACTER       0x169
#define PC_SCREEN_WRITE_SUBSTRING       0x16A 
#define PC_NEXT_FILE_MATCHING	        0x16B
/* Unused 			        0x16C */
#define PC_TTY_WRITE_BYTE	        0x16D
#define PC_FILE_READ_BYTE	        0x16E
#define PC_FILE_WRITE_BYTE	        0x16F
#define PC_SAVE_SCREEN		        0x170
#define PC_RESTORE_SCREEN	        0x171
#define PC_SUBSCREEN_CLEAR	        0x172
#define PC_AND_GCD		        0x173
#define PC_TTY_REDRAW_SCREEN	        0x174
#define PC_SCREEN_INVERSE_VIDEO	        0x175
#define PC_STRING_TO_SYNTAX_ENTRY       0x176
#define PC_SCAN_WORD_FORWARD	        0x177
#define PC_SCAN_WORD_BACKWARD	        0x178
#define PC_SCAN_LIST_FORWARD	        0x179
#define PC_SCAN_LIST_BACKWARD	        0x17A
#define PC_SCAN_SEXPS_FORWARD	        0x17B
#define PC_SCAN_FORWARD_TO_WORD	        0x17C
#define PC_SCAN_BACKWARD_PREFIX_CHARS   0x17D
#define PC_CHAR_TO_SYNTAX_CODE	        0x17E
#define PC_QUOTED_CHAR_P	        0x17F
#define PC_MICROCODE_TABLES_FILENAME    0x180
/* Unused			        0x181 */
#define PC_FIND_PASCAL_PROGRAM	        0x182
#define PC_EXECUTE_PASCAL_PROGRAM       0x183
#define PC_GRAPHICS_MOVE	        0x184
#define PC_GRAPHICS_LINE	        0x185
#define PC_GRAPHICS_PIXEL	        0x186
#define PC_GRAPHICS_SET_DRAWING_MODE    0x187
#define PC_ALPHA_RASTER_P	        0x188
#define PC_TOGGLE_ALPHA_RASTER	        0x189
#define PC_GRAPHICS_RASTER_P	        0x18A
#define PC_TOGGLE_GRAPHICS_RASTER       0x18B
#define PC_GRAPHICS_CLEAR	        0x18C
#define PC_GRAPHICS_SET_LINE_STYLE      0x18D
#define PC_ERROR_PROCEDURE	        0x18E
#define PC_VOLUME_EXISTS_P	        0x18F
#define PC_RE_CHAR_SET_ADJOIN           0x190
#define PC_RE_COMPILE_FASTMAP	        0x191
#define PC_RE_MATCH		        0x192
#define PC_RE_SEARCH_FORWARD	        0x193
#define PC_RE_SEARCH_BACKWARD	        0x194
#define PC_SYS_MEMORY_REF	        0x195
#define PC_SYS_MEMORY_SET	        0x196
#define PC_BIT_STRING_FILL_X            0x197
#define PC_BIT_STRING_MOVE_X            0x198
#define PC_BIT_STRING_MOVEC_X           0x199
#define PC_BIT_STRING_OR_X              0x19A
#define PC_BIT_STRING_AND_X             0x19B
#define PC_BIT_STRING_ANDC_X            0x19C
#define PC_BIT_STRING_EQUAL_P           0x19D
#define PC_WORKING_DIRECTORY_PATHNAME   0x19E
#define PC_OPEN_DIRECTORY               0x19F
#define PC_DIRECTORY_READ               0x1A0
#define PC_UNDER_EMACS_P                0x1A1
#define PC_TTY_FLUSH_OUTPUT             0x1A2
#define PC_RELOAD_BAND_NAME		0x1A3

#define MAX_PRIMITIVE_NUMBER            0x1A3
