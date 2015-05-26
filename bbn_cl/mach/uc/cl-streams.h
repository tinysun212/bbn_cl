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


#define private static

extern Pointer lexical_reference();

/* 
  We use the Fast Vector ops so that we may directly use
  assignment in the code. The assumption is that any 
  synchronization required between multiple processes operating
  on the same stream will take place before a process accesses
  any fields.
*/

#define St_Ref(P,N)         Fast_User_Vector_Ref((P),(N))

/* Stream field definitions */

#define ST_SIZE                   7

#define st_opvec_index(S)         St_Ref((S),0)   /* Index into the array of operation vectors 
						     (tagged; avoids extra indirect on dispatch) */
#define st_handle(S)              St_Ref((S),1)   /* Type-specific handle, often a Scheme vector, as below */
#define st_direction(S)           St_Ref((S),2)   /* :direction keyword */
#define st_element_type(S)        St_Ref((S),3)   /* :element-type (type symbol) */
#define st_pathname(S)            St_Ref((S),4)   /* Pathname, if stream opened to a file, NIL otherwise */
#define st_nm(S)                  St_Ref((S),5)   /* NM part */
#define st_info(S)                St_Ref((S),6)   /* General "user" (i.e., system hacker)  accessible info */

#define ST_NM_SIZE                1

#define st_nm_col(NM)             St_Ref((NM),0)  /* Column number, if output ascii stream 
						     (not guaranteed under seek) */

/* Specific stream handle (xxx_ST_H) field definitions */

#define FILE_ST_H_SIZE            3

#define file_st_h_buf(H)             St_Ref((H),0)
#define file_st_h_read_string_acc(H) St_Ref((H),1)
#define file_st_h_nm(H)              St_Ref((H),2)

#define FILE_ST_H_NM_SIZE         6

#define file_st_h_nm_fd(NM)               St_Ref((NM),0)
#define file_st_h_nm_buf_index(NM)        St_Ref((NM),1)
#define file_st_h_nm_buf_end(NM)          St_Ref((NM),2) /* 1 greater than the last usable element */
#define file_st_h_nm_pos_base(NM)         St_Ref((NM),3)
                                                         /* 4 unused; used to be max_index */
#define file_st_h_nm_read_string_len(NM)  St_Ref((NM),5)

#define STRING_ST_H_SIZE          2

#define string_st_h_string(H)     St_Ref((H),0)  /* Simple string, for input */
#define string_st_h_acc(H)        St_Ref((H),0)  /* List in which to accumulate chars for output */
#define string_st_h_nm(H)         St_Ref((H),1)

#define STRING_ST_H_NM_SIZE       2

#define string_st_h_nm_len(NM)    St_Ref((NM),0)  /* Number of chars accumulated in output stream acc list */
#define string_st_h_nm_index(NM)  St_Ref((NM),0)
#define string_st_h_nm_end(NM)    St_Ref((NM),1)

#define TTY_ST_H_SIZE             3

#define tty_st_h_read_string_acc(H)    St_Ref((H),0)  /* a string stream for accumulating chars */
#define tty_st_h_unread_char_buf(H)    St_Ref((H),1)  /* a ring buf */
#define tty_st_h_nm(H)                 St_Ref((H),2)

#define TTY_ST_H_NM_SIZE          3

#define tty_st_h_nm_read_char_index(NM)    St_Ref((NM),0)  /* char-to-be-read loc. in ring */
#define tty_st_h_nm_unread_char_offset(NM) St_Ref((NM),1)  /* offset to unread point */
#define tty_st_h_nm_bfio_p(NM)             St_Ref((NM),2)  /* non-zero -> bfio in use (defaults to false) */

#define TTY_UNREAD_BUF_SIZE            16

#define BROAD_ST_H_SIZE           0              /* Will use a list, probably */

#define CONCAT_ST_H_SIZE          3

#define concat_st_h_current(H)         St_Ref((H),0)
#define concat_st_h_streams(H)         St_Ref((H),1)
#define concat_st_h_read_string_acc(H) St_Ref((H),2) /* a string stream for accumulating chars */

#define SYN_ST_H_SIZE             0             /* Is a symbol whose value is a stream */

#define TWO_WAY_ST_H_SIZE         2

#define two_way_st_h_input_st(H)  St_Ref((H),0)
#define two_way_st_h_output_st(H) St_Ref((H),1)

#define ECHO_ST_H_SIZE         3

#define echo_st_h_input_st(H)  St_Ref((H),0)
#define echo_st_h_output_st(H) St_Ref((H),1)
#define echo_st_h_nm(H)        St_Ref((H),2)

#define ECHO_ST_H_NM_SIZE      1

#define echo_st_h_nm_unread_cnt(NM)  St_Ref((NM),0)

/* Operation Vector and Method type definitions */

#define N_OPVECS                 15
#define N_METHODS                9

typedef Pointer (*STREAM_METHOD)();

typedef STREAM_METHOD STREAM_OPVECS[N_OPVECS][N_METHODS];

/* Opvec indices */

#define CLOSED_OPVEC              0
#define FILE_ASCII_INPUT_OPVEC    1
#define FILE_ASCII_OUTPUT_OPVEC   2
#define FILE_ASCII_IO_OPVEC       3
#define FILE_BINARY_INPUT_OPVEC   4
#define FILE_BINARY_OUTPUT_OPVEC  5
#define FILE_BINARY_IO_OPVEC      6
#define STRING_INPUT_OPVEC        7
#define STRING_OUTPUT_OPVEC       8
#define TTY_OPVEC                 9
#define BROAD_OPVEC              10
#define CONCAT_OPVEC             11
#define SYN_OPVEC                12
#define TWO_WAY_OPVEC            13
#define ECHO_OPVEC               14

/* Method Indices */

#define READ_CHAR_METH            0
#define UNREAD_CHAR_METH          1
#define LISTEN_METH               2
#define READ_BYTE_METH            3
#define WRITE_STRING_METH         4
#define WRITE_CHAR_METH           5
#define WRITE_BYTE_METH           6
#define READ_STRING_METH          7
#define MISC_METH                 8   /* Includes clear-input, finish-output, force-output,
					 clear-output, close, file-position, file-length, and
					 isa-tty-p. Constants are defined below.
					 Note that all of these are non-performance-intensive
					 and accept a simple arg structure (either one arg
					 or two) */

/* Misc-Method constants */

#define CLEAR_INPUT_METH          0
#define FORCE_OUTPUT_METH         1
#define FINISH_OUTPUT_METH        2
#define CLEAR_OUTPUT_METH         3
#define CLOSE_METH                4
#define FILE_POSITION_METH        5
#define FILE_LENGTH_METH          6
#define ISA_TTY_P_METH            7
#define ELEMENT_TYPE_METH         8
#define DIRECTION_METH            9
#define CHARPOS_METH             10
#define PATHNAME_METH            11


#define Dispatch_Stream_Method(Stream,Method_Index)                             \
  (*stream_opvecs[Get_Integer(st_opvec_index(Stream))][Method_Index])

/*
  Resolve stream arg to an input call
*/

#define Resolve_Input_Stream(Arg_No)                                            \
  if (Number_Of_Args >= (Arg_No))                                               \
    {                                                                           \
      stream = Primitive_Variable_Arg((Arg_No));                                \
      Touch_In_Primitive(stream, stream);                                       \
      if (Type_Code(stream) != TC_CL_STREAM)                                    \
       {                                                                        \
	  if (stream == NIL)                                                    \
	    stream = lexical_reference(NIL, Sym_star_standard_input_star);      \
	  else if (stream == TRUTH)                                             \
	    stream = lexical_reference(NIL, Sym_star_terminal_io_star);         \
	  else stream_type_error(stream);                                       \
	}                                                                       \
    }                                                                           \
  else                                                                          \
    stream = lexical_reference(NIL, Sym_star_standard_input_star);

/*
  Resolve stream arg to an output call
*/

#define Resolve_Output_Stream(Arg_No)                                           \
  if (Number_Of_Args >= (Arg_No))                                               \
    {                                                                           \
      stream = Primitive_Variable_Arg((Arg_No));                                \
      Touch_In_Primitive(stream, stream);                                       \
      if (Type_Code(stream) != TC_CL_STREAM)                                    \
	{                                                                       \
	  if (stream == NIL)                                                    \
	    stream = lexical_reference(NIL, Sym_star_standard_output_star);     \
	  else if (stream == TRUTH)                                             \
	    stream = lexical_reference(NIL, Sym_star_terminal_io_star);         \
	  else stream_type_error(stream);                                       \
	}                                                                       \
    }                                                                           \
  else                                                                          \
    stream = lexical_reference(NIL, Sym_star_standard_output_star);

/*
  Setup the standard args to an input call.
*/

#define Resolve_Default_Input_Stream_Args(START_ARGNO)                          \
  if (Number_Of_Args >= START_ARGNO)                                            \
  { eof_error_p = Primitive_Variable_Arg(START_ARGNO);                          \
    Touch_In_Primitive(eof_error_p, eof_error_p);                               \
  }                                                                             \
  else                                                                          \
    eof_error_p = TRUTH;                                                        \
  if (Number_Of_Args >= START_ARGNO + 1)                                        \
  { eof_value = Primitive_Variable_Arg(START_ARGNO + 1);                        \
    Touch_In_Primitive(eof_value, eof_value);                                   \
  }                                                                             \
  else                                                                          \
    eof_value = NIL;                                                            \
  if (Number_Of_Args >= START_ARGNO + 2)                                        \
  { recursive_p = Primitive_Variable_Arg(START_ARGNO + 2);                      \
    Touch_In_Primitive(stream, stream);                                         \
  }                                                                             \
  else                                                                          \
    recursive_p = NIL;

#define Check_At_Most_N_Args(N)                                                 \
  if (Number_Of_Args > (N))                                                     \
    error_too_many_args((N))

#define Check_Type_Of_Arg(ARGNO, TYPE_CODE)                                     \
  if (Type_Code(Primitive_Variable_Arg((ARGNO))) != (TYPE_CODE))                \
    arg_type_error(Primitive_Variable_Arg((ARGNO)),TYPE_CODE);

#define Newline_Char   Make_Non_Pointer(TC_CHARACTER, '\n')

/* Options to c_set_file_position */

#define POS_END    -1

/* File stream buffer size */

#define FILE_STREAM_BUFFER_SIZE    2048

/* Binary stream utilities */

#define OS_BPE                     8    /* The basic size of a datum in a file */

#define Binary_Stream_P(S)         (st_element_type((S)) != Sym_string_char)

#define Abs(X)                     (X < 0 ? (- X) : X)

#define BPE(S)                     (Abs(cl_ivector_subtype(file_st_h_buf(st_handle((S))))))
