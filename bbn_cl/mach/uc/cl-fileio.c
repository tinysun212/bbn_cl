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

/* -*-C-*- */

/* Extended file I/O primitives for common lisp */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"
#include "bignum.h"
#include "cl-fileio.h"
#include <sys/types.h>


Define_Primitive (Prim_CL_File_Open_Fd, 4, "CL-FILE-OPEN-FD")
{
  long direction, if_exists, if_does_not_exist;
  long status, fd, in, out;
  Boolean file_exists_p;

  Primitive_4_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Arg_4_Type(TC_FIXNUM);
  direction = Get_Integer(Arg2);
  if_exists = Get_Integer(Arg3);
  if_does_not_exist = Get_Integer(Arg4);
  fd = OS_cl_file_open(Scheme_String_To_C_String(Arg1),direction,if_exists,if_does_not_exist,
		       &status,&file_exists_p);
  if (!(file_exists_p))
    {
      in = ((direction==CL_FILE_INPUT) || (direction==CL_FILE_IO));
      out = ((direction==CL_FILE_OUTPUT) || (direction==CL_FILE_IO));
      if ((in && (if_does_not_exist==CL_FILE_NIL)) ||
	  (out && ((if_exists==CL_FILE_NIL) ||
		   (if_does_not_exist==CL_FILE_NIL)))) return NIL;
    }
  if (fd == -1)return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  return Make_Signed_Fixnum(fd);
}

Define_Primitive (Prim_CL_File_Close_Fd, 1, "CL-FILE-CLOSE-FD")
{
  int status, result;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  result =  OS_cl_file_close(Get_Integer(Arg1),&status);
  if (result == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  else return NIL;
}

Define_Primitive(prim_cl_file_author, 1, "CL-FILE-AUTHOR")
{
  char *s;
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);

  s = (char *)OS_file_author(Nth_Vector_Loc(Arg1,STRING_CHARS));
  if ((int)s == 0) return NIL;
  return C_String_To_Scheme_String(s);
}

Define_Primitive(prim_cl_file_write_date, 1, "CL-FILE-WRITE-DATE")
{
  time_t time;
  long status;
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);

  status = OS_file_write_date(Nth_Vector_Loc(Arg1,STRING_CHARS),&time);
  if (status != 0) return NIL;
  return blk_to_num(&time,(sizeof(time_t) / sizeof(bigdigit)));
}

Define_Primitive(prim_print_address, 1, "PRINT-ADDRESS")
{
  Primitive_1_Arg();

  printf("%x\n",Datum(Arg1));
  return NIL;
}

Define_Primitive(prim_make_io_error_code, 1, "MAKE-IO-ERROR-CODE")
{
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  return Make_Non_Pointer(TC_IO_ERROR_CODE,Datum(Arg1));
}

Define_Primitive(prim_io_error_code_p, 1, "IO-ERROR-CODE-P")
{
  Primitive_1_Arg();

  if (Type_Code(Arg1) == TC_IO_ERROR_CODE) return TRUTH;
  else return NIL;
}

Define_Primitive(prim_get_io_error_code_num, 1, "GET-IO-ERROR-CODE-NUM")
{
  Primitive_1_Arg();

  Arg_1_Type(TC_IO_ERROR_CODE);
  return Make_Non_Pointer(TC_FIXNUM,Datum(Arg1));
}
