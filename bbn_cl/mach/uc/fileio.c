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

/* $Header: fileio.c,v 10.0 88/12/07 13:07:23 las Exp $
 * $MIT-Header: fileio.c,v 9.27 87/12/15 16:07:06 GMT cph Exp $
 */

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"

extern FILE   *OS_file_open();
extern Boolean OS_file_close();
extern Boolean OS_file_eof_p();
extern long    OS_file_length();
extern long    OS_file_read_chars();
extern Boolean OS_file_write_chars();
extern Boolean OS_file_copy();
extern Boolean OS_file_rename();
extern Boolean OS_file_remove();
extern Boolean OS_file_link_physical();
extern Boolean OS_file_link_symbolic();
extern Pointer OS_working_dir_pathname();
extern Boolean OS_set_working_dir_pathname();
extern Boolean OS_directory_make();
extern Pointer OS_directory_open();
extern Pointer OS_directory_read();

#define CHANNEL_NUMBER_TO_LONG(location, argument, argument_number)	\
{									\
  if (! (FIXNUM_P (argument)))						\
    error_wrong_type_arg (argument_number);				\
  (location) = (UNSIGNED_FIXNUM_VALUE (argument));			\
  if (((location) < 0) || ((location) > FILE_CHANNELS))			\
    error_bad_range_arg (argument_number);				\
  if (((Channels [(location)]) == ((FILE *) NULL)) ||			\
      ((Channels [(location)]) == ((FILE *) (-1))))			\
    error_bad_range_arg (argument_number);				\
}

static long
channel_to_long (argument_number)
     int argument_number;
{
  fast Pointer argument;
  fast Pointer channel;
  fast long channel_number;

  argument = (ARG_REF (argument_number));
  if ((OBJECT_TYPE (argument)) != TC_HUNK3)
    error_wrong_type_arg (argument_number);
  channel = (Vector_Ref (argument, 0));
  Touch_In_Primitive (channel, channel);
  CHANNEL_NUMBER_TO_LONG (channel_number, channel, argument_number);
  return (channel_number);
}

FILE *
arg_channel (argument_number)
     int argument_number;
{
  return (Channels [(channel_to_long (argument_number))]);
}

/* (FILE-OPEN-CHANNEL filename output?)
   Open a file called FILENAME, returning a channel number.
   The argument OUTPUT?, if false, means open an input file.
   Otherwise, open an output file. */

DEFINE_PRIMITIVE ("FILE-OPEN-CHANNEL", Prim_File_Open_Channel, 2)
{
  long i;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);

#ifdef butterfly
  while (atomior(&SHARED_DATA->File_Open_Lock, 0x8000) != 0)
    Standard_Delay();
#endif /* butterfly */

  for (i = 1; (i <= FILE_CHANNELS); i++)
    if ((Channels [i]) == NULL)
    {

#ifdef butterfly
      Channels[i] = ((FILE *) (-1));
      SHARED_DATA->File_Open_Lock = 0;
#endif /* butterfly */

      Channels[i] =
	OS_file_open ((Scheme_String_To_C_String (ARG_REF(1))), (ARG_REF(2) != NIL));
      if ((Channels[i]) == NULL)
      {
	Pointer string, flag;

	flag = ARG_REF(2);
	string = ARG_REF(1);
	Primitive_Error_String(((flag == NIL) ?
				"File not found" :
				"Unable to create file"),
			       string);
      }
      Open_File_Hook (i);
      PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (i));
    }

#ifdef butterfly
  SHARED_DATA->File_Open_Lock = 0;
#endif /* butterfly */

  Primitive_Error (ERR_OUT_OF_FILE_HANDLES);
}

/* (FILE-CLOSE-CHANNEL channel_number)
   Closes the file represented by channel_number. */

DEFINE_PRIMITIVE ("FILE-CLOSE-CHANNEL", Prim_File_Close_Channel, 1)
{
  long channel;
  Boolean result;
  PRIMITIVE_HEADER (1);

  CHANNEL_NUMBER_TO_LONG (channel, (ARG_REF (1)), 1);
  result = (OS_file_close (Channels [channel]));
  (Channels [channel]) = NULL;
  Close_File_Hook ();
  if (! result)
    error_external_return ();
  PRIMITIVE_RETURN (TRUTH);
}

DEFINE_PRIMITIVE ("FILE-EOF?", Prim_File_Eof_P, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN ((OS_file_eof_p (arg_channel (1))) ? TRUTH : NIL);
}

DEFINE_PRIMITIVE ("FILE-LENGTH", Prim_File_Length, 1)
{
  PRIMITIVE_HEADER (1);
    
  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM (OS_file_length (arg_channel (1))));
}

DEFINE_PRIMITIVE ("FILE-READ-CHAR", Prim_File_Read_Char, 1)
{
  char ascii;
  PRIMITIVE_HEADER (1);

  if ((OS_file_read_chars ((arg_channel (1)), &ascii, 1)) != 1)
    error_external_return ();
  PRIMITIVE_RETURN (c_char_to_scheme_char (ascii));
}

DEFINE_PRIMITIVE ("FILE-FILL-INPUT-BUFFER-OFFSET", Prim_File_Fill_Input_Buffer_Offset, 3)
{
  fast Pointer buffer;
  PRIMITIVE_HEADER (3);

  CHECK_ARG (2, STRING_P);
  CHECK_ARG (3, FIXNUM_P);
  buffer = (ARG_REF (2));
  PRIMITIVE_RETURN (fill_input_buffer((arg_channel(1)), Scheme_String_To_C_String(buffer),
				      string_length(buffer), Get_Integer(ARG_REF(3))));
}

DEFINE_PRIMITIVE ("FILE-FILL-INPUT-BUFFER", Prim_File_Fill_Input_Buffer, 2)
{
  fast Pointer buffer;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (2, STRING_P);
  buffer = (ARG_REF (2));
  return fill_input_buffer((arg_channel(1)), Scheme_String_To_C_String(buffer),
			   string_length(buffer), 0);
}

fill_input_buffer(file, buffer, buffer_length, offset)
FILE *file;
char *buffer;
long buffer_length;
long offset;
{
  return (Make_Unsigned_Fixnum (OS_file_read_chars (file, buffer + offset,
						    buffer_length - offset)));
}


DEFINE_PRIMITIVE ("FILE-WRITE-CHAR", Prim_File_Write_Char, 2)
{
  char ascii;
  PRIMITIVE_HEADER (2);

  ascii = (arg_ascii_char (1));
  if (! (OS_file_write_chars ((arg_channel (2)), &ascii, 1)))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("FILE-WRITE-STRING", Prim_File_Write_String, 2)
{
  fast Pointer buffer;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  buffer = (ARG_REF (1));
  if (! (OS_file_write_chars ((arg_channel (2)),
			      (Scheme_String_To_C_String (buffer)),
			      (string_length (buffer)))))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

/* File System Operations */

DEFINE_PRIMITIVE ("FILE-EXISTS?", Prim_File_Exists, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (((OS_file_existence_test (Scheme_String_To_C_String (ARG_REF (1)))) > 0)
     ? TRUTH
     : NIL);
}

/* (COPY-FILE old-name new-name)
   Make a new copy of the file OLD-NAME, called NEW-NAME. */

DEFINE_PRIMITIVE ("COPY-FILE", Prim_Copy_File, 2)
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  CHECK_ARG (2, STRING_P);
  if (! (OS_file_copy ((Scheme_String_To_C_String (ARG_REF (1))),
		       (Scheme_String_To_C_String (ARG_REF (2))))))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

/* (RENAME-FILE old-name new-name)
   Moves the file from OLD-NAME to NEW-NAME. */

DEFINE_PRIMITIVE ("RENAME-FILE", Prim_Rename_File, 2)
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  CHECK_ARG (2, STRING_P);
  if (! (OS_file_rename ((Scheme_String_To_C_String (ARG_REF (1))),
			 (Scheme_String_To_C_String (ARG_REF (2))))))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

/* (REMOVE-FILE filename)
   Delete the given file from the file system.  Signals an error if
   unable to delete the file. */

DEFINE_PRIMITIVE ("REMOVE-FILE", Prim_Remove_File, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  if (! (OS_file_remove (Scheme_String_To_C_String (ARG_REF (1)))))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

/* (LINK-FILE old-file new-file physical?)
   Make NEW-FILE be a link pointing at OLD-FILE.  If PHYSICAL? is
   true, a physical link is created, otherwise a symbolic link is
   created. */

DEFINE_PRIMITIVE ("LINK-FILE", Prim_Link_File, 3)
{
  fast char *old_file, *new_file;
  PRIMITIVE_HEADER (3);

  CHECK_ARG (1, STRING_P);
  old_file = (Scheme_String_To_C_String (ARG_REF (1)));
  new_file = (Scheme_String_To_C_String (ARG_REF (2)));
  if (! (((ARG_REF (3)) != NIL)
	 ? (OS_file_link_physical (old_file, new_file))
	 : (OS_file_link_symbolic (old_file, new_file))))
    error_external_return ();
  PRIMITIVE_RETURN (NIL);
}

/* (WORKING-DIRECTORY-PATHNAME)
   Returns the current working directory as a string.*/

DEFINE_PRIMITIVE ("WORKING-DIRECTORY-PATHNAME", Prim_working_dir_pathname, 0)
{
  fast Pointer result;
  PRIMITIVE_HEADER (0);

  result = (OS_working_dir_pathname ());
  if (result == NIL)
    error_external_return ();
  PRIMITIVE_RETURN (result);
}

/* (SET-WORKING-DIRECTORY-PATHNAME! string)
   Changes the current working directory to the given string.
   Returns the old working directory as its value. */

DEFINE_PRIMITIVE ("SET-WORKING-DIRECTORY-PATHNAME!", Prim_set_working_dir_pathname, 1)
{
  fast Pointer result;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  result = (OS_working_dir_pathname ());
  if (result == NIL)
    error_external_return ();
  if (! (OS_set_working_dir_pathname
	 (Scheme_String_To_C_String (ARG_REF (1)))))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (result);
}

/* (MAKE-DIRECTORY pathname)
   Create a new directory of the given name. */

DEFINE_PRIMITIVE ("MAKE-DIRECTORY", Prim_Make_Directory, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  if (! (OS_directory_make (Scheme_String_To_C_String (ARG_REF (1)))))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (NIL);
}

/* (OPEN-DIRECTORY pathname)
   Attempts to open the directory specified by the string `pathname'.
   If successful, it returns the first file in the directory, as a string.
   If there is no such file, or the directory cannot be opened, returns #F. */

DEFINE_PRIMITIVE ("OPEN-DIRECTORY", Prim_open_directory, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (OS_directory_open (Scheme_String_To_C_String (ARG_REF (1))));
}

/* (DIRECTORY-READ)
   Returns the next file in the directory opened by `open-directory',
   or #F if there are no more files in the directory. */

DEFINE_PRIMITIVE ("DIRECTORY-READ", Prim_directory_read, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (OS_directory_read ());
}


/*
  Binary (dump) file operations
*/

Define_Primitive (prim_fasopen_write, 1, "FASOPEN-WRITE")
{
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);
  return fasopen(Arg1, "w");
}

Define_Primitive (prim_fasopen_read, 1, "FASOPEN-READ")
{
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);
  return fasopen(Arg1, "r");
}

fasopen(name, write_flag)  /* returns Pointer */
Pointer name;
char *write_flag;
{
  long status, fd;

  fd = OS_open_dump_file_fd(Scheme_String_To_C_String(name),write_flag,&status);
  if (fd == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  return Make_Signed_Fixnum(fd);
}

Define_Primitive(prim_fasclose, 1, "FASCLOSE")
{
  int status, result;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  result =  OS_close_dump_file_fd(Get_Integer(Arg1),&status);
  if (result == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  else return NIL;
}

Define_Primitive(prim_fasseek, 2, "FASSEEK")
{
  long fd, object_no;
  long status, result;

  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  fd = Get_Integer(Arg1);
  object_no = Get_Integer(Arg2);

  result = OS_dump_file_seek(fd,object_no,&status);
  if (result == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  if (result == 0)  return Make_Non_Pointer(TC_IO_ERROR_CODE,0xE0F);
  return NIL;
}

Define_Primitive(prim_faspos, 1, "FASPOS")
{
  long fd;
  long status, result;

  Primitive_1_Args();

  Arg_1_Type(TC_FIXNUM);
  fd = Get_Integer(Arg1);

  result = OS_dump_file_pos(fd, &status);
  if (result == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,status);
  return C_Integer_To_Scheme_Integer(result);
}

