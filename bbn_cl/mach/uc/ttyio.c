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

/* $Header: ttyio.c,v 10.0 88/12/07 13:11:30 las Exp $
 * $MIT-Header: ttyio.c,v 9.25 88/01/14 23:24:49 GMT cph Exp $
 */

/* Primitives to perform I/O to and from the console. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"

extern char OS_tty_read_char ();
extern char OS_tty_read_char_immediate ();
extern Boolean OS_read_char_ready_p ();

extern void OS_tty_write_char ();
extern Boolean OS_tty_write_chars ();
extern void OS_Flush_Output_Buffer ();

extern FILE *OS_file_open ();
extern Boolean OS_file_close ();
extern Boolean OS_file_write_chars ();

extern void OS_tty_beep ();
extern Boolean OS_Clear_Screen ();
extern Boolean OS_tty_move_cursor ();
extern Boolean OS_tty_get_cursor ();

extern long NColumns ();
extern long NLines ();

/* (TTY-READ-CHAR-READY? delay)
   Return #T iff a character is ready to be read from the console.
   If no such character is ready, wait at most DELAY microseconds
   for a character to be typed.  Return #T immediately if one is
   typed, otherwise return #F after DELAY. */

DEFINE_PRIMITIVE ("TTY-READ-CHAR-READY?", Prim_Tty_Read_Char_Ready_P, 1)
{
  long delay;
  PRIMITIVE_HEADER (1);

  delay = (arg_nonnegative_integer (1));
  PRIMITIVE_RETURN ((OS_read_char_ready_p (delay)) ? TRUTH : NIL);
}

/* (TTY-READ-CHAR)
   Read a character from the console, allowing the user to edit the
   input.  This is used for reading entire expressions. */

DEFINE_PRIMITIVE ("TTY-READ-CHAR", Prim_Tty_Read_Char, 0)
{
  char chr;
  PRIMITIVE_HEADER (0);

  chr = (OS_tty_read_char ());
  if (Photo_Open)
    OS_file_write_chars (Photo_File_Handle, &chr, 1);
  PRIMITIVE_RETURN (c_char_to_scheme_char (chr));
}

/* (TTY-READ-CHAR-IMMEDIATE)
   Read a character from the console, without editing. */

DEFINE_PRIMITIVE ("TTY-READ-CHAR-IMMEDIATE", Prim_Tty_Read_Char_Immediate, 0)
{
  char chr;
  PRIMITIVE_HEADER (0);

  chr = (OS_tty_read_char_immediate ());
  if (Photo_Open)
    OS_file_write_chars (Photo_File_Handle, &chr, 1);
  PRIMITIVE_RETURN  (c_char_to_scheme_char (chr));
}

/* (TTY-READ-FINISH)
   After having read an entire expression using TTY-READ-CHAR, this
   primitive is called to allow the input editor to clean up.
   Since there is no input editor in this implementation, this is
   a noop. */

DEFINE_PRIMITIVE ("TTY-READ-FINISH", Prim_Tty_Read_Finish, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-WRITE-CHAR", Prim_Tty_Write_Char, 1)
{
  fast long ascii;
  PRIMITIVE_HEADER (1);

  ascii = (arg_ascii_char (1));
  OS_tty_write_char (ascii);
  if (Photo_Open)
    {
      char c_ascii = ascii;

      OS_file_write_chars (Photo_File_Handle, (& c_ascii), 1);
    }
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-WRITE-STRING", Prim_Tty_Write_String, 1)
{
  fast Pointer string_argument;
  fast char *string;
  fast long length;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  string_argument = (ARG_REF (1));
  string = (Scheme_String_To_C_String (string_argument));
  length = (string_length (string_argument));
  if (! (OS_tty_write_chars (string, length)))
    error_external_return ();
  if (Photo_Open)
    OS_file_write_chars (Photo_File_Handle, string, length);
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-FLUSH-OUTPUT", Prim_tty_flush_output, 0)
{
  PRIMITIVE_HEADER (0);

  OS_Flush_Output_Buffer ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-BEEP", Prim_Tty_Beep, 0)
{
  PRIMITIVE_HEADER (0);

  OS_tty_beep ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-CLEAR", Prim_Tty_Clear, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN ((OS_Clear_Screen ()) ? TRUTH : NIL);
}

DEFINE_PRIMITIVE ("PHOTO-OPEN", Prim_Photo_Open, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  if (Photo_Open)
    PRIMITIVE_RETURN (NIL);
  Photo_File_Handle =
    (OS_file_open ((Scheme_String_To_C_String (ARG_REF (1))), true));
  Photo_Open = (Photo_File_Handle != NULL);
  if (! Photo_Open)
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (TRUTH);
}

DEFINE_PRIMITIVE ("PHOTO-CLOSE", Prim_Photo_Close, 0)
{
  PRIMITIVE_HEADER (0);

  if (Photo_Open)
    {
      Boolean result;

      result = (OS_file_close (Photo_File_Handle));
      Photo_Open = false;
      if (! result)
	error_external_return ();
      PRIMITIVE_RETURN (TRUTH);
    }
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("TTY-MOVE-CURSOR", Prim_Tty_Move_Cursor, 2)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN
    (OS_tty_move_cursor ((arg_nonnegative_integer (1)),
			 (arg_nonnegative_integer (2)))
     ? TRUTH : NIL);
}

DEFINE_PRIMITIVE ("TTY-GET-CURSOR", Prim_Tty_Get_Cursor, 0)
{
  int temp;
  long x, y;
  Pointer *result;
  PRIMITIVE_HEADER (0);

  if (! (OS_tty_get_cursor (&x, &y)))
    PRIMITIVE_RETURN (NIL);
  Primitive_GC_If_Needed (2);
  result = Free;
  *Free++ = (Make_Unsigned_Fixnum (x));
  *Free++ = (Make_Unsigned_Fixnum (y));
  PRIMITIVE_RETURN (Make_Pointer (TC_LIST, result));
}

DEFINE_PRIMITIVE ("SCREEN-X-SIZE", Prim_Screen_X_Size, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (NColumns ()));
}

DEFINE_PRIMITIVE ("SCREEN-Y-SIZE", Prim_Screen_Y_Size, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (NLines ()));
}

/* Old primitive (fossil?) */

DEFINE_PRIMITIVE ("CLEAR-TO-END-OF-LINE", Prim_Clear_To_End_Of_Line, 0)
{
  extern Boolean OS_Clear_To_End_Of_Line ();
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN ((OS_Clear_To_End_Of_Line ()) ? TRUTH : NIL);
}
