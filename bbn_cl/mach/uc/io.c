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

/* $Header: io.c,v 10.0 88/12/07 13:08:43 las Exp $
 *
 * Input/Output primitives.
 */

#include "scheme.h"
#include "primitive.h"

extern char OS_tty_tyi();

#define Get_Character(Immediate)				\
{ int Channel;							\
  long Char;							\
  Primitive_1_Arg();						\
  Arg_1_Type(TC_HUNK3);						\
  Range_Check(Channel, Vector_Ref(Arg1, 0),			\
              0, FILE_CHANNELS, ERR_ARG_1_BAD_RANGE);		\
  if (Channel==0) while (true)					\
  { Boolean Interrupted;					\
    Char = OS_tty_tyi(Immediate, &Interrupted);			\
    if (Interrupted)						\
    { if ((IntEnb & IntCode)  != 0) Primitive_Interrupt()	\
    }								\
    else return Make_Non_Pointer(TC_FIXNUM, Char);		\
  }								\
  else								\
  { FILE *File_Block;						\
    if ((File_Block = Channels[Channel]) == NULL)		\
       Primitive_Error(ERR_ARG_1_BAD_RANGE);			\
    return Make_Non_Pointer(TC_FIXNUM,				\
			    ((OS_Get_C(File_Block))&MAX_CHAR));	\
  }								\
}

/* (GET_CHAR FILE_BLOCK)
      [Primitive number 0x32]
      The FILE_BLOCK must be a hunk-3, whose 0th element is a file
      created by the appropriate operating system call.  A character is
      read from that file and the ASCII code is returned.  The input is
      also echoed to the photo file if one is open.  
*/
Built_In_Primitive(Prim_Get_Character, 1, "GET-CHARACTER") 
Get_Character(false);

/* (GET_CHAR_IMMEDIATE FILE_BLOCK)
      [Primitive number 0x8B]
      Same as GET_CHAR except that if the input file is the terminal,
      then a character is returned without waiting for an entire line to
      be accumulated.  The exact behaviour of this primitive is operating
      system dependent.
*/
Built_In_Primitive(Prim_Get_Char_Immediate, 1, "GET-CHARACTER-IMMEDIATE")
Get_Character(true);

/* (PRINT_STRING STRING TRIPLE)
      [Primitive number 0x25]
      The first entry of TRIPLE (its CXR 0) should be an output
      channel as supplied from the operating system.  Prints the
      string (a la PRINC) to the output channel and the photo file if
      one is open.
*/
Built_In_Primitive(Prim_Print_String, 2, "PRINT-STRING")
{ long Channel;
  fast char *String;
  FILE *File_Block;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_HUNK3);
  Range_Check(Channel, Vector_Ref(Arg2, 0), 0, FILE_CHANNELS,
              ERR_ARG_2_BAD_RANGE);
  if (Channel==0) File_Block = stdout;
  else File_Block = Channels[Channel];
  String = Scheme_String_To_C_String(Arg1);
#ifdef String_Out_Code
  String_Out_Code(Channel, File_Block, String, Arg1);
#else
  if (File_Block == NULL) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  fputs(String, File_Block);
  if (Channel==0)
  { OS_Flush_Output_Buffer();
    if (Photo_Open) fputs(String, Photo_File_Handle);
  }
#endif
  return NIL;    
}

/* (PUT_CHAR_TO_OUTPUT_CHANNEL CHAR-CODE TRIPLE)
      [Primitive number 0xC8]
      The first entry of TRIPLE (its CXR 0) should be an output
      channel as supplied from the operating system.  Prints the
      character (a la TYO) whose ASCII code is suppled as CHAR-CODE to
      the output channel and the photo file if one is open.
*/
Built_In_Primitive(Prim_Put_Char_To_Output_Channel, 2,
		   "PUT-CHAR-TO-OUTPUT-CHANNEL")
{ long Channel, Char;
  FILE *File_Block;
  char C;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(Char, Arg1, 0, 255, ERR_ARG_1_BAD_RANGE);
  Arg_2_Type(TC_HUNK3);
  Range_Check(Channel, Vector_Ref(Arg2, 0), 0, FILE_CHANNELS,
              ERR_ARG_2_BAD_RANGE);
  C = (char) Char;
  if (Channel==0)
  { if (Photo_Open) putc(C, Photo_File_Handle);
    putc(C, stdout);
    OS_Flush_Output_Buffer();
    return NIL;
  }
  else File_Block = Channels[Channel];
  if (File_Block == NULL) Primitive_Error(ERR_ARG_2_BAD_RANGE)
  else OS_Put_C(C, File_Block);
  return NIL;
}
