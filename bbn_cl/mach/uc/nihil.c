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

/* $Header: nihil.c,v 10.0 88/12/07 13:09:24 las Exp $

   Contains unimplemented primitives.

*/

#include "scheme.h"
#include "primitive.h"

Pointer Not_Implemented_Yet(S)
char S[];
{ printf("\nIgnoring unimplemented primitive: %s\n", S);
  return NIL;
}

/* Primitives contiguous below are contiguous in storage.c */

NIY(Prim_Prim_Fasload, 1, "PRIMITIVE-FASLOAD")

NIY(Prim_Complete_Garbage_Collect, 3, "COMPLETE-GARBAGE-COLLECT")

NIY(Prim_Current_Date, 0, "CURRENT-DATE")
NIY(Prim_Current_Time, 0, "CURRENT-TIME")
NIY(Prim_Translate_File, 2, "TRANSLATE-FILE")

NIY(Prim_Volume_Name, 1, "VOLUME-NAME")

NIY(Prim_Open_Catalog, 0, "OPEN-CATALOG")
NIY(Prim_Close_Catalog, 0, "CLOSE-CATALOG")
NIY(Prim_Next_File, 0, "NEXT-FILE")
NIY(Prim_Cat_Name, 0, "CAT-NAME")
NIY(Prim_Cat_Kind, 0, "CAT-KIND")
NIY(Prim_Cat_Psize, 0, "CAT-PSIZE")
NIY(Prim_Cat_Lsize, 0, "CAT-LSIZE")
NIY(Prim_Cat_Info, 0, "CAT-INFO")
NIY(Prim_Cat_Block, 0, "CAT-BLOCK")
NIY(Prim_Cat_Create_Date, 0, "CAT-CREATE-DATE")
NIY(Prim_Cat_Create_Time, 0, "CAT-CREATE-TIME")
NIY(Prim_Cat_Last_Date, 0, "CAT-LAST-DATE")
NIY(Prim_Cat_Last_Time, 0, "CAT-LAST-TIME")
NIY(Prim_Error_Message, 0, "ERROR-MESSAGE")

NIY(Prim_Init_Floppy, 1, "INIT-FLOPPY")
NIY(Prim_Zero_Floppy, 1, "ZERO-FLOPPY")
NIY(Prim_Pack_Volume, 1, "PACK-VOLUME")
NIY(Prim_Load_Picture, 1, "LOAD-PICTURE")
NIY(Prim_Store_Picture, 1, "STORE-PICTURE")
NIY(Prim_Lookup_System_Symbol, 1, "LOOKUP-SYSTEM-SYMBOL")

NIY(Prim_Extract_Non_Marked_Vector, 0, "EXTRACT-NON-MARKED-VECTOR")
NIY(Prim_Unsnap_Links, 0, "UNSNAP-LINKS!")
NIY(Prim_Safe_Primitive_P, 0, "SAFE-PRIMITIVE?")
NIY(Prim_Substring_Read, 0, "SUBSTRING-READ")
NIY(Prim_Substring_Write, 0, "SUBSTRING-WRITE")

NIY(Prim_Screen_Write_Cursor, 0, "SCREEN-WRITE-CURSOR")
NIY(Prim_Screen_Write_Character, 0, "SCREEN-WRITE-CHARACTER")
NIY(Prim_Screen_Write_Substring, 0, "SCREEN-WRITE-SUBSTRING")
NIY(Prim_Next_File_Matching, 0, "NEXT-FILE-MATCHING")

NIY(Prim_Tty_Write_Byte, 0, "TTY-WRITE-BYTE")
NIY(Prim_File_Read_Byte, 0, "FILE-READ-BYTE")
NIY(Prim_File_Write_Byte, 0, "FILE-WRITE-BYTE")

NIY(Prim_Volume_Exists_P, 0, "VOLUME-EXISTS?")
NIY(Prim_Re_Char_Set_Adjoin, 0, "RE-CHAR-SET-ADJOIN!")
NIY(Prim_Re_Compile_Fastmap, 0, "RE-COMPILE-FASTMAP")
NIY(Prim_Re_Match, 0, "RE-MATCH")
NIY(Prim_Re_Search_Forward, 0, "RE-SEARCH-FORWARD")
NIY(Prim_Re_Search_Backward, 0, "RE-SEARCH-BACKWARD")
NIY(Prim_Sys_Memory_Ref, 0, "SYS-MEMORY-REF")
NIY(Prim_Sys_Memory_Set, 0, "SYS-MEMORY-SET!")

#if false

/* Removed as built-in primitives. */

NIY(Prim_And_Gcd, 0, "AND-GCD")
NIY(Prim_Save_Screen, 0, "SAVE-SCREEN")
NIY(Prim_Restore_Screen, 0, "RESTORE-SCREEN")
NIY(Prim_Subscreen_Clear, 0, "SUBSCREEN-CLEAR")
NIY(Prim_Tty_Redraw_Screen, 0, "TTY-REDRAW-SCREEN")
NIY(Prim_Screen_Inverse_Video, 0, "SCREEN-INVERSE-VIDEO")
NIY(Prim_Find_Pascal_Program, 0, "FIND-PASCAL-PROGRAM")
NIY(Prim_Execute_Pascal_Program, 0, "EXECUTE-PASCAL-PROGRAM")
NIY(Prim_Alpha_Raster_P, 0, "ALPHA-RASTER?")
NIY(Prim_Toggle_Alpha_Raster, 0, "TOGGLE-ALPHA-RASTER")
NIY(Prim_Graphics_Raster_P, 0, "GRAPHICS-RASTER?")
NIY(Prim_Toggle_Graphics_Raster, 0, "TOGGLE-GRAPHICS-RASTER")

NIY(Prim_Graphics_Clear, 0, "GRAPHICS-CLEAR")
NIY(Prim_Graphics_Move, 2, "GRAPHICS-MOVE")
NIY(Prim_Graphics_Line, 2, "GRAPHICS-LINE")
NIY(Prim_Graphics_Pixel, 2, "GRAPHICS-PIXEL")
NIY(Prim_Graphics_Set_Line_Style, 1, "GRAPHICS-SET-LINE-STYLE")
NIY(Prim_Graphics_Set_Drawing_Mode, 1, "SET-DRAWING-MODE")
NIY(Prim_Graphics_Set_Drawing_Style, 1, "SET-DRAWING-MODE")

#endif
