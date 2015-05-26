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

/* $Header: string.c,v 10.0 88/12/07 13:10:56 las Exp $
 * $MIT-Header: ostring.c,v 9.23 87/11/17 08:17:21 GMT jinx Exp $
 *
 * Support for strings and conversion to and from lists of characters.
 */

#include "scheme.h"
#include "primitive.h"

                    /****************************/
                    /* Making Character Strings */
                    /****************************/

#define Empty_String()							\
{									\
  Pointer_Count = 2 + 1 + (Get_Integer(Arg1) / sizeof(Pointer));	\
  Primitive_GC_If_Needed(Pointer_Count);				\
  Free[STRING_HEADER] =							\
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Pointer_Count - 1));	\
  Free[STRING_LENGTH] = ((Pointer) 0);					\
  /* put a null in first character position */				\
  (*((char *) (Free + STRING_CHARS))) = '\0';				\
  Free += Pointer_Count;						\
}

Built_In_Primitive(Prim_Make_Empty_String, 1, "MAKE-EMPTY-STRING", 0x1F)
Define_Primitive(Prim_Make_Empty_String, 1, "MAKE-EMPTY-STRING")
{
  long Pointer_Count;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  Empty_String();
  PRIMITIVE_RETURN(Make_Pointer(TC_CHARACTER_STRING, (Free - Pointer_Count)));
}
 
Built_In_Primitive(Prim_Make_Fld_String, 2, "MAKE-FILLED-STRING", 0x4B)
Define_Primitive(Prim_Make_Fld_String, 2, "MAKE-FILLED-STRING")
{
  Pointer Result;
  long C, Pointer_Count, Count;
  fast long i;
  fast char *P;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(C, Arg2, 0, 255, ERR_ARG_2_BAD_RANGE);
  Empty_String();
  Result = Make_Pointer(TC_CHARACTER_STRING, (Free - Pointer_Count));
  P = ((char *) Nth_Vector_Loc(Result, STRING_CHARS));
  Count = Get_Integer(Arg1);
  for (i = 0; i < Count; i++)
  {
    *P++ = ((char) C);
  }
  *P++ = '\0';				       /* Add null */
  Vector_Set(Result, STRING_LENGTH, ((long) (Count)));
  PRIMITIVE_RETURN(Result);
}

extern Pointer list_to_string();

Pointer
list_to_string(Orig_List)
     Pointer Orig_List;
{
  char *Next;
  long Length;
  Pointer Result;

  Result = Make_Pointer(TC_CHARACTER_STRING, Free);
  Next = ((char *) Nth_Vector_Loc(Result, STRING_CHARS));
  Length = 0;
  Touch_In_Primitive(Orig_List, Orig_List);

  while (Type_Code(Orig_List) == TC_LIST)
  {
    Pointer This_Char;
    long The_Character;

    Primitive_GC_If_Needed(Free - ((Pointer *) Next));
    Touch_In_Primitive(Vector_Ref(Orig_List, CONS_CAR), This_Char);
    if (Type_Code(This_Char) != TC_CHARACTER)
    {
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    }
    Range_Check(The_Character, This_Char,
		0, MAX_CHAR, ERR_ARG_1_BAD_RANGE);
    *Next++ = ((char) The_Character);
    Touch_In_Primitive(Vector_Ref(Orig_List, CONS_CDR), Orig_List);
    Length += 1;
  }
  if (Orig_List != NIL)
  {
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  *Next++ = '\0';		  /* Add the null */
  Free += 2 + (Length + sizeof(Pointer)) / sizeof(Pointer);
  Vector_Set(Result, STRING_LENGTH, ((Pointer) (Length)));
  Vector_Set(Result, STRING_HEADER,
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Free - Get_Pointer(Result)) - 1));
  return (Result);
}

/* (LIST->STRING LIST)
      Takes a list of the ASCII codes for characters and converts it
      into a string containing those characters.  For example, on
      input '(#/A #/B #/C) it returns "ABC".
*/

Built_In_Primitive(Prim_Build_String_From_List, 1, "LIST->STRING", 0x5F)
Define_Primitive(Prim_Build_String_From_List, 1, "LIST->STRING")
{ 
  Primitive_1_Arg();

  PRIMITIVE_RETURN(list_to_string(Arg1));
}

/* (EQUAL-STRING-TO-LIST? STRING LIST)
   Compares characters from the string with ASCII character codes from
   the LIST.  Returns #!TRUE if the string and the list have the same
   number of characters and all the characters match.  Returns NIL
   otherwise.
*/
Built_In_Primitive(Prim_Equal_String_To_List, 2, "EQUAL-STRING-TO-LIST?", 0x60)
Define_Primitive(Prim_Equal_String_To_List, 2, "EQUAL-STRING-TO-LIST?")
{
  fast long Count, i;
  fast char *Next;
  fast Pointer Next_List;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Count = ((long) Fast_Vector_Ref(Arg1, STRING_LENGTH));
  Next = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  Touch_In_Primitive(Arg2, Next_List);
  for (i = 0; ((i < Count) && (Type_Code(Next_List) == TC_LIST)); i++)
  {
    fast Pointer Next_Val;

    Touch_In_Primitive(Vector_Ref(Next_List, CONS_CAR), Next_Val);
    if (Type_Code(Next_Val) != TC_FIXNUM)
    {
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
    }
    if (*Next++ != Get_Integer(Next_Val))
    {
      PRIMITIVE_RETURN(NIL);
    }
    Touch_In_Primitive(Vector_Ref(Next_List, CONS_CDR), Next_List);
  }
  if ((i == Count) && (Next_List == NIL))
  {
    PRIMITIVE_RETURN(TRUTH);
  }
  if ((Next_List != NIL) && (Type_Code(Next_List) != TC_LIST))
  {
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  }
  PRIMITIVE_RETURN(NIL);
}

/* (INSERT-STRING ORIG-STRING N INSERTION)
   ORIG-STRING and INSERTION must be strings.  A new string is
   created which contains INSERTION between the (N-1)st and Nth
   characters of ORIG-STRING.  If N is 0, this is the concatenation
   of INSERTION followed by ORIG-STRING.  If N is (STRING_LENGTH
   ORIG_STRING) it is the concatenation of ORIG-STRING followed by
   INSERTION.  Both strings are copied in the process (i.e. the new
   string never shares characters with the original ones).
*/
Built_In_Primitive(Prim_Insert_String, 3, "INSERT-STRING", 0x1D)
Define_Primitive(Prim_Insert_String, 3, "INSERT-STRING")
{
  char *To, *From_New, *From_Old; 
  long Index, Old_Length, New_Length, Length, i, Pointer_Count;
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_CHARACTER_STRING);

  Old_Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  New_Length = ((long) (Fast_Vector_Ref(Arg3, STRING_LENGTH)));
  Range_Check(Index, Arg2, 0, Old_Length, ERR_ARG_2_BAD_RANGE);
  From_Old = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  From_New = ((char *) Nth_Vector_Loc(Arg3, STRING_CHARS));
  To = ((char *) (Free + 2));
  Length = (((Old_Length - Index) >= New_Length) ?
	    Old_Length :
	    (Old_Length + New_Length));
  Pointer_Count =
    ((sizeof(Pointer) + (Length * sizeof(char))) / sizeof(Pointer));
  Primitive_GC_If_Needed(Pointer_Count + 1);
  Free[STRING_HEADER] =
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Pointer_Count + 1);
  Free[STRING_LENGTH] = ((long) Length);
  for (i = 0; i < Index; i++)
  {
    *To++ = *From_Old++;
  }
  for (i = 0; i < New_Length; i++)
  {
    *To++ = *From_New++;
  }
  for (i = Index; i < Old_Length; i++)
  {
    *To++ = *From_Old++;
  }
  *To++ = '\0';		 		 /* Add the null */
  Free += (Pointer_Count + 2);
  PRIMITIVE_RETURN(Make_Pointer(TC_CHARACTER_STRING,
				(Free - (Pointer_Count+2))));
}

/* (STRING-EQUAL? STRING-1 STRING-2)
   Compare two strings for equality.  This comparison is
   case-sensitive.
*/
Built_In_Primitive(Prim_String_Equal, 2, "STRING-EQUAL?", 0xE)
Define_Primitive(Prim_String_Equal, 2, "STRING-EQUAL?")
{
  fast char *Finger_1, *Finger_2;
  fast long Length_1, Length_2, i; 
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_CHARACTER_STRING);
  Length_1 = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Length_2 = ((long) (Fast_Vector_Ref(Arg2, STRING_LENGTH)));
  if (Length_1 != Length_2)
  {
    PRIMITIVE_RETURN(NIL);
  }
  Finger_1 = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  Finger_2 = ((char *) Nth_Vector_Loc(Arg2, STRING_CHARS));
  for (i = 0; i < Length_1; i++)
  {
    if (*Finger_1++ != *Finger_2++)
    {
      PRIMITIVE_RETURN(NIL);
    }
  }
  PRIMITIVE_RETURN(TRUTH);
}

/* (INSERT-STRING! ORIG-STRING POSITION NEW-STRING)
   Side-effects the ORIG-STRING by replacing characters starting at
   POSITION with characters from NEW-STRING.  If POSITION is 0, the
   replacement is at the start of the string, and so on.  It is not
   possible to extend the length of a string this way unless
   the string contains room for more characters than it currently 
   has, which should be priviledged information. The value returned
   is ORIG-STRING, which will have been modified.
*/
Built_In_Primitive(Prim_Overwrite_String, 3, "INSERT-STRING!", 0x2B)
Define_Primitive(Prim_Overwrite_String, 3, "INSERT-STRING!")
{
  char *To, *From;
  long Max_Length, Old_Length, Addition, New_Length, Index, i;
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_CHARACTER_STRING);

  Max_Length = ((Get_Integer(Fast_Vector_Ref(Arg1, STRING_HEADER)) - 1) *
		(sizeof(Pointer))) - 1;
  Old_Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Addition = ((long) (Fast_Vector_Ref(Arg3, STRING_LENGTH)));
  Index = Get_Integer(Arg2);
  if (Index > Old_Length)
  {
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  }
  if ((New_Length = Index + Addition) > Max_Length)
  {
    Primitive_Error(ERR_ARG_3_BAD_RANGE);
  }
  From = ((char *) Nth_Vector_Loc(Arg3, STRING_CHARS));
  To = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  To += Index;
  for (i = 0; i < Addition; i++)
  {
    *To++ = *From++;
  }
  /* If we have gone past the bounds of the old string,
     add Null and adjust string length.
  */
  if (New_Length > Old_Length)
  {
    *To++ = '\0';
    Fast_Vector_Set(Arg1, STRING_LENGTH, ((Pointer) (New_Length)));
  }
  for (i = 0; i < New_Length; i++)
  {
    *To++ = *From++;
  }
  PRIMITIVE_RETURN(Arg1);
}

/* (SUBSTRING STRING FROM TO)
   Extracts the substring of STRING beginning with the FROMth
   character and continuing to (but not including) the TOth
   character. Thus (SUBSTRING S 0 (STRING_LENGTH S)) returns a copy
   of all of S.
*/

Pointer
string_substring(Arg1, Arg2, Arg3)
Pointer	Arg1, Arg2, Arg3;
{
  char *To, *From; long Low, High, Length,
       Elements, i, Pointer_Count;

  Length = Get_Integer(Fast_Vector_Ref(Arg1, STRING_LENGTH));
  Range_Check(Low, Arg2, 0, Length, ERR_ARG_2_BAD_RANGE);
  Range_Check(High, Arg3, 0, Length, ERR_ARG_3_BAD_RANGE);
  Elements = (High - Low);
  Pointer_Count =
    ((sizeof(Pointer) + (Elements * sizeof(char))) / sizeof(Pointer));
  Primitive_GC_If_Needed(Pointer_Count + 1);
  Free[0] = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Pointer_Count + 1));
  Free[1] = ((Pointer) Elements);
  if (Low > High)
    Primitive_Error(ERR_ARG_3_BAD_RANGE);
  To = ((char *) (Free + 2));
  From = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  From += Low;
  for (i = 0; i < Elements; i++)
    *To++ = *From++;
  *To++ = '\0';			/* Add the null */
  Free += (Pointer_Count + 2);
  return Make_Pointer(TC_CHARACTER_STRING, (Free - (Pointer_Count + 2)));
}

DEFINE_PRIMITIVE("SUBSTRING", Prim_Substring, 3)
{
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);

  return string_substring(Arg1, Arg2, Arg3);
}


/* (SUBSTRING-SEARCH STRING SUBSTRING)
   Returns the character position in STRING where the first
   occurrence of SUBSTRING begins.  The search is case-sensitive.
   Returns NIL if the SUBSTRING does not occur.
*/
Built_In_Primitive(Prim_Substring_Search, 2, "SUBSTRING-SEARCH", 0xB8)
Define_Primitive(Prim_Substring_Search, 2, "SUBSTRING-SEARCH")
{
  fast char *String, *Sub;
  fast long i, j;
  long String_Length, Sub_Length, Stop_At;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_CHARACTER_STRING);
  String = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  Sub = ((char *) Nth_Vector_Loc(Arg2, STRING_CHARS));
  String_Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Sub_Length = ((long) (Fast_Vector_Ref(Arg2, STRING_LENGTH)));
  Stop_At = String_Length - Sub_Length;
  for (i = 0; i <= Stop_At; i++)
  {
    for (j = 0; j < Sub_Length; j++)
    {
      if (String[i+j] != Sub[j])
      {
	break;
      }
    }
    if (j == Sub_Length)
    {
      PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(i));
    }
  }
  PRIMITIVE_RETURN(NIL);
}

/* (SUBSTRING->LIST STRING FROM TO)
   The same as SUBSTRING, except the result is a list of ASCII
   character codes rather than a string.
*/
Built_In_Primitive(Prim_Substring_To_List, 3, "SUBSTRING->LIST", 0x4A)
Define_Primitive(Prim_Substring_To_List, 3, "SUBSTRING->LIST")
{
  fast char *Finger;
  fast long i, Elements;
  long Low, High, Length;
  Pointer Result;
  Primitive_3_Args();    

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);

  Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Range_Check(Low, Arg2, 0, ((Length == 0)? 0 : (Length - 1)),
              ERR_ARG_2_BAD_RANGE);
  Range_Check(High, Arg3, Low, Length, ERR_ARG_3_BAD_RANGE);
  Elements = (High - Low);
  Primitive_GC_If_Needed(2 * Elements);
  if (Elements == 0)
  {
    PRIMITIVE_RETURN(NIL);
  }
  Finger = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  Finger += Low;
  Result = Make_Pointer(TC_LIST, Free);
  for (i = 0; i < Elements; i++, Free++)
  {
    *Free++ = MAKE_UNSIGNED_FIXNUM(MAX_CHAR & (*Finger++));
    *Free = Make_Pointer(TC_LIST, (Free + 1));
  }
  Free[-1] = NIL;
  PRIMITIVE_RETURN(Result);
}

/* (STRING-UPCASE STRING)
   Returns a copy of STRING with all lower-case letters changed to
   upper-case.
*/
Built_In_Primitive(Prim_Raise_String, 1, "STRING-UPCASE", 0xB3)
Define_Primitive(Prim_Raise_String, 1, "STRING-UPCASE")
{
  long Length, Word_Length, i;
  char *From, *To;
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);
  Word_Length =
    1 + Get_Integer(Fast_Vector_Ref(Arg1, STRING_HEADER));
  Primitive_GC_If_Needed(Word_Length);
  Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Free[STRING_HEADER] = Fast_Vector_Ref(Arg1, STRING_HEADER);
  Free[STRING_LENGTH] = ((Pointer) (Length));
  From = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  To = ((char *) &Free[STRING_CHARS]);
  for (i = 0; i < Length; i++, From++, To++)
  {
    fast char c;

    c = *From;
    if ((c >= 'a') && (c <= 'z'))
    {
      *To = c-'a'+'A';
    }
    else
    {
      *To = c;
    }
  }
  *To++ = '\0';
  Free += Word_Length;
  PRIMITIVE_RETURN(Make_Pointer(TC_CHARACTER_STRING, (Free - Word_Length)));
}

/* (CHARACTER-UPCASE CHAR-CODE)
   If CHAR-CODE is the ASCII code for a lower-case letter, returns
   the ASCII code for the upper-case letter.  Otherwise returns
   CHAR-CODE unchanged.
*/
Built_In_Primitive(Prim_Raise_Char, 1, "CHARACTER-UPCASE", 0x64)
Define_Primitive(Prim_Raise_Char, 1, "CHARACTER-UPCASE")
{
  long Value;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(Value, Arg1, 0, MAX_CHAR, ERR_ARG_1_BAD_RANGE);
  if (Value >= 'a' && Value <= 'z')
  {
    Value += ('A'-'a');
  }
  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(Value));
}

/* (STRING-LESS? STRING-1 STRING-2)
   Returns #!TRUE if STRING-1 < STRING-2 using the ASCII character
   code collating sequence.  The test is case-sensitive.
*/
Built_In_Primitive(Prim_String_Less, 2, "STRING-LESS?", 0x59)
Define_Primitive(Prim_String_Less, 2, "STRING-LESS?")
{
  char *Finger_1, *Finger_2; long Length_1, Length_2, i, Count;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_CHARACTER_STRING);
  Length_1 = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Length_2 = ((long) (Fast_Vector_Ref(Arg2, STRING_LENGTH)));
  Count = ((Length_1 < Length_2) ? Length_1 : Length_2);
  Finger_1 = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  Finger_2 = ((char *) Nth_Vector_Loc(Arg2, STRING_CHARS));
  for (i = 0; i<Count; i++, Finger_1++, Finger_2++)
  {
    if (*Finger_1 == *Finger_2)
    {
      continue;
    }
    else
    {
      PRIMITIVE_RETURN((*Finger_1 < *Finger_2) ? TRUTH : NIL);
    }
  }
  if (Length_1 < Length_2)
  {
    PRIMITIVE_RETURN(TRUTH);
  }
  else
  {
    PRIMITIVE_RETURN(NIL);
  }
}
    
/* (STRING-POSITION STRING MASK CHARACTER)
   Scans STRING one character at a time looking for a character
   which is the same as CHARACTER when both are compared after
   masking them with MASK.  If MASK=255 (the usual case) it will
   look for an exact match.  If MASK=255-32=223 then it "ignores"
   the bit which (in ASCII) differentiates upper and lower case
   letters, etc.  Returns the position of the match, or NIL if one
   isn't found.
*/
Built_In_Primitive(Prim_String_Position, 3, "STRING-POSITION", 0x58)
Define_Primitive(Prim_String_Position, 3, "STRING-POSITION")
{
  char *Finger; long Length, Character, Mask, i;
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);

  Range_Check(Mask, Arg2, 0, MAX_CHAR, ERR_ARG_2_BAD_RANGE);
  Range_Check(Character, Arg3, 0, MAX_CHAR, ERR_ARG_3_BAD_RANGE);
  Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Finger= ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  /* The test is made by finding all bits where the character from the
     string and the argument character differ (using XOR, ^).  This is
     then masked and tested to see if the only differences are in
     bits that the mask ignores.
  */
  for (i = 0; i < Length; i++)
  {
    if ((Mask & (*Finger++ ^ Character)) == 0)
    {
      PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(i));
    }
  }
  PRIMITIVE_RETURN(NIL);
}

/* (TRUNCATE-STRING! STRING INDEX)
   A side-effecting string primitive. Lops off the characters in
   STRING from INDEX on, leaving the maximum size (i.e. the GC
   size) of STRING the same.  Returns STRING.
*/
Built_In_Primitive(Prim_Truncate_String, 2, "TRUNCATE-STRING!", 0x44)
Define_Primitive(Prim_Truncate_String, 2, "TRUNCATE-STRING!")
{
  long Length, Index, i;
  char *To;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  Sign_Extend(Arg2, Index);
  if (Index < 0)
  {
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  }
  if (Index <= Length)
  {
    Fast_Vector_Set(Arg1, STRING_LENGTH, ((Pointer) (Index)));
  }
  /* Now add a null after the last valid positon */
  To = ((char *) Nth_Vector_Loc(Arg1, STRING_CHARS));
  for (i = 0; i < Index; i++)
  {
    To++;
  }
  *To = '\0';
  PRIMITIVE_RETURN(Arg1);
}

/* (VECTOR-8B? OBJECT)
   Returns #!TRUE if OBJECT is an 8-bit vector (a string).
*/
Built_In_Primitive(Prim_Vector_8b, 1, "VECTOR-8B?", 0xA4)
Define_Primitive(Prim_Vector_8b, 1, "VECTOR-8B?")
{
  Primitive_1_Arg();

  Touch_In_Primitive(Arg1, Arg1);
  return ((Type_Code(Arg1) == TC_VECTOR_8B) ? TRUTH : NIL);
}

/* (VECTOR-8B-CONS LENGTH)
   Creates an uninitialized 8-bit vector (string) to hold LENGTH
   characters.
*/
Built_In_Primitive(Prim_Vector_8b_Cons, 1, "VECTOR-8B-CONS", 0xA3)
Define_Primitive(Prim_Vector_8b_Cons, 1, "VECTOR-8B-CONS")
{
  long Length;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  Length = (NM_HEADER_LENGTH +
	    ((sizeof(char) * Get_Integer(Arg1)) /
	     sizeof(Pointer)) +
	    1);
  Primitive_GC_If_Needed(Length);
  Free[STRING_HEADER] = 
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Length - 1));
  Free[STRING_LENGTH] = ((Pointer) (Get_Integer(Arg1)));
  Free += Length;
  PRIMITIVE_RETURN(Make_Pointer(TC_VECTOR_8B, (Free - Length)));
}

/* (VECTOR-8B-SIZE STRING)
   Returns the number of entries (characters) in the 8-bit vector
   (string).
*/
Built_In_Primitive(Prim_Vec_8b_Size, 1, "VECTOR-8B-SIZE", 0xAD)
Define_Primitive(Prim_Vec_8b_Size, 1, "VECTOR-8B-SIZE")
{
  Primitive_1_Arg();

  Arg_1_Type(TC_VECTOR_8B);
  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(Vector_Ref(Arg1, STRING_LENGTH)));
}
