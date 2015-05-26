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

/* $Header: scanprim.c,v 10.0 88/12/07 13:10:24 las Exp $ */

/* Primitives to support syntax tables in Edwin. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"
#include "scanprim.h"

/* Remember:  the backward primitive have as a 4th arg  - the
   START of the region */

char *Quoted_From_Where();  /* forward ref */

/* Prims start here */

Built_In_Primitive(Prim_Scan_Word_Forward, 4, "SCAN-WORD-FORWARD")
{ long dbugr;
  long  Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End;
  Pointer *SyntaxTable_Start;

  Primitive_4_Args();
  Check_4_Args();
  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);

  SetUp_Point_Vars( Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);

  SyntaxTable_Start = Beginning_Of_Vector(Arg1);
  
  while (!(It_Is_A_(Sword, Point, SyntaxTable_Start))
	 && (Point < Region_End))
    {Increment(Point);}
  if (Point >= Region_End) return NIL;

  Increment(Point);
  /* we know Point is on an Sword, why bother checking it? */
  while (It_Is_A_(Sword, Point, SyntaxTable_Start) && (Point < Region_End))
    {Increment(Point);}
  
  
  Fix_Gap_If_Needed(Point, Gap_Start, -Gap_Length);
  return FIXNUM_0 + Point - String_Start;
}


/* does this do the right thing?
going until part of a word */
Built_In_Primitive(Prim_Scan_Forward_To_Word, 4, "SCAN-FORWARD-TO-WORD")
{ long Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End;
  Pointer *SyntaxTable_Start;

  Primitive_4_Args();
  Check_4_Args();

  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);

  while (!It_Is_A_(Sword, Point, SyntaxTable_Start) && (Point < Region_End))
    {Increment(Point);}
  if (Point == Region_End) return NIL;


  Fix_Gap_If_Needed(Point, Gap_Start, -Gap_Length);
  return FIXNUM_0 + Point - String_Start;
 }
    
  
Built_In_Primitive(Prim_Scan_Word_Backward, 4, "SCAN-WORD-BACKWARD")
{ long Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End, *Point_Start, *Left;
  Pointer *SyntaxTable_Start;

  Primitive_4_Args();
  Check_4_Args();

  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_Start,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);

  while (Point >= Region_Start)
    { One_Less_Than(Point, Left);
      if (It_Is_A_(Sword, Left, SyntaxTable_Start)) 
	break;
      Point = Left;
    }
  if (Point < Region_Start) return NIL;

  Decrement(Point);
  Point_Start = Point;
  /* we know Point is on an Sword, why bother checking it? */

  while (Point > Region_Start)
    { One_Less_Than(Point, Left);
      if (!It_Is_A_(Sword, Left, SyntaxTable_Start)) break;
      Point = Left;
    }

  Fix_Gap_If_Needed(Point, Gap_Start, -Gap_Length);
  return FIXNUM_0 + Point - String_Start;
}



Built_In_Primitive(Prim_Quoted_Char_P, 4, "QUOTED-CHAR?")
{ long Gap_Length, Number_Of_Quotes;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End, *Point_Start, *Quote_Start;
  Pointer *SyntaxTable_Start;

  Primitive_4_Args();
  Check_4_Args();

  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);

  Quote_Start = Quoted_From_Where(Point, Region_Start,
				  Gap_Start, Gap_End, 
				  SyntaxTable_Start);

  Diff(Point, Quote_Start, Number_Of_Quotes);
  return ((Odd (Number_Of_Quotes)) ? TRUTH : NIL);
}


Built_In_Primitive(Prim_Scan_Backward_Prefix_Chars, 4, "SCAN-BACKWARD-PREFIX-CHARS")
{ long Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End, *Point_Start, *Left;
  Pointer *SyntaxTable_Start;
  long Counter, Quoted;

  Primitive_4_Args();
  Check_4_Args();

  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);
  Point_Start = Point;

  while (Point > Region_Start)
    { One_Less_Than(Point, Left);
      if (!It_Is_A_(Sprefix, Left, SyntaxTable_Start)) break;
      Point = Left;
    }

  Fix_Gap_If_Needed(Point, Gap_Start, -Gap_Length);
  return FIXNUM_0 + Point - String_Start;
}


/* not great code(below), but I think it works */
/* explaintory comment here + code should be GC'd */

Built_In_Primitive(Prim_String_To_Syntax_Entry, 1, "STRING->SYNTAX-ENTRY")
{ char *String_Start;
  long Length, Syntax_Entry, bit;
  Pointer *SyntaxTable_Start;
  Primitive_1_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Length = (string_length (Arg1));
  if (Length > 4)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  String_Start = Scheme_String_To_C_String(Arg1);


  Syntax_Entry = Syntax_Spec_Code[*String_Start];
  if (Syntax_Entry == NO_SYNTAX_SPEC) 
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (String_End(Length)) return FIXNUM_0 + Syntax_Entry;

  String_Start++;
  if (*(String_Start) != ASCII_SPACE) 
    Syntax_Entry |= ((*String_Start) << BYTE_SIZE);
  if (String_End(Length)) return FIXNUM_0 + Syntax_Entry;

  String_Start++;
  Check_Bit_And_Put_It_In(String_Start, Syntax_Entry, bit);
  if (String_End(Length)) return FIXNUM_0 + Syntax_Entry;

  String_Start++;
  Check_Bit_And_Put_It_In(String_Start, Syntax_Entry, bit);
  return FIXNUM_0 + Syntax_Entry;
}



Built_In_Primitive(Prim_Char_To_Syntax_Code, 2, "CHAR->SYNTAX-CODE")
{ long Ascii, Code;
  Pointer *SyntaxTable_Start;
  Primitive_2_Args();

  Arg_1_Type(TC_VECTOR);
  Arg_2_Type(TC_CHARACTER);
  Ascii = scheme_char_to_c_char(Arg2);
  if (Ascii == NOT_ASCII) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);

  Code = CChar_To_Self_Syntax_Code(Ascii, SyntaxTable_Start);
  if (Code > Smax || Code < 0)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  return (c_char_to_scheme_char(Syntax_Code_Spec[Code]));
}


/* Scan from character number START by COUNT lists.                      
 Returns the character number of the position thus found.              
 if DEPTH is nonzero, paren depth begins counting from that value,     
only places where the depth in parentheses becomes zero                
are candidates for stopping; COUNT such places are counted.            
Thus, a positive value for DEPTH means go out levels.                  
Comments are ignored if parse-sexp-ignore-comments is non-nil.         
If the beginning or end of (the visible part of) the buffer is reached 
and the depth is wrong, an error is signaled.                          
If the depth is right but the count is not used up, nil is returned.*/    

Built_In_Primitive(Prim_Scan_List_Forward, 7, "SCAN-LIST-FORWARD")
{ long Depth, Sexp_P, Ignore_P;
  long Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End;
  Pointer *SyntaxTable_Start;
  char *Scan_Lists_Forward(); /*forward reference */
  Primitive_7_Args();
  Check_4_Args();				    
  Arg_5_Type(TC_FIXNUM);			    
 
  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);		    
  Depth = Get_Integer(Arg5);			    
  Sexp_P = (Arg6 == NIL) ? false : true;	    
  Ignore_P = (Arg7 == NIL) ? false : true;	    

  Point = Scan_Lists_Forward(Point, Depth, Sexp_P, 
			     Ignore_P, SyntaxTable_Start, 
			     Gap_Start, Gap_End,
			     Region_End);
  if (Point == false) return NIL;
  return FIXNUM_0 + (Point - String_Start);	    
}						    



char *
Scan_Lists_Forward (Point, Depth, Sexp_P, 
		    Ignore_P, SyntaxTable_Start, 
		    Gap_Start, Gap_End, Region_End)
     char *Point, *Gap_Start, *Gap_End, *Region_End;
     long Depth, Sexp_P, Ignore_P;
     Pointer *SyntaxTable_Start;
{ long Val, Gap_Length;
  register long Ascii;
  char StringTerm, *ATemp;
  int Quoted, Min_Depth, MathExit;
  /*register enum syntaxcode SyntaxCode;*/
  register long SyntaxCode;

  Gap_Length = Gap_End - Gap_Start;
  Min_Depth = (Depth > 0) ? 0 : Depth;		    /* Err out if depth gets less than this. */
  MathExit = false;
  
  while (Point < Region_End)
    { Ascii = CharAt (Point);
      SyntaxCode = CChar_To_Self_Syntax_Code(Ascii, SyntaxTable_Start);
      Increment(Point);
      if ((Point < Region_End) 
	  && Two_Char_Start_Comment(Ascii, CharAt(Point))
	  && Ignore_P)
	{ SyntaxCode = Scomment;
	  Increment(Point); 
	}
 /* at this point, Point is just ahead of the place SyntaxCode is the code of*/
      switch ((int) SyntaxCode)
	{
	case Sescape:
	  Dont_Break_Just_Continue;
	case Scharquote:                                   
	  if (Point == Region_End) goto lose;                                
	  Increment(Point);  /* go even one more forward */
	  Dont_Break_Just_Continue;
	  /* assumption: these two cases drop down to those for Sword */
	case Sword:          
	  Dont_Break_Just_Continue;
	case Ssymbol:                                                
	  if (Depth || !Sexp_P) BREAK_OUT_OF_SWITCH;                              
	  /* This word counts as a sexp; return at end of it. */      
	  FINISH_WORD_Off(Point);
	  goto done;
	case Scomment:                           
	  if (!Ignore_P) BREAK_OUT_OF_SWITCH;                     
	  SKIP_OVER_COMMENT(Point);
	  BREAK_OUT_OF_SWITCH;

	case Smath:                                                  
	  if (!Sexp_P)
	    BREAK_OUT_OF_SWITCH;
	  if ((Point != Region_End) && (Ascii == CharAt (Point)))
	    Increment(Point);
	  if (MathExit) goto close_forward;
	  MathExit = true;
	  Dont_Break_Just_Continue;

	  /* assumption: case smath drops down to sopen, unless it goes to close1 */
	case Sopen:                                     
	  if (!++Depth) goto done;                                    
	  BREAK_OUT_OF_SWITCH;                                                      

	case Sclose:
	close_forward:
	  if (!--Depth) goto done;
	  if (Depth < Min_Depth)
	    return false;
	  BREAK_OUT_OF_SWITCH;

	case Sstring:                                                
	  ATemp =  Point;
	  Decrement(ATemp);
	  StringTerm = CharAt(ATemp);
	  SKIP_OVER_STRING(Point, StringTerm);
	  Increment(Point);
	  if (Point == Region_End) goto lose;
	  if (!Depth && Sexp_P) goto done;
	  BREAK_OUT_OF_SWITCH;
	}
    }
  if (Depth) goto lose;			    /* Reached end of buffer.  Error if within object*/
  return false;				    /* return nil if between   End of object reached */

 done_with_word:
 done:		
  return Point;
 
 lose_eof_in_quote:
 lose_eof_quoted_in_string:
 lose:
  return false;
  /* NOT REACHED (i hope) */
}


 
Built_In_Primitive(Prim_Scan_List_Backward, 7, "SCAN-LIST-BACKWARD")
{ long Depth, Sexp_P, Ignore_P;
  long Gap_Length;
  char *String_Start, *Region_Start, *Point, *Gap_Start;
  char *Gap_End, *Region_End;
  Pointer *SyntaxTable_Start;
  char *Scan_List_Backward(); /*forward reference */
  Primitive_7_Args();
  Check_4_Args();				    
  Arg_5_Type(TC_FIXNUM);			    
 
  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_End,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);		    
  Depth = Get_Integer(Arg5);			    
  Sexp_P = (Arg6 == NIL) ? false : true;	    
  Ignore_P = (Arg7 == NIL) ? false : true;	    

  Point = Scan_List_Backward(Point, Depth, Sexp_P, 
			      Ignore_P, SyntaxTable_Start, 
			      Gap_Start, Gap_End,
			      Region_End);
  if (Point == false) return NIL;
  return FIXNUM_0 + (Point - String_Start);	    
}						 


char *
Scan_List_Backward(Point, Depth, Sexp_P, 
		    Ignore_P, SyntaxTable_Start, 
		    Gap_Start, Gap_End, Region_Start)
     char *Point, *Gap_Start, *Gap_End, *Region_Start;
     long Depth, Sexp_P, Ignore_P;
     Pointer *SyntaxTable_Start;
{ long Val, Gap_Length, Quoted;
  register long Ascii;
  char StringTerm, *Left;
  long Min_Depth, MathExit;
  /*register enum syntaxcode SyntaxCode;*/
  register long SyntaxCode;

  Gap_Length = Gap_End - Gap_Start;
  Min_Depth = (Depth > 0) ? 0 : Depth;
  /* Err out if depth gets less than this. */
  MathExit = false;

  while (Point > Region_Start)
    { Decrement(Point);
      Quoted = Quoted_P(Point, Region_Start,
			Gap_Start, Gap_End, 
			SyntaxTable_Start);
      if (Quoted) Decrement(Point);
      Ascii = CharAt(Point);
      SyntaxCode = CChar_To_Self_Syntax_Code(Ascii, SyntaxTable_Start);
      One_Less_Than(Point, Left)
      if ((Point > Region_Start)
	  && Two_Char_End_Comment(Ascii, CharAt(Left))
	  && !Quoted_P(Left)
	  && Ignore_P)
	{ SyntaxCode = Sendcomment;
	  Point = Left;
	  One_Less_Than(Point, Left);
	}
     /* At this point Left is just one behind the SyntaxCode's Point
	 and Point >= Region_Start */
      switch ((int) (Quoted ? Sword : SyntaxCode))
	{
	case Sword:
	  Dont_Break_Just_Continue;
	case Ssymbol:
	  if (Depth || !Sexp_P) BREAK_OUT_OF_SWITCH;
	  /* Else, this word counts as a sexp;
	     return at end of it. */
	  FINISH_WORD_Backwards(Left);
	  goto done_backwards;

	case Smath:
	  if (!Sexp_P) BREAK_OUT_OF_SWITCH;
	  if ((Left != Region_Start) && (Ascii == CharAt(Left)))
	    { Point = Left;
	      One_Less_Than(Point, Left);
	    }
	  if (MathExit) goto open_backwards;
	  MathExit = true;
	  Dont_Break_Just_Continue;
	  
	case Sclose:
	  if (!++Depth) goto done_backwards;
	  BREAK_OUT_OF_SWITCH;
	  
	case Sopen:
	open_backwards:
	  if (!--Depth) goto done_backwards;
	  if (Depth < Min_Depth)	
	    return false;
  	  BREAK_OUT_OF_SWITCH;

	case Sendcomment:
	  if (!Ignore_P) break;
	  if (Left != Region_Start) 
	    { Point = Left;
	      One_Less_Than(Point, Left);
	    }
	  SKIP_OVER_COMMENT_Back(Point);
	  BREAK_OUT_OF_SWITCH;

	case Sstring:
	  SKIP_OVER_STRING_Back(Left);
	  BREAK_OUT_OF_SWITCH;
	}
    }
  if (Depth) goto lose_backwards;
  /* Reached end of buffer.  Error if within object*/

 done_backwards:			    /* End of object reached */
  return Point;
  
 lose_backwards:
  return false;/* NOTREACHED */
}

  


char *
Quoted_From_Where(Point, Region_Start, Gap_Start, Gap_End, SyntaxTable_Start)	    
     char *Point, *Region_Start, *Gap_End, *Gap_Start;
     Pointer *SyntaxTable_Start;
{ long Number_of_Quotes, Gap_Length;
  char *Left, *Point_Start;
  Gap_Length = Gap_End - Gap_Start;

  while (Point > Region_Start)
    { One_Less_Than(Point, Left);
      if (!It_Is_A_(Squote, Left, SyntaxTable_Start)) break;
      Point = Left;
    }

  return Point;
}

Quoted_P(Point, Region_Start, Gap_Start, Gap_End, SyntaxTable_Start)	    
     char *Point, *Region_Start, *Gap_End, *Gap_Start;
     Pointer *SyntaxTable_Start;
{ long Quoted_Length;
  
  Quoted_Length = Point -
                  Quoted_From_Where(Point, Region_Start,
				    Gap_Start, Gap_End,
				    SyntaxTable_Start);
  return (Odd(Quoted_Length));
}


/*
Sscan_sexps, 2, 2, 0,
Scan from character number FROM by COUNT balanced expressions.
Returns the character number of the position thus found.
Comments are ignored if parse-sexp-ignore-comments is non-nil.
If the beginning or end of (the visible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings but before count is used up,
nil is returned.*/


/* (scan-sexps-forward table group start end
                       target-depth stop-before? old-state)
		       5            6            7        */


Define_Primitive(Prim_Scan_Sexps_Forward, 7, "SCAN-SEXPS-FORWARD")
{ long State_Quoted, State_In_Comment, State_In_String;
  long Depth;
  char *Level_Array[Level_Array_Size], **Level_Start, **Level_End, **Level, **Level_Array_End;
  long Target_Depth, Stop_Before, Start_Quoted, SSF_Frame_Size;

  long Gap_Length, SyntaxCode;
  char *String_Start, *Region_Start, *Point, *Gap_Start, *ATemp;
  char *Gap_End, *Region_End, c;
  Pointer *SyntaxTable_Start, *Hold_Free, **Value;

  Primitive_7_Args();
  Check_4_Args();

  SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, 
		   Gap_Length, Gap_End, Region_End,
		   Arg2);
  SetUp_Point_Vars(Point, Region_Start,
		   String_Start, Gap_Start, Gap_Length,
		   Arg3, Arg4);
  SyntaxTable_Start = Beginning_Of_Vector(Arg1);

  Target_Depth = Get_Integer(Arg5);
  SSF_INIT(Stop_Before, Level_Start, Level_Array_End,
	   Level_End, Level_Array, Depth, State_In_Comment,
	   State_Quoted, Start_Quoted, State_In_String, Arg6);
  /* get old state */
  if (Arg7 != NIL) 
    { Arg_7_Type(TC_VECTOR);
      Put_In_Old_State(Depth, State_In_String, State_In_Comment, Start_Quoted, Arg7);
    }
  Level = Level_Start;
  Level[1] = 0;
  /* enter main loop at appropriate time for init state. */

  Go_Into_Loop_Wherever(State_In_Comment, State_In_String, Start_Quoted);
  /* loop */
  while (Point < Region_End)
    { c = CharAt(Point);
      SyntaxCode = CChar_To_Self_Syntax_Code(c, SyntaxTable_Start);
      Increment(Point);
      if (Two_Char_Start_Comment(c, CharAt(Point))
	  && (Point < Region_End))
	SyntaxCode = Scomment;

      /* super duper switcher */
      switch ((int) SyntaxCode)
	{
	case Squote:
	case Sescape:
	case Sword:
	case Ssymbol:
	ssf_start_quoted:
	  if (Stop_Before) goto ssf_stop;
	  *Level = Point;
	  if (Start_Quoted)
	    { if (Point == Region_End)
		{ State_Quoted = 1;
		  goto done;
		}
	    else Increment(Point);
	    }	
	ssf_start_atom:
	  FINISH_WORD_Off(Point);
	done_with_word:
	ssf_end_atom:
	  *(Level+1) = *Level;
	  BREAK_OUT_OF_SWITCH;
	  
	lose_eof_in_quote:
	ssf_end_quoted: 
	lose_eof_quoted_in_string:
	  State_Quoted = -1;
	  goto done;
	  
	case Scomment:
	ssf_start_in_comment:
	  GO_OVER_ssf_COMMENT(Point);
	  State_In_Comment = 0;
	  BREAK_OUT_OF_SWITCH;
	  
	ssf_start_in_comment2:
	  GO_OVER_CStyle_ssf_COMMENT(Point)
	  State_In_Comment = 0;
	  BREAK_OUT_OF_SWITCH;
	  
	case Sopen:
	  if (Stop_Before) goto ssf_stop;
	  Bump_Up_A_Level(Point, Level, Depth);
	  if (!--Target_Depth) goto done;
	  BREAK_OUT_OF_SWITCH;
	  
	case Sclose:
	  Drop_Down_A_Level(Point, Level, Depth);
	  if (!++Target_Depth) goto done;
	  BREAK_OUT_OF_SWITCH;

	case Sstring:
	  if (Stop_Before) goto ssf_stop;
	  *Level = Point;
	  Get_State_In_String(State_In_String, Point);
	ssf_start_in_string:
	  SKIP_OVER_STRING(Point, State_In_String)
	  State_In_String = -1;
	  *(Level+1) = *Level;
	  Increment(Point);
	  BREAK_OUT_OF_SWITCH;
	}
    }
  
 ssf_stop:
  Decrement(Point);

 done:
  Allocate_Vector(7, Hold_Free);
  PUT_STATE_INTO_VECTOR(Hold_Free, Depth, State_In_String, State_In_Comment, State_Quoted, Level, Point);
  return Make_Pointer(TC_VECTOR, Hold_Free);
}



