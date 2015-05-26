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

/* $Header: scanprim.h,v 10.0 88/12/07 13:10:27 las Exp $ */

#define Check_4_Args()   \
{ Arg_1_Type(TC_VECTOR); \
  Arg_2_Type(TC_VECTOR); \
  Arg_3_Type(TC_FIXNUM); \
  Arg_4_Type(TC_FIXNUM); \
  }

#define CharAt(pointer) (*(pointer))
#define SYNTAX(c, table_start) CharAt(table_start + c)


#define Swhitespace               0
#define Spunct			  1
#define Sword			  2
#define Ssymbol			  3
#define Sopen			  4
#define Sclose			  5
#define Squote			  6
#define Sstring			  7
#define Smath			  8
#define Sescape			  9
#define Scharquote		  10
#define Scomment		  11
#define Sendcomment		  12
#define Sprefix			  Squote
#define Smax                      12
#define COMSTART_FIRST_PLACE	  16
#define COMSTART_SECOND_PLACE	  17
#define COMEND_FIRST_PLACE	  18
#define COMEND_SECOND_PLACE	  19

#define CChar_To_Self_Syntax_Code(CChar, SyntaxTable_Start)	\
(MASK_ASCII & Get_Integer(SyntaxTable_Start[CChar]))

#define Beginning_Of_Vector(VectorArg) (Nth_Vector_Loc(VectorArg, 1))
/* Syntax table Start should be the result of beginning of vector
called on the Scheme Syntax Vector.  Doing the scheme_to_c vector conversion
at this level saves instructions in loops */

#define It_Is_A_(Stype, Point, SyntaxTable_Start)\
((Stype) == CChar_To_Self_Syntax_Code(CharAt(Point), SyntaxTable_Start))

#define Fix_Gap_If_Needed(Point, Gap_Start, Length)		\
{if ((Point) >= (Gap_Start)) (Point) += (Length);}

#define Fix_Gap_For_Dec(Point, Gap_End, Gap_Start)		\
{if (Point == Gap_End) (Point) = (Gap_Start);}

#define Fix_Gap_For_Inc(Point, Gap_Start, Gap_End)		\
{if (Point == Gap_Start) (Point) = (Gap_End);}

#define Increment(Point)					\
{ Point++;							\
  Fix_Gap_For_Inc((Point), Gap_Start, Gap_End);			\
  }

#define Decrement(Point)					\
{ Fix_Gap_For_Dec(Point, Gap_End, Gap_Start);			\
  Point--;							\
  }

#define One_Less_Than(Point, Left)				\
{ Left = Point;							\
  Decrement(Left);						\
  }

#define GAP_START_PLACE 3
#define GAP_LENGTH_PLACE 4
#define GAP_END_PLACE 5
#define REG_START_PLACE 7
#define REG_END_PLACE 8
#define STR_START_PLACE 2

#define Get_Number_From_Vector(Vector_Arg, Vector_Place)	\
(Get_Integer(Vector_Ref(Vector_Arg, Vector_Place)))

#define Get_String_Position(String_Start, Group_Vector_Start, Place) \
((String_Start) + (Get_Integer(Group_Vector_Start[Place])))


/* enter gap_length as negative to subtract gap, as positive to add gap */

/*    group -> string with place #2 in vector (zero-indexed)
      ----------->abcdefghijkl~~~~~~~~~~~~~~~~~mnopqr
                  ^  ^   ^    ^<-  b - a     ->^  ^ ^
                  |  |   |    |                |  | |
                  0  s   p    a                b  e c
		  each of these silly one letter variables 
		  is associated with a pointer to a character.
		  The pointers have much better names.
Dumb one letter name -> Legible name for a pointer to a char.
		  0 -> string_start
		  s -> region_start
		  p -> point
		  a -> gap_start
		  b -> gap_end
		  e -> region_end
		  c either isn't needed or == e, in which case
		           it isn't needed.
		  b - a -> gap_length, which is a long, not 
		           a pointer of any sort.

		 SetUp_Group_Vars does all of this, taking as arguments:
			all the legible names, and the Group Arg
		 SetUp_Point_Vars does this for the point, and adjusts
			region_end if need be, taking as args
			(point, region_end, and the 2 args startarg & end arg.)
*/

/* sets up the scheme-to-c interface.  In the best of all possible
worlds, these would be c globals, but this isn't and they aren't  */

#define SetUp_Group_Vars(String_Start, Region_Start, Gap_Start, Gap_Length, Gap_End, Region_End, Group_Arg) \
{ Pointer *Group_Vector_Start;					\
  Group_Vector_Start = Beginning_Of_Vector(Group_Arg);		\
  String_Start =						\
    Scheme_String_To_C_String(Group_Vector_Start[STR_START_PLACE]); \
/* that line ^ is tricky - the String is held in the vector - STONE */ \
  Gap_Length = Get_Integer(Group_Vector_Start[GAP_LENGTH_PLACE]); \
  Region_Start =						\
    Get_String_Position(String_Start, Group_Vector_Start, REG_START_PLACE); \
  Gap_Start =							\
    Get_String_Position(String_Start, Group_Vector_Start, GAP_START_PLACE); \
  Gap_End =							\
    Get_String_Position(String_Start, Group_Vector_Start, GAP_END_PLACE); \
  Region_End =							\
    Get_String_Position(String_Start, Group_Vector_Start, REG_END_PLACE); \
  }
  /* Region_End will generally be disregarded from SetUp_Point_Vars
     and perhaps should not be assigned here if always disregarded  */

#define SetUp_Point_Vars(Point, Region_Delimiter, String_Start, Gap_Start, Gap_Length, Arg3, Arg4) \
{ Region_Delimiter = String_Start + Get_Integer(Arg4);		\
  Fix_Gap_If_Needed(Region_Delimiter, Gap_Start, Gap_Length);	\
  Point = String_Start + Get_Integer(Arg3);			\
  Fix_Gap_If_Needed(Point, Gap_Start, Gap_Length);		\
}

#define NO_SYNTAX_SPEC 0377

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things. */
/* ASCII (1 -> 128) --->--->---> } syntax_spec_code { --->--->---> Swhatever */
char Syntax_Spec_Code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, 0377, (char) Sstring, 0377,
        (char) Smath, 0377, 0377, (char) Squote,
    (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it. */
/* Swhatever --->--->---> } syntax_spec_code
   { --->--->---> Representative ASCII (1 -> 256) */

char Syntax_Code_Spec[13] =
  { ' ',       /* for a whitespace character */                          
    '.',       /* for random punctuation characters */                   
    'w',       /* for a word constituent */                              
    '_',       /* symbol constituent but not word constituent */         
    '(',       /* for a beginning delimiter */                           
    ')',       /* for an ending delimiter */                             
    '\'',      /* for a prefix character like Lisp ' */                  
    '\"',      /* for a string-grouping character like Lisp " */         
    '$',       /* for delimiters like $ in Tex. */                       
    '\\',      /* for a character that begins a C-style escape */        
    '/',       /* for a character that quotes the following character */ 
    '<',       /* for a comment-starting character */                    
    '>'	       /* for a comment-ending character:
		  Upper bound on codes that are meaningful */
  };
#define Diff(High, Low, Diff_Place)				\
{ (Diff_Place) = (High) - (Low);				\
  if ((High) > (Gap_Start) && (Low) < (Gap_Start))		\
    (Diff_Place) -= (Gap_Length);				\
}


#define Odd(Counter) ((Counter) & 1)


/* had been used until I realized it was wrong.
  Hanging around just in case it wasn't - stone */
/* had been in scan-word-backwards and scan-back-prefix-chars */
#define Correct_Point_For_Decing_Too_Far(Point, Point_Start)	\
{  long Number_Of_Quotes;					\
   char *Quote_Start;						\
								\
   Quote_Start = Quoted_From_Where(Point, Region_Start,		\
				   Gap_Start, Gap_End, SyntaxTable_Start); \
   Diff(Point, Quote_Start, Number_Of_Quotes);			\
   if (Odd(Number_Of_Quotes))					\
     Increment(Point);						\
   if (Point > Point_Start) Point = Point_Start;		\
   }

#define BYTE_SIZE 8 /*size of a byte*/
#define TWO_BYTES 16
#define ASCII_SPACE 0x20
#define BREAK_OUT_OF_SWITCH break
#define Dont_Break_Just_Continue 
#define ASCII_ONE '1'
#define Check_Bit_And_Put_It_In(String_Start, Syntax_Entry, bit) \
{ bit  = (*String_Start) - ASCII_ONE;				\
  if ((bit >= 0) && (bit <= 3))					\
    Syntax_Entry |= (1 << (bit + TWO_BYTES));			\
}

#define SYNTAX_COMSTART_FIRST(c)				\
((SYNTAX(c, SyntaxTable_Start) >> COMSTART_FIRST_PLACE) & 1)

#define SYNTAX_COMSTART_SECOND(c)				\
((SYNTAX(c, SyntaxTable_Start) >> COMSTART_SECOND_PLACE) & 1)

#define SYNTAX_COMEND_FIRST(c)					\
((SYNTAX(c, SyntaxTable_Start) >> COMEND_FIRST_PLACE) & 1)

#define SYNTAX_COMEND_SECOND(c)					\
((SYNTAX(c, SyntaxTable_Start) >> COMEND_SECOND_PLACE) & 1)

#define Two_Char_Start_Comment(c, PointChar)			\
(SYNTAX_COMSTART_FIRST(c) && SYNTAX_COMSTART_SECOND(PointChar))

#define String_End(String_Length)	(!(--String_Length))

#define FINISH_WORD_Off(Point)					\
{ while (Point < Region_End)					\
    {								\
      switch ((int) CChar_To_Self_Syntax_Code(CharAt(Point),	\
					      SyntaxTable_Start)) \
	{							\
	case Scharquote:					\
	case Sescape:						\
	  Increment(Point);					\
	  if (Point == Region_End) goto lose_eof_in_quote;	\
	  BREAK_OUT_OF_SWITCH;					\
	case Sword:						\
	case Ssymbol:						\
	  BREAK_OUT_OF_SWITCH;					\
	default:						\
	  goto done_with_word;					\
	}							\
      Increment(Point);						\
    }								\
 }   

#define Two_Char_End_Comment(c, PointChar)		      \
(SYNTAX_COMEND_FIRST(c) && SYNTAX_COMEND_SECOND(PointChar))

#define SKIP_OVER_COMMENT(Point)				\
{ long c;							\
  while (true)							\
    { if (Point == Region_End) goto lose;			\
      c = CharAt(Point);					\
      if (CChar_To_Self_Syntax_Code(c, SyntaxTable_Start) == Sendcomment) \
	break;							\
      Increment(Point);						\
      if ((Point < Region_End) && Two_Char_End_Comment(c, CharAt(Point))) \
	{ Increment(Point);					\
          break;}						\
	}							\
}



#define SKIP_OVER_STRING(Point, stringterm)			\
{ long c;							\
  while (Point < Region_End)					\
    { c = CharAt(Point);					\
      if (c == stringterm) break;				\
      switch ((int) CChar_To_Self_Syntax_Code(c, SyntaxTable_Start)) \
	{ case Scharquote:					\
          case Sescape:						\
	  ssf_start_quoted_in_string:				\
            if (Region_End == Point) goto lose_eof_quoted_in_string; \
	    Increment(Point);					\
	}							\
      Increment(Point);						\
    }								\
}


#define Level_Array_Size 200

#define FINISH_WORD_Backwards(Left)				\
{ while (Left >= Region_Start)					\
    { Quoted = Quoted_P(Left);					\
      if (Quoted)						\
	{ Point = Left;						\
	  One_Less_Than(Point, Left);				\
	}							\
      if (! (Quoted ||						\
	     It_Is_A_(Sword, Left, SyntaxTable_Start) ||	\
	     It_Is_A_(Ssymbol, Left, SyntaxTable_Start)))	\
	goto done_backwards;					\
      Point = Left;						\
      One_Less_Than(Point, Left);				\
    }								\
}

#define SKIP_OVER_COMMENT_Back(Left)				\
{  while (true)							\
     { if (It_Is_A_(Scomment, Left, SyntaxTable_Start)) break;	\
       if (Left == Region_Start) return false;			\
       Point = Left;						\
       One_Less_Than(Point, Left);				\
       if (Two_Char_Start_Comment(CharAt(Left), CharAt(Point))	\
	  && !Quoted_P(Left))					\
	 break;							\
    }								\
}

#define SKIP_OVER_STRING_Back(Left)			    \
{ StringTerm = CharAt(Point);				    \
  while (true)						    \
    { if (Left == Region_Start) goto lose_backwards;	    \
      if (!Quoted_P(Left)				    \
          && (CharAt (Left) == StringTerm))		    \
        break;						    \
      Decrement(Left)					    \
	}						    \
  Point = Left;						    \
  One_Less_Than(Point, Left);				    \
  if (!Depth && Sexp_P) goto done_backwards;		    \
  break;						    \
}

/* ssf macs */

#define VECTOR_SIZE_INDEX -1

#define Put_In_Old_State(Depth, State_In_String, State_In_Comment, Start_Quoted, Arg_State); \
{ Pointer *State_Vector_Start;				    \
  long Dummy;						    \
							    \
  State_Vector_Start = Beginning_Of_Vector(Arg_State);	    \
  if (Get_Integer(*(State_Vector_Start + VECTOR_SIZE_INDEX)) \
      != 7) Primitive_Error(ERR_ARG_7_BAD_RANGE);	    \
  Dummy = State_Vector_Start[0];			    \
  if (Dummy == NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);  \
  Depth = Get_Integer(Dummy);				    \
  Dummy = State_Vector_Start[1];			    \
  if (Dummy == NIL) Primitive_Error(ERR_ARG_2_WRONG_TYPE);  \
  State_In_String = Get_Integer(Dummy);			    \
  Dummy = State_Vector_Start[2];			    \
  if (Dummy == NIL) Primitive_Error(ERR_ARG_3_WRONG_TYPE);  \
  State_In_Comment = Get_Integer(Dummy);		    \
  Dummy = State_Vector_Start[3];			    \
  if (Dummy == NIL) Primitive_Error(ERR_ARG_4_WRONG_TYPE);   \
  Start_Quoted = Get_Integer(Dummy);			    \
}

#define SSF_INIT(Stop_Before, Level_Start, Level_Array_End, Level_End, Level_Array, Depth, State_In_Comment, State_Quoted, Start_Quoted, State_In_String, Arg6)                           \
{ Stop_Before = (Arg6 == NIL) ? 0 : -1;			    \
  Level_Start = Level_Array;				    \
  Level_Array_End = Level_Array + Level_Array_Size;	    \
  Level_End = Level_Array_End;				    \
  Depth = 0; State_In_Comment = 0; State_Quoted = 0;	    \
  Start_Quoted = 0; State_In_String = -1;		    \
}

#define Go_Into_Loop_Wherever(State_In_Comment, State_In_String, Start_Quoted) \
{ if (State_In_Comment)					    \
    { if (State_In_Comment == 1)			    \
        goto ssf_start_in_comment;			    \
      goto ssf_start_in_comment2;			    \
    }							    \
  if (State_In_String >= 0)				    \
    { if (Start_Quoted)					    \
	goto ssf_start_quoted_in_string;	   	    \
      goto ssf_start_in_string;				    \
    }							    \
 if (Start_Quoted) goto ssf_start_quoted;		    \
}

#define GO_OVER_ssf_COMMENT(Point)			      \
{ while (true)						      \
    { if (Point >= Region_End)				      \
	{ State_In_Comment = 1;				      \
	  goto done;					      \
	}						      \
      if (CChar_To_Self_Syntax_Code(CharAt(Point), SyntaxTable_Start) \
	  == Sendcomment)				      \
	break;						      \
      Increment(Point);					      \
    }							      \
}

#define GO_OVER_CStyle_ssf_COMMENT(Point)		      \
{ char c2;						      \
  c2 = CharAt(Point + 1);				      \
  while (true)						      \
    { c = c2;						      \
      if (Point == Region_End)				      \
	{ State_In_Comment = 2;				      \
	  goto done;					      \
	}						      \
      Increment(Point);					      \
      c2 = CharAt(Point);				      \
      if (Two_Char_End_Comment(c, c2))			      \
	break;						      \
    }							      \
}

#define Bump_Up_A_Level(Point, Level, Depth)		      \
{ *Level = Point;					      \
  Depth++;						      \
  Level += 2;						      \
  *Level = 0;						      \
  *(Level+1) = 0;					      \
}

#define Drop_Down_A_Level(Point, Level, Depth)		      \
{ Depth--;						      \
  if (Level != Level_Start) Level -= 2;			      \
  *(Level+1) = *Level;					      \
}

/* returns a vector filled with
Depth
State-In-String
State-In-Comment
State-Quoted
End of Last Sexp        ]
Begining of Last Sexp   ]  not sure
Point     } fixed for gap
*/

#define PUT_STATE_INTO_VECTOR(Vector_Pointer, Depth, State_In_String, State_In_Comment, State_Quoted, Level, Point) \
{ char *Level_Contents;						\
  *++Vector_Pointer = Make_Unsigned_Fixnum(Depth);		\
  *++Vector_Pointer =						\
    (State_In_String >= 0) ?					\
      Make_Unsigned_Fixnum(State_In_String) : TC_FALSE;		\
  *++Vector_Pointer =						\
    (State_In_Comment) ?					\
      Make_Unsigned_Fixnum(State_In_Comment) : TC_FALSE;	\
  *++Vector_Pointer = (State_Quoted) ? TC_TRUE : TC_FALSE;	\
  Level_Contents = Level[1];					\
  if (Level_Contents)						\
    { Fix_Gap_If_Needed(Level_Contents, Gap_Start, -Gap_Length); \
      *++Vector_Pointer =					\
	Make_Unsigned_Fixnum(Level_Contents - String_Start - 1); \
    }								\
  else *++Vector_Pointer = TC_FALSE;				\
  Level_Contents = *(Level - 2);				\
  if (Level != Level_Start && Level_Contents)			\
    { Fix_Gap_If_Needed(Level_Contents, Gap_Start, -Gap_Length); \
      *++Vector_Pointer =					\
	Make_Unsigned_Fixnum(Level_Contents - String_Start - 1); \
    }								\
  else *++Vector_Pointer = TC_FALSE;				\
  Fix_Gap_If_Needed(Point, Gap_Start, Gap_Length);		\
  *++Vector_Pointer = Make_Unsigned_Fixnum(Point - String_Start); \
  Forward_Vector_Slots(Hold_Free, -7);				\
}

#define Forward_Vector_Slots(Vector_Pointer, Slot_Number)	\
{ Vector_Pointer += Slot_Number;}

#define   Allocate_Vector(Length, Vector_Pointer)		\
{ long Slots = Length + 1;					\
  Primitive_GC_If_Needed(Slots);				\
  *Free = Make_Non_Pointer(TC_MANIFEST_VECTOR, Length);		\
  Vector_Pointer = Free;					\
  Forward_Vector_Slots(Free, Slots);				\
}

#define Get_State_In_String(State_In_String, Point)	      \
{ char *ATemp;						      \
  ATemp = Point;					      \
  Decrement(ATemp);					      \
  State_In_String = CharAt(ATemp);			      \
}
