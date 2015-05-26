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

/*          Hey EMACS, this is -*- C -*- code!                 */

/*
  Symbol-processing extensions for common lisp.
*/

#include "scheme.h"
#include "primitive.h"
#include "trap.h"
#include "stringprim.h"
#include "cl-symbol.h"

#include <ctype.h>

Define_Primitive(prim_make_symbol,1,"MAKE-SYMBOL")
{ 
  Pointer String, New_Symbol;

  Primitive_1_Arg();
  Arg_1_Type(TC_CHARACTER_STRING);
  String = Arg1;

  Primitive_GC_If_Needed(2);
  New_Symbol = Make_Pointer(TC_INTERNED_SYMBOL, Free);
  Free[SYMBOL_NAME] = String;
  Free[SYMBOL_GLOBAL_VALUE] = UNASSIGNED_OBJECT;
  Free += 2;
  add_clsav(New_Symbol);
  return New_Symbol;
}

set_symbol_global_value(symbol)
Pointer symbol;
{
  if (!CL_Mode()) return;
  if ((symbol == TRUTH) || (symbol == NIL)) return;
  if (lexical_unboundp(NIL, symbol) != NIL)
    local_assignment(NIL, symbol, UNASSIGNED_OBJECT);
}

Pointer 
cl_get_symbol_name(symbol)
Pointer symbol;
{
  Pointer Temp = Vector_Ref(symbol, SYMBOL_NAME);
  if (SYMBOL_P(Temp)) Temp = Vector_Ref(Temp, SYMBOL_NAME);
  if (Type_Code(Temp) == TC_CLSAV) return(User_Vector_Ref(Temp, CLSAV_NAME));
  else return(Temp);
}

add_clsav(symbol)
Pointer symbol;
{ long i;
  Pointer Old_Symbol_Name, The_Vector, New_Symbol;

  Old_Symbol_Name = cl_get_symbol_name(symbol);
  Primitive_GC_If_Needed(CLSAV_SIZE+1+2);
  The_Vector = Make_Pointer(TC_CLSAV, Free);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, CLSAV_SIZE);
  for (i = 0; i < CLSAV_SIZE; i++) *Free++ = NIL;
  User_Vector_Set(The_Vector,
		  CLSAV_NAME,
		  Make_Pointer(TC_CHARACTER_STRING, Old_Symbol_Name));
  Vector_Set(symbol, SYMBOL_NAME, The_Vector);

  New_Symbol = Make_New_Pointer(TC_UNINTERNED_SYMBOL, Free);
  Free[SYMBOL_NAME] = symbol;
  Free[SYMBOL_GLOBAL_VALUE] = UNASSIGNED_OBJECT;
  Free += 2;
  User_Vector_Set(The_Vector, CLSAV_FUNCTION_SYMBOL, New_Symbol);
}

Define_Primitive(prim_cl_add_clsav,1,"CL-ADD-CLSAV")
{
  Primitive_1_Arg();
  if ((Type_Code(Arg1) != TC_INTERNED_SYMBOL) &&
      (Type_Code(Arg1) != TC_UNINTERNED_SYMBOL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  add_clsav(Arg1);
  return(Arg1);
}

Pointer
get_clsav(Arg1)
Pointer	Arg1;
{
  Pointer result;

  result = Vector_Ref(Arg1, SYMBOL_NAME);
  if (Type_Code(result) != TC_CLSAV)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (Type_Code(User_Vector_Ref(result, CLSAV_FUNCTION_SYMBOL)) ==
      TC_INTERNED_SYMBOL)
  { printf("Error, interned symbol in info block\n");
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  return(result);
}

Pointer
cl_get_symbol_package(symbol)
Pointer	symbol;
{
	Pointer clsav;

	clsav = get_clsav(symbol);
	return User_Vector_Ref(clsav, CLSAV_PACKAGE_CELL);
}

Define_Primitive(prim_cl_get_clsav,1,"CL-GET-CLSAV")
{
  Primitive_1_Arg();
  if ((Type_Code(Arg1) != TC_INTERNED_SYMBOL) &&
      (Type_Code(Arg1) != TC_UNINTERNED_SYMBOL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  return get_clsav(Arg1);
}

Define_Primitive(prim_cl_get_name,1,"CL-GET-SYMBOL-NAME")
{
  Primitive_1_Arg();
  if ((Type_Code(Arg1) != TC_INTERNED_SYMBOL) &&
      (Type_Code(Arg1) != TC_UNINTERNED_SYMBOL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  return(cl_get_symbol_name(Arg1));
}

Boolean
cl_expanded_symbol_p(sym)
Pointer sym;
{
  if (Type_Code(Vector_Ref(sym, SYMBOL_NAME)) == TC_CLSAV)
    return true;
  else
    return false;
}

Define_Primitive(prim_cl_expanded_symbol_p, 1,"CL-EXPANDED-SYMBOL?")
{
  Primitive_1_Arg();
  if ((Type_Code(Arg1) != TC_INTERNED_SYMBOL) &&
      (Type_Code(Arg1) != TC_UNINTERNED_SYMBOL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  if (cl_expanded_symbol_p(Arg1))
    return TRUTH;
  else
    return NIL;
}

Define_Primitive(prim_cl_function_symbol_p,1,"CL-FUNCTION-SYMBOL?")
{
  Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);

  if (SYMBOL_P(Arg1) && (SYMBOL_P(Vector_Ref(Arg1, SYMBOL_NAME))))
    return TRUTH;
  else return NIL;
}

Define_Primitive(prim_cl_function_symbol_parent,1,"CL-GET-FUNCTION-SYMBOL-PARENT")
{
  Primitive_1_Arg();
  if (SYMBOL_P(Arg1) && (SYMBOL_P(Vector_Ref(Arg1, SYMBOL_NAME))))
    return Vector_Ref(Arg1, SYMBOL_NAME);
  else Primitive_Error(ERR_ARG_1_WRONG_TYPE);
}

/*
  Either returns the plist or #t if the symbol was not expanded,
  or is #t or #f. This is to avoid all the overhead for this
  common case.
*/

Define_Primitive(prim_cl_fast_symbol_plist, 1, "CL-FAST-SYMBOL-PLIST")
{
  long tc;
  Pointer clsav;

  Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);

  tc = Type_Code(Arg1);
  if ((tc != TC_INTERNED_SYMBOL) &&
      (tc != TC_UNINTERNED_SYMBOL))
    return TRUTH;
  clsav = Vector_Ref(Arg1, SYMBOL_NAME);
  if (Type_Code(clsav) != TC_CLSAV)
    return TRUTH;
  return User_Vector_Ref(clsav, CLSAV_PROPERTY_LIST);
}

Define_Primitive(prim_cl_fast_system_symbol_plist, 1, "CL-FAST-SYSTEM-SYMBOL-PLIST")
{
  long tc;
  Pointer clsav;

  Primitive_1_Arg();
  Touch_In_Primitive(Arg1, Arg1);

  tc = Type_Code(Arg1);
  if ((tc != TC_INTERNED_SYMBOL) &&
      (tc != TC_UNINTERNED_SYMBOL))
    return TRUTH;
  clsav = Vector_Ref(Arg1, SYMBOL_NAME);
  if (Type_Code(clsav) != TC_CLSAV)
    return TRUTH;
  return User_Vector_Ref(clsav, CLSAV_SYSTEM_INFO_CELL);
}

Define_Primitive(prim_cl_string_capitalize, 1, "CL-STRING-CAPITALIZE")
{ long i;
  char c, *s;
  Pointer copy;
  Boolean in_word;
  Primitive_1_Arg();
  Arg_1_Type(TC_CHARACTER_STRING);

  copy = C_String_To_Scheme_String(Scheme_String_To_C_String(Arg1));
  s = Scheme_String_To_C_String(copy);

  in_word = false;
  for (i = 0; s[i] != '\0'; i++)
  { if (in_word)
    { if (!(isalnum(s[i])))
	in_word = false;
      else if (isalpha(s[i]))
	{ if (isupper(s[i]))
	    s[i] = tolower(s[i]);
	}
    }
    else
    { if (isalnum(s[i]))
      { if (isalpha(s[i]))
	{ if (islower(s[i]))
	    s[i] = toupper(s[i]);
	}
	in_word = true;
      }
    }
  }
  return copy;
}
  
Define_Primitive(prim_string_capitalize_bang, 1, "CL-STRING-CAPITALIZE!")
{ long i;
  char c, *s;
  Boolean in_word;
  Primitive_1_Arg();
  Arg_1_Type(TC_CHARACTER_STRING);

  s = Scheme_String_To_C_String(Arg1);

  in_word = false;
  for (i = 0; s[i] != '\0'; i++)
  { if (in_word)
    { if (!(isalnum(s[i])))
	in_word = false;
      else if (isalpha(s[i]))
	{ if (isupper(s[i]))
	    s[i] = tolower(s[i]);
	}
    }
    else
    { if (isalnum(s[i]))
      { if (isalpha(s[i]))
	{ if (islower(s[i]))
	    s[i] = toupper(s[i]);
	}
	in_word = true;
      }
    }
  }
  return Arg1;
}
  
