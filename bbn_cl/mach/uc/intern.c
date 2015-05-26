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

/* $Header: intern.c,v 10.0 88/12/07 13:08:28 las Exp $
 * $MIT-Header: intern.c,v 9.44 87/11/23 05:17:30 GMT cph Exp $
 */

/* Utilities for manipulating symbols. */

#include "scheme.h"
#include "primitive.h"
#include "trap.h"
#include "stringprim.h"

/* Hashing strings and character lists. */

long
Do_Hash (String_Ptr, String_Length)
     char *String_Ptr;
     long String_Length;
{
  fast long i, Value, End_Count;

  Value = (LENGTH_MULTIPLIER * String_Length);
  End_Count = ((String_Length > MAX_HASH_CHARS) ?
	       MAX_HASH_CHARS :
	       String_Length);
  for (i = 0; i < End_Count; i++)
    Value = ((Value << SHIFT_AMOUNT) + (MAX_CHAR & String_Ptr[i]));
  return (Value);
}

long
scheme_string_hash (string)
     Pointer string;
{
    int	hash_value;

    hash_value = Do_Hash (Scheme_String_To_C_String (string),
		          string_length(string));
    return hash_value;
}

Pointer
Hash (string)
     Pointer string;
{
  return (MAKE_SIGNED_FIXNUM (scheme_string_hash (string)));
}

/* NOTE: string_equal() belongs in string.c, and the string-equal?
   primitive should be written in terms of it.  Why it isn't done
   that way, even after the "great cleanup of july '87" I don't know.
   But I want you to know that I have resisted the urge to fix things
   up because I know that it will make merges harder in the future.
   The only reason I am inserting the large comment is that I have to
   add a procedure here anyway, so I may as well take advantage of that
   and let off a little steam - JP 7/8/87 */

Boolean 
c_string_equal_len(S1,Length1,S2,Length2)
     fast char *S1, *S2;
     long Length1, Length2;
{ 
  fast long i;

  if (S1 == S2) return true;
  if (Length1 != Length2)
    return false;
  for (i=0; i < Length1; i++)
    if (*S1++ != *S2++)
      return false;
  return true;
}

Boolean
string_equal (String1, String2)
     Pointer String1, String2;
{
  fast char *S1, *S2;
  fast long i, Length1, Length2;

  if (Address(String1) == Address(String2))
    return true;
  Length1 = ((long) (Fast_Vector_Ref(String1, STRING_LENGTH)));
  Length2 = ((long) (Fast_Vector_Ref(String2, STRING_LENGTH)));
  if (Length1 != Length2)
  {
    return false;
  }

  S1 = ((char *) Nth_Vector_Loc(String1, STRING_CHARS));
  S2 = ((char *) Nth_Vector_Loc(String2, STRING_CHARS));
  for (i = 0; i < Length1; i++)
  {
    if (*S1++ != *S2++)
    {
      return (false);
    }
  }
  return (true);
}

/* Interning involves hashing the input string and either returning
   an existing symbol with that name from the ObArray or creating a
   new symbol and installing it in the ObArray. The resulting interned
   symbol is stored in *Un_Interned. */

/* Set this to be informed of symbols as they are interned. */
void (*intern_symbol_hook) ();

extern void Intern();

void
Intern (Un_Interned)
     Pointer *Un_Interned;
{
  void add_symbol_to_ht();
  Pointer get_system_obarray();

  add_symbol_to_ht(Un_Interned, get_system_obarray());

  if (intern_symbol_hook)
    (*intern_symbol_hook) (*Un_Interned);

  New_Symbol_Hook(*Un_Interned);
}

Pointer
get_system_obarray()
{
  return (Get_Fixed_Obj_Slot (OBArray));
}

#ifndef butterfly

#define LOCK_TABLE(table)
#define UNLOCK_TABLE(table)

#else /* butterfly */

#define LOCK_TABLE(table)						\
{									\
  while (atomior(&SHARED_DATA->Intern_Lock, 0x8000) != 0)		\
    Standard_Delay();							\
}

#define UNLOCK_TABLE(table)						\
{									\
  SHARED_DATA->Intern_Lock = 0;						\
}

#endif /* butterfly */

void
add_symbol_to_ht(Un_Interned, Ob_Array)
     Pointer *Un_Interned, Ob_Array;
{
  fast Pointer string, *bucket, symbol;

  string = cl_get_symbol_name(*Un_Interned);
  bucket =
    (Nth_Vector_Loc (Ob_Array,
		     (((scheme_string_hash (string)) %
		       (Vector_Length (Ob_Array)))
		      + 1)));
  LOCK_TABLE(Ob_Array);
  while (*bucket != NIL)
  {
    symbol = (Vector_Ref (*bucket, CONS_CAR));
    if (string_equal(string, cl_get_symbol_name(symbol)))
    {
      *Un_Interned = symbol;
      UNLOCK_TABLE(Ob_Array);
      return;
    }
    bucket = (Nth_Vector_Loc (*bucket, CONS_CDR));
  }

  /* Symbol does not exist yet in obarray.  bucket points to the
     cell containing the final '() in the list.  Replace this
     with the CONS of the new symbol and '() (i.e. extend the
     list in the bucket by 1 new element). */

  Store_Type_Code (*Un_Interned, TC_INTERNED_SYMBOL);
  *bucket = (Make_Pointer (TC_LIST, Free));
  Free[CONS_CAR] = *Un_Interned;
  Free[CONS_CDR] = NIL;
  UNLOCK_TABLE(Ob_Array);
  Free += 2;
  return;
}


Pointer 
string_to_symbol (String)
     Pointer String;
{
  Pointer New_Symbol, Interned_Symbol, *Orig_Free;

  String_To_Symbol_Hook(String, string_length(String));
  /* may do a RETURN if String is "NIL" */

  Orig_Free = Free;
  New_Symbol = Make_Pointer(TC_UNINTERNED_SYMBOL, Free);
  Free[SYMBOL_NAME] = String;
  Free[SYMBOL_GLOBAL_VALUE] = UNBOUND_OBJECT;
  Free += 2;
  Interned_Symbol = New_Symbol;

  /* The work is done by Intern which returns in Interned_Symbol
     either the same symbol we gave it (in which case we need to check
     for GC) or an existing symbol (in which case we have to release
     the heap space acquired to hold New_Symbol).
  */

  Intern(&Interned_Symbol);
  if (Address(Interned_Symbol) == Address(New_Symbol))
  {
    Primitive_GC_If_Needed(0);	
  }
  else
    Free = Orig_Free;
  return Interned_Symbol;
}




/* Returns symbol with pname STRING in hash table TABLE.  The reason
   hashed_value is passed in instead of calculated here is that the
   commonlisp packages will want to look in one table and then the
   other, but won't want to hash twice. */

Pointer
find_hashed_symbol(string, length, hashed_value, table)
     char *string;
     long length;
     long hashed_value;
     Pointer table;
{
  fast Pointer *Bucket;

  hashed_value %= Vector_Length(table);
  Bucket = Nth_Vector_Loc(table, hashed_value + 1);
  LOCK_TABLE(table);
  while (*Bucket != NIL) 
  {
    Pointer sym_string;
    char *c_sym_string;

    sym_string = cl_get_symbol_name(Vector_Ref(*Bucket, CONS_CAR));
    c_sym_string = Scheme_String_To_C_String(sym_string);
    if (c_string_equal_len(string,
			   length,
			   c_sym_string,
			   string_length(sym_string)))
    {
      Pointer value;

      value = Vector_Ref(*Bucket, CONS_CAR);
      UNLOCK_TABLE(table);
      return (value);
    }
    else
      Bucket = Nth_Vector_Loc(*Bucket, CONS_CDR);
  }
  UNLOCK_TABLE(table)
  return (NIL);
}

Pointer
find_symbol_in_system_obarray(string)
     char *string;
{
  fast long length;
  fast char *p;
  Pointer get_system_obarray();
  long hashed_value;

  p = string;
  length = 0;
  while (*p++ != '\0')
    length++;
  hashed_value = Do_Hash(string, length);
  return (find_hashed_symbol(string, length, hashed_value,
			     get_system_obarray()));
}

Pointer
remove_symbol_from_ht(string, table)
     Pointer table, string;
{
  long Hashed_Value;
  Pointer Temp;
  fast Pointer *Bucket;

  Temp = Hash(string);
  Hashed_Value = Get_Integer(Temp);
  Hashed_Value %= Vector_Length(table);
  Bucket = Nth_Vector_Loc(table, Hashed_Value + 1);
  LOCK_TABLE(table);
  while (*Bucket != NIL)
  {
    Pointer thecar;

    thecar = Vector_Ref(*Bucket, CONS_CAR);
    if (string_equal(string, cl_get_symbol_name(thecar)))
    {
      /* unlink it */

      *Bucket = Vector_Ref(*Bucket, CONS_CDR);
      UNLOCK_TABLE(table);
      return (TRUTH);
    }
    Bucket = Nth_Vector_Loc(*Bucket, CONS_CDR);
  }
  /* symbol not found */
  UNLOCK_TABLE(table);
  return (NIL);
}


/* For debugging, given a String, return either a "not interned"
 * message or the address of the symbol and its global value.
 */

void 
Find_Symbol(Scheme_String)
     Pointer	Scheme_String;
{
  Pointer the_symbol;

  the_symbol = find_hashed_symbol(Scheme_String_To_C_String(Scheme_String),
				  string_length(Scheme_String),
				  Get_Integer(Hash(Scheme_String)),
				  get_system_obarray());
  if (the_symbol != NIL)
  {
    printf("\nInterned Symbol: 0x%x", the_symbol);
    Print_Expression(Vector_Ref(the_symbol, SYMBOL_GLOBAL_VALUE), "Value");
    printf("\n");
  }
  else
    printf("\nNot interned.\n");
}


/* (STRING->SYMBOL STRING)
   Similar to INTERN-CHARACTER-LIST, except this one takes a string
   instead of a list of ascii values as argument.  */

DEFINE_PRIMITIVE ("STRING->SYMBOL", Prim_String_To_Symbol, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (string_to_symbol (ARG_REF (1)));
}

/* (INTERN-CHARACTER-LIST LIST)
   LIST should consist of the ASCII codes for characters.  Returns
   a new (interned) symbol made out of these characters.  Notice
   that this is a fairly low-level primitive, and no checking is
   done on the characters except that they are in the range 0 to
   255.  Thus non-printing, lower-case, and special characters can
   be put into symbols this way.  */

DEFINE_PRIMITIVE ("INTERN-CHARACTER-LIST", Prim_Intern_Character_List, 1)
{
  extern Pointer list_to_string();
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (string_to_symbol (list_to_string (ARG_REF (1))));
}

/* (STRING-HASH STRING)
   Return a hash value for a string.  This uses the hashing
   algorithm used for interning symbols.  It is intended for use by
   the reader in creating interned symbols.  */

DEFINE_PRIMITIVE ("STRING-HASH", Prim_String_Hash, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (Hash (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("STRING-HASH-MOD", Prim_string_hash_mod, 2)
{
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     ((scheme_string_hash (ARG_REF (1))) %
      (arg_nonnegative_integer (2))));
}

/* (CHARACTER-LIST-HASH LIST)
   Takes a list of ASCII codes for characters and returns a hash
   code for them.  This uses the hashing function used to intern
   symbols in Fasload, and is really intended only for that
   purpose.  */

DEFINE_PRIMITIVE ("CHARACTER-LIST-HASH", Prim_Character_List_Hash, 1)
{
  fast Pointer char_list;
  long Length;
  Pointer This_Char;
  char String[MAX_HASH_CHARS];
  PRIMITIVE_HEADER (1);

  char_list = (ARG_REF (1));
  Touch_In_Primitive (char_list, char_list);
  for (Length = 0; (PAIR_P (char_list)); Length++)
    {
      if (Length < MAX_HASH_CHARS)
	{
	  Touch_In_Primitive
	    ((Vector_Ref (char_list, CONS_CAR)), This_Char);
	  if (! (CHARACTER_P (This_Char)))
	    error_wrong_type_arg (1);
	  Range_Check((String [Length]), This_Char,
		      '\0', ((char) MAX_CHAR),
		      ERR_ARG_1_WRONG_TYPE);
	  Touch_In_Primitive
	    ((Vector_Ref (char_list, CONS_CDR)), char_list);
	}
    }
  if (char_list != NIL)
    error_wrong_type_arg (1);
  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM (Do_Hash (String, Length)));
}
