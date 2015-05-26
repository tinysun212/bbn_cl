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

#include "scheme.h"
#include "primitive.h"
#include "stringprim.h"
#include "trap.h"
#include "cl-symbol.h"

extern void
  add_symbol_to_ht();

extern Pointer
  find_hashed_symbol(),
  remove_symbol_from_ht();

#define private	static

/* This contains code for CommonLisp packages and INTERN into these
   packages.  As little code as possible will be written here - just
   primitives to access the various parts that make up the packages,
   and INTERN (and friends), of course.  The structure is basically
   the same as the SpiceLisp organization. */

#define PKG_LENGTH		0	/* don't mess with the order of these */
#define PKG_NAME		1	/* string name of package */
#define PKG_NICKNAMES		2	/* list of nickname strings */
#define PKG_USE_LIST		3	/* list of packages we use */
#define PKG_USED_BY_LIST	4	/* list of packages that use this package */
#define PKG_SHADOWING_SYMBOLS	5	/* list of shadowing symbols */
#define PKG_TABLES		6	/* list of all the hashtables for inherited symbols */
#define PKG_INTERNAL_SYMBOLS	7	/* hashtable of internal symbols */
#define PKG_EXTERNAL_SYMBOLS	8	/* hashtable of external symbols */
#define PKG_HEADER_SIZE		9

#define pkg_name(p)		(Vector_Ref(p, PKG_NAME))
#define pkg_internals(p)	(Vector_Ref(p, PKG_INTERNAL_SYMBOLS))
#define pkg_externals(p)	(Vector_Ref(p, PKG_EXTERNAL_SYMBOLS))

extern Pointer	make_vector();

private Pointer current_pkg; /* *package*, set by calling set_current_pkg from binary-fasload-fd */

void
set_current_pkg (pkg)
Pointer pkg;
{
  current_pkg = pkg;
}

Pointer get_current_pkg()
{
  return current_pkg;
}


/* This silly procedure depends on the #defines at the top of the
   file.  It allocates a CommonLisp package and puts the name in
   the first usable slot. */

Pointer
Make_CL_Package(name)
Pointer	name;
{
	Pointer	pkg;
	register int	n;

	Primitive_GC_If_Needed(PKG_HEADER_SIZE);
	pkg = Make_Pointer(TC_CL_PACKAGE, Free);
	*Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, PKG_HEADER_SIZE - 1);
	for (n = PKG_HEADER_SIZE; --n >= 0; )
		*Free++ = NIL;
	Vector_Set(pkg, PKG_NAME, name);

	return pkg;
}

Define_Primitive(prim_pkg_add_symbol_to_ht, 2, "PKG-ADD-SYMBOL-TO-HASH-TABLE")
{
	Pointer	symbol;
	long	t;
	Primitive_2_Args();

	Arg_1_Type(TC_VECTOR);
	if (((t = Type_Code(Arg2)) != TC_UNINTERNED_SYMBOL) &&
	    (t != TC_INTERNED_SYMBOL))
		Arg_2_Type(TC_INTERNED_SYMBOL);
	symbol = Arg2;
	add_symbol_to_ht(&symbol, Arg1);
	return symbol;
}

Boolean
is_NIL_string(s, length)
Pointer	s;
{
	if ((length == 3) &&
	    (strncmp("NIL", Scheme_String_To_C_String(s), 3) == 0))
		return true;
	return false;
}

Boolean
is_T_string(s, length)
Pointer	s;
{
	if ((length == 1) &&
	    (strncmp("T", Scheme_String_To_C_String(s), 1) == 0))
		return true;
	return false;
}

private Pointer
pkg_find_symbol(pkg, string, length, where)
     Pointer pkg, string, length, *where;
{
  Pointer table, symbol;
  long hashed_value;
  long c_length;
  char *c_string;

  c_string = Scheme_String_To_C_String(string);
  c_length = Get_Integer(length);
  hashed_value = Do_Hash(c_string, c_length);
  if ((symbol = find_hashed_symbol(c_string, c_length, hashed_value, pkg_internals(pkg))) != NIL)
    *where = Sym_colon_internal;
  else if ((symbol = find_hashed_symbol(c_string, c_length, hashed_value, pkg_externals(pkg))) != NIL)
    *where = Sym_colon_external;
  else 
  {
    Pointer head;

    head = Vector_Ref(Vector_Ref(pkg, PKG_TABLES), CONS_CDR);
    while (head != NIL) 
    {
      Pointer table;

      table = Vector_Ref(head, CONS_CAR);
      if ((symbol = find_hashed_symbol(c_string, c_length,
				       hashed_value, table)) != NIL)
      {
	*where = Sym_colon_inherited;
	break;
      }
      head = Vector_Ref(head, CONS_CDR);
    }
    if (head == NIL) 
    {
      *where = NIL;
      return NIL;
    }
  }
  if (CL_Mode())
  {
    if (symbol == Get_Fixed_Obj_Slot(CL_NIL_Symbol))
      return NIL;
    if (symbol == Sym_t)
      return TRUTH;
  }
  return symbol;
}


Define_Primitive(prim_pkg_find_symbol, 3, "PKG-FIND-SYMBOL")
{
  Pointer name, length, pkg, symbol, where;
  Primitive_3_Args();
  
  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_CL_PACKAGE);

  name = Arg1;
  length = Arg2;
  pkg = Arg3;

  symbol = pkg_find_symbol(pkg, name, length, &where);
  Multiple_Value_Return(3, 2, symbol, { Push(where);
					Push(symbol);
				      });
}

/* called from the microcode fasloader */

pkg_intern_symbol(symbol, pkg)
Pointer	*symbol, pkg;
{
  Pointer found_symbol,
  symbol_name;
  long where;

  symbol_name = cl_get_symbol_name(*symbol);
  found_symbol = pkg_find_symbol(pkg, symbol_name,
				 Vector_Ref (symbol_name, STRING_LENGTH),
				 &where);
  if (where != NIL)
    *symbol = found_symbol;
  else 
  {
    if (strcmp(Scheme_String_To_C_String(pkg_name(pkg)), "KEYWORD") == 0) 
    {
      /* here we add_symbol_to_ht BEFORE we set the symbol's
	 global value, since adding the symbol changes the
	 type from uninterned to interned, which is what the
	 global value should be, too */
  
      add_symbol_to_ht(symbol, pkg_externals(pkg));
      Vector_Set(*symbol, SYMBOL_GLOBAL_VALUE, *symbol);
    } 
    else
      add_symbol_to_ht(symbol, pkg_internals(pkg));
  }
  New_Symbol_Hook(*symbol);
}

private Pointer
expanded_symbol_p(symbol)
     Pointer	symbol;
{
  if (Type_Code(Vector_Ref(symbol, SYMBOL_NAME)) == TC_CLSAV)
    return (TRUTH);
  else
    return (NIL);
}

/* called from the CommonLisp reader */

Pointer
pkg_intern_string(name, length, pkg, where)
     Pointer	name, length, pkg, *where;
{
  Pointer symbol, Interned_Symbol, *Orig_Free, result;

  symbol = pkg_find_symbol(pkg, name, length, where);
  if (*where == NIL)
  {
      Pointer	string;

      string = string_substring(name, FIXNUM_ZERO, length);

      Primitive_GC_If_Needed(2);
      symbol = Make_Pointer(TC_INTERNED_SYMBOL, Free);
      Free[SYMBOL_NAME] = string;
      Free[SYMBOL_GLOBAL_VALUE] = UNBOUND_OBJECT;
      Free += 2;

      /* kludge for KEYWORD package */
      if (strcmp(Scheme_String_To_C_String(pkg_name(pkg)), "KEYWORD") == 0)
	{
	  add_symbol_to_ht(&symbol, pkg_externals(pkg));
	  Vector_Set(symbol, SYMBOL_GLOBAL_VALUE, symbol);
	  *where = Sym_colon_external;
	} 
      else
	{
	  add_symbol_to_ht(&symbol, pkg_internals(pkg));
	  *where = Sym_colon_internal;
	}
      if (!expanded_symbol_p(symbol))
	{			/* should never be expanded */
	  add_clsav(symbol);
	  User_Vector_Set(Vector_Ref(symbol, SYMBOL_NAME),
			  CLSAV_PACKAGE_CELL,
			  pkg);
	} 
      else
	printf("\r\nYikes!  New symbol is born expanded!!  Everyone duck!");
    }
  New_Symbol_Hook(symbol);
  return symbol;
}

Define_Primitive(prim_pkg_intern_string, 3, "PKG-INTERN-STRING")
{
        Pointer result, where;
	Primitive_3_Args();

	Arg_1_Type(TC_CHARACTER_STRING);
	Arg_2_Type(TC_FIXNUM);
	Arg_3_Type(TC_CL_PACKAGE);

	result = pkg_intern_string(Arg1, Arg2, Arg3, &where);
	Multiple_Value_Return(3, 2, result, { Push(where);
					      Push(result);
					    });
}

Define_Primitive(prim_get_symbol, 3, "PKG-GET-SYMBOL-FROM-HASH-TABLE")
{
  Pointer table, string, length, hashed_value;
  char *c_string;
  long c_length;
  Primitive_3_Args();

  Arg_1_Type(TC_VECTOR);
  Arg_2_Type(TC_CHARACTER_STRING);
  Arg_3_Type(TC_FIXNUM);
  table = Arg1;
  string = Arg2;
  length = Arg3;

  c_string = Scheme_String_To_C_String(string);
  c_length =  Get_Integer(length);
  hashed_value = Do_Hash(c_string, c_length);
  return (find_hashed_symbol(c_string, c_length, hashed_value, table));
}

Define_Primitive(prim_rm_symbol_from_ht, 2, "PKG-REMOVE-SYMBOL-FROM-HASH-TABLE")
{
  Primitive_2_Args();

  Arg_1_Type(TC_VECTOR);
  Arg_2_Type(TC_CHARACTER_STRING);
  return (remove_symbol_from_ht(Arg2, Arg1));
}

private Pointer
make_ob_array()
{
	return make_vector(OBARRAY_SIZE, NIL);
}

Define_Primitive(Prim_Make_OBArray, 0, "MAKE-OBARRAY")
{
	Primitive_0_Args();

	return make_ob_array();
}


/* Code to maintain the hash table of packages. */

#define pkg_get_package_names()	Get_Fixed_Obj_Slot(CL_SystemPackage)

private Pointer
pkg_lookup(name, table)
Pointer	name, table;
{
	Pointer	bp;
	long	hashvalue = scheme_string_hash(name) % Vector_Length(table);

	for (bp = User_Vector_Ref(table, hashvalue); bp != NIL; bp = Vector_Ref(bp, CONS_CDR)) {
		Pointer	pkg_cons = Vector_Ref(bp, CONS_CAR);

		if (string_equal(Vector_Ref(pkg_cons, CONS_CAR), name))
			return Vector_Ref(pkg_cons, CONS_CDR);
	}
	return NIL;
}

Pointer
pkg_add_name(name, table, value)
Pointer	name, table, value;
{
	Pointer	bp,
		new_pair;
	long	hashvalue = scheme_string_hash(name) % Vector_Length(table);

	for (bp = User_Vector_Ref(table, hashvalue); bp != NIL; bp = Vector_Ref(bp, CONS_CDR)) {
		Pointer	pkg_cons = Vector_Ref(bp, CONS_CAR);

		/* if name already exists, reset its value */
		if (string_equal(Vector_Ref(pkg_cons, CONS_CAR), name))
			return Vector_Set(pkg_cons, CONS_CDR, value);
	}
	Primitive_GC_If_Needed(4);		/* two pairs */
	new_pair = Make_Pointer(TC_LIST, Free);
	*Free++ = name;
	*Free++ = value;
	bp = Make_Pointer(TC_LIST, Free);
	*Free++ = new_pair;
	*Free++ = User_Vector_Ref(table, hashvalue);
	User_Vector_Set(table, hashvalue, bp);
	return value;
}

Pointer
pkg_remove_name(name, table)
Pointer	name, table;
{
	Pointer	*bp;
	long	hashvalue = scheme_string_hash(name) % Vector_Length(table);

	for (bp = Nth_Vector_Loc(table, hashvalue + 1); *bp != NIL; *bp = Vector_Ref(*bp, CONS_CDR)) {
		Pointer	pkg_cons = Vector_Ref(*bp, CONS_CAR);

		if (string_equal(Vector_Ref(pkg_cons, CONS_CAR), name)) {
			*bp = Vector_Ref(*bp, CONS_CDR);
			return TRUTH;
		}
	}
	return NIL;
}

/* Packages are stored in the package hash table, which is stored
   in the Fixed_Objects vector. */

Define_Primitive(prim_pkg_lookup_name, 2, "PKG-LOOKUP-NAME")
{
	Primitive_2_Args();

	Arg_1_Type(TC_CHARACTER_STRING);
	Arg_2_Type(TC_VECTOR);
	return pkg_lookup(Arg1, Arg2);
}

Define_Primitive(prim_pkg_add_name, 3, "PKG-ADD-NAME")
{
	Primitive_3_Args();

	Arg_1_Type(TC_CHARACTER_STRING);
	Arg_2_Type(TC_VECTOR);
	Arg_3_Type(TC_CL_PACKAGE);
	return pkg_add_name(Arg1, Arg2, Arg3);
}

Define_Primitive(prim_pkg_remove_name, 2, "PKG-REMOVE-NAME")
{
	Primitive_2_Args();

	Arg_1_Type(TC_CHARACTER_STRING);
	Arg_2_Type(TC_VECTOR);
	return pkg_remove_name(Arg1, Arg2);
}

Define_Primitive(Prim_Make_CL_Package, 1, "MAKE-CL-PACKAGE")
{
	Pointer	pkg;
	Primitive_1_Arg();

	Arg_1_Type(TC_CHARACTER_STRING);
	pkg = Make_CL_Package(Arg1);

	return pkg;
}

/*
  This routine takes into account the "current" package,
  as well as whether the current package is valid. If not valid,
  the LISP (scheme obarray) package is desired, but since we don't have a handle
  on it, TRUTH is returned instead. The interners know that a TRUTH package
  means the scheme obarray.

  This hack can be fixed by installing the LISP package in the fixed-obj-vec
  right from the start, and always shunting everything through the package
  system.
*/

Pointer
pkg_get_pkg_by_name(name)
Pointer	name;
{
  if (name == TRUTH)
    if (current_pkg == NIL) return TRUTH;
    else return current_pkg;
  return pkg_lookup(name, pkg_get_package_names());
}

Pointer
pkg_get_name_by_pkg(pkg)
Pointer	pkg;
{
	return Vector_Ref(pkg, PKG_NAME);
}
