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

/*
   The "fast loader" which reads in and relocates binary files and then
   interns symbols.  It is called with one argument: the (character
   string) name of a file to load.  It is called as a primitive, and
   returns a single object read in.
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "trap.h"
#include "cl-symbol.h"

#ifdef butterfly
#include "transact.h"

extern long *Code_Copy_Area;
#endif /* butterfly */

#include "load.c"

#define CCheck_or_Reloc_Debug Or2(Consistency_Check, Reloc_Debug)

Pointer binary_fasload_fd();

extern int Debug_Flags[];

/*
  This flag and its accessor are purely for debugging 
*/

static Boolean CL_FASLOAD_CHECK = false;

Define_Primitive(prim_set_cl_fasload_check, 1, "%SET-CL-FASLOAD-CHECK!")
{
  Pointer old;
  Primitive_1_Args();

  old = ((CL_FASLOAD_CHECK) ? TRUTH : NIL);
  if (Arg1 != NIL) CL_FASLOAD_CHECK = true;
  else CL_FASLOAD_CHECK = false;
  return old;
}

long
read_file_start()
{
  long value, heap_length;

  if (Per_File)
  {
    Handle_Debug_Flags();
  }
  value = Read_Header();
  if (value != FASL_FILE_FINE)
  {
    Close_Dump_File();

    if (Debug_Flags[0])
    {
      printf("Error %d reading header\n", value);
      fflush(stdout);
    }

    switch (value)
    {
      /* These may want to be separated further. */
      case FASL_FILE_TOO_SHORT:
      case FASL_FILE_NOT_FASL:
      case FASL_FILE_BAD_MACHINE:
      case FASL_FILE_BAD_VERSION:
      case FASL_FILE_BAD_SUBVERSION:
        return (ERR_FASL_FILE_BAD_DATA);

      case FASL_FILE_BAD_PROCESSOR:
      case FASL_FILE_BAD_INTERFACE:
	return (ERR_FASLOAD_COMPILED_MISMATCH);
    }
  }  

  if (Or2(Reloc_Debug, File_Load_Debug))
  {
    print_fasl_information();
  }

  if (!Test_Pure_Space_Top(Free_Constant + Const_Count))
  {
    if (Debug_Flags[0])
    {
      printf("\nFree_Constant = 0x%x Const_Count = 0d%d ",
	     Free_Constant, Const_Count);
      printf("FC+CC = 0x%x Constant_Top = 0x%x\n",
	     (Free_Constant + Const_Count), Constant_Top);
    }
    Close_Dump_File();
    return (ERR_FASL_FILE_TOO_BIG);
  }
  
  heap_length = Heap_Count + Primitive_Table_Size + Primitive_Table_Length;
  
  if (GC_Check(heap_length))
  {
    unread_dump_file_header();
    Request_GC(heap_length);
    return (PRIM_INTERRUPT);
  }

#ifdef butterfly
  if (Code_Space_Check(Code_Count))
  {
    if (Debug_Flags[0])
      printf("\nCode_Free = 0x%x Code_Count = 0d%d CF+CC = 0x%x Code_Top = 0x%x\n",
	     SHARED_DATA->Code_Free, Code_Count,
	     (SHARED_DATA->Code_Free + Code_Count), 
	     SHARED_DATA->Code_Top);
    return (ERR_FASL_FILE_TOO_BIG);
  }
#endif

  return (PRIM_DONE);
}

Pointer *
read_file_end(status, use_code_copy_area)
     long *status;
     Boolean use_code_copy_area;
{
  Pointer *table;
  long count;

  *status = PRIM_DONE;

  if (Debug_Flags[0])
  {
    printf("Loading 0x%x bytes of heap at 0x%x ...",
	   (Heap_Count * sizeof(Pointer)), Free);
    fflush(stdout);
  }
  count = Load_Data(Heap_Count, ((char *) Free));
  if (count != Heap_Count)
  {
    if (Debug_Flags[0])
      {
	printf("Heap space data size incorrect.\n");
	printf("Expected %d (0x%x) pointers, got %d (0x%x) pointers\n",
	       Heap_Count, Heap_Count, count, count);
	fflush(stdout);
      }
    *status = ERR_EXTERNAL_RETURN;
    return (NIL);
  }
  NORMALIZE_REGION(((char *) Free), Heap_Count);
  Free += Heap_Count;

  if (Debug_Flags[0])
  {
    printf(" done, new heap is at 0x%x\n", Free);
    printf("Loading 0x%x bytes of constant at 0x%x ...",
	   (Const_Count * sizeof(Pointer)), Free_Constant);
    fflush(stdout);
  }
  count = Load_Data(Const_Count, ((char *) Free_Constant));
  if (count != Const_Count)
  {
    if (Debug_Flags[0])
      {
	printf("Constant space data size incorrect.\n");
	printf("Expected %d (0x%x) pointers, got %d (0x%x) pointers\n",
	       Const_Count, Const_Count, count, count);
	fflush(stdout);
      }
    *status = ERR_EXTERNAL_RETURN;
    return (NIL);
  }
  NORMALIZE_REGION(((char *) Free_Constant), Const_Count);
  Free_Constant += Const_Count;

  if (Debug_Flags[0])
  {
    printf(" done, new constant is at 0x%x\n", Free_Constant);
    printf("Loading 0x%x bytes of code at 0x%x ...",
	   (Code_Count * sizeof(Pointer)), Code_Space_Free());
    fflush(stdout);
  }
#ifdef butterfly
  if (use_code_copy_area)
    count = Load_Data(Code_Count, (char *) Code_Copy_Area);
  else
#endif
    count = Load_Data(Code_Count, ((char *) Code_Space_Free()));
  if (count != Code_Count)
  {
    if (Debug_Flags[0])
      {
	printf("Code space data size incorrect.\n");
	printf("Expected %d (0x%x) pointers, got %d (0x%x) pointers\n",
	       Code_Count, Code_Count, count, count);
	fflush(stdout);
      }
    *status = ERR_EXTERNAL_RETURN;
    return (NIL);
  }
  Code_Space_Set(Code_Space_Free() + Code_Count);
  if (Debug_Flags[0])
  {
    printf(" done, new code is at 0x%x\n", Code_Space_Free());
    fflush(stdout);
  }

  table = Free;
  count = Load_Data(Primitive_Table_Size, ((char *) Free));
  if (count != Primitive_Table_Size)
  {
    if (Debug_Flags[0])
      {
	printf("Primitive table data size incorrect.\n");
	printf("Expected %d (0x%x) pointers, got %d (0x%x) pointers\n",
	       Primitive_Table_Size, Primitive_Table_Size, count, count);
	fflush(stdout);
      }
    *status = ERR_EXTERNAL_RETURN;
    return (NIL);
  }
  NORMALIZE_REGION(((char *) table), Primitive_Table_Size);
  Free += Primitive_Table_Size;
  return (table);
}

/* Statics used by Relocate, below */

relocation_type
  heap_relocation,
  const_relocation,
  stack_relocation;

relocation_type code_relocation;

/* Relocate a pointer as read in from the file.  If the pointer used
   to point into the heap, relocate it into the heap.  If it used to
   be constant area, relocate it to constant area.  Otherwise give an
   error.
*/

#ifdef ENABLE_DEBUGGING_TOOLS

static Boolean Warned = false;

Pointer *
Relocate(P)
     long P;
{
  Pointer *Result;

  if ((P >= Heap_Base) && (P < Dumped_Heap_Top))
  {
    Result = ((Pointer *) (P + heap_relocation));
  }
  else if ((P >= Code_Base) && (P < Dumped_Code_Top))
    Result = (Pointer *) (P + code_relocation);
  else if ((P >= Const_Base) && (P < Dumped_Constant_Top))
  {
    Result = ((Pointer *) (P + const_relocation));
  }
  else if ((P >= Dumped_Constant_Top) && (P < Dumped_Stack_Top))
  {
    Result = ((Pointer *) (P + stack_relocation));
  }
  else
  {
    printf("Pointer out of range: 0x%x\n", P, P);
    if (!Warned)
    {
      printf("Heap: 0x%x-0x%x, Constant: 0x%x-0x%x, Stack: ?-0x%x\n",
             Heap_Base, Dumped_Heap_Top,
             Const_Base, Dumped_Constant_Top, Dumped_Stack_Top);
      Warned = true;
    }
    Result = ((Pointer *) 0);
  }
  if (Reloc_Debug)
  {
    printf("0x%06x -> 0x%06x\n", P, Result);
  }
  return (Result);
}

#define Relocate_Into(Loc, P) (Loc) = Relocate(P)

#else /* not ENABLE_DEBUGGING_TOOLS */

#define Relocate_Into(Loc, P)						\
{									\
  if ((P) < Dumped_Code_Top)						\
  {									\
    if ((P) < Code_Base)						\
      (Loc) = ((Pointer *) ((P) + heap_relocation));			\
    else								\
      (Loc) = ((Pointer *) ((P) + code_relocation));			\
  }									\
  else									\
  {									\
    if ((P) < Dumped_Constant_Top)					\
      (Loc) = ((Pointer *) ((P) + const_relocation));			\
    else								\
      (Loc) = ((Pointer *) ((P) + stack_relocation));			\
  }									\
}

#define Relocate(P)							\
  ((P < Dumped_Code_Top) ?						\
   ((P < Code_Base) ?							\
    ((Pointer *) (P + heap_relocation)) :				\
    ((Pointer *) (P + code_relocation))) :				\
   ((P < Dumped_Constant_Top) ?						\
    ((Pointer *) (P + const_relocation)) :				\
    ((Pointer *) (P + stack_relocation))))

#endif /* ENABLE_DEBUGGING_TOOLS */

/* Next_Pointer starts by pointing to the beginning of the block of
   memory to be handled.  This loop relocates all pointers in the
   block of memory.

   This procedure shadows the global parameters of the same name
   to make sure that the macros above work both inside and outside.
*/

void
Relocate_Block_Internal(Scan, Stop_At,
			heap_relocation, const_relocation,
			stack_relocation, code_relocation,
			Code_Base, Dumped_Code_Top, Dumped_Constant_Top,
			load_renumber_table)
     fast Pointer *Scan, *Stop_At;
     fast long Code_Base, Dumped_Code_Top, Dumped_Constant_Top;
     fast relocation_type heap_relocation, const_relocation, code_relocation;
     relocation_type stack_relocation;
     fast Pointer *load_renumber_table;
{
  fast Pointer Temp;
  fast long address;

  if (Reloc_Debug)
  {
    fprintf(stderr,
	    "Relocation beginning, block = 0x%x, length = 0x%x, end = 0x%x.\n",
	    Scan, (Stop_At - Scan) - 1, Stop_At);
  }
  while (Scan < Stop_At)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case_Fasload_Non_Pointer:
        Scan += 1;
	break;
	
      case TC_PRIMITIVE:
	*Scan++ = load_renumber_table[PRIMITIVE_NUMBER(Temp)];
	break;
	
      case TC_PCOMB0:
	*Scan++ =
	  Make_Non_Pointer(TC_PCOMB0,
			   load_renumber_table[PRIMITIVE_NUMBER(Temp)]);
        break;

      case TC_MANIFEST_NM_VECTOR:
        Scan += (Get_Integer(Temp) + 1);
        break;

      case TC_LINKAGE_SECTION:
      {
	if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	{
	  /* Assumes that all others are objects of type TC_QUAD without
	     their type codes.
	   */

	  fast long count;

	  Scan++;
	  for (count = READ_CACHE_LINKAGE_COUNT(Temp);
	       --count >= 0;
	       )
	  {
	    address = ((long) *Scan);
	    *Scan++ = ((Pointer) Relocate(address));
	  }
	  break;
	}
	else
	{
	  fast long count;
	  fast machine_word *word_ptr;
	  Pointer *end_scan;

	  count = READ_OPERATOR_LINKAGE_COUNT(Temp);
	  word_ptr = FIRST_OPERATOR_LINKAGE_ENTRY(Scan);
	  end_scan = END_OPERATOR_LINKAGE_AREA(Scan, count);

	  while(--count >= 0)
	  {
	    Scan = OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr);
	    word_ptr = NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr);
	    address = ((long) *Scan);
	    *Scan = ((Pointer) Relocate(address));
	  }
	  Scan = &end_scan[1];
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	machine_word *start_ptr;
	fast machine_word *word_ptr;

	Scan += 1;
	word_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Scan);
	start_ptr = word_ptr;

	while (VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  Scan = MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr);
	  word_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	  address = ((long) *Scan);
	  *Scan = ((Pointer) Relocate(address));
	}
	Scan = &((MANIFEST_CLOSURE_END(word_ptr, start_ptr))[1]);
	break;
      }

#ifdef BYTE_INVERSION
      case TC_CHARACTER_STRING:
	String_Inversion(Relocate(OBJECT_DATUM(Temp)));
	goto normal_pointer;
#endif

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  Scan += 1;
	  break;
	}

	/* It is a pointer, fall through. */

      	/* Compiled entry points and stack environments work automagically. */
	/* This should be more strict. */

      default:
      normal_pointer:
	address = OBJECT_DATUM(Temp);
	*Scan++ = Make_Pointer(OBJECT_TYPE(Temp), Relocate(address));
	break;
      }
  }
  return;
}

void
Relocate_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  extern Pointer *load_renumber_table;

  Relocate_Block_Internal(Next_Pointer, Stop_At,
			  heap_relocation, const_relocation,
			  stack_relocation, code_relocation,
			  Code_Base, Dumped_Code_Top, Dumped_Constant_Top,
			  load_renumber_table);
  return;    
}

#ifdef butterfly

extern void relocate_area();

void
relocate_area(rid)
     fast struct relocate_interrupt_data *rid;
{
    if (Debug_Flags[0])
    {
      printf("(on %d/%d) relocate_area invoked.\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
      printf("from = 0x%x; to = 0x%x\n",
	     rid->from, rid->to);
      printf("heap_relocation = 0x%x; const_relocation = 0x%x\n",
	     rid->heap_relocation, rid->const_relocation);
      printf("code_relocation = 0x%x; stack_relocation = 0x%x\n",
	     rid->code_relocation, rid->stack_relocation);
      printf("Code_Base = 0x%x; Dumped_Code_Top = 0x%x\n",
	     rid->code_base, rid->code_top);
      printf("Dumped_Constant_Top = 0x%x; load_renumber_table = 0x%x\n",
	     rid->const_top, rid->load_renumber_table);
      fflush(stdout);
    }

  Relocate_Block_Internal(((Pointer *) rid->from),
			  ((Pointer *) rid->to),
			  ((relocation_type) rid->heap_relocation),
			  ((relocation_type) rid->const_relocation),
			  ((relocation_type) rid->stack_relocation),
			  ((relocation_type) rid->code_relocation),
			  ((long) rid->code_base),
			  ((long) rid->code_top),
			  ((long) rid->const_top),
			  ((Pointer *) rid->load_renumber_table));
  return;
}
#endif /* butterfly */

Boolean
check_primitive_numbers(table, length)
     fast Pointer *table;
     fast long length;
{
  fast long count, top;

  top = NUMBER_OF_DEFINED_PRIMITIVES();
  if (length < top)
    top = length;

  for (count = 0; count < top; count += 1)
  {
    if (table[count] != MAKE_PRIMITIVE_OBJECT(0, count))
      return (false);
  }
  /* Is this really correct?  Can't this screw up if there
     were more implemented primitives in the dumping microcode
     than in the loading microcode and they all fell after the
     last implemented primitive in the loading microcode?
   */
  if (length == top)
    return (true);
  for (count = top; count < length; count += 1)
  {
    if (table[count] != MAKE_PRIMITIVE_OBJECT(count, top))
      return (false);
  }
  return (true);
}

extern void get_band_parameters();

void
get_band_parameters(heap_size, const_size, code_size)
     long *heap_size, *const_size, *code_size;
{
  /* This assumes we have just aborted out of a band load. */

  *heap_size = Heap_Count;
  *const_size = Const_Count;
  *code_size = Code_Count;
  return;
}

static Pointer current_pkg;    /* *package*, as passed into binary-fasload-fd */

Repackage_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{ if (CL_FASLOAD_CHECK)
    fprintf(stderr,
	    "Repackaging beginning, block=0x%x, length=0x%x, end=0x%x.\n",
	    Next_Pointer, (Stop_At-Next_Pointer)-1, Stop_At);
  while (Next_Pointer < Stop_At)
  { fast Pointer Temp;

    Temp = *Next_Pointer;
    Switch_by_GC_Type(Temp)
    { case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(Temp)+1;
        break;

      case TC_CLSAV:
      { Pointer cl_pkg_name, cl_pkg;
	cl_pkg_name = User_Vector_Ref(*Next_Pointer, CLSAV_PACKAGE_CELL);
	if (cl_pkg_name == NIL)
	{ if (CL_FASLOAD_CHECK) printf("Repackaging NIL package.\n");
	  /* Do nothing to package cell */
	}
	else
	{ cl_pkg =  pkg_get_pkg_by_name(cl_pkg_name);
	  if (cl_pkg == NIL)
	  { printf("\nRepackage_Block Error: Package doesn't exist: %s\n",
		   Scheme_String_To_C_String(cl_pkg_name));
	    Primitive_Error(ERR_EXTERNAL_RETURN);
	  }	       
	  if (CL_FASLOAD_CHECK)
	    {
	      char *pkg_name;

	      if (cl_pkg_name == NIL)
		pkg_name = "()";
	      else if (cl_pkg_name  == TRUTH)
		pkg_name = "TRUTH";
	      else
		pkg_name = Scheme_String_To_C_String(cl_pkg_name);
	      printf("Repackaging package named %s.\n", pkg_name);
	    }
	  User_Vector_Set(*Next_Pointer, CLSAV_PACKAGE_CELL, cl_pkg);
	}
      }
      default:
        Next_Pointer += 1;
      }
  }
}

extern void Intern();

/* add_interned_symbol returns true if the symbol moved
   as a result of interning (i.e., the symbol already existed) */

Boolean
add_interned_symbol(Next_Pointer, cl_pkg)
Pointer *Next_Pointer, cl_pkg;
{ if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_GLOBAL_VALUE)) ==
      TC_BROKEN_HEART)
  { Pointer Old_Symbol;
    
    Old_Symbol = *Next_Pointer;
    Vector_Set(*Next_Pointer, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);
    if (cl_pkg == TRUTH) /* If TRUTH made it through repackage_block, then it means the Scheme obarray */
      {
	Intern(Next_Pointer);
	New_Symbol_Hook(*Next_Pointer);
      }
    else pkg_intern_symbol(Next_Pointer, cl_pkg);
    Primitive_GC_If_Needed(0);
    if (*Next_Pointer != Old_Symbol)
    { char  *Old_Name, *New_Name;

      New_Name = Scheme_String_To_C_String(cl_get_symbol_name(*Next_Pointer));
      Old_Name = Scheme_String_To_C_String(cl_get_symbol_name(Old_Symbol));
      if (strcmp(New_Name, Old_Name) != 0)
      { printf("\nadd_interned_symbol Error: %s got interned to %s\n",
	       Old_Name, New_Name);
	Microcode_Termination(TERM_EXIT);
      }
      /* Symbol existed, redirect to pre-existing symbol */
      Vector_Set(Old_Symbol, SYMBOL_NAME,
		 Make_New_Pointer(TC_BROKEN_HEART, *Next_Pointer));
      return true;
    }
    else
    { /* Symbol didn't exist */
      return false;
    }
  }
  else if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
	   TC_BROKEN_HEART)
  { *Next_Pointer =
      Make_New_Pointer(Type_Code(*Next_Pointer),
		       Fast_Vector_Ref(*Next_Pointer, SYMBOL_NAME));
    return true; /* Symbol got relocated */
  }
  else return true;
}

Boolean
Intern_Cl_Symbol(Parent)
Pointer *Parent;
{ Pointer Old_Parent, Parent_Clsav, pkg;
    
  Parent_Clsav = Vector_Ref(*Parent, SYMBOL_NAME);
  Old_Parent = *Parent;
  pkg = User_Vector_Ref(Parent_Clsav, CLSAV_PACKAGE_CELL);
  if (CL_FASLOAD_CHECK)
    {
      char *pkg_name;

      if (pkg == NIL) pkg_name = "NIL";
      else pkg_name = Scheme_String_To_C_String(pkg_get_name_by_pkg(pkg));
      printf("Intern_Cl_Symbol: name: %s package: %s\n",
	     Scheme_String_To_C_String(cl_get_symbol_name(*Parent)),
	     pkg_name);
    }
  if (pkg == NIL) 
  { if (CL_FASLOAD_CHECK)
      printf("The symbol %s is Common Lisp uninterned.\n",
	     Scheme_String_To_C_String(cl_get_symbol_name(*Parent)));
    /* Do nothing to Common Lisp uninterned symbols except set their values */
    Vector_Set(*Parent, SYMBOL_GLOBAL_VALUE, UNASSIGNED_OBJECT);
    return false;
  }
  else if (!add_interned_symbol(Parent, pkg))
  { /* Symbol got added */
    if (CL_FASLOAD_CHECK) printf("Added symbol to an obarray.\n");
    return false;
  }
  else if (*Parent == Old_Parent)
  { /* Parent has been previously added FROM THIS FILE */
    if (CL_FASLOAD_CHECK) printf("This parent was already added.\n");
    return false;
  }
  else
  { /* Symbol got moved. There are two cases:
       1: The pre-existing Parent symbol has a clsav.
       2: The pre-existing parent symbol does not have a clsav. */
    if (Type_Code(Vector_Ref(*Parent, SYMBOL_NAME)) == TC_CLSAV)
      {	Pointer Preexisting_Clsav;
	
	if (CL_FASLOAD_CHECK) printf("The existing symbol has a clsav.\n");
	Preexisting_Clsav = Vector_Ref(Old_Parent, SYMBOL_NAME);
	/*  Preexisting_Clsav may have been relocated, indirect accordingly */
	if (Type_Code(Preexisting_Clsav) == TC_BROKEN_HEART)
	{ if (CL_FASLOAD_CHECK) printf("Preexisting_Clsav is redirected.\n");
	  /* Redirect */
	  Preexisting_Clsav = Make_Pointer(TC_CLSAV,
					   *(Get_Pointer(Preexisting_Clsav)));
	}
	else if (CL_FASLOAD_CHECK) printf("Prexisting_Clsav not redirected.\n");
	Vector_Set(User_Vector_Ref(Parent_Clsav, CLSAV_FUNCTION_SYMBOL),
		   SYMBOL_NAME,
		   Make_New_Pointer(TC_BROKEN_HEART,
				    User_Vector_Ref(Preexisting_Clsav,
						    CLSAV_FUNCTION_SYMBOL)));
      }
    else 
      { if (CL_FASLOAD_CHECK) printf("The existing symbol has no clsav.\n");
	/* Add the clsav that came from the file */
	Vector_Set(*Parent, SYMBOL_NAME, Parent_Clsav);
      }
  }
  return true;
}

void
Intern_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  if (Reloc_Debug)
  {
    printf("Interning a block.\n");
  }

  while (Next_Pointer < Stop_At)
  {
    switch (Type_Code(*Next_Pointer))
    {
      case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += (1 + Get_Integer(*Next_Pointer));
        break;

      case TC_UNINTERNED_SYMBOL:
	if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
	    TC_BROKEN_HEART)
	{ /* Symbol has been relocated */
	  *Next_Pointer =
	    Make_New_Pointer(Type_Code(*Next_Pointer),
			     Fast_Vector_Ref(*Next_Pointer, SYMBOL_NAME));
	  if (CL_FASLOAD_CHECK)
	    printf("Found and relocated uninterned symbol %s\n",
		   Scheme_String_To_C_String(cl_get_symbol_name(*Next_Pointer)));
	}
	else if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
		 TC_INTERNED_SYMBOL)
	{ Pointer Parent = Vector_Ref(*Next_Pointer, SYMBOL_NAME);
	  /* The parent symbol may have been relocated at this point */
	  if (Type_Code(Vector_Ref(Parent, SYMBOL_NAME)) ==
	      TC_BROKEN_HEART)
	  { /* Parent has been relocated */
	    Pointer Parent_Clsav =
	      Make_Pointer(TC_CLSAV,
			   *Get_Pointer(Vector_Ref(Parent, SYMBOL_NAME)));
	    *Next_Pointer =
	      User_Vector_Ref(Parent_Clsav, CLSAV_FUNCTION_SYMBOL);
	    if (CL_FASLOAD_CHECK)
	      { printf("Found function symbol with relocated parent %s\n",
 	         Scheme_String_To_C_String(cl_get_symbol_name(Parent)));
		/* Previous interning of the parent did the rest, so done */
	      }
	  }
	  else
	  { if (CL_FASLOAD_CHECK)
	    { printf("Found function symbol with unrelocated parent %s\n",
		     Scheme_String_To_C_String(cl_get_symbol_name(*Next_Pointer)));
	    }
	    if (Type_Code(Vector_Ref(Parent, SYMBOL_NAME)) != TC_CLSAV)
	    { printf("\nIntern_Block Error: Parent of function symbol 0x%xis not a CLisp symbol\n",
		     *Next_Pointer);
	      if (Type_Code(Vector_Ref(Parent, SYMBOL_NAME)) ==
		  TC_CHARACTER_STRING)
		printf("Parent is a normal symbol with pname %s\n",
	           Scheme_String_To_C_String(Vector_Ref(Parent, SYMBOL_NAME)));
	      else
		printf("Parent has 0x%x in its pname slot.\n",
		       Vector_Ref(Parent, SYMBOL_NAME));
	      Microcode_Termination(TERM_EXIT);
	    }
	    if (!Intern_Cl_Symbol(&Parent))
	      { if (CL_FASLOAD_CHECK)
		  printf("Intern indicates parent %s didn't already exist.\n",
			 Scheme_String_To_C_String(cl_get_symbol_name(Parent)));
		/* Make new function cells come in unassigned */
		Vector_Set(*Next_Pointer, SYMBOL_GLOBAL_VALUE, 
			   UNASSIGNED_OBJECT);

	      }
	    else 
	    { if (CL_FASLOAD_CHECK)
		printf("Parent %s already existed, fowarding function symbol.\n",
		       Scheme_String_To_C_String(cl_get_symbol_name(Parent)));
	      /* And replace the function symbol with pre-existing symbol */
	      *Next_Pointer = 
		Make_New_Pointer(TC_UNINTERNED_SYMBOL,
				 User_Vector_Ref(Vector_Ref(Parent, SYMBOL_NAME),
						 CLSAV_FUNCTION_SYMBOL));
	    }
	  }
	}
	Next_Pointer += 1;
	break;

      case TC_INTERNED_SYMBOL:
	if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
	    TC_CLSAV)	
	{ /* This is a Common Lisp parent symbol */
	  if (CL_FASLOAD_CHECK)
	    printf("Found a CL parent symbol with name %s\n",
		 Scheme_String_To_C_String(cl_get_symbol_name(*Next_Pointer)));
	  Intern_Cl_Symbol(Next_Pointer);
	}
	else if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
	    TC_INTERNED_SYMBOL)
	{ printf("\nIntern_Block Error: Interned symbol has symbol pname: %s\n",
		 Scheme_String_To_C_String(cl_get_symbol_name(*Next_Pointer)));
	  Microcode_Termination(TERM_EXIT);
	}
	/* This is just a Scheme symbol */
	else add_interned_symbol(Next_Pointer, TRUTH);
	Next_Pointer += 1;
	break;

	/* Fall through to default case */
      
      default: Next_Pointer += 1;
    }
  }
  if (Reloc_Debug)
  {
    printf("Done interning block.\n");
  }
  return;
}

Pointer
load_file(from_band_load, status)
     Boolean from_band_load;
     long *status;
{
  Pointer
    *Orig_Heap,
    *Constant_End, *Orig_Constant,
    *temp, *primitive_table;
  Pointer *Orig_Code, *Code_End;
  Boolean need_relocation(), use_code_copy_area;

  extern void install_primitive_table();
  extern Pointer *load_renumber_table;

#ifdef butterfly
  use_code_copy_area = N_Interps > 1 && from_band_load;
#else
  use_code_copy_area = false;
#endif

  if (Debug_Flags[23] && use_code_copy_area)
    printf("**** Using code copy area ****\n");

  /* Read File */

#ifdef ENABLE_DEBUGGING_TOOLS
  Warned = false;
#endif

  Orig_Heap = Free;
  Orig_Constant = Free_Constant;
  Orig_Code = Code_Space_Free();
  primitive_table = read_file_end(status, use_code_copy_area);
  load_renumber_table = Free;
  Free += Primitive_Table_Length;
  if (*status != PRIM_DONE)
    return (NIL);
  Constant_End = Free_Constant;
  heap_relocation = ((relocation_type) Orig_Heap) - Heap_Base;
  const_relocation= ((relocation_type) Orig_Constant) - Const_Base;
  stack_relocation = ((relocation_type) Stack_Top) - Dumped_Stack_Top;
  Code_End = Code_Space_Free();
  code_relocation = ((relocation_type) Orig_Code) - Code_Base;

  /*
    Magic!
    The relocation of compiled code entry points depends on the fact
    that fasdump never dumps a constant section.

    If the file is not a band, any pointers into constant space are
    pointers into the compiler utilities vector.  const_relocation is
    computed appropriately.

    Otherwise (the file is a band, and only bands can contain constant
    space segments) the utilities vector stuff is relocated
    automagically: the utilities vector is part of the band.
   */

  if ((!band_p) && (dumped_utilities != NIL))
  {
    extern Pointer compiler_utilities;

    if (compiler_utilities == NIL)
    {
      Primitive_Error(ERR_FASLOAD_COMPILED_MISMATCH);
    }

    const_relocation = (((relocation_type) Get_Pointer(compiler_utilities)) -
			Datum(dumped_utilities));
    Dumped_Constant_Top =
      C_To_Scheme(Nth_Vector_Loc(dumped_utilities,
				 (1 + Vector_Length(compiler_utilities))));
  }
  else
  {
    const_relocation = (((relocation_type) Orig_Constant) - Const_Base);
  }
  stack_relocation = ((relocation_type) Stack_Top) - Dumped_Stack_Top;

#ifdef BYTE_INVERSION
  Setup_For_String_Inversion();
#endif
      
  /* Setup the primitive table */
  
  install_primitive_table(primitive_table,
			  Primitive_Table_Length,
			  from_band_load);

  if ((!from_band_load)					||
      (heap_relocation != ((relocation_type) 0))	||
      (const_relocation != ((relocation_type) 0))	||
      (stack_relocation != ((relocation_type) 0))	||
      (code_relocation != ((relocation_type) 0))	||
      (!check_primitive_numbers(load_renumber_table,
				Primitive_Table_Length)))
  {
    /* We need to relocate.  Oh well. */

    if (Debug_Flags[0])
    {
      printf("heap_relocation = 0x%x; const_relocation = 0x%x\n",
	     heap_relocation, const_relocation);
      printf("code_relocation = 0x%x; stack_relocation = 0x%x\n",
	     code_relocation, stack_relocation);
      printf("Code_Base = 0x%x; Dumped_Code_Top = 0x%x\n",
	     Code_Base, Dumped_Code_Top);
      printf("Dumped_Constant_Top = 0x%x; load_renumber_table = 0x%x\n",
	     Dumped_Constant_Top, load_renumber_table);
      fflush(stdout);
    }

    if (Debug_Flags[6])
    {
      printf("== starting relocation at %d\n", System_Clock());
      fflush(stdout);
    }

#if defined(butterfly) && !defined(BYTE_INVERSION)
    if (from_band_load && (N_Interps != 1))
    {
      /* Use other processors to help with relocation */

      int sucker;
      struct relocate_interrupt_data rid;

      /* This canont be locked by anyone else. */

      SHARED_DATA->GC_Propagate_Lock = ((N_Interps == 2) ? 1 : 2);

      rid.heap_relocation = ((long) heap_relocation);
      rid.const_relocation = ((long) const_relocation);
      rid.stack_relocation = ((long) stack_relocation);
      rid.code_relocation = ((long) code_relocation);
      rid.load_renumber_table = ((long *) load_renumber_table);
      rid.code_base = ((long *) Code_Base);
      rid.code_top = ((long *) Dumped_Code_Top);
      rid.const_top = ((long *) Dumped_Constant_Top);

      rid.from = ((long *) Orig_Constant);
      rid.to = ((long *) Free_Constant);
      sucker = ((Who_Am_I + 1) % N_Interps);
      if (Debug_Flags[0])
      {
	printf("Constant space (from 0x%x to 0x%x) relocated by processor %d.\n",
	       Orig_Constant, Free_Constant, sucker);
	fflush(stdout);
      }
      Send_Signal_Info(sucker, SIG_RELOCATE, &rid, sizeof(rid));

      if (N_Interps == 2)
      {
	Relocate_Block(Orig_Heap, primitive_table);
      }
      else
      {
	rid.from = ((long *) Orig_Heap);
	rid.to = ((long *) primitive_table);
	sucker = ((Who_Am_I + 2) % N_Interps);
	if (Debug_Flags[0])
	{
	  printf("Heap (from 0x%x to 0x%x) relocated by processor %d.\n",
		 Orig_Heap, primitive_table, sucker);
	  fflush(stdout);
	}
	Send_Signal_Info(sucker, SIG_RELOCATE, &rid, sizeof(rid));
      }
      /* Code space must be relocated locally since it is not
	 shared memory.
       */
      if (use_code_copy_area)
	Relocate_Block(Code_Copy_Area, Code_Copy_Area + Code_Count);
      else
	Relocate_Block(Orig_Code, Code_Space_Free());
      while (SHARED_DATA->GC_Propagate_Lock != 0)
	Standard_Delay();
    }
    else
#endif /* defined(butterfly) ... */

    {
      /*
	Relocate the new data.

	There are no pointers in the primitive table, thus
	there is no need to relocate it.
       */

      Relocate_Block(Orig_Heap, primitive_table);
      Relocate_Block(Orig_Constant, Free_Constant);
      Relocate_Block(Orig_Code, Code_Space_Free());
    }

#ifdef BYTE_INVERSION
    Finish_String_Inversion();
#endif

    if (!from_band_load)
    {
      Repackage_Block(Orig_Heap, primitive_table);
      Repackage_Block(Orig_Constant, Constant_End);
      Repackage_Block(Orig_Code, Code_Space_Free());

      /* Again, there are no symbols in the primitive table. */

      Intern_Block(Orig_Heap, primitive_table);
      Intern_Block(Orig_Constant, Constant_End);
      Intern_Block(Orig_Code, Code_Space_Free());
    }
  }
  else if (Debug_Flags[0])
  {
    printf("Avoiding relocation!  All parameters compatible with image.\n");
    fflush(stdout);
  }

  Set_Pure_Top();

  if (Debug_Flags[6])
    printf("== starting propagating at %d\n", System_Clock());

  if (use_code_copy_area)
  {
    FASLOAD_RELOCATE_HOOK(from_band_load,
			  Orig_Heap, primitive_table,
			  Orig_Constant, Free_Constant,
			  Orig_Code, Orig_Code);
  }
  else
  {
    FASLOAD_RELOCATE_HOOK(from_band_load,
			  Orig_Heap, primitive_table,
			  Orig_Constant, Free_Constant,
			  Orig_Code, Code_Space_Free());
  }

  Relocate_Into(temp, Dumped_Object);
  if (Debug_Flags[0])
  {
    printf("Dumped Object was 0x%x, now is 0x%x\n", Dumped_Object, *temp);
    fflush(stdout);
  }
  return (*temp);
}

/* (BINARY-FASLOAD FILE-NAME)
   Load the contents of FILE-NAME into memory.  The file was
   presumably made by a call to PRIMITIVE-FASDUMP, and may contain
   data for the heap and/or the pure area.  The value returned is
   the object which was dumped.  Typically (but not always) this
   will be a piece of SCode which is then evaluated to perform
   definitions in some environment.
*/

DEFINE_PRIMITIVE("BINARY-FASLOAD", Prim_Binary_Fasload, 1)
{
  long status;
  Pointer object;
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);
  if (!(Open_Dump_File(Arg1,OPEN_FLAG)))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  object = binary_fasload_fd(get_file_des(),NIL,&status);

  if (band_p)
  {
    Close_Dump_File();
    Primitive_Error(ERR_FASLOAD_BAND);
  }
  if (status != PRIM_DONE)
  {
    if (status == PRIM_INTERRUPT)
    {
      Primitive_Interrupt();
    }
    else
    {
      Close_Dump_File();
      Primitive_Error(status);
    }
  }
  Close_Dump_File();
  PRIMITIVE_RETURN(object);
}

DEFINE_PRIMITIVE("BINARY-FASLOAD-FD", Prim_Binary_Fasload_Fd, 2)
{
  long status, fd;
  Pointer object, current_pkg;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  fd = Get_Integer(Arg1);
  current_pkg = Arg2;
  if (OS_eof_p(fd))
    {
      return Make_Non_Pointer(TC_IO_ERROR_CODE,0xE0F);
    }
  object = binary_fasload_fd(fd,current_pkg,&status);
  if (status != PRIM_DONE)
    {
      if (status == PRIM_INTERRUPT)
	{
	  Primitive_Interrupt();
	}
      else
	{
	  Primitive_Error(status);
	}
    }
  PRIMITIVE_RETURN (object);
}

Pointer 
binary_fasload_fd(fd, current_pkg, status) 
     long fd;
     Pointer current_pkg;
     long *status;
{
  set_current_pkg(current_pkg);
  set_file_des(fd);
  *status = read_file_start();
  if (*status != PRIM_DONE)
    return (NIL);
  return (load_file(false, status));
}

/* Band loading. */

static char *reload_band_name = ((char *) NULL);

/* (RELOAD-BAND-NAME)
   Returns the filename (as a Scheme string) from which the runtime system
   was band loaded (load-band'ed ?), or NIL if the system was fasl'ed.
*/

DEFINE_PRIMITIVE("RELOAD-BAND-NAME", Prim_reload_band_name, 0)
{
  Primitive_0_Args();

#ifdef butterfly

  if (SHARED_DATA->Band_Load_Name[0] == '\0')
    PRIMITIVE_RETURN(NIL);
  else
    PRIMITIVE_RETURN(C_String_To_Scheme_String(SHARED_DATA->Band_Load_Name));

#else /* not butterfly */

  if (reload_band_name == NULL)
  {
    PRIMITIVE_RETURN(NIL);
  }

  PRIMITIVE_RETURN(C_String_To_Scheme_String(reload_band_name));

#endif /* butterfly */
}

/* Utility for load band below. */

extern void compiler_reset_error();

void 
compiler_reset_error()
{
  fprintf(stderr,
	  "\ncompiler_restart_error: The band being restored and\n");
  fprintf(stderr,
	  "the compiled code interface in this microcode are inconsistent.\n");
  Microcode_Termination(TERM_COMPILER_DEATH);
}

/* (LOAD-BAND FILE-NAME)
   Restores the heap and pure space from the contents of FILE-NAME,
   which is typically a file created by DUMP-BAND.  The file can,
   however, be any file which can be loaded with BINARY-FASLOAD.
*/

#ifdef butterfly

extern void
  start_band_load(),
  end_band_load();

#else /* not butterfly */

#define start_band_load()
#define end_band_load()

#endif /* butterfly */

DEFINE_PRIMITIVE("LOAD-BAND", Prim_Band_Load, 1)
{
  extern char *malloc();
  extern void compiler_reset();
  extern Pointer compiler_utilities;

  jmp_buf swapped_buf, *saved_buf;
  Pointer *saved_free, *saved_free_constant, *saved_stack_pointer;
  Pointer *saved_code, *saved_memtop;
  long temp, length;
  long status;
  Pointer result;
  char *band_name; 

  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);

  if (Debug_Flags[6])
    printf("== starting band load at %d\n", System_Clock());
  if (Debug_Flags[28])
    really_spin_for_debugger ("Band Load");

  saved_free = Free;
  saved_memtop = MemTop;
  Free = Heap_Bottom;
  SET_MEMTOP(Heap_Top);

  /* This must be here, before shared parameters are set. */

  start_band_load();

  saved_free_constant = Free_Constant;
  Free_Constant = Constant_Space;
  saved_stack_pointer = Stack_Pointer;
  Stack_Pointer = Highest_Allocated_Address;
  saved_code = Code_Space_Free();
  Code_Space_Reset();

  if (!(Open_Dump_File(Arg1,OPEN_FLAG)))
  {
    Free = saved_free;
    SET_MEMTOP(saved_memtop);
    Free_Constant = saved_free_constant;
    Stack_Pointer = saved_stack_pointer;
    Code_Space_Set(saved_code);
    end_band_load(false);
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }

  if (Debug_Flags[2])
  {
    printf("Band load called with %s\n",
	   Scheme_String_To_C_String(Arg1));
    fflush(stdout);
  }

  status = read_file_start();
  if (Debug_Flags[2])
  {
    printf("Return from read_file_start -> %d\n", status);
    fflush(stdout);
  }

  if (status != PRIM_DONE)
  {
    Free = saved_free;
    SET_MEMTOP(saved_memtop);
    Free_Constant = saved_free_constant;
    Stack_Pointer = saved_stack_pointer;
    Code_Space_Set(saved_code);
    if (status == PRIM_INTERRUPT)
    {
      end_band_load(false);
      Primitive_Error(ERR_FASL_FILE_TOO_BIG);
    }
    else
    {
      Close_Dump_File();
      end_band_load(false);
      Primitive_Error(status);
    }
  }

  /* Point of no return. */

  length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  band_name = malloc(length);
  if (band_name != ((char *) NULL))
  {
    strcpy(band_name, Scheme_String_To_C_String(Arg1));
  }

  /* There is some jiggery-pokery going on here to make sure
     that all returns from Fasload (including error exits) return to
     the clean-up code before returning on up the C call stack.
  */

  saved_buf = Back_To_Eval;
  temp = setjmp(swapped_buf);
  if (temp != 0)
  {
    extern char *Error_Names[], *Abort_Names[];

    if (temp > 0)
    {
      fprintf(stderr,
	      "\nload-band: Error %d (%s) past the point of no return.\n",
	      temp, Error_Names[temp]);
    }
    else
    {
      fprintf(stderr,
	      "\nload-band: Abort %d (%s) past the point of no return.\n",
	      temp, Abort_Names[(-temp)-1]);
    }

    if (band_name != ((char *) NULL))
    {
      fprintf(stderr, "band-name = \"%s\".\n", band_name);
      free(band_name);
    }
    end_band_load(false);
    Close_Dump_File();
    Back_To_Eval = saved_buf;
    Microcode_Termination(TERM_DISK_RESTORE);
    /*NOTREACHED*/
  }

  if (Debug_Flags[2])
  {
    printf("Ready to call load_file\n");
    fflush(stdout);
  }
  Back_To_Eval = ((jmp_buf *) swapped_buf);

  result = load_file(true, &status);
  if (status != PRIM_DONE)
  {
    /* Note that this longjmp's back to the conditional above. */
    Primitive_Error(status);
  }

  Back_To_Eval = saved_buf;

  if (reload_band_name != ((char *) NULL))
  {
    free(reload_band_name);
  }
  reload_band_name = band_name;

#ifdef butterfly
  strcpy(SHARED_DATA->Band_Load_Name, reload_band_name);
#endif

  Initialize_Stack();
  Set_Pure_Top();
  compiler_utilities = Vector_Ref(result, 1);
  compiler_reset(compiler_utilities);

  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();

  Store_Expression(Vector_Ref(result, 0));
  Store_Env(Make_Non_Pointer(GLOBAL_ENV, GO_TO_GLOBAL));

  /* Clear various state parameters. */

  Trapping = false;
  Return_Hook_Address = NULL;
  History = Make_Dummy_History();
  Prev_Restore_History_Stacklet = NIL;
  Prev_Restore_History_Offset = 0;
  Fluid_Bindings = NIL;
  Current_State_Point = NIL;

  end_band_load(true);
  Close_Dump_File();
  Band_Load_Hook();

  if (Debug_Flags[0])
    printf("Ready to run 0x%x\n", Fetch_Expression());
  if (Debug_Flags[6])
    printf("== finished band load at %d\n", System_Clock());

  PRIMITIVE_ABORT(PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
}

#ifdef BYTE_INVERSION

#define MAGIC_OFFSET (TC_FIXNUM + 1)

Pointer String_Chain, Last_String;

Setup_For_String_Inversion()
{
  String_Chain = NIL;
  Last_String = NIL;
  return;
}

Finish_String_Inversion()
{
  if (Byte_Invert_Fasl_Files)
  {
    while (String_Chain != NIL)
    {
      long Count;
      Pointer Next;

      Count = Get_Integer(Fast_Vector_Ref(String_Chain, STRING_HEADER));
      Count = 4*(Count-2)+Type_Code(String_Chain)-MAGIC_OFFSET;
      if (Reloc_Debug)
      {
	printf("String at 0x%x: restoring length of %d.\n",
	       Address(String_Chain), Count);
      }
      Next = Fast_Vector_Ref(String_Chain, STRING_LENGTH);
      Fast_Vector_Set(String_Chain, STRING_LENGTH, ((Pointer) (Count)));
      String_Chain = Next;
    }
  }
  return;
}

#define print_char(C) printf(((C < ' ') || (C > '|')) ?	\
			     "\\%03o" : "%c", (C && MAX_CHAR));

String_Inversion(Orig_Pointer)
     Pointer *Orig_Pointer;
{
  Pointer *Pointer_Address;
  char *To_Char;
  long Code;

  if (!Byte_Invert_Fasl_Files)
  {
    return;
  }

  Code = Type_Code(Orig_Pointer[STRING_LENGTH]);
  if (Code == 0)	/* Already reversed? */
  {
    long Count, old_size, new_size, i;

    old_size = Get_Integer(Orig_Pointer[STRING_HEADER]);
    new_size = 
      2 + (((long) (Orig_Pointer[STRING_LENGTH]))) / 4;

    if (Reloc_Debug)
    {
      printf("\nString at 0x%x with %d characters",
             Orig_Pointer,
             ((long) (Orig_Pointer[STRING_LENGTH])));
    }

    if (old_size != new_size)
    {
      printf("\nWord count changed from %d to %d: ",
             old_size , new_size);
      printf("\nWhich, of course, is impossible!!\n");
      Microcode_Termination(TERM_EXIT);
    }

    Count = ((long) (Orig_Pointer[STRING_LENGTH])) % 4;
    if (Count == 0)
    {
      Count = 4;
    }
    if (Last_String == NIL)
    {
      String_Chain = Make_Pointer(Count + MAGIC_OFFSET, Orig_Pointer);
    }
    else
    {
      Fast_Vector_Set(Last_String, STRING_LENGTH,
		      Make_Pointer(Count + MAGIC_OFFSET, Orig_Pointer));
    }

    Last_String = Make_Pointer(TC_NULL, Orig_Pointer);
    Orig_Pointer[STRING_LENGTH] = NIL;
    Count = Get_Integer(Orig_Pointer[STRING_HEADER]) - 1;
    if (Reloc_Debug) 
    {
       printf("\nCell count=%d\n", Count);
     }
    Pointer_Address = &(Orig_Pointer[STRING_CHARS]);
    To_Char = (char *) Pointer_Address;
    for (i = 0; i < Count; i++, Pointer_Address++)
    {
      int C1, C2, C3, C4;

      C4 = Type_Code(*Pointer_Address) & 0xFF;
      C3 = (((long) *Pointer_Address)>>16) & 0xFF;
      C2 = (((long) *Pointer_Address)>>8) & 0xFF;
      C1 = ((long) *Pointer_Address) & 0xFF;
      if (Reloc_Debug || (old_size != new_size))
      {
	print_char(C1);
        print_char(C2);
        print_char(C3);
        print_char(C4);
      }
      *To_Char++ = C1;
      *To_Char++ = C2;
      *To_Char++ = C3;
      *To_Char++ = C4;
    }
  }
  if (Reloc_Debug)
  {
    printf("\n");
  }
  return;
}
#endif /* BYTE_INVERSION */
