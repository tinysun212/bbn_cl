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

/* $Header: primitive.h,v 10.0 88/12/07 13:09:46 las Exp $
 * $MIT-Header: primitive.h,v 9.32 87/12/23 03:44:55 GMT cph Exp $
 */

/* This file contains some macros for defining primitives,
   for argument type or value checking, and for accessing
   the arguments. */

/* Definition of primitives. */

#define Define_Primitive(C_Name, Number_of_args, Scheme_Name)	\
extern Pointer C_Name();					\
Pointer C_Name()

#define DEFINE_PRIMITIVE(Scheme_Name, C_Name, Number_of_args)	\
extern Pointer C_Name();					\
Pointer C_Name()

/* This is a NOP.
   Any primitive declared this way must also be declared
   with Define_Primitive.
 */

#define Built_In_Primitive(C_Name, Number_of_args, Scheme_Name, index)

#ifdef ENABLE_PRIMITIVE_PROFILING
#define primitive_entry_hook() record_primitive_entry (Fetch_Expression ())
#else
#define primitive_entry_hook() {}
#endif

/* This is new header for primitives, which gives better control over
   variable allocation than older `Primitive_N_Args' macros. */

#define PRIMITIVE_HEADER(n_args) primitive_entry_hook ()

/* Primitives return by performing one of the following operations. */

#define PRIMITIVE_RETURN(value)	return (value)

#define PRIMITIVE_ABORT(action)	longjmp(*Back_To_Eval, (action))

/* Preambles for primitive procedures.  These store the arguments into
 * local variables for fast access.
 */

#define Primitive_0_Args()	primitive_entry_hook ()

#define Primitive_1_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				primitive_entry_hook ()

#define Primitive_1_Arg()	Primitive_1_Args()

#define Primitive_2_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				primitive_entry_hook ()

#define Primitive_3_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				primitive_entry_hook ()

#define Primitive_4_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				primitive_entry_hook ()

#define Primitive_5_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				primitive_entry_hook ()

#define Primitive_6_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				primitive_entry_hook ()

#define Primitive_7_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				fast Pointer Arg7 = Stack_Ref(6);	\
				primitive_entry_hook ()

#define Primitive_8_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				fast Pointer Arg7 = Stack_Ref(6);	\
				fast Pointer Arg8 = Stack_Ref(7);	\
				primitive_entry_hook ()

#define Primitive_9_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				fast Pointer Arg7 = Stack_Ref(6);	\
				fast Pointer Arg8 = Stack_Ref(7);	\
				fast Pointer Arg9 = Stack_Ref(8);	\
				primitive_entry_hook ()

#define Primitive_10_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				fast Pointer Arg7 = Stack_Ref(6);	\
				fast Pointer Arg8 = Stack_Ref(7);	\
				fast Pointer Arg9 = Stack_Ref(8);	\
				fast Pointer Arg10 = Stack_Ref(9);	\
				primitive_entry_hook ()

/* Various utilities */

#define Primitive_Error signal_error_from_primitive

#define Primitive_GC(Amount)						\
{									\
  Request_GC (Amount);							\
  Primitive_Interrupt ();						\
}

#define Primitive_GC_If_Needed(Amount)					\
  if (GC_Check (Amount)) Primitive_GC(Amount)

#define Primitive_Error_String(String, Irritant)		\
{ Will_Push(STACK_ENV_EXTRA_SLOTS + 3 + CONTINUATION_SIZE);	\
    Store_Return(RC_REPEAT_PRIMITIVE);				\
    Save_Cont();						\
    Push(NIL);							\
    Push(Irritant);						\
    Push(C_String_To_Scheme_String(String));			\
    Push(Get_Fixed_Obj_Slot(Error_Procedure));			\
    Push(STACK_FRAME_HEADER + 3);				\
  Pushed();							\
    longjmp(*Back_To_Eval, PRIM_APPLY);				\
}

#define Primitive_Interrupt()					\
{								\
  signal_interrupt_from_primitive ();				\
}

#define Range_Check(To_Where, P, Low, High, Error)			\
{									\
  To_Where = Get_Integer (P);						\
  if ((To_Where < (Low)) || (To_Where > (High)))			\
    Primitive_Error (Error);						\
}

#define Sign_Extend_Range_Check(To_Where, P, Low, High, Error)		\
{									\
  Sign_Extend ((P), To_Where);						\
  if ((To_Where < (Low)) || (To_Where > (High)))			\
    Primitive_Error (Error);						\
}

#define CHECK_ARG(argument, type_p)					\
do									\
{									\
  if (! (type_p (ARG_REF (argument))))					\
    error_wrong_type_arg (argument);					\
} while (0)

#define ARG_LOC(argument) (STACK_LOC (argument - 1))
#define ARG_REF(argument) (STACK_REF (argument - 1))

#define LEXPR_N_ARGUMENTS() (Regs [REGBLOCK_LEXPR_ACTUALS])

extern long arg_nonnegative_integer ();
extern long arg_index_integer ();
extern long object_to_long ();
extern Pointer allocate_non_marked_vector ();
extern Pointer allocate_marked_vector ();

/* Instances of the following should be flushed. */

#define Arg_1_Type(TC)  					\
do { if ((pointer_type (Arg1)) != (TC)) error_wrong_type_arg (1); } while (0)

#define Arg_2_Type(TC)  					\
do { if ((pointer_type (Arg2)) != (TC)) error_wrong_type_arg (2); } while (0)

#define Arg_3_Type(TC)						\
do { if ((pointer_type (Arg3)) != (TC)) error_wrong_type_arg (3); } while (0)

#define Arg_4_Type(TC)  					\
do { if ((pointer_type (Arg4)) != (TC)) error_wrong_type_arg (4); } while (0)

#define Arg_5_Type(TC)  					\
do { if ((pointer_type (Arg5)) != (TC)) error_wrong_type_arg (5); } while (0)

#define Arg_6_Type(TC)						\
do { if ((pointer_type (Arg6)) != (TC)) error_wrong_type_arg (6); } while (0)

#define Arg_7_Type(TC)						\
do { if ((pointer_type (Arg7)) != (TC)) error_wrong_type_arg (7); } while (0)

#define Arg_8_Type(TC)						\
do { if ((pointer_type (Arg8)) != (TC)) error_wrong_type_arg (8); } while (0)

#define Arg_9_Type(TC)						\
do { if ((pointer_type (Arg9)) != (TC)) error_wrong_type_arg (9); } while (0)

#define Arg_10_Type(TC)						\
do { if ((pointer_type (Arg10)) != (TC)) error_wrong_type_arg (10); } while (0)


#define Arg_1_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg1)) != GCTC) error_wrong_type_arg (1); } while (0)

#define Arg_2_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg2)) != GCTC) error_wrong_type_arg (2); } while (0)

#define Arg_3_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg3)) != GCTC) error_wrong_type_arg (3); } while (0)


#define FIXNUM_ARG arg_fixnum

#define UNSIGNED_FIXNUM_ARG(arg)					\
  ((FIXNUM_P (ARG_REF (arg)))						\
   ? (UNSIGNED_FIXNUM_VALUE (ARG_REF (arg)))				\
   : ((long) (error_wrong_type_arg (arg))))

#define STRING_ARG(arg)							\
  ((STRING_P (ARG_REF (arg)))						\
   ? (Scheme_String_To_C_String (ARG_REF (arg)))			\
   : ((char *) (error_wrong_type_arg (arg))))

#define BOOLEAN_ARG(arg) ((ARG_REF (arg)) != NIL)

#define CELL_ARG(arg)							\
  ((CELL_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((Pointer) (error_wrong_type_arg (arg))))

#define PAIR_ARG(arg)							\
  ((PAIR_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((Pointer) (error_wrong_type_arg (arg))))

#define WEAK_PAIR_ARG(arg)						\
  ((WEAK_PAIR_P (ARG_REF (arg)))					\
   ? (ARG_REF (arg))							\
   : ((Pointer) (error_wrong_type_arg (arg))))

#define VECTOR_ARG(arg)							\
  ((VECTOR_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((Pointer) (error_wrong_type_arg (arg))))

/* Variable argument primitives must use the following macros instead
   of the normal primitive macros.  In time, perhaps a more robust 
   interface is needed, but this suffices for now.

   A variable argument primitive should perform a Primitive_Variable_Args()
   as opposed to the Primitive_N_Args().  This will set up the variable
   Number_Of_Args, which the primitive can use in order to get the
   arguments off the stack.

   Arguments will be referenced by Primitive_Variable_Arg(N).
   N will range from 1 to Number_Of_Args, inclusive.

   Returns and errors should be performed normally.  error_wrong_type_arg(n)
   and error_bad_range_arg(n) are very useful since you can just pass
   which argument is bad.
*/

#define Primitive_Variable_Args() long Number_Of_Args = LEXPR_N_ARGUMENTS()

#define Primitive_Variable_Arg(N)  ARG_REF(N)

/* To return multiple values from any primitive, you should call this
   macro with the number of arguments to the primitive, (Number_Of_Args
   for a variable-arg primimtive), the number of values to return, the
   first value to return (i.e., value that is returned if there is no
   receiver), and the code that pushes ALL of the values (including the
   first one).  The pushing code is of the form "Push(valueN-1);
   Push(valueN-2); ... Push(value0);"
*/

#define Multiple_Value_Return(Number_Of_Prim_Args, Number_Of_Return_Values, First, The_Code_For_Values) \
  { Pointer Return_Object = Stack_Ref(Number_Of_Prim_Args);          \
    Pointer Object_Above =  Stack_Ref(Number_Of_Prim_Args+1);        \
    if ((Type_Code(Return_Object) == TC_RETURN_CODE) &&              \
        (Get_Integer(Return_Object) == RC_MULTIPLE_VALUE_RECEIVE))   \
    { int i;                                                         \
      Pointer Expr = Stack_Ref(Number_Of_Prim_Args + 1);             \
      /* Remove this primitive's arguments */                        \
      for (i = 0; i < Number_Of_Prim_Args; i++)                      \
      { Pop();                                                       \
      }                                                              \
      Pop();                                                         \
      Pop();                                                         \
      The_Code_For_Values;                                           \
      Push(Expr);                                                    \
      Push(STACK_FRAME_HEADER + Number_Of_Return_Values);            \
      longjmp(*Back_To_Eval, PRIM_APPLY);                            \
    }                                                                \
    else if ((Return_Object == return_to_interpreter) &&             \
	     (Type_Code(Object_Above) == TC_RETURN_CODE) &&          \
	     (Get_Integer(Object_Above) == RC_MULTIPLE_VALUE_RECEIVE)) \
    { int i;                                                         \
      Pointer Expr = Stack_Ref(Number_Of_Prim_Args + 2);             \
      /* Remove this primitive's arguments */                        \
      for (i = 0; i < Number_Of_Prim_Args; i++)                      \
      { Pop();                                                       \
      }                                                              \
      Pop();                                                         \
      Pop();                                                         \
      Pop();                                                         \
      The_Code_For_Values;                                           \
      Push(Expr);                                                    \
      Push(STACK_FRAME_HEADER + Number_Of_Return_Values);            \
      longjmp(*Back_To_Eval, PRIM_APPLY);                            \
    }                                                                \
    else return(First);                                              \
  }

