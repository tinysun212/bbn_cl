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

/*
  	Site specific definitions for BBN stuff.
*/

#ifndef butterfly

#define Code_Space_Base() ((long) NULL)
#define Code_Space_Size() 0
#define Code_Space_Check(size) false
#define Code_Space_Free() ((Pointer *) NULL)
#define Code_Space_Reset() 
#define Code_Space_Set(new) 
#define Code_Space_Lock() 
#define Code_Space_Unlock()
#define Propagate_Code(X, Y)

#endif

/*
  Easy sensor of commonlisp mode.
*/

#define CL_Mode() (Get_Fixed_Obj_Slot(UCode_CommonLisp_Mode)!=NIL)

/*
  Accessors for the CL fixed objects vector
*/

#define Get_CL_Fixed_Obj_Slot(N)     \
  Fast_User_Vector_Ref(Get_Fixed_Obj_Slot(CL_Fixed_Objects), N)

#define Set_CL_Fixed_Obj_Slot(N,S)   \
  Fast_User_Vector_Set(Get_Fixed_Obj_Slot(CL_Fixed_Objects), N, S)

/* 
  Macros to access cl fixed objs.
  Must track cl-fixobj.scm.
  These are defined as macros for speed;
   would be easier if C preprocessor would
   expand macros until no #define's remained.
*/

#define Sym_colon_input                 Get_CL_Fixed_Obj_Slot(0)
#define Sym_colon_output                Get_CL_Fixed_Obj_Slot(1)
#define Sym_colon_io                    Get_CL_Fixed_Obj_Slot(2)
#define Sym_colon_error                 Get_CL_Fixed_Obj_Slot(3)
#define Sym_colon_create                Get_CL_Fixed_Obj_Slot(4)
#define Sym_colon_new_version           Get_CL_Fixed_Obj_Slot(5)
#define Sym_colon_rename                Get_CL_Fixed_Obj_Slot(6)
#define Sym_colon_rename_and_delete     Get_CL_Fixed_Obj_Slot(7)
#define Sym_colon_overwrite             Get_CL_Fixed_Obj_Slot(8)
#define Sym_colon_append                Get_CL_Fixed_Obj_Slot(9)
#define Sym_colon_suprsede              Get_CL_Fixed_Obj_Slot(10)
#define Sym_colon_probe                 Get_CL_Fixed_Obj_Slot(11)
#define Sym_colon_eof                   Get_CL_Fixed_Obj_Slot(12)
#define Sym_star_standard_input_star    Get_CL_Fixed_Obj_Slot(13)
#define Sym_star_standard_output_star   Get_CL_Fixed_Obj_Slot(14)
#define Sym_star_terminal_io_star       Get_CL_Fixed_Obj_Slot(15)
#define Sym_t                           Get_CL_Fixed_Obj_Slot(16)
#define Sym_string_char                 Get_CL_Fixed_Obj_Slot(17)
#define Sym_colon_start                 Get_CL_Fixed_Obj_Slot(18)
#define Sym_colon_end                   Get_CL_Fixed_Obj_Slot(19)
#define Sym_colon_abort                 Get_CL_Fixed_Obj_Slot(20)
#define Sym_colon_ok                    Get_CL_Fixed_Obj_Slot(21)
#define Newline_char_set                Get_CL_Fixed_Obj_Slot(22)
#define Sym_bit				Get_CL_Fixed_Obj_Slot(23)
#define Sym_mod				Get_CL_Fixed_Obj_Slot(24)
#define List_unsigned_byte_1            Get_CL_Fixed_Obj_Slot(25)
#define List_unsigned_byte_2            Get_CL_Fixed_Obj_Slot(26)
#define List_unsigned_byte_4            Get_CL_Fixed_Obj_Slot(27)
#define List_unsigned_byte_8            Get_CL_Fixed_Obj_Slot(28)
#define List_unsigned_byte_16           Get_CL_Fixed_Obj_Slot(29)
#define error_procedure			Get_CL_Fixed_Obj_Slot(30)
#define Sym_signed_byte                 Get_CL_Fixed_Obj_Slot(31)
#define Sym_unsigned_byte               Get_CL_Fixed_Obj_Slot(32)
#define List_signed_byte_1              Get_CL_Fixed_Obj_Slot(33)
#define List_signed_byte_2              Get_CL_Fixed_Obj_Slot(34)
#define List_signed_byte_4              Get_CL_Fixed_Obj_Slot(35)
#define List_signed_byte_8              Get_CL_Fixed_Obj_Slot(36)
#define List_signed_byte_16             Get_CL_Fixed_Obj_Slot(37)
#define Sym_colon_internal              Get_CL_Fixed_Obj_Slot(38)
#define Sym_colon_external              Get_CL_Fixed_Obj_Slot(39)
#define Sym_colon_inherited             Get_CL_Fixed_Obj_Slot(40)

/*
  Patches to interpret.c
*/

#define SITE_EXPRESSION_DISPATCH_HOOK()						\
 case TC_G_VECTOR:							\
 case TC_IO_ERROR_CODE:							\
 case TC_CL_PACKAGE:							\
 case TC_CLSAV:                                                         \
 case TC_RATIO:                                                         \
 case TC_CL_STREAM:                                                     \
 case TC_VECTOR_32B:                                                    \
 case TC_CL_ARRAY:							\
 case TC_CL_IVECTOR:							\
   Val = Fetch_Expression(); break;

#define SITE_RETURN_DISPATCH_HOOK()					\
    case RC_RESTORE_PROCESS_STATE:					\
      Current_State_Point = Fetch_Expression(); 			\
      Fluid_Bindings = Pop();						\
      goto Pop_Return;							\
									\
    case RC_DETERMINE_SELF:						\
      Do_Determine_Self(Val);						\
      Env = Make_Non_Pointer(GLOBAL_ENV, 0);				\
      IntEnb = INT_Mask;						\
      Initialize_Stack();	/* Clear out the stack. */		\
     Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS);		\
      Store_Return(RC_END_OF_COMPUTATION);				\
      Store_Expression(NIL);						\
      Save_Cont();							\
      Push(Get_Fixed_Obj_Slot(Default_Rescheduler));			\
      Push(STACK_FRAME_HEADER);						\
     Pushed();								\
      goto Apply_Non_Trapping;

/*
	Initialize the fixed objects vector.
*/
#define Init_Fixed_Objects() Site_Init_Fixed_Objects(Fixed_Objects)

#define Site_Init_Fixed_Objects(Fixed_Objects)				\
	{ Default_Init_Fixed_Objects(Fixed_Objects);			\
	  /* no longer necessary (didn't work, anyway) - JP 7/9/87 */	\
	}

/*	Make Generic_Reduced_Flonum_Result in flonum.h depended on
	CL_Mode() at BBN.
*/
/*
  [The following macro name is from Rel6 from MIT -- it appears to be mis-named,
    and in fact means "don't-downward-coerce-flonum-p" -las]
*/

#define DOWNWARD_COERCE_FLONUM_P(number) ((floor (number)) != (number) || CL_Mode())

#define New_Symbol_Hook(New_Symbol) set_symbol_global_value(New_Symbol)

#define USE_FILE_EXISTENCE_TEST

/*	Handle Common Lisp Strings a bit better.			*/

#define String_To_Symbol_Hook(String, length)				\
  if ((CL_Mode()) && is_NIL_string(String, length))			\
      return NIL;                                                       \
  if ((CL_Mode()) && is_T_string(String, length))			\
      return TRUTH;
