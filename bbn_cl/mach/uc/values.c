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
  Multiple values code for Scheme
*/

#include "scheme.h"
#include "primitive.h"

/* (WITH-VALUES THUNK RECEIVER) applies the thunk and passes
   one or more values returned to receiver.  Note: multiple values
   are passed back via VALUES below.
*/

Define_Primitive(prim_with_values, 2, "WITH-VALUES")
{
  Primitive_2_Args();

  Pop_Primitive_Frame(2);
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+2));
  Store_Return(RC_MULTIPLE_VALUE_RECEIVE);
  Store_Expression(Arg2);
  Save_Cont();
  Push(Arg1);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
  /*NOTREACHED*/
}
    
/* (VALUES VAL-1 ... VAL-N) is a variable argument primitive that 
   passes back the multiple values if there is a receiver waiting for
   them or returns just the first value if no receiver is waiting.
*/

Define_Primitive(prim_values, -1, "VALUES")
{ Pointer Expr;
  long i;
  Pointer Return_Object, Object_Above;
  Primitive_Variable_Args();

  Return_Object = Stack_Ref(Number_Of_Args);
  Object_Above  = Stack_Ref(Number_Of_Args+1);

  if ((Type_Code(Return_Object) == TC_RETURN_CODE) &&
      (Get_Integer(Return_Object) == RC_MULTIPLE_VALUE_RECEIVE))
  { Expr = Stack_Ref(Number_Of_Args + 1);
    
    for (i = 1; i <= Number_Of_Args; i++)
    { Stack_Ref(Number_Of_Args + CONTINUATION_SIZE - i) =
	Stack_Ref(Number_Of_Args - i);
    }
    /* Adjust the stack pointer by 2 to get rid of
       the RC_MULTIPLE_VALUE_RECEIVE and the receiver */
    Pop();
    Pop();
    Push(Expr);
    Push(STACK_FRAME_HEADER + Number_Of_Args);
    longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  else if ((Return_Object == return_to_interpreter) &&
	   (Type_Code(Object_Above) == TC_RETURN_CODE) &&
	   (Get_Integer(Object_Above) == RC_MULTIPLE_VALUE_RECEIVE))
  { Expr = Stack_Ref(Number_Of_Args + 2);
    
    for (i = 1; i <= Number_Of_Args; i++)
    { Stack_Ref(Number_Of_Args + CONTINUATION_SIZE + 1 - i) =
	Stack_Ref(Number_Of_Args - i);
    }
    /* Adjust the stack pointer by 3 to get rid of
       the return_to_interpreter, RC_MULTIPLE_VALUE_RECEIVE and the receiver */
    Pop();
    Pop();
    Pop();
    Push(Expr);
    Push(STACK_FRAME_HEADER + Number_Of_Args);
    longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  else if (Number_Of_Args > 0)
  { Pointer Arg1;
    Arg1 = Primitive_Variable_Arg(1);

    /* Get rid of all but the first value */
    for (i = 0; i < Number_Of_Args; i++)
    { Pop();
    }
    return(Arg1);
  }
  else
  { return(NIL);
  }
}

/* (VALUES-LIST '(VAL-1 ... VAL-N)) is equivalent to
   (VALUES VAL-1 ... VAL-N)
*/

Define_Primitive(prim_values_list, 1, "VALUES-LIST")
{ Pointer Expr, scan;
  Pointer Return_Object, Object_Above;
  long i = 0;
  long cnt = 0;
  Primitive_1_Arg();

  Return_Object = Stack_Ref(1);
  Object_Above = Stack_Ref(2);

  if ((Type_Code(Return_Object) == TC_RETURN_CODE) &&
      (Get_Integer(Return_Object) == RC_MULTIPLE_VALUE_RECEIVE))
  { Expr = Stack_Ref(2);
    
    /* Get rid of argument */
    Pop();
    /* Get rid of return code and receiver */
    Pop();
    Pop();

    scan = Arg1;
    while (Type_Code(scan) == TC_LIST)
    { scan = Vector_Ref(scan, CONS_CDR);
      i++;
    }
    if (Type_Code(scan) != NIL)
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);

   Will_Push(i + 2);

    for (cnt = 0; cnt < i; cnt++)
      Push(NIL);
    for (cnt = 0; cnt < i; cnt++)
    { Stack_Ref(cnt) = Vector_Ref(Arg1, CONS_CAR);
      Arg1 = Vector_Ref(Arg1, CONS_CDR);
    }

    Push(Expr);
    Push(STACK_FRAME_HEADER + i);
   Pushed();
    longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  else if ((Return_Object == return_to_interpreter) &&
	   (Type_Code(Object_Above) == TC_RETURN_CODE) &&
	   (Get_Integer(Object_Above) == RC_MULTIPLE_VALUE_RECEIVE))
  { Expr = Stack_Ref(3);
    
    /* Get rid of argument */
    Pop();
    /* Get rid of return_to_interpreter, return code and receiver */
    Pop();
    Pop();
    Pop();

    scan = Arg1;
    while (Type_Code(scan) == TC_LIST)
    { scan = Vector_Ref(scan, CONS_CDR);
      i++;
    }
    if (Type_Code(scan) != NIL)
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);

   Will_Push(i + 2);

    for (cnt = 0; cnt < i; cnt++)
      Push(NIL);
    for (cnt = 0; cnt < i; cnt++)
    { Stack_Ref(cnt) = Vector_Ref(Arg1, CONS_CAR);
      Arg1 = Vector_Ref(Arg1, CONS_CDR);
    }

    Push(Expr);
    Push(STACK_FRAME_HEADER + i);
   Pushed();
    longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  else if (Arg1 == NIL)
    return (NIL);
  else
    return (Vector_Ref(Arg1, CONS_CAR));
}
