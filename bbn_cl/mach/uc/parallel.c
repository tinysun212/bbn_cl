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
  This is a set of routines which provide semaphores, queues and
  other such facilities for concurrent processing.
*/

#include "scheme.h"
#include "primitive.h"
#include "locks.h"

/*
  QUEUES:
    A queue is implemented as a HUNK3 containing the queue length and
    pointers to the head and tail of the queue.  All enqueueing and
    dequeueing operations are done atomically as long as only this
    set of primitives is used.  When the first location is locked, the
    entire object is considered locked.

    When enqueueing, the user provides the cons like object for the
    list.  This can be a strong or weak cons.  The cdr of this object
    will be used for chaining.
*/

#define QUEUE_LENGTH 0
#define QUEUE_HEAD 1
#define QUEUE_TAIL 2
#define LISP_QUEUE_SIZE 3

Define_Primitive(Prim_Enqueue_Head, 2, "ENQUEUE-HEAD!")
{ Lock_Handle the_lock;
  Pointer Old_Head;

  Primitive_2_Args();
  Arg_1_Type(TC_HUNK3);
  Arg_2_GC_Type(GC_Pair);
  the_lock=Lock_Cell(Nth_Vector_Loc(Arg1, QUEUE_LENGTH));

  Old_Head = Vector_Ref(Arg1, QUEUE_HEAD);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg2, CONS_CDR), Old_Head);
  if (Old_Head == NIL)
    Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_TAIL), Arg2);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_HEAD), Arg2);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_LENGTH),
		   Make_Non_Pointer(TC_FIXNUM,
				    Get_Integer(Vector_Ref(Arg1, QUEUE_LENGTH))+1));
  
  Unlock_Cell(the_lock);
  return Arg2;
}

Define_Primitive(Prim_Enqueue_Tail, 2, "ENQUEUE-TAIL!")
{ Lock_Handle the_lock;
  Pointer Old_Tail;
  
  Primitive_2_Args();
  Arg_1_Type(TC_HUNK3);
  Arg_2_GC_Type(GC_Pair);
  the_lock=Lock_Cell(Nth_Vector_Loc(Arg1, QUEUE_LENGTH));
  
  Old_Tail = Vector_Ref(Arg1, QUEUE_TAIL);
  if (Old_Tail != NIL)
    Do_Store_No_Lock(Nth_Vector_Loc(Old_Tail, CONS_CDR), Arg2);
  else
    Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_HEAD), Arg2);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg2, CONS_CDR), NIL);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_TAIL), Arg2);
  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_LENGTH),
		   Make_Non_Pointer(TC_FIXNUM,
				    Get_Integer(Vector_Ref(Arg1, QUEUE_LENGTH))+1));
  
  Unlock_Cell(the_lock);
  return Arg2;
}

Define_Primitive(Prim_Dequeue_Head, 1, "DEQUEUE-HEAD!")
{ Lock_Handle the_lock;
  Pointer Result;

  Primitive_1_Arg();
  Arg_1_Type(TC_HUNK3);
  Result = Vector_Ref(Arg1, QUEUE_HEAD);
  if (GC_Type(Result) != GC_Pair && Result != NIL)
    error_wrong_type_arg(1);

  the_lock = Lock_Cell(Nth_Vector_Loc(Arg1, QUEUE_LENGTH));
  
  Result = Vector_Ref(Arg1, QUEUE_HEAD);
  if (Result != NIL) 
    { Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_HEAD),
		       Vector_Ref(Result, CONS_CDR));
      if (Vector_Ref(Arg1, QUEUE_TAIL) == Result)
	Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_TAIL), NIL);
      Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_LENGTH),
		       Make_Non_Pointer(TC_FIXNUM,
					Get_Integer(Vector_Ref(Arg1, QUEUE_LENGTH))
					-1));
      Do_Store_No_Lock(Nth_Vector_Loc(Result, CONS_CDR), NIL);
    }
		       
  Unlock_Cell(the_lock);
  return Result;
}

Define_Primitive(Prim_Dequeue_Tail, 1, "DEQUEUE-TAIL!")
{ Lock_Handle the_lock;
  Pointer Result, Head, Next;

  Primitive_1_Arg();
  Arg_1_Type(TC_HUNK3);
  Head = Vector_Ref(Arg1, QUEUE_HEAD);
  if (GC_Type(Head) != GC_Pair && Head != NIL)
    error_wrong_type_arg(1);
  Result = Vector_Ref(Arg1, QUEUE_TAIL);
  if (GC_Type(Result) != GC_Pair && Result != NIL)
    error_wrong_type_arg(1);

  the_lock = Lock_Cell(Nth_Vector_Loc(Arg1, QUEUE_LENGTH));
  
  Result = Vector_Ref(Arg1, QUEUE_TAIL);
  if (Result != NIL) 
    { Head = Vector_Ref(Arg1, QUEUE_HEAD);
      if (Head == Result)
	{ Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_HEAD), NIL);
	  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_TAIL), NIL);
	}
      else
	{ for (;
	       GC_Type(Head) == GC_Pair;
	       Head = Next)
	    { Next = Vector_Ref(Head, CONS_CDR);
	      if (Next == Result)
		{ Do_Store_No_Lock(Nth_Vector_Loc(Head, CONS_CDR), NIL);
		  Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_TAIL), Head);
		  break;
		}
	    }
	}
      Do_Store_No_Lock(Nth_Vector_Loc(Arg1, QUEUE_LENGTH),
		       Make_Non_Pointer(TC_FIXNUM,
					Get_Integer(Vector_Ref(Arg1,
							       QUEUE_LENGTH))
					-1));
      Do_Store_No_Lock(Nth_Vector_Loc(Result, CONS_CDR), NIL);
    }

  Unlock_Cell(the_lock);
  return Result;
}

Pointer Increment_Cell(Base,Offset,Increment)
Pointer Base, Increment;
long Offset;
{ Lock_Handle lock;
  Pointer old_value;

  lock=Lock_Cell(Nth_Vector_Loc(Base,Offset));
  old_value=Fast_Vector_Ref(Base,Offset);
  if (Type_Code(old_value)!=TC_FIXNUM) {
    Unlock_Cell(lock);
    return NIL; }
  Fast_Vector_Set(Base,Offset,
		  Make_Non_Pointer(TC_FIXNUM,
				   Get_Integer(old_value)+
				   Get_Integer(Increment)));
  Unlock_Cell(lock);
  return old_value; }

Define_Primitive(Prim_Atomic_Add_Car,2,"ATOMIC-ADD-CAR!")
{ Primitive_2_Args();
  Arg_1_GC_Type(GC_Pair);
  Arg_2_Type(TC_FIXNUM);
  return Increment_Cell(Arg1,CONS_CAR,Arg2); }

Define_Primitive(Prim_Atomic_Add_Cdr,2,"ATOMIC-ADD-CDR!")
{ Primitive_2_Args();
  Arg_1_GC_Type(GC_Pair);
  Arg_2_Type(TC_FIXNUM);
  return Increment_Cell(Arg1,CONS_CDR,Arg2); }

Define_Primitive(Prim_Atomic_Add_Vector,3,"ATOMIC-ADD-VECTOR!")
{ long offset;
  Primitive_3_Args();

  Arg_1_GC_Type(GC_Vector);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Range_Check(offset,Arg2,0,Vector_Length(Arg1)-1,ERR_ARG_2_BAD_RANGE);
  return Increment_Cell(Arg1,offset+1,Arg3); }

Define_Primitive(Prim_Logical_And,2,"LOGICAL-AND")
{ Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1) & Get_Integer(Arg2));
}

Define_Primitive(Prim_Logical_Ior,2,"LOGICAL-IOR")
{ Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1) | Get_Integer(Arg2));
}

Define_Primitive(Prim_Logical_Not,1,"LOGICAL-NOT")
{ Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  return Make_Non_Pointer(TC_FIXNUM,~Get_Integer(Arg1));
}

Define_Primitive(Prim_Logical_Lsh,2,"LOGICAL-LSH")
{ int shift;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Sign_Extend(Arg2,shift);
  if (shift>0)
    return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1)<<shift);
  else return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1)>>(-shift));
}

Define_Primitive(Prim_Logical_Rsh,2,"LOGICAL-RSH")
{ int shift;
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Sign_Extend(Arg2,shift);
  if (shift>0)
    return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1)>>shift);
  else return Make_Non_Pointer(TC_FIXNUM,Get_Integer(Arg1)<<(-shift));
}

Define_Primitive(Prim_Purify_Code, 0, "PURIFY-CODE-SPACE")
{
#ifdef butterfly
  purify_code_space();
#endif
}

Define_Primitive(Prim_Impurify_Code, 0, "IMPURIFY-CODE-SPACE")
{
#ifdef butterfly
  impurify_code_space();
#endif
}
