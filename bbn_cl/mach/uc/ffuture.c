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
#include "locks.h"
#include "futures.h"
#include "zones.h"

extern int Debug_Flags[];

#ifdef butterfly
extern Boolean Lifo_Not_Fifo;
#define Bump_Useful_Meter(n) atomadd32(&SHARED_DATA->Useful_Meters[n], 1)
#else
#define Who_Am_I 0
#define Standard_Delay()
#define Bump_Useful_Meter(n)
#endif

#define Get_Current_Future() \
	Fast_Vector_Ref(Get_Fixed_Obj_Slot(Future_Vector), \
			Who_Am_I + 1)

#define Get_Status_Symbol(name) \
	Fast_Vector_Ref(Get_Fixed_Obj_Slot(Status_Symbols), name)

#define SYM_RUNNABLE 1		/* 'RUNNABLE */
#define SYM_CREATED 2		/* 'CREATED */
#define SYM_DETERMINED 3	/* 'DETERMINED */
#define SYM_WAITING 4		/* 'WAITING */
#define SYM_YOUR_TURN 5		/* 'YOUR-TURN */
#define SYM_WORK_WAIT 6		/* 'WAITING-FOR-WORK */
#define SYM_RUNNING 7		/* 'RUNNING */
#define SYM_DELAYED 8		/* 'DELAYED */
#define SYM_PAUSED 9		/* 'PAUSED */

#define Have_Scheduler_Info() \
  	(Get_Fixed_Obj_Slot(Scheduler_Info) != NIL)
#define Get_Scheduler_Info(name) \
	Fast_Vector_Ref(Get_Fixed_Obj_Slot(Scheduler_Info),name)
#define Set_Scheduler_Info(name, value) \
	Fast_Vector_Set(Get_Fixed_Obj_Slot(Scheduler_Info),name,value)

#define SCHINFO_DELTA 1		/* clock delta */
#define SCHINFO_OVERFLOW 2	/* is queue overflow an error or a call? */

Define_Primitive(Prim_Start_Preempting, 0, "%START-PREEMPTING")
{
  Primitive_0_Args();
  if (Have_Scheduler_Info() &&	/* If no preemption, turn it on. */
      Type_Code(Get_Scheduler_Info(SCHINFO_DELTA)) == TC_FIXNUM)
    {
      long delta;
      Sign_Extend(Get_Scheduler_Info(SCHINFO_DELTA), delta);
      Set_Int_Timer(0, delta);
      return TRUTH;
    }
  return NIL;
}

Define_Primitive(Prim_Stop_Preempting, 0, "%STOP-PREEMPTING")
{
  Primitive_0_Args();
  Clear_Int_Timer();
  return TRUTH;
}


/*
  (%SPAWN-PROCESS THUNK DOC SCHEDULE-IT? #!optional PRIORITY)

  If SCHEDULE-IT? is true, then the new task is put on the work queue.
  This primitive returns a future object.
*/

#if 0

/**** Define_Primitive(Prim_Spawn_Process, -1, "%SPAWN-PROCESS") ****/
{ Pointer Make_A_Future(), Make_Initial_Process(), The_Future;
  long Saved_Zone;

  Primitive_Variable_Args();
  fast Pointer Arg1 = Primitive_Variable_Arg(1);
  fast Pointer Arg2 = Primitive_Variable_Arg(2);
  fast Pointer Arg3 = Primitive_Variable_Arg(3);
  fast Pointer Arg4;

  switch (Number_Of_Args)
  { case 3: Arg4 = Fast_Vector_Ref(Get_Current_Future(), FUTURE_PRIORITY);
            break;
    case 4: Arg4 = Primitive_Variable_Arg(4);
            Arg_4_Type(TC_FIXNUM);
            break;
    case 0:
    case 1:
    case 2: error_too_few_args(3); break;
    default: error_too_many_args(4);
  } 

#endif


Define_Primitive(Prim_Spawn_Process, 3, "%SPAWN-PROCESS")
{ Pointer Make_A_Future(), Make_Initial_Process(), The_Future;
  long Saved_Zone;

  fast Pointer Arg4 = Fast_Vector_Ref(Get_Current_Future(), FUTURE_PRIORITY);

  Primitive_3_Args();


  /* ******************************* */

  Save_Time_Zone(Zone_Scheduler);
  Primitive_GC_If_Needed(32 + FUTURE_SIZE);

#ifdef butterfly
  Bump_Useful_Meter(2);
#ifdef FINISH_LINE
  if (SHARED_DATA->Work_Queue.queue_count > SHARED_DATA->Queue_Overflow) 
  { if (Have_Scheduler_Info() &&
	Vector_Length(Get_Fixed_Obj_Slot(Scheduler_Info)) >= 2 &&
	Get_Scheduler_Info(SCHINFO_OVERFLOW) == NIL) 
    { /* Subsume the Future */
      Pop_Primitive_Frame(3);	/* Pop off the primitives arguments */
     Will_Push(STACK_ENV_EXTRA_SLOTS);
      Push(Arg1);
      Push(STACK_FRAME_HEADER);
     Pushed();
      longjmp(*Back_To_Eval, PRIM_APPLY); 
    }
    else
    { /* Report an Error */
      Primitive_Error_String("Work queue overflow", Arg2); 
    }
  }
#endif FINISH_LINE
  Bump_Useful_Meter(3);
#endif

  The_Future = Make_A_Future(Make_Initial_Process(Arg1), Arg2, Arg1);
  Fast_Vector_Set(The_Future, FUTURE_PRIORITY, Arg4);
  if (Arg3 == Get_Status_Symbol(SYM_DELAYED))
  { Fast_Vector_Set(The_Future, FUTURE_STATUS, Get_Status_Symbol(SYM_DELAYED));
    return The_Future;
  }
  else if (Arg3 == NIL) return The_Future;

  Enqueue_The_Task(The_Future);
  Restore_Time_Zone();
  return The_Future;
}


Enqueue_The_Task(The_Future)
Pointer The_Future;
{
  /* DANGER DANGER DANGER: This assumes that the futures are either locked */
			/* or otherwise safe to store 'RUNNABLE into. */
  Fast_Vector_Set(The_Future, FUTURE_STATUS, Get_Status_Symbol(SYM_RUNNABLE));

#ifdef butterfly
  Put_Work(The_Future);
#else
  { Pointer The_Queue, Queue_Tail, New_Entry;
    
    The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
    if (The_Queue==NIL) {
      The_Queue = Make_Pointer(TC_LIST, Free);
      Set_Fixed_Obj_Slot(The_Work_Queue, The_Queue);
      *Free++ = NIL;
      *Free++ = NIL; }
    
    Queue_Tail = Vector_Ref(The_Queue, CONS_CDR);
    New_Entry = Make_Pointer(TC_WEAK_CONS, Free);
    *Free++ = The_Future;
    *Free++ = NIL;
    Vector_Set(The_Queue, CONS_CDR, New_Entry);
    if (Queue_Tail==NIL)
      Vector_Set(The_Queue, CONS_CAR, New_Entry);
    else
      Vector_Set(Queue_Tail, CONS_CDR, New_Entry); }
#endif
}

Pointer Make_A_Future(Process, Doc, Func)
Pointer Process, Doc, Func;
{ Pointer The_Future, Parent_Future, Spawn_Cons, Old_Counter, Parent_Id;
  Pointer IO_Vector, IO_Cons, IO_Hunk3, Empty_Queue, IO_String;
  extern Pointer Get_New_Future_Number();
 
  Empty_Queue=Make_Pointer(TC_LIST,Free);
  *Free++=NIL;
  *Free++=NIL;

  The_Future=Make_Pointer(TC_FUTURE,Free);
  *Free++=Make_Non_Pointer(TC_MANIFEST_VECTOR, FUTURE_SIZE);
  *Free++=NIL;			   /* 1 - No value yet. */
  *Free++=NIL;			   /* 2 - Not locked. */
  *Free++=Empty_Queue;		   /* 3 - Put the empty queue here. */
  *Free++=Process;		   /* 4 - The process slot. */
  *Free++=Get_Status_Symbol(SYM_CREATED); /* 5 - Status slot. */
  *Free++=Func;			   /* 6 - Original code. */
  *Free++=Doc;			   /* 7 - Put the I/O system stuff here. */
  *Free++=NIL;			   /* 8 - Waiting on list. */
  *Free++=Get_New_Future_Number(); /* 9 - Metering number. */
  *Free++=Make_Unsigned_Fixnum(0); /* 10 - Future spawning count. */
  *Free++=NIL;			   /* 11 - Position in spawning graph. */
  *Free++=NIL;			   /* 12 - User data slot */
  *Free++=C_Integer_To_Scheme_Integer(0); /* 13 - future's priority */

  Parent_Future = Get_Current_Future();
  if (Type_Code(Parent_Future) == TC_FUTURE) {
    Old_Counter=Fast_Vector_Ref(Parent_Future, FUTURE_COUNTER);
    Fast_Vector_Set(Parent_Future, FUTURE_COUNTER,
		    Make_Unsigned_Fixnum(Get_Integer(Old_Counter) + 1));
    Parent_Id = Fast_Vector_Ref(Parent_Future, FUTURE_SPAWNID); }
  else {
    Old_Counter = Fast_Vector_Ref(The_Future, FUTURE_METERING);
    Parent_Id = NIL; }

  Spawn_Cons = Make_Pointer(TC_LIST, Free);
  *Free++=Old_Counter;
  *Free++=Parent_Id;
  Fast_Vector_Set(The_Future, FUTURE_SPAWNID, Spawn_Cons);

  return The_Future; }

/*
  (MAKE-CHEAP-FUTURE CODE DEBUG-CODE DOC)

  - CODE	The actual item to run - usually a control point.
  - DEBUG-CODE	The thunk embodied in CODE.
  - DOC		The documentation string.
*/

Define_Primitive(Prim_Make_Cheap_Future, 3, "MAKE-CHEAP-FUTURE")
{ 
  Primitive_3_Args();

  Primitive_GC_If_Needed(15 + FUTURE_SIZE);
  return Make_A_Future(Arg1, Arg3 ,Arg2); }

/* %MAKE-INITIAL-PROCESS is called to create a continuation 
   which calls the specified thunk having first restored the fluid
   bindings and dynamic state.

   NOTE:: We must add RC_RESTORE_STATE_POINT to returns.h and the code
   to do the requisite restore to interpret.c.

*/
Pointer Make_Initial_Process(Thunk)
Pointer Thunk;
{ Pointer Continue;
  long Useful_Length;

  Continue = Make_Pointer(TC_CONTROL_POINT, Free);

				/* Three continuations, one extra slot and a call frame. */
  Useful_Length = 3*CONTINUATION_SIZE+1+STACK_ENV_EXTRA_SLOTS+1;
  Free[STACKLET_LENGTH] =
    Make_Non_Pointer(TC_MANIFEST_VECTOR,
	     	     Useful_Length + STACKLET_HEADER_SIZE - 1);
  Free[STACKLET_REUSE_FLAG] = TRUTH;
  Free[STACKLET_UNUSED_LENGTH] =
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0);
  Free += STACKLET_HEADER_SIZE;

  Free[CONTINUATION_RETURN_CODE] = 
    Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_PROCESS_STATE);
  Free[CONTINUATION_EXPRESSION] = Current_State_Point;;
  Free += CONTINUATION_SIZE;
  *Free++ = Fluid_Bindings;

  Free[CONTINUATION_RETURN_CODE] = 
    Make_Non_Pointer(TC_RETURN_CODE, RC_INTERNAL_APPLY);
  Free[CONTINUATION_EXPRESSION] = NIL;
  Free += CONTINUATION_SIZE;

  *Free++ = STACK_FRAME_HEADER;
  *Free++ = Thunk;

  Free[CONTINUATION_RETURN_CODE] = 
    Make_Non_Pointer(TC_RETURN_CODE, RC_DETERMINE_SELF);
  Free[CONTINUATION_EXPRESSION] = Thunk;	/* For testing & debugging */
  Free += CONTINUATION_SIZE;

  return Continue; }

/*
  (%DETERMINE! FUTURE VALUE KEEP?)
*/
Define_Primitive(Prim_Determine, 3, "%DETERMINE!")
{ Primitive_3_Args();
  
  Determine_The_Future(Arg1, Arg2, Arg3);
  return Arg2; 
}

Do_Determine_Self(The_Value)
Pointer The_Value;
{ Pointer Self;

  Determine_The_Future(Get_Current_Future(), The_Value, NIL); 
}

Determine_The_Future(The_Future, The_Value, Keep_Slot)
Pointer The_Future, The_Value, Keep_Slot;
{ Pointer Waiters, Known;
  long Saved_Zone;

  Save_Time_Zone(Zone_Scheduler);

  if (Lock_The_Future(The_Future)) {
    Known = Fast_Vector_Ref(The_Future, FUTURE_IS_DETERMINED);
    if (Known == TRUTH) {
      Unlock_The_Future(The_Future);
      Restore_Time_Zone();
      Primitive_Error(ERR_ARG_1_WRONG_TYPE); }
    Waiters = Fast_Vector_Ref(The_Future, FUTURE_QUEUE);
    Fast_Vector_Set(The_Future, FUTURE_VALUE, The_Value);
    Fast_Vector_Set(The_Future, FUTURE_STATUS, Get_Status_Symbol(SYM_DETERMINED));
    if (Keep_Slot == NIL) {
      if (Known == NIL)
	Fast_Vector_Set(The_Future, FUTURE_IS_DETERMINED, TRUTH); }
    else
      Fast_Vector_Set(The_Future, FUTURE_IS_DETERMINED, Keep_Slot);
    Clear_Int_Timer();
    Unlock_The_Future(The_Future);
    if (Known == NIL) Wake_Up_List(Fast_Vector_Ref(Waiters, CONS_CAR),
				   The_Future); }
  else Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  Restore_Time_Zone(); }

#define PRIORITY_VERY_LOW		-2000	/* Priority for speculative rivals */

Wake_Up_List(Future_List, Waker)
Pointer Future_List, Waker;
{ Pointer This, Task, Waitin_on, Waitin, Inherit_Decreasing(), Priority;
  Boolean Found_Self;
  C_Integer_To_Scheme_Integer(PRIORITY_VERY_LOW, &Priority);
  for (This=Future_List; This!=NIL; This=Fast_Vector_Ref(This, CONS_CDR))
  { Found_Self = false;
    Task = Fast_Vector_Ref(This, CONS_CAR);
    if (Lock_The_Future(Task))
    { if (Fast_Vector_Ref(Task, FUTURE_STATUS) == Get_Status_Symbol(SYM_WAITING))
      { for (Waitin = Waitin_on = Fast_Vector_Ref(Task, FUTURE_WAITING_ON); 
	     Waitin_on != NIL;
	     Waitin_on = Fast_Vector_Ref(Waitin_on, CONS_CDR))
	{ if (Fast_Vector_Ref(Waitin_on, CONS_CAR) == Waker)
	  { Fast_Vector_Set(Task, FUTURE_WAITING_ON, Waker);
	    Fast_Vector_Set(Task, FUTURE_STATUS, Get_Status_Symbol(SYM_RUNNABLE));
	    Found_Self = true;
	    break;
	  }
	}
	Unlock_The_Future(Task);
	if (Found_Self)
	{ Enqueue_The_Task(Task);
#if 0
	  for (; Waitin != NIL; Waitin = Fast_Vector_Ref(Waitin, CONS_CDR))
	  { long Propority;
	    Pointer Customer = Fast_Vector_Ref(Waitin, CONS_CAR);
	    if (Customer != Waker)
	    { /* Task used to be waiting on someone else in addition to me; demote my rival */
	      Lock_The_Future(Customer);
	      Scheme_Integer_To_C_Integer(Fast_Vector_Ref(Customer, FUTURE_PRIORITY), &Propority);
	      Inherit_Decreasing(Customer, PRIORITY_VERY_LOW, Priority, Propority);
	    }
	  } /* for (; Waitin ...) */
#endif
	} /* if (Found_Self) */
      }
      else /* Task wasn't waiting */ Unlock_The_Future(Task);
    } /* if (Lock_The_Future(Task)) */
  } /* for (This-Future_List ... */
}

/*
  (%AWAIT-FUTURE FUTURE)
  This has the current future wait for the specified future.
  It builds its own continuation.
*/

Define_Primitive(Prim_Await_Future, 1, "%AWAIT-FUTURE")
{ Pointer Save_Current_State(), State, Current, Inherit_Increasing();
  long Saved_Zone;
  short int the_priority;
  long The_Priority, Thro_away;
  Primitive_1_Args();
  
  Save_Time_Zone(Zone_Scheduler);
  Primitive_GC_If_Needed((Stack_Top - Stack_Pointer) - 1 +
			 CONTINUATION_SIZE + 1 + STACKLET_HEADER_SIZE);
  if (Lock_The_Future(Arg1)) {
    if (Fast_Vector_Ref(Arg1, FUTURE_IS_DETERMINED) != NIL) {
      Unlock_The_Future(Arg1);
      Restore_Time_Zone();
      return Vector_Ref(Arg1, FUTURE_VALUE); }

    Current = Get_Current_Future();

    if (Fast_Vector_Ref(Arg1, FUTURE_STATUS) == Get_Status_Symbol(SYM_DELAYED) ||
	Fast_Vector_Ref(Arg1, FUTURE_STATUS) == Get_Status_Symbol(SYM_PAUSED))
      Enqueue_The_Task(Arg1);

    if (Lock_The_Future(Current)) {
      Pop();			/* Throw away the argument. */
      Fast_Vector_Set(Current, FUTURE_PROCESS, Save_Current_State());
      Fast_Vector_Set(Current, FUTURE_STATUS, Get_Status_Symbol(SYM_WAITING));
      Fast_Vector_Set(Current, FUTURE_WAITING_ON, Make_Pointer(TC_LIST, Free));
      *Free++ = Arg1;
      *Free++ = NIL;
      Stuff_Onto_Queue(Fast_Vector_Ref(Arg1, FUTURE_QUEUE), Current);

      Scheme_Integer_To_C_Integer(Fast_Vector_Ref(Current, FUTURE_PRIORITY), &The_Priority);
      Scheme_Integer_To_C_Integer(Fast_Vector_Ref(Arg1, FUTURE_PRIORITY), &Thro_away);
      if(The_Priority > Thro_away){
	Unlock_The_Future(Current);
	the_priority = The_Priority;
	Inherit_Increasing(Arg1, The_Priority, C_Integer_To_Scheme_Integer(the_priority), Thro_away);
      }
      else {
	Unlock_The_Future(Current);
	Unlock_The_Future(Arg1);
      }

      Rescheduler_Common(); }
    else {
      Unlock_The_Future(Arg1);
      printf("%AWAIT_FUTURE: Existential crisis %x\n", Current); } }
  else {
    Restore_Time_Zone();
    return Arg1; } }

Stuff_Onto_Queue(Queue, What)
Pointer Queue, What;
{ Pointer Queue_Tail, New_Entry;

  Queue_Tail = Fast_Vector_Ref(Queue, CONS_CDR);
  New_Entry = Make_Pointer(TC_WEAK_CONS, Free);
  *Free++ = What;
  *Free++ = NIL;
  Fast_Vector_Set(Queue, CONS_CDR, New_Entry);
  if (Queue_Tail==NIL)
    Fast_Vector_Set(Queue, CONS_CAR, New_Entry);
  else
    Fast_Vector_Set(Queue_Tail, CONS_CDR, New_Entry); }

/*
  (Change-Priority Future Priority)

  what about inheritance?
*/

Define_Primitive(Prim_Change_Priority, 2, "%CHANGE-PRIORITY")
{
  Primitive_2_Args();
  FIXNUM_ARG(2);

  Fast_Vector_Set(Arg1, FUTURE_PRIORITY, Arg2);
  Unlock_The_Future(Arg1);
  return TRUTH;
}


Define_Primitive(Prim_Inherit_Up, 2, "%INHERIT-PRIORITY-UP")
{
  Primitive_2_Args();
  FIXNUM_ARG(2);

  printf("This is not supported yet");
  return NIL;
}


Define_Primitive(Prim_Inherit_Down, 2, "%INHERIT-PRIORITY-DOWN")
{
  long New, Current;
  Pointer Inherit_Increasing(), Inherit_Decreasing();
  int the_priority;
  Primitive_2_Args();
  FIXNUM_ARG(2);
  
  Scheme_Integer_To_C_Integer(Arg2, &New);
  Scheme_Integer_To_C_Integer(Fast_Vector_Ref(Arg1, FUTURE_PRIORITY), &Current);
  printf("In Inherit-Priority-Down New (%d) [=, >, <] Current (%d)\n", New, Current);
  if(New == Current){
    Unlock_The_Future(Arg1);
    return NIL;
  }
  else if(New > Current)
    return(Inherit_Increasing(Arg1, New, Arg2, Current));
  else {
    return(Inherit_Decreasing(Arg1, New, Arg2, Current));
  }
}

Pointer
Inherit_Increasing(futu, C_new, S_new, Old)
Pointer futu, S_new;
long C_new, Old;
{
  Pointer *Continue_Increasing();
  long Current; 
  Scheme_Integer_To_C_Integer(Fast_Vector_Ref(futu, FUTURE_PRIORITY), &Current);
  printf("In Inherit_Increasing Old (%d) [=, >, <] Current (%d)\n", Old, Current);
  if(Old == Current){
    Fast_Vector_Set(futu, FUTURE_PRIORITY, S_new);
    return(C_To_Scheme(Make_Pointer(Continue_Increasing(futu, C_new, S_new, Current), TC_WEAK_CONS)));
  }
  else if(Old > Current){
    Unlock_The_Future(futu);
    return(futu);
  }
  else {
    printf("New (%d) [= , <, >] Current (%d)\n", C_new, Current);
    if(C_new <= Current){
      Unlock_The_Future(futu);
      return NIL;
    }
    else {
      Fast_Vector_Set(futu, FUTURE_PRIORITY, S_new);
      return(C_To_Scheme(Make_Pointer(Continue_Increasing(futu, C_new, S_new, Current), TC_WEAK_CONS)));
    }
  }
}

Pointer
*Continue_Increasing(futu, C_new, S_new, Old)
Pointer futu, S_new;
long C_new, Old;
{
  Pointer Waiting_on, new_futu, *List, *Append_List(), Inherit_Increasing(); 
  List = NIL;
  printf("In Continue Increasing\n");
  if((Waiting_on = Fast_Vector_Ref(futu, FUTURE_WAITING_ON)) == NIL){
    Unlock_The_Future(futu);
    return NIL;
  }
  else {
    Unlock_The_Future(futu);
    for(; Waiting_on != NIL; Waiting_on = Fast_Vector_Ref(Waiting_on, CONS_CDR)){
      Lock_The_Future(new_futu = Fast_Vector_Ref(Waiting_on, CONS_CAR));
      printf("About to call Append_List with new_futu, C_new (%d), S_new, Old (%d)\n",C_new, Old);
      Append_List(List, Inherit_Increasing(new_futu, C_new, S_new, Old));
    }
    return(List);
  }
}

Pointer
*Append_List(List, Answer)
Pointer *List, Answer;
{
  Pointer *Where;
  if(Answer == NIL)
    return(List);
  else {
    Where = Free;
    Primitive_GC_If_Needed(2);
    *Free++ = Answer;
    if (List == NIL){
      *Free++ = NIL;
      return(Where);
    }
    else {
      *Free++ = Make_Pointer(TC_WEAK_CONS, List);
      return(Where);
    }
  }
}

#define OBSCENE_LOW_PRIORITY -5000
long 
Get_Max_Wait_Priority(future,prio)
Pointer future;
long prio;
{
  long Cur, Max;
  Pointer This, Task, Waitin;
  Boolean Found_Self;
  Max = OBSCENE_LOW_PRIORITY;
  printf("In Get Max prio = %d\n",prio);
  for(This = Fast_Vector_Ref(Fast_Vector_Ref(future, FUTURE_QUEUE), CONS_CAR);
      This != NIL;
      This = Fast_Vector_Ref(This, CONS_CDR)){
    Found_Self = false;
    Task = Fast_Vector_Ref(This, CONS_CAR);
    for (Waitin = Fast_Vector_Ref(Task, FUTURE_WAITING_ON); Waitin != NIL; Waitin= Fast_Vector_Ref(Waitin, CONS_CDR)){
      if (Fast_Vector_Ref(Waitin, CONS_CAR) == future){
	Found_Self = true;
	break;
      }
    }
    if (Found_Self){ 
      Scheme_Integer_To_C_Integer(Fast_Vector_Ref(Task, FUTURE_PRIORITY), &Cur);
      printf("In Get Max Cur (%d) [=, !=] prio (%d)\n",Cur, prio);
      if (Cur > Max)
	Max = Cur;
    } 
  }
  printf("In Get Max and Max is %d\n",Max);
  return(Max);
}

Pointer
Inherit_Decreasing(futu, C_new, S_new, Old)
Pointer futu, S_new;
long C_new, Old;
{
  Pointer *Continue_Decreasing();
  long Current, Maximum, Get_Max_Wait_Priority(); 
  int the_priority;
  Scheme_Integer_To_C_Integer(Fast_Vector_Ref(futu, FUTURE_PRIORITY), &Current);
  printf("In Inherit_Decreasing Old (%d) [=, >, <] Current (%d)\n", Old, Current);
  if(Old == Current){
    Maximum = Get_Max_Wait_Priority(futu, Current);
    if (Maximum > Current){
      printf("In Inherit Decreasing Maximum (%d) > Current (%d)\n",Maximum, Current);
      Unlock_The_Future(futu);
      return(futu);
    }
    else if(Maximum == Current){
      Unlock_The_Future(futu);
      return NIL;
    }
    else {
      if (C_new > Maximum)
	Maximum = C_new; 
      the_priority = Maximum;     /*truncate*/
      printf("In Inherit Decreasing the priotiry (%d) = Maximum (%d)\n",the_priority, Maximum);
      Fast_Vector_Set(futu, FUTURE_PRIORITY, C_Integer_To_Scheme_Integer(the_priority));
      return(C_To_Scheme(Make_Pointer(Continue_Decreasing(futu, C_new, S_new, Current), TC_WEAK_CONS)));
    }
  }
  else if(Old > Current){
    Unlock_The_Future(futu);
    return(futu);
  }
  else {
    Unlock_The_Future(futu);
    return NIL;
  } 
}

Pointer
*Continue_Decreasing(futu, C_new, S_new, Old)
Pointer futu, S_new;
long C_new, Old;
{
  Pointer Waiting_on, new_futu, *List, *Append_List(); 
  List = NIL;
  printf("In Continue Decreasing\n");
  if((Waiting_on = Fast_Vector_Ref(futu, FUTURE_WAITING_ON)) == NIL){
    Unlock_The_Future(futu);
    return NIL;
  }
  else {
    Unlock_The_Future(futu);
    for(; Waiting_on != NIL; Waiting_on = Fast_Vector_Ref(Waiting_on, CONS_CDR)){
      Lock_The_Future(new_futu = Fast_Vector_Ref(Waiting_on, CONS_CAR));
      printf("About to call Append_List with new_futu, C_new (%d), S_new, Old (%d)\n",C_new, Old);
      Append_List(List, Inherit_Decreasing(new_futu, C_new, S_new, Old));
    }
    return(List);
  }
}


/*
  Make a continuation/control point object which contains the current stack
  and a return code which restores the Current_State_Point and Fluid_Bindings.

  When this returns, Stack_Pointer is back at Stack_Top.

  WARNING: This assumes that there is enough room in the heap.  The caller
  	must do the Primitive_GC_If_Needed!
*/
Pointer Save_Current_State()
{ Pointer Result;
  fast long NCells, i;
  
  NCells = (Stack_Top - Stack_Pointer) + (CONTINUATION_SIZE + 1);

  Result = Make_Pointer(TC_CONTROL_POINT, Free);
  Free[STACKLET_LENGTH] =
    Make_Non_Pointer(TC_MANIFEST_VECTOR,
		     NCells + STACKLET_HEADER_SIZE - 1);
  Free[STACKLET_REUSE_FLAG] = TRUTH;
  Free[STACKLET_UNUSED_LENGTH] =
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0);
  Free += STACKLET_HEADER_SIZE;

  Free[CONTINUATION_RETURN_CODE] = 
    Make_Non_Pointer(TC_RETURN_CODE, RC_RESTORE_PROCESS_STATE);
  Free[CONTINUATION_EXPRESSION] = Current_State_Point;
  Free += CONTINUATION_SIZE;

  *Free++ = Fluid_Bindings;

  NCells -= (CONTINUATION_SIZE + 1);
  for (i=0; i<NCells; i++)
    *Free++ = *Stack_Pointer++;	/* Stack_Pointer will end at Stack_Top */

  return Result; }

/*
	Get Work from the queue inside a primitive.
*/

#ifdef butterfly
void
Put_Work(The_Future)
Pointer The_Future;
{
  short int the_priority;
  long The_Priority;
  Pointer *Where;
  struct priority_queue *the_right_queue, *Find_Right_Queue();
  int flag;

  if (Debug_Flags[24])
    {
      printf("Process %d Put_Work future %d\n", getpid(),
	     Get_Integer(Fast_Vector_Ref(The_Future, FUTURE_METERING)));
      fflush(stdout);
    }

  /* find the priority of the The_Future we want to put on the queue*/
  flag = (Scheme_Integer_To_C_Integer
	  (Fast_Vector_Ref(The_Future, FUTURE_PRIORITY), &The_Priority));
  if (flag != PRIM_DONE) Primitive_Error(flag);
  the_priority = The_Priority;	/* Truncate it to 16 bits */

  /* lock the work queue*/
  while (atomior(&SHARED_DATA->Work_Queue_Lock, 0x8000) != 0x0000);
  SHARED_DATA->Work_Queue_Length += 1;
   
  /* find the right queue and return it (if it doesn't exist make it) */
  the_right_queue = 
    Find_Right_Queue(the_priority, &(SHARED_DATA->The_New_Improved_Work_Queue));

  /* lock the right queue*/
  while (atomior(&the_right_queue->queue_lock, 0x8000) != 0x0000);
  
  /*unlock the work queue*/
  SHARED_DATA->Work_Queue_Lock = 0x0000;

  /*allocate space and put the future on the right queue*/
  Where = Free;
  Primitive_GC_If_Needed(2);
  *Free++ = The_Future;
  *Free++ = NIL;
  if (the_right_queue->head == NULL)
    the_right_queue->head = the_right_queue->tail = Where;
  else
  { Fast_Vector_Set(C_To_Scheme(the_right_queue->tail), CONS_CDR,
		    Make_Pointer(TC_WEAK_CONS, Where));
    the_right_queue->tail = Where;
  }
  the_right_queue->queue_length += 1;
  the_right_queue->queue_lock = 0x0000;
}

struct priority_queue 
*Find_Right_Queue(the_priority, queue_pointer)
short int the_priority;
struct priority_queue **queue_pointer;
{ 
  struct priority_queue *Allocate_Priority_Queue();
  while (true)
  { if (*queue_pointer == NULL)
      return (Allocate_Priority_Queue(queue_pointer, the_priority));
    else if ((*queue_pointer)->priority < the_priority)
      return (Allocate_Priority_Queue(queue_pointer, the_priority));
    else if ((*queue_pointer)->priority == the_priority)
      return (*queue_pointer);
    queue_pointer = &((*queue_pointer)->next);
  }
}

struct priority_queue
*Allocate_Priority_Queue(queue_pointer, the_priority)
struct priority_queue **queue_pointer;
short int the_priority;
{ struct priority_queue *Where = (struct priority_queue *) Free;
  long Size = ((sizeof(struct priority_queue)-1) / sizeof(Pointer)) + 1;
  Primitive_GC_If_Needed(Size);
  Free += Size;
  Where->queue_lock = 0x0000;
  Where->queue_length = 0x0000;
  Where->priority = the_priority;
  Where->head = Where->tail = NULL;
  Where->next = *queue_pointer;
  *queue_pointer = Where;
  return Where;
}
#endif

#ifdef butterfly
#define N_IDLE_INFO 3
int get_work_idle_loops[] = {1, 10, 100, 1000};
int get_work_idle_counts[] = {1000, 1000, 1000};

Pointer Get_Work(Idle_Handler)
Pointer Idle_Handler;
{ Pointer The_Future;

#ifdef butterfly
  { int iinfo, i, ntimes, count;
    int Saved_Zone;

    Save_Time_Zone(Zone_GetWork);
    OS_start_idle_time();
    atomadd(&SHARED_DATA->Idle_Processor_Count, 1);

    iinfo = 0;
    count = 0;
    while (true) {
      if ((INT_Mask & IntCode) != 0) {
	if (Debug_Flags[9]) {
	  printf("Process %d interrupt in get-work %x %x\n",
		 getpid(), IntEnb, IntCode);
	  fflush(stdout); }
	atomadd(&SHARED_DATA->Idle_Processor_Count, -1);
	Restore_Time_Zone();
	OS_end_idle_time();
	Special_Primitive_Interrupt(INT_Mask); }

      if (Get_From_Work_Queue(&The_Future))
	break;

      if (iinfo < N_IDLE_INFO && /* Are we in the final state? */
	  count == get_work_idle_counts[iinfo]) { /* Are we ready to jump? */
	iinfo++;
	count = 0; }
      count++;
      ntimes = get_work_idle_loops[iinfo];
      for (i = 0; (i < ntimes) && ((IntCode & INT_Mask) == 0); i++)
	Standard_Delay(); }

    atomadd(&SHARED_DATA->Idle_Processor_Count, -1);
    Restore_Time_Zone();
    OS_end_idle_time(); }
#else				/* Just hack data structure in the fixed objects vector */
  { Pointer The_Queue, Queue_Head;

    The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
    if (The_Queue != NIL) {
      Queue_Head = Vector_Ref(The_Queue, CONS_CAR);
      if (Queue_Head != NIL) {
	The_Future = Vector_Ref(Queue_Head, CONS_CAR);
	Queue_Head = Vector_Ref(Queue_Head, CONS_CDR);
	Vector_Set(The_Queue, CONS_CAR, Queue_Head);
	if (Queue_Head == NIL) Vector_Set(The_Queue, CONS_CDR, NIL); } } }
#endif

  if (Debug_Flags[24])
    {
      printf("Process %d Get_Work returning future %d\n", getpid(),
	     Get_Integer(Fast_Vector_Ref(The_Future, FUTURE_METERING)));
      fflush(stdout);
    }

  return The_Future; }

#endif


#ifdef butterfly
Get_From_Work_Queue(value)
Pointer *value;
{
  struct priority_queue *the_right_queue, *Find_For_Get();

  /* lock the work queue*/
  while (atomior(&(SHARED_DATA->Work_Queue_Lock), 0x8000) != 0x0000);
  /* NOTICE: Find_For_Get finds and locks the queue for you */
  if ((the_right_queue = Find_For_Get(SHARED_DATA->The_New_Improved_Work_Queue))==NULL) {
    SHARED_DATA->Work_Queue_Lock = 0x0000;
    return (false);
  }
  SHARED_DATA->Work_Queue_Length -= 1;
  SHARED_DATA->Work_Queue_Lock = 0x0000;
  the_right_queue->queue_length -= 1;
  *value = Fast_Vector_Ref(C_To_Scheme(the_right_queue->head), CONS_CAR);
  { Pointer New_Head =
      Fast_Vector_Ref(C_To_Scheme(the_right_queue->head), CONS_CDR);
    if (New_Head == NIL)
      the_right_queue->head = NULL;
    else the_right_queue->head = Get_Pointer(New_Head);
  }
  the_right_queue->queue_lock = 0x0000;
  return (true);
}

struct priority_queue
*Find_For_Get(queue_pointer)
struct priority_queue *queue_pointer;
{
  while (queue_pointer != NULL){ 
    /* lock the right queue*/
    while (atomior(&queue_pointer->queue_lock, 0x8000) != 0x0000);
    if (queue_pointer->head != NULL)
      return (queue_pointer);
    else{
      fast struct priority_queue *Next=queue_pointer->next;
      queue_pointer->queue_lock = 0x0000;
      queue_pointer = Next;
    }
  }
}

#endif


/*
  (%RESCHEDULE)

  Wait for something good to come off the queue and prepare to run it.
*/
Define_Primitive(Prim_Scheduler, 0, "%RESCHEDULE")
{ Rescheduler_Common(); }

Rescheduler_Common()
{ Pointer The_Future, Idle_Handler;
  long Saved_Zone;

  Initialize_Stack();		/* Reset the stack! */
 Will_Push(CONTINUATION_SIZE);
  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();
 Pushed();
				/* Arrange things so we restart w/%reschedule. */
  Store_Expression(Get_Fixed_Obj_Slot(Default_Rescheduler));

#ifdef HANDLE_IDLE_FUTURES
  Idle_Handler = Get_Fixed_Obj_Slot(Idle_Rescheduler);
#else
  Idle_Handler = NIL;
#endif

  Clear_Int_Timer();
  Fast_Vector_Set(Get_Fixed_Obj_Slot(Future_Vector), Who_Am_I + 1,
		  Get_Status_Symbol(SYM_WORK_WAIT));

 Try_Another:
  The_Future = Get_Work(NIL);

  /* ... Now we run The_Future ... */

  if (The_Future == NIL) 
  {
#ifdef butterfly
printf("\nI thought on the Butterfly Get_Work hangs until work is available!\n  --Jim Miller\n");
#endif
    if (Idle_Handler == NIL) 
    { printf("\nNo work available, but some has been requested!\n");
      Primitive_Error(ERR_BAD_SET);
    }
    Will_Push(STACK_ENV_EXTRA_SLOTS);
    Push(Idle_Handler);
    Push(STACK_FRAME_HEADER);
    Pushed();
    longjmp(*Back_To_Eval, PRIM_APPLY);
  }
  Run_A_Future(The_Future, false);
  goto Try_Another;
}

Run_A_Future(The_Future, force)
Pointer The_Future;
Boolean force;
{ Pointer The_Work, Old_Status;

  if (Debug_Flags[24])
    {
      printf("Process %d Run_A_Future The_Future %d force %c\n",
	     getpid(), Get_Integer(Fast_Vector_Ref(The_Future, FUTURE_METERING)),
	     force);
      fflush(stdout);
    }

  if (Lock_The_Future(The_Future))
  { The_Work = Fast_Vector_Ref(The_Future, FUTURE_PROCESS);
    Old_Status = Fast_Vector_Ref(The_Future, FUTURE_STATUS);
    if (force || Old_Status == Get_Status_Symbol(SYM_RUNNABLE))
    { Fast_Vector_Set(The_Future, FUTURE_STATUS, Get_Status_Symbol(SYM_RUNNING));
      Fast_Vector_Set(The_Future, FUTURE_PROCESS, Make_Unsigned_Fixnum( Who_Am_I));
      Fast_Vector_Set(Get_Fixed_Obj_Slot(Future_Vector), Who_Am_I + 1, The_Future);
      Unlock_The_Future(The_Future);
    }
    else
    { 
      if (Debug_Flags[24])
	{
	  printf("Process %d Run_A_Future The_Future %d not runnable\n",
		 getpid(), Get_Integer(Fast_Vector_Ref(The_Future, FUTURE_METERING)));
	  fflush(stdout);
	}
      Unlock_The_Future(The_Future);
      return;
    }
  }     

  if (Have_Scheduler_Info() &&
      Type_Code(Get_Scheduler_Info(SCHINFO_DELTA)) == TC_FIXNUM)
  { long delta;
    Sign_Extend(Get_Scheduler_Info(SCHINFO_DELTA), delta);
    Set_Int_Timer(0, delta); 
  }
  IntEnb = INT_Mask;

 Will_Push(STACK_ENV_EXTRA_SLOTS + STACK_ENV_EXTRA_SLOTS + 1);
  Push(Get_Fixed_Obj_Slot(Default_Rescheduler));
  Push(STACK_FRAME_HEADER);
  Push(Get_Status_Symbol(SYM_YOUR_TURN));
  Push(The_Work);
  Push(STACK_FRAME_HEADER+1);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* (CURRENT-FUTURE)				returns the current future
   (SET-CURRENT-FUTURE! FUTURE) 		sets the current future
*/

Define_Primitive(Prim_Current_Future, 0, "CURRENT-FUTURE")
{ return Get_Current_Future(); }

Define_Primitive(Prim_Set_Current_Future, 1, "SET-CURRENT-FUTURE!")
{ Pointer Vector, Result;
  Primitive_1_Arg();
  Vector = (Pointer) Get_Fixed_Obj_Slot(Future_Vector);
  Result = User_Vector_Ref(Vector, Who_Am_I);
  User_Vector_Set(Vector, Who_Am_I, Arg1);
  return Result; }

/*
  Subroutines for locking and unlocking futures.
*/

#ifdef butterfly

#if (TC_TRUE != 0x08) || (NIL != 0)
#include "The special lock future hack will no longer work!" 
#endif

Lock_The_Future(The_Future)
Pointer The_Future;
{ if (Type_Code(The_Future) != TC_FUTURE) return false;
  while (true) {		/*  while ((IntEnb & IntCode) == 0) { */
    if (atomior(Nth_Vector_Loc(The_Future, FUTURE_LOCK), 0x0800) == 0x0000)
      return true;
    else Standard_Delay(); }
  Primitive_Interrupt(); }

Unlock_The_Future(The_Future)
Pointer The_Future;
{ if (Type_Code(The_Future) != TC_FUTURE) return false;
  if (!Future_Is_Locked(The_Future))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  else
    *((short *) Nth_Vector_Loc(The_Future, FUTURE_LOCK)) = 0x0000;
  return true; }

#else

Lock_The_Future(The_Future)
Pointer The_Future;
{ if (Type_Code(The_Future) != TC_FUTURE) return false;
  while ((IntEnb & IntCode) == 0) {
    if (Swap_Pointers(Nth_Vector_Loc(The_Future, FUTURE_LOCK), TRUTH) == NIL)
      return true;
    else Standard_Delay(); }
  Primitive_Interrupt(); }

Unlock_The_Future(The_Future)
Pointer The_Future;
{ if (Type_Code(The_Future) != TC_FUTURE) return false;
  if (!Future_Is_Locked(The_Future))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  else
    Vector_Set(The_Future, FUTURE_LOCK, NIL);
  return true; }
#endif

/*
  The following is called by MAKE-CHEAP-FUTURE on the Butterfly to assign a
  unique future number.
*/

#ifdef butterfly
Pointer Get_New_Future_Number()
{ short int old_lock;
  Pointer Result;

  while(true) {
    old_lock=atomior(&SHARED_DATA->Future_Meter_Lock, 0x8000);
    if (old_lock==0) break; }

  SHARED_DATA->Future_Meter_Number++;
  SHARED_DATA->Future_Meter_Number =
    SHARED_DATA->Future_Meter_Number & 0x007fffff;
  
  Result=Make_Unsigned_Fixnum(SHARED_DATA->Future_Meter_Number);

  SHARED_DATA->Future_Meter_Lock=0;
  return Result; }
#else
Pointer Get_New_Future_Number()
{ static long future_number = 0;

 return Make_Unsigned_Fixnum(future_number++); }
#endif
