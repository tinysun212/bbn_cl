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
#include "zones.h"
#include "locks.h"
#include "cmp68kgc.h"

extern int Debug_Flags[];

DEFINE_PRIMITIVE("SET-QUEUE-OVERFLOW-LEVEL!", Prim_Set_Queue_Level, 1)
{
  long new_value, old_value, max_value;
  Primitive_1_Arg();

  if (Arg1 == NIL)
    PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM,
				      SHARED_DATA->Queue_Overflow));

  Arg_1_Type(TC_FIXNUM);
  
  max_value = min(SHARED_DATA->Work_Queue.queue_max - (2 * N_Interps),
		  SHARED_DATA->GC_Reserve / 2 - 1000);
  Range_Check(new_value, Arg1, 0, max_value, ERR_ARG_1_BAD_RANGE);
  old_value = SHARED_DATA->Queue_Overflow;
  SHARED_DATA->Queue_Overflow = new_value;
  PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM, old_value));
}

DEFINE_PRIMITIVE("SET-IDLE-LIMIT!", Prim_Set_Idle_Limit, 1)
{
  long New_Value, Old_Value;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(New_Value, Arg1, 0, N_Interps-1, ERR_ARG_1_BAD_RANGE);
  Old_Value = SHARED_DATA->Idle_Processor_Level;
  SHARED_DATA->Idle_Processor_Level = New_Value;
  PRIMITIVE_RETURN(Make_Unsigned_Fixnum(Old_Value));
}

/*
	Get and put work primitives.
*/

DEFINE_PRIMITIVE("GET-WORK", Prim_Get_Work, 1)
{
  Primitive_1_Arg();

  PRIMITIVE_RETURN(Get_Work(Arg1));
}

DEFINE_PRIMITIVE("PUT-WORK", Prim_Put_Work, 1)
{
  Primitive_1_Arg();
  Put_Work(Arg1);
  PRIMITIVE_RETURN(TRUTH);
}

Pointer get_place_in_code (code)
Pointer code;
{
  fast int Type;
  Pointer *Block, *Name, *Where, Result;

  Where = (Pointer *) (Get_Integer(code));
  if (Debug_Flags[22])
    printf("GET-PLACE-IN-CODE %x: ", Where);

  for (Block = SHARED_DATA->Code_Base;
       Block < SHARED_DATA->Code_Free;
       Block += Get_Integer(*Block) + 1)
  {
    Type = OBJECT_TYPE(*Block);
    if (Type != TC_MANIFEST_VECTOR
	&& Type != TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf("(on %d/%d) Incorrectly formatted code space %x at %x\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     *Block, Block);
      while (true)
	Standard_Delay();
    }
    Name = Block + Get_Integer(*Block) - 1;
    if (Debug_Flags[22])
      printf("%x-%x ", Block, Name);
    if (Where >= Block && Where <= Name)
    {
      Result = Make_Pointer(TC_LIST, Free);
      *Free++ = *Name;
      *Free++ = Make_Non_Pointer(TC_FIXNUM, (((long) Where) - ((long) Block)));
      if (Debug_Flags[22])
	printf("Name=%x\n", Name);
      return (Result);
    }
  }
  if (Debug_Flags[22])
    printf("Failed\n");
  return (NIL);
}

DEFINE_PRIMITIVE("GET-PLACE-IN-CODE", Prim_Get_Place_In_Code, 1)
{
  Primitive_1_Arg();

  Primitive_GC_If_Needed(2);

  PRIMITIVE_RETURN (get_place_in_code (Arg1));
}

DEFINE_PRIMITIVE("DRAIN-WORK-QUEUE!", Drain_Work_Queue, 0)
{
  Pointer Result = NIL;
  long Count;
  PRIMITIVE_HEADER(0);

#ifndef butterfly
  Count = 0;
  while (Get_From_Queue(&SHARED_DATA->Work_Queue, Free))
  { Count++;
    Free++;
    *Free++ = Result;
    Result = Make_Pointer(TC_WEAK_CONS, Free-2);
  }
  SHARED_DATA->The_New_Improved_Work_Queue = NULL;
  SHARED_DATA->Work_Queue_Lock = 0x0000;
  SHARED_DATA->Work_Queue_Length = 0;
#else
  /* lock the work queue*/
  while (atomior(&SHARED_DATA->Work_Queue_Lock, 0x8000) != 0x0000);
  { struct priority_queue *queue_pointer =
      SHARED_DATA->The_New_Improved_Work_Queue;
    SHARED_DATA->The_New_Improved_Work_Queue = NULL;
    SHARED_DATA->Work_Queue_Length = 0x0000;
    SHARED_DATA->Work_Queue_Lock = 0x0000;
    /* Returns answer in REVERSE priority order */
    while (queue_pointer != NULL)
    { if (queue_pointer->head != NULL) {
        Vector_Set((Pointer) queue_pointer->tail, CONS_CDR, Result);
	Result = Make_Pointer(TC_WEAK_CONS, queue_pointer->head);
      }
      queue_pointer = queue_pointer->next;
    }
  }
#endif
  if GC_Check(0)
    Request_GC(0);
  PRIMITIVE_RETURN(Result);
}

#ifdef butterfly

/* The NEW synchronizer is a special cons with a magic symbol in its car
   and the count in its cdr.  Only the new waiter is written in C.
   NEW synchronizers can be backed out of when global interrupts come in!

   NOTE: We can assume that count_pointer doesn't move since a GC
   requires our cooperation which will cause us to back out.

   NOTE: There is no lock on Awake_Others since if someone is calling
   that from here then they are assured that ALL the other processors
   are already waiting in this mess.

   EXPLANATION: The counter has a 1 bit lock (0x8000) and a 14 bit
   counter.  Essentially this works by decrementing the counter and
   testing for 0; on 0, you lock the counter and recheck its value
   awakening others if it is still 0 and leaving it unlocked with a
   special flag (0x7FFF) to show that the sync is complete.  In case
   of interrupt you lock the counter, see if it has gone to 0 or
   0x7FFF -- in the former case you unlock and wait to be told to
   proceed; in the latter you exit saying the sync is complete;
   alternatively you execute the interrupt.
*/

DEFINE_PRIMITIVE("AWAIT-SYNCHRONY", Prim_Await_Sync, 1)
{
  short *count_pointer, old_count, old_bits;
  Pointer Data;
  Primitive_1_Arg();

  Arg_1_Type(TC_LIST);
  if (OBJECT_TYPE(Vector_Ref(Arg1, CONS_CDR)) != TC_FIXNUM)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);

  count_pointer = ((short *) Nth_Vector_Loc(Arg1, CONS_CDR)) + 1;
	/* CROCK -- we know that low order part is at +1 */
  old_count = atomadd(count_pointer, -1);
  if ((old_count & 0x7fff) == 1)
  {
    while (true)
    {
      old_bits = atomior(count_pointer, 0x8000);
      if ((old_bits & 0x8000) == 0)
	break;
    }

    if (old_bits == 0)
    {
      *count_pointer = 0xFFFF;
      *count_pointer = 0x7FFF;
      PRIMITIVE_RETURN(TRUTH);
    }
    
    atomand(count_pointer, 0x7FFF); /* Unlock */
  }

  while (true)
  {
    if ((IntEnb & IntCode) != 0)
    {
      while (true)
      {
	old_bits = atomior(count_pointer, 0x8000);
        if ((old_bits & 0x8000) == 0)
	  break;
      }

      if (old_bits == 0x7FFF)
      {
	*count_pointer = 0x7FFF;
        PRIMITIVE_RETURN(NIL);
      }

      if (old_bits != 0)
      {
	atomadd(count_pointer, 1);
        atomand(count_pointer, 0x7FFF);
	if (Debug_Flags[9])
	  {
	    printf ("Process %d interrupted in await-sync\n", getpid());
	  }
        Primitive_Interrupt();
      }

      atomand(count_pointer, 0x7FFF);
    }

    if ((*count_pointer & 0x7FFF) == 0x7FFF)
      PRIMITIVE_RETURN(NIL);
    Standard_Delay();
  }
}

/* Primitives to get various microcode information */

DEFINE_PRIMITIVE("N-INTERPRETERS", Prim_N_Interps, 0)
{
  PRIMITIVE_HEADER(0);

  PRIMITIVE_RETURN(Make_Unsigned_Fixnum(N_Interps));
}

DEFINE_PRIMITIVE("MY-INTERPRETER-NUMBER", Prim_My_Interp_Number, 0)
{
  PRIMITIVE_HEADER(0);
  
  PRIMITIVE_RETURN(Make_Unsigned_Fixnum(Who_Am_I));
}

DEFINE_PRIMITIVE("WORK-QUEUE-LENGTH", Prim_work_queue_length, 0)
{ PRIMITIVE_HEADER(0);
#ifndef butterfly
  PRIMITIVE_RETURN((Make_Unsigned_Fixnum(FQ_Nitems(SHARED_DATA->Work_Queue)));
#else
  PRIMITIVE_RETURN(C_Integer_To_Scheme_Integer(SHARED_DATA->Work_Queue_Length));
#endif
}

#ifdef butterfly
/* The following primitive DOES NOT give an "atomic" result */
DEFINE_PRIMITIVE("WORK-QUEUE-SIZES", Prim_work_queue_sizes, 1)
{ Pointer Result_Vec;
  long Answer_Length;
  Pointer *Next, *End;
  struct priority_queue *PQ = SHARED_DATA->The_New_Improved_Work_Queue;
  PRIMITIVE_HEADER(1);
  Result_Vec = VECTOR_ARG(1);
  Answer_Length = (Vector_Length(Result_Vec)) & ~1; /* Make it even */
  End = Nth_Vector_Loc(Result_Vec, Answer_Length+1);
  for (Next=Nth_Vector_Loc(Result_Vec, 1); (PQ != NULL) && (Next <= End);)
  { *Next++ = C_Integer_To_Scheme_Integer(PQ->priority);
    *Next++ = C_Integer_To_Scheme_Integer(PQ->queue_length);
    PQ = PQ->next;
  }
  PRIMITIVE_RETURN((PQ==NULL) ? TRUTH : NIL);
}
#endif

DEFINE_PRIMITIVE("N-IDLE-PROCESSORS", Prim_n_idle_procs, 0)
{
  PRIMITIVE_HEADER(0);

  PRIMITIVE_RETURN(Make_Unsigned_Fixnum(SHARED_DATA->Idle_Processor_Count));
}

DEFINE_PRIMITIVE("SYSTEM-ACTIVE?", Prim_system_active, 0)
{
  PRIMITIVE_HEADER(0);

  if (FQ_Nitems(SHARED_DATA->Work_Queue) == 0 &&
      SHARED_DATA->Idle_Processor_Count == (N_Interps - 1) &&
      FQ_Nitems(SHARED_DATA->Work_Queue) == 0 &&
      SHARED_DATA->Idle_Processor_Count == (N_Interps - 1))
    PRIMITIVE_RETURN(TRUTH);
  else
    PRIMITIVE_RETURN(NIL);
}

DEFINE_PRIMITIVE("GET-STATISTICS", Prim_Get_Stats, 0)
{
  PRIMITIVE_HEADER(0);

  PRIMITIVE_RETURN(NIL);
}

/*	November 12, 1985 - sas

	The following routine delays a bit so that the system can use
	spin locks and polling as opposed to having to stop and wait.
	According to Walter Milliken and William Crowther it is
	perfectly reasonable to use spin locks as long as the references
	are spaced out a bit.  Furthermore, a processor executing
	local instructions does not place a burden on processors trying
	to reference this processors local memory.

	The delay gets longer as the number of processors goes up.
	It also wastes a lot more cycles (but not a whole lot more time)
	on the PLUS.
*/

long
  delay_entries = 0,
  delay_exits = 0,
  delay_count;

Standard_Delay()
{
  int i, cache_bache, *bache_cache, counter;

  delay_entries++;
  delay_count = 0;
  bache_cache=(&cache_bache);
  counter = N_Interps * 20;
  for (i = 0; i < counter; i++)
  {
    delay_count++;
    *bache_cache = i * i;
  }
  delay_exits++;
  return;
}

DEFINE_PRIMITIVE("GET-COMMAND-LINE-OPTION", Prim_get_comline_option, 2)
{
  long position;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  position = Parse_Option(Scheme_String_To_C_String(Arg1),
			  Saved_argc, Saved_argv, true);
  if ((position == NOT_THERE) ||
      (position == (Saved_argc-1)))
    PRIMITIVE_RETURN(Arg2);
  else
    PRIMITIVE_RETURN(C_String_To_Scheme_String(Saved_argv[position+1]));
}

DEFINE_PRIMITIVE("GET-HARDWARE-TRAP-INFO", Prim_get_hardware_info, 0)
{
  Pointer result;
  PRIMITIVE_HEADER(0);

  if (SHARED_DATA->Hardware_Trap_PC == NIL)
    PRIMITIVE_RETURN(NIL);

  Primitive_GC_If_Needed(7);
  result = Make_Pointer(TC_VECTOR, Free);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 6);
  *Free++ = SHARED_DATA->Hardware_Trap_PC;
  *Free++ = SHARED_DATA->Hardware_Trap_Irritant;
  *Free++ = SHARED_DATA->Hardware_Trap_Code;
  *Free++ = SHARED_DATA->Hardware_Trap_Task_Code;
  *Free++ = SHARED_DATA->Hardware_Trap_Task_Name;
  *Free++ = SHARED_DATA->Hardware_Trap_Task_Number;
  PRIMITIVE_RETURN(result);
}

DEFINE_PRIMITIVE("GET-HARDWARE-STACK-SLOT", Prim_get_hardware_stack, 1)
{
  extern Pointer *Absolute_Heap_Base;
  int slotno, gctype;
  Pointer entry, car, cdr, *target, result;
  Primitive_1_Arg();

  slotno = Get_Integer(Arg1);
  if (slotno < 0 || slotno >= HSTACK_SIZE)
    PRIMITIVE_RETURN(NIL);

  entry = SHARED_DATA->Hardware_Trap_Stack[slotno];
  if (entry == (-1))
    PRIMITIVE_RETURN(NIL);

  Primitive_GC_If_Needed(2);
  result = Make_Pointer(TC_LIST, Free);
  car = Make_Non_Pointer(TC_FIXNUM, OBJECT_TYPE(entry));
  cdr = Make_Non_Pointer(TC_FIXNUM, OBJECT_DATUM(entry));

  gctype = GC_Type_Map[OBJECT_TYPE(entry)];
  switch (gctype)
  {
    case GC_Non_Pointer:
      car = TRUTH;
      cdr = entry;
      break;

    case GC_Compiled:
    case GC_Cell:
    case GC_Pair:
    case GC_Triple:
    case GC_Quadruple:
    case GC_Vector:
      target = Get_Pointer(entry);
      if (target >= SHARED_DATA->Code_Base)
      {				/* Pure or Code */
	car = TRUTH;
	cdr = entry;
      }
      else if ((target >= Absolute_Heap_Base) &&
	       (OBJECT_TYPE(*target) == TC_BROKEN_HEART))
      {
	car = TRUTH;
	cdr = Make_Pointer(OBJECT_TYPE(entry), *target);
      }
      break;

    default:
      break;
  }

  *Free++ = car;
  *Free++ = cdr;
  PRIMITIVE_RETURN(result);
}

GC_Error_To_Real_Error()
{
  Will_Push(STACK_ENV_EXTRA_SLOTS + 3);
    Push(NIL);
    Push(Make_Unsigned_Fixnum(GC_Space_Needed));
    Push(C_String_To_Scheme_String("Unable to allocate after a GC"));
    Push(Get_Fixed_Obj_Slot(Error_Procedure));
    Push(STACK_FRAME_HEADER + 3);
  Pushed();
  GC_Space_Needed = 0;
  longjmp(*Back_To_Eval, PRIM_APPLY);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE("SET-SUBSUMPTION-RATIO!", Prim_Set_Subsumer_Hack, 2)
{
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);

  SHARED_DATA->Subsumption_Numerator = Get_Integer(Arg1);
  SHARED_DATA->Subsumption_Denominator = Get_Integer(Arg2);
  PRIMITIVE_RETURN(NIL);
}

DEFINE_PRIMITIVE("GET-USEFUL-METERS", Prim_Get_Useful_Meters, 0)
{
  Pointer Result;
  int i;
  PRIMITIVE_HEADER(0);

  Primitive_GC_If_Needed(33);

  Result = Make_Pointer(TC_VECTOR, Free);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 32);
  for (i = 0; i < 32; i++)
    *Free++ = Make_Non_Pointer(TC_FIXNUM,
			       SHARED_DATA->Useful_Meters[i]);
  PRIMITIVE_RETURN(Result);
}

extern int Debug_Flags[];

DEFINE_PRIMITIVE("SET-DEBUG-FLAG!", Prim_Set_Debug_Flag, 2)
{
  int flagno;
  Pointer Old;
  Primitive_2_Args();

  Range_Check(flagno, Arg1, 0, 99, ERR_ARG_1_BAD_RANGE);
  Old = (Debug_Flags[flagno] ? TRUTH : NIL);
  Debug_Flags[flagno] = (Arg2 != NIL);
  PRIMITIVE_RETURN(Old);
}

/*
  Codes are as follows:
	1	Total Master GC Real Time
	2	Total Master GC CPU Time
	3	Vector of GC Real Times
	4	Vector of GC CPU Times

*/

DEFINE_PRIMITIVE("GET-TIMING-INFO", Prim_Timing_Info, 2)
{
  int option;
  Pointer Make_Processor_Vector(), result;
  Primitive_2_Args();

  Range_Check(option, Arg1, 1, 4, ERR_ARG_1_BAD_RANGE);
  switch (option)
  {
    case 1:
      result =Make_Non_Pointer(TC_FIXNUM, SHARED_DATA->GC_Master_Real_Time);
      if (Arg2 != NIL)
	SHARED_DATA->GC_Master_Real_Time = 0;
      break;

    case 2:
      result = Make_Non_Pointer(TC_FIXNUM, SHARED_DATA->GC_Master_CPU_Time);
      if (Arg2 != NIL)
	SHARED_DATA->GC_Master_CPU_Time = 0;
      break;

    case 3:
      result = Make_Processor_Vector(&SHARED_DATA->GC_Processor_Real_Time[0],
				     N_Interps);
      if (Arg2 != NIL)
	Clear_Processor_Vector(&SHARED_DATA->GC_Processor_Real_Time[0], N_Interps);
      break;

    case 4:
      result = Make_Processor_Vector(&SHARED_DATA->GC_Processor_CPU_Time[0],
				     N_Interps);
      if (Arg2 != NIL)
	Clear_Processor_Vector(&SHARED_DATA->GC_Processor_CPU_Time[0], N_Interps);
      break;
  }
  PRIMITIVE_RETURN(result);
}

Pointer
Make_Processor_Vector(longs, nvalues)
     long *longs;
     int nvalues;
{
  int i;
  Pointer result;

  Primitive_GC_If_Needed(nvalues + 1);
  result = Make_Pointer(TC_VECTOR, Free);
  *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, nvalues);
  for (i = 0; i < nvalues; i++)
  {
    while (longs[i] == 0)
      Standard_Delay();
    *Free++ = Make_Non_Pointer(TC_FIXNUM, longs[i]);
  }
  return (result);
}

Clear_Processor_Vector(longs, nvalues)
     long *longs;
     int nvalues;
{
  int i;

  for (i = 0; i < nvalues; i++)
    longs[i] = 0;
  return;
}

DEFINE_PRIMITIVE("CODE->CODE-SPACE", Prim_Move_To_Codespace, 1)
{
  Pointer
    *Code, Proc, *NewCode,
    *Allocate_Code();
  long offset, length;
  Primitive_1_Arg();

  if (OBJECT_TYPE(Arg1) == TC_COMPILED_CODE_BLOCK)
  {
    Code = Get_Pointer(Arg1);
    offset = 0;
  }
  else
  {
    Arg_1_Type(TC_COMPILED_ENTRY);
    Get_Compiled_Block(Code, Get_Pointer(Arg1));
    offset = ((((long) Arg1) & 0x00ffffff) - ((long) Code));
  }

  if (Debug_Flags[14]) 
    printf("Arg1 = %x, Code = %x, offset = %x\n",
	   Arg1, Code, offset);

  length = (1 + Get_Integer(Code[VECTOR_LENGTH]));
  NewCode = Allocate_Code(length);
  
  if (NewCode == ((Pointer *) NULL))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);

  if (Debug_Flags[14]) 
    printf("Allocated NewCode = %x, for %x pointers\n",
	   NewCode, length);

  code_copy(Code, NewCode, length * sizeof(Pointer));
  if (Debug_Flags[14]) 
    printf("Moved %x pointers from %x to %x\n",
	   length, Code, NewCode);

  Propagate_Code(NewCode, NewCode + length);
  if (Debug_Flags[14]) 
    printf("Propagating from %x to %x\n",
	   NewCode, NewCode + length);
  
  NewCode = ((Pointer *) (((long) NewCode) + offset));
  if (Debug_Flags[14]) 
    printf("Resulting NewCode is %x\n", NewCode);

  PRIMITIVE_RETURN(Make_Pointer(OBJECT_TYPE(Arg1), NewCode));
}

/* The system defined NCPUS is wrong! */

#define MAXCPUS 256

short int map_cluster_to_physical[MAXCPUS];
short int map_physical_to_cluster[MAXCPUS];

/* called from BUTTERFLY.C after cluster_create */

initialize_processor_maps()
{
  kern_return_t code, cluster_stat();
  short int map_cluster_to_system[MAXCPUS];
  short int map_system_to_physical[MAXCPUS];
  int ncluster, nphysical, n;

  code = cluster_stat(SHARED_DATA->Cluster_Id, GET_NODE_LIST,
		      map_cluster_to_system, &ncluster);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr, "-->> Unable to get GET_NODE_LIST on cluster %d - %d\n",
	    SHARED_DATA->Cluster_Id, code);
    exit(1);
  }

  code = cluster_stat(SHARED_DATA->Cluster_Id, GET_PHYS_PNN_MAP,
		      map_system_to_physical, &nphysical);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr, "-->> Unable to get GET_PHYS_PNN_MAP on cluster %d - %d\n",
	    SHARED_DATA->Cluster_Id, code);
    exit(1);
  }

  for (n = 0; n < MAXCPUS; n++)
    map_physical_to_cluster[n] = (4096 + n);

  for (n = 0; n < ncluster; n++)
  {
    map_cluster_to_physical[n] = map_system_to_physical[map_cluster_to_system[n]];
    map_physical_to_cluster[map_cluster_to_physical[n]] = n;
  }
}

DEFINE_PRIMITIVE("GET-PHYSICAL-NODE", Prim_Get_Pointer_Processor, 1)
{
  int gctype, type, physaddr, procno;
  Primitive_1_Arg();

  type = OBJECT_TYPE(Arg1);
  gctype = GC_Type_Map[type];
  switch(gctype)
  {
    case GC_Cell:
    case GC_Pair:
    case GC_Triple:
    case GC_Quadruple:
    case GC_Compiled:
    case GC_Vector:
      physaddr = (long) (getphysaddr(Get_Pointer(Arg1)));
      procno = physaddr >> 24;
      PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM,
					map_physical_to_cluster[procno]));

    default:
      PRIMITIVE_RETURN(NIL);
  }
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE("MY-PROCESSOR-NUMBER", Prim_My_Proc, 0)
{
  PRIMITIVE_HEADER(0);

  PRIMITIVE_RETURN(Make_Non_Pointer(TC_FIXNUM,
				    map_cluster_to_physical[Who_Am_I]));
}
