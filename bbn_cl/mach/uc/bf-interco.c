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
 *
 * Butterfly locking, propagating, communicating stuff. 
 */

#include "scheme.h"
#include "transact.h"
#include "primitive.h"
#include "locks.h"
#include "zones.h"

extern int Debug_Flags[];
extern int input_server_pid;

Pointer *Local_Heap_Base;	/* The current half of the heap */
long Local_Chunk_Size;		/* The size of each chunk */

/* Lock_Cell is the general purpose write serializing lock and
   returns a Lock_Handle for use with Unlock_Cell

   A Lock_Handle has the following format:
   		0:	16 bit lock used by the PNC
		2:	16 bit processor ID plus one
*/

Lock_Handle
Lock_Cell(Place)
     Pointer *Place;
{
  long heap_offset, Saved_Zone, retries;
  short int *lock_itself;

  Save_Time_Zone(Zone_Store_Lock);
#if false
  atomadd32(&SHARED_DATA->Useful_Meters[0], 1);
#endif
  if (Place >= Constant_Space && Place < Constant_Top)
  {
    lock_itself = ((short int *) &SHARED_DATA->Constant_Lock);
  }
  else
  {
    heap_offset = ((Place - Local_Heap_Base) / Local_Chunk_Size);
    if (heap_offset < 0 || heap_offset >= N_Interps)
    {
      return ((Lock_Handle) NULL);
    }
    lock_itself = ((short int *) &SHARED_DATA->Heap_Locks[heap_offset]);
  }
  
  if (Debug_Flags[7])
  {
    printf("(on %d/%d) Locking cell at %x, CS=%x CT=%x LHB=%x Handle=%x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   Place, Constant_Space, Constant_Top, Local_Heap_Base,
	   lock_itself);
    fflush(stdout);
  }

  for (retries = 0; retries < 100000; retries++)
  {
    if (atomior(lock_itself, 0xffff) == 0x0000)
    {
      lock_itself[1] = (Who_Am_I + 1); /* Set our processor node. */
      Restore_Time_Zone();
      return ((Lock_Handle) lock_itself);
    }
    if (lock_itself[1] == (Who_Am_I + 1))
    {
      /* Are we waiting on our own lock?  */

      Restore_Time_Zone();	/* Then we have it already. */
      return ((Lock_Handle) NULL);
    }
    Standard_Delay();
  }
  
  if (Debug_Flags[7])
  {
    printf("(on %d/%d) Cannot lock: 0x%x; lock_address: 0x%x; Held by %d\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   Place, lock_itself, (lock_itself[1] - 1));
    fflush(stdout);
  }

  atomadd(&SHARED_DATA->Contention_Count, 1);
  Restore_Time_Zone();
  return ((Lock_Handle) NULL);
}

void
Unlock_Cell(the_lock)
     Lock_Handle the_lock;
{ 
  if (Debug_Flags[7])
  {
    printf("(on %d/%d) Unlocking cell with lock %x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   the_lock);
    fflush(stdout);
  }

  if (the_lock != ((Lock_Handle) NULL))
  {
#if false
    atomadd32(&SHARED_DATA->Useful_Meters[1], 1);
#endif
    *((long *) the_lock) = 0;
  }
  return;
}

void
Initialize_Heap_Locks()
{
  fast int i;

  SHARED_DATA->Constant_Lock = 0;
  for (i = 0; i < N_Interps; i++)
    SHARED_DATA->Heap_Locks[i] = 0;
  return;
}

Pointer 
Atomic_Swap_Pointers(P, S)
     Pointer *P, S;
{
  Lock_Handle heap_lock;
  Pointer temp;

  heap_lock = Lock_Cell(P);
  temp = *P;
  *P = S;
  Unlock_Cell(heap_lock);

  return (temp);
}

/* (GLOBAL-INTERRUPT LEVEL WORK TEST)

   There are 4 global interrupt level, level 0 (highest priority)
   being reserved for GC.  See Scheme.H for details of the dist-
   ribution of these bits with respect to local interrupt levels.

   Force all other processors to begin executing WORK (an interrupt
   handler [procedure of two arguments]) with all interrupts disabled
   provided that TEST returns true.  TEST is supplied to allow this
   primitive to be restarted if it is unable to begin because another
   processor wins the race to generate a global interrupt and makes it
   no longer necessary that this processor generate one (TEST receives
   no arguments).  Notice that this primitive effectively enables
   receipt of global interrupts regardless of the current interrupt
   mask to prevent a deadlock situation.  This primitive returns the
   value of the call to TEST (i.e. non-#!FALSE if the interrupt was
   really generated), and returns only after all other processors have
   begun execution of WORK (or TEST returns false).
*/

DEFINE_PRIMITIVE("GLOBAL-INTERRUPT", Send_Global_Interrupt, 3)
{
  long Saved_Zone, Which_Level, Int_Index, Pseudo_Mask;
  Primitive_3_Args();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(Which_Level, Arg1, 0, 3, ERR_ARG_1_BAD_RANGE);

  switch(Which_Level)
  {
  case 0:
    Which_Level = INT_Global_GC;
    Int_Index = Global_GC_Level;
    break;

  case 1:
    Which_Level = INT_Global_1;
    Int_Index = Global_1_Level;
    break;

  case 2:
    Which_Level = INT_Global_2;
    Int_Index = Global_2_Level;
    break;

  case 3:
    Which_Level = INT_Global_3;
    Int_Index = Global_3_Level;
    break;
  }

  /* We will accept all interrupts at the same or higher priorities
     than the one we are sending.
   */

  Pseudo_Mask = ((Which_Level << 1) -1);

  if (Debug_Flags[9])
  {
    printf("Process %d global interrupt(%d) w_lev = %d int_ind = %d lock=%x\n",
	   getpid(), Get_Integer(Arg1), Which_Level, Int_Index,
	   SHARED_DATA->Global_Int_Lock);
    fflush(stdout);
  }

  if (Debug_Flags[29])
    {
      printf ("Process %d waiting in global_interrupt\n", getpid());
      really_spin_for_debugger ("Global Interrupt");
    }

  Save_Time_Zone(Zone_Global_Int);

#if false
  if (Pseudo_Mask > IntEnb)
    fprintf(stderr, "Global interrupt allowing 0x%x, was 0x%x interrupts.\n",
	    Pseudo_Mask, IntEnb);
#endif

  while (atomior(&SHARED_DATA->Global_Int_Lock, 0x8000) != 0)
  {
    if ((Pseudo_Mask & IntCode) != 0)
    {
      Restore_Time_Zone();
      Special_Primitive_Interrupt(Pseudo_Mask);
    }
  }

  if ((Pseudo_Mask & IntCode) != 0)
  {
    Restore_Time_Zone();
    SHARED_DATA->Global_Int_Lock = 0;
    Special_Primitive_Interrupt(Pseudo_Mask);
  }

  SHARED_DATA->Global_Interrupt_Handlers[Int_Index] = Arg2;

 Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
  Pop_Primitive_Frame(3);
  Store_Return(RC_FINISH_GLOBAL_INT);
  Store_Expression(Make_Unsigned_Fixnum(Which_Level));
  Save_Cont();
  Push(Arg3);
  Push(STACK_FRAME_HEADER);
 Pushed();

  Restore_Time_Zone();
  PRIMITIVE_ABORT(PRIM_APPLY);
}

Pointer
Global_Int_Part_2(Which_Level, Do_It)
     Pointer Do_It, Which_Level;
{
  long Saved_Zone, Int_Bit;
  struct global_interrupt_data gid;
  
  if (Debug_Flags[9])
  {
    printf("Process %d global interrupt part 2 level = %d flag = %x\n",
	   getpid(), Which_Level, Do_It);
    fflush(stdout);
  }

  Save_Time_Zone(Zone_Global_Int);
  if (Do_It == NIL)
  {
    SHARED_DATA->Global_Int_Lock = 0;
    Restore_Time_Zone();
    return (NIL);
  }

  Int_Bit = Get_Integer(Which_Level);
  gid.code = Int_Bit;
  Send_Signal_Info(-1, SIG_GLOBAL_INT, &gid, sizeof(gid));

  SHARED_DATA->Global_Int_Lock = 0;
  Restore_Time_Zone();
  return (Do_It);
}

/* In both of these procedures, buflen is always in chars.
   It is converted to ints for faster copying.
 */

void
Send_Signal_Info(target, meaning, buffer, buflen)
     int target, meaning, buflen, *buffer;
{
  long lock;
  fast int i;

  if (Debug_Flags[10])
  {
    printf("(on %d/%d) send signal info(%d, %d, 0x%x, %d) at %d\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   target, meaning, buffer, buflen, System_Clock());
    fflush(stdout);
  }

  while (atomior(&SHARED_DATA->Signal_Lock, 0x8000) != 0x0000)
    Standard_Delay();

  SHARED_DATA->Signal_Meaning = meaning;
  SHARED_DATA->Signal_Data_Length = buflen;

  buflen = ((buflen + 3) >> 2);

  for (i = 0; i < buflen; i++)
  {
    SHARED_DATA->Signal_Data[i] = buffer[i];
  }

  if (target < 0)
  {
    SHARED_DATA->Signal_Lock = 0x8000 + N_Interps;
    for (i = 0; i < N_Interps; i++)
    {
      if (i != Who_Am_I)
	{
	  if (Debug_Flags[2])
	    printf ("Send_signal_info (1) SIGUSR1: %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
	  kill(SHARED_DATA->Task_Id[i], SIGUSR1);
	}
    }
  }
  else
  {
    if (Debug_Flags[2])
      printf ("Send_signal_info (2) SIGUSR1: %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
    SHARED_DATA->Signal_Lock = 0x8000 + 2;
    kill(SHARED_DATA->Task_Id[target], SIGUSR1);
  }
  
  lock = SHARED_DATA->Signal_Lock;

  while((SHARED_DATA->Signal_Lock & 0x7fff) != 1)
    Standard_Delay();

  SHARED_DATA->Signal_Lock = 0;

  if (Debug_Flags[10])
  {
    printf("(on %d/%d) message acknowledged at %d; lock was 0x%x.\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), lock);
    fflush(stdout);
  }
  return;
}

void
Get_Signal_Data(buffer, buflen)
     int *buffer, buflen;
{
  int i;

  if (Debug_Flags[10])
  {
    printf("getting %d bytes of signal data on %d\n",
	   buflen, Who_Am_I);
    fflush(stdout);
  }

  buflen = ((buflen + 3) >> 2);

  for (i = 0; i < buflen; i++)
    buffer[i] = SHARED_DATA->Signal_Data[i];

  atomadd(&SHARED_DATA->Signal_Lock, -1);
  return;
}
