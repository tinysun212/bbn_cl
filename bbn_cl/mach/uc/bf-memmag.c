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
#include "locks.h"

extern int
  Debug_Flags[];

extern long
  Memory_Quantum,
  Local_Chunk_Size;

extern void
  Reset_Memory_Table(),
  print_memory_table(),
  Re_Initialize_Memory_Table(),
  Grab_Heap_Space_After_GC();
  
extern Boolean
  Grab_Heap_Space();

/* Allocating more memory while running.  
   Part of the allocator, not the collector.
 */

void
Reset_Memory_Table()
{
  fast int i;

  SHARED_DATA->Memory_Lock = 0;

  for (i = 0; i < N_Interps; i++)
  {
    SHARED_DATA->Memory_Emergency[i] = false;
    SHARED_DATA->Memory_Table[i].Free_Top = 
      SHARED_DATA->InterpreterTable[i].My_Cons_Top;
    SHARED_DATA->Memory_Table[i].Free_Bottom = 
      SHARED_DATA->InterpreterTable[i].My_Free;
  }
  return;
}

void
print_memory_table()
{
  int fast i;

  for (i = 0; i < N_Interps; i++)
    printf("<< %d | 0x%x - 0x%x >>\n", i,
	   SHARED_DATA->Memory_Table[i].Free_Bottom,
	   SHARED_DATA->Memory_Table[i].Free_Top);
  return;
}

Boolean
Try_Allocating(which, needed, slack)
     int which, needed, slack;
{ 
  while (atomior(&SHARED_DATA->Memory_Lock, 0x8000) != 0)
    Standard_Delay();

  if ((SHARED_DATA->Memory_Table[which].Free_Top -
       SHARED_DATA->Memory_Table[which].Free_Bottom) <
      (needed + slack))
  {
    SHARED_DATA->Memory_Lock = 0;
    return (false);
  }

  /* If the next chunk is contiguous to the one we already have,
     we need not reset these parameters.  In this way we use whatever
     is left over from the previous buffer as well.
   */

  if (Heap_Top != SHARED_DATA->Memory_Table[which].Free_Bottom)
  {
    Heap_Bottom = SHARED_DATA->InterpreterTable[which].My_Cons_Bottom;
    Heap = Heap_Bottom;
    Free = SHARED_DATA->Memory_Table[which].Free_Bottom;
  }

  Heap_Top = SHARED_DATA->Memory_Table[which].Free_Bottom + needed;
  SHARED_DATA->Memory_Table[which].Free_Bottom = Heap_Top;
  SHARED_DATA->Memory_Lock = 0;

  if (Debug_Flags[16])
  {
    printf("\n(on %d/%d): Allocated 0d%d + 0d%d pointers",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], needed, slack);
    printf(" in %d from 0x%x to 0x%x (0x%x)\n",
	   which, Heap_Bottom, Heap_Top, Free);
  }

  SET_MEMTOP(Heap_Top - SHARED_DATA->GC_Reserve);
  return (true);
}

Boolean
Grab_Heap_Space(forcep)
     Boolean forcep;
{
  long needed, i, slack;

#if false
  if (SHARED_DATA->Memory_Emergency[Who_Am_I])
  {
    fprintf(stderr,
	    "Processor %d (%d) already has a memory emergency\n",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    Microcode_Termination(TERM_GC_OUT_OF_SPACE);
  } 
#endif
  
  if (N_Interps == 1)
  {
    if (SHARED_DATA->Memory_Table[0].Free_Bottom ==
	SHARED_DATA->Memory_Table[0].Free_Top)
      return (false);

    Heap_Bottom = SHARED_DATA->InterpreterTable[0].My_Cons_Bottom;
    Heap = Heap_Bottom;
    Free = SHARED_DATA->Memory_Table[0].Free_Bottom;
    Heap_Top = SHARED_DATA->Memory_Table[0].Free_Top;
    SET_MEMTOP(Heap_Top - SHARED_DATA->GC_Reserve);

    SHARED_DATA->Memory_Table[0].Free_Bottom = Heap_Top;

    if (Debug_Flags[16])
    {
      printf("\n(on 0/%d): Allocated 0d%d in 0 from 0x%x to 0x%x (0x%x)\n",
	     SHARED_DATA->Task_Id[0],
	     (Heap_Top - Free), Heap_Bottom, Heap_Top, Free);
    }
    return (true);
  }

  needed = GC_Space_Needed + Memory_Quantum;

  slack = ((INTERRUPT_QUEUED_P(INT_GC)) ?
	   0 :
	   SHARED_DATA->GC_Reserve);

  if ((!forcep) && ((MemTop - Free) >= needed))
    return (true);

  if (Try_Allocating(Who_Am_I, needed, slack))
  {
    if (SHARED_DATA->Memory_Table[Who_Am_I].Free_Top -
	SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom <=
	SHARED_DATA->GC_Reserve)
      SHARED_DATA->Memory_Emergency[Who_Am_I] = true;
    return (true);
  }

  for (i = 0; i < N_Interps; i++)
    if (Try_Allocating(i, needed, SHARED_DATA->GC_Reserve))
      return (true);

#if false
  fprintf(stderr,
	  "Processor %d (%d) unable to grab memory\n",
	  Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
  Microcode_Termination(TERM_GC_OUT_OF_SPACE);
#endif false

  return (false);
}

void
Grab_Heap_Space_After_GC()
{
  Pointer *my_free, *my_top, *top;
  long delta;

  my_free = SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom;
  if (my_free > SHARED_DATA->InterpreterTable[Who_Am_I].My_Cons_Bottom)
  {
    /* Let's recover the space at the end of the last heaplet
       used during GC.  TERMINATE_HEAPLET has conveniently
       left a word right before my_free with the length of the
       usable area.
     */

    delta = (1 + Get_Integer(*(my_free - 1)));
    atomadd(&SHARED_DATA->Total_Waste, ((short int) (-delta)));
    my_free -= delta;
  }
  SHARED_DATA->InterpreterTable[Who_Am_I].My_Free = my_free;
  my_top =  SHARED_DATA->Memory_Table[Who_Am_I].Free_Top;
  atomadd32(&SHARED_DATA->Total_Free, (my_top - my_free));
  top = my_free + (Memory_Quantum + GC_Space_Needed);
  if ((my_top < top) || (N_Interps == 1))
    top = my_top;
  SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom = top;
  Free = my_free;
  SET_MEMTOP(top - SHARED_DATA->GC_Reserve);
  Heap_Top = top;
  Heap_Bottom = Local_Heap_Base + (Local_Chunk_Size * Who_Am_I);
  Heap = Heap_Bottom;
  return;
}

/* This is done by the master after a disk-restore */

void
Re_Initialize_Memory_Table()
{
  fast Pointer *new_top, *temp;
  fast int i;

  SHARED_DATA->Memory_Lock = 0;

  new_top = Free;

  if (Local_Heap_Base != SHARED_DATA->Heap_Base)
  {
    /* The band load has flipped the heaps.
       Update the data structures to reflect this.
     */

    struct Interpreter_Info *My_Data;

    for (i = 0; i < N_Interps; i++)
    {
      My_Data = &SHARED_DATA->InterpreterTable[i];
      My_Data->My_Scan = My_Data->My_Cons2_Bottom;
      My_Data->My_Cons2_Bottom = My_Data->My_Cons_Bottom;
      My_Data->My_Cons_Bottom = My_Data->My_Scan;

      temp = My_Data->My_Cons2_Top;

      My_Data->My_Cons2_Top = My_Data->My_Cons_Top;
      My_Data->My_Cons_Top = temp;
      My_Data->My_Free = My_Data->My_Cons_Bottom;
    }
    Local_Heap_Base = SHARED_DATA->Heap_Base;
  }

  if (N_Interps == 1)
  {
    SHARED_DATA->Memory_Emergency[0] = false;
    SHARED_DATA->Memory_Table[0].Free_Bottom = Free;
    SHARED_DATA->Memory_Table[0].Free_Top =
      SHARED_DATA->InterpreterTable[0].My_Cons_Top;
    SHARED_DATA->InterpreterTable[0].My_Free = Free;
  }
  else
  {
    for (i = 0; i < N_Interps; i++)
    {
      SHARED_DATA->Memory_Emergency[i] = false;
      temp = SHARED_DATA->InterpreterTable[i].My_Cons_Top;
      SHARED_DATA->Memory_Table[i].Free_Top = temp;
      
      SHARED_DATA->Memory_Table[i].Free_Bottom =
	((temp <= new_top) ? temp :
	 ((new_top < SHARED_DATA->InterpreterTable[i].My_Cons_Bottom) ?
	  SHARED_DATA->InterpreterTable[i].My_Cons_Bottom :
	  new_top));

      SHARED_DATA->InterpreterTable[i].My_Free =
	SHARED_DATA->Memory_Table[i].Free_Bottom;
    }
  }
  return;
}
