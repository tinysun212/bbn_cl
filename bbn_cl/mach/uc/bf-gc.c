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
#include "gccode.h"
#include "locks.h"
#include "zones.h"

extern void gc_death();
extern char gc_death_message_buffer[];

extern int Debug_Flags[];

extern void
  print_memory_table(),
  Grab_Heap_Space_After_GC();

extern Pointer
  *GCLoop(),
  *get_new_consing_partition(),
  *cons_heaplet, *cons_heaplet_bound, *cons_heaplet_end,
  *scan_heaplet_end;

/* This is the buffer zone at the end of a heaplet to avoid
   overconsing.  It must be as large as the largest fixed
   size object (a quad, currently) + 1 for the end-of-scan
   broken heart object.

   This constant is also defined in bf-gcloop.s.  They should
   always agree.
 */

#define HEAPLET_BUFFER_SIZE 5

Pointer
  *cons_heaplet, *cons_heaplet_bound,
  *cons_heaplet_end, *scan_heaplet_end;

static short Max_Index;
static Heaplet *Current_Cons_Heaplet;

extern Pointer *Transport_Bound;
extern long Transport_Delta;

#define GC_LENGTH			1
#define STANDARD_LENGTH			-1

#define GC_FLIP_DELAY			0
#define GC_FLIP_DELAY_LENGTH		STANDARD_LENGTH
#define GC_PUSH_DELAY			1
#define GC_PUSH_DELAY_LENGTH		GC_LENGTH
#define GC_POP_DELAY			2
#define GC_POP_DELAY_LENGTH		(25 * GC_LENGTH)
#define GC_MEM_LOCK_DELAY		3
#define GC_MEM_LOCK_DELAY_LENGTH	GC_LENGTH
#define GC_IDLE_DELAY			4
#define GC_IDLE_DELAY_LENGTH		STANDARD_LENGTH
#define GC_CONSING_DELAY		5
#define GC_CONSING_DELAY_LENGTH		STANDARD_LENGTH
#define GC_MASTER_DELAY			6
#define GC_MASTER_DELAY_LENGTH		(10 * GC_LENGTH)
#define GC_COPY_DELAY			7
#define GC_COPY_DELAY_LENGTH		STANDARD_LENGTH
#define GC_DONE_DELAY			8
#define GC_DONE_DELAY_LENGTH		STANDARD_LENGTH

#define NUMBER_OF_GC_DELAYS		9

static char *
  gc_delay_names[NUMBER_OF_GC_DELAYS] =
  {
    "flp",
    "psh",
    "pop",
    "mlk",
    "idl",
    "cns",
    "mst",
    "cpy",
    "don"
  };

static int
  gc_delay_lengths[NUMBER_OF_GC_DELAYS] =
  {
    GC_FLIP_DELAY_LENGTH,
    GC_PUSH_DELAY_LENGTH,
    GC_POP_DELAY_LENGTH,
    GC_MEM_LOCK_DELAY_LENGTH,
    GC_IDLE_DELAY_LENGTH,
    GC_CONSING_DELAY_LENGTH,
    GC_MASTER_DELAY_LENGTH,
    GC_COPY_DELAY_LENGTH,
    GC_DONE_DELAY_LENGTH
  },
  gc_delay_counts[NUMBER_OF_GC_DELAYS],
  gc_delay_array[MAX_INTERPRETERS];

void
initialize_gc_delays()
{
  fast int i;

  for (i = NUMBER_OF_GC_DELAYS; --i >= 0;)
    gc_delay_counts[i] = 0;

  return;
}

void
show_gc_delays()
{
  fast int i;

  printf("(on %d/%d)", Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
  for (i = 0; i < NUMBER_OF_GC_DELAYS; i++)
  {
    printf(" %s: %d", gc_delay_names[i], gc_delay_counts[i]);
  }
  printf("\n");
  fflush(stdout);
  return;
}

void
GC_Delay(delay_index)
     int delay_index;
{
  int i, delay;

  gc_delay_counts[delay_index] += 1;
  delay = gc_delay_lengths[delay_index];

  if (delay < 0)
  {
    do
    {
      Standard_Delay();
      delay += 1;
    } while (delay < 0);
  }
  else
  {
    while (--delay >= 0)
    {
      for (i = N_Interps; --i >= 0; )
	gc_delay_array[i] = i;
    }
  }
  return;
}

/* Heaplet queue operations. */

#define Heaplet_Queue 		SHARED_DATA->Work_Queue;
#define QUEUE_ALL_DONE		1
#define QUEUE_GOT_ONE		0
#define QUEUE_SPIN		-1

#define Initialize_Heaplet_Queue()					\
{									\
  struct fifo_queue *queue;						\
									\
  queue = &Heaplet_Queue;						\
  queue->queue_busy_count = N_Interps;					\
}

Boolean
Push_To_Heaplet_Queue(Heaplet_ptr)
     Heaplet *Heaplet_ptr;
{
  fast struct fifo_queue *queue;

  queue = &Heaplet_Queue;

  while (atomior(&queue->queue_lock, 0x8000) != 0x0000)
    GC_Delay(GC_PUSH_DELAY);

  if (queue->queue_count >= queue->queue_max)
  {
    queue->queue_lock = 0x0000;
    return (false);
  }

  queue->queue_content[queue->queue_put++] = ((long) Heaplet_ptr);
  if (queue->queue_put == queue->queue_max)
    queue->queue_put = 0;

  queue->queue_count++;
  queue->queue_lock = 0x0000;
  return (true);
}

int
Pop_From_Heaplet_Queue(Heaplet_ptr_loc, idlep)
     Heaplet **Heaplet_ptr_loc;
     Boolean *idlep;
{
  fast struct fifo_queue *queue;

  queue = &Heaplet_Queue;
  while (atomior(&queue->queue_lock, 0x8000) != 0x0000)
    GC_Delay(GC_POP_DELAY);

  if (queue->queue_count > 0)
  {

#ifdef GC_QUEUE

    /* Use the gc queue as a queue */

    *Heaplet_ptr_loc = queue->queue_content[queue->queue_get++];
    if (queue->queue_get == queue->queue_max)
      queue->queue_get = 0;

#else not GC_QUEUE

    /* Use the gc queue as a stack */

    if (queue->queue_put == 0)
      queue->queue_put = queue->queue_max;
    *Heaplet_ptr_loc =
      ((Heaplet *) queue->queue_content[--queue->queue_put]);

#endif GC_QUEUE

    if (*idlep)
    {
      *idlep = false;
      queue->queue_busy_count += 1;
    }
    queue->queue_count--;
    queue->queue_lock = 0x0000;
    return (QUEUE_GOT_ONE);
  }
  if (!*idlep)
  {
    *idlep = true;
    queue->queue_busy_count -= 1;
  }
  if (queue->queue_busy_count == 0)
  {
    queue->queue_lock = 0x0000;
    return (QUEUE_ALL_DONE);
  }
  else
  {
    queue->queue_lock = 0x0000;
    return (QUEUE_SPIN);
  }
}

#define CLOSE_AREA(ptr)							\
{									\
  *(ptr) = Make_Pointer(TC_BROKEN_HEART, ptr);				\
}

#define CLOSE_CONSTANT_AREA	CLOSE_AREA

/* This assumes that it is always the case that (end - free) >= 1
   This should hold because of the space used to place the
   broken heart to stop scanning.
   Note that the second assignment may overwrite the first, but that's
   OK because only the value stored by the second is ever used.
   The fist is there only to allow forward scanning of the heap.
 */

#define TERMINATE_HEAPLET_SHARED(free, end)				\
{									\
  long delta;								\
									\
  delta = ((end - free) - 1);						\
  *(free) = Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, delta);		\
  *(end - 1) = Make_Non_Pointer(TC_TRUE, delta);			\
}

#ifndef USE_SINGLE_HEAP

#define OTHER_HEAP(ptr)		ptr
#define CLOSE_HEAPLET  		CLOSE_AREA
#define TERMINATE_HEAPLET	TERMINATE_HEAPLET_SHARED

#else USE_SINGLE_HEAP
#define OTHER_HEAP(ptr)							\
  ((Pointer *) (((char *) (ptr)) + Transport_Delta))

#define CLOSE_HEAPLET(ptr)						\
{									\
  *(OTHER_HEAP(ptr)) = Make_Pointer(TC_BROKEN_HEART, ptr);		\
}

#define TERMINATE_HEAPLET(free, end)					\
{									\
  TERMINATE_HEAPLET_SHARED((OTHER_HEAP(free)), (OTHER_HEAP(end)));	\
}

#endif USE_SINGLE_HEAP

Pointer *current_free;

void
GCScan(area, low, high, heaplet_p)
     char *area;
     Pointer *low, *high;
     Boolean heaplet_p;
{ 
  Pointer *result;

  if (low >= high)
    return;

  if (Debug_Flags[3])
  {
    printf("(on %d/%d) Scanning %s from 0x%x to 0x%x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], area, low, high);
    fflush(stdout);
  }

  /* This assumes that the location at high does not need
     to be scanned and contains garbage.
     Whoever calls GCScan must make sure that this is true,
     or save the old value and make sure that it gets scanned.
   */

  if (heaplet_p)
  {
    CLOSE_HEAPLET(high);
  }
  else
  {
    CLOSE_CONSTANT_AREA(high);
  }
  result = GCLoop(low, &current_free);
  if (result != high)
  {
    sprintf(gc_death_message_buffer,
	    "(on %d/%d) GCScan: found a spurious broken heart",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    gc_death(TERM_BROKEN_HEART, gc_death_message_buffer,
	     result, current_free);
    /*NOTREACHED*/
  }
  return;
}

/* Heaplet management

   - First we grab the desired quantity of memory + the basic
   memory quantum using the table of available per machine partitions.
   (Sound familiar - remember this is a first cut).

   - Then we find a slot in the heaplet array and set the three
   important values.

   - Since our partitions are small we should be able to get away
   without setting up Current_Info, which is just used to make sure
   the free pointer gets set up correctly for each processor
   partition.

*/

#define GC_QUANTUM	512			/* in Pointers */
#define GC_FUDGE	HEAPLET_BUFFER_SIZE	/* in Pointers */

Pointer *
get_consing_partition(desired)
     long desired;
{
  Pointer
    *Current_Bottom, *Current_Top;
  long
    Current_Cons_Index, Current_Processor, Current_Partition;

  /* (1) Figure out how many pointers we are going to need */

  desired = ((desired < (GC_QUANTUM - GC_FUDGE)) ?
	     GC_QUANTUM :
	     (desired + GC_FUDGE));

  /* (2) Now allocate a heaplet index entry */

  Current_Cons_Index = atomadd(&SHARED_DATA->Heaplet_Index, 1);
  if (Current_Cons_Index > Max_Index)
  {
    sprintf(gc_death_message_buffer,
	    "(on %d/%d) Completely out of heaplet indices %d out of %d\n",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	    Current_Cons_Index, Max_Index);
    gc_death(TERM_GC_OUT_OF_SPACE, gc_death_message_buffer,
	     scan_heaplet_end, current_free);
    /*NOTREACHED*/
  }

  Current_Processor = (Current_Cons_Index % N_Interps);
  Current_Cons_Heaplet = &(GC_DATA->Heaplet_Array[Current_Cons_Index]);

  /* (3) Now allocate the memory in the table */

  while (atomior(&SHARED_DATA->Memory_Lock, 0x8000) != 0)
    GC_Delay(GC_MEM_LOCK_DELAY);

  if (Debug_Flags[3])
    print_memory_table();

  Current_Bottom = SHARED_DATA->Memory_Table[Current_Processor].Free_Bottom;
  Current_Top = Current_Bottom + desired;

  if (Current_Top > SHARED_DATA->Memory_Table[Current_Processor].Free_Top)
  {
    fast int i;

    for (i = (N_Interps - 1); --i >= 0; )
    {
      Current_Processor += 1;
      if (Current_Processor == N_Interps)
	Current_Processor = 0;

      Current_Bottom = 
	SHARED_DATA->Memory_Table[Current_Processor].Free_Bottom;
      Current_Top = Current_Bottom + desired;
      if (Current_Top <=
	  SHARED_DATA->Memory_Table[Current_Processor].Free_Top)
      {
	SHARED_DATA->Memory_Table[Current_Processor].Free_Bottom =
	  Current_Top;
	break;
      }
    }
  }
  else
    SHARED_DATA->Memory_Table[Current_Processor].Free_Bottom =
      Current_Top;
  
  SHARED_DATA->Memory_Lock = 0;
  
  /* (3a) If we couldn't get the memory, we must die */

  if (Current_Top > SHARED_DATA->Memory_Table[Current_Processor].Free_Top)
  {
    sprintf(gc_death_message_buffer,
	    "(on %d/%d) Completely out of memory (needing %d)\n",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	    desired);
    gc_death(TERM_GC_OUT_OF_SPACE, gc_death_message_buffer,
	     scan_heaplet_end, current_free);
    /*NOTREACHED*/
  }

  /* (4) Now export our result */

  Current_Cons_Heaplet->Heaplet_Top = Current_Top;
  Current_Cons_Heaplet->Heaplet_Free = Current_Bottom;
  Current_Cons_Heaplet->Heaplet_Scan = Current_Bottom;
  Current_Cons_Heaplet->Heaplet_No = Current_Cons_Index;

  if (Debug_Flags[3])
  {
    printf("(on %d/%d) Getting consing partition %d from 0x%x to 0x%x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   Current_Cons_Index, Current_Bottom, Current_Top);
    fflush(stdout);
  }

  /* Reset global paramenters */

  cons_heaplet = Current_Bottom;
  cons_heaplet_bound = (Current_Top - HEAPLET_BUFFER_SIZE);
  cons_heaplet_end = Current_Top;
  return (Current_Bottom);
}

void
terminate_consing_partition()
{
  /* Consistency check */

  if (&current_free[1] > cons_heaplet_end)
  {
    sprintf(gc_death_message_buffer,
	    "(on %d/%d) We have overconsed a heaplet",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    gc_death(TERM_EXIT, gc_death_message_buffer,
	     current_free, cons_heaplet_end);
    /*NOTREACHED*/
  }

  Current_Cons_Heaplet->Heaplet_Free = current_free;

  atomadd(&SHARED_DATA->Total_Waste,
	  ((short int) (Current_Cons_Heaplet->Heaplet_Top - current_free)));
  return;
}

void
terminate_last_consing_partition()
{
  terminate_consing_partition();
  if (Current_Cons_Heaplet->Heaplet_Scan !=
      Current_Cons_Heaplet->Heaplet_Free)
  {
    sprintf(gc_death_message_buffer,
	    "(on %d/%d) Scan has not met Free at the end of the GC",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    gc_death(TERM_EXIT, gc_death_message_buffer, 
	     Current_Cons_Heaplet->Heaplet_Scan,
	     Current_Cons_Heaplet->Heaplet_Free);
    /*NOTREACHED*/
  }
  TERMINATE_HEAPLET(current_free, Current_Cons_Heaplet->Heaplet_Top);
  return;
}

Pointer *
get_new_consing_partition(free, desired)
     Pointer *free;
     long desired;
{
  if (Debug_Flags[3])
  {
    printf("(on %d/%d) get_new_consing_partition(%x, %x)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], free, desired);
  }

  current_free = free;
  terminate_consing_partition();

  /* Place the heaplet on the queue of heaplets to be scanned unless
     we are in the process of scanning it.
   */

  if (scan_heaplet_end != cons_heaplet_end)
  {
    /* The return value should be checked for consistency,
       but the total number of heaplets is smaller than the
       length of the queue, so it should never fill up.
     */
    Push_To_Heaplet_Queue(Current_Cons_Heaplet);
  }
  else
  {
    /* Make sure that Scan stops. */
    CLOSE_HEAPLET(current_free);
  }
  return (get_consing_partition(desired));
}

void
Scan_Heap()
{
  fast Pointer *temp, *end;
  Heaplet *Current_Heaplet;
  Boolean idlep;
  int result;

  idlep = false;

  while (true)
  {
    if (Current_Cons_Heaplet->Heaplet_Scan != current_free)
    {
      /* Let's scan our own consing heaplet */

      Current_Heaplet = Current_Cons_Heaplet;
      Current_Heaplet->Heaplet_Free = current_free;
      temp = Current_Heaplet->Heaplet_Scan;

      if (Debug_Flags[3])
      {
	printf("(on %d/%d) Scanning my consing heaplet %d from 0x%x to 0x%x\n",
	       Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	       Current_Heaplet->Heaplet_No,
	       Current_Heaplet->Heaplet_Scan,
	       Current_Heaplet->Heaplet_Free);
	fflush(stdout);
      }

      scan_heaplet_end = Current_Heaplet->Heaplet_Top;
      end = GCLoop(temp, &current_free);
      Current_Heaplet->Heaplet_Scan = end;
      if (end == current_free)
      {
	/* We are still consing in the same heaplet, but free
	   may have moved during the scan.  Update it for paranoia.
	 */
	Current_Heaplet->Heaplet_Free = current_free;
      }
      else if (end == Current_Heaplet->Heaplet_Free)
      {
	/* We have a new consing heaplet, and we've finished
	   scanning our previous consing heaplet.
	   we should terminate it properly, and scan the new one.
	 */
	TERMINATE_HEAPLET(end, Current_Heaplet->Heaplet_Top);
	continue;
      }

      else
      {
	/* What? */

	sprintf(gc_death_message_buffer,
		"(on %d/%d) Scanning the current heaplet failed",
		Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], temp);
	gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, temp, end);
	/*NOTREACHED*/
      }
    }

    /* We are done with our own stuff,
       try to get something from the queue.
     */

    result = Pop_From_Heaplet_Queue(&Current_Heaplet, &idlep);
    if (result == QUEUE_GOT_ONE)
    {
      /* The swapping of Heaplet_Scan is done early for robustness:
	 If someone else gets a hold of this heaplet and tries to scan it,
	 they are less likely to rescan this region.
       */
      end = Current_Heaplet->Heaplet_Free;
      temp = Current_Heaplet->Heaplet_Scan;
      Current_Heaplet->Heaplet_Scan = end;
      scan_heaplet_end = Current_Heaplet->Heaplet_Top;
      if (Debug_Flags[3])
      {
	printf("(on %d/%d) Scanning heaplet %d from the queue from 0x%x to 0x%x\n",
	       Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	       Current_Heaplet->Heaplet_No, temp, end);
      }
      GCScan("a heaplet (from the queue)", temp, end, true);
      TERMINATE_HEAPLET(end, Current_Heaplet->Heaplet_Top);
    }
    else if (result == QUEUE_ALL_DONE)
    {
      return;
    }
    else
    {
      GC_Delay(GC_IDLE_DELAY);
    }
  }
  /*NOTREACHED*/
}

/*
  Now walk the weak chain and for each WEAK_CONS see if the car has
  already been copied.  If it has, we just fix the code.
*/

Pointer Weak_Chain;

void
Fix_Weak_Chain()
{
  fast Pointer *Old_Weak_Cell, *Scan, Old_Car, Temp, *Old, *Low_Constant;

  Low_Constant = Transport_Bound;
  while (Weak_Chain != NIL)
  {
    Old_Weak_Cell = Get_Pointer(Weak_Chain);
    Scan = Get_Pointer(*Old_Weak_Cell++);
    if (Scan < Transport_Bound) Scan = OTHER_HEAP(Scan);
    Weak_Chain = *Old_Weak_Cell;
    Old_Car = *Scan;
    Temp = Make_New_Pointer(Type_Code(Weak_Chain), Old_Car);
    Weak_Chain = Make_New_Pointer(TC_NULL, Weak_Chain);

    switch(GC_Type(Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      case GC_Special:
	if (Type_Code(Temp) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
	 The BH code updates *Scan if the object has been relocated.
	 Otherwise it falls through and we replace it with a full NIL.
	 Eliminating this assignment would keep old data (pl. of datum).
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	Normal_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Compiled:
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	Compiled_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Undefined:
      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        fprintf(stderr,
		"\nFix_Weak_Chain: Bad Object: Type = 0x%02x; Datum = %x\n",
		Type_Code(Temp), Datum(Temp));
	Microcode_Termination(TERM_INVALID_TYPE_CODE);
    }
  }
  return;
}

/* Shared GC routine */

void
GC()
{
  Pointer *low_constant, *high_constant, temp;

  if (Debug_Flags[27])
    really_spin_for_debugger ("GC");

  /* Get some heap to work with */ 

  scan_heaplet_end = ((Pointer *) 0xfffffffc);
  current_free = get_consing_partition(0);

  /* Scan my section of constant space, my stack,
     and the impure section of code space (everyone).

     In order to reduce contention on the stack and code space,
     the scanning of these areas could be permutted.
  */

  low_constant = SHARED_DATA->Constant_Partition[Who_Am_I].Constant_Start;
  high_constant = SHARED_DATA->Constant_Partition[Who_Am_I].Constant_End;

  if (low_constant < high_constant)
  {
    /*
       NOTE:
       It is assumed that constant space partitions end at constant
       space footers.  The first word of the footer is used to
       store the broken heart that tells the GCLoop to stop scanning.
       The footer will not be scanned, but that is fine, since it
       contains only non pointers.
     */

    high_constant -= 2;
    temp = *high_constant;
    if (temp != (Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1)))
    {
      sprintf(gc_death_message_buffer,
	      "(on %d/%d) Constant Space partition does not have a valid footer",
	      Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
      gc_death(TERM_EXIT, gc_death_message_buffer, high_constant, current_free);
      /*NOTREACHED*/
    }
    GCScan("constant space", low_constant, high_constant, false);
    *high_constant = temp;
  }

  GCScan("stack", Ext_Stack_Pointer, Stack_Top, false);

  GCScan("code space",
	 ((Pointer *) SHARED_DATA->Code_Impure),
	 ((Pointer *) SHARED_DATA->Code_Free),
	 false);

  Scan_Heap();

  terminate_last_consing_partition();

  atomadd(&SHARED_DATA->Consing_Done_Count, -1);

  /* This does not cons, it merely updates pointers. */

  Fix_Weak_Chain();

#ifdef USE_SINGLE_HEAP

  /* We can't copy until everyone else has finished fixing their
     weak conses.
   */

  if (atomadd(&SHARED_DATA->Copy_Ready_Count, -1) > 1)
  {
    do
    {
      GC_Delay(GC_COPY_DELAY);
    } while(SHARED_DATA->Copy_Ready_Count != 0);
  }
  else if (Debug_Flags[6])
  {
    printf("(on %d/%d) Moving data at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }
  
  /* Now transfer all the stuff in the virtual second heap into
     the real first heap.
   */

  {
    fast Pointer *from, *to, *end;

    to = SHARED_DATA->InterpreterTable[Who_Am_I].My_Cons_Bottom;
    end = SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom;
    from = OTHER_HEAP(to);

    if (Debug_Flags[3])
    {
      printf("(on %d/%d) Moving data (0x%x - 0x%x) -> (0x%x - 0x%x)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     from, (from + (end - to)), to, end);
      fflush(stdout);
    }

    while (to < end)
    {
      *to++ = *from++;
    }
  }
  
#endif USE_SINGLE_HEAP

  atomadd(&SHARED_DATA->Slave_Done_Count, -1);
  return;
}

extern void Re_Partition_Constant_Space();

void
Re_Partition_Constant_Space()
{
  if (Debug_Flags[3])
  {
    printf("(on %d/%d) Repartitioning Constant Space; Old = 0x%x; New = 0x%x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   SHARED_DATA->Old_Constant_Top, Free_Constant);
    fflush(stdout);
  }

  if (N_Interps == 1)
  {
    SHARED_DATA->Constant_Partition[0].Constant_Start = Constant_Space;
  }
  else
  {
    fast long i;
    Boolean from_scratch;
    long part_size;

    from_scratch =
      (SHARED_DATA->Old_Constant_Top == ((Pointer *) 0xffffffff));
    part_size = ((Free_Constant - Constant_Space) / N_Interps);

    if (!from_scratch)
    {
      /* Find the last interpreter with a non empty partition. */

      for (i = N_Interps; --i >= 0;)
      {
	if (SHARED_DATA->Constant_Partition[i].Constant_Start <
	    SHARED_DATA->Old_Constant_Top)
	  break;
      }
      if (i < 0)
	i = 0;

      /* Is it worth repartitioning? */

      if ((Free_Constant -
	   (SHARED_DATA->Constant_Partition[i].Constant_Start)) >
	  ((3 * part_size) / 2))
	from_scratch = true;
      else
      {
	/* Update all the pointers */

	SHARED_DATA->Constant_Partition[i].Constant_End = Free_Constant;
	for (i += 1; i < N_Interps; i++)
	{
	  SHARED_DATA->Constant_Partition[i].Constant_Start = Free_Constant;
	  SHARED_DATA->Constant_Partition[i].Constant_End = Free_Constant;
	}
      }
    }

    if (from_scratch)
    {
      fast Pointer *Constant_Start, *Constant_Target_End, *Old;

      Constant_Start = Constant_Space;

      for (i = 0; i < N_Interps; i++)
      {
	Old = Constant_Start;
	SHARED_DATA->Constant_Partition[i].Constant_Start = Constant_Start;
	Constant_Target_End = Constant_Space + (part_size * (i + 1));

	while (Constant_Start < Constant_Target_End)
	{
	  /* Consistency check. */

	  if (OBJECT_TYPE(*Constant_Start) != TC_MANIFEST_SPECIAL_NM_VECTOR)
	  {
	    sprintf(gc_death_message_buffer,
		    "(on %d/%d) Constant space bad format 0x%x at 0x%x\n",
		    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
		    Constant_Start, *Constant_Start);
	    gc_death(TERM_EXIT, gc_death_message_buffer,
		     ((Pointer *) NULL), ((Pointer *) NULL));
	    /*NOTREACHED*/
	  }
	  Constant_Start += Get_Integer(*(Constant_Start + 1)) + 1;
	}
	SHARED_DATA->Constant_Partition[i].Constant_End = Constant_Start;
	Constant_Start = SHARED_DATA->Constant_Partition[i].Constant_End;
      }
    }
  }
  SHARED_DATA->Constant_Partition[N_Interps - 1].Constant_End = Free_Constant;
  SHARED_DATA->Old_Constant_Top = Free_Constant;
  return;
}

/* This assumes (like all the global interrupt code) that
   another global interrupt of the same priority is not sent
   until everyone has processed the previous one, since there
   is a single shared location per priority level.
   Unfortunately the microcode does not currently enforce this.
   If it ever does (by using a queue or a similar mechanism),
   the code below will have to be modified.
   In the meantime, it is consistent with the rest of the code
   and there is not much else that can be done.
*/

void
Save_Global_Interrupt_Handlers()
{
  long count, code, int_number;

  count = 0;
  int_number = 0;
  code = ((FETCH_INTERRUPT_CODE()) & (INT_Global_Mask));

  while (code != 0)
  {
    while ((code & 0x1) == 0)
    {
      int_number += 1;
      code = code >> 1;
    }
    Push(SHARED_DATA->Global_Interrupt_Handlers[int_number]);
    Push(Make_Unsigned_Fixnum(int_number));
    code &= (~0x1);
    count += 1;
  }
  Push(Make_Unsigned_Fixnum(count));
  return;
}

void
Restore_Global_Interrupt_Handlers()
{
  fast long count, index;

  for(count = Get_Integer(Pop()); --count >= 0;)
  {
    index = Get_Integer(Pop());
    SHARED_DATA->Global_Interrupt_Handlers[index] = Pop();
  }
  return;
}

/* Here everybody prepares for the GC by flipping their heaps, and at
   this point it is known that as far as the Scheme world is
   concerned, everybody is quiescent.  
 */

void
Start_Flip()
{
  struct Interpreter_Info *My_Data;
  Pointer *Temp1, *Temp2;
  
  My_Data = &SHARED_DATA->InterpreterTable[Who_Am_I];

#ifndef USE_SINGLE_HEAP

  Temp2 = My_Data->My_Cons2_Top;
  My_Data->My_Cons2_Top = My_Data->My_Cons_Top;
  My_Data->My_Cons_Top = Temp2;

  Temp1 = My_Data->My_Cons2_Bottom;
  My_Data->My_Cons2_Bottom = My_Data->My_Cons_Bottom;
  My_Data->My_Cons_Bottom = Temp1;

#else USE_SINGLE_HEAP

  Temp2 = My_Data->My_Cons_Top;
  Temp1 = My_Data->My_Cons_Bottom;
  
#endif USE_SINGLE_HEAP

  My_Data->My_Free = Temp1;
  My_Data->My_Scan = Temp1;

  SHARED_DATA->Memory_Table[Who_Am_I].Free_Top = Temp2;
  SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom = Temp1;

  /* Other processors need not wait to start the main GC. */

  atomadd(&SHARED_DATA->Flip_Count, -1);

  SHARED_DATA->Memory_Emergency[Who_Am_I] = false;

  Transport_Bound = ((Pointer *) SHARED_DATA->Code_Base);

  initialize_gc_delays();

#ifndef USE_SINGLE_HEAP

  Heap_Top = My_Data->My_Cons_Top;
  Heap_Bottom = My_Data->My_Cons_Bottom;
  Local_Heap_Base = ((SHARED_DATA->Heap_Base == Local_Heap_Base) ?
		     SHARED_DATA->Other_Heap_Base :
		     SHARED_DATA->Heap_Base);
  Unused_Heap = My_Data->My_Cons2_Bottom;
  Unused_Heap_Top = My_Data->My_Cons2_Top;
  Transport_Delta = 0;

#else USE_SINGLE_HEAP

  Transport_Delta = (((char *) SHARED_DATA->Other_Heap_Base) -
		     ((char *) SHARED_DATA->Heap_Base));

#endif USE_SINGLE_HEAP

  Max_Index = sizeof(SHARED_DATA->GC_Data_Area) / sizeof(Heaplet);
 
  Save_Global_Interrupt_Handlers();
  Push(Make_Pointer(TC_HUNK3, History));
  Push(Current_State_Point);
  Push(Fluid_Bindings);
  Push(Fixed_Objects);
  Push(Env);			/* Temporary fix for a compiler problem */
  Push(Undefined_Primitives);
  Push(Undefined_Primitives_Arity);

  /* We need to wait until everyone has updated their data structures. */

  while (SHARED_DATA->Flip_Count > 0)
    GC_Delay(GC_FLIP_DELAY);
  return;
}

void
Restore_GC_State()
{
  Undefined_Primitives_Arity = Pop();
  Undefined_Primitives = Pop();
  Store_Env(Pop());		/* Temporary fix for a compiler problem */
  Fixed_Objects = Pop();
  Fluid_Bindings = Pop();
  Current_State_Point = Pop();
  History = Get_Pointer(Pop());
  Restore_Global_Interrupt_Handlers();
  return;
}

/* Timing stuff. */

static long GC_Start_CPU, GC_Start_Real;

/* Hook called from interp.c */

extern long Finish_Master_GC();

long
Finish_Master_GC()
{
  long CPU_time, real_time;

  /* Let other processors continue. */

  while (SHARED_DATA->End_Ready_Count > 0)
    GC_Delay(GC_DONE_DELAY);

  if (Debug_Flags[6])
  {
    printf("(on %d/%d) GC complete at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  SHARED_DATA->GC_Complete = true;

  CPU_time = (OS_process_clock() - GC_Start_CPU);
  real_time = (OS_real_time_clock() - GC_Start_Real);
  SHARED_DATA->GC_Master_CPU_Time += CPU_time;
  SHARED_DATA->GC_Processor_CPU_Time[Who_Am_I] += CPU_time;
  SHARED_DATA->GC_Master_Real_Time += real_time;
  SHARED_DATA->GC_Processor_Real_Time[Who_Am_I] += real_time;

  if (Debug_Flags[6] || Debug_Flags[3])
    show_gc_delays();

  if (Debug_Flags[3])
  {
    fast int mn;
    fast long memmax, memmin, memnew;

    memmax = (SHARED_DATA->InterpreterTable[0].My_Cons_Top -
	      SHARED_DATA->InterpreterTable[0].My_Free);
    memmin = memmax;
    for (mn = 1; mn < N_Interps; mn++)
    {
      memnew = (SHARED_DATA->InterpreterTable[mn].My_Cons_Top -
		SHARED_DATA->InterpreterTable[mn].My_Free);
      if (memnew > memmax)
	memmax = memnew;
      if (memnew < memmin)
	memmin = memnew;
    }
    printf("%d heaplets created, max mem = 0x%x, min mem = 0x%x\n",
	   SHARED_DATA->Heaplet_Index, memmax, memmin);
    printf("Total Free = %d (0x%x); Total Waste = %d (0x%x)\n",
	   SHARED_DATA->Total_Free, SHARED_DATA->Total_Free,
	   SHARED_DATA->Total_Waste, SHARED_DATA->Total_Waste);
    fflush(stdout);
  }

  /* Note:
     There use to be a test here forcing it to be at least 8192.
     That seems wrong, but ...
   */
     
  return (SHARED_DATA->Total_Free);
}

/*
   This routine does the bulk of the master's garbage collection
   work and is called by MASTER-GC-LOOP and by PRIMITIVE-PURIFY.
*/

void
GC_Do_Work(How_Many_To_Pop)
     int How_Many_To_Pop;
{ void Finish_GC();
  
  Weak_Chain = NIL;
  Finish_GC(How_Many_To_Pop);
}

void
Finish_GC(How_Many_To_Pop)
{ Pointer GC_Daemon_Proc;
  long Saved_Zone;
  fast long i;

  Save_Time_Zone(Zone_GCLoop);

  if (Debug_Flags[3])
  {
    printf("(on %d/%d) Starting the flip.\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    fflush(stdout);
  }

  SHARED_DATA->Memory_Lock = 0;

  /* Re-adjust scan boundaries of constant space and code space. */

  if ((SHARED_DATA->Code_Impure < SHARED_DATA->Code_Free) &&
      (OBJECT_TYPE(*((Pointer *) SHARED_DATA->Code_Impure)) ==
       TC_MANIFEST_SPECIAL_NM_VECTOR))
  {
    fast Pointer *impure, *free;

    if (Debug_Flags[6])
    {
      printf("(on %d/%d) Examining Code Space at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }

    impure = ((Pointer *) SHARED_DATA->Code_Impure);
    free = ((Pointer *) SHARED_DATA->Code_Free);

    do
    {
      impure += (Get_Integer(*impure) + 1);
    } while ((impure < free) &&
	     ((OBJECT_TYPE(*impure)) == TC_MANIFEST_SPECIAL_NM_VECTOR));

    if (Debug_Flags[3])
    {
      printf("(on %d/%d) New impure code boundary;",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
      printf(" Old = 0x%x; New = 0x%x; Max = 0x%x\n",
	     SHARED_DATA->Code_Impure, impure, free);
      fflush(stdout);
    }

    SHARED_DATA->Code_Impure = ((long *) impure);    

    if (Debug_Flags[6])
    {
      printf("(on %d/%d) Done examining Code Space at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }
  }

  if (SHARED_DATA->Old_Constant_Top != Free_Constant)
  {
    if (Debug_Flags[6])
    {
      printf("(on %d/%d) Repartitioning Constant Space at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }
    Re_Partition_Constant_Space();
    if (Debug_Flags[6])
    {
      printf("(on %d/%d) Done repartitioning Constant Space at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }
  }

  Initialize_Heaplet_Queue();

#ifndef USE_SINGLE_HEAP

  if (Debug_Flags[3])
    printf("Local_Heap_Base before flip 0x%x\n", Local_Heap_Base);

#endif USE_SINGLE_HEAP

  Start_Flip();

#ifndef USE_SINGLE_HEAP

  if (Debug_Flags[3])
    printf("Local_Heap_Base after flip 0x%x\n", Local_Heap_Base);

#endif USE_SINGLE_HEAP

  if (Debug_Flags[3] || Debug_Flags[6])
  {
    printf("(on %d/%d) Starting the GC proper at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  GC();

  if (Debug_Flags[3] || Debug_Flags[6])
  {
    printf("(on %d/%d) Finished the GC proper at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  CLEAR_INTERRUPT(INT_GC);	/* Slaves do it in slave-before-sync */

  Restore_GC_State();
  SHARED_DATA->FObj = Fixed_Objects;

  Pop_Primitive_Frame(How_Many_To_Pop);

  /* Wait until other processors release their consing partitions which
     may be in my heap.
   */

  while (SHARED_DATA->Consing_Done_Count > 0)
  {
    GC_Delay(GC_CONSING_DELAY);
  }

  Grab_Heap_Space_After_GC();
  atomadd(&SHARED_DATA->End_Ready_Count, -1);

  /* Wait until other processors update the weak pairs they have found,
     and possibly move the contents of their heap from the virtual heap
     to the real heap.
   */

  while (SHARED_DATA->Slave_Done_Count > 0)
  {
    GC_Delay(GC_MASTER_DELAY);
  }

  if (Debug_Flags[6])
  {
    printf("(on %d/%d) about to invoke GC daemons at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);

  if (GC_Daemon_Proc == NIL)
  {
   Will_Push(CONTINUATION_SIZE);
    Store_Return(RC_NORMAL_GC_DONE);
    Store_Expression(Make_Unsigned_Fixnum(0));
    Save_Cont();
   Pushed();

    Restore_Time_Zone();
    PRIMITIVE_ABORT(PRIM_POP_RETURN);
    /*NOTREACHED*/
  }

 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+1));
  Store_Return(RC_NORMAL_GC_DONE);
  Store_Expression(Make_Unsigned_Fixnum(0));	
  Save_Cont();
  Push(GC_Daemon_Proc);
  Push(STACK_FRAME_HEADER);
 Pushed();

  Restore_Time_Zone();
  PRIMITIVE_ABORT(PRIM_APPLY);
}

DEFINE_PRIMITIVE("SLAVE-GC-BEFORE-SYNC", Slave_Before_Sync, 0)
{
  PRIMITIVE_HEADER(0);

  CLEAR_INTERRUPT(INT_GC);
  GC_Start_CPU = OS_process_clock();
  GC_Start_Real = OS_real_time_clock();

  PRIMITIVE_RETURN(TRUTH);
}

DEFINE_PRIMITIVE("SLAVE-GC-AFTER-SYNC", Slave_After_Sync, 0)
{
  long Saved_Zone;
  PRIMITIVE_HEADER(0);

  Save_Time_Zone(Zone_GCIdle);
  Weak_Chain = NIL;
  Start_Flip();
  GC();

  /* Wait until other processors release their consing partitions,
     which may be in my heap.
   */

  while (SHARED_DATA->Consing_Done_Count != 0)
    GC_Delay(GC_CONSING_DELAY);

  Grab_Heap_Space_After_GC();
  atomadd(&SHARED_DATA->End_Ready_Count, -1);

  Restore_GC_State();

  /* Wait until the daemons have been run, etc. */

  while (!SHARED_DATA->GC_Complete)
    GC_Delay(GC_DONE_DELAY);

  if (Debug_Flags[6] || Debug_Flags[3])
    show_gc_delays();

  SHARED_DATA->GC_Processor_CPU_Time[Who_Am_I] +=
    OS_process_clock() - GC_Start_CPU;
  SHARED_DATA->GC_Processor_Real_Time[Who_Am_I] +=
    OS_real_time_clock() - GC_Start_Real;

  Restore_Time_Zone();
  PRIMITIVE_RETURN(TRUTH);
}

DEFINE_PRIMITIVE("MASTER-GC-BEFORE-SYNC", Master_Before_Sync, 0)
{
  long Saved_Zone;
  PRIMITIVE_HEADER(0);

  Save_Time_Zone(Zone_GCLoop);

  if (Debug_Flags[6])
  {
    printf("(on %d/%d) MASTER-GC-BEFORE-SYNC at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  GC_Start_CPU = OS_process_clock();
  GC_Start_Real = OS_real_time_clock();

  if ((Consistency_Check) &&
      (Free > SHARED_DATA->InterpreterTable[Who_Am_I].My_Cons_Top))
  {
    fprintf(stderr,
	    "(on %d/%d) Used up all of the heap -- better shut down.\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
    Microcode_Termination(TERM_NO_SPACE);
  }

  SHARED_DATA->Heaplet_Index = 0;
  SHARED_DATA->GC_Complete = false;
  SHARED_DATA->Flip_Count = N_Interps;
  SHARED_DATA->Consing_Done_Count = N_Interps;
  SHARED_DATA->Copy_Ready_Count = N_Interps;
  SHARED_DATA->Slave_Done_Count = N_Interps;
  SHARED_DATA->End_Ready_Count = N_Interps;
  SHARED_DATA->Total_Waste = 0;
  SHARED_DATA->Total_Free = 0;
  Restore_Time_Zone();
  PRIMITIVE_RETURN(TRUTH);
}

/* (MASTER-GC-LOOP RESERVE)
   Requests a garbage collection from the current heap into the
   spare heap, retaining RESERVE cells as the guard to begin the
   next GC.  The primitive ends by invoking the GC daemon process
   if there is one. Otherwise it returns the new address of the top
   of the heap.
*/

DEFINE_PRIMITIVE("MASTER-GC-LOOP", Master_GC, 1)
{
  long new_value;
  Pointer result;
  Primitive_1_Arg();

  if (Debug_Flags[6])
  {
    printf("(on %d/%d) MASTER-GC-LOOP at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  new_value = Get_Integer(Arg1);
  if (new_value != SHARED_DATA->GC_Reserve)
  {
    SHARED_DATA->Queue_Overflow =
      min(SHARED_DATA->Queue_Overflow,
	  min((SHARED_DATA->Work_Queue.queue_max - (2 * N_Interps)),
	      ((new_value - 1000) / 2)));
    SHARED_DATA->GC_Reserve = new_value;
  }
  GC_Do_Work(1);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE("GC-NEEDED?", GC_Needed, 0)
{
  PRIMITIVE_HEADER(0);

  /* Can we grab it ourself? */

  if (Grab_Heap_Space(false))
  {
    CLEAR_INTERRUPT(INT_GC);
    PRIMITIVE_RETURN(NIL);
  }

  /* No interrupt pending -> someone beat us! ????EXTRANEOUS???? */

  if ((IntCode & INT_GC) == 0 && Grab_Heap_Space(false))
  {
    CLEAR_INTERRUPT(INT_GC);
    PRIMITIVE_RETURN(NIL);
  }

  /* Otherwise we had better GC. */

  PRIMITIVE_RETURN(TRUTH);
}
