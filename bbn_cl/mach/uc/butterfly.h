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

/* -*-C-*- */

#include <mach.h>
#include <sys/cluster.h>
#include <sys/kern_return.h>

#define Command_Line_Hook()	Butterfly_Command_Line(argc, argv)

/* Swapping must really be atomic */

extern Pointer Atomic_Swap_Pointers();
#define Swap_Pointers Atomic_Swap_Pointers

#define End_GC_Hook()							\
{									\
  extern long Finish_Master_GC();					\
									\
  Val = Make_Unsigned_Fixnum(Finish_Master_GC());			\
}

extern Pointer Global_Int_Part_2();

#define Global_Interrupt_Hook()						\
if (((1 << Int_Number) & INT_Global_Mask) != 0)				\
{									\
  CLEAR_INTERRUPT((1 << Int_Number));					\
  Handler = SHARED_DATA->Global_Interrupt_Handlers[Int_Number];		\
  goto Passed_Checks;							\
}

/* Fasl and Band file support */

#define Band_Dump_Permitted()						\
{									\
  if (SHARED_DATA->N_Interpreters > 1)					\
    Primitive_Error_String						\
      ("Attempting to BAND DUMP with more than one interpreter",	\
       NIL);								\
}

#define MAX_INTERPRETERS	128	/* Maximum number of processors */
#define HSTACK_SIZE		128	/* How much of the stack do we save on a fault */

/* Definition of the heaplet data structure used during GC */

typedef struct
{
  Pointer *Heaplet_Free;
  Pointer *Heaplet_Scan;
  Pointer *Heaplet_Top;
  short int Heaplet_No;
  short int padding;
} Heaplet;

struct Gc_Data
{
  Heaplet Heaplet_Array[8192];
};

#define GC_DATA         ((struct Gc_Data *) (SHARED_DATA->GC_Data_Area))

/* Structure to hold Constant space partitioning information
   Used by bf-gc.c for garbage collecting pure/constant space.
 */

struct space_partition
{
  Pointer *Constant_Start;
  Pointer *Constant_End;
};

struct Interpreter_Info
{
  Pointer *My_Scan;
  Pointer *My_Free;
  Pointer *My_Cons_Top;		/* Virtual address of end of heap 1 */
  Pointer *My_Cons_Bottom;	/* Virtual address of start of heap 1 */
  Pointer *My_Cons2_Top;	/* Virtual address of end of heap 2 */
  Pointer *My_Cons2_Bottom; 	/* Virtual address of start of heap 2 */
};

#define FQ_Nitems(queue) ((&(queue))->queue_count)

struct fifo_queue
{
  short int queue_lock;
  short int queue_count;
  short int queue_max;
  short int queue_put;
  short int queue_get;
  short int queue_busy_count;	/* Used to be pad */
  long *queue_content;
};


struct priority_queue
{
  short int queue_length;
  short int queue_lock;
  short int priority;
  struct priority_queue *next;
  Pointer *head;
  Pointer *tail;
};
  


/* The following data structure is stored in a single, shared communications
   segment which is mapped into each process's address space at segment 0xF6

   IMPORTANT:
   The alignment below is crucial since the atomic operations have
   particular alignment constraints.
   Therefore, if you punt a field, make sure that the alignment
   of all the fields below the one punted remains the same!
*/

struct Shared_Data_Type {

  Pointer FObj;			/* Shared copy of the fixed objects vector */
				/* This must be first for cmp68020.m4! */
  struct fifo_queue Work_Queue;

  short int Queue_Overflow;
  short int GC_Propagate_Lock;	/* Lock on GC_Data_Area */

  short int Subsumption_Numerator;
  short int Subsumption_Denominator;

  cluster_id_t Cluster_Id;

  long N_Interpreters;	/* Number of interpreters */

  short int Idle_Processor_Count;
  short int Idle_Processor_Level;

  short int Signal_Lock;
  short int Signal_Meaning;
  long Signal_Data_Length;
  long Signal_Data[128];

  short int Global_Int_Lock;
  short int Global_Int_Count;

  Pointer Global_Interrupt_Handlers[MAX_INTERRUPT_NUMBER + 1];

  long File_Server_Pipe[2];
  long File_Answer_Pipe[2];

  short int File_Server_Lock;
  short int Hardware_Trap_Count;

  Pointer Hardware_Trap_PC;
  Pointer Hardware_Trap_Irritant;
  Pointer Hardware_Trap_Code;
  Pointer Hardware_Trap_Task_Code;
  Pointer Hardware_Trap_Task_Name;
  Pointer Hardware_Trap_Task_Number;
  Pointer Hardware_Trap_Task_Spawn;
  Pointer Hardware_Trap_Stack[HSTACK_SIZE];

  long Task_Id[MAX_INTERPRETERS];

  struct Interpreter_Info InterpreterTable[MAX_INTERPRETERS];

  short int Special_Synch;
  short int Code_Lock;

  long *Code_Base;		/* Base + Top and Free pointer for Code */
  long *Code_Impure;
  long *Code_Free;
  long *Code_Top;

  Pointer *Bfly_Constant_Space;
  Pointer *Bfly_Free_Constant;
  Pointer *Bfly_Constant_Top;

  long GC_Reserve;	/* Save this much space in heap for gc */

  short int Memory_Lock;
  short int Constant_Space_Lock; /* For use by Atomic_Allocate */

  Boolean Memory_Emergency[MAX_INTERPRETERS];

  struct {
    Pointer *Free_Bottom;
    Pointer *Free_Top; } Memory_Table[MAX_INTERPRETERS];

  short int Sync_Lock;
  short int Sync_Count;

  short int Slave_Done_Count;
  short int Update_Lock;

  short int Max_Partitions;
  short int Partition_Allocation_Lock;

  short int Total_Waste;
  short int Flip_Count;

  Pointer *Old_Constant_Top;

  short int Contention_Count;
  short int Future_Meter_Lock;	/* Lock on the number below. */

  long Future_Meter_Number;	/* Unique tag for the next future. */

  short int Exit_In_Progress;
  short int Dont_Try_Interrupting;

  long Heap_Chunk_Size;		/* Size of each Chunk for locking */
  Pointer *Heap_Base;		/* The base addresses of the heap for locking */
  Pointer *Other_Heap_Base;

  short int pad1;
  short int Error_Interlock;	/* Serialize error output */

  long Useful_Meters[32];

  long Initial_Real_Time;
  long Initial_Real_Millitm;
  long Initial_Process_Time;
  long Initial_System_Process_Time;

  short int pad2;
  short int Compiler_Recache_Lock;

  FILE *Bfly_Channels[FILE_CHANNELS];

  long Constant_Lock;
  long Heap_Locks[MAX_INTERPRETERS];

  Pointer Bfly_Undef_Prims;
  Pointer Bfly_Undef_Arity;

  struct space_partition Constant_Partition[MAX_INTERPRETERS];

  short int Heaplet_Index;
  short int GC_Complete;

  long GC_Array[MAX_INTERPRETERS];

  char Processor_Zone[MAX_INTERPRETERS];
  long Processor_Free[MAX_INTERPRETERS];

  char Band_Load_Name[256];

  long GC_Processor_Real_Time[MAX_INTERPRETERS]; /* Times are in centiseconds */
  long GC_Master_Real_Time;

  long GC_Processor_CPU_Time[MAX_INTERPRETERS];	/* Times are in centiseconds */
  long GC_Master_CPU_Time;

  long Total_Free;

  short int Copy_Ready_Count;
  short int Consing_Done_Count;

  short int End_Ready_Count;
  short int Intern_Lock;

  short int File_Open_Lock;
  short int Work_Queue_Lock;

  short int Work_Queue_Length;
  short int padding;

  struct priority_queue *The_New_Improved_Work_Queue;

  long Ext_Msg_Ack; /* Used by input server to assure input data received before transmitting more */

  long debug_spin_flag;

  long reserved_for_locks[123];

  char GC_Data_Area[131072];
} *SHARED_DATA;

/* The following are static data PRIVATE to each processor */

extern long Who_Am_I;	/* My interpreter number */
extern long N_Interps;	/* Private copy from shared area */

/* Procedures & macros for mapping other interpreter's data */

extern Map_Data(), Unmap_Data();

#define Get_Mapped_Address(Type, My_Variable)	\
((Type *) (((char *) (My_Variable)) -	\
	   ((char *) Private_Low_Bound) +	\
	   ((char *) PEEKING_AREA)))

#define Get_Mapped(Type, My_Variable)   	\
(* Get_Mapped_Address(Type, &(My_Variable)))

#define New_Future_Number()	Get_New_Future_Number()

/* This is to get around a bug in the Green Hills C compiler! */
#define void int

/*	Added to the GC table */
#define GC_Weak 7
#define GC_Trap 8

/*	We can't use the in-line code for variable lookup in interpret.h */
#define No_In_Line_Lookup

#define FASLOAD_RELOCATE_HOOK(from_band_load,				\
			      Heap_Low, Heap_High,			\
			      Constant_Low, Constant_High,		\
			      Code_Low, Code_High)			\
{									\
  if (!from_band_load)							\
  {									\
    Move_Code_To_Codespace(Heap_Low, Heap_High);			\
    Move_Code_To_Codespace(Constant_Low, Constant_High);		\
  }									\
  Propagate_Code(Code_Low, Code_High);					\
}

#define CODE_SPACE		/* A fact on the butterfly */

#define Code_Space_Base() ((long) SHARED_DATA->Code_Base)
#define Code_Space_Size() ((long) (SHARED_DATA->Code_Free - SHARED_DATA->Code_Base))
#define Code_Space_Check(size) (SHARED_DATA->Code_Free + (size) > \
		SHARED_DATA->Code_Top)
#define Code_Space_Free() ((Pointer *) SHARED_DATA->Code_Free)
#define Code_Space_Reset() SHARED_DATA->Code_Free = SHARED_DATA->Code_Base;
#define Code_Space_Set(new) SHARED_DATA->Code_Free = (new);
#define Code_Space_Lock() \
  while (Atomic_ior(&SHARED_DATA->Code_Lock, 0x8000) == 0x8000);
#define Code_Space_Unlock() SHARED_DATA->Code_Lock = 0;

/* Constants for sections, the block of
   data manipulated by the code-space "section faulter" */

#define SECTIONSIZE   0x10000     /* 2^16 (Must be a power of 2) */
#define SECTIONMASK   (~(SECTIONSIZE - 1))
#define SECTIONS(Address)   (((Address) + SECTIONSIZE - 1) & SECTIONMASK)

#define Absolute_Stack_Base Bfly_Stack_Base
extern Pointer *Bfly_Stack_Base;

#define Might_Be_Non_Terminal(Err) \
  if (((Err) == TERM_NO_SPACE) || ((Err) == TERM_GC_OUT_OF_SPACE)) \
     GC_Error_To_Real_Error();

#define Constant_Space SHARED_DATA->Bfly_Constant_Space
#define Free_Constant SHARED_DATA->Bfly_Free_Constant
#define Constant_Top SHARED_DATA->Bfly_Constant_Top

#define Undefined_Primitives SHARED_DATA->Bfly_Undef_Prims
#define Undefined_Primitives_Arity SHARED_DATA->Bfly_Undef_Arity

#define Channels SHARED_DATA->Bfly_Channels

#undef task_self		/* Get around a Mach problem! */

#define Fixed_Objects SHARED_DATA->FObj
