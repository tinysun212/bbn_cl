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

#include "scheme.h"
#include "primitive.h"
#include "transact.h"
#include "locks.h"
#include <signal.h>
#include <fcntl.h>
#include "fasl.h"

#define PAGE_SIZE		0x2000
#define PAGE_MASK		(~(PAGE_SIZE-1))
#define PAGES(n)		(((n) + (PAGE_SIZE - 1)) & PAGE_MASK)

#define MARGIN_FOR_MALLOC (256 * 1024)

#define MEMORY_QUANTUM_DEFAULT 32

extern Pointer *Absolute_Heap_Base;

long Who_Am_I, N_Interps, Local_Chunk_Size, Partition_Size, Memory_Quantum;
long New_Time, Old_Time;
Pointer *Local_Heap_Base, *Bfly_Stack_Base, *Absolute_Heap_Base;

long Desired_Nints, Desired_Mems, N_Mems;
long Queue_Size;
long N_Code_Space, N_Cee_Space;
long real_stack_size;
long Desired_Heap_Size;

Boolean boot_band_p, Magic_Flip, Use_Current_Cluster;

extern int Debug_Flags[];

char *boot_name;

/*
  Parse the command line for:

	-nint		Use this many interpreters
				Desired_Nints
	-nmem		Allocate this many memory only processors.
				Desired_Mems
	-code		Allocate this many K pointers for code space
				N_Code_Space
	-qsize		Allow this many entries on the Queue.
				Queue_Size
	-quantum	Basic GC memory quantum
				Memory_Quantum
	-heap		Desired heap size (otherwise use all available memory)
				Desired_Heap_Size
 	-flipped	Reverse the order of heap pieces
 				Magic_Flip
	-auto		Automatically set the values of -code and -constant
				N_Code_Space, Constant_Size
        -ucc            Use Current Cluster -- will not create its own cluster

	-xnnn
        -Xnnn           Set debug flag nnn (cap version is for csh shell scripts so does not
                                            interfere with -x file query)
				Debug_Flags[nnn]
			0 -> fasload stuff
			1 -> miscellaneous startup
			2 -> file server stuff
			3 -> GC and purify
			4 -> definitions and assignments
			5 -> get/set fixed objects
			6 -> timing fasload and GC
			7 -> lock/unlock cell
			8 -> allocation & startup
			9 -> interrupts
			10 -> signal data mechanism
			11 -> lookup
			12 -> atomic allocate
			13 -> band dumping
			14 -> code space
			15 -> disable SIGVEC handler
			16 -> new memory allocator
 			17 -> code space GC hack
 			18 -> hardware trap should loop for dbx
 			19 -> code gc
 			20 -> purify code space
			21 -> butterfly I/O
			22 -> get place in code
			23 -> section fault stuff
			24 -> scheduler
			25 -> futures
			26 -> test allocates: don't really create a cluster nor
			      allocate any memory; exit before forking anything.
			27 -> Loop upon entering GC.
			28 -> Loop upon entering band load.
			29 -> Loop upon entering global interrupt primitive.
*/

#define BLOCKSIZE	1024
#define BLOCKS(x)	(x * BLOCKSIZE)
#define UNBLOCKS(x)	((x + (BLOCKSIZE - 1)) / BLOCKSIZE)

#define DEFAULT_CODE_SIZE	512

/* These parameters specify how much to grow the code and constant
   areas when the -auto option is present with respect to the minimum
   sizes required by the band.
 */

#define AUTO_CODE_FACTOR	0.3
#define AUTO_CONSTANT_FACTOR	0.3
#define AUTO_CODE_DELTA		50
#define AUTO_CONSTANT_DELTA	50

#define BRK_BUMP		0x03000000	/* We will push brk to this location (48MB) */

void
Butterfly_Command_Line(argc, argv)
     int argc;
     char *argv[];
{
  int file_exists();
  int position, i, default_nints;
  char *getenv();

  default_nints = 0;

  for (position = 1; position < argc; position++)
    if (argv[position][0] == '-' &&
	(argv[position][1] == 'x' ||
	 argv[position][1] == 'X'))
    {
      i = atoi(&argv[position][2]);
      Debug_Flags[i] = true;
    }

  boot_band_p = true;
  boot_name = DEFAULT_BAND_NAME;

  position = Parse_Option("-band", argc, argv, true);
  if (position != NOT_THERE)
  {
    if (position == argc - 1)
    {
      fprintf(stderr, "%s: The -band option requires a file name\n", argv[0]);
      exit(1);
    }
    boot_name = argv[position + 1];
  }

  position = Parse_Option("-fasl", argc, argv, true);
  if (position != NOT_THERE)
  {
    if (position == argc - 1)
    {
      fprintf(stderr, "%s: The -fasl option requires a file name\n", argv[0]);
      exit(1);
    }
    default_nints = 1;		/* Change the default. */
    boot_band_p = false;
    boot_name = argv[position + 1];
  }


  Magic_Flip = Parse_Option("-flipped", argc, argv, true) != NOT_THERE;

  Use_Current_Cluster = Parse_Option("-ucc", argc, argv, true) != NOT_THERE;

  Desired_Nints = Def_Number("-nint", argc, argv, default_nints);
  Desired_Mems =  Def_Number("-nmem", argc, argv, 0);

  N_Code_Space = Def_Number("-code", argc, argv, DEFAULT_CODE_SIZE);
  N_Code_Space *= (sizeof(Pointer) * 1024);
  N_Code_Space = SECTIONS(N_Code_Space);

  Desired_Heap_Size = Def_Number("-heap", argc, argv, -1);
  if (Desired_Heap_Size > 0)
    Desired_Heap_Size *= sizeof(Pointer) * 1024 * 2;

  Queue_Size = Def_Number("-qsize", argc, argv, 4096);

  Memory_Quantum = -1;
  position = Parse_Option("-quantum", argc, argv, true);
  if (position != NOT_THERE)
    Memory_Quantum = 1024 * Def_Number("-quantum", argc, argv,
				       MEMORY_QUANTUM_DEFAULT);

  if ((file_exists(boot_name)) <= 0)
  {
    fprintf(stderr, "%s: The %s file %s does not exist\n",
	    argv[0], boot_band_p ? "band" : "fasl", boot_name);
    exit(1);
  }

  if (Parse_Option("-auto", argc, argv, true) != NOT_THERE && boot_band_p)
  {
    long fd, header[FASL_HEADER_LENGTH], heap, pure, code, delta;
  
    fd = open(boot_name, O_RDONLY, 0666);
    if (fd < 0)
    {
      fprintf(stderr, "%s: Unable to open %s to autosize it\n",
	      argv[0], boot_name);
      exit(1);
    }
    if (read(fd, header, sizeof(header)) != sizeof(header)
	|| header[FASL_Offset_Marker] != FASL_FILE_MARKER)
    {
      fprintf(stderr, "%s: %s is not a FASL format file\n",
	      argv[0], boot_name);
      exit(1);
    }
    close(fd);

    heap = UNBLOCKS(Get_Integer(header[FASL_Offset_Heap_Count]));
    pure = UNBLOCKS(Get_Integer(header[FASL_Offset_Const_Count]));
    code = UNBLOCKS(Get_Integer(header[FASL_Offset_Code_Count]));
    printf("This file requires -heap %d -constant %d -code %d\n",
	   heap, pure, code);
    delta = ((long) (AUTO_CONSTANT_FACTOR * ((double) pure)));
    pure += ((delta > AUTO_CONSTANT_DELTA) ? delta : AUTO_CONSTANT_DELTA);
    delta = ((long) (AUTO_CODE_FACTOR * ((double) code)));
    code += ((delta > AUTO_CODE_DELTA) ? delta : AUTO_CODE_DELTA);
    if (Parse_Option("-code", argc, argv, true) == NOT_THERE)
      N_Code_Space = SECTIONS(sizeof(Pointer) * BLOCKS(code));
    if (Parse_Option("-constant", argc, argv, true) == NOT_THERE)
      Constant_Size = pure;
    printf("Autosizing to -constant %d -code %d\n",
	   Constant_Size,
	   UNBLOCKS(N_Code_Space / sizeof(Pointer)));
  }

  if (Desired_Nints > MAX_INTERPRETERS)
  {
    fprintf(stderr, "%s: The system may only configure %d interpreters\n",
	    argv[0], MAX_INTERPRETERS);
    exit(1);
  } 
  return;
}

/* This is used at boot time and after disk restore to make sure
   that we have a blank slate.
 */

extern void
  reset_shared_data();

void
reset_shared_data()
{
  fast long i;

  Initialize_Queue(&SHARED_DATA->Work_Queue,
		   SHARED_DATA->Work_Queue.queue_content,
		   Queue_Size);

/* Initialize The_New_Improved_Work_Queue */
  SHARED_DATA->The_New_Improved_Work_Queue = NULL;
  SHARED_DATA->Work_Queue_Lock = 0x0000; 
  SHARED_DATA->Work_Queue_Length = 0x0000; 

  SHARED_DATA->Queue_Overflow = Queue_Size / 2;
  SHARED_DATA->Subsumption_Numerator = 0;
  SHARED_DATA->Subsumption_Denominator = 0;

  SHARED_DATA->Idle_Processor_Count = 0;
  SHARED_DATA->Idle_Processor_Level = 0;

  SHARED_DATA->Constant_Space_Lock = 0x0000;

  SHARED_DATA->Global_Int_Lock = 0;
  SHARED_DATA->Global_Int_Count = 0;

  for (i = 0; i <= MAX_INTERRUPT_NUMBER; i++)
    SHARED_DATA->Global_Interrupt_Handlers[i] = NIL;

  SHARED_DATA->Hardware_Trap_Count = 0;
  SHARED_DATA->Hardware_Trap_PC = NIL;
  SHARED_DATA->Hardware_Trap_Irritant = NIL;
  SHARED_DATA->Hardware_Trap_Code = NIL;

  SHARED_DATA->GC_Reserve = SHARED_DATA->Queue_Overflow * 2 + 1000;
  SHARED_DATA->Update_Lock = 0;

  SHARED_DATA->Dont_Try_Interrupting = 0;
  SHARED_DATA->Exit_In_Progress = 0;

  SHARED_DATA->Intern_Lock = 0;
  SHARED_DATA->File_Open_Lock = 0;

  /* All the parameters are set ready for a GC.
     The first purification (when fasl'ing) is done without the
     gc interface.
   */

  SHARED_DATA->Heaplet_Index = 0;
  SHARED_DATA->GC_Complete = false;
  SHARED_DATA->Flip_Count = N_Interps;
  SHARED_DATA->Consing_Done_Count = N_Interps;
  SHARED_DATA->Copy_Ready_Count = N_Interps;
  SHARED_DATA->Slave_Done_Count = N_Interps;
  SHARED_DATA->End_Ready_Count = N_Interps;
  SHARED_DATA->Total_Waste = 0;
  SHARED_DATA->Total_Free = 0;

  SHARED_DATA->GC_Master_Real_Time = 0;
  SHARED_DATA->GC_Master_CPU_Time = 0;
  for (i = 0; i < MAX_INTERPRETERS; i++)
  {
    SHARED_DATA->GC_Processor_Real_Time[i] = 0;
    SHARED_DATA->GC_Processor_CPU_Time[i] = 0;
  }

  SHARED_DATA->GC_Propagate_Lock = 0;
  SHARED_DATA->Code_Lock = 0;
  SHARED_DATA->Compiler_Recache_Lock = 0;
  Initialize_Heap_Locks();

  SHARED_DATA->Old_Constant_Top = ((Pointer *) 0xffffffff);
  SHARED_DATA->Code_Impure = SHARED_DATA->Code_Base;
  SHARED_DATA->debug_spin_flag = 0;
  return;
}

extern void
  init_subprocess();

void
init_subprocess(mach_init_p)
     Boolean mach_init_p;
{
  if (mach_init_p)
    mach_init();
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  signal(SIGTSTP, SIG_DFL);
  return;
}

void
Wait_For_Shutdown()
{				/* Wait for child tasks to start dying. */
  int pid, status;

  pid = wait(&status);
  if (Debug_Flags[2])
    printf ("Wait returned; pid = %d; status = 0x%8x\n", pid, status);
  return;
}

void
kill_the_lisp_tasks(last)
     int last;
{
  int i;
  
  for (i = 0; (i < last); i++)
  {
    if (Debug_Flags[2])
      printf ("Kill_the_lisp_tasks: SIGTERM %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
    kill(SHARED_DATA->Task_Id[i], SIGTERM);
  }
  if (fork() <= 0)
  {
    /* This kludge allows the allocator to die immediately
       so that the shell will get immediate control of the tty.
       A temporariy process waits for 2 secs. and then really kills
       the children.
     */
    sleep(2);
    for (i = 0; (i < last); i++)
    {
    if (Debug_Flags[2])
      printf ("Kill_the_lisp_tasks: SIGKILL %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
      kill(SHARED_DATA->Task_Id[i], SIGKILL);
    }
    _exit(0);
  }
  return;
}

void 
suspend_the_lisp_tasks()
{
  long i;

  for (i = 0; i < N_Interps; i++)
    {
    if (Debug_Flags[2])
      printf ("Suspend_the_lisp_tasks: SIGSTOP %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
      kill(SHARED_DATA->Task_Id[i], SIGSTOP);
    }
}

void 
resume_the_lisp_tasks()
{
  long i;

  for (i = 0; i < N_Interps; i++)
    {
      if (Debug_Flags[2])
	printf ("Resume_the_lisp_tasks: SIGCONT %d -> %d\n", getpid(), SHARED_DATA->Task_Id[i]);
      kill(SHARED_DATA->Task_Id[i], SIGCONT);
    }
}

static int the_allocator_id;

void
kill_all_the_tasks()
{
  extern void kill_the_servers();

  if (Debug_Flags[2])
    printf ("Kill_all_the_tasks: SIGTERM %d -> %d\n", getpid(), the_allocator_id);

  kill(the_allocator_id, SIGTERM);
  kill_the_servers();
  return;
}

/* Signal handlers */

void
kill_all_lisp_tasks(sig)
     int sig;
{
  if (Debug_Flags[2])
    {
      char *find_signal_name();
      char *signame = find_signal_name (sig);
      printf ("Kill_all_lisp_tasks process %d signal %s (0x%x)\n", getpid(), signame, sig);
    }
  kill_the_lisp_tasks(N_Interps);
  _exit(0);
}

void
kill_everything(sig)
     int sig;
{
  if (Debug_Flags[2])
    {
      char *find_signal_name();
      char *signame = find_signal_name (sig);
      printf ("Kill_everything process %d signal %s (0x%x)\n", getpid(), signame, sig);
    }
  kill_all_the_tasks();
  _exit(0);
}

#ifndef USE_SINGLE_HEAP

#define HEAP_FACTOR			2
#define OTHER_HEAP_BASE(base, size)					\
  ((Pointer *) (base + size))

#else USE_SINGLE_HEAP

#define HEAP_FACTOR			1
#define OTHER_HEAP_BASE(base, size)					\
  ((Pointer *) (((char *) Code_Copy_Area) + Code_Copy_Size))

#endif USE_SINGLE_HEAP


long
my_cluster_id()
{
  struct home_node_data my_home_node_data;
  long return_count;
  kern_return_t code;

  code = cluster_stat((cluster_id_t) 0, GET_HOME_NODE,
		      &my_home_node_data, &return_count);
  if (code != KERN_SUCCESS)
    {
      fprintf(stderr, "cluster_stat/home node info in my _cluster_id. code = %d\n", code);
      exit(1);
    }
  return my_home_node_data.home_cluster_id;
}

long 
n_nodes_in_cluster(cluster_id)
cluster_id_t cluster_id;
{
  kern_return_t code;
  struct home_node_data my_home_node_data;
  short int node_list[NCPUS];
  long return_count;

  code = cluster_stat(cluster_id, GET_NODE_LIST,
		      node_list, &return_count);
  if (code != KERN_SUCCESS)
    {
      fprintf(stderr, "cluster_stat/get node list in n_nodes_in_cluster failed. code = %d\n", code);
      exit(1);
    }
  return return_count;
}

/*
	Allocate memory and set up inheritance for:
	- the work queue
	- the shared data
	- the GC temporary space
	- constant space
	- the heap

   NOTE!!! This code depends on the virtual address layout determined by the
   operating system. We assume sbrk will start at a low address. We require
   all addresses for tagged Lisp data to be at most 24 bits in size (16MB).

   To guarantee that we get the spaces we need, an initial sbrk(0) is done
   to find the lowest address. Then, we brk to 48MB, and immediately deallocate
   that space. This insures that sbrk will not interfere with our allocations.

   Use of vm_allocate/anywhere in the Lisp system should be avoided, to retain
   compatability between old and new vm systems.
*/

long
  *Code_Copy_Area,		/* Locked with GC_Propagate_Lock */
  Code_Copy_Size,		/* The size is Code_Top - Code_Base in bytes */
  Half_Heap_Size;		/* The size of a heap in Pointers */

void
Setup_Memory(Heap_Size, Stack_Size, Constant_Size)
     long Heap_Size, Stack_Size, Constant_Size;
{
  extern char
    *malloc();
  extern void
    start_the_servers();
  char
    *Allocate_Shared_Memory();
  void
    Allocate_Data_In(),
    initialize_allocation_parameters();
  long
    Normalize_Size();

  extern long
    end, etext;
  int
    nnodes, i, allocator_id, child, nmemonly;
  long
    heap_size, half_heap_size, chunk_size;
  char
    death_buffer[4], *malloc_margin,
    *heap_bottom, *heap_top, *heap1_now, *heap2_now,
    *p;
  kern_return_t
    code;
  FILE
    *killer_file;

  heap_bottom = (char *) sbrk (0);

  if (Debug_Flags[1])
    printf ("Initial heap_bottom via sbrk: 0x%x\n", (int) heap_bottom);

  if ((int)heap_bottom <= 0)
    {
      fprintf (stderr, "Sbrk could not find lowest address (code %d)\n", (int) heap_bottom);
      exit (1);
    }
  	/* Ceiling to next page */
  heap_bottom = (char *)(((int) heap_bottom + PAGE_SIZE - 1) & PAGE_MASK);

  	/* Allocate to 48 MB point */
  if (brk (BRK_BUMP) < 0)
    {
      fprintf (stderr, "Brk could not allocate temporary init space");
      exit (1);
    }

  if (Debug_Flags[1])
    printf ("Bumped brk: 0x%x\n", (int) sbrk (0));

  code = vm_deallocate (task_self(), heap_bottom, BRK_BUMP - heap_bottom);
  if (code != KERN_SUCCESS)
  {
    fprintf (stderr, "Cannot deallocate temporary init space (code %d)\n", code);
    exit (1);
  }

  SHARED_DATA =
    ((struct Shared_Data_Type *)
     Allocate_Shared_Memory(sizeof(struct Shared_Data_Type),
			    "shared data area"));

  SHARED_DATA->Work_Queue.queue_content =
    ((long *)
     Allocate_Shared_Memory(Queue_Size * sizeof(Pointer),
			    "work queue"));

  if (!Debug_Flags[26])
    {
      if (!Use_Current_Cluster)
	{
	  code = cluster_create(Desired_Nints == 0 ?
				MAX_INTERPRETERS : Desired_Nints + Desired_Mems,
				(node_type_t) NULL, &SHARED_DATA->Cluster_Id,
				&nnodes);
	  if ((Desired_Nints == 0 &&
	       (code != KERN_RESOURCE_SHORTAGE &&
		code != KERN_SUCCESS)) ||
	      (Desired_Nints != 0 && (code != KERN_SUCCESS)))
	    {
	      fprintf(stderr, "    Unable to create cluster - %d\n", code);
	      exit(1);
	    }
	}
      else
	{
	  SHARED_DATA->Cluster_Id = my_cluster_id();
	  nnodes = n_nodes_in_cluster(SHARED_DATA->Cluster_Id);
	  if (nnodes == 1)
	    printf("Using current cluster %d of 1 node\n", SHARED_DATA->Cluster_Id);
	  else
	    printf("Using current cluster %d of %d nodes\n", SHARED_DATA->Cluster_Id, nnodes);
	}

      initialize_processor_maps(); /* in BF-PRIMS.C */
      initialize_section_fault_data(); /* in CODEMAN.C */

      if (Desired_Nints != 0 &&
	  Desired_Mems != 0 &&
	  Desired_Nints + Desired_Mems > nnodes)
	{
	  fprintf(stderr, "Insufficient nodes (%d) in cluster %d\n", nnodes, SHARED_DATA->Cluster_Id);
	  exit(1);
	}
    } /* if Debug_Flags */
  else
    {
      /* Just testing allocations -- fake out cluster and number of nodes */
      SHARED_DATA->Cluster_Id = my_cluster_id();
      nnodes = Desired_Nints + Desired_Mems;
    }

  N_Mems = nnodes;
  if (Desired_Nints == 0)
    N_Interps = nnodes - Desired_Mems;
  else
    N_Interps = Desired_Nints;
  SHARED_DATA->N_Interpreters = N_Interps;
  if (N_Interps == 1)
    printf("There is 1 processor running Lisp\n");
  else
    printf("There are %d processors running Lisp\n", N_Interps);

  if (N_Mems > N_Interps)
  {
    nmemonly = N_Mems - N_Interps;
    if (nmemonly == 1) 
      printf("There is 1 processor being used for memory only\n");
    else if (nmemonly > 1)
      printf("There are %d processors being used for memory only\n", nmemonly);
  }

  SHARED_DATA->Bfly_Undef_Prims = NIL;
  SHARED_DATA->Bfly_Undef_Arity = NIL;

  SHARED_DATA->Band_Load_Name[0] = '\0';

  N_Cee_Space = PAGES(((long) &etext));
  initialize_allocation_parameters();

  Stack_Size *= sizeof(Pointer);
  Stack_Size = PAGES(Stack_Size);
  real_stack_size = Stack_Size;

  Highest_Allocated_Address = (Pointer *)
    (0x01000000 - 4); /* stack space ends at 16mb limit. */

  Absolute_Stack_Base =
    ((Pointer *) (((long) Highest_Allocated_Address) - Stack_Size + 4));

  printf("%d (0x%x) bytes (%5.1fKP) of stack will run from 0x%x to 0x%x\n",
	 Stack_Size, Stack_Size,
	 Stack_Size / ((double) 1024 * sizeof(Pointer)),
	 Absolute_Stack_Base, Highest_Allocated_Address);

  /* Leave a page missing, and adjust to a page boundary. */

  Constant_Size = Normalize_Size((Constant_Size * sizeof(Pointer)), 1, true);

  SHARED_DATA->Bfly_Constant_Space =
    ((Pointer *)
     ((((long) Bfly_Stack_Base) - (Constant_Size + PAGE_SIZE)) &
      PAGE_MASK));
  SHARED_DATA->Bfly_Free_Constant = SHARED_DATA->Bfly_Constant_Space;

  SHARED_DATA->Bfly_Constant_Top =
    ((Pointer *)
     (((char *) SHARED_DATA->Bfly_Constant_Space) + Constant_Size));

  printf("%d (0x%x) bytes (%5.1fKP) of constant space will run from 0x%x to 0x%x\n",
	 Constant_Size, Constant_Size,
	 Constant_Size / ((double) 1024 * sizeof(Pointer)),
	 SHARED_DATA->Bfly_Constant_Space,
	 SHARED_DATA->Bfly_Constant_Top);

  /* Drop code top down to align on nearest section.
     This assures that Code_Base will be aligned, too. */

  SHARED_DATA->Code_Top = ((long *) (((long) SHARED_DATA->Bfly_Constant_Space) & SECTIONMASK));
  SHARED_DATA->Code_Base =
    ((long *)
     (((long) SHARED_DATA->Code_Top) - N_Code_Space));

  /* One word is for purify_code_space when booting. */
  SHARED_DATA->Code_Free = (SHARED_DATA->Code_Base + 1);

  /* One word is required by the GC. */
  SHARED_DATA->Code_Top -= 1; 

  printf("%d (0x%x) bytes (%5.1fKP) of code space will run from 0x%x to 0x%x\n",
	 N_Code_Space, N_Code_Space,
	 N_Code_Space / ((double) 1024 * sizeof(Pointer)),
	 SHARED_DATA->Code_Base, SHARED_DATA->Code_Top);

  /* One section past the end of memory. */
  Code_Copy_Area = ((long *) (0x1000000 + SECTIONSIZE));
  Code_Copy_Size = Normalize_Size(N_Code_Space, 1, true);


  if (Debug_Flags[8])
    printf ("First allocation is at 0x%x\n", heap_bottom);

  heap_bottom += (2 * PAGE_SIZE);
  heap_bottom = ((char *) (PAGES(((long) heap_bottom))));
  heap_top = ((char *) SHARED_DATA->Code_Base);

#ifdef ALLOCATE_DEBUG
  heap_top -= (2 * PAGE_SIZE);
#endif /* ALLOCATE_DEBUG */

  heap_size = (heap_top - heap_bottom);

  if ((Desired_Heap_Size > 0) && (heap_size > Desired_Heap_Size))
    heap_size = Desired_Heap_Size;
  heap_size = Normalize_Size(heap_size, HEAP_FACTOR, false);
  heap_top = (heap_bottom + heap_size);

  half_heap_size = (heap_size / HEAP_FACTOR);
  chunk_size = (half_heap_size / SHARED_DATA->N_Interpreters);

  Half_Heap_Size = (half_heap_size / sizeof(Pointer));
  Local_Chunk_Size = (chunk_size / sizeof(Pointer));

  /* This is done because chunk_size is not guaranteed
     to be a multiple of N_Interps unless N_Interps = N_Mems.
   */

  chunk_size = (Local_Chunk_Size * sizeof(Pointer));

  printf("%d (0x%x) bytes (%5.1fKP) of heap space will run from 0x%x to 0x%x\n",
	 heap_size, heap_size,
	 heap_size / ((double) 1024 * sizeof(Pointer)),
	 heap_bottom, heap_top);
  printf("%d pointers in each heap share\n",
	 Local_Chunk_Size);

  if (Memory_Quantum <= 0)
    Memory_Quantum = (MEMORY_QUANTUM_DEFAULT * 1024);

  SHARED_DATA->Heap_Chunk_Size = chunk_size;

  Absolute_Heap_Base = ((Pointer *) heap_bottom);
  SHARED_DATA->Heap_Base = Absolute_Heap_Base;

  SHARED_DATA->Other_Heap_Base =
    OTHER_HEAP_BASE(heap_bottom, half_heap_size);

#ifdef USE_SINGLE_HEAP
				/* For fasdump.c - scratch area. */
  Unused_Heap = SHARED_DATA->Other_Heap_Base;
  Unused_Heap_Top = Unused_Heap + Half_Heap_Size;
#endif

  heap1_now = ((char *) SHARED_DATA->Heap_Base);
  heap2_now = ((char *) SHARED_DATA->Other_Heap_Base);

  for (i = 0; i < SHARED_DATA->N_Interpreters; i++)
  {
    SHARED_DATA->InterpreterTable[i].My_Cons_Bottom = heap1_now;
    SHARED_DATA->InterpreterTable[i].My_Free = heap1_now;
    SHARED_DATA->InterpreterTable[i].My_Cons_Top = (heap1_now + chunk_size);
    SHARED_DATA->InterpreterTable[i].My_Cons2_Bottom = heap2_now;
    SHARED_DATA->InterpreterTable[i].My_Cons2_Top = (heap2_now + chunk_size);
    heap1_now += chunk_size;
    heap2_now += chunk_size;
  }

  Reset_Memory_Table();
  reset_shared_data();

  Who_Am_I = (-1);		/* -1 in servers and initializers */
				/* 0 ... n-1 for Lisp tasks */

  if (!Debug_Flags[26])
    {
      start_the_servers();

      code = fork_and_bind(0, SHARED_DATA->Cluster_Id, &allocator_id);
      if (code != KERN_SUCCESS)
	{
	  extern void kill_the_servers();

	  kill_the_servers();
	  fprintf(stderr,
		  "    Unable to create the allocator task - %d\n",
		  code);
	  exit(1);
	}

      if (allocator_id != 0)
	{
	  /* We are the parent i.e. not in the cluster. */

	  init_subprocess(false);
	  the_allocator_id = allocator_id;
	  signal(SIGTERM, kill_everything);
	  Wait_For_Shutdown();
	  kill_all_the_tasks();
	  _exit(0);
	}
      init_subprocess(true);
    } /* if Debug_Flags */

  /* We are running in the cluster so we can now allocate memory! */

  Allocate_Data_In(Constant_Size,
		   SHARED_DATA->Bfly_Constant_Space,
		   "constant space");
  Allocate_Data_In(half_heap_size,
		   ((char *) SHARED_DATA->Heap_Base),
		   "lower heap");
  Allocate_Data_In(half_heap_size,
		   ((char *) SHARED_DATA->Other_Heap_Base),
		   "upper heap");
  Allocate_Data_In(Code_Copy_Size,
		   Code_Copy_Area,
		   "code copy area");

  /* If just allocation test, get out */

  if (Debug_Flags[26])
    exit(0);

  compiler_initialize(true);	/* Set up constant space and Registers */

  /* Now start all the Lisp tasks */

  SHARED_DATA->Special_Synch = 99999;

  killer_file = fopen(".kill_lisp", "w");

  for (i = 0; i < N_Interps; i++)
  {
    fflush (stdout);	/* Must do this prior to each fork to avoid extra stuff in buffers of children */
    code = fork_and_bind(i, SHARED_DATA->Cluster_Id, &child);
    if (code != KERN_SUCCESS)
    {
      fprintf(stderr, "    Unable to create process on %d in %d - %d\n",
	      i, SHARED_DATA->Cluster_Id, code);
      kill_the_lisp_tasks(i);
      exit(1);
    }
    
    if (child == 0)
    {
      void Child_Lisp_Task();

      init_subprocess(true);
      Child_Lisp_Task(i); 
      _exit (0);
    }
    else
    {
      if (killer_file != (FILE *) NULL)
	fprintf(killer_file, "kill -9 %d\n", child);
      SHARED_DATA->Task_Id[i] = child;
      printf("Created task %d on processor %d\n", child, i);
    }
  }

  if (killer_file != ((FILE *) NULL))
    fclose(killer_file);
  SHARED_DATA->Special_Synch = 0;


  signal(SIGTERM, kill_all_lisp_tasks);
  Wait_For_Shutdown();
  kill_the_lisp_tasks(N_Interps);
  _exit(0);
}

/* We use sbrk here to insure we get the same memory space on
   both new and old vm systems. Use of vm_allocate/anywhere in the Lisp
   system should be avoided. */

char *
Allocate_Shared_Memory(size, error)
     long size;
     char *error;
{
  kern_return_t code;
  task_t myself, task_self();
  char *result;

  myself = task_self();

  /* Page-align to avoid attribute spillover */

  size = (size + PAGE_SIZE - 1) & PAGE_MASK;

  result = (char *) sbrk (size);
  if (!result)
    {
      fprintf(stderr, "    Unable to allocate 0x%x bytes of %s\n",
	      size, error);
      exit(1);
    }

  if (Debug_Flags[8])
  {
    printf("    Allocated %s running from 0x%x to 0x%x\n",
	   error, result, ((long) result) + size);
    fflush(stdout);
  }

  code = vm_inherit(myself, result, size, VM_INHERIT_SHARE);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr, "    Unable to share 0x%x bytes of %s - %d\n",
	    size, error, code);
    exit(1);
  }

  return (result);
}

void
allocate_bound_memory(myself, address, share, where, procno)
     task_t myself;
     char *address, *where;
     long share, procno;
{
  kern_return_t code;

  if (Debug_Flags[8]) 
    printf("    Allocating 0x%x bytes of %s on node %d from 0x%x to 0x%x\n",
	   share, where, procno, address, (address + share));

  /* If just testing allocations, don't do any real allocation */

  if (Debug_Flags[26])
    return;

  code = vm_allocate_and_bind(myself, &address, share, false, procno);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr,
	    "    Unable to allocate 0x%x bytes of %s at 0x%x on processor %d - %d (task=%d)\n",
	    share, where, address, procno, code, myself);
    exit(1);
  }

  code = vm_inherit(myself, address, share, VM_INHERIT_SHARE);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr,
	    "    Unable to share 0x%x bytes of %s at 0x%x on processor %d - %d (task=%d)\n",
	    share, where, address, procno, code, myself);
    exit(1);
  }
  return;
}

/* Approximately 3.25 Mbyte */

#define MAX_AVAIL_MEMORY	(3328 * 1024)

static long n_real, n_mem;
static double m_real, m_mem;
static double divisor;

void
initialize_allocation_parameters()
{
  n_real = N_Interps;
  n_mem = (N_Mems - n_real);

  m_mem = ((double) MAX_AVAIL_MEMORY);
  m_real = (m_mem - ((double) (N_Code_Space + N_Cee_Space)));

  if (m_real < 0.0)
    m_real = 0.0;

  if (n_mem == 0)
  {
    m_real = m_mem;
    m_mem = 0.0;
  }

  divisor = ((n_mem * m_mem) + (n_real * m_real));
  return;
}

/* Compute a size close to nbytes that can be split cleanly between
   the interpreters and the memory only processors.
 */

long
Normalize_Size(nbytes, wrt, round_up)
     long nbytes, wrt;
     Boolean round_up;
{
  double this_divisor, temp;
  long s_real, s_mem, result;

  this_divisor = divisor * wrt;
  temp = ((nbytes * m_real) / this_divisor);
  s_real = (PAGE_MASK & ((long) temp));
  temp = ((nbytes * m_mem) / this_divisor);
  s_mem = (PAGE_MASK & ((long) temp));
  do
  {
    result = (wrt * ((s_real * n_real) + (s_mem * n_mem)));
    if ((!round_up) || (result >= nbytes))
      break;
    if (n_mem != 0)
      s_mem += PAGE_SIZE;
    else
      s_real += PAGE_SIZE;
  } while (true);

  return (result);
}

void
Allocate_Data_In(nbytes, address, where)
     long nbytes;
     char *address, *where;
{
  double temp;
  long procno, share, share_real, share_mem, n2alloc;
  task_t myself, task_self();

  if (Debug_Flags[8])
  {
    printf("Preparing to allocate %d (0x%x) bytes of %s at 0x%x\n",
	   nbytes, nbytes, where, address);
    fflush(stdout);
  }

  myself = task_self();
  n2alloc = nbytes;

  temp = ((nbytes * m_mem) / divisor);
  share_mem = (((long) temp) & PAGE_MASK);
  temp = ((nbytes * m_real) / divisor);
  share_real = (((long) temp) & PAGE_MASK);
  
  if (share_real != 0)
  {
    for (procno = 0; procno < n_real; procno++)
    {
      if (Debug_Flags[8])
	printf("  Real ");
      allocate_bound_memory(myself, address, share_real, where,
			    (Magic_Flip ? (n_real - (procno + 1)) : procno));
      address += share_real;
      n2alloc -= share_real;
    }
  }
  if (share_mem != 0)
  {
    for (procno = n_real; procno < (n_real + n_mem); procno++)
    {
      if (Debug_Flags[8])
	printf("  Mem ");
      allocate_bound_memory(myself, address, share_mem, where, procno);
      address += share_mem;
      n2alloc -= share_mem;
    }
  }
  if (n2alloc > 0)
    {
      if (Debug_Flags[8])
	printf("  Leftover ");
      allocate_bound_memory(myself, address, n2alloc, where, n_real + n_mem - 1);
    }
  return;
}

task_t My_Task_Id;

#ifndef INITIALIZE_LOCAL_IO
#define INITIALIZE_LOCAL_IO()
#endif

extern void Initialize_Child_Task();

void
Initialize_Child_Task(first_time)
     Boolean first_time;
{
  extern Boolean Grab_Heap_Space();

  INITIALIZE_INTERRUPTS();
  INITIALIZE_LOCAL_IO();
  Initialize_Stack();

  Local_Heap_Base = SHARED_DATA->Heap_Base;
  Free = SHARED_DATA->Memory_Table[Who_Am_I].Free_Bottom;
  SET_MEMTOP(Free);
  Heap_Top = Free;

  GC_Space_Needed = 0;
  if (!Grab_Heap_Space(true)) 
  {
    if (first_time)
    {
      fprintf(stderr,
	      "(on %d/%d) Unable to allocate basic heap quantum (%d)\n",
	      Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], Memory_Quantum);
      exit(1);
    }
    else
    {
      fprintf(stderr,
	      "(on %d/%d) No space after disk restore!\n",
	      Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I]);
      Microcode_Termination(TERM_NO_SPACE);
    }
    /*NOTREACHED*/
  }

  Env = Make_Non_Pointer(GLOBAL_ENV, GO_TO_GLOBAL);
  Trapping = false;
  Return_Hook_Address = NULL;

  History = Make_Dummy_History(); /* To keep Stop_History from
				     putting junk in the heap */
  Prev_Restore_History_Stacklet = NIL;
  Prev_Restore_History_Offset = 0;

  Fluid_Bindings = NIL;
  Current_State_Point = NIL;
  return;
}

void
Child_Lisp_Task(Interp_No)
     long Interp_No;
{
  kern_return_t code;
  task_t task_self();

  if (Debug_Flags[8])
  {
    printf("Child %d waking up\n", Interp_No);
  }

  Who_Am_I = Interp_No;
  My_Task_Id = task_self();

/*
  CODE_COPY_IMAGE - Only make our code space if we are the only processor!
  (was Who_Am_I == 0)
*/
  if (N_Interps == 1)
    {
      if (Debug_Flags[8])
	{
	  printf("    %d allocating %d (0x%x) bytes of code space at 0x%x - 0x%x\n",
		 Interp_No, N_Code_Space, N_Code_Space,
		 SHARED_DATA->Code_Base,
		 ((long) SHARED_DATA->Code_Base) + N_Code_Space);
	  fflush(stdout);
	}
      code = vm_allocate_and_bind(My_Task_Id, &SHARED_DATA->Code_Base,
				  N_Code_Space, false, Who_Am_I);
      if (code != KERN_SUCCESS)
	{
	  fprintf(stderr, "    Unable to allocate 0d%d bytes of code space ",
		  N_Code_Space);
	  fprintf(stderr, "on processor %d - %d (task=%d)\n",
		  Who_Am_I, code, My_Task_Id);
	  {
	    long *page, npages, one_page;
	    
	    one_page = PAGE_SIZE / sizeof(Pointer);
	    npages = N_Code_Space / PAGE_SIZE;
	    page = SHARED_DATA->Code_Base;
	    while (npages > 0)
	      {
		code = vm_allocate_and_bind(My_Task_Id, &page, PAGE_SIZE, false, Who_Am_I);
		if (code != KERN_SUCCESS)
		  {
		    fprintf(stderr, "    Unable to allocate page at 0x%x on %d\n",
			    page, Who_Am_I);
		  }
		page += one_page;
		npages--;
	      }
	    exit(1);
	  }
	}
    }

  if (Debug_Flags[8])
  {
    printf("    %d allocating %d (0x%x) bytes of stack at 0x%x\n",
	   Interp_No, real_stack_size, real_stack_size,
	   Absolute_Stack_Base,
	   (((long) Absolute_Stack_Base) + real_stack_size));
    fflush(stdout);
  }
  code = vm_allocate_and_bind(My_Task_Id, &Absolute_Stack_Base,
			      real_stack_size, false, Who_Am_I);
  if (code != KERN_SUCCESS)
  {
    fprintf(stderr, "    Unable to allocate %d bytes of stack space ",
	    real_stack_size);
    fprintf(stderr, "on processor %d - %d (task=%d)\n",
	    Who_Am_I, code, My_Task_Id);
    fflush(stderr);
    exit(1);
  }

  Initialize_Child_Task(true);

  if (Debug_Flags[8])
  {
    printf("    %d starting scheme\n", Interp_No);
    printf("Free = 0x%x, MemTop = 0x%x, Stack_Pointer = 0x%x, Stack_Guard = 0x%x\n",
	   Free, MemTop, Stack_Pointer, Stack_Guard);
  }

  if (Interp_No == 0)
    Start_Scheme(boot_band_p ? BOOT_LOAD_BAND : BOOT_FASLOAD, boot_name);
  else
    Start_Scheme(BOOT_GET_WORK, "");
  /*NOTREACHED*/
}

/*
	Memory Management Routines
*/

void
Reset_Memory()
{
  /* Do nothing for now. */

  return;
}

void
Clear_Memory()
{
  /* Do nothing for now. */

  return;
}

/* Memory management and restarting accross bands. */

extern void
  start_band_load(),
  end_band_load(),
  slave_after_band_load();

extern Pointer compiler_utilities;

static struct disk_restore_interrupt_data droid;

void
start_band_load()
{
  if (N_Interps != 1)
  {
    droid.node = Who_Am_I;
    droid.state = true;
    /* Should be innocuous. */
    droid.succeeded = true;
    droid.utils = compiler_utilities;

    /* Guarantee that everyone is ready at boot time. */
    while (SHARED_DATA->Slave_Done_Count != 0)
    {
      Standard_Delay();
    }

    while (atomior(&SHARED_DATA->GC_Propagate_Lock, 0x8000) != 0)
    {
      Standard_Delay();
    }

    /* This stops the other processors. */

    Send_Signal_Info(-1, SIG_DISK_RESTORE, &droid, sizeof(droid));
    SHARED_DATA->GC_Propagate_Lock = 0;
  }

  /* Maybe this should try to reserve N_Interps * Memory_Quantum,
     by setting MemTop lower?
   */

  Free = SHARED_DATA->Heap_Base;
  SET_MEMTOP(Free + Half_Heap_Size);

  Start_Direct_IO();
  return;
}

void
slave_after_band_load(new_utils)
     Pointer new_utils;
{
  extern void
    compiler_reset(),
    Initialize_Child_Task();
  extern Pointer
    make_primitive();
  Pointer prim;

  /* The master has already flipped my heap if necessary.
     I just need to update local variables that the master
     cannot access.
   */

  compiler_reset(new_utils);
  Initialize_Child_Task(false);

  /* I'm ready for further damage. */

  atomadd(&SHARED_DATA->Slave_Done_Count, -1);

  prim = make_primitive("GET-WORK");

 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 2));
  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();
  Push(NIL);
  Push(prim);
  Push(STACK_FRAME_HEADER + 1);
 Pushed();

  longjmp(*Back_To_Eval, PRIM_APPLY);
  /*NOTREACHED*/
}

#ifndef INITIALIZE_GLOBAL_IO
#define INITIALIZE_GLOBAL_IO()
#endif

void
end_band_load(success)
     Boolean success;
{
  Finish_Direct_IO();
  if (success)
  {
    extern void
      reset_shared_data(),
      Initialize_Child_Task(),
      Re_Initialize_Memory_Table();

    Re_Initialize_Memory_Table();
    reset_shared_data();
    INITIALIZE_GLOBAL_IO();
    Initialize_Child_Task(false);

    /* I'm ready for further damage. */

    atomadd(&SHARED_DATA->Slave_Done_Count, -1);

  }

  if (N_Interps != 1)
  {
    droid.node = Who_Am_I;
    droid.state = false;
    droid.succeeded = success;
    droid.utils = compiler_utilities;

    /* This restarts the other processors. */

    Send_Signal_Info(-1, SIG_DISK_RESTORE, &droid, sizeof(droid));
  }
  return;
}
