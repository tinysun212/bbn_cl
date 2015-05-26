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


/* Stuff for serializing write operations on the Butterfly */

typedef short int *Lock_Handle;

#ifdef butterfly

extern void
  Unlock_Cell(),
  Initialize_Heap_Locks();

extern Lock_Handle
  Lock_Cell();

#define Do_Store_No_Lock(Where, What) *((Pointer *) Where) = ((Pointer) What)

#else

#define Lock_Cell(Cell)		NULL	/* Start lock */
#define Unlock_Cell(Cell)		/* End lock */
#define Initialize_Heap_Locks()		/* Clear at start up */
#define Do_Store_No_Lock(To, F)	*(To) = F
#define Sleep(How_Long)		{ }	/* Delay for locks, etc. */

#endif
