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


/* *** We should be able to make these all obsolete */

/* Transaction type codes */

/* From LM to Scheme: */

#define	CHAR_AVAIL	0101
#define BUFFER_AVAIL	0102
#define	PROC_INTRUPT	0103


/* Definitions for the communication based on signals and pipes. */

#define READ_FROM_PIPE 0
#define WRITE_TO_PIPE 1

/* The size below depends on the size of
   SHARED_DATA->Signal_Data.  See butterfly.h
 */

struct unknown_interrupt_data
{
  char data[128 * sizeof(long)];
};

#define SIG_GLOBAL_INT 1
struct global_interrupt_data
{
  int code;
};

#define SIG_CHAR_INT 2
struct character_interrupt_data
{
  int kind;
};

#define SIG_INPUT_INT 3
struct input_interrupt_data
{
  int length;
  char data[(sizeof(struct unknown_interrupt_data) -
	     (1 * sizeof(int)))];
};

#define SIG_NEW_CODE 4
struct new_code_interrupt_data
{
  int length;
  long *target;
  long *source;
};

#define SIG_DISK_RESTORE 5
struct disk_restore_interrupt_data
{
  int node;
  int state;
  int succeeded;
  long utils;
};

#define SIG_RELOCATE 6
struct relocate_interrupt_data
{
  long *from;
  long *to;
  long heap_relocation;
  long const_relocation;
  long stack_relocation;
  long code_relocation;
  long *load_renumber_table;
  long *code_base;
  long *code_top;
  long *const_top;
};

#define SIG_FILE_CODE 7
struct file_code_data
{
  long *to;
  long length;
  char fname[(sizeof(struct unknown_interrupt_data) -
	     (2 * sizeof(long)))];
};

/* Definitions for the shared character interrupt/input interrupt
   for BF-IO.
 */

extern char Int_Char;
extern char Int_String[];
extern int Int_Stream;
extern int Int_State;

#define INT_STATE_CHAR		0x1
#define INT_STATE_INPUT		0x2
