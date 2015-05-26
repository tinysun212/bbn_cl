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
  MACH/UNIX stack trace for the 68020
*/

struct frame {
  struct frame *old_fp;
  short int *old_pc; };

#define NULL 0

stack_trace(ignored_arg)
long ignored_arg;
{ struct frame *fp;
  long *argptr;
  int nargs, argno, segno;

  for (fp = (struct frame *) ((&ignored_arg) - 2); /* hmm */
       fp != (struct frame *) NULL;
       fp = fp->old_fp) {
    segno = (((long) fp) >> 16) & 0x0000ffff;
    if (segno == 0xff || segno == 0xbfff) {
      printf("%x -> %x %x\n", fp, fp->old_fp, fp->old_pc);
      argptr = ((long *) fp) + 2;
      if (fp->old_pc < ((long *) 32)) {
	printf("HARDWARE TRAP %d\n", fp->old_pc); }
      else {
	nargs = get_nargs(fp->old_pc);
	printf("        (");
	for (argno = 0; argno < nargs; argno++) {
	  printf("%x", *argptr++);
	  if (argno + 1 != nargs) printf(", "); }
	printf(")\n"); } }
    else {
      printf("%x -> INVALID STACK FRAME\n", fp);
      return; } } }

get_nargs(pc)
short int *pc;
{ int instr, masked, size, segno;

  segno = ((long) pc) >> 16;
  if (segno < 0 || segno >= 0xc) return 0;

  instr = *pc;
  masked = instr & 0xf1ff;

  if (masked == 0x508f || masked == 0x504f) { /* addql #n,sp */
    size = (instr >> 9) & 7;
    if (size == 0) size = 8; }
  else if (instr == 0xdefc) {	/* addaw #n,sp */
    size = pc[1]; }
  else if (instr == 0xdffc) {	/* addal #nn,sp */
    size = (pc[1] << 16) | pc[2]; }
  else if (instr == 0x4fef) {	/* lea sp@(nn),sp */
    size = pc[1]; }
  else {
    size = 0; }

  size = (size + 2) >> 2;
  return size; }

/*
  Starting at the fixed objects vector - recursively find a specified
  pointer and say what it was.
*/

#include "gc.h"

debug_search(here, target)
     long here, target;
{
  search_for(here, 0, target);
  return;
}

#define search_max 512

struct {
  int d_what;
  int d_slot; } search_state[search_max];

extern int GC_Type_Map[];

search_for(here, depth, target)
     long here, target;
     int depth;
{
  int i, gct, length;
  long *object;

  if ((here & 0x00ffffff) == target)
  {
    printf("%x found at:\n", here);
    for (i = depth; i >= 0; i--)
      printf("        %x (%x)\n",
	     search_state[i].d_what,
	     search_state[i].d_slot);
    return;
  }

  search_state[depth].d_what = here;
  search_state[depth].d_slot = 0;
  object = ((long *) (here & 0x00ffffff));
  gct = GC_Type_Map[((here >> 24) & 0x7f)];
  switch (gct)
  {
  case GC_Cell:
    search_for(object[0], depth + 1, target);
    break;

  case GC_Pair:
    search_for(object[0], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[1], depth + 1, target);
    break;

  case GC_Triple:
    search_for(object[0], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[1], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[2], depth + 1, target);
    break;

  case GC_Quadruple:
    search_for(object[0], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[1], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[2], depth + 1, target);
    search_state[depth].d_slot++;
    search_for(object[3], depth + 1, target);
    break;

  case GC_Vector:
    length = object[0] & 0x0000ffff;
    for (i = 0; i < length; i++)
    {
      search_for(object[i+1], depth + 1, target);
      search_state[depth].d_slot++;
    }
    break;
  }
  return;
}
