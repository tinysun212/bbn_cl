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

#define true 1
#define false 0

#include <mach.h>

#undef task_self

#define empty_element (-1)

struct work_queue_t {
  int get_index;
  int put_index;
  int n_procs;
  int n_elements; } *work_queue;

long *work_queue_vectors[128];

/*
  Set up a work queue.  This must be done BEFORE spinning off any subtasks!

  It takes a total queue size, the number of processors which will be using
  the queue, and a cluster id for memory allocation.
*/

initialize_work_queue(nelemts, nprocs, cluster)
int nelemts, nprocs;
cluster_t cluster;
{ int i, j, share;
  kern_return_t code;
  task_t self, task_self();

  self = task_self();

  code = vm_allocate(self, &work_queue, sizeof(struct work_queue_t),
		     true);
  if (code != KERN_SUCCESS) return false;
  code = vm_inherit(self, work_queue, sizeof(struct work_queue_t),
		    VM_INHERIT_SHARE);
  if (code != KERN_SUCCESS) return false;

  work_queue->get_index = 0;
  work_queue->put_index = 0;
  work_queue->n_procs = nprocs;
  share = (nelemts + nprocs - 1) / nprocs;
  work_queue->n_elements = nprocs * share;
  
  for (i = 0; i < nprocs; i++) {

    code = vm_allocate_and_bind(self, &work_queue_vectors[i],
				share * sizeof(long), true, i);
    if (code != KERN_SUCCESS) return false;
    code = vm_inherit(self, work_queue_vectors[i], 
		      share * sizeof(long), VM_INHERIT_SHARE);

    for (j = 0; j < share; j++)
      work_queue_vectors[i][j] = empty_element; }

  return true; }

/*
  Put something on the queue.
*/

put_on_queue(value)
long value;
{ int put_loc, vn, vo, nelemts, nprocs;

  nelemts = work_queue->n_elements;
  nprocs = work_queue->n_procs;

  put_loc = atomadd32(&work_queue->put_index, 1);
  if (put_loc >= nelemts) {
    put_loc -= nelemts;
    if (put_loc == 0)
      atomadd32(&work_queue->put_index, -nelemts); }

  vn = put_loc / nprocs;
  vo = put_loc % nprocs;

  if (work_queue_vectors[vn][vo] != empty_element)
    return false;

  work_queue_vectors[vn][vo] = value;
  return true; }

/*
  Get something from a queue.  Return T/F depending on whether value is set.
*/

get_from_queue(value)
int *value;
{ long val, *ve;
  int get_loc, vn, vo, nelemts, nprocs;

  nelemts = work_queue->n_elements;
  nprocs = work_queue->n_procs;

  get_loc = atomadd32(&work_queue->get_index, 1);
  if (get_loc >= nelemts) {
    get_loc -= nelemts;
    if (get_loc == 0)
      atomadd32(&work_queue->get_index, -nelemts); }
  
  vn = get_loc / nprocs;
  vo = get_loc % nprocs;
  ve = &work_queue_vectors[vn][vo];

  val = *ve;
  if (val == empty_element)
    return false;

  *value = val;
  *ve = empty_element;
  return true; }
