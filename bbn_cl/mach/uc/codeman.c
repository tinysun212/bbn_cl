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
	This module does most of the Code Space manipulations to
	keep the Butterfly Code Space up to date.
*/

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "zones.h"
#include "transact.h"

extern int Debug_Flags[];

/*
	Scan through the fasloaded code and move all chunks of code
	into code space and then propagate them.
	It only moves code in the heap.
	It leaves code in constant/pure space in place, otherwise it would
	be leaving broken hearts behind.
*/

#ifdef butterfly

void
code_copy(from, to, bytes)
     fast long *from, *to;
     long bytes;
{
  fast long *end;

  end = ((long *) (((char *) from) + bytes));
  while (from < end)
  {
    *to++ = *from++;
  }
  return;
}

Boolean 
Move_Code_To_Codespace(Start, Finish)
     fast Pointer *Start, *Finish;
{
  long *Allocate_Code();
  fast char *heap_bound, *code_base, *code_locn;
  Pointer *code_object;
  fast int typecode;
  long *target, code_length;

  if (Debug_Flags[14])
  {
    printf("move code to codespace(0x%x, 0x%x) CS=%0xx\n",
	   Start, Finish, SHARED_DATA->Code_Free);
  }

  heap_bound = ((char *) SHARED_DATA->Code_Base);
  while (Start < Finish)
  {

    Switch_by_GC_Type(*Start)
    {
      case TC_COMPILED_CODE_BLOCK:
     	code_locn = ((char *) Get_Pointer(*Start));
	if (code_locn < heap_bound)
	{
	  code_object = ((Pointer *) code_locn);
	  goto move_code_object;
	}
	break;

      case_compiled_entry_point:
     	code_locn = ((char *) Get_Pointer(*Start));
	if (code_locn < heap_bound)
	{
	  Get_Compiled_Block(code_object, code_locn);

move_code_object:
	
	  typecode = OBJECT_TYPE(*Start);
	  code_base = ((char *) code_object);
	  if (OBJECT_TYPE(*code_object) != TC_BROKEN_HEART)
	  {
	    code_length = (Get_Integer(*code_object) + 1);
	    target = Allocate_Code(code_length);
	    if (target == ((long *) NULL))
	    {
	      printf("Unable to allocate %x words of code space for %x\n",
		     code_length, code_object);
	      signal_error_from_primitive(ERR_FASL_FILE_TOO_BIG);
	    }
	    code_copy(code_object, target, (code_length * sizeof(Pointer)));
	    *code_object = Make_New_Pointer(TC_BROKEN_HEART, target);
	  }
	  *Start = Make_New_Pointer(typecode,
				    ((char *) Get_Pointer(*code_object)) +
				    (code_locn - code_base));
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
        Start += Get_Integer(*Start);
	break;

      case TC_LINKAGE_SECTION:
      {
	fast long count;

	if (READ_LINKAGE_KIND(*Start) != OPERATOR_LINKAGE_KIND)
	{
	  count = READ_CACHE_LINKAGE_COUNT(*Start);
	  Start += count;
	}
	else
	{
	  printf("\nMove_Code_To_Codespace: Ignoring operator linkage ");
	  printf("section at 0x%lx\n", Start);
	  count = READ_OPERATOR_LINKAGE_COUNT(*Start);
	  Start = END_OPERATOR_LINKAGE_AREA(Start, count);
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast machine_word *start_ptr, *word_ptr;

	printf("\nMove_Code_To_Codespace: Ignoring manifest closure ");
	printf("section at 0x%lx\n", Start);
	
	Start += 1;
	word_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Start);
	start_ptr = word_ptr;

	while (VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  word_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	}
	Start = MANIFEST_CLOSURE_END(word_ptr, start_ptr);
	break;
      }

      default:
	break;
    }
    Start += 1;
  }
  return (true);
}

#endif /* butterfly */

/*
	Used by lookup to allocate room for cache in constant space.
 */

extern Pointer
  *Atomic_Allocate();

/*
	Used by cmp68020 to propagate changed values.
 */

extern void
  propagate_variable_cache(),
  propagate_uuo_link();

#ifndef butterfly

Pointer *
Atomic_Allocate(nwords)
     int nwords;
{
  Pointer *result;

  result = Free;
  Free += nwords;
  return (result);
}

void
propagate_variable_cache(block, offset)
     long *block, offset;
{
  return;
}

void
propagate_uuo_link(block, offset)
     long *block, offset;
{
  return;
}

/* This is a hack for this file itself, to avoid all the ifdefs. */

#define propagate_variable_cache(b, o)

#else /* butterfly */

void
propagate_variable_cache(block, offset)
     long *block, offset;
{
  void Propagate_Code();

  Propagate_Code(block + offset, block + offset + 1);
  return;
}

void
propagate_uuo_link(block, offset)
     long *block, offset;
{
  void Propagate_Code();

  /* UUO links are 2 words long:
	jmp/jsr	#address.l	; 6 bytes
	dc.w	nargs		; 2 bytes
   */

  Propagate_Code(block + offset, block + (offset + 2));
  return;
}


Pointer *
Atomic_Allocate(nwords)
     int nwords;
{
  Pointer *result, *footer, *header;

  if (Debug_Flags[12])
  {
    printf("Atomic_Allocate(%d) FC = %x ... ",
	   nwords, SHARED_DATA->Bfly_Free_Constant);
    fflush(stdout);
  }

  while (atomior(&SHARED_DATA->Constant_Space_Lock, 0xffff) != 0x0000)
    Standard_Delay();

  if (SHARED_DATA->Bfly_Constant_Space == SHARED_DATA->Bfly_Free_Constant)
    copy_to_constant_space(NULL, 0);

  footer = SHARED_DATA->Bfly_Free_Constant - 1;
  result = footer - 1;		/* Write over the old footer. */
  header = footer - Get_Integer(*footer) + 1;

  SHARED_DATA->Bfly_Free_Constant +=  nwords;

  *(SHARED_DATA->Bfly_Free_Constant - 2) = *result;
  *(SHARED_DATA->Bfly_Free_Constant - 1) = *footer + nwords;
  *header += nwords;

  SHARED_DATA->Constant_Space_Lock = 0x0000;

  if (Debug_Flags[12])
  {
    printf(" -> %x\n", result);
    fflush(stdout);
  }

  return (result);
}

/*
	Used by Move_Code_To_Codespace to allocate space for compiled code.
*/

long *
Allocate_Code(nwords)
     long nwords;
{
  long *result;

  if (SHARED_DATA->Code_Free == (long *) NULL)
    return ((long *) NULL);

  Code_Space_Lock();
  result = SHARED_DATA->Code_Free;
  if (result + nwords > SHARED_DATA->Code_Top) 
    result = ((long *) NULL);
  else
    SHARED_DATA->Code_Free += nwords; /* Code_Free is (long *) */
  Code_Space_Unlock();
  return (result);
}

extern long
  *Code_Copy_Area,
  Code_Copy_Size;

/* Only one of the CODE_COPY_... must be defined. */

#define CODE_COPY_IMAGE

/*
  CODE_COPY_BRUTE_FORCE
  CODE_COPY_SPREAD
  CODE_COPY_USE_VMCOPY
  CODE_COPY_USE_FILE
  CODE_COPY_IMAGE
 */

#ifdef CODE_COPY_USE_VMCOPY
#define PAGE_SIZE			0x2000
#define PAGE_MASK			(~(PAGE_SIZE-1))
#include <mach.h>
#endif /* CODE_COPY_USE_VMCOPY */

#ifdef CODE_COPY_USE_FILE
#define MAXIMUM_PARALLEL_LENGTH		0x2000
#ifndef CODE_COPY_FNAME_DEFAULT
#define CODE_COPY_FNAME_DEFAULT		"/usr2/tmp/XXXXXX"
#endif
#include <fcntl.h>
#endif /* CODE_COPY_USE_FILE */

void
Propagate_Code(xstart, xend)
     long xstart, xend;
{
  extern long OS_real_time_clock();

  long length;
  struct new_code_interrupt_data ncid;
  char *start, *end, *target;

  if ((N_Interps == 1) || (xstart == xend))
    return;

  start = ((char *) (xstart & 0x00ffffff));
  end = ((char *) (xend & 0x00ffffff));
  length = (end - start);

  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Propagating 0x%x bytes from 0x%x to 0x%x\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], length, start, end);
    printf("(on %d/%d) Starting propagation at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  while(atomior(&SHARED_DATA->GC_Propagate_Lock, 0x8000) != 0)
    Standard_Delay();

#ifdef CODE_COPY_IMAGE
  {
    long *image_area;

    SHARED_DATA->GC_Propagate_Lock = (0x8000 + N_Interps);

    image_area = (long *) (((char *) Code_Copy_Area) +
			   (start - ((char *) SHARED_DATA->Code_Base)));

    code_copy(start, image_area, length);

    if (Debug_Flags[23])
      printf("(on %d) Copying %x bytes at %x into image at %x\n",
	     Who_Am_I, length, start, image_area);

    ncid.target = start;
    ncid.length = length;
    ncid.source = image_area;
    Send_Signal_Info(-1, SIG_NEW_CODE, &ncid, sizeof(ncid));
    while((SHARED_DATA->GC_Propagate_Lock & 0x7fff) != 1)
      Standard_Delay();

    if (Debug_Flags[23])
      printf("... finished propagating\n");
  }
#endif

#ifdef CODE_COPY_SPREAD

  {
    long share, n2move, division, pn;
    long *part;

    SHARED_DATA->GC_Propagate_Lock = (0x8000 + N_Interps);

    length = (length / sizeof(Pointer));
    share = (length / N_Interps);
    part = Code_Copy_Area;
    division = (Code_Copy_Size / (N_Interps * sizeof(Pointer)));

    for (pn = 0; pn < N_Interps; pn++)
    {
      if (pn + 1 < N_Interps)
	n2move = share;
      else
	n2move = (length - ((N_Interps - 1) * share));
      n2move *= sizeof(Pointer);
      part[0] = n2move;
      part[1] = ((long) start);
      if (Debug_Flags[14])
      {
	printf("-> %x (%x) -> %x\n", part + 2, part[0], part[1]);
	fflush(stdout);
      }
      code_copy(start, part + 2, n2move);
      start += n2move;
      part += division;
    }

    if (Debug_Flags[14])
    {
      printf("(on %d/%d) Starting distribution at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }
    ncid.target = start;		/* IGNORED */
    ncid.length = length;		/* IGNORED */
    ncid.source = Code_Copy_Area;	/* IGNORED */
    Send_Signal_Info(-1, SIG_NEW_CODE, &ncid, sizeof(ncid));
    while((SHARED_DATA->GC_Propagate_Lock & 0x7fff) != 1)
      Standard_Delay();
  }

#endif /* CODE_COPY_SPREAD */

#ifdef CODE_COPY_USE_VMCOPY
  {
    extern kern_return_t vm_copy();
    extern task_t task_self();

    kern_return_t result;

    start = ((char *) (((unsigned long) start) & PAGE_MASK));
    end = ((char *) ((((unsigned long) end) + (PAGE_SIZE - 1)) & PAGE_MASK));
    length = (end - start);
    target = (((char *) Code_Copy_Area) +
	      (start - ((char *) SHARED_DATA->Code_Base)));

    if (Debug_Flags[14])
    {
      printf("(on %d/%d) In pages: length = 0x%x; source = 0x%x; target = 0x%x\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], length, start, target);
      fflush(stdout);
    }

    result = vm_copy(task_self(),
		     ((vm_address_t) start), ((vm_size_t) length),
		     ((vm_address_t) target));
    if (result != KERN_SUCCESS)
    {
      fprintf(stderr, "vm_copy failed. result = 0d%d\n", result);
      Microcode_Termination(TERM_EXIT);
    }

    if (Debug_Flags[14])
    {
      printf("(on %d/%d) Starting distribution at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }
    ncid.target = start;
    ncid.length = length;
    ncid.source = target;

    SHARED_DATA->GC_Propagate_Lock = (0x8000 + (N_Interps - 1));

    Send_Signal_Info(-1, SIG_NEW_CODE, &ncid, sizeof(ncid));

    while((SHARED_DATA->GC_Propagate_Lock & 0x7fff) != 0)
      Standard_Delay();
  }
#endif /* CODE_COPY_USE_VMCOPY */

#ifdef CODE_COPY_USE_FILE

  if (length > MAXIMUM_PARALLEL_LENGTH)
  {
    extern int mkstemp();
    struct file_code_data fcd;
    int fd, size;

    strcpy(fcd.fname, CODE_COPY_FNAME_DEFAULT);
    fcd.to = ((long *) start);
    fcd.length = length;

    fd = mkstemp(fcd.fname);
    if (fd < 0)
    {
      fprintf(stderr, "Failed to open %s\n", fcd.fname);
      Microcode_Termination(TERM_EXIT);
    }

    if (Debug_Flags[14])
    {
      printf("(on %d/%d) Writing 0x%x bytes to file %s at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     fcd.length, fcd.fname, System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }

    size = write(fd, ((char *) fcd.to), fcd.length);
    if (size != fcd.length)
    {
      close(fd);
      unlink(fcd.fname);
      fprintf(stderr, "Wrote only 0x%x bytes to file %s instead of 0d%d\n",
	      size, fcd.fname, fcd.length);
      Microcode_Termination(TERM_EXIT);
    }
    close(fd);
    
    if (Debug_Flags[14])
    {
      printf("(on %d/%d) Starting distribution at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }

    SHARED_DATA->GC_Propagate_Lock = (0x8000 + (N_Interps - 1));

    Send_Signal_Info(-1, SIG_FILE_CODE, &fcd, sizeof(fcd));

    while((SHARED_DATA->GC_Propagate_Lock & 0x7fff) != 0)
      Standard_Delay();

    unlink(fcd.fname);
  }
  else

#endif /* CODE_COPY_USE_FILE */

#if defined(CODE_COPY_USE_FILE) || defined(CODE_COPY_BRUTE_FORCE)

  {
    code_copy(start, Code_Copy_Area, length);

    if (Debug_Flags[14])
    {
      printf("(on %d/%d) Starting distribution at %d (%d real)\n",
	     Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	     System_Clock(), OS_real_time_clock());
      fflush(stdout);
    }

    ncid.target = start;	/* First send the code */
    ncid.length = length;
    ncid.source = Code_Copy_Area;

    SHARED_DATA->GC_Propagate_Lock = (0x8000 + (N_Interps - 1));

    Send_Signal_Info(-1, SIG_NEW_CODE, &ncid, sizeof(ncid));

    while((SHARED_DATA->GC_Propagate_Lock & 0x7fff) != 0)
      Standard_Delay();
  }

#endif /* not CODE_COPY_USE_VMCOPY */

  SHARED_DATA->GC_Propagate_Lock = 0;

  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Finished propagation at %d (%d real)\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   System_Clock(), OS_real_time_clock());
    fflush(stdout);
  }

  return;
}

extern void
  get_file_code(),
  get_new_code();

#ifdef CODE_COPY_IMAGE
#define MAX_CODE_SECTIONS 128
Boolean code_space_section_exists[MAX_CODE_SECTIONS];
#endif

void
get_new_code(ncid)
     struct new_code_interrupt_data *ncid;
{
#ifdef CODE_COPY_IMAGE
  /* We only get code for sections that actually exist since the image
     is guaranteed to be kept up to date, we'll grab it when we touch it */

  int section_no, n2copy;
  long *image_address, *section_address, length;

  image_address = ncid->source;
  section_address = ncid->target;
  length = ncid->length;

  /* Note the C hackery below -- multiply by sizeof(long) to get byte address,
     since the pointer subtraction will be in pointed-to units, i.e., long */

  section_no = (((section_address - SHARED_DATA->Code_Base) * sizeof(long)) /
		SECTIONSIZE);

  if (Debug_Flags[23])
    printf("(on %d) Getting %x bytes from %x in image to %x, section_no = %d\n",
	   Who_Am_I, length, image_address, section_address, section_no);
  
  n2copy = SECTIONSIZE - (((long) section_address) & (SECTIONSIZE - 1));
  if (n2copy > length) n2copy = length;
  while (length > 0)
    {
      if (code_space_section_exists[section_no])
	{
	  if (Debug_Flags[23])
	    printf("        moving section %d from %x to %x for %x\n",
		   section_no, image_address, section_address, n2copy);
	  code_copy(image_address, section_address, n2copy);
	}

      length -= n2copy;
      image_address += (n2copy / sizeof(long));
      section_address += (n2copy / sizeof(long));
      section_no++;

      if (length > SECTIONSIZE)
	n2copy = SECTIONSIZE;
      else
	n2copy = length;
    }

  if (Debug_Flags[23])
    printf("... finished receiving on %d\n", Who_Am_I);
#endif

#ifdef CODE_COPY_SPREAD

  long division, *part;
  int pn;

  division = (Code_Copy_Size / (N_Interps * sizeof(Pointer)));
  part = Code_Copy_Area + (Who_Am_I * division);

  for (pn = Who_Am_I; pn < N_Interps; pn++)
  {
    code_copy(part + 2, part[1], part[0]);
    part += division;
  }

  part = Code_Copy_Area;
  for (pn = 0; pn < Who_Am_I; pn++)
  {
    code_copy(part + 2, part[1], part[0]);
    part += division;
  }

#endif /* CODE_COPY_SPREAD */

#ifdef CODE_COPY_USE_VMCOPY

  extern kern_return_t vm_copy();
  extern task_t task_self();

  kern_return_t result;

  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Getting code section at 0x%x 0x%x long\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   ncid->target, ncid->length);
    fflush(stdout);
  }
  result = vm_copy(task_self(),
		   ((vm_address_t) ncid->source), ((vm_size_t) ncid->length),
		   ((vm_address_t) ncid->target));
  if (result != KERN_SUCCESS)
  {
    fprintf(stderr, "(on %d/%d) vm_copy failed; result = %d\n",
	    Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I], result);
    Microcode_Termination(TERM_EXIT);
  }
#endif /* CODE_COPY_USE_VMCOPY */

#if defined(CODE_COPY_USE_FILE) || defined(CODE_COPY_BRUTE_FORCE)

  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Getting code section at 0x%x 0d%d long\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   ncid->target, ncid->length);
    fflush(stdout);
  }
  code_copy(ncid->source, ncid->target, ncid->length);

#endif /* CODE_COPY_USE_FILE || CODE_COPY_BRUTE_FORCE */

  return;
}

void
get_file_code(fcd)
     struct file_code_data *fcd;
{
#ifdef CODE_COPY_USE_FILE

  long fd, size;

  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Getting code section at 0x%x 0d%d long\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   fcd->to, fcd->length);
    fflush(stdout);
  }
  fd = open(fcd->fname, O_RDONLY, 0);
  if (fd < 0)
  {
    fprintf(stderr, "Failed to open code file %s\n",
	    fcd->fname);
    Microcode_Termination(TERM_EXIT);
  }
  size = read(fd, ((char *) fcd->to), fcd->length);
  if (size != fcd->length)
  {
    close(fd);
    fprintf(stderr, "Read 0d%d bytes from file %s rather than 0d%d.\n",
	    size, fcd->fname, fcd->length);
    Microcode_Termination(TERM_EXIT);
  }
  close(fd);
  if (Debug_Flags[14])
  {
    printf("(on %d/%d) Done getting code section at 0x%x 0d%d long\n",
	   Who_Am_I, SHARED_DATA->Task_Id[Who_Am_I],
	   fcd->to, fcd->length);
    fflush(stdout);
  }
  return;

#else /* not CODE_COPY_USE_FILE */

  fprintf(stderr, "get_file_code: invoked without CODE_COPY_USE_FILE\n");
  Microcode_Termination(TERM_EXIT);
  /*NOTREACHED*/

#endif /* not CODE_COPY_USE_FILE */
}

/*
  Scan all of code space.  Move all constants into pure space and all
  cells, quads, variables, environments, and whatnot into constant
  space.  We then have to scan and grow the pure and constant spaces
  accordingly and the mark all of code space as unscannable

  It uses purify_area, which is the core of Purify.
*/

Pointer
purify_code_space()
{
  extern void purify_area();

  if (SHARED_DATA->Code_Impure < SHARED_DATA->Code_Base)
  {
    /* Paranoia. */

    SHARED_DATA->Code_Impure = SHARED_DATA->Code_Base;
  }

  if (SHARED_DATA->Code_Impure >= SHARED_DATA->Code_Free)
  {
    extern void GC_Do_Work();

    /* Paranoia and compatibility with bands with
       off-by-one error.
     */

    SHARED_DATA->Code_Impure = SHARED_DATA->Code_Free;

    /* Avoid the purification which allocates useless header
       space.
     */

    GC_Do_Work(0);
  }
  else
  {
    purify_area("purify_code_space",
		((Pointer *) SHARED_DATA->Code_Impure),
		((Pointer *) SHARED_DATA->Code_Free),
		0, true, true);
  }
  /*NOTREACHED*/
}

extern void end_code_space_purification();

void
end_code_space_purification(start, end)
     Pointer *start, *end;
{
  Pointer *base;

  base = ((Pointer *) SHARED_DATA->Code_Base);
  *base = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR,
			   ((end - base) - 1));
  Propagate_Code(((long) start), ((long) end));
  if (start != base)
    Propagate_Code(((long) base), ((long) (base + 1)));
  SHARED_DATA->Code_Impure = ((long *) end);
  return;
}

Pointer
impurify_code_space()
{

  Pointer *start;

  /* This is obsolete, and assumes that there is one word of header
     at the beginning of code space marking what is pure.
   */

  start = ((Pointer *) SHARED_DATA->Code_Base);
  *start = NIL;

  Propagate_Code(((long) start), ((long) (start + 1)));

  return (Make_Non_Pointer(TC_FIXNUM,
			   (((Pointer *) SHARED_DATA->Code_Free) - start)));
}

#endif /* butterfly */

long
store_variable_cache(value, block, offset)
     Pointer value, block, offset;
{
  long result;

  result = real_store_variable_cache(value, block, offset);
  propagate_variable_cache(block, offset); 
  return (result);
}

long
make_uuo_link(value, extension, block, offset)
     Pointer value, extension, block, offset;
{
  long result;

  result = real_make_uuo_link(value, extension, block, offset);
  propagate_uuo_link(block, offset);
  return (result);
}

long
make_fake_uuo_link(extension, block, offset)
     Pointer extension, block, offset;
{
  long result;

  result = real_make_fake_uuo_link(extension, block, offset);
  propagate_uuo_link(block, offset);
  return (result);
}

#ifdef CODE_COPY_IMAGE
#include <signal.h>

void
initialize_section_fault_data()
{
  int i;

  for (i = 0; i < MAX_CODE_SECTIONS; i++)
    code_space_section_exists[i] = false;
}

void
Section_Fault(sig, fault_address, scp)
     int sig;
     long *fault_address;
     struct sigcontext *scp;
{
  extern int Hardware_Trap();
  int section_no, count;
  kern_return_t code;
  long *new_section, *image_section, value;

  if (Debug_Flags[23])
    printf("Section_Fault(%d, %x, %x -> %x)\n", sig, fault_address, scp, scp->sc_pc);

  if (fault_address < SHARED_DATA->Code_Base &&
      fault_address >= SHARED_DATA->Code_Top)
    {
      Hardware_Trap(sig, fault_address, scp);
    }

  /* Note the C hackery below -- multiply by sizeof(long) to get byte address,
     since the pointer subtraction will be in pointed-to units, i.e., long */

  section_no = (((fault_address - SHARED_DATA->Code_Base) * sizeof(long)) /
		SECTIONSIZE);

  if (section_no < 0 || section_no >= MAX_CODE_SECTIONS || code_space_section_exists[section_no])
    {
      Hardware_Trap(sig, fault_address, scp);
    }

  new_section = (long *) (((long) fault_address) & SECTIONMASK);
  image_section = &Code_Copy_Area[section_no * (SECTIONSIZE / sizeof(long))];

  if (Debug_Flags[23])
    printf("section number = %d, new_section is at %x, image_section is at %x\n",
	   section_no, new_section, image_section);

  code = vm_allocate_and_bind(task_self(), &new_section, SECTIONSIZE, false, Who_Am_I);
  if (code != KERN_SUCCESS)
    {
      fprintf(stderr, "-->> Unable to create code space section %d at %x -> %d\n",
	      section_no, new_section, code);
      Hardware_Trap(sig, fault_address, scp);
    }

  code_space_section_exists[section_no] = true;

  for (count = 0; count < (SECTIONSIZE / sizeof(long)); count++)
    *new_section++ = *image_section++;

  if (Debug_Flags[23])
    printf("... return from section fault ...\n");
}
#else
initialize_section_fault_data()
{
}
#endif
