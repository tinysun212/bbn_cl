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
 *	File:	sbrk.c
 *
 *	Unix compatibility for sbrk system call.
 *
 * HISTORY
 * 16-Oct-86  Michael Young (mwyoung) at Carnegie-Mellon University
 *	Fixed case where sbrk(0) was being mishandled; put in
 *	initialization to get break value right before first call.
 *
 * 25-Sep-86  Michael Young (mwyoung) at Carnegie-Mellon University
 *	Merged in changes for the "romp" compilation.
 *
 * 18-Aug-86  Michael Young (mwyoung) at Carnegie-Mellon University
 *	Added in "minbrk" and "curbrk" here, rather than in a separate
 *	assembly file.
 *
 * 14-Jun-86  Avadis Tevanian (avie) at Carnegie-Mellon University
 *	Really made the types correct this time.
 *
 *  9-Jun-86  Michael Young (mwyoung) at Carnegie-Mellon University
 *	Corrected type usage, added necessary external symbols (curbrk,
 *	minbrk), added bogus brk call to avoid library hassles.
 *
 *  2-Jun-86  Avadis Tevanian (avie) at Carnegie-Mellon University
 *	Created.
 *
 */

#include <kern/mach.h>		/* for vm_allocate, vm_offset_t */
#include <stdio.h>		/* for stderr */
#include <sys/types.h>		/* for caddr_t */
#include <mach_init.h>		/* for vm_page_size */
#include <sys/errno.h>		/* for error return from sbrk() */

#if	(defined(vax) || defined(romp) || defined(butterfly))
static DEF_FUNC() {
asm(".data");
asm(".globl	curbrk");
asm(".globl	minbrk");
asm(".globl	_curbrk");
asm(".globl	_minbrk");
asm(".globl	_end");
asm("_minbrk:");
asm("minbrk:	.long	0x2000000");
asm("_curbrk:");
asm("curbrk:	.long	0x2000000");
asm(".text");
}
#else	(defined(vax) || defined(romp)

/* Will not find get "assembler" forms of cubrk, minbrk. */

#endif	(defined(vax) || defined(romp) || defined(butterfly))

extern	caddr_t	curbrk;
extern	caddr_t	minbrk;
extern	int errno;

#define	round(a,b)	((((a) + (b) - 1) / (b)) * (b))

static int sbrk_needs_init = FALSE;

caddr_t sbrk(size)
	int	size;
{
	vm_offset_t	addr;
	kern_return_t	ret;
	caddr_t		ocurbrk;

	if (sbrk_needs_init) {
		sbrk_needs_init = FALSE;
		/*
		 *	Get "curbrk" set up...
		 */
	}
	
	if (size == 0)
		return(curbrk);
	else if (size < 0) {
		errno = EINVAL;
		return((caddr_t) -1);
        }
	  
	addr = (vm_offset_t) round((int)curbrk,vm_page_size);
	ocurbrk = curbrk;
	if (((int)curbrk+size) > addr)
	{	ret = vm_allocate(task_self(), &addr, 
			    size -((int)addr-(int)curbrk), FALSE);
		if (ret == KERN_NO_SPACE) {
			ret = vm_allocate(task_self(), &addr, size, TRUE);
			ocurbrk = (caddr_t)addr;
		}
		if (ret != KERN_SUCCESS) 
			return((caddr_t) -1);
	}

	curbrk = (caddr_t)ocurbrk + size;
	return(ocurbrk);
}

caddr_t brk(x)
	caddr_t x;
{
  	caddr_t retval;

	retval = sbrk(x-curbrk);
	if (retval > 0)
		return(0);
	else return(retval);
}
