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

/* Copyright 1987 Bolt Beranek and Newman Inc. */
/* All Rights Reserved */
/* 9/21/87 Fixed to support named files by Seth Steinberg */
/* delete a file from the ram board */

#include <public.h>
#include <ramboard.h>
#define de_size sizeof(struct dir_entry)

extern long ram_board_mb_address; /* Stored in Chrysalis Data seg */

main(argc,argv)
int argc;
char * argv[];
{
  OID pid;
  catch
    do_rb_rm(argc,argv);
  onthrow
    when (TRUE) {
      if (Proc_Node != King) {
/*
	printf("ram-rm failed on node %d. Trying king node %x\n",
	       Proc_Node,King);
Assume only on ramboard file cache
*/
	pid = Execute(King,argv[0],argv,NULL,NULL,NULL);
	while (Obj_OK(pid))
	  Sleep(1000);
      } else { rethrow; }
    }
  endcatch;
}


do_rb_rm (argc, argv)
int argc;
char * argv[];
{
    int i;
    unsigned long board_address;
    struct ram_directory * dir;
    char * ofile;

    if (argc<2) {
	printf("Usage: ram-rm <filename>...");
	exit(1);
	}


    board_address = ram_board_mb_address; 
    dir = (struct ram_directory *) Map_MB(board_address, 64 * 1024, 0, RW_rw_);
    if (RB_dir_check(dir) != 0)	
      throw(FAILED,"ramboard directory checksum invalid",0);

    for (i=1; i<argc; i++) {
      ofile = argv[i];
      catch
	RBdelete(ofile);
      onthrow
	when(TRUE) {
	  printf("Unable to delete %s - %s\n",
		 ofile,throwtext);
	}
      endcatch
   }
}
