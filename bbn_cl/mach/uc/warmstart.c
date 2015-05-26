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

/* Convenient loading of the Scheme system with newio code and extra
   processes */

#define true 1
#define false 0

#include <stdio.h>

main(argc, argv)
int argc; char *argv[];
{ char *arg, *newargs[64], *nodenum, *whotime, *getenv();
  int argno, newargno, cleanup, debug, ddtp;

  cleanup=false;
  debug=false;
  ddtp=false;
  whotime="1000";

  nodenum=getenv("LISP_MASTER");
  if (nodenum==(char *) NULL) {
    fprintf(stderr,"warmstart: Lisp has not been run in this cluster before.\n");
    fprintf(stderr,"           Please use startup to reinitialize things.\n");
    exit(1); }

  newargno=0;
  for (argno=1; argno<argc; argno++) {
    arg=argv[argno];
    if (strcmp(arg,"-debug")==0) debug=true;
    else if (strcmp(arg,"-ddt")==0) ddtp=true;
    else if (strcmp(arg,"-console")==0) cleanup=true;
    else if (strcmp(arg,"-i")==0 || strcmp(arg,"-interval")==0)
      whotime=argv[argno+1];
    else if (strcmp(arg,"-on")==0) nodenum=argv[argno+1];
    else newargs[newargno++]=arg; }

  if (cleanup) {
    printf("cleanup -tcp -lport 26\n");
    printf("cleanup -tcp -lport 27\n");
    printf("wholine -i %s &\n",whotime); }

  printf("run -on %s ", nodenum);
  if (debug || ddtp) printf("ddt ");
  if (debug) printf("d");
  printf("blisp ");
  if (cleanup) printf("-console ");

  for (argno=0; argno<newargno; argno++)
    printf("%s ",newargs[newargno]);

  printf("\n");
  exit(0); }
