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

#include <public.h>
#include <stdio.h>

int biggest_in_cluster, get_biggest_in_cluster(), size_of_biggest, satisfiedp;

main(argc,argv)
int argc;
char *argv[];
{ int debugx, consolex, ddtx, wholinex, argno, master, use_inet, nargs;
  char *arg, *whichlisp, *xstring, *newargs[64], *system, *version, temp[32];
  
  biggest_in_cluster=9999;
  satisfiedp=false;
  For_All_Nodes(get_biggest_in_cluster,0); 

  system="";
  version=LDIR;
  nargs=0;
  debugx=false;
  ddtx=false;
  consolex=false;
  wholinex=500;
  whichlisp="blisp";
  xstring="";
  master=biggest_in_cluster;
  use_inet=true;

  for (argno=1; argno<argc; argno++) {
    arg=argv[argno];
    if (strcmp(arg,"-debug")==0) debugx=true;
    else if (strcmp(arg,"-ddt")==0) ddtx=true;
    else if (strcmp(arg,"-console")==0) {
      consolex=true;
      newargs[nargs++]=arg; }
    else if (strcmp(arg,"-version")==0 || strcmp(arg,"-v")==0) {
      argno++;
      version=argv[argno]; }
    else if (strcmp(arg,"-system")==0) {
      argno++;
      system=argv[argno]; }
    else if (strcmp(arg,"-on")==0) {
      sscanf(argv[argno+1],"%x",&master);
      argno++; }
    else if (strcmp(arg,"-x")==0) xstring="x-";
    else if (strcmp(arg,"-i")==0 || strcmp(arg,"-interval")==0) {
      sscanf(argv[argno+1],"%x",&wholinex);
      argno++; }
    else if (strcmp(arg,"-sfs")==0) use_inet=false;
    else newargs[nargs++]=arg; }

  fprintf(stderr, "Master processor is node %x\n", master);

  sprintf(temp,"%x",master);
  putgenv("LISP_MASTER",temp);

  if (debugx) whichlisp="dblisp";

  printf("setgenv sdir %s\n",version);
  printf("setgenv HOME %s\n",version);
  if (use_inet) {
    printf("load -on %x -sars 32 transman.68\n",master);
    printf("load -on %x -sars 16 outman.68\n",master);
    printf("load -on %x -sars 248 %s.68\n",master,whichlisp); }
  else {
    printf("setgenv wdir /usr/blisp/%s/%suc%s\n",version,xstring,system);
    printf("sfsload -sars 32 transman.68\n");
    printf("sfsload -sars 16 outman.68\n");
    printf("sfsload -sars 248 %s.68\n",whichlisp); }

  if (debugx || ddtx) {
    if (use_inet) {
      printf("load /usr/butterfly/chrys/%s/tools/ddt\n",master,SVER);
      printf("load %s.syms\n",whichlisp); }
    else {
      printf("sfsload /usr/butterfly/chrys/%s/tools/ddt -16\n",SVER);
      printf("sfsload %s.syms\n",whichlisp); } }
    
  if (consolex) {
/*    printf("milliken -l &\n"); */
    printf("cleanup -tcp -lport 26\n");
    printf("cleanup -tcp -lport 27\n");
    printf("wholine -i %d &\n",wholinex); }
  else if (use_inet)
    printf("load -on %x hostem.68\n",master);
  else printf("sfsload hostem.68 -16\n");

  if (debugx) {
    printf("ddt ");
    printf("-k -n%x ",master); }
  else printf("run -on %x -kernel ",master);

  printf("%s ",whichlisp);

  for (argno=0; argno<nargs; argno++)
    printf("%s ",newargs[argno]);
  printf("\n"); 
  exit(0); }

node_memory_size(node_number)
int node_number;
{ int n_objects, no_throw, i;
  OID objects[256];

  no_throw=true;
  n_objects=0;
  while (n_objects<16 && no_throw) {
    catch
      objects[n_objects]=Make_Obj(' ', node_number, 65536, 0);
      n_objects++;
    onthrow
	when(true)
	  { no_throw=false; }
    endcatch }

  for (i=0; i<n_objects; i++)
    Del_Obj(objects[i]);

  return n_objects; }

int get_biggest_in_cluster(node_number,zero)
int node_number, zero;
{ int this_node_size;

  if (satisfiedp) return;

  this_node_size=node_memory_size(node_number);
  if (this_node_size > 16) satisfiedp = true;

  if (biggest_in_cluster==9999 || this_node_size>size_of_biggest) {
    biggest_in_cluster=node_number;
    size_of_biggest=this_node_size; } }
