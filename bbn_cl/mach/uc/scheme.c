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

#include <stdio.h>

#define true 1
#define false 0

int biggest_in_cluster, get_biggest_in_cluster(), size_of_biggest, satisfiedp;

main(argc,argv)
int argc;
char *argv[];
{ int debugx, consolex, ddtx, wholinex, argno, master, nargs;
  int compx, codex, bandx;
  char *arg, *whichlisp, *xstring, *newargs[64], *version, temp[32];
  
  biggest_in_cluster=9999;
  satisfiedp=false;
  For_All_Nodes(get_biggest_in_cluster,0); 

  version=LDIR;
  nargs=0;
  debugx=false;
  ddtx=false;
  consolex=false;
  wholinex=500;
  whichlisp="blisp";
  xstring="";
  master=biggest_in_cluster;
  compx=false;
  codex=false;
  bandx=false;

  for (argno=1; argno<argc; argno++) {
    arg=argv[argno];
    if (strcmp(arg,"-debug")==0) debugx=true;
    else if (strcmp(arg,"-comp")==0 || strcmp(arg, "-compiled")==0)
      compx=true;
    else if (strcmp(arg,"-ddt")==0) ddtx=true;
    else if (strcmp(arg,"-code")==0) {
      codex=true;
      newargs[nargs++]=arg; }
    else if (strcmp(arg,"-console")==0) {
      consolex=true;
      newargs[nargs++]=arg; }
    else if (strcmp(arg,"-version")==0 || strcmp(arg,"-v")==0) {
      argno++;
      version=argv[argno]; }
    else if (strcmp(arg,"-lisp")==0) {
      argno++;
      whichlisp=argv[argno]; }
    else if (strcmp(arg,"-on")==0) {
      sscanf(argv[argno+1],"%x",&master);
      argno++; }
    else if (strcmp(arg,"-band")==0 || strcmp(arg,"-fasl")==0) {
      bandx=true;
      newargs[nargs++]=arg; }
    else if (strcmp(arg,"-i")==0 || strcmp(arg,"-interval")==0) {
      sscanf(argv[argno+1],"%x",&wholinex);
      argno++; }
    else newargs[nargs++]=arg; }

  if (!bandx) {
    if (compx) {
      newargs[nargs++]="-band";
      newargs[nargs++]="bf-comp.com.band";
      if (!codex) {
	newargs[nargs++]="-code";
	newargs[nargs++]="32"; } }
    else {
      newargs[nargs++]="-band";
      newargs[nargs++]="bf-lisp.com.band";
      if (!codex) {
	newargs[nargs++]="-code";
	newargs[nargs++]="16"; } } }

  fprintf(stderr, "Master processor is node %x\n", master);

  printf("setgenv HOME %s\n",version);
  load_if_needed("sfs",16,master);
  if (!sfs_running())
    printf("run -normal sfs &\n");

  load_if_needed("transman",32,master);
  load_if_needed("outman",16,master);
  load_if_needed(whichlisp,248,master);

if (debugx || ddtx)
    load_if_needed("ddt",32,master);
    
  if (consolex) {
    printf("cleanup -tcp -lport 26\n");
    printf("cleanup -tcp -lport 27\n");
    printf("wholine -i %d &\n",wholinex); }
  else load_if_needed("hostem",16,master);

  if (debugx) {
#ifdef BF_PLUS
    printf("ddt -n%x ",master); }
#else
    printf("ddt -k -n%x ",master); }
#endif
  else
#ifdef BF_PLUS
    printf("run -sars 248 -on %x ",master);
#else
    printf("run -sars 248 -on %x -kernel ",master);
#endif

  printf("%s ",whichlisp);

  for (argno=0; argno<nargs; argno++)
    printf("%s ",newargs[argno]);

  printf("\n"); 
  close(stdout);
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

/*
	Load if necessary.
*/

load_if_needed(name,sars,node)
char *name;
int sars, node;
{ char name68[128];
  OID Find_Named_Object();

  sprintf(name68,"%s.68",name);
  if (Find_Named_Object(name68)==(OID) NULL) {
    printf("load -sars %d -on %x %s\n",sars,node,name68); } }

/*
	Look for the specified template.
*/

OID Find_Named_Object(name)
char *name;
{ extern char *getenv();
  int Find_Named_Object_In_Path();
  OID For_All_Directories_in_Path(), temp;

  temp=Find_Named_Object_In_Path(name, NULL);
  if (temp != (OID) NULL) return temp;

  return For_All_Directories_in_Path(getenv("B_PATH"), name,
				     Find_Named_Object_In_Path, NULL); }

int Find_Named_Object_In_Path(name, ignored)
char *name;
long ignored;
{ OID result;
  
  catch {
    result=Find_Value(name, NTYPE_OBJ); }
  onthrow
    when (true) {
      result=NULL; }
  endcatch
    
  return result; }
