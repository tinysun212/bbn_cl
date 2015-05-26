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
	Since the Butterfly assembler requires ALL symbols to be
	defined while the SUN and HP assemblers do NOT have this
	requirement we must keep track of all symbols defined and
	all symbols referenced so we can put out the globals header
	file for Butterfly assemblies.
*/

#define true 1
#define false 0

#include <stdio.h>
#include "macform.h"

struct a_symbol {
  struct a_symbol *symnext;
  char *symname;
  int symdefp; } *s_buckets[m_buckets];
#define nullsym (struct a_symbol *) 0

init_symref_table()
{	int i;

	for (i=0; i<m_buckets; i++)
	  s_buckets[i]=nullsym; }

char *string_copy();

enter_symbol(name, definedp)
char *name;
int definedp;
{	struct a_symbol *symp;
	int bn;

	bn=hash_macro(name);
	for (symp=s_buckets[bn]; symp!=nullsym; symp=symp->symnext)
	  if (strcmp(symp->symname,name)==0) {
	    if (definedp) symp->symdefp=true;
	    return; }
	symp=(struct a_symbol *) malloc(sizeof(struct a_symbol));
	symp->symnext=s_buckets[bn];
	symp->symname=string_copy(name);
	symp->symdefp=definedp;
	s_buckets[bn]=symp; }

process_symbol_names(rand, defp)
char *rand;
int defp;
{ char possible[128];
  int ch, posind, ignorep;

  posind=0;
  ignorep=false;
  for ( ; *rand!='\0'; rand++)
    if (*rand=='%') ignorep=true;
    else if (*rand=='/') ignorep=true;
    else if (*rand=='0' && rand[1]=='x') ignorep=true;
    else if (is_constituent(*rand, posind==0))
      possible[posind++]=(*rand);
    else {
      if (!ignorep && posind>0) {
	possible[posind]='\0';
	enter_symbol(possible,defp); }
      if (ignorep) {
	if (!is_constituent(*rand, false)) ignorep=false; }
      posind=0; }

  if (!ignorep && posind>0) {
    possible[posind]='\0';
    enter_symbol(possible,defp); } }    

int is_constituent(ch, is_first)
int ch, is_first;
{	if (ch>='A' && ch<='Z') return(true);
	if (ch>='a' && ch<='z') return(true);
	if (!is_first && (ch>='0' && ch<='9')) return(true);
	if (!is_first && ch=='.') return(true);
	if (ch=='_') return(true);
	return(false); }

dump_symbols(fname)
char *fname;
{	FILE *symfile;
	int bn;
	struct a_symbol *symp;

	symfile=fopen(fname,"w");
	if (symfile==(FILE *) NULL) {
	  fprintf(stderr,"cvt: Unable to create %s\n",fname);
	  exit(1); }

	for (bn=0; bn<m_buckets; bn++)
	  for (symp=s_buckets[bn]; symp!=nullsym; symp=symp->symnext)
	    if (!symp->symdefp)
	      fprintf(symfile,"	.globl	%s\n",symp->symname);

	fclose(symfile); }
