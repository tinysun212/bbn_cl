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
#define nullfile (FILE *) NULL
#define n_buckets 247

#include <stdio.h>
#include "macform.h"

extern struct parsed_line parsed;
extern int debugp, butterflyp;
extern FILE *ofile;
struct binding *bindings=nullbind, *equs[n_buckets];
char *string_copy(), *expand_name();

expand_macro(macdef)
struct macro *macdef;
{	struct maclist *part;
	int i;
	struct binding *oldbindings, *newbind, *oldbind;

	fprintf(ofile,"; Expansion of %s\n",macdef->macname);
	if (debugp) {
		printf("Expansion of %s\n",macdef->macname);
		fflush(stdout); }
	oldbindings=bindings;

	oldbind=bindings;
	for (i=0; i<macdef->macnargs; i++) {
		newbind=(struct binding *) malloc(sizeof(struct binding));
		newbind->bindnext=oldbind;
		oldbind=newbind;
		newbind->bindname=macdef->macarg[i];
		if (debugp) {
			printf("Argument %s = ",newbind->bindname);
			fflush(stdout); }
		if (i<parsed.n_operands)
			expand_copy(newbind->bindval,parsed.operands[i]);
		else	strcpy(newbind->bindval,"");
		if (debugp) {
			printf("%s\n",newbind->bindval);
			fflush(stdout); } }
	bindings=oldbind;

	for (part=macdef->macdef; part!=nullmaclist; part=part->maccdr) {
		strcpy(parsed.label,part->maclabel);
		parsed.label_end=part->maclabel_end;
		strcpy(parsed.opcode,part->macopcode);
		parsed.n_operands=part->macn_operands;
		for (i=0; i<parsed.n_operands; i++) {
			strcpy(parsed.operands[i],part->macoperands[i]); }
		convert_line(); }

	while(bindings!=oldbindings && bindings!=nullbind) {
		newbind=bindings->bindnext;
		free(bindings);
		bindings=newbind;
		oldbindings=newbind; } }

expand_copy(result,source)
char *result, *source;
{	char name[64], *dbtgt, *target, buffer[256];
	int wasinname, isinname, ch, namelen;

/*	if (debugp) {
		printf("%s ->  ",source);
		fflush(stdout); }
*/
	target=buffer;
	wasinname=false;
	namelen=0;
	for (ch=(*source); ch!='\0'; ch=(*++source)) {
		isinname=is_name_ch(ch);
		if (isinname) name[namelen++]=ch;
		else if (wasinname) {
			name[namelen]='\0';
			target=expand_name(target,name);
			namelen=0;
			if (ch!='\'') *target++=ch; }
		else if (ch!='\'') *target++=ch;
		wasinname=isinname; }
	if (namelen>0) {
		name[namelen]='\0';
		target=expand_name(target,name); }
	*target='\0';

/*	if (debugp) {
		printf("%s\n",buffer);
		fflush(stdout); }
*/
	strcpy(result,buffer); }

char *expand_name(target,name)
char *target, *name;
{	struct binding *newbind;
	char *value;
	int ch, hashit;

	hashit=false;
	value=name;
	for (newbind=bindings; newbind!=nullbind; newbind=newbind->bindnext) {
			if (strcmp(name,newbind->bindname)==0) {
				hashit=true;
				value=newbind->bindval;
				break; } }

	if (!hashit) for (newbind=equs[hash_macro(name)%n_buckets];
				newbind!=nullbind;
				newbind=newbind->bindnext) {
			if (strcmp(name,newbind->bindname)==0) {
				hashit=true;
				value=newbind->bindval;
				break; } }

	for (ch=(*value); ch!='\0'; ch=(*++value)) {
		*target++=ch; }
	*target='\0';
	return(target); }

is_name_ch(ch)
int ch;
{	if (ch>='a' && ch<='z') return(true);
	if (ch>='A' && ch<='Z') return(true);
	if (ch>='0' && ch<='9') return(true);
	if (ch=='_') return(true);
	if (ch=='-') return(true);
	return(false); }

convert_set()
{	char *p;

	append_to_line(parsed.operands[0]);
	append_to_line("\011=\011");
	if (butterflyp) {
	  enter_symbol(parsed.operands[0],true);
	  for (p=parsed.operands[1]; *p!='\0'; p++)
	    if (*p=='(') *p='[';
	    else if (*p==')') *p=']'; }
	append_to_line(parsed.operands[1]);
	
	output_line("="); }

bind_equ(name,target)
char *name, *target;
{	struct binding *newbind;
	int bucket;

	newbind=(struct binding *) malloc(sizeof(struct binding));
	bucket=hash_macro(name)%n_buckets;
	newbind->bindnext=equs[bucket];
	equs[bucket]=newbind;
	newbind->bindname=string_copy(name);
	expand_copy(newbind->bindval,target); }

init_equs()
{	int i;

	for (i=0; i<n_buckets; i++) {
		equs[i]=nullbind; } }

is_a_reg(str)
char *str;
{	int first, second;

	if (strlen(str)!=2) return(false);
	first=str[0];
	if (first>='A' && first<='Z') first+=32;
	second=str[1];
	if (second>='A' && second<='Z') second+=32;
	if (first=='c' && second=='c') return(true);
	if (first=='s' && second=='r') return(true);
	if (first=='p' && second=='c') return(true);
	if (first=='s' && second=='p') return(true);
	if ((first=='a' || first=='d') && second>='0' && second<='7')
		return(true);
	return(false); }

move_multiple()
{	char *reglist, regname[3], regp[16];
	int which, mask, ch, i, fromreg, other, incdec, oldhi, newhi;

	if (is_not_reglist(parsed.operands[0])) which=1;
				       else which=0;
	other=1-which;
	incdec=parsed.operands[other][strlen(parsed.operands[other])-1];
	reglist=parsed.operands[which];
	regname[2]='\0';
	mask=0;

	while(true) {
		ch=(*reglist);
		if (ch<=' ') break;
		if (ch=='%') {
		  reglist++;
		  ch=(*reglist); }
		regname[0]=(*reglist++);
		regname[1]=(*reglist++);
		fromreg=get_regind(regname,incdec);
		if (fromreg==0) break;
		ch=(*reglist++);
		if (ch=='/' || ch=='\0') mask|=fromreg;
		else if (ch=='-') {
			oldhi=regname[1];
			if (*reglist=='%') reglist++;
			regname[0]=(*reglist++);
			regname[1]=(*reglist++);
			newhi=regname[1];
			for (i=oldhi; i<=newhi; i++) {
				if (i=='8') break;
				regname[1]=i;
				fromreg=get_regind(regname,incdec);
				if (fromreg==0) break;
				mask|=fromreg; }
			ch=(*reglist++);
			if (ch=='\0') break;
			else if (ch!='/') break; }
		else break; }

	sprintf(parsed.operands[which],"#/%x",mask); }

get_regind(regname,incdec)
char *regname;
int incdec;
{	int result, ch;
	
	ch=regname[0];
	if (ch=='a' || ch=='A') result=8;
	else if (ch=='d' || ch=='D') result=0;
	else return(0);

	ch=regname[1];
	if (ch>='0' && ch<='7') result+=ch-'0';
			   else return(0);
	if (incdec=='+') return(1<<result);
		    else return(1<<(15-result)); }

is_not_reglist(reglist)
char *reglist;
{	return *reglist!='%'; }

fixup_to_ccr()
{	char three[4];

	three[0]=parsed.opcode[0];
	three[1]=parsed.opcode[1];
	three[2]=parsed.opcode[2];
	three[3]='\0';
	if (strcmp(three,"mov")==0) strcpy(parsed.opcode,"movw");
	else if (strcmp(three,"and")==0) strcpy(parsed.opcode,"andb");
	else if (strcmp(three,"or")==0) strcpy(parsed.opcode,"orb"); }

fixup_to_sr()
{	char three[4];

	three[0]=parsed.opcode[0];
	three[1]=parsed.opcode[1];
	three[2]=parsed.opcode[2];
	three[3]='\0';
	if (strcmp(three,"mov")==0) strcpy(parsed.opcode,"movw");
	else if (strcmp(three,"and")==0) strcpy(parsed.opcode,"andw");
	else if (strcmp(three,"or")==0) strcpy(parsed.opcode,"orw"); }

comp_multiple()
{	char temp[128];
	int i;

	for (i=0; i<2; i++) {
		strcpy(temp,parsed.operands[i]);
		strcpy(parsed.operands[i],"+");
		strcat(parsed.operands[i],temp); } }
