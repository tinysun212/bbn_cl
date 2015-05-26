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
#define nullchar (char *) NULL
#define nullfile (FILE *) NULL

#include <stdio.h>
#include "macform.h"

char inline[256], outline[256], *outptr,
	*malloc(), *fgets(), *skip_white(), *get_token(), *skip_comma(),
	*string_copy(), *last_part_of(), incldir[128];
int commentp, debugp, lastchar, butterflyp;
FILE *fopen(), *ofile;
extern struct binding *bindings;

struct parsed_line parsed;
struct macro *macros[m_buckets], *lookup_macro();

main(argc,argv)
int argc;
char *argv[];
{	char iname[128], oname[128], *fname, *arg, incname[128],
		listname[1288], *asmname, *output;
	int i, argno, callasm, helperp, listp, deletep;
	FILE *ifile;

	if (argc<2) {
		printf("CVT converts files from MIDAS to AS68 format\n");
		return; }

	butterflyp=false;
	incldir[0]='\0';
	listp=false;
	deletep=true;
	helperp=true;
	callasm=false;
	bindings=nullbind;
	commentp=false;
	debugp=false;
	fname=nullchar;
	output=nullchar;
	asmname="gas";
	for (i=0; i<m_buckets; i++)
		macros[i]=nullmac;
	make_op_hash();
	init_equs();
	bind_equ("ccr","cc");

	for (argno=1; argno<argc; argno++) {
		arg=argv[argno];
		if (arg[0]=='-') {
			switch(arg[1]) {
			case 'a': case 'A':
				callasm=true;
				break;
			case 'b': case 'B':
				butterflyp=true;
				asmname="/usr/butterfly/bin/as68";
				break;
			case 'c': case 'C':
				commentp=true;
				break;
			case 'd': case 'D':
				debugp=true;
				break;
			case 'i': case 'I':
				strcpy(incldir,&arg[2]);
				break;
			case 'l': case 'L':
				callasm=true;
				listp=true;
				break;
			case 'n': case 'N':
				helperp=false;
				break;
			case 'o': case 'O':
				argno++;
				output=argv[argno];
				break;
			case 'p': case 'P':
				deletep=false;
				break;
			case 'u': case 'U':
				asmname=(&arg[2]);
				break;
			default:
				printf("cvt: Unknown option: %s\n",arg);
				return; } }
		else if (fname!=nullchar) {
			printf("cvt: More than one filename specified: %s\n",
						arg);
			return; }
		else fname=arg; }

	if (butterflyp) {
	  replace_mapping("space",".blkb"); }

	{	int len;
		len=strlen(fname);
		if (fname[len-2]=='.' && fname[len-1]=='s') 
		  fname[len-2]='\0'; }
	strcpy(iname,fname);
	{ int len;

	  len=strlen(iname);
	  if (strcmp(&iname[len-2],".s")!=0 &&
	      strcmp(&iname[len-3],".m4")!=0)
	    strcat(iname,".s"); }

	strcpy(incname,last_part_of(fname));
	strcat(incname,".inc68");
	strcpy(oname,last_part_of(fname));
	strcat(oname,".a68");
	if (listp) {
		strcpy(listname,last_part_of(fname));
		strcat(listname,".l68"); }

	ifile=fopen(iname,"r");
	if (ifile==nullfile) {
		printf("cvt: Unable to find the file: %s\n",iname);
		return; }

	ofile=fopen(oname,"w");
	if (ofile==nullfile) {
		printf("cvt: Unable to create the file: %s\n",oname);
		fclose(ifile);
		return; }

	printf(butterflyp?
	       "Converting %s (Butterfly)\n":
	       "Converting %s (SUN3)\n",iname);
	fflush(stdout);

	if (butterflyp) {
	  init_symref_table();
	  fprintf(ofile,"	.insrt \"%s\"\n",incname); }

	if (helperp) scan_for_macros("default",false);
	convert_loop(ifile);

	fclose(ifile);
	fclose(ofile);

	if (butterflyp) dump_symbols(incname);

	if (callasm) {
		printf("Assembling\n");
		fflush(stdout);
		if (output==nullchar) {
		  if (listp)
		    execlp(asmname,asmname,oname,"-l",listname,0);
		  else execlp(asmname,asmname,oname,0); }
		else {
		  if (listp)
		    execlp(asmname,asmname,oname,"-l",listname,
			   "-o",output,0);
		  else execlp(asmname,asmname,oname,"-o",output,0); } } }

convert_loop(ifile)
FILE *ifile;
{	while(fgets(inline,sizeof(inline),ifile)!=nullchar) {
		parse_line();
		if (strcmp(parsed.opcode,"INCLUDE")==0) {
			scan_for_macros(parsed.operands[0],true); }
		else if (strcmp(parsed.opcode,"%INCLUDE")==0) {
			scan_for_macros(parsed.operands[0],true); }
		else if (strcmp(parsed.label,"%DEFMACRO")==0) {
			define_macro(ifile); }
		else convert_line(); } }

parse_line()
{	char *here;
	int i, ch, quoted;

	/* Change HP comments into our comments */

	quoted=false;
	for (i=0; inline[i]!='\0'; i++) {
	  ch=inline[i];
	  if (!quoted) {
	    if (ch=='"') quoted=true;
	    else if (ch=='#') inline[i]=';'; }
	  else if (ch=='"') quoted=false; }

	parsed.label[0]='\0';
	parsed.label_end=' ';
	parsed.opcode[0]='\0';
	parsed.n_operands=0;
	parsed.comment[0]='\0';

	here=inline;
	if (at_eol(here)) return;
	if (inline[0]!=' ' && inline[0]!='*') {
		here=get_token(here,parsed.label);
		i=strlen(parsed.label);
		if (i>0) {
			ch=parsed.label[i-1];
			if (ch==':') {
				parsed.label[i-1]='\0';
				parsed.label_end=':'; }
			else parsed.label_end=' '; } }

	here=skip_white(here);
	if (here==nullchar || *here=='*') return;

	here=get_token(here,parsed.opcode);

	for (i=0; i<8; i++) {
		here=skip_white(here);
		if (here==nullchar) return;

		here=get_token(here,parsed.operands[i]);
		if (parsed.operands[i][0]!='\0') parsed.n_operands=i+1;
		here=skip_comma(here);
		if (here==nullchar) return; } }

char *get_token(line,token)
char *line, *token;
{	int ch, in_string, tp, level;

	level=0;
	in_string=false;
	tp=0;
	while(!at_eol(line)) {
		ch=(*line);
/*		if (is_white(ch)) break; */
		if (!in_string) {
			if (level==0 && ch==',') break;
			else if (level==0 && is_white(ch)) break;
			else if (ch=='(') level++;
			else if (ch==')') level--; }
		if (ch=='"') in_string=(in_string)?false:true;
		token[tp++]=ch;
		line++; }
	token[tp]='\0';
	return(line); }

char *skip_comma(line)
char *line;
{	int ch;

	line=skip_white(line);
	if (line==nullchar) return nullchar;
	ch=(*line);
	if (ch==',') line++;
	line=skip_white(line);
	return line; }

int at_eol(line)
char *line;
{	int ch;

	if (line==nullchar) return(true);
	ch=line[0];
	if (ch==';' || ch=='|') copy_comment(line);
	if (ch=='\0' || ch=='\012' || ch=='\015'|| ch==';' || ch=='|')
		return(true);
	return(false); }

copy_comment(text)
char *text;
{	int ch, cp;

	cp=0;
	for (ch=(*text); ch>=' ' || ch=='\011' || ch=='\010'; ch=(*++text)) {
		parsed.comment[cp++]=ch; }
	parsed.comment[cp]='\0'; }

char *skip_white(line)
char *line;
{	int ch;

	while(!at_eol(line) && is_white(*line)) line++;
	if (at_eol(line)) return(nullchar);
	return(line); }
		
int is_white(ch)
int ch;
{	if (ch<=' ') return(true);
	return(false); }

int down_case(ch)
int ch;
{	if (ch>='A' && ch<='Z') ch+=32;
	return(ch); }

convert_line()
{	int i, definingp;
	char newop[128], oldop[128];
	struct macro *macdef;

	outptr=outline;

	if (strcmp(parsed.opcode,"set")==0 || strcmp(parsed.opcode,"SET")==0) {
		convert_set();
		return; }

	if (parsed.label[0]!='\0') {
		if (parsed.label_end!=':') {
			macdef=lookup_macro(parsed.label);
			if (macdef!=nullmac) {
				for (i=parsed.n_operands; i>0; i--)
					strcpy(parsed.operands[i],
						parsed.operands[i-1]);
				strcpy(parsed.operands[i],parsed.opcode);
				parsed.n_operands++;
				expand_macro(macdef);
				return; } }
		if (butterflyp) enter_symbol(parsed.label, true);
		expand_to_line(parsed.label);
		if (parsed.opcode[0]!='=' || parsed.opcode[1]!='\0')
							append_to_line(":"); }
	append_to_line("\011");

	if (butterflyp) {
	  if (strcmp(parsed.opcode,"movm.l")==0) move_multiple();
	  else if (strcmp(parsed.opcode,"movm.w")==0) move_multiple();
	  else if (strcmp(parsed.opcode,"lea")==0 &&
		   handle_special_lea()) return;
	  else if (parsed.opcode[0]=='m' && parsed.opcode[1]=='o' &&
		   parsed.opcode[2]=='v' &&
		   handle_special_mov()) return;
	  else if (strcmp(parsed.opcode,"jsr")==0 &&
		   handle_special_jsr()) return; }

	if (strcmp(parsed.opcode,"cmpm.b")==0) comp_multiple();
	else if (strcmp(parsed.opcode,"cmpm.w")==0) comp_multiple();
	else if (strcmp(parsed.opcode,"cmpm.l")==0) comp_multiple();
	else if (strcmp(parsed.opcode,"cmp.l")==0) hack_compare();
	else if (strcmp(parsed.opcode,"cmp.w")==0) hack_compare();
	else if (strcmp(parsed.opcode,"cmp.b")==0) hack_compare();
	else if (strcmp(parsed.opcode,"cmpi.l")==0) hack_compare();
	else if (strcmp(parsed.opcode,"cmpi.w")==0) hack_compare();
	else if (strcmp(parsed.opcode,"cmpi.b")==0) hack_compare();
	else if (strcmp(parsed.opcode,"mnote")==0) {
		strcpy(parsed.opcode,"; mnote");
		strcpy(oldop,"Note: ");
		for (i=0; i<parsed.n_operands; i++) {
			expand_copy(newop,parsed.operands[i]);
			strcat(oldop,newop);
			strcat(oldop," "); }
		printf("%s\n",oldop); }
	else if (parsed.n_operands==2) {
		strcpy(newop,parsed.operands[1]);
		if (strcmp(newop,"ccr")==0) fixup_to_ccr();
		if (strcmp(newop,"sr")==0) fixup_to_sr(); }

	newop[0]='\0';
	if (parsed.opcode[0]!='\0') {
		expand_copy(oldop,parsed.opcode);
		macdef=lookup_macro(oldop);
		if (macdef!=nullmac) {
			if (parsed.label[0]!='\0') {
				*outptr='\0';
				fprintf(ofile,"%s\n",outline); }
			expand_macro(macdef);
			return; }
		else if (cvtop(oldop,newop)) {
			append_to_line(newop); }
		else {	strcpy(newop,oldop);
			expand_to_line(newop); } }

	if (butterflyp) definingp=(strcmp(newop,".globl")==0);

	for (i=0; i<parsed.n_operands; i++) {
		if (i==0) append_to_line("\011");
		expand_operand(parsed.operands[i]);
		if (butterflyp) process_symbol_names(parsed.operands[i],definingp);
	 	if (i<parsed.n_operands-1) append_to_line(",");
				      else append_to_line("\011"); }

	if (commentp && parsed.comment[0]!='\0') {
		append_to_line(parsed.comment); }

	output_line(newop); }

output_line(newop)
char *newop;
{	*outptr='\0';
	if (!output_only_white()) fprintf(ofile,"%s\n",outline);
	if (debugp) fflush(ofile); }

append_to_line(what)
char *what;
{	int ch;

	for (ch=(*what); ch!='\0'; ch=(*++what)) {
		lastchar=ch;
		*outptr++=ch; } }


expand_to_line(text)
char *text;
{	char temp[256];

	expand_copy(temp,text);
	append_to_line(temp); }


output_only_white()
{	int i, ch;

	for (i=0; outline[i]!='\0'; i++) {
		ch=outline[i];
		if (!is_white(ch)) return(false); }
	return(true); }

scan_for_macros(fname,reportp)
char *fname;
int reportp;
{	FILE *ifile;
	char realname[128], temp[128];
	int i;

	for (i=0; fname[i]!='\0'; i++) {
		if (fname[i]==':') {
			fname=(&fname[i+1]);
			break; } }

	strcpy(realname,fname);
	i=strlen(realname);
	if (i<4 || strcmp(&realname[i-4],".s")!=0) strcat(realname,".s");

	ifile=fopen(realname,"r");
	if (ifile==nullfile) {
		strcpy(temp,incldir);
		strcat(temp,"/");
		strcat(temp,realname);
		strcpy(realname,temp); }
	if (ifile==nullfile) {
		if (reportp)
			printf("cvt: Unable to find include file: %s\n",realname);
		return; }

	convert_loop(ifile);

	fclose(ifile); }

struct macro *lookup_macro(name)
char *name;
{	struct macro *othermac;
	char macroname[64];
	int buckno;

	if (name[0]=='%') strcpy(macroname,&name[1]);
		     else strcpy(macroname,name);

	buckno=hash_macro(macroname);
	for (othermac=macros[buckno];
			othermac!=nullmac;
			othermac=othermac->macnext) {
		if (strcmp(othermac->macname,macroname)==0)
			return(othermac); }
	return(nullmac); }

define_macro(file)
FILE *file;
{	struct macro *newmac, *othermac;
	struct maclist *lastpart, *thispart;
	int i, buckno;
	char *macname;

	macname=parsed.opcode;
	if (*macname=='%') macname++;
	if (debugp) printf("Defining macro %s\n",macname);
	othermac=lookup_macro(macname);
	if (othermac!=nullmac) {
			printf("cvt: Redefinition of macro: %s\n",macname);
			return; }

	buckno=hash_macro(macname);
	newmac=(struct macro *) malloc(sizeof(struct macro));
	newmac->macnext=macros[buckno];
	macros[buckno]=newmac;
	newmac->macname=string_copy(macname);
	newmac->macdef=nullmaclist;
	newmac->macnargs=parsed.n_operands;
	for (i=0; i<parsed.n_operands; i++)
		strcpy(newmac->macarg[i],parsed.operands[i]);

	lastpart=nullmaclist;
	while(fgets(inline,sizeof(inline),file)!=nullchar) {
		parse_line();
		if (strcmp(parsed.label,"%ENDMACRO")==0) break;
		thispart=(struct maclist *) malloc(sizeof(struct maclist));
		thispart->maccdr=nullmaclist;
		if (lastpart==nullmaclist) newmac->macdef=thispart;
				      else lastpart->maccdr=thispart;
		thispart->maclabel=string_copy(parsed.label);
		thispart->maclabel_end=parsed.label_end;
		thispart->macopcode=string_copy(parsed.opcode);
		thispart->macn_operands=parsed.n_operands;
		for (i=0; i<parsed.n_operands; i++) {
			thispart->macoperands[i]=
				string_copy(parsed.operands[i]); }
		lastpart=thispart; } }

hash_macro(name)
char *name;
{	int i, buckno;

	buckno=0;
	for (i=0; name[i]!='\0'; i++)
		buckno+=name[i];
	buckno%=m_buckets;
	return(buckno); }

char *string_copy(old)
char *old;
{	char *new;

	new=malloc(strlen(old)+1);
	strcpy(new,old);
	return(new); }

char *last_part_of(name)
char *name;
{	char *sofar;
	int ch;

	sofar=name;
	for (ch=(*name); ch!='\0'; ch=(*++name)) {
		if (ch=='/') sofar=(&name[1]); }
	return(sofar); }

hack_compare()
{	char temp[128];

	strcpy(temp,parsed.operands[0]);
	strcpy(parsed.operands[0],parsed.operands[1]);
	strcpy(parsed.operands[1],temp);

/*	if (parsed.operands[1][0]=='&') {
	  strcpy(temp,parsed.operands[0]);
	  strcpy(parsed.operands[0],parsed.operands[1]);
 	  strcpy(parsed.operands[1],temp); }
	else if (strlen(parsed.operands[0])==3 && parsed.operands[0][0]=='%') {
	  strcpy(temp,parsed.operands[0]);
	  strcpy(parsed.operands[0],parsed.operands[1]);
	  strcpy(parsed.operands[1],temp); } */ }
