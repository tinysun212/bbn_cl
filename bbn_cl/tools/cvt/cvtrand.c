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

extern int debugp, butterflyp;
extern int lastchar;		/* Last character append to output line. */
extern FILE *ofile;

struct parsed_line parsed;

int nparts, partlevel[32];
char *parts[8], insquares[32];

handle_special_mov()
{ int byte1, byte2, byte3;
  char temp[256];

  if (parsed.operands[0][0]!='(') return false;
  if (parsed.operands[1][0]!='%' && strlen(parsed.operands[1])!=3) return false;
  strcpy(temp, parsed.operands[0]);
  if (!parse_parenthesized(temp, false)) return false;
  if (nparts!=3) return false;

  byte1 = (get_move_mode(parsed.opcode) << 4) |
    (get_reg_number(parsed.operands[1]) << 1);
  byte2 = 0x30 |
    (get_reg_da_bit(parsed.operands[1]) << 6) |
      (get_reg_number(parts[1]));
  byte3 = (get_reg_da_bit(parts[2]) << 7) |
    (get_reg_number(parts[2]) << 4) |
      (get_reg_wl_bit(parts[2]) << 3) |
	(get_scale_bits(parts[2]) << 1);

  handle_the_label();
  fprintf(ofile,"	.byte	/%x,/%x,/%x,%s\n",
	 byte1, byte2, byte3, parts[0]);

  return true; }

handle_special_jsr()
{ int byte1, byte2, byte3, byte4, base_disp_0, index_disp_0;
  char temp[256];

  if (parsed.operands[0][0]!='(') return false;
  strcpy(temp, parsed.operands[0]);
  if (!parse_parenthesized(temp, false)) return false;
  if (nparts!=5 || strcmp(insquares," xxx ")!=0) return false;

  base_disp_0 = (strcmp(parts[1],"0") == 0) ? 0 : 1;
  index_disp_0 = (strcmp(parts[4],"0") == 0) ? 0 : 1;

  byte1 = 0x4e;
  byte2 = 0xb0 | get_reg_number(parts[2]);
  byte3 = (get_reg_da_bit(parts[3]) << 7) |
    (get_reg_number(parts[3]) << 4) |
      (get_reg_wl_bit(parts[3]) << 3) |
	(get_scale_bits(parts[3]) << 1) |
	  0x01;
  byte4 = (((base_disp_0 == 0) ? 1 : 2) << 4) |
    ((index_disp_0 == 0) ? 1 : 2);

  handle_the_label();
  fprintf(ofile,"	.byte	/%x,/%x,/%x,/%x\n",
	 byte1, byte2, byte3, byte4);

  if (base_disp_0 != 0)
    fprintf(ofile,"	.word	%s\n", parts[1]);
  if (index_disp_0 != 0)
    fprintf(ofile,"	.word	%s\n", parts[4]);

  return true; }

handle_special_lea()
{ int byte1, byte2, byte3;
  char temp[256];

  if (parsed.operands[0][0]!='(') return false;
  strcpy(temp, parsed.operands[0]);
  if (!parse_parenthesized(temp, false)) return false;
  if (nparts!=3) return false;

  byte1 = 0x41 + (get_reg_number(parsed.operands[1]) << 1);
  byte2 = 0xf0 + get_reg_number(parts[1]);
  byte3 = (get_reg_da_bit(parts[2]) << 7) |
    (get_reg_number(parts[2]) << 4) |
      (get_reg_wl_bit(parts[2]) << 3) |
	(get_scale_bits(parts[2]) << 1);

  handle_the_label();
  fprintf(ofile,"	.byte	/%x,/%x,/%x,%s\n",
	 byte1, byte2, byte3, parts[0]);

  return true; }

handle_the_label()
{
  if (parsed.label[0]!='\0') {
    enter_symbol(parsed.label, true);
    fprintf(ofile,"%s:\n", parsed.label); } }

expand_operand(rand)
char *rand;
{	int ch, inquote, haveinside, unaryp, triplex, colonp, autoincflag;
	char inside[64], outside[128], *thisptr, temp[256], *oldoutside;

	expand_copy(temp,rand);
	if (*temp=='(') {
	  parse_parenthesized(temp,true);
	  return; }

	rand=temp;
	autoincflag=false;
	inquote=false;
	haveinside=false;
	unaryp=true;
	thisptr=outside;
	triplex=false;
	colonp=false;
	for (ch=(*rand); ch!='\0'; ch=(*++rand)) {
		if (inquote) {
			if (ch=='"') inquote=false;
			*thisptr++=ch; }
		else if (ch=='%');
		else if (ch=='&') *thisptr++='#';
		else if (triplex && ch=='.') {
			*thisptr++=':';
			colonp=true; }
		else if (unaryp && ch=='$') *thisptr++='/';
		else if (unaryp && ch=='*') *thisptr++='.';
		else if (ch=='"') {
			*thisptr++=ch;
			inquote=true; }
		else if (haveinside && ch==',') {
			*thisptr='\0';
			thisptr=oldoutside;
			*thisptr++=',';
			triplex=true; }
		else if (ch=='(') {
			haveinside=true;
			oldoutside=thisptr;
			*thisptr='\0';
			thisptr=inside; }
		else if (ch==')') {
			ch=(*++rand);
			if (ch=='+') autoincflag=true;
			break; }
		else *thisptr++=ch;
		if (!inquote && (ch=='+' || ch=='-' || ch=='#'))
			unaryp=true;
		   else unaryp=false; }
	*thisptr='\0';

	if (strcmp(inside,"sp")==0 || strcmp(inside,"SP")==0)
	  	strcpy(inside,"a7");

	if (haveinside) {
		if (!triplex && (inside[0]=='d' || inside[0]=='D')
				&& inside[2]=='\0') {
			append_to_line("pc@(");
			append_to_line(outside);
			append_to_line("-.-2,");
			append_to_line(inside);
			append_to_line(":w)"); }
		else {	append_to_line(inside);
			append_to_line("@");
			if ((outside[0]=='-' || outside[0]=='+')
					&& outside[1]=='\0') {
				append_to_line(outside); }
			else if (outside[0]!='\0') {
				append_to_line("(");
				append_to_line(outside);
				if (triplex && !colonp)
					append_to_line(":w");
				append_to_line(")"); } } }
	else append_to_line(outside);
	if (autoincflag && lastchar!='+') append_to_line("+");  }

/*
	Since this code is 
*/

parse_parenthesized(rand, actionp)
char *rand;
int actionp;
{ 	char *rest, oldrand[256];
	int ch, insquarep, level;

	strcpy(oldrand,rand);
	rand++;
	parts[0]=rand;
	insquares[0]=' ';
	partlevel[0]=(*rand=='(')?1:0;
	nparts=1;
	insquarep=' ';
	rest="";
	level=0;
	for (ch=(*rand); ch!='\0'; ch=(*++rand)) {
	  if (ch==')') {
	    level--;
	    if (level<=0) {
	      *rand='\0';
	      rest=rand+1; } }
	  else if (ch=='(') level++;
	  else if (ch=='[') {
	    *rand='\0';
	    insquarep='x';
	    rand++;
	    partlevel[nparts]=level;
	    parts[nparts]=rand;
	    insquares[nparts]='x';
	    nparts++; }
	  else if (ch==']') {
	    *rand='\0';
	    insquarep=' '; }
	  else if (ch==',') {
	    *rand='\0';
	    rand++;
	    partlevel[nparts]=level;
	    parts[nparts]=rand;
	    insquares[nparts]=insquarep;
	    nparts++; } }

	insquares[nparts]='\0';

	if (debugp) { int i;
	  printf("nparts=%d insquares=|%s|\n",nparts,insquares);
	  for (i=0; i<nparts; i++)
	    printf("%d (%d) -> %s\n",i,partlevel[i],parts[i]); }

	if (!actionp) return true;

	if (nparts==3) {
	  spec_to_line(parts[1]);
	  append_to_line("@(");
	  if (butterflyp) alter_parens(parts[0]);
	  spec_to_line(parts[0]);
	  if (partlevel[0]==0) append_to_line(","); 
	  else if (butterflyp) append_to_line("],");
	  else append_to_line("),");
	  spec_to_line(parts[2]);
	  append_to_line(")"); }
	else if (nparts==1) {
	  spec_to_line(parts[0]);
	  append_to_line("@");
	  append_to_line(rest); }
	else if (nparts==5 && strcmp(insquares," xxx ")==0) {
	  spec_to_line(parts[2]);
	  append_to_line("@(");
	  spec_to_line(parts[1]);
	  append_to_line(",");
	  spec_to_line(parts[3]);
	  append_to_line(")@(");
	  spec_to_line(parts[4]);
	  append_to_line(")"); }
	else printf("cvt: Unable to convert %s\n",oldrand);

      return false; }

alter_parens(what)
char *what;
{
  for ( ; *what!='\0'; what++)
    if (*what=='(') *what='[';
    else if (*what==')') *what=']'; }

get_move_mode(rator)
char *rator;
{
  switch(rator[4]) {
  case 'b':
    return 1;
  case 'w':
    return 3;
  case 'l':
    return 2;
  default:
    printf("cvt: Misformed move instruction (B/W/L) - %s\n",rator);
    return 0; } }

get_reg_number(field)
char *field;
{ if (field[1]=='s') return 7;
  if (field[2]>='0' && field[2]<='7') return field[2]-'0';
  printf("cvt: Misformed field when looking for register number - %s\n",field);
  return 0; }

get_reg_da_bit(field)
char *field;
{ if (field[1]=='s') return 1;
  if (field[1]=='a') return 1;
  if (field[1]=='d') return 0;
  printf("cvt: Misformed field when looking for D or A - %s\n",field);
  return 0; }

get_reg_wl_bit(field)
char *field;
{ if (field[4]=='w') return 0;
  if (field[4]=='l') return 1;
  printf("cvt: Misformed field when looking for W or L - %s\n",field);
  return 0; }

get_scale_bits(field)
char *field;
{
  switch (field[6]) {
  case '1':
    return 0;
  case '2':
    return 1;
  case '4':
    return 2;
  case '8':
    return 3;
  default:
    printf("cvt: Misformed field when looking for scale factor - %s\n",field);
    return 0; } }

spec_to_line(spec)
char *spec;
{	char buffer[128], *output;
	int ch, level;

	level=0;
	output=buffer;
	for (ch=(*spec); ch!='\0'; ch=(*++spec)) {
	  if (ch=='%')
	    {
	      if (spec[1]=='s' && spec[2]=='p')
		{
		  *output++='a';
		  *output++='7';
		  spec+=2;
		}
	      else if (((spec [1]) == 'z') &&
		       ((spec [2]) == 'a') &&
		       ((spec [3]) >= '0') &&
		       ((spec [3]) <= '7'))
		spec += 3;
	    }
	  else if (level==0 && ch=='.') *output++=':';
	  else if (ch=='&') *output++='#';
	  else if (ch=='*') *output++=':';
	  else {
	    if (ch=='(' || (butterflyp && ch=='[')) level++;
	    else if (ch==')' || (butterflyp && ch==']')) level--;
	    *output++=ch; } }
	*output='\0';
	append_to_line(buffer); }
