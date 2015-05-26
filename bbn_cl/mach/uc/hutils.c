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

#define Type_Code(x) (((x) >> 24) & 0x7f)

#define Hist_Car(x) (fetch_word(x))
#define Hist_Cdr(x) (fetch_word(((long *)x)+1))
#define Hist_Nth(x,n) (fetch_word(((long *)x)+n))

output_item(item)
int item;
{	int type, n, i;
	long symbols;

	type=(item>>24)&0x7f;
	switch(type) {
	case 0x01:		/* list */
	  output_list(item, 0, 0);
	  break;
	case 0x12:		/* environment */
	  symbols=Hist_Cdr(Hist_Car(Hist_Nth(item,1)));
	  n=Hist_Nth(symbols,0);
	  for (i=0; i<n; i++) {
	    output_string(fetch_word(Hist_Nth(symbols,i+1)));
	    if (i+1<n) append_char(' '); }
	  break;
	case 0x1a:		/* fixnum */
	  append_printf("%d", (item & 0x01000000) ?
			(item | 0xff000000) : (item & 0x00ffffff));
	  break;
	case 0x1d:		/* symbol */
	  append_char('\'');
	  output_string(fetch_word(item));
	  break;
	case 0x1e:		/* string */
	  append_char('"');
	  output_string(item);
	  append_char('"');
	  break;
	case 0x2c:		/* variable */
	  output_string(fetch_word(fetch_word(item)));
	  break;
	default:
	  append_printf("0x%08x", item);
	  break; } }

output_string(addr)
int addr;
{	int slen, wlen, w, s, word;

	slen=(fetch_word(addr+4)&0x00ffffff);
	wlen=((slen+3)&0x0000fffc)/4;
	for (w=0; w<wlen && !at_end_of_buffer(); w++) {
		word=fetch_word((addr+8+(w*4)));
		if (slen>0) append_char((word>>24)&0xff);
		if (slen>1) append_char((word>>16)&0xff);
		if (slen>2) append_char((word>>8)&0xff);
		if (slen>3) append_char(word&0xff);
		slen-=4; } }

/*!!!!*/
int maxlistlen = 10, maxlistdep = 3;

output_list(item, depth)
long item, depth;
{ int the_car, the_cdr;

  if (at_end_of_buffer()) return;

  if (depth >= maxlistdep) {
    append_string("(...)");
    return; }

  append_char('(');
  for (; ; item = the_cdr) {
    the_car = fetch_word(item);
    if (Type_Code(the_car) == 0x01)
      output_list(the_car, depth+1);
    else
      output_item(the_car);

    the_cdr = fetch_word(item);
    if (the_cdr == NIL) break;
    else if (Type_Code(the_cdr) != 0x01) {
      append_string(" . ");
      output_item(the_cdr);
      break; }
    else append_char(' '); }

  append_char(')'); }

fetch_word(addr)
int addr;
{	int result;

	Read_Process(process_oid,addr&0x00ffffff,4,&result);
	return result; }

char output_buffer[256], *output_bufpos;
int output_length, output_maxlen = 70;

start_output()
{ output_length = 0;
  output_bufpos = output_buffer; }

finish_output(outfile, nl)
FILE *outfile;
int nl;
{ *output_bufpos = '\0';
  fprintf(outfile, nl ? "%s\n" : "%s", output_buffer);
  start_output(); }

int append_char(ch)
int ch;
{
  if (output_length >= output_maxlen) return false;
  *output_bufpos++ = ch;
  output_length++;
  return true; }

int append_string(str)
char *str;
{ 
  while (*str != '\0') {
    if (output_length >= output_maxlen) return false;
    *output_bufpos++ = *str++;
    output_length++; }
  return true; }

int append_printf(format, value)
char *format;
long value;
{ char buffer[256];

  sprintf(buffer, format, value);
  return append_string(buffer); }

int at_end_of_buffer()
{ return output_length >= output_maxlen; }
