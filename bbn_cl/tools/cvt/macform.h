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

#define m_buckets 247

struct macro {
	struct macro *macnext;
	char *macname;
	struct maclist *macdef;
	int macnargs;
	char macarg[8][64]; };

struct maclist {
	struct maclist *maccdr;
	char *maclabel;
	int maclabel_end;
	char *macopcode;
	int macn_operands;
	char *macoperands[8]; };

#define nullmac (struct macro *) NULL
#define nullmaclist (struct maclist *) NULL

struct parsed_line {
	char label[64];
	int label_end;
	char opcode[64];
	int n_operands;
	char operands[8][128];
	char comment[128]; };

struct binding {
	struct binding *bindnext;
	char *bindname;
	char bindval[128]; };
#define nullbind (struct binding *) NULL
