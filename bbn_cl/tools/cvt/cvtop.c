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

#include <stdio.h>
#include "macform.h"

struct optrans {
	struct optrans *opnext;
	int opwarn;
	char opfrom[8];
	char opto[8]; } *opbucks[m_buckets];
#define nullop (struct optrans *) 0

char oppairs[][9]={
	"abcd",		"abcd",
	"addb",		"add.b",
	"addw",		"add.w",
	"addl",		"add.l",
	"addb",		"addi.b",
	"addw",		"addi.w",
	"addl",		"addi.l",
	"addxb",	"addx.b",
	"addxw",	"addx.w",
	"addxl",	"addx.l",
	"addqb",	"addq.b",
	"addqw",	"addq.w",
	"addql",	"addq.l",
	"addql",	"addq",
	"addb",		"adda.b",
	"addw",		"adda.w",
	"addl",		"adda.l",
	"andb",		"and.b",
	"andw",		"and.w",
	"andl",		"and.l",
	"andb",		"andi.b",
	"andw",		"andi.w",
	"andl",		"andi.l",
	"andl",		"andi",
	"aslb",		"asl.b",
	"aslw",		"asl.w",
	"asll", 	"asl.l",
	"asrb",		"asr.b",
	"asrw",		"asr.w",
	"asrl",		"asr.l",
	"bcc",		"bcc",
	"bcc",		"bcc.w",
	"bccs",		"bcc.b",
	"bchg",		"bchg",
	"bclr",		"bclr",
	"bcs", 		"bcs",
	"bcs", 		"bcs.w",
	"bcss",		"bcs.b",
	"beq",		"beq",
	"beq",		"beq.w",
	"beqs",		"beq.b",
	"bge",		"bge",
	"bge",		"bge.w",
	"bges",		"bge.b",
	"bgt",		"bgt",
	"bgt",		"bgt.w",
	"bgts",		"bgt.b",
	"bhi", 		"bhi",
	"bhi", 		"bhi.w",
	"bhis",		"bhi.b",
	"bcc",		"bhs",
	"bcc",		"bhs.w",
	"bccs",		"bhs.b",
	"bcs",		"blo",
	"bcs",		"blo.w",
	"bcss",		"blo.b",
	"ble",		"ble",
	"ble",		"ble.w",
	"bles",		"ble.b",
	"bls",		"bls",
	"bls",		"bls.w",
	"blss",		"bls.b",
	"blt",		"blt",
	"blt",		"blt.w",
	"blts",		"blt.b",	
	"bmi", 		"bmi",
	"bmi",		"bmi.w",
	"bmis",		"bmi.b",
	"bne",		"bne",
	"bne",		"bne.w",
	"bnes",		"bne.b",
	"bpl",		"bpl",
	"bpl",		"bpl.w",
	"bpls",		"bpl.b",
	"bra",		"bra",
	"bra",		"bra.w",
	"bras",		"bra.b",
	"bset", 	"bset",
	"bsr",		"bsr",
	"bsr",		"bsr.w",
	"bsrs",		"bsr.b",
	"btst",		"btst",
	"bvc",		"bvc",
	"bvc",		"bvc.w",
	"bvcs",		"bvc.b",
	"bvs",		"bvs",
	"bvs",		"bvs.w",
	"bvss",		"bvs.b",
	"chk", 		"chk",
	"clrb",		"clr.b",
	"clrw",		"clr.w",
	"clrl",		"clr.l",
	"cmpb",		"cmp.b",
	"cmpw",		"cmp.w",
	"cmpl",		"cmp.l",
	"cmpb",		"cmpi.b",
	"cmpw",		"cmpi.w",
	"cmpl",		"cmpi.l",
	"cmpl",		"cmpa.l",
	"cmpmb",	"cmpm.b",
	"cmpmw", 	"cmpm.w",
	"cmpml",	"cmpm.l",
	".byte",	"dc.b",
	".word",	"dc.w",
	".word",	"short",
	".long",	"dc.l",
	".long",	"long",
	".globl",	"global",
	".blkb",	"ds.b",
	".blkw",	"ds.w",
	".blkl",	"ds.l",
	"divs",		"divs",
	"divu",		"divu",
	"eorb",		"eor.b",
	"eorw",		"eor.w",
	"eorl",		"eor.l",
	"exg",		"exg",
	"extw", 	"ext.w",
	"extl",		"ext.l",
	"extbl",	"extb.l",
	"illegal",	"illegal",
	"jbsr",		"jbsr",
	"jcc",		"jcc",
	"jcs",		"jcs",
	"jeq",		"jeq",
	"jge",		"jge",
	"jgt", 		"jgt",
	"jhi",		"jhi",
	"jle",		"jle",
	"jls",		"jls",
	"jlt",		"jlt",
	"jmi",		"jmi",
	"jmp",		"jmp",
	"jne",		"jne",
	"jpl", 		"jpl",
	"jra",		"jra",
	"jsr",		"jsr",
	"jvc",		"jvc",
	"jvs",		"jvs",
	"lea",		"lea",
	"link",		"link",
	"; lmode",	"lmode",
	"; rorg",	"rorg",
	"; nosym",	"nosyms",
	"; radix",	"radix",
	"; dec",	"decimal",
	"lslb",		"lsl.b",
	"lslw", 	"lsl.w",
	"lsll",		"lsl.l",
	"lsll",		"lsl",
	"lsrb",		"lsr.b",
	"lsrw",		"lsr.w",
	"lsrl",		"lsr.l",
	"; mname",	"mname",
	"movb",		"mov.b",
	"movw",		"mov.w",
	"movl",		"mov.l",
	"movb",		"movea.b",
	"movea.w",	"movea.w",
	"movea.l",	"movea.l",
	"movl",		"move",
	"movemw", 	"movem.w",
	"moveml",	"movm.l",
	"movepw",	"movep.w",
	"movepl",	"movep.l",
	"moveq",	"movq",
	"muls",		"muls",
	"mulsl",	"muls.l",
	"mulu",		"mulu",
	"mulul",	"mulu.l",
	"nbcd",		"nbcd",
	"negb", 	"neg.b",
	"negw",		"neg.w",
	"negl",		"neg.l",
	"negl",		"neg",
	"negxb",	"negx.b",
	"negxw",	"negx.w",
	"negxl",	"negx.l",
	"nop",		"nop",
	"notb",		"not.b",
	"notw", 	"not.w",
	"notl",		"not.l",
	"orb",		"or.b",
	"orw",		"or.w",
	"orl",		"or.l",
	"orb",		"ori.b",
	"orw",		"ori.w",
	"orl",		"ori.l",
	"pea",		"pea",
	".globl",	"refa",
	".globl",	"refr",
	"; lmode",	"lmode",
	"reset",	"reset",
	"rolb",		"rol.b",
	"rolw", 	"rol.w",
	"roll",		"rol.l",
	"rorb",		"ror.b",
	"rorw",		"ror.w",
	"rorl",		"ror.l",
	"roxlb",	"roxl.b",
	"roxlw",	"roxl.w",
	"roxll",	"roxl.l",
	"roxrb", 	"roxr.b",
	"roxrw",	"roxr.w",
	"roxrl",	"roxr.l",
	"rte",		"rte",
	"rtr",		"rtr",
	"rts",		"rts",
	"sbcd",		"sbcd",
	"scc",		"scc",
	"scs", 		"scs",
	"seq",		"seq",
	"sf",		"sf",
	"sge",		"sge",
	"sgt",		"sgt",
	"shi",		"shi",
	"sle",		"sle",
	"sls",		"sls",
	"slt", 		"slt",
	"smi",		"smi",
	"sne",		"sne",
	"spl",		"spl",
	"; src",	"src",
	"st",		"st",
	"stop",		"stop",
	"subb",		"sub.b",
	"subw",		"sub.w",
	"subl", 	"sub.l",
	"subb",		"subi.b",
	"subw",		"subi.w",
	"subl", 	"subi.l",
	"subl",		"suba.l",
	"subl",		"suba",
	"subqb",	"subq.b",
	"subqw",	"subq.w",
	"subql",	"subq.l",
	"subxb",	"subx.b",
	"subxw",	"subx.w",
	"subxl",	"subx.l",
	"svc",		"svc",
	"svs", 		"svs",
	"swap",		"swap",
	"tas",		"tas",
	"trap",		"trap",
	"trapv",	"trapv",
	"tstb",		"tst.b",
	"tstw",		"tst.w",
	"tstl",		"tst.l",
	"unlk",		"unlk",
	"dbra",		"dbra",
	"dbcc",		"dbcc",
	"dbcs",		"dbcs",
	"dbeq",		"dbeq",
	"dbf",		"dbf",
	"dbge",		"dbge",
	"dbgt",		"dbgt",
	"dbhi", 	"dbhi",
	"dble",		"dble",
	"dbls",		"dbls",
	"dblt",		"dblt",
	"dbmi",		"dbmi",
	"dbne",		"dbne",
	"dbpl",		"dbpl",
	"dbt",		"dbt",
	"dbvc", 	"dbvc",
	"dbvs",		"dbvs",
	".end",		"end",
	".text",	"text",
	".data",	"data",
	".bss",		"bss",
	".byte",	"byte",
	".word",	"short",
	".long",	"long",
	".asciz",	"asciz",
	".single",	"float",
	".double",	"double",
	".skip",	"space",
	".lcomm",	"lcomm",
	".comm",	"comm",
	".globl",	"global",
	".align",	"lalign",
	".even",	"even",
	"!",		"!" };

int make_op_hash()
{	int i, bn;
	struct optrans *newop;

	for (i=0; i<m_buckets; i++) opbucks[i]=nullop;

	for (i=0; oppairs[i][0]!='!'; i+=2) {
		bn=hash_macro(oppairs[i+1]);
		newop=(struct optrans *) malloc(sizeof(struct optrans));
		newop->opwarn=false;
		newop->opnext=opbucks[bn];
		opbucks[bn]=newop;
		if (oppairs[i+1][0]=='?') {
			strcpy(newop->opfrom,&oppairs[i+1][1]);
			newop->opwarn=true; }
		else strcpy(newop->opfrom,oppairs[i+1]);
		strcpy(newop->opto,oppairs[i]); } }

replace_mapping(from, to)
char *from;
{	int i, bn;
	struct optrans *op;

	bn=hash_macro(from);
	for (op=opbucks[bn]; op!=nullop; op=op->opnext) {
		if (strcmp(from,op->opfrom)==0) {
		  strcpy(op->opto,to);
		  return; } }

	printf("cvt: Internal error - unable to replace with %s->%s\n",
	       from,to); }

cvtop(from,to)
char *from, *to;
{	int bn, i;
	struct optrans *op;
	char name[64];

	strcpy(name,from);
	for (i=0; name[i]!='\0'; i++)
	  name[i]=down_case(name[i]);
	bn=hash_macro(name);
	for (op=opbucks[bn]; op!=nullop; op=op->opnext) {
		if (strcmp(name,op->opfrom)==0) {
			strcpy(to,op->opto);
			if (op->opwarn) {
			  fprintf(stderr,
				  "cvt: Unsure of translation of the opcode %s\n",from);
			  op->opwarn=false; }
			return(true); } }
	to[0]='\0';
	return(false); }
