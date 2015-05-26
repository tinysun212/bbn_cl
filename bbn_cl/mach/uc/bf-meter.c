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
  This code handles the metering packet stuff.
*/

#include "scheme.h"
#include "primitive.h"
#include "transact.h"

Define_Primitive(Prim_Send_Meter, 3, "SEND-METERING")
{ long streamid;
  Primitive_3_Args();

  Arg_1_Type(TC_FIXNUM);
  Sign_Extend(Arg1, streamid);
  Send_Metering_Packet(streamid, Arg2, Arg3);
  return NIL; }

Send_Metering_Packet(streamid, Arg2, Arg3)
long streamid;
Pointer Arg2, Arg3;
{ Pointer MString=NIL, MVector=NIL, MFixnum1=NIL, MFixnum2=NIL;
  long time, value;
  int packlen, infobyte, packpos, sent, i, stringlen, arraylen;
  char *temp_string;
  char packet[512];

  if (Arg2 != NIL)
    switch (Type_Code(Arg2)) {
    case TC_FIXNUM:
      MFixnum1 = Arg2;   
      break;
    case TC_VECTOR: 
      MVector = Arg2;
      break;
    case TC_CHARACTER_STRING:
      MString = Arg2;
      break;
    default:
      Primitive_Error(ERR_ARG_2_WRONG_TYPE); }

  if (Arg3!=NIL)
    switch (Type_Code(Arg3)) {
    case TC_FIXNUM:
      if (MFixnum1 == NIL)
	MFixnum1 = Arg3;
      else
	MFixnum2 = Arg3;
      break;
    case TC_VECTOR:
      if (MVector != NIL)
	Primitive_Error(ERR_ARG_3_WRONG_TYPE);
      MVector = Arg3;
      break;
    case TC_CHARACTER_STRING:
      if (MString != NIL)
	Primitive_Error(ERR_ARG_3_WRONG_TYPE);
      MString = Arg3;
      break;
    default:
      Primitive_Error(ERR_ARG_3_WRONG_TYPE); }

  packlen=15;
  infobyte=0;
  if (MFixnum1 != NIL) 
    { packlen+=3; 
      infobyte++; }
  if (MFixnum2 != NIL)
    { packlen+=3; 
      infobyte++; }
  if (MVector != NIL) {
    arraylen = Vector_Length(MVector);
    if (arraylen > 126)
      arraylen -= 126;
    packlen += 3 * arraylen;
    infobyte += arraylen; }
  if (MString != NIL) {
    temp_string = Scheme_String_To_C_String(MString);
    stringlen = strlen(temp_string);
    if (stringlen > 128)
      stringlen = 128;
    packlen += stringlen + 2;
    infobyte |= 0x80; }

  packlen -= 4;
  packet[0] = 0;
  packet[1] = 0;
  packet[2] = packlen >> 8;
  packet[3] = packlen;
  packet[4] = METER_INFO;

  packlen -= 3;
  packet[5] = packlen >> 8;
  packet[6] = packlen;
  packet[7] = streamid >> 8;
  packet[8] = streamid;

  time=OS_real_time_clock();
  packet[9] = time >> 24;
  packet[10] = time >> 16;
  packet[11] = time >> 8;
  packet[12] = time;
  packet[13] = infobyte;
  packet[14] = Who_Am_I;

  packpos=15;
  if (MFixnum1 != NIL) {
    packet[packpos++] = MFixnum1 >> 16;
    packet[packpos++] = MFixnum1 >> 8;
    packet[packpos++] = MFixnum1; }
  if (MFixnum2 != NIL) {
    packet[packpos++] = MFixnum2 >> 16;
    packet[packpos++] = MFixnum2 >> 8;
    packet[packpos++] = MFixnum2; }
  if (MVector != NIL)
    for (i = 0; i < arraylen; i++) {
      value = User_Vector_Ref(MVector, i);
      packet[packpos++] = value >> 16;
      packet[packpos++] = value >> 8;
      packet[packpos++] = value; }
  if (MString != NIL) {
    packet[packpos++] = stringlen >> 8;
    packet[packpos++] = stringlen;
    for (i = 0; i < stringlen; i++) {
      packet[packpos++] = temp_string[i]; } }
  
  while(atomior(&SHARED_DATA->Lisp_Output_Lock, 0x8000) != 0);
  write(SHARED_DATA->Lisp_Output_Pipe[WRITE_TO_PIPE],
	packet+4, packpos-4);
  SHARED_DATA->Lisp_Output_Lock = 0; }
