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
  Structure containing data about the memory bus temperature.
*/

struct temperature {
  int t_sequence;
  long t_last_update;
  short int deviations[512]; } *temperature_seg;

OID temperature_OID;

Map_In_Temperature(createp)
int createp;
{ catch
    temperature_OID = Find_Value("Temperature_Info", NTYPE_OBJ);
    temperature_seg = (struct temperature *) Map_Obj(temperature_OID, 0, RW_rw_);
    temperature_seg->t_sequence = 0;
  onthrow
    when (true) {
      temperature_OID = (OID) NULL; }
  endcatch

  if (createp && temperature_OID == (OID) NULL) {
      temperature_OID = (OID)
	Make_Obj(' ', -1, sizeof(struct temperature), 0);
      Disown_Obj(temperature_OID, NULL);
      Name_Bind("Temperature_Info", temperature_OID, NTYPE_OBJ);
      Map_In_Temperature(false); } }


  
