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

#include "scheme.h"
#include "character.h"
#include "primitive.h"
#include "transact.h"		/* Transaction Manager/Scheme protocol defs */
#include <signal.h>

extern int Debug_Flags[];


/* (WAIT INTERRUPT-MASK)
   Suspends the single-processor interpreter entirely.  This is only
   useful when there is no work anywhere, so the Scheme world is
   assumed to be needed only in case of external interrupts; other
   universes running on the same machine will get a larger percent
   of the available computrons.
*/
Define_Primitive(Suspend, 1, "WAIT")
{ long NewIntEnb, none_pending, mask;

  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  NewIntEnb = Get_Integer(Arg1);
  mask = sigblock(~0);
  none_pending = ((IntCode & NewIntEnb) == 0) ;
  if (none_pending) sigpause(mask);
  return NIL; }


/* Add one to the FIXNUM to be found at the pointer location of the argument;
   this is not checked for type except to ensure that it is a pointer type,
   but the CDR must be a string and the CAR must be the index.

   The major point for this primitive is speedy extraction of a character
   from a buffer, plus the return of TRUTH for the FIRST call which discovers
   the buffer empty and NIL for all others who discover the buffer empty.
   This facilitates the multiprocessor use of these as character streams.
   Note that fill vectors ala Common Lisp with atomic removal/insertion
   operations would be an improvement over this scheme.
*/

Define_Primitive(Get_Character_From_Buffer, 1, "GET-CHARACTER-FROM-BUFFER!")
{ Pointer Index, Buffer;
  long Result, Length;
  char *String_Ptr;
  short *The_Index;

  Primitive_1_Arg();
  if (Arg1==NIL) return TRUTH;
  if (Type_Code(Arg1)==TC_FUTURE) return NIL;

  Arg_1_Type(TC_LIST);
  Index = Vector_Ref(Arg1,CONS_CAR);
  if (Type_Code(Index)!=TC_FIXNUM)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Buffer = Vector_Ref(Arg1,CONS_CDR);
  if (Type_Code(Buffer)!=TC_CHARACTER_STRING)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  The_Index=(short *) Get_Pointer(Arg1);
  Result=Atomic_add(&The_Index[1],1);

  Length = Get_Integer(Fast_Vector_Ref(Buffer, STRING_LENGTH));
  String_Ptr = (char *) Nth_Vector_Loc(Buffer, STRING_CHARS);
  if(Result < Length) return Make_Non_Pointer(TC_FIXNUM, String_Ptr[Result]);
  if(Result == Length) return TRUTH;
  return NIL; }



     /* The external message is left under a rock (in a microcode slot),
      and must be translated into Scheme format and into the heap.  The
      object thus constructed is returned.
      
      The format of the return value is a HUNK3:
      _________________________________________________
      |    TYPE     |    STREAM     | IMMEDIATE DATA  |
      |    (fix)    |     (fix)     | (string or char)|
      |_____________|_______________|_________________|

      The "immediate data" is either a string (input buffer of characters)
      or a character representing an external interrupt vector.  Note that
      stream numbers can be used as external descriptors for any external
      device or process capable of interrupting through the Transaction
      Manager; this will make it possible to use this mechanism for
      external event-driven processing of all kinds.
*/ 

Define_Primitive(Return_External_Msg, 0, "RETURN-EXTERNAL-MESSAGE")
{
  int stream, count, result, i, pointer_count;
  int mask;
  char *p, tag, *cp;
  Pointer value;

  mask = sigblock(sigmask(SIGUSR1));
  if ((Int_State & INT_STATE_CHAR) != 0)
  {
    /* Interrupts have priority. */

    if (GC_Check(3))
    {
      sigsetmask(mask);
      Primitive_GC(3);
    }

    *Free++ = Make_Unsigned_Fixnum(PROC_INTRUPT);
    *Free++ = NIL;
    *Free++ = Make_Unsigned_Fixnum(Int_Char);
    value = Make_Pointer(TC_HUNK3, Free-3);

    Int_State &= (~INT_STATE_CHAR);
    Int_Char = '\0';
  }
  else
  {
    /* This assumes that
       (Int_State & INT_STATE_INPUT) != 0)
     */
    count = strlen(Int_String);
    pointer_count = 2 + ((sizeof(Pointer) + count * sizeof(char) - 1) /
			 sizeof(Pointer));

    if (GC_Check(pointer_count + 3))
    {
      sigsetmask(mask);
      Primitive_GC(pointer_count + 3);
    }

    result = Make_Pointer(TC_CHARACTER_STRING, Free);
    Free[STRING_HEADER] = 
      Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, pointer_count - 1);
    Free[STRING_LENGTH] = Make_Unsigned_Fixnum(count);
    Free += pointer_count;

    cp = Int_String;
    p = (char *) Nth_Vector_Loc(result, STRING_CHARS);
    for (i = 0; i < count; i++)
      *p++ = *cp++;

    *Free++ = Make_Unsigned_Fixnum(BUFFER_AVAIL);
    *Free++ = Make_Unsigned_Fixnum(Int_Stream);
    *Free++ = result;

    value = Make_Pointer(TC_HUNK3, Free-3);

    Int_State &= (~INT_STATE_INPUT);
  }
  if (Int_State == 0)
    /* Safe to turn interrupt back on */
    IntCode &= ~INT_Character;
  sigsetmask(mask);
  return (value);
}

/*
  Common Lisp (and other C level) I/O system interfaces
*/

#define CHANNEL_NUMBER	0
#define CHANNEL_NAME	1
#define CHANNEL_BUFFER	2

#define INFO_NUMBER	1
#define INFO_ALIST	2

#define Get_Current_Future() \
	Fast_Vector_Ref(Get_Fixed_Obj_Slot(Future_Vector), \
			Who_Am_I + 1)

Pointer
BFIO_Get_Info()
{ Pointer Info;

  Info = Get_Fixed_Obj_Slot(BFIO_Info);
  if (Type_Code(Info) != TC_VECTOR) {
    Info = Make_Pointer(TC_VECTOR, Free);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 2);
    *Free++ = Make_Non_Pointer(TC_FIXNUM, 1);
    *Free++ = NIL;
    Set_Fixed_Obj_Slot(BFIO_Info, Info); }

  return Info; }

Pointer
BFIO_Next_Hash_Number(Info)
Pointer Info;
{
  return atomadd32(Nth_Vector_Loc(Info, INFO_NUMBER), 1); }

Pointer The_Channel;		/* Side effected by Get_Stream_Number */

Pointer
Get_Stream_Number(Future)
Pointer Future;
{ Pointer Current_Future, Private_Value, Channel, Number, Name;
  char *C_Name;

  Name = NIL;
  if (Future == NIL)
    Current_Future = Get_Current_Future();
  else
    Current_Future = Future;
  if (Type_Code(Current_Future) != TC_FUTURE) return NIL;
  Private_Value = Fast_Vector_Ref(Current_Future, FUTURE_PRIVATE);
  if (Type_Code(Private_Value) != TC_VECTOR) {
    if (Type_Code(Private_Value) == TC_CHARACTER_STRING)
      Name = Private_Value;
    Private_Value = Make_Pointer(TC_VECTOR, Free);
    Fast_Vector_Set(Current_Future, FUTURE_PRIVATE, Private_Value);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, 1);
    *Free++ = NIL; }
  Channel = User_Vector_Ref(Private_Value, 0);
  if (Type_Code(Channel) != TC_HUNK3) {
    Channel = Make_Pointer(TC_HUNK3, Free);
    User_Vector_Set(Private_Value, 0, Channel);
    *Free++ = NIL;
    *Free++ = Name;
    *Free++ = NIL; }
  Number = Fast_Vector_Ref(Channel, CHANNEL_NUMBER);
  if (Type_Code(Number) != TC_FIXNUM) {
    Number = BFIO_Next_Hash_Number(BFIO_Get_Info());
    Fast_Vector_Set(Channel, CHANNEL_NUMBER, Number);
    Name = Fast_Vector_Ref(Channel, CHANNEL_NAME);
    C_Name = Name == NIL ? "Unnamed Future" : Scheme_String_To_C_String(Name);
/*
    Send_Out_Packet(CREATE_STREAM, Get_Integer(Number),
		    true, C_Name, strlen(C_Name));
*/
  }
  The_Channel = Channel;
  return Number; }

Make_Input_Future(immediate, Channel, Stream)
int immediate;
Pointer Channel, Stream;
{ Pointer Make_A_Future(), Future;

  Future = Make_A_Future(NIL, NIL, NIL);
  Fast_Vector_Set(The_Channel, CHANNEL_BUFFER, Future);

/*
  Send_Out_Packet(immediate ? REQ_INPUT : REQ_BUF_INPUT,
		  Get_Integer(Stream), false, NULL, 0); 
*/
}

Pointer
BFIO_Task_Hash(Future)
Pointer Future;
{ Pointer Info, AList, Entry, Number, NewList;

  Info = BFIO_Get_Info();
  
  for (AList = Fast_Vector_Ref(Info, INFO_ALIST);
       Type_Code(AList) == TC_LIST;
       AList = Fast_Vector_Ref(AList, CONS_CDR)) {
    Entry = Fast_Vector_Ref(AList, CONS_CAR);
    if (Type_Code(Entry) == TC_LIST &&
	Future == Fast_Vector_Ref(Entry, CONS_CAR))
      return Fast_Vector_Ref(Entry, CONS_CDR); }
  
  Number = Get_Stream_Number(Future);
  Entry = Make_Pointer(TC_LIST, Free);
  *Free++ = Future;
  *Free++ = Number;
  NewList = Make_Pointer(TC_LIST, Free);
  *Free++ = Entry;
  *Free++ = Fast_Vector_Ref(Info, INFO_ALIST);
  Fast_Vector_Set(Info, INFO_ALIST, NewList);
  return Number; }

Pointer
BFIO_Task_Unhash(Number)
Pointer Number;
{ Pointer Info, AList, Entry;

  Info = Get_Fixed_Obj_Slot(BFIO_Info);

  for (AList = Fast_Vector_Ref(Info, INFO_ALIST);
       Type_Code(AList) == TC_LIST;
       AList = Fast_Vector_Ref(AList, CONS_CDR)) {
    Entry = Fast_Vector_Ref(AList, CONS_CAR);
    if (Type_Code(Entry) == TC_LIST &&
	Number == Fast_Vector_Ref(Entry, CONS_CDR))
      return Fast_Vector_Ref(Entry, CONS_CAR); }

  return NIL; }


/*
  Use BFIO_Read_String to read a string, but be prepared to have your
  primitive start from the beginning since it works by touching a future!
*/

long
BFIO_Read_String(channel, immediate, gotten, maxlen)
long channel;
char *gotten;
int maxlen, immediate;
{ Pointer Stream, Buffer, String;
  int posn, len, nelemt, stream;
  char *string;
  
  if (channel >= 0)
    return (-1);
  
  Stream = Get_Stream_Number(NIL);
  if (Stream == NIL) {
    *gotten = OS_tty_read_char();
    return 1; }
  stream = Get_Integer(Stream);
  
  Buffer = Fast_Vector_Ref(The_Channel, CHANNEL_BUFFER);
  if (Buffer == NIL)
    {	/* We must make the future and then touch it! */
      Make_Input_Future(immediate, The_Channel, Make_Non_Pointer(TC_FIXNUM, stream));
      BFIO_Task_Hash(Get_Current_Future());
      Buffer = Fast_Vector_Ref(The_Channel, CHANNEL_BUFFER);
      Touch_In_Primitive(Buffer, Buffer); 
    }
  else if (Type_Code(Buffer) == TC_FUTURE &&
	   !Future_Has_Value(Buffer)) 
    {
/*
      Send_Out_Packet(immediate ? REQ_INPUT : REQ_BUF_INPUT,
		      stream, false, NULL, 0);
*/
      Touch_In_Primitive(Buffer, Buffer); 
    }
  else if (Type_Code(Buffer) == TC_LIST ||
	   Type_Code(Buffer) == TC_FUTURE) 
    {
      Touch_In_Primitive(Buffer, Buffer);
      posn = Get_Integer(Fast_Vector_Ref(Buffer, CONS_CAR));
      String = Fast_Vector_Ref(Buffer, CONS_CDR);
      string = (char *) Nth_Vector_Loc(String, STRING_CHARS);
      len = Get_Integer(Fast_Vector_Ref(String, STRING_LENGTH));
      for (nelemt = 0; nelemt < maxlen && posn < len; nelemt++, posn++)
	gotten[nelemt] = string[posn];
      if (posn >= len && nelemt == 0) 
	{
	  Make_Input_Future(immediate, The_Channel, Make_Non_Pointer(TC_FIXNUM, stream));
	  Buffer = Fast_Vector_Ref(The_Channel, CHANNEL_BUFFER);
	  ack_ext_msg(); /* If this message is from the input server, acknowledge it to avoid races */
	  Touch_In_Primitive(Buffer, Buffer); 
	}
      else 
	{
	  Fast_Vector_Set(Buffer, CONS_CAR, Make_Non_Pointer(TC_FIXNUM, posn));
	  return nelemt; 
	} 
    }
}

/*
  Primitives for use in bf-io.scm
*/

Define_Primitive(Prim_Get_Next_Hash_Number, 0, "BFIO-NEXT-HASH-NUMBER")
{ Pointer Info;
  Primitive_0_Args();

  return BFIO_Next_Hash_Number(BFIO_Get_Info()); }

Define_Primitive(Prim_Get_Stream_Number, 0, "BFIO-GET-STREAM-NUMBER")
{ Primitive_0_Args();

  return Get_Stream_Number(NIL); }

Define_Primitive(Prim_BFIO_Task_Hash, 1, "BFIO-TASK-HASH")
{ Primitive_1_Arg();

  return BFIO_Task_Hash(Arg1); }

Define_Primitive(Prim_BFIO_Task_Unhash, 1, "BFIO-TASK-UNHASH")
{ Primitive_1_Arg();

  return BFIO_Task_Unhash(Arg1); }

Define_Primitive(Prim_BFIO_Read_Char, 2, "BFIO-READ-CHAR")
{ char buffer[1];
  long nelemt;
  Primitive_2_Args();
  
  if (Arg1 != NIL)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

  nelemt = BFIO_Read_String(Arg1 == NIL ? (-1) : Get_Integer(Arg1),
			    Arg2 != NIL,
			    buffer, 1);
  if (nelemt != 1)
    return NIL;
  else
    {
      if (Debug_Flags[10])
	printf("bfio-read-char: %c\n", buffer[0]);
      return Make_Non_Pointer(TC_CHARACTER, buffer[0]);
    }
}

Define_Primitive(Prim_BFIO_Read_String, 3, "BFIO-READ-STRING")
{ long wanted, nelemt;
  char buffer[256];
  Primitive_3_Args();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(wanted, Arg1, 1, 255, ERR_ARG_1_BAD_RANGE);
  if (Arg2 != NIL) 
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  Primitive_GC_If_Needed(100);	/* Reasonable guess */

  nelemt = BFIO_Read_String(Arg2 == NIL ? (-1) : Get_Integer(Arg2),
			    Arg3 != NIL,
			    buffer, wanted);
  if (nelemt < 1)
    return NIL;

  buffer[nelemt] = '\0';
  if (Debug_Flags[10])
    printf("bfio-read-string: %s\n", buffer);
  return C_String_To_Scheme_String(buffer);
}

Define_Primitive(Prim_BFIO_Kick_Input_Server, 0, "BFIO-KICK-INPUT-SERVER")
{
  Primitive_0_Args();

  kick_input_server();
  return NIL;
}
