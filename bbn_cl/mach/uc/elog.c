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
#include "primitive.h"
#include "flonum.h"
#include "zones.h"
#include "bignum.h"
#include "stringprim.h"
#include "character.h"
#include <math.h>
#ifdef butterfly
#include <elog.h>
#endif

static Boolean elog_setup_p = false;

/*
  (prim-elog-init <filename>)
*/

Define_Primitive(prim_elog_init, 1, "PRIM-ELOG-INIT")
{ 
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);

#ifdef butterfly
  elog_init(Scheme_String_To_C_String(Arg1));
#endif
  return NIL;
}

/*
  (prim-elog-setup <filename> <process-tag> <buffer-size>)
*/

Define_Primitive(prim_elog_setup, 3, "PRIM-ELOG-SETUP")
{
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);

#ifdef butterfly
  elog_setup(Scheme_String_To_C_String(Arg1), Get_Integer(Arg2), Get_Integer(Arg3));
  elog_setup_p = true;
#endif
  return NIL;
}

/*
  (prim-elog-define <event-code> <event-name> <data-format-string>)
*/

Define_Primitive(prim_elog_define, 3, "PRIM-ELOG-DEFINE")
{
  Primitive_3_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_CHARACTER_STRING);
  Arg_3_Type(TC_CHARACTER_STRING);

#ifdef butterfly
  elog_define(Get_Integer(Arg1),
	      Scheme_String_To_C_String(Arg2),
	      Scheme_String_To_C_String(Arg3));
#endif
  return NIL;
}

/*
  (prim-elog-log <event-code> <datum>)

  <datum> may be either fixnum or float. If float, is coerced
  to single-precision.
*/

Define_Primitive(prim_elog_log, -1, "PRIM-ELOG-LOG")
{
  long tc;
  long event;
  long c_datum;
  float float_datum;
  Pointer datum;
  Primitive_Variable_Args();

#ifdef butterfly
  if (!elog_setup_p) return NIL;
  if (Number_Of_Args < 1 ||
      Number_Of_Args > 2)
    Primitive_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
  CHECK_ARG(1, FIXNUM_P);
  event = Get_Integer(Primitive_Variable_Arg(1));
  if (Number_Of_Args < 2)
    datum = Make_Unsigned_Fixnum(0);
  else
    datum = Primitive_Variable_Arg(2);
  tc = Type_Code(datum);
  if (tc == TC_FIXNUM)
    {
      c_datum = Get_Integer(datum);
    }
  else
    {
      if (tc != TC_BIG_FLONUM)
	error_wrong_type_arg(2);
      float_datum = Get_Float(datum);
      c_datum = (long) float_datum;
    }
  ELOG_LOG(event, c_datum);
#endif
  return NIL;
}

Define_Primitive(prim_elog_output, 0, "PRIM-ELOG-OUTPUT")
{
  Primitive_0_Args();

#ifdef butterfly
  elog_output();
  elog_setup_p = false;
#endif
  return NIL;
}

