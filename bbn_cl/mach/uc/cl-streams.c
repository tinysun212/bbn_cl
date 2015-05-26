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

#include "scheme.h
#include "primitive.h"
#include "stringprim.h"
#include "cl-streams.h"
#include "character.h"

/*
  Forward-ref decls so that we may keep things 
  in a reasonable order.
*/

Pointer move_buffer();
Pointer cl_write_char();
Pointer cl_force_output();
Pointer nyi_error();
Pointer input_error1();
Pointer output_error1();
Pointer cl_write_string();
Pointer cl_finish_output();
Pointer cl_clear_output();
Pointer cl_write_byte();
Pointer cl_file_position();
Pointer cl_close();
Pointer cl_read_char();
Pointer cl_unread_char();
Pointer cl_listen();
Pointer cl_clear_input();
Pointer cl_read_byte();
Pointer cl_isa_tty_p();
Pointer cl_read_string();
Pointer cl_misc();
Pointer cl_charpos();
long c_get_file_position();

/* Externs */

extern char OS_tty_read_char();
extern Boolean OS_read_char_ready_p();
extern char *cl_ivector_to_c_string();

#ifdef butterfly
extern long BFIO_Read_String();
#endif

#undef max
long max(x, y)
long x, y;
{
  if (x > y)
    return x;
  else return y;
}

#undef min
private
long min(x, y)
long x, y;
{
  if (x < y)
    return x;
  else return y;
}

/* Making streams, handles, and other things */

Pointer make_bare_vector_32b(size)
{
  return Make_Non_Pointer(TC_VECTOR_32B, make_non_marked_vector(size, 0));
}

Pointer make_bare_stream()
{
  Pointer stream;

  stream = Make_Non_Pointer(TC_CL_STREAM, make_vector(ST_SIZE, NIL));
  st_nm(stream) = make_bare_vector_32b(ST_NM_SIZE);
  return stream;
}

Pointer make_bare_vector(size)
long size;
{
  return make_vector(size, NIL);
}
  
Pointer make_bare_string(nchars)
long nchars;
{
  long nptrs;
  Pointer result;
  nptrs = 2 + ((nchars + 1 + sizeof(Pointer) - 1) / sizeof(Pointer)); /* extra 1 for null char */
  Primitive_GC_If_Needed(nptrs);
  result = Make_Non_Pointer(TC_CHARACTER_STRING, Free);
  Free[STRING_HEADER] = 
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, nptrs - 1);
  Free[STRING_LENGTH] = ((Pointer) nchars);
  Free += nptrs;
  return result;
}

/*
  Basic primitives for accessing NM vectors consisting of objects represented
  as 32-bit unsigned integers.
  Used mainly for TC_VECTOR_32B, but can be used with any type as
  does not check type or header.
*/

Define_Primitive(prim_system_vector_32b_ref, 2, "SYSTEM-VECTOR-32B-REF")
{
  
  Primitive_2_Args();

  Arg_1_Type(TC_VECTOR_32B);
  Arg_2_Type(TC_FIXNUM);
  return C_Integer_To_Scheme_Integer((long)User_Vector_Ref(Arg1, Get_Integer(Arg2)));
}

Define_Primitive(prim_system_vector_32b_set, 3, "SYSTEM-VECTOR-32B-SET!")
{
  long value, old_value;
  Primitive_3_Args();

  Arg_1_Type(TC_VECTOR_32B);
  Arg_2_Type(TC_FIXNUM);
  if ((Type_Code(Arg3) != TC_FIXNUM) &&
      (Type_Code(Arg3) != TC_BIG_FIXNUM))
    Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  old_value = (long)User_Vector_Ref(Arg1, Get_Integer(Arg2));
  Scheme_Integer_To_C_Integer(Arg3, &value);
  User_Vector_Set(Arg1, Get_Integer(Arg2), value);
  return C_Integer_To_Scheme_Integer(old_value);
}

Define_Primitive(prim_system_vector_32b_length,1, "SYSTEM-VECTOR-32B-LENGTH")
{
  Primitive_1_Args();

  return Make_Unsigned_Fixnum(Datum(Fast_Vector_Ref(Arg1, 0)));
}

/*
 ****************
 * FILE STREAMS *
 ****************
*/

Pointer file_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  long index;
  Pointer status, handle, nm, result;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  index = file_st_h_nm_buf_index(nm);
  if (index >= file_st_h_nm_buf_end(nm))
    {
      status = move_buffer(stream,
			   file_st_h_nm_buf_end(nm) + file_st_h_nm_pos_base(nm),
			   NIL);
      if (status == Sym_colon_eof)
	{
	  if (eof_error_p != NIL)
	    eof_error(stream);
	  else
	    return eof_value;
	}
      else if (status != Sym_colon_ok)
	io_error(status);
      else
	return file_read_char(stream, eof_error_p, eof_value, recursive_p);
    }
  else
    {
      result = c_char_to_scheme_char((unsigned char)(Scheme_String_To_C_String(file_st_h_buf(handle))[index]));
      file_st_h_nm_buf_index(nm) = index + 1;
      return result;
    }
}

Pointer file_unread_char(character, stream)
Pointer character, stream;
{
  c_set_file_position(stream, c_get_file_position(stream) - 1);
  return NIL;
}

Pointer file_listen(stream)
Pointer stream;
{
  Pointer c;

  c = cl_read_char(stream, NIL, NIL, NIL);
  cl_unread_char(c, stream);
  if (c == NIL)
    return NIL;
  else
    return TRUTH;
}

Pointer file_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  long index;
  Pointer status, handle, nm, result;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  index = file_st_h_nm_buf_index(nm);
  if (index >= file_st_h_nm_buf_end(nm))
    {
      status = move_buffer(stream,
			   file_st_h_nm_buf_end(nm) + file_st_h_nm_pos_base(nm),
			   NIL);
      if (status == Sym_colon_eof)
	{
	  if (eof_error_p != NIL)
	    eof_error(stream);
	  else
	    return eof_value;
	}
      else if (status != Sym_colon_ok)
	io_error(status);
      else
	return file_read_byte(stream, eof_error_p, eof_value, recursive_p);
    }
  else
    {
      result = cl_ivector_ref(file_st_h_buf(handle), index);
      file_st_h_nm_buf_index(nm) = index + 1;
      return result;
    }
}

/*
  For now, we call the generic write-char to pick up
   column processing, etc. This can probably be
   much improved by specializing the logic for the
   file methods.
*/

Pointer file_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  char *cstring;
  long cstart, cend, i;

  cstring = Scheme_String_To_C_String(string);
  cstart = Get_Integer(start);
  cend = Get_Integer(end);
  cstring += cstart;
  for (i = cstart; i < cend; i++)
    cl_write_char(c_char_to_scheme_char(*cstring++), stream, NIL);
  return string;
}

Pointer file_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  long index; 
  Pointer status, handle, nm;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  index = file_st_h_nm_buf_index(nm);
  if (index >= file_st_h_nm_buf_end(nm))
    {
      status = move_buffer(stream,
			   file_st_h_nm_buf_end(nm) + file_st_h_nm_pos_base(nm),
			   NIL);
      if (status != Sym_colon_ok)
	io_error(status);
      else
	return file_write_char(character, stream, flush_p);
    }
  else
    {
      Scheme_String_To_C_String(file_st_h_buf(handle))[index] = (char)Datum(character);
      file_st_h_nm_buf_index(nm) = index + 1;
      return character;
    }
}

Pointer file_write_byte(integer, stream)
Pointer integer, stream;
{
  long index; 
  Pointer status, handle, nm;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  index = file_st_h_nm_buf_index(nm);
  if (index >= file_st_h_nm_buf_end(nm))
    {
      status = move_buffer(stream,
			   file_st_h_nm_buf_end(nm) + file_st_h_nm_pos_base(nm),
			   NIL);
      if (status != Sym_colon_ok)
	io_error(status);
      else
	return file_write_byte(integer, stream);
    }
  else
    {
      long c_integer;

      Scheme_Integer_To_C_Integer(integer, &c_integer);
      cl_ivector_set(file_st_h_buf(handle), index, c_integer);
      file_st_h_nm_buf_index(nm) = index + 1;
      return integer;
    }
}

Pointer file_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  char *char_set, *buf;
  Pointer status, handle, nm, c;
  long index, end, i;
  Pointer result, char_list, cell;
  char *p;
  long len, j, k;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  char_set = Scheme_String_To_C_String(scheme_char_set);
  if (file_st_h_read_string_acc(handle) == NIL)
    {
      Primitive_GC_If_Needed(2 + ((FILE_STREAM_BUFFER_SIZE + 1 + sizeof(Pointer) - 1) / sizeof(Pointer)));
      buf = Scheme_String_To_C_String(file_st_h_buf(handle));
      /* First try scan within buffer.
	 If first scan fails, slide buffer
	 to start of token, thereby giving
	 us highest probability that we can read a string
	 without creating a temp list */
      for (k = 1; k <= 2; k++)
	{
	  index = file_st_h_nm_buf_index(nm);
	  end = file_st_h_nm_buf_end(nm);
	  for (i = index; i < end; i++)
	    if (char_set[(unsigned char)buf[i]] != '\0')
	      {
		file_st_h_nm_buf_index(nm) = i;
		len = i - index;
		*eof_term_p = NIL;
		if (len == 0)
		  *chars_read_p = NIL;
		else
		  *chars_read_p = TRUTH;
		if (discard_p == NIL)
		  {
		    result = make_bare_string(len);
		    p = Scheme_String_To_C_String(result);
		    for (j = index; j < i; j++)
		      *p++ = buf[j];
		    *p = '\0';
		  }
		else
		  result = NIL;
		return result;
	      }
	  move_buffer(stream, file_st_h_nm_pos_base(nm) + index, NIL);
	}
    }
  /* If within-buffer fails, or we have restarted from a GC, scan via read-char,
     accumulating into stream-local char list */
  if (file_st_h_read_string_acc(handle) == NIL)
    {
      file_st_h_nm_read_string_len(nm) = 0;
      file_st_h_read_string_acc(handle) = TRUTH;      /* Mark it in use */
    }
  while (1)
    {
      Primitive_GC_If_Needed(2);
      c = file_read_char(stream, NIL, NIL, NIL);
      if ((c == NIL) ||
	   (char_set[Datum(c)] != '\0'))
	{
	  if (c != NIL) file_unread_char(c, stream);
	  len = file_st_h_nm_read_string_len(nm);
	  if (len == 0)
	    *chars_read_p = NIL;
	  else
	    *chars_read_p = TRUTH;
	  if (discard_p == NIL)
	    {
	      result = make_bare_string(len);
	      p = Scheme_String_To_C_String(result) + len;
	      *p = '\0';
	      char_list = file_st_h_read_string_acc(handle);
	      while (char_list != TRUTH)
		{
		  *--p = (char)Datum(Fast_Vector_Ref(char_list,CONS_CAR));
		  char_list = Fast_Vector_Ref(char_list,CONS_CDR);
		}
	    }
	  else
	    result = NIL;
	  file_st_h_read_string_acc(handle) = NIL;
	  file_st_h_nm_read_string_len(nm) = 0;
	  if (c == NIL) *eof_term_p == NIL;
	  else *eof_term_p == TRUTH;
	  return result;
	}
      else
	{
	  cell = Make_Non_Pointer(TC_LIST, Free);
	  Fast_Vector_Set(cell, CONS_CAR, c);
	  Fast_Vector_Set(cell, CONS_CDR, file_st_h_read_string_acc(handle));
	  Free += 2;
	  file_st_h_read_string_acc(handle) = cell;
	  file_st_h_nm_read_string_len(nm) += 1;
	}
    }
}

long c_get_file_position(stream)
Pointer stream;
{
  Pointer handle, nm;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  return file_st_h_nm_pos_base(nm) + file_st_h_nm_buf_index(nm);
}

/*
  pos_in = POS_END = -1 means position to end
*/

c_set_file_position(stream, pos)
Pointer stream;
long pos;
{
  Pointer handle, nm;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  if (pos == POS_END)
    move_buffer(stream, POS_END, NIL);
  else
    {
      long pb  = file_st_h_nm_pos_base(nm);
      long e   = file_st_h_nm_buf_end(nm);

      if ((pb <= pos) && (pos < (pb + e)))
	{
	  file_st_h_nm_buf_index(nm) = pos - pb;
	}
      else
	move_buffer(stream, pos, NIL);
    }
}

Pointer c_set_file_position_base(stream, pos)
Pointer stream;
long pos;
{
  long os_status, os_errno;
  Pointer status;
  long fd;

  if (Binary_Stream_P(stream))
    pos = (pos * BPE(stream)) / OS_BPE;
  fd = file_st_h_nm_fd(file_st_h_nm(st_handle(stream)));
  os_status = OS_cl_file_set_pos(fd, pos, &os_errno);
  if (os_status == -1)
    return Make_Non_Pointer(TC_IO_ERROR_CODE, os_errno);
  else 
    return Sym_colon_ok;
}

long c_file_length(stream)
Pointer stream;
{
  long os_len;

  os_len = OS_file_length_fd(file_st_h_nm_fd(file_st_h_nm(st_handle(stream))));
  if (Binary_Stream_P(stream))
    return (os_len * OS_BPE) / BPE(stream);
  else return os_len;
}

Pointer c_file_output_buf(stream, nbytes)
Pointer stream;
long nbytes;
{
  Pointer handle, nm;
  char *buffer;
  long n_written, os_errno, fd, os_nbytes;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  if (Binary_Stream_P(stream))
    {
      buffer = cl_ivector_to_c_string(file_st_h_buf(handle));
      os_nbytes = (nbytes * BPE(stream) + (OS_BPE - 1)) / OS_BPE;
    }
  else
    {
      buffer = Scheme_String_To_C_String(file_st_h_buf(handle));
      os_nbytes = nbytes;
    }
  fd = file_st_h_nm_fd(nm);
  n_written = OS_cl_file_write_chars(fd, buffer, os_nbytes, &os_errno); 
  if (n_written == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,os_errno);
  else return Sym_colon_ok;
}

Pointer c_file_input_buf(stream, nbytes, bytes_read)
Pointer stream;
long nbytes, *bytes_read;
{
  Pointer handle, nm;
  char *buffer;
  long os_errno, fd, os_nbytes, os_bytes_read, bpe;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  if (Binary_Stream_P(stream))
    {
      bpe = BPE(stream);
      buffer = cl_ivector_to_c_string(file_st_h_buf(handle));
      os_nbytes = (nbytes * bpe + (OS_BPE - 1));
    }
  else
    {
      buffer = Scheme_String_To_C_String(file_st_h_buf(handle));
      os_nbytes = nbytes;
    }
  fd = file_st_h_nm_fd(nm);
  os_bytes_read = OS_cl_file_read_chars(fd, buffer, os_nbytes, &os_errno);
  if (os_bytes_read == -1) return Make_Non_Pointer(TC_IO_ERROR_CODE,os_errno);
  if (os_bytes_read == 0) return Sym_colon_eof;
  if (Binary_Stream_P(stream))
    *bytes_read = (os_bytes_read * OS_BPE) / bpe;
  else
    *bytes_read = os_bytes_read;
  return Sym_colon_ok;
}
  
Pointer file_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, nm, status;
  long os_status, os_errno;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  switch (methno)
    {
    case CLEAR_INPUT_METH:
      {
	file_st_h_nm_buf_index(nm) = 0;
	file_st_h_nm_buf_end(nm) = 0;
	return NIL;
      }
    case FORCE_OUTPUT_METH:
    case FINISH_OUTPUT_METH:
      {
	status = move_buffer(stream, 
			     file_st_h_nm_pos_base(nm) + file_st_h_nm_buf_index(nm),
			     NIL);
	if (status != Sym_colon_ok)
	  io_error(status);
	else
	  return NIL;
      }
    case CLEAR_OUTPUT_METH:
      {
	file_st_h_nm_buf_index(nm) = 0;
	move_buffer(stream, file_st_h_nm_pos_base(nm), NIL);
	return NIL;
      }
    case CLOSE_METH:
      {
	Pointer direction;

	direction = st_direction(stream);
	if ((direction == Sym_colon_output) ||
	    (direction == Sym_colon_io))
	  cl_force_output(stream);
	os_status = OS_cl_file_close(file_st_h_nm_fd(nm), &os_errno);
	if (os_status == -1)
	  return Make_Non_Pointer(TC_IO_ERROR_CODE, os_errno);
	st_opvec_index(stream) = Make_Unsigned_Fixnum(CLOSED_OPVEC);
	return TRUTH;
      }
    case FILE_POSITION_METH:
      {
	if (file_pos == NIL)
	  return C_Integer_To_Scheme_Integer(c_get_file_position(stream));
	else
	  {
	    long pos;

	    if (file_pos == Sym_colon_end)
	      pos = POS_END;
	    else 
	      Scheme_Integer_To_C_Integer(file_pos, &pos);
	    c_set_file_position(stream, pos);
	    return TRUTH;
	  }
      }
    case FILE_LENGTH_METH:
      {
	return C_Integer_To_Scheme_Integer(c_file_length(stream));
      }
    case ISA_TTY_P_METH:
      {
	return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return st_element_type(stream);
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return Make_Unsigned_Fixnum(st_nm_col(st_nm(stream)));
      }
    case PATHNAME_METH:
      {
	return st_pathname(stream);
      }
    }
}

Boolean buffer_at_eof_p(stream)
Pointer stream;
{
  /* ">=" below compensates for strange files like /dev/null */
  if (file_st_h_nm_pos_base(file_st_h_nm(st_handle(stream))) >= c_file_length(stream))
    return true;
  else
    return false;
}

long buffer_length(buffer)
Pointer buffer;
{
  if (Type_Code(buffer) == TC_CL_IVECTOR)
    return cl_ivector_length(buffer);
  else
    return string_length(buffer);
}
      

Pointer move_buffer(stream, pos, initp)
Pointer stream;
long pos;
Pointer initp;
{
  Pointer handle, nm, dir, status;
  long pb, l, i, len, bytes_read, new_pb, buf_len;
  Boolean input_or_io_p;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  dir = st_direction(stream);
  input_or_io_p = ((dir == Sym_colon_input) || (dir == Sym_colon_io));
  pb = file_st_h_nm_pos_base(nm);
  l = file_st_h_nm_buf_end(nm);
  i = file_st_h_nm_buf_index(nm);
  buf_len = buffer_length(file_st_h_buf(handle));
  if ((initp == NIL) &&
      ((dir == Sym_colon_output) ||
       (dir == Sym_colon_io)))
    {
      c_set_file_position_base(stream, pb);
      if (buffer_at_eof_p(stream))
	len = i;
      else
	len = l;
      status = c_file_output_buf(stream, len);
      if (status != Sym_colon_ok)
	return status;
    }
  if (pos == POS_END)
    {
      if (input_or_io_p)
	new_pb = max(0, c_file_length(stream) - buf_len);
      else
	new_pb = c_file_length(stream);
    }
  else
    new_pb = pos;
  c_set_file_position_base(stream, new_pb);
  file_st_h_nm_pos_base(nm) = new_pb;
  if (dir == Sym_colon_input)
    {
      status = c_file_input_buf(stream, buf_len, &bytes_read);
      if (status == Sym_colon_ok)
	file_st_h_nm_buf_end(nm) = bytes_read;
      else
	{
	  if (initp != NIL)	/* If initial call, let system get error on first read */
	    file_st_h_nm_buf_end(nm) = 0;
	  else
	    return status;
	}
    }
  else
    if (buffer_at_eof_p(stream))
      file_st_h_nm_buf_end(nm) = buf_len;
    else
    {
      status = c_file_input_buf(stream, buf_len, &bytes_read);
      if (status != Sym_colon_ok)
	return status;
      file_st_h_nm_buf_end(nm) = bytes_read;
    }
  if (pos == Sym_colon_end)
    {
      if (input_or_io_p)
	file_st_h_nm_buf_index(nm) = file_st_h_nm_buf_end(nm);
      else
	file_st_h_nm_buf_index(nm) = 0;
    }
  else
    file_st_h_nm_buf_index(nm) = 0;
  return Sym_colon_ok;
}

/* Called by OPEN */

/*
  Element-types not string-char are assumed to be binary, and that type 
  is assumed canonicalized.
*/

Define_Primitive(prim_cl_make_file_stream, 5, "CL-MAKE-FILE-STREAM")
{
  long fd, opvec_index;
  Pointer pathname, pos_to_end_p, direction, element_type;
  Pointer stream, handle, nm;
  long ivector_subtype;
  Primitive_5_Args();

  fd = Get_Integer(Arg1);
  pathname = Arg2;
  pos_to_end_p = Arg3;
  direction = Arg4;
  element_type = Arg5;
  stream = make_bare_stream();
  handle = make_bare_vector(FILE_ST_H_SIZE);
  nm = make_bare_vector_32b(FILE_ST_H_NM_SIZE);
  st_handle(stream) = handle;
  st_direction(stream) = direction;
  st_element_type(stream) = element_type;
  st_pathname(stream) = pathname;
  file_st_h_nm(handle) = nm;
  if (element_type == Sym_string_char)
    file_st_h_buf(handle) = make_bare_string(FILE_STREAM_BUFFER_SIZE);  
  else
    {
      if (is_cl_ivector_spec(element_type, &ivector_subtype))
	file_st_h_buf(handle) = make_cl_ivector(FILE_STREAM_BUFFER_SIZE, ivector_subtype, 0);
      else
	CL_Error("Bad element type ~a to open (pathname ~a)", 2, element_type, pathname);
    }
  file_st_h_nm_fd(nm) = fd;
  file_st_h_nm_buf_index(nm) = 0;
  file_st_h_nm_buf_end(nm) = FILE_STREAM_BUFFER_SIZE;
  if (element_type == Sym_string_char)
    {
      if      (direction == Sym_colon_input)
	opvec_index = FILE_ASCII_INPUT_OPVEC;
      else if (direction == Sym_colon_output)
	opvec_index = FILE_ASCII_OUTPUT_OPVEC;
      else if (direction == Sym_colon_io)
	opvec_index = FILE_ASCII_IO_OPVEC;
    }
  else
    {
      if      (direction == Sym_colon_input)
	opvec_index = FILE_BINARY_INPUT_OPVEC;
      else if (direction == Sym_colon_output)
	opvec_index = FILE_BINARY_OUTPUT_OPVEC;
      else if (direction == Sym_colon_io)
	opvec_index = FILE_BINARY_IO_OPVEC;
    }
  st_opvec_index(stream) = Make_Unsigned_Fixnum(opvec_index);
  move_buffer(stream, ((pos_to_end_p != NIL) ? POS_END : 0), TRUTH);
  return stream;
}

/*
  ******************
  * STRING STREAMS *
  ******************
*/

Pointer c_string_open(string, direction, start, end)
Pointer string, direction;
long start, end;
{
  Pointer stream, handle, nm;

  if ((string != NIL) && (direction == Sym_colon_output))
    nyi_error();
  stream = make_bare_stream();
  handle = make_bare_vector(STRING_ST_H_SIZE);
  nm = make_bare_vector_32b(STRING_ST_H_NM_SIZE);
  st_handle(stream) = handle;
  string_st_h_nm(handle) = nm;
  st_direction(stream) = direction;
  st_element_type(stream) = Sym_string_char;
  if (direction == Sym_colon_input)
    {
      st_opvec_index(stream) = STRING_INPUT_OPVEC;
      string_st_h_string(handle) = string;
      string_st_h_nm_index(nm) = start;
      string_st_h_nm_end(nm) = end;
    }
  else
    {
      st_opvec_index(stream) = STRING_OUTPUT_OPVEC;
      string_st_h_acc(handle) = TRUTH;
      /* Remaining fields take defaults */
    }
  return stream;
}

Pointer cl_string_open(string, direction, start, end)
Pointer string, direction, start, end;
{
  return c_string_open(string, direction, Get_Integer(start), Get_Integer(end));
}

Define_Primitive(prim_cl_string_open, 4, "CL-STRING-OPEN")
{
  Pointer string, direction, start, end;
  Primitive_4_Args();

  string = Arg1;
  direction = Arg2;
  start = Arg3;
  end = Arg4;

  return cl_string_open(string, direction, start, end);
}

cl_make_string_output_stream()
{
  return c_string_open(NIL, Sym_colon_output, 0, 0);
}

Define_Primitive(prim_cl_make_string_output_stream, 0, "CL-MAKE-STRING-OUTPUT-STREAM")
{
  Primitive_0_Args();
  return cl_make_string_output_stream();
}

Define_Primitive(prim_cl_make_string_input_stream, -1, "CL-MAKE-STRING-INPUT-STREAM")
{
  Pointer string;
  long start, end;
  Primitive_Variable_Args();
  
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  if (Number_Of_Args > 3)
    error_too_many_args(Number_Of_Args);
  Check_Type_Of_Arg(1, TC_CHARACTER_STRING);
  string = Primitive_Variable_Arg(1);
  if (Number_Of_Args >= 2)
    {
      Check_Type_Of_Arg(2, TC_FIXNUM);
      start = Get_Integer(Primitive_Variable_Arg(2));
    }
  else
    start = 0;
  if (Number_Of_Args >= 3)
    {
      Check_Type_Of_Arg(3, TC_FIXNUM);
      end = Get_Integer(Primitive_Variable_Arg(3));
    }
  else
    end = string_length(string);
  return c_string_open(string, Sym_colon_input, start, end);
}

Pointer string_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  Pointer handle, nm, result;
  long index;

  handle = st_handle(stream);
  nm = string_st_h_nm(handle);
  index = string_st_h_nm_index(nm);
  if (index == string_st_h_nm_end(nm))
    {
      if (eof_error_p != NIL)
	eof_error(stream);
      else
	return eof_value;
    }
  else
    {
      result = c_char_to_scheme_char(
				     (unsigned char)Scheme_String_To_C_String(string_st_h_string(handle))[index]);
      string_st_h_nm_index(nm) = index + 1;
      return result;
    }
}

Pointer string_unread_char(character, stream)
Pointer character, stream;
{
  string_st_h_nm_index(string_st_h_nm(st_handle(stream))) -= 1;
  return NIL;
}

Pointer string_listen(stream)
{
  Pointer c;

  c = cl_read_char(stream, NIL, NIL, NIL);
  cl_unread_char(c, stream);
  if (c == NIL)
    return NIL;
  else
    return TRUTH;
}

Pointer string_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  Pointer handle, nm, result;
  char *char_set, *str, *p;
  long start, end, i, j, len;

  handle = st_handle(stream);
  nm = string_st_h_nm(handle);
  char_set = Scheme_String_To_C_String(scheme_char_set);
  str = Scheme_String_To_C_String(string_st_h_string(handle));
  start = string_st_h_nm_index(nm);
  end = string_st_h_nm_end(nm);
  i = start;
  while (i < end)
    {
      if (char_set[(unsigned char)str[i]] != '\0')
	break;
      i++;
    }
  if (i == end)
    *eof_term_p = TRUTH;
  else
    *eof_term_p = NIL;
  len = i - start;
  if (len == 0)
    *chars_read_p = NIL;
  else
    *chars_read_p = TRUTH;
  if (discard_p == NIL)
    {
      result = make_bare_string(len);
      p = Scheme_String_To_C_String(result);
      for (j = start; j < i; j++)
	*p++ = str[j];
      *p = '\0';
    }
  else
    result = NIL;
  string_st_h_nm_index(nm) = i; /* important to do this after string-alloc due to possible gc-restart */
  return result;
}

/* string_write_string -- same as file_write_string */

/* string_write_char */

Pointer string_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  Pointer cell;

  Primitive_GC_If_Needed(2);
  cell = Make_Non_Pointer(TC_LIST, Free);
  Fast_Vector_Set(cell, CONS_CAR, character);
  Fast_Vector_Set(cell, CONS_CDR, string_st_h_acc(st_handle(stream)));
  string_st_h_acc(st_handle(stream)) = cell;
  Free += 2;
  string_st_h_nm_len(string_st_h_nm(st_handle(stream))) += 1;
  return character;
}

cl_get_output_stream_string(stream)
Pointer stream;
{
  Pointer handle, nm;
  Pointer char_list, result;
  long len;
  char *p;
  
  handle = st_handle(stream);
  nm = string_st_h_nm(handle);
  len = string_st_h_nm_len(nm);
  result = make_bare_string(len);
  p = Scheme_String_To_C_String(result) + len;
  *p = '\0';
  char_list = string_st_h_acc(handle);
  while (char_list != TRUTH)
    {
      *--p = (char)Datum(Fast_Vector_Ref(char_list,CONS_CAR));
      char_list = Fast_Vector_Ref(char_list,CONS_CDR);
    }
  string_st_h_acc(handle) = TRUTH;
  string_st_h_nm_len(nm) = 0;
  return result;
}

Define_Primitive(prim_cl_get_output_stream_string, 1, "CL-GET-OUTPUT-STREAM-STRING")
{
  Pointer stream;
  Primitive_1_Args();
  
  Arg_1_Type(TC_CL_STREAM);
  stream = Arg1;
  if (st_opvec_index(stream) != STRING_OUTPUT_OPVEC)
    error_not_a_string_output_stream(stream);
  return cl_get_output_stream_string(stream);
}

Pointer string_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, nm, status, dir;

  handle = st_handle(stream);
  nm = file_st_h_nm(handle);
  dir = st_direction(stream);
  switch (methno)
    {
    case CLEAR_INPUT_METH:
      {
	if (dir == Sym_colon_output)
	  output_error1(stream);
	else
	  return NIL;
      }
    case FORCE_OUTPUT_METH:
    case FINISH_OUTPUT_METH:
    case CLEAR_OUTPUT_METH:
      {
	if (dir == Sym_colon_input)
	  input_error1(stream);
	else
	  return NIL;
      }
    case CLOSE_METH:
      {
	st_handle(stream) = NIL; /* So gets gc'ed */
	st_opvec_index(stream) = Make_Unsigned_Fixnum(CLOSED_OPVEC);
	return TRUTH;
      }
    case FILE_POSITION_METH:
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return st_element_type(stream);
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return Make_Unsigned_Fixnum(st_nm_col(st_nm(stream)));
      }
    case PATHNAME_METH:
      {
	return st_pathname(stream);
      }
    }
}

/*
 ***************
 * TTY STREAMS *
 ***************
*/

Pointer cl_make_the_tty_stream()
{
  Pointer stream, handle, nm;

  stream = make_bare_stream();
  handle = make_bare_vector(TTY_ST_H_SIZE);
  nm = make_bare_vector_32b(TTY_ST_H_NM_SIZE);
  tty_st_h_unread_char_buf(handle) = make_bare_vector(TTY_UNREAD_BUF_SIZE);
  tty_st_h_nm(handle) = nm;
  st_opvec_index(stream) = Make_Unsigned_Fixnum(TTY_OPVEC);
  st_direction(stream) = Sym_colon_io;
  st_element_type(stream) = Sym_string_char;
  st_handle(stream) = handle;
  return stream;
}

Define_Primitive(prim_cl_make_the_tty_stream, 0, "CL-MAKE-THE-TTY-STREAM")
{
  Primitive_0_Args();

  return cl_make_the_tty_stream();
}

private long mod(x, m)
long x, m;
{
  long r;

  r = x % m;
  if (r < 0)
    return (m + r);
  else
    return r;
}

#ifdef butterfly
#define IMMEDIATE_TTY_IN 1
#define BUFFERED_TTY_IN 0
#endif

Pointer tty_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  Pointer handle, nm, result, unread_buf;
  char tty_char;
  long r;  /* read-char index */
  long u;  /* unread-char offset */
  long m;  /* buffer modulus */

  handle = st_handle(stream);
  nm = tty_st_h_nm(handle);
  r = tty_st_h_nm_read_char_index(nm);
  u = tty_st_h_nm_unread_char_offset(nm);
  unread_buf = tty_st_h_unread_char_buf(handle);
  m = TTY_UNREAD_BUF_SIZE;
  if (u < 0)
    {
      result = User_Vector_Ref(unread_buf, mod(r + u, m));
      u += 1;
      tty_st_h_nm_unread_char_offset(nm) = u;
      return result;
    }
  else
    {
#ifdef butterfly
      if (tty_st_h_nm_bfio_p(tty_st_h_nm(st_handle(stream))))
	BFIO_Read_String(-1, BUFFERED_TTY_IN, &tty_char, 1);
      else
#endif
	tty_char = OS_tty_read_char();

      result = c_char_to_scheme_char((unsigned char)tty_char);
      User_Vector_Set(unread_buf, r, result);
      r = mod(r + 1, m);
      tty_st_h_nm_read_char_index(nm) = r;
      return result;
    }
}

Pointer tty_unread_char(character, stream)
Pointer character, stream;
{
  tty_st_h_nm_unread_char_offset(tty_st_h_nm(st_handle(stream))) -= 1;
  return NIL;
}

Pointer tty_listen(stream)
{
  Boolean char_ready_p;
  if (tty_st_h_nm_unread_char_offset(tty_st_h_nm(st_handle(stream))) < 0)
    return TRUTH;
  else
    {
      char_ready_p = OS_read_char_ready_p(0);
      if (char_ready_p)
	return TRUTH;
      else
	return NIL;
    }
}

Pointer tty_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  char *cstring, *p;
  long cstart, cend, i, len;

  cstring = Scheme_String_To_C_String(string);
  cstart = Get_Integer(start);
  cend = Get_Integer(end);
  cstring += cstart;
  len = cend - cstart;
  if (nl_esc_p != NIL)
    {
      p = cstring + len;
      i = 0;
      while (p > cstring)
	{
	  if (*--p == '\n')
	    {
	      st_nm_col(st_nm(stream)) = 0;
	      break;
	    }
	  i++;
	}
      st_nm_col(st_nm(stream)) += i;
    }
  else st_nm_col(st_nm(stream)) += len;
  if (! (OS_tty_write_chars(cstring, len)))
    io_error(Make_Non_Pointer(TC_IO_ERROR_CODE, 0));  /* Need a code for this */
  if (flush_p != NIL)
    {
      OS_Flush_Output_Buffer();
    }
  return string;
}

Pointer tty_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  char c;

  c = (char)Datum(character);
  if (! (OS_tty_write_chars(&c, 1)))
    io_error(Make_Non_Pointer(TC_IO_ERROR_CODE, 0));  /* Need a code for this */
  if (flush_p != NIL)
    {
      OS_Flush_Output_Buffer();
    }
  return character;
}

Pointer tty_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  Pointer handle, c, result, ss;
  char *char_set;

  handle = st_handle(stream);
  char_set = Scheme_String_To_C_String(scheme_char_set);
  if (tty_st_h_read_string_acc(handle) == NIL)
    tty_st_h_read_string_acc(handle) = cl_make_string_output_stream();
  while (1)
    {
      Primitive_GC_If_Needed(2);
      ss = tty_st_h_read_string_acc(handle);
      c = tty_read_char(stream, NIL, NIL, NIL);
      if (char_set[(unsigned char)Datum(c)] != '\0')
	{
	  tty_unread_char(c, stream);
	  result = cl_get_output_stream_string(ss);
	  tty_st_h_read_string_acc(handle) = NIL;
	  *eof_term_p = NIL;
	  *chars_read_p = TRUTH;
	  return result;
	}
      else
	{
	  string_write_char(c, ss, NIL);
	}
    }
}

Pointer tty_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  switch (methno)
    {
    case FORCE_OUTPUT_METH:
      {
	OS_Flush_Output_Buffer();
	return NIL;
      }
    case CLEAR_INPUT_METH:
      {
#ifdef butterfly
	if (tty_st_h_nm_bfio_p(tty_st_h_nm(st_handle(stream))))
	  {}
	else
#endif
	  OS_Flush_Input_Buffer();
	tty_st_h_nm_read_char_index(tty_st_h_nm(st_handle(stream))) = 0;
	tty_st_h_nm_unread_char_offset(tty_st_h_nm(st_handle(stream))) = 0;
	tty_st_h_read_string_acc(st_handle(stream)) = NIL;
	return NIL;
      }
    case FINISH_OUTPUT_METH:
    case CLEAR_OUTPUT_METH:
    case CLOSE_METH:
      {
	return NIL;
      }
    case FILE_POSITION_METH:
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	return TRUTH;
      }
    case ELEMENT_TYPE_METH:
      {
	return st_element_type(stream);
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return Make_Unsigned_Fixnum(st_nm_col(st_nm(stream)));
      }
    case PATHNAME_METH:
      {
	return st_pathname(stream);
      }
    }
}

/*
 *********************
 * BROADCAST STREAMS *
 *********************
*/

Define_Primitive(prim_cl_make_broadcast_stream, 2, "CL-%MAKE-BROADCAST-STREAM")
{
  Pointer stream;
  Primitive_2_Args();
  
  stream = make_bare_stream();
  st_opvec_index(stream) = Make_Unsigned_Fixnum(BROAD_OPVEC);
  st_handle(stream) = Arg2;
  st_direction(stream) = Sym_colon_output;
  st_element_type(stream) = Arg1;
  return stream;
}
  
Pointer broad_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  Pointer handle, result;

  handle = st_handle(stream);
  result = NIL;
  while (handle != NIL)
    {
      result = cl_write_string(string, Fast_Vector_Ref(handle,CONS_CAR), start, end, nl_esc_p, flush_p);
      handle = Fast_Vector_Ref(handle,CONS_CDR);
    }
  return result;
}

Pointer broad_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  Pointer handle, result;

  handle = st_handle(stream);
  result = NIL;
  while (handle != NIL)
    {
      result = cl_write_char(character, Fast_Vector_Ref(handle,CONS_CAR), flush_p);
      handle = Fast_Vector_Ref(handle,CONS_CDR);
    }
  return result;
}

Pointer broad_write_byte(integer, stream)
Pointer integer, stream;
{
  Pointer handle, result;

  handle = st_handle(stream);
  result = NIL;
  while (handle != NIL)
    {
      result = cl_write_byte(integer, Fast_Vector_Ref(handle,CONS_CAR));
      handle = Fast_Vector_Ref(handle,CONS_CDR);
    }
  return result;
}

Pointer broad_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, result, s;

  handle = st_handle(stream);
  switch (methno)
    {
    case CLEAR_INPUT_METH:
      {
	input_error1(stream);
      }
    case FORCE_OUTPUT_METH:
    case FINISH_OUTPUT_METH:
    case CLEAR_OUTPUT_METH:
    case CLOSE_METH:
      {
	while (handle != NIL)
	  {
	    s = Fast_Vector_Ref(handle, CONS_CAR);
	    switch (methno)
	      {
	      case FORCE_OUTPUT_METH: result = cl_force_output(s); break;
	      case FINISH_OUTPUT_METH: result = cl_finish_output(s); break;
	      case CLEAR_OUTPUT_METH: result = cl_clear_output(s); break;
	      case CLOSE_METH: result = cl_close(s); break;
	      }
	    handle = Fast_Vector_Ref(handle, CONS_CDR);
	  }
	return result;
      }
    case FILE_POSITION_METH:
      {
	while (handle != NIL)
	  {
	    s = Fast_Vector_Ref(handle, CONS_CAR);
	    result = cl_file_position(s, file_pos);
	    handle = Fast_Vector_Ref(handle, CONS_CDR);
	  }
	return result;
      }
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return st_element_type(stream);
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return Make_Unsigned_Fixnum(st_nm_col(st_nm(stream)));
      }
    case PATHNAME_METH:
      {
	return st_pathname(stream);
      }
    }
}

/*
 ************************
 * CONCATENATED STREAMS *
 ************************
*/

Define_Primitive(prim_cl_make_concatenated_stream, 1, "CL-%MAKE-CONCATENATED-STREAM")
{
  Pointer stream, handle;
  Primitive_1_Args();

  stream = make_bare_stream();
  handle = make_bare_vector(CONCAT_ST_H_SIZE);
  st_opvec_index(stream) = Make_Unsigned_Fixnum(CONCAT_OPVEC);
  st_handle(stream) = handle;
  st_direction(stream) = Sym_colon_input;
  concat_st_h_current(handle) = Arg1;
  concat_st_h_streams(handle) = Arg1;
  return stream;
}

Pointer concat_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  Pointer handle, c, current, s;

  handle = st_handle(stream);
  while (1)
    {
      current = concat_st_h_current(handle);
      if (current == NIL)
	if (eof_error_p != NIL)
	  eof_error(stream);
	else
	  return eof_value;
      s = Fast_Vector_Ref(current, CONS_CAR);
      c = cl_read_char(s, NIL, NIL, NIL);
      if (c != NIL)
	return c;
      else
	concat_st_h_current(handle) = Fast_Vector_Ref(current, CONS_CDR);
    }
}

Pointer concat_unread_char(character, stream)
Pointer character, stream;
{
  return cl_unread_char(character, Fast_Vector_Ref(concat_st_h_current(st_handle(stream)), CONS_CAR));
}

Pointer concat_listen(stream)
{
  return cl_listen(Fast_Vector_Ref(concat_st_h_current(st_handle(stream)), CONS_CAR));
}

Pointer concat_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  Pointer handle, b, current, s;

  handle = st_handle(stream);
  while (1)
    {
      current = concat_st_h_current(handle);
      if (current == NIL)
	if (eof_error_p != NIL)
	  eof_error(stream);
	else
	  return eof_value;
      s = Fast_Vector_Ref(current, CONS_CAR);
      b = cl_read_byte(s, NIL, NIL, NIL);
      if (b != NIL)
	return b;
      else
	concat_st_h_current(handle) = Fast_Vector_Ref(current, CONS_CDR);
    }
}

Pointer concat_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  Pointer handle, c, result, ss;
  char *char_set;

  handle = st_handle(stream);
  char_set = Scheme_String_To_C_String(scheme_char_set);
  if (concat_st_h_read_string_acc(handle) == NIL)
    concat_st_h_read_string_acc(handle) = cl_make_string_output_stream();
  while (1)
    {
      Primitive_GC_If_Needed(2);
      ss = concat_st_h_read_string_acc(handle);
      c = concat_read_char(stream, NIL, NIL, NIL);
      if (c == NIL)
	{
	  result = cl_get_output_stream_string(ss);
	  if (string_length(result) == 0)
	    *chars_read_p = TRUTH;
	  else
	    *chars_read_p = NIL;
	  *eof_term_p = TRUTH;
	  concat_st_h_read_string_acc(handle) = NIL;
	  return result;
	}
      if (char_set[(unsigned char)Datum(c)] != '\0')
	{
	  concat_unread_char(c, stream);
	  result = cl_get_output_stream_string(ss);
	  concat_st_h_read_string_acc(handle) = NIL;
	  *eof_term_p = NIL;
	  *chars_read_p = TRUTH;
	  return result;
	}
      else
	{
	  string_write_char(c, ss, NIL);
	}
    }
}

Pointer concat_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, status, current, streams;
  long os_status, os_errno;

  handle = st_handle(stream);
  current = Fast_Vector_Ref(concat_st_h_current(handle), CONS_CAR);
  switch (methno)
    {
    case CLEAR_INPUT_METH:
      {
	return cl_clear_input(current);
      }
    case FORCE_OUTPUT_METH:
    case FINISH_OUTPUT_METH:
    case CLEAR_OUTPUT_METH:
      {
	input_error1(stream);
      }
    case CLOSE_METH:
      {
	while (streams != NIL)
	  {
	    cl_close(Fast_Vector_Ref(streams, CONS_CAR), close_abortp);
	    streams = Fast_Vector_Ref(streams, CONS_CDR);
	  }
	return TRUTH;
      }
    case FILE_POSITION_METH:
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return st_element_type(stream);
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return Make_Unsigned_Fixnum(st_nm_col(st_nm(stream)));
      }
    case PATHNAME_METH:
      {
	return st_pathname(stream);
      }
    }
}

/*
 *******************
 * SYNONYM STREAMS *
 *******************
*/

Define_Primitive(prim_cl_make_synonym_stream, 1, "CL-MAKE-SYNONYM-STREAM")
{
  Pointer stream;
  Primitive_1_Args();

  if ((Type_Code(Arg1) != TC_INTERNED_SYMBOL) &&
      (Type_Code(Arg1) != TC_UNINTERNED_SYMBOL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  stream = make_bare_stream();
  st_opvec_index(stream) = Make_Unsigned_Fixnum(SYN_OPVEC);
  st_handle(stream) = Arg1;
  return stream;
}

Pointer syn_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return cl_read_char(lexical_reference(NIL, st_handle(stream)), eof_error_p, eof_value, recursive_p);
}

Pointer syn_unread_char(character, stream)
Pointer character, stream;
{
  return cl_unread_char(character, lexical_reference(NIL, st_handle(stream)));
}

Pointer syn_listen(stream)
Pointer stream;
{
  return cl_listen(lexical_reference(NIL, st_handle(stream)));
}

Pointer syn_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return cl_read_byte(lexical_reference(NIL, st_handle(stream)), eof_error_p, eof_value, recursive_p);
}

Pointer syn_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  return cl_write_string(string, lexical_reference(NIL, st_handle(stream)), start, end, nl_esc_p, flush_p);
}

Pointer syn_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  return cl_write_char(character, lexical_reference(NIL, st_handle(stream)), flush_p);
}

Pointer syn_write_byte(integer, stream)
Pointer integer, stream;
{
  return cl_write_byte(integer, lexical_reference(NIL, st_handle(stream)));
}

Pointer syn_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  return cl_read_string(scheme_char_set, discard_p,
			lexical_reference(NIL, st_handle(stream)),
			eof_term_p, chars_read_p);
}

Pointer syn_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  stream = lexical_reference(NIL, st_handle(stream));
  return cl_misc(methno, stream, close_abortp, file_pos);
}

/*
 *******************
 * TWO-WAY STREAMS *
 *******************
*/

Define_Primitive(prim_cl_make_two_way_stream, 2, "CL-MAKE-TWO-WAY-STREAM")
{
  Pointer stream, handle;
  Primitive_2_Args();

  Arg_1_Type(TC_CL_STREAM);
  Arg_2_Type(TC_CL_STREAM);
  stream = make_bare_stream();
  handle = make_bare_vector(TWO_WAY_ST_H_SIZE);
  st_opvec_index(stream) = Make_Unsigned_Fixnum(TWO_WAY_OPVEC);
  st_direction(stream) = Sym_colon_io;
  st_handle(stream) = handle;
  two_way_st_h_input_st(handle) = Arg1;
  two_way_st_h_output_st(handle) = Arg2;
  return stream;
}

Pointer two_way_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return cl_read_char(two_way_st_h_input_st(st_handle(stream)), eof_error_p, eof_value, recursive_p);
}

Pointer two_way_unread_char(character, stream)
Pointer character, stream;
{
  return cl_unread_char(character, two_way_st_h_input_st(st_handle(stream)));
}

Pointer two_way_listen(stream)
Pointer stream;
{
  return cl_listen(two_way_st_h_input_st(st_handle(stream)));
}

Pointer two_way_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return cl_read_byte(two_way_st_h_input_st(st_handle(stream)), eof_error_p, eof_value, recursive_p);
}

Pointer two_way_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  return cl_write_string(string, two_way_st_h_output_st(st_handle(stream)), start, end, nl_esc_p, flush_p);
}

Pointer two_way_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  return cl_write_char(character, two_way_st_h_output_st(st_handle(stream)), flush_p);
}

Pointer two_way_write_byte(integer, stream)
Pointer integer, stream;
{
  return cl_write_byte(integer, two_way_st_h_output_st(st_handle(stream)));
}

Pointer two_way_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  return cl_read_string(scheme_char_set, discard_p,
			two_way_st_h_input_st(st_handle(stream)),
			eof_term_p, chars_read_p);
}

Pointer two_way_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, in_str, out_str;

  handle = st_handle(stream);
  in_str = two_way_st_h_input_st(handle);
  out_str = two_way_st_h_output_st(handle);
  switch (methno)
    {
    case CLEAR_INPUT_METH: return cl_clear_input(in_str);
    case FORCE_OUTPUT_METH: return cl_force_output(out_str);
    case FINISH_OUTPUT_METH: return cl_finish_output(out_str);
    case CLEAR_OUTPUT_METH: return cl_clear_output(out_str);
    case CLOSE_METH:
      {
	cl_close(out_str, close_abortp);
	cl_close(in_str, close_abortp);
	return TRUTH;
      }
    case FILE_POSITION_METH:
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	if ((cl_isa_tty_p(in_str) != NIL) &&
	    (cl_isa_tty_p(out_str) != NIL))
	  return TRUTH;
	else
	  return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return NIL;
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return cl_charpos(out_str);
      }
    case PATHNAME_METH:
      {
	return NIL;
      }
    }
}

/*
 ****************
 * ECHO STREAMS *
 ****************
*/

Define_Primitive(prim_cl_make_echo_stream, 2, "CL-MAKE-ECHO-STREAM")
{
  Pointer stream, handle, nm;
  Primitive_2_Args();

  Arg_1_Type(TC_CL_STREAM);
  Arg_2_Type(TC_CL_STREAM);
  stream = make_bare_stream();
  handle = make_bare_vector(ECHO_ST_H_SIZE);
  nm = make_bare_vector_32b(ECHO_ST_H_NM_SIZE);
  st_opvec_index(stream) = Make_Unsigned_Fixnum(ECHO_OPVEC);
  st_direction(stream) = Sym_colon_io;
  st_handle(stream) = handle;
  echo_st_h_nm(handle) = nm;
  echo_st_h_input_st(handle) = Arg1;
  echo_st_h_output_st(handle) = Arg2;
  echo_st_h_nm_unread_cnt(nm) = 0;
  return stream;
}

Pointer echo_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  Pointer c, cnt;

  c = cl_read_char(echo_st_h_input_st(st_handle(stream)), eof_error_p, eof_value, recursive_p);
  cnt = echo_st_h_nm_unread_cnt(echo_st_h_nm(st_handle(stream)));
  if (cnt == 0)
    cl_write_char(c, echo_st_h_output_st(st_handle(stream)), TRUTH);
  if (cnt != 0) cnt--;
  echo_st_h_nm_unread_cnt(echo_st_h_nm(st_handle(stream))) = cnt;
  return c;
}

Pointer echo_unread_char(character, stream)
Pointer character, stream;
{
  echo_st_h_nm_unread_cnt(echo_st_h_nm(st_handle(stream))) += 1;
  return cl_unread_char(character, echo_st_h_input_st(st_handle(stream)));
}

Pointer echo_listen(stream)
Pointer stream;
{
  return cl_listen(echo_st_h_input_st(st_handle(stream)));
}

Pointer echo_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return cl_write_byte(cl_read_byte(echo_st_h_input_st(st_handle(stream)), eof_error_p, eof_value, recursive_p),
		       echo_st_h_output_st(st_handle(stream)));
}

Pointer echo_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  return cl_write_string(string, echo_st_h_output_st(st_handle(stream)), start, end, nl_esc_p, flush_p);
}

Pointer echo_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  return cl_write_char(character, echo_st_h_output_st(st_handle(stream)), flush_p);
}

Pointer echo_write_byte(integer, stream)
Pointer integer, stream;
{
  return cl_write_byte(integer, echo_st_h_output_st(st_handle(stream)));
}

Pointer echo_read_string(scheme_char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer scheme_char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  Pointer string, out_stream;
  long cnt, len, start;

  cnt = echo_st_h_nm_unread_cnt(echo_st_h_nm(st_handle(stream)));
  out_stream = echo_st_h_output_st(st_handle(stream));
  string = cl_read_string(scheme_char_set, discard_p,
		       echo_st_h_input_st(st_handle(stream)),
		       eof_term_p, chars_read_p);
  len = string_length(string);
  start = min(cnt, len);
  cnt -= len;
  if (cnt < 0) cnt = 0;
  echo_st_h_nm_unread_cnt(echo_st_h_nm(st_handle(stream))) = cnt;
  cl_write_string(string, out_stream, Make_Unsigned_Fixnum(start), Make_Unsigned_Fixnum(len), TRUTH,
		  (cl_isa_tty_p(out_stream) ? TRUTH : NIL));
  return string;
}

Pointer echo_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  Pointer handle, in_str, out_str;

  handle = st_handle(stream);
  in_str = echo_st_h_input_st(handle);
  out_str = echo_st_h_output_st(handle);
  switch (methno)
    {
    case CLEAR_INPUT_METH: return cl_clear_input(in_str);
    case FORCE_OUTPUT_METH: return cl_force_output(out_str);
    case FINISH_OUTPUT_METH: return cl_finish_output(out_str);
    case CLEAR_OUTPUT_METH: return cl_clear_output(out_str);
    case CLOSE_METH:
      {
	cl_close(out_str, close_abortp);
	cl_close(in_str, close_abortp);
	return TRUTH;
      }
    case FILE_POSITION_METH:
    case FILE_LENGTH_METH:
      {
	return NIL;
      }
    case ISA_TTY_P_METH:
      {
	if ((cl_isa_tty_p(in_str) != NIL) &&
	    (cl_isa_tty_p(out_str) != NIL))
	  return TRUTH;
	else
	  return NIL;
      }
    case ELEMENT_TYPE_METH:
      {
	return NIL;
      }
    case DIRECTION_METH:
      {
	return st_direction(stream);
      }
    case CHARPOS_METH:
      {
	return cl_charpos(out_str);
      }
    case PATHNAME_METH:
      {
	return NIL;
      }
    }
}


/* 
  Error functions. We do the best we can.
*/

#define Define_Error(P1, P2, P3, Text)     \
Pointer P1(s)                              \
{                                          \
  do_error(s, Text);                       \
}                                          \
                                           \
Pointer P2(x1, s)                          \
{                                          \
  do_error(s, Text);                       \
}                                          \
                                           \
Pointer P3(x1, x2, s)                      \
{                                          \
  do_error(s, Text);                       \
}

void do_error(stream, format_string)
Pointer stream;
char *format_string;
{
  CL_Error(format_string, 1, stream);
}

Define_Error(closed_error1, closed_error2, closed_error3, "Operation illegal on closed stream ~a")
Define_Error(ascii_error1, ascii_error2, ascii_error3, "Operation illegal on ascii stream ~a")
Define_Error(binary_error1, binary_error2, binary_error3, "Operation illegal on binary stream ~a")
Define_Error(input_error1, input_error2, input_error3, "Operation illegal on input stream ~a")
Define_Error(output_error1, output_error2, output_error3, "Operation illegal on output stream ~a")

Pointer nyi_error()
{
  CL_Error("Operation not yet implemented.",0);
}

stream_type_error(s)
{
  CL_Error("~A is not a stream.",1,s);
}

string_start_end_error()
{
  CL_Error("Inconsistent string start/end spec.",0);
}

error_too_few_args(n)
{
  CL_Error("Too few args to stream operation: ~a",1,Make_Non_Pointer(TC_FIXNUM, n));
}

error_too_many_args(n)
{
  CL_Error("Too many args to stream operation: ~a",1,Make_Non_Pointer(TC_FIXNUM, n));
}

error_odd_length_keyword_list()
{
  CL_Error("Odd length keyword list.",0);
}

error_improper_keyword(s)
{
  CL_Error("~a is an improper keyword.",1,s);
}

eof_error(s)
{
  CL_Error("End of file on stream ~a",1,s);
}

arg_type_error(arg, type)
Pointer arg;
long type;
{
  CL_Error("Argument ~a is not of type ~a.",2,arg,type);
}

io_error(status)
Pointer status;
{
  CL_Error("I/O error ~a on stream.",1,status);
}

error_not_a_string_output_stream(s)
{
  CL_Error("~a is not a string output stream.",1,s);
}

error_not_a_file_stream(s)
{
  CL_Error("~a is not a file stream.",1,s);
}

/*
  The table of operations.
*/

static STREAM_OPVECS stream_opvecs = 
{
  { /* CLOSED_OPVEC */
    closed_error1,
    closed_error2,
    closed_error1,
    closed_error1,
    closed_error2,
    closed_error2,
    closed_error2,
    closed_error3,
    closed_error2
    },
  { /* FILE_ASCII_INPUT_OPVEC */
    file_read_char,
    file_unread_char,
    file_listen,
    ascii_error1,
    input_error2,
    input_error2,
    ascii_error2,
    file_read_string,
    file_misc
    },
  { /* FILE_ASCII_OUTPUT_OPVEC */
    output_error1,
    output_error2,
    output_error1,
    ascii_error1,
    file_write_string,
    file_write_char,
    ascii_error2,
    output_error3,
    file_misc
    },
  { /* FILE_ASCII_IO_OPVEC */
    file_read_char,
    file_unread_char,
    file_listen,
    ascii_error1,
    file_write_string,
    file_write_char,
    ascii_error2,
    file_read_string,
    file_misc
    },
  { /* FILE_BINARY_INPUT_OPVEC */
    binary_error1,
    binary_error2,
    binary_error1,
    file_read_byte,
    output_error2,
    output_error2,
    output_error2,
    binary_error3,
    file_misc
    },
  { /* FILE_BINARY_OUTPUT_OPVEC */
    binary_error1,
    binary_error2,
    binary_error1,
    output_error1,
    binary_error2,
    binary_error2,
    file_write_byte,
    binary_error3,
    file_misc
    },
  { /* FILE_BINARY_IO_OPVEC */
    binary_error1,
    binary_error2,
    binary_error1,
    file_read_byte,
    binary_error2,
    binary_error2,
    file_write_byte,
    binary_error3,
    file_misc
    },
  { /* STRING_INPUT_OPVEC */
    string_read_char,
    string_unread_char,
    string_listen,
    ascii_error1,
    output_error2,
    output_error2,
    ascii_error2,
    string_read_string,
    string_misc
    },
  { /* STRING_OUTPUT_OPVEC */
    output_error1,
    output_error2,
    output_error1,
    ascii_error1,
    file_write_string,  /* Calls generic write_char; make more efficient someday */
    string_write_char,
    ascii_error2,
    output_error3,
    string_misc
    },
  { /* TTY_OPVEC */
    tty_read_char,
    tty_unread_char,
    tty_listen,
    ascii_error1,
    tty_write_string,
    tty_write_char,
    ascii_error2,
    tty_read_string,
    tty_misc
    },
  { /* BROAD_OPVEC */
    input_error1,
    input_error2,
    input_error1,
    input_error1,
    broad_write_string,
    broad_write_char,
    broad_write_byte,
    input_error3,
    broad_misc
    },
  { /* CONCAT_OPVEC */
    concat_read_char,
    concat_unread_char,
    concat_listen,
    concat_read_byte,
    input_error2,
    input_error2,
    concat_read_string,
    concat_misc
    },
  { /* SYN_OPVEC */
    syn_read_char,
    syn_unread_char,
    syn_listen,
    syn_read_byte,
    syn_write_string,
    syn_write_char,
    syn_write_byte,
    syn_read_string,
    syn_misc
    },
  { /* ECHO_OPVEC */
    two_way_read_char,
    two_way_unread_char,
    two_way_listen,
    two_way_read_byte,
    two_way_write_string,
    two_way_write_char,
    two_way_write_byte,
    two_way_read_string,
    two_way_misc
    },
  { /* ECHO_OPVEC */
    echo_read_char,
    echo_unread_char,
    echo_listen,
    echo_read_byte,
    echo_write_string,
    echo_write_char,
    echo_write_byte,
    echo_read_string,
    echo_misc
    }
};

/*
  Main entries
*/

/*
 *************
 * READ-CHAR *
 *************
*/

Pointer cl_read_char(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return Dispatch_Stream_Method(stream, READ_CHAR_METH)(stream, eof_error_p, eof_value, recursive_p);
}

Define_Primitive(prim_cl_read_char, -1, "CL-READ-CHAR")
{
  Pointer stream, eof_error_p, eof_value, recursive_p;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(4);
  Resolve_Input_Stream(1);
  Resolve_Default_Input_Stream_Args(2);
  /* We dispatch instead of calling cl_read_char for highest efficiency */
  return Dispatch_Stream_Method(stream, READ_CHAR_METH)(stream, eof_error_p, eof_value, recursive_p);
}

/*
 *********************
 * READ-CHAR-NO-HANG *
 *********************
*/

Define_Primitive(prim_cl_read_char_no_hang, -1, "CL-READ-CHAR-NO-HANG")
{
  Pointer stream, eof_error_p, eof_value, recursive_p;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(4);
  Resolve_Input_Stream(1);
  Resolve_Default_Input_Stream_Args(2);
  if (cl_listen(stream) != NIL)
    return cl_read_char(stream, eof_error_p, eof_value, recursive_p);
  else return NIL;
}

/*
 ***************
 * UNREAD-CHAR *
 ***************
*/

Pointer cl_unread_char(character, stream)
Pointer character, stream;
{
  return Dispatch_Stream_Method(stream, UNREAD_CHAR_METH)(character, stream);
}

Define_Primitive(prim_cl_unread_char, -1, "CL-UNREAD-CHAR")
{
  Pointer character, stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(2);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  character = Primitive_Variable_Arg(1);
  Resolve_Input_Stream(2);
  /* We dispatch instead of calling cl_unread_char for highest efficiency */
  return Dispatch_Stream_Method(stream, UNREAD_CHAR_METH)(character, stream);
}


/*
 **********
 * LISTEN *
 **********
*/

Pointer cl_listen(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, LISTEN_METH)(stream);
}

Define_Primitive(prim_cl_listen, -1, "CL-LISTEN")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Input_Stream(1);
  return cl_listen(stream);
}

/*
 ***************
 * CLEAR-INPUT *
 ***************
*/

Pointer cl_clear_input(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(CLEAR_INPUT_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_clear_input, -1, "CL-CLEAR-INPUT")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Input_Stream(1);
  return cl_clear_input(stream);
}

/*
 *************
 * READ-BYTE *
 *************
*/

Pointer cl_read_byte(stream, eof_error_p, eof_value, recursive_p)
Pointer stream, eof_error_p, eof_value, recursive_p;
{
  return Dispatch_Stream_Method(stream, READ_BYTE_METH)(stream, eof_error_p, eof_value, recursive_p);
}

Define_Primitive(prim_cl_read_byte, -1, "CL-READ-BYTE")
{
  Pointer stream, eof_error_p, eof_value, recursive_p;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(4);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  Check_Type_Of_Arg(1,TC_CL_STREAM);
  stream = Primitive_Variable_Arg(1);
  Resolve_Default_Input_Stream_Args(2);
  /* We dispatch instead of calling cl_read_char for highest efficiency */
  return Dispatch_Stream_Method(stream, READ_BYTE_METH)(stream, eof_error_p, eof_value, recursive_p);
}

/*
 **************
 * WRITE-CHAR *
 **************
*/

#define Check_Newline()              \
  if (character == Newline_Char)     \
    st_nm_col(st_nm(stream)) = 0;    \
  else                               \
    st_nm_col(st_nm(stream)) += 1

Pointer cl_write_char(character, stream, flush_p)
Pointer character, stream, flush_p;
{
  Check_Newline();
  return Dispatch_Stream_Method(stream, WRITE_CHAR_METH)(character, stream, flush_p);
}

Define_Primitive(prim_cl_write_char, -1, "CL-WRITE-CHAR")
{
  Pointer character, stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(2);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  character = Primitive_Variable_Arg(1);
  Touch_In_Primitive(character, character);
  Resolve_Output_Stream(2);
  Check_Newline();
  /* We dispatch instead of calling cl_write_char for highest efficiency */
  return Dispatch_Stream_Method(stream, WRITE_CHAR_METH)(character, stream, TRUTH);
}

/*
 ***********************
 * INTERNAL-WRITE-CHAR *
 ***********************
*/

Define_Primitive(prim_cl_internal_write_char, 1, "CL-INTERNAL-WRITE-CHAR")
{
  Pointer character, stream;
  Primitive_1_Args();
  character = Arg1;
  stream = lexical_reference(NIL, Sym_star_standard_output_star);
  Check_Newline();
  /* We dispatch instead of calling cl_write_char for highest efficiency */
  return Dispatch_Stream_Method(stream, WRITE_CHAR_METH)(character, stream, NIL);
}

/*
 ****************
 * WRITE-STRING *
 ****************
*/

Pointer cl_write_string(string, stream, start, end, nl_esc_p, flush_p)
Pointer string, stream, start, end, nl_esc_p, flush_p;
{
  long cstart, cend;

  Sign_Extend(start, cstart);
  Sign_Extend(end, cend);
  if ((cstart < 0) ||
      (cend < cstart) ||
      (cend > string_length(string)))
    string_start_end_error();
  return Dispatch_Stream_Method(stream, WRITE_STRING_METH)(string, stream, start, end, nl_esc_p, flush_p);
}

Pointer cl_write_string_opt_nl(nl_p)
Pointer nl_p;
{
  Pointer string, stream, start, end;
  long i;
  Pointer keyword;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(6);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  string = Primitive_Variable_Arg(1);
  Resolve_Output_Stream(2);
  start = Make_Unsigned_Fixnum(0);
  end = Make_Unsigned_Fixnum(string_length(string));
  for (i = 3; i <= Number_Of_Args; i += 2)
    {
      if ((i + 1) > Number_Of_Args)
	error_odd_length_keyword_list();
      keyword = Primitive_Variable_Arg(i);
      if (keyword == Sym_colon_start)
	start = Primitive_Variable_Arg(i + 1);
      else if (keyword == Sym_colon_end)
	end = Primitive_Variable_Arg(i + 1);
      else
	error_improper_keyword(keyword);
    }
  cl_write_string(string, stream, start, end, TRUTH, TRUTH);
  if (nl_p != NIL) cl_write_char(Newline_Char, stream);
  return string;
}
 
Define_Primitive(cl_prim_write_string, -1, "CL-WRITE-STRING")
{
  return cl_write_string_opt_nl(NIL);
}

/*
 **************
 * WRITE-LINE *
 **************
*/

Define_Primitive(cl_prim_write_line, -1, "CL-WRITE-LINE")
{
  return cl_write_string_opt_nl(TRUTH);
}

/*
 *************************
 * INTERNAL-WRITE-STRING *
 *************************
*/

/*
  (internal-write-string string nl_esc_p)

  Assumes *standard-output* as the stream.
*/

Define_Primitive(prim_cl_internal_write_string, 2, "CL-INTERNAL-WRITE-STRING")
{
  Pointer stream, string, start, end, nl_esc_p;
  Primitive_2_Args();

  stream = lexical_reference(NIL, Sym_star_standard_output_star);
  string = Arg1;
  start = Make_Non_Pointer(TC_FIXNUM, 0);
  end = Make_Non_Pointer(TC_FIXNUM, string_length(string));
  nl_esc_p = Arg2;
  return Dispatch_Stream_Method(stream, WRITE_STRING_METH)(string, stream, start, end, nl_esc_p, NIL);
}

/*
 ***********************************
 * INTERNAL-WRITE-STRING-START-END *
 ***********************************
*/

/*
  (internal-write-string-start-end string nl_esc_p start end)

  Assumes *standard-output* as the stream.
*/

Define_Primitive(prim_cl_internal_write_string_start_end, 4, "CL-INTERNAL-WRITE-STRING-START-END")
{
  Pointer stream, string, start, end, nl_esc_p;
  Primitive_4_Args();

  stream = lexical_reference(NIL, Sym_star_standard_output_star);
  string = Arg1;
  start = Arg3;
  end = Arg4;
  nl_esc_p = Arg2;
  return Dispatch_Stream_Method(stream, WRITE_STRING_METH)(string, stream, start, end, nl_esc_p, NIL);
}

/*
 *****************
 * FINISH-OUTPUT *
 *****************
*/

Pointer cl_finish_output(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(FINISH_OUTPUT_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_finish_output, -1, "CL-FINISH-OUTPUT")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Output_Stream(1);
  return cl_finish_output(stream);
}

/*
 ****************
 * FORCE-OUTPUT *
 ****************
*/

Pointer cl_force_output(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(FORCE_OUTPUT_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_force_output, -1, "CL-FORCE-OUTPUT")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Output_Stream(1);
  return cl_force_output(stream);
}

/*
 ****************
 * CLEAR-OUTPUT *
 ****************
*/

Pointer cl_clear_output(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(CLEAR_OUTPUT_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_clear_output, -1, "CL-CLEAR-OUTPUT")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Output_Stream(1);
  return cl_clear_output(stream);
}

/*
 **************
 * WRITE-BYTE *
 **************
*/

Pointer cl_write_byte(integer, stream)
Pointer integer, stream;
{
  return Dispatch_Stream_Method(stream, WRITE_BYTE_METH)(integer, stream);
}

Define_Primitive(prim_cl_write_byte, 2, "CL-WRITE-BYTE")
{
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_CL_STREAM);
  return cl_write_byte(Arg1, Arg2);
}

/*
 *****************
 * FILE-POSITION *
 *****************
*/

Pointer cl_file_position(stream, pos)
Pointer stream, pos;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(FILE_POSITION_METH, stream, NIL, pos);
}

Define_Primitive(prim_cl_file_position, -1, "CL-FILE-POSITION")
{
  Pointer pos;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(2);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  Check_Type_Of_Arg(1,TC_CL_STREAM);
  if (Number_Of_Args > 1)
  { pos = Primitive_Variable_Arg(2);
    Touch_In_Primitive(pos, pos);
  }
  else
    pos = NIL;
  return cl_file_position(Primitive_Variable_Arg(1), pos);
}

/*
 ***************
 * FILE-LENGTH *
 ***************
*/

Pointer cl_file_length(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(FILE_LENGTH_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_file_length, 1, "CL-FILE-LENGTH")
{
  Primitive_1_Args();
  Arg_1_Type(TC_CL_STREAM);
  return cl_file_length(Arg1);
}

/*
 *************
 * ISA-TTY-P *
 *************
*/

Pointer cl_isa_tty_p(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(ISA_TTY_P_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_isa_tty, 1, "CL-ISA-TTY-P")
{
  Primitive_1_Args();
  Arg_1_Type(TC_CL_STREAM);
  return cl_isa_tty_p(Arg1);
}

/*
 *********
 * CLOSE *
 *********
*/

Pointer cl_close(stream, abortp)
Pointer stream, abortp;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(CLOSE_METH, stream, abortp, NIL);
}

Define_Primitive(prim_cl_close, -1, "CL-CLOSE")
{
  Pointer stream, abortp;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(3);
  if (Number_Of_Args < 1)
    error_too_few_args(Number_Of_Args);
  Check_Type_Of_Arg(1,TC_CL_STREAM);
  stream = Primitive_Variable_Arg(1);
  if (Number_Of_Args == 2)
    error_odd_length_keyword_list();
  if (Number_Of_Args == 3)
    {
      if (Primitive_Variable_Arg(2) != Sym_colon_abort)
	error_improper_keyword(Primitive_Variable_Arg(2));
      else
      { abortp = Primitive_Variable_Arg(3);
	Touch_In_Primitive(abortp, abortp);
      }
    }
  else
    abortp = NIL;
  return cl_close(stream, abortp);
}

/*
 ***************
 * READ-STRING *
 ***************
*/

Pointer cl_read_string(char_set, discard_p, stream, eof_term_p, chars_read_p)
Pointer char_set, discard_p, stream, *eof_term_p, *chars_read_p;
{
  return Dispatch_Stream_Method(stream, READ_STRING_METH)
    (char_set, discard_p, stream, eof_term_p, chars_read_p);
}

/*
  Not a commonlisp function. Used analogously to Scheme's
  read-string. Accepts similar args to read-line, 
  with a discard flag to emulate scheme's discard-chars.
  When <discard-p> is true, the first value is always nil;
  the second value is eof-term-p as usual.

  (read-string <char-set> <discard-p> &optional <stream> <eof-error-p> <eof-value> <recursive-p>)
*/

Define_Primitive(prim_cl_read_string, -1, "CL-READ-STRING")
{
  Pointer char_set, discard_p, stream, eof_error_p, eof_value, recursive_p;
  Pointer eof_term_p, chars_read_p, r;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(6);
  if (Number_Of_Args < 2)
    error_too_few_args(Number_Of_Args);
  char_set = Primitive_Variable_Arg(1);
  Touch_In_Primitive(char_set, char_set);
  discard_p = Primitive_Variable_Arg(2);
  Touch_In_Primitive(discard_p, discard_p);
  Resolve_Input_Stream(3);
  Resolve_Default_Input_Stream_Args(4);
  r = cl_read_string(char_set, discard_p, stream, &eof_term_p, &chars_read_p);
  if ((eof_term_p != NIL) && 
      (chars_read_p == NIL))
    {
      if (eof_error_p != NIL)
	eof_error(stream);
      else
	r = eof_value;
    }
  Multiple_Value_Return(Number_Of_Args + 1, 2, r, Push(eof_term_p); Push(r));
}

/*
 *************
 * READ-LINE *
 *************
*/

Define_Primitive(prim_cl_read_line, -1, "CL-READ-LINE")
{
  Pointer stream, eof_error_p, eof_value, recursive_p;
  Pointer eof_term_p, chars_read_p;
  Pointer r;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(4);
  Resolve_Input_Stream(1);
  Resolve_Default_Input_Stream_Args(2);
  r = cl_read_string(Newline_char_set, NIL, stream, &eof_term_p, &chars_read_p);
  if ((eof_term_p != NIL) &&
      (chars_read_p == NIL))
    {
      if (eof_error_p != NIL)
	eof_error(stream);
      else
	r = eof_value;
    }
  cl_read_char(stream, NIL, NIL, NIL);
  Multiple_Value_Return(Number_Of_Args + 1, 2, r, Push(eof_term_p); Push(r));
}

/*
 ********
 * MISC *
 ********
*/

/* Called internally only */

Pointer cl_misc(methno, stream, close_abortp, file_pos)
long methno;
Pointer stream, close_abortp, file_pos;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(methno, stream, close_abortp, file_pos);
}

/*
 ***********
 * CHARPOS *
 ***********
*/

Pointer cl_charpos(stream)
Pointer stream;
{
  return Dispatch_Stream_Method(stream, MISC_METH)(CHARPOS_METH, stream, NIL, NIL);
}

Define_Primitive(prim_cl_charpos, -1, "CL-CHARPOS")
{
  Pointer stream;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(1);
  Resolve_Output_Stream(1);
  return cl_charpos(stream);
}

/*
 *************
 * PEEK-CHAR *
 *************
*/

Pointer cl_peek_char(peek_type, stream, eof_error_p, eof_value, recursive_p)
Pointer peek_type, stream, eof_error_p, eof_value, recursive_p;
{
  Pointer c;

  c = cl_read_char(stream, eof_error_p, eof_value, NIL);
  if (c == eof_value)
    return c;
/*
  else if (Type_Code(c) == TC_CHARACTER)
    {
      for (c = c;
	   !((c == eof_value) || (c == peek_type));
	   c = cl_read_char(stream, eof_error_p, eof_value, NIL))
	{};
      if (c != eof_value)
	cl_unread_char(c, stream);
      return c;
    }
*/
  else
    {
      cl_unread_char(c, stream);
      return c;
    }
}

Define_Primitive(prim_cl_peek_char, -1, "CL-PEEK-CHAR")
{
  Pointer peek_type, stream, eof_error_p, eof_value, recursive_p;
  Primitive_Variable_Args();
  Check_At_Most_N_Args(5);
  if (Number_Of_Args < 1)
    peek_type = NIL;
  else
  {
    peek_type = Primitive_Variable_Arg(1);
    Touch_In_Primitive(peek_type, peek_type);
  }
  Resolve_Input_Stream(2);
  Resolve_Default_Input_Stream_Args(3);
  return cl_peek_char(peek_type, stream, eof_error_p, eof_value, recursive_p);
}

/*
 ***********************
 * STREAM-ELEMENT-TYPE *
 ***********************
*/

Define_Primitive(prim_cl_stream_element_type, 1, "CL-STREAM-ELEMENT-TYPE")
{
  Primitive_1_Args();

  Arg_1_Type(TC_CL_STREAM);
  return cl_misc(ELEMENT_TYPE_METH, Arg1, NIL, NIL);
}

/*
 ********************
 * STREAM-DIRECTION *
 ********************
*/

Define_Primitive(prim_cl_stream_direction, 1, "CL-STREAM-DIRECTION")
{
  Primitive_1_Args();

  Arg_1_Type(TC_CL_STREAM);
  return cl_misc(DIRECTION_METH, Arg1, NIL, NIL);
}

/*
 *******************
 * STREAM-PATHNAME *
 *******************
*/

Define_Primitive(prim_cl_stream_pathname, 1, "CL-STREAM-PATHNAME")
{
  Primitive_1_Args();

  Arg_1_Type(TC_CL_STREAM);
  return cl_misc(PATHNAME_METH, Arg1, NIL, NIL);
}

/*
  Internal accessors, mainly for printing
*/

/*
 **************
 * Predicates *
 **************
*/

/* Note that these are order-dependent */

Define_Primitive(prim_cl_closed_stream_p, 1, "CL-CLOSED-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == CLOSED_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_file_stream_p, 1, "CL-FILE-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return(((Get_Integer(st_opvec_index(Arg1)) >= FILE_ASCII_INPUT_OPVEC) &&
	  (Get_Integer(st_opvec_index(Arg1)) <= FILE_BINARY_IO_OPVEC)) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_string_stream_p, 1, "CL-STRING-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return(((Get_Integer(st_opvec_index(Arg1)) >= STRING_INPUT_OPVEC) &&
	  (Get_Integer(st_opvec_index(Arg1)) <= STRING_OUTPUT_OPVEC)) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_tty_stream_p, 1, "CL-TTY-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == TTY_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_broadcast_stream_p, 1, "CL-BROADCAST-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == BROAD_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_concat_stream_p, 1, "CL-CONCATENATED-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == CONCAT_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_synonym_stream_p, 1, "CL-SYNONYM-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == SYN_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_two_way_stream_p, 1, "CL-TWO-WAY-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == TWO_WAY_OPVEC) ? TRUTH : NIL);
}

Define_Primitive(prim_cl_echo_stream_p, 1, "CL-ECHO-STREAM?")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return((Get_Integer(st_opvec_index(Arg1)) == ECHO_OPVEC) ? TRUTH : NIL);
}

/*
 ****************************
 * BROADCAST-STREAM-STREAMS *
 ****************************
*/

Define_Primitive(prim_broadcast_stream_streams, 1, "CL-BROADCAST-STREAM-STREAMS")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);

  return st_handle(Arg1);
}

/*
 *******************************
 * CONCATENATED-STREAM-STREAMS *
 *******************************
*/

Define_Primitive(prim_concatenated_stream_streams, 1, "CL-CONCATENATED-STREAM-STREAMS")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return concat_st_h_streams(st_handle(Arg1));
}

/*
 *************************
 * SYNONYM-STREAM-SYMBOL *
 *************************
*/

Define_Primitive(prim_synonym_stream_symbol, 1, "CL-SYNONYM-STREAM-SYMBOL")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return st_handle(Arg1);
}

/*
 *******************************
 * TWO-WAY-STREAM-INPUT-STREAM *
 *******************************
*/

Define_Primitive(prim_two_way_stream_input_stream, 1, "CL-TWO-WAY-STREAM-INPUT-STREAM")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return two_way_st_h_input_st(st_handle(Arg1));
}

/*
 ********************************
 * TWO-WAY-STREAM-OUTPUT-STREAM *
 ********************************
*/

Define_Primitive(prim_two_way_stream_output_stream, 1, "CL-TWO-WAY-STREAM-OUTPUT-STREAM")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return two_way_st_h_output_st(st_handle(Arg1));
}

/*
 ****************************
 * ECHO-STREAM-INPUT-STREAM *
 ****************************
*/

Define_Primitive(prim_echo_stream_input_stream, 1, "CL-ECHO-STREAM-INPUT-STREAM")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return echo_st_h_input_st(st_handle(Arg1));
}

/*
 *****************************
 * ECHO-STREAM-OUTPUT-STREAM *
 *****************************
*/

Define_Primitive(prim_echo_stream_output_stream, 1, "CL-ECHO-STREAM-OUTPUT-STREAM")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return echo_st_h_output_st(st_handle(Arg1));
}

/*
 *****************************
 * STRING-INPUT-STREAM-INDEX *
 *****************************
*/

Define_Primitive(prim_string_input_stream_index, 1, "CL-STRING-INPUT-STREAM-INDEX")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return C_Integer_To_Scheme_Integer(string_st_h_nm_index(string_st_h_nm(st_handle(Arg1))));
}

/*
 *****************************
 * STREAM-INFO               *
 *****************************
*/

/* The general information associated with a stream (e.g., format conversion string) */

Define_Primitive(prim_stream_info, 1, "CL-STREAM-INFO")
{
  Primitive_1_Args();
  Touch_In_Primitive(Arg1, Arg1);
  return st_info(Arg1);
}

/*
 *****************************
 * SET-STREAM-INFO!          *
 *****************************
*/

/* Set info described above */

Define_Primitive(prim_set_stream_info, 2, "CL-SET-STREAM-INFO!")
{
  Primitive_2_Args();
  Touch_In_Primitive(Arg1, Arg1);
  st_info(Arg1) = Arg2;
  return Arg2;
}

/*
 ******************
 * SET-BFIO-MODE! *
 ******************
*/

Define_Primitive(prim_set_bfio_mode, 2, "CL-SET-BFIO-MODE!")
{
  Pointer result;
  Primitive_2_Args();
  Touch_In_Primitive(Arg1, Arg1);
  Touch_In_Primitive(Arg2, Arg2);
  if (Get_Integer(st_opvec_index(Arg1)) != TTY_OPVEC)
    CL_Error("Argument ~a to set-bfio-mode! is not a tty stream", 1, Arg1);
  result = (tty_st_h_nm_bfio_p(tty_st_h_nm(st_handle(Arg1))) ? TRUTH : NIL);
  tty_st_h_nm_bfio_p(tty_st_h_nm(st_handle(Arg1))) =
    (Arg2 != NIL ? 1 : 0);
  return result;
}
