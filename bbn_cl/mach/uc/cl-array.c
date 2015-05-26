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
#include "character.h"
#include "stringprim.h"
#include "bitstr.h"
#include "cl-array.h"

#define private static
#define debug	1

/* Things to worry about:

	(2) Perhaps the element_type_slot (ET_SLOT) is redundant, since it
	    is always possible to figure out the element type from the
	    data vector.  (That is in fact how array-element-type works.)

	    A related problem is that if an array of type '(mod 12) is
	    created, currently the type (mod 16) is what gets remembered,
	    which means that if the values 12-15 are valid elements to be
	    stored in the array.  I don't know if that's okay, but I do
	    know that Lucid does it this way, too.

	(5) All of the procedures that do element-type specific things
	    currently switch on the type code of the data vector they are
	    passed.  Currently there are 4 different kinds of data vectors.
	    Whether this means that it should be done with a big table or
	    not is up to you.  I decided that it didn't, because there are
	    so few, and because often the operation being done was actually
	    done entirely by a C macro, which meant I would have had to
	    define a whole new procedure (horrors!) to do this simple thing
	    for this particular type of vector.

	    In other words, it was a mostly random decision, based on the
	    mood I was in at the time (lazy).

	(6) In a couple of places I increment a SCHEME integer as though
	    it were a C integer.  I'm not sure if this is cool or not.  It
	    works, but it might not in the future.  It probably should be
	    changed, but I don't feel up to it.

	(7) copy_array_contents() could be made faster by taking the
	    dimension multiplication out of the procedure and put it
	    in a higher level procedure, storing the results in global
	    array.

	(8) Some utility functions in this file return C integers and
	    some return scheme integers.  It depended on what seemed
	    more convenient at the time.  I think I wish I had come up
	    with some naming convention at least.

   I will try to comment the rest of the file in a reasonable, helpful
   way, in the event that I am not around long enough to implement
   everything, and/or fix bugs.  I refuse to comment procedures which
   are obvious.

   NOTE: I use the words subscripts and indices interchangeably
	 throughout the code.  I think I used whatever the manual uses,
	 when the manual uses it.

   JP - 1/8/88 */

/* I was getting really sick of this names, let me tell you! */
#define citsi	C_Integer_To_Scheme_Integer
#define sitci	Scheme_Integer_To_C_Integer

private Pointer	array_has_fill_pointer(),
		copy_vector_contents(),
		cl_fill_pointer();

Define_Primitive(prim_cl_array_dimension_limit, 0, "cl-array-dimension-limit")
{
	return citsi(ARRAY_DIMENSION_LIMIT);
}

Define_Primitive(prim_cl_array_rank_limit, 0, "cl-array-rank-limit")
{
	return citsi(ARRAY_RANK_LIMIT);
}

Define_Primitive(prim_cl_array_total_size_limit, 0, "cl-array-total-size-limit")
{
	return citsi(ARRAY_TOTAL_SIZE_LIMIT);
}

#define error_too_few_args(me) \
CL_Error("Too few args to ~A", 1, C_String_To_Scheme_String(me))

#define error_too_many_args(me) \
CL_Error("Too many args to ~A", 1, C_String_To_Scheme_String(me))

long
scheme_to_c_integer(i)
Pointer	i;
{
	long	c_i;

	if (sitci(i, &c_i) == PRIM_DONE)
		return c_i;
	CL_Error("Internal error converting scheme integer ~A to C integer", 1, i);
}

private
vector_like(v)
Pointer	v;
{
	long	type = Type_Code(v);

	return ((type == TC_VECTOR) || (type == TC_CHARACTER_STRING) ||
		(type == TC_VECTOR_1B) || (type == TC_CL_IVECTOR));
}

private
array_like(array)
Pointer	array;
{
	long	type = Type_Code(array);

	return ((type == TC_CL_ARRAY) || vector_like(array));
}

private
simple_array(array)
Pointer	array;
{
	long	type = Type_Code(array);

	return (vector_like(array) ||
		((type == TC_CL_ARRAY) &&
		 !array_has_fill_pointer(array) &&
		 !array_is_displaced(array) &&
		 !array_is_adjustable(array)));
}

Define_Primitive(prim_simple_array_p, 1, "cl-simple-array-p")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	return simple_array(Arg1) ? TRUTH : NIL;
}

/* Extract_dimensions.  DIM_SPEC is a dimension specification directly
   from lisp.  That is, it's either an integer or a list of integers.
   This procedure examines DIM_SPEC, does ALL the necessary error
   checking, and stores the dimensions in the parameter DIM_BUF, which
   should be a C array of longs.  It also calculates the total size of
   the array and passes that back in TOTAL_SIZE, if TOTAL_SIZE is not
   a NULL pointer (that is, if the caller really wants to know).

   The return value is the number of dimensions.

   NOTE:  The total size is 1 if this is a zero-dimensional array.
          This fits with the commonlisp spec, which says that an
	  array of 0 dimensions has exactly one element. */

private long
extract_dimensions(dim_spec, dim_buf, total_size)
Pointer	dim_spec;
long	dim_buf[],
	*total_size;
{
	register int	i;		/* number of dimensions */
	long	total = 1;
	long    tc;

	if (Type_Code(dim_spec) == TC_FIXNUM) {
		dim_buf[1] = scheme_to_c_integer(dim_spec);
		if (total_size != 0)
			*total_size = dim_buf[1];
		return 1;		/* that is, one dimension */
	}
	if ((tc = Type_Code(dim_spec)) != TC_LIST && dim_spec != NIL)
	  CL_Error("~A not a valid dimension spec", 1, dim_spec);
	i = 1;
	while (Type_Code(dim_spec) == TC_LIST) {
		Pointer	dim_car = Vector_Ref(dim_spec, CONS_CAR);
		long	dim_value;

		if (i > ARRAY_RANK_LIMIT)
		  CL_Error("Array rank too large: ~A", 1, dim_spec);
		if (Type_Code(dim_car) != TC_FIXNUM)
		  CL_Error("Subscript not fixnum: ~A", 1, dim_car);
		dim_value = scheme_to_c_integer(dim_car);
		if (dim_value < 0)
		  CL_Error("Subscript out of range: ~A", 1, dim_value);
		if ((total_size != 0)  &&
		    ((total * dim_value < total) ||		/* wrapped */
		     ((total *= dim_value) > ARRAY_TOTAL_SIZE_LIMIT)))
		  CL_Error("Array total size too large: ~A", 1, total_size);
		dim_buf[i++] = dim_value;
		dim_spec = Vector_Ref(dim_spec, CONS_CDR);
	}
	if (total_size != 0)
		*total_size = total;
	return i - 1;			/* since 'i' started at 1 */
}

#define header_set	Vector_Set
#define header_ref	Vector_Ref

/* Make a commonlisp array header.  This builds a commonlisp array
   header (a pointer of type TC_CL_ARRAY) and creates a nonmarked
   32B vector to store the dimensions in.  This initializes everything
   in the header to NIL, except for the dimensions slot. */

private Pointer
make_cl_array_header(n_dims, dims)
long	n_dims,
	dims[];
{
	Pointer	header,
		header_dims;
	register int	i;

	Primitive_GC_If_Needed(ARRAY_HEADER_SIZE + 1);
	header = Make_Pointer(TC_CL_ARRAY, Free);
	*Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, ARRAY_HEADER_SIZE);
	for (i = ARRAY_HEADER_SIZE; --i >= 0; )
		*Free++ = NIL;

	header_dims = make_bare_vector_32b(n_dims);
	for (i = 1; i <= n_dims; i++)
		Vector_Set(header_dims, i, dims[i]);
	header_set(header, DIMS_SLOT, header_dims);

	return header;
}

/* returns the number of dimensions in array */

private long
array_rank(array)
Pointer	array;
{
	if (Type_Code(array) == TC_CL_ARRAY)
		return Vector_Length(header_ref(array, DIMS_SLOT));
	else
		return 1;
}

/* Here's one of those procedures that switches on the type of the vector
   being passed.  This one figures out the element type based on the type
   of vector. */

private Pointer
data_vector_type(v)
Pointer	v;
{
	switch (Type_Code(v)) {
	case TC_VECTOR:
		return TRUTH;

	case TC_VECTOR_1B:
		return Sym_bit;

	case TC_CL_IVECTOR:
		return cl_ivector_subtype_indicator(v);

	case TC_CHARACTER_STRING:
		return Sym_string_char;

	default:
		CL_Error("Unknown data vector type: ~A", 1, citsi(Type_Code(v)));
	}
}

/* This returns the number of pointers of overhead for vector V.  This is
   gross in a rather serious way.  It's used by vector_push_extend()
   which is gross in it's own way.  What we have here is something
   gross.  */

private long
pointer_overhead(v)
Pointer	v;
{
  switch (Type_Code(v))
    {
    case TC_VECTOR: return 1;

    case TC_VECTOR_1B: return 2;

    case TC_CL_IVECTOR:
      return tc_cl_ivector_overhead();

    case TC_CHARACTER_STRING: return 2;

    default:
      CL_Error("Unknown data vector type: ~A", 1, citsi(Type_Code(v)));
    }
}

/* Get_data_vector() returns the data vector associated with the array-like
   object, OBJ.  If OBJ is just a vector it is returned.  If OBJ is an
   array header, the DATA_SLOT is returned.  If the DATA_SLOT contains an
   array (i.e., the original OBJ was a displaced array) then ITS DATA_SLOT
   is returned, and so on . ..  Just wanted to explain why there could be
   more than one iteration through he loop. */

private Pointer
get_data_vector(obj)
Pointer	obj;
{
	int	type;
	
	while ((type = Type_Code(obj)) == TC_CL_ARRAY)
		obj = header_ref(obj, DATA_SLOT);
	return obj;
}

/* Data_vector_length() returns the number of entries in the array.  Again,
   here we have a procedure switching on the type code.  NOTE:  This returns
   the number of ENTRIES, which isn't the same as the number of words used
   to store the array data. */

private long
data_vector_length(obj)
Pointer	obj;
{
	obj = get_data_vector(obj);
	switch (Type_Code(obj)) {
	case TC_VECTOR:
		return Vector_Length(obj);

	case TC_VECTOR_1B:
		return bit_string_length(obj);

	case TC_CL_IVECTOR:
		return cl_ivector_length(obj);

	case TC_CHARACTER_STRING:
		return string_length(obj);

	default:
		CL_Error("Unknown data vector type: ~A", 1, citsi(Type_Code(obj)));
	}
}

Define_Primitive(prim_vector_length, 1, "cl-vector-length")
{
	Primitive_1_Arg();
        Touch_In_Primitive(Arg1, Arg1);

	if ((!array_like(Arg1)) ||
	    (array_rank(Arg1) > 1))
		CL_Error("~A is not a vector", 1, Arg1);
	if (array_has_fill_pointer(Arg1))
		return cl_fill_pointer(Arg1);
	if (array_is_displaced(Arg1))
	{ Pointer dims =  header_ref(Arg1, DIMS_SLOT);
	  return citsi(User_Vector_Ref(dims, 0));
	}
	return citsi(data_vector_length(Arg1));
}

/* Do the type specific vector reference.  This is always called with
   the data vector of some array. */

private Pointer
data_vector_ref(vector, offset)
Pointer	vector;
long	offset;
{
	switch (Type_Code(vector)) {
	case TC_VECTOR:
		return User_Vector_Ref(vector, offset);

	case TC_VECTOR_1B:
		return bit_string_ref(vector, offset);

	case TC_CL_IVECTOR:
		return cl_ivector_ref(vector, offset);

	case TC_CHARACTER_STRING:
		return c_char_to_scheme_char(string_ref(vector, offset));

	default:
		CL_Error("Unknown data vector type: ~A", 1, citsi(Type_Code(vector)));
	}
}

private Pointer
string_set(str, offset, value)
Pointer	str,
	value;
long	offset;
{
	return *(string_pointer(str, offset)) = scheme_char_to_c_char(value);
}

/* Do the type specific vector set, error checking and all.  Actually,
   either we do the checking specifically here, or the routines we hand
   off to does the checking.  But rest assured - the checking DOES gets
   done. */

private Pointer
data_vector_set(vector, offset, value)
Pointer	vector,
	value;
long	offset;
{
	switch (Type_Code(vector)) {
	case TC_VECTOR:
		return User_Vector_Set(vector, offset, value);

	case TC_VECTOR_1B:
	    {
		int	c_value;

		if ((Type_Code(value) != TC_FIXNUM) ||
		    (((c_value = scheme_to_c_integer(value)) != 0) &&
		     (c_value != 1)))
		  CL_Error("~A is not a BIT", 1, value);
		return bit_string_set(vector, offset, c_value);
	    }

	case TC_CL_IVECTOR:
		return cl_ivector_set(vector, offset, scheme_to_c_integer(value));

	case TC_CHARACTER_STRING:
		if (Type_Code(value) != TC_CHARACTER)
		  CL_Error("~A is not a string character", 1, value);
		return c_char_to_scheme_char(string_set(vector, offset, value));

	default:
		CL_Error("Unknown data vector type: ~A", 1, citsi(Type_Code(vector)));
	}
}

/* Here we go.  The commonlisp make-array.  Here, we do the following
   error checking:
	(1) Dimensional analysis, i.e., make sure the dimensions are
	    OK.

	(2) In the case of a displaced array, make sure that the new
	    array fits in the displaced array, displaced-index-offset
	    and all.

	(3) Make sure fill-pointer is specified only with 1-dimensional
	    arrays.

   We also do the following:
	(1) Allocate the vector, and setup the fill-pointer.

	(2) Do the initial-element filling, if required.

	(3) Return the array.  (This is the key.)

   This is always passed all 8 arguments by the lisp procedure that
   calls us.  The lisp procedure does the rest of the error checking:
	(1) initial-element is of type element-type.

	(2) initial-element is not supplied with initial-contents
	    or displaced-to.

	(3) initial-contents is not supplied with initial-element
	    or displaced-to.

	(4) The displaced-to argument is in fact an array.

	(5) The displaced-to array has the element-type as the
	    new array (that we are building).

   We recognize the following types:
	bit
	string-char
	(mod n)		where n <= 65536
   and everything else gets placed in a general (scheme) vector, which is
   of type T. */

Pointer
do_make_array(dimensions, et, init_element, init_contents, adjustable,
	      fill_ptr, displaced_to, displaced_offset)
Pointer	dimensions, et, init_element, init_contents, adjustable,
	fill_ptr, displaced_to, displaced_offset;
{
	Pointer	data_vector,
		header;
	long	dims[ARRAY_RANK_LIMIT + 1],
		n_dims,
		total_size,
		iv_subtype,
	        c_fill_ptr;

	/* extract the dimension information */
	n_dims = extract_dimensions(dimensions, dims, &total_size);

	/* make sure the fill pointer is reasonable (if one is supplied) */
	if (fill_ptr != NIL) {
		if (n_dims != 1)
		  CL_Error("Fill pointer can be used only with one-dimensional arrays", 0);
		if (fill_ptr == TRUTH)
		  { c_fill_ptr = total_size;
		    fill_ptr = citsi(c_fill_ptr);
		  }
		else {
			if (Type_Code(fill_ptr) != TC_FIXNUM)
				error_wrong_type_arg(6);
			c_fill_ptr = scheme_to_c_integer(fill_ptr);
		}
		if (c_fill_ptr < 0 || c_fill_ptr > total_size)
		  CL_Error("Fill-pointer ~A out of range:[0, ~A]", 2, citsi(c_fill_ptr), 
			                                              citsi(total_size));
	}	

	/* now let's build the data_vector */
	if (displaced_to != NIL) {
		long	d_total_size,
			offset;

		data_vector = displaced_to;
		/* It's a displaced array.  Do some error checking to
		   make sure the new array will fit into the displaced
		   array. */
		d_total_size = data_vector_length(displaced_to);
		if (total_size > d_total_size)
		  CL_Error("Displaced array is too small", 0);
		if (Type_Code(displaced_offset) == TC_FIXNUM)
		{ offset = scheme_to_c_integer(displaced_offset);
		  if (offset < 0)
		    CL_Error("Displaced-index-offset must be positive fixnum: ~A", 1, displaced_offset);
		  if (offset + total_size > d_total_size)
		    CL_Error("Displaced-index-offset is too large for displaced array: ~A", 1, 
			     displaced_offset);
		}
		else if (displaced_offset == NIL)
		{ displaced_offset = Make_Non_Pointer(TC_FIXNUM, 0);
		}
		else
		{ CL_Error("Displaced-index-offset must be a fixnum or NIL: ~A", 1, displaced_offset);
		}
	} else if (et == Sym_string_char) {
		data_vector = allocate_string(total_size);
		if (init_element != NIL) {
			int	fillc;
			fast char	*fillp;
			fast int	nbytes;

			Range_Check(fillc, init_element, 0, 255, ERR_ARG_3_BAD_RANGE);
			fillp = string_pointer(data_vector, 0);
			nbytes = total_size;
			while (--nbytes >= 0)
				*fillp++ = fillc;
		}
	} else if (et == Sym_bit) {
bitstr:		data_vector = allocate_bit_string(total_size);
		if (init_element != NIL)
			fill_bit_string(data_vector, scheme_to_c_integer(init_element));
	} else if (is_cl_ivector_spec(et, &iv_subtype)) {
		long	init = (init_element == NIL) ? 0 : scheme_to_c_integer(init_element);

		if (iv_subtype == 1)		/* (unsigned-byte 1) */
			goto bitstr;		/* pretend et == 'bit */
		data_vector = make_cl_ivector(total_size, iv_subtype, init);
	} else
		data_vector = make_vector(total_size, init_element);

	if (init_contents != NIL)
		do_initial_contents(n_dims, dims + 1, 0, data_vector, init_contents);
	/* Okay, now we have the data vector, and it's initialized
	   according to initial-element or initial-contents.  Now we
	   decide whether we have to create a header or whether we have a
	   simple vector. */
	if ((n_dims == 1) && (adjustable == NIL) && (fill_ptr == NIL) &&
	    (displaced_to == NIL))
		return data_vector;

	header = make_cl_array_header(n_dims, dims);
	header_set(header, ET_SLOT, et);
	header_set(header, FILL_PTR_SLOT, fill_ptr);
	header_set(header, ADJUSTABLE_SLOT, adjustable);
	header_set(header, DISPLACED_OFFSET_SLOT, displaced_offset);
	header_set(header, DATA_SLOT, data_vector);

	return header;
}

Define_Primitive(prim_cl_make_array, -1, "cl-make-array")
{
	Pointer	dimensions, et, init_element, init_contents, adjustable,
		fill_ptr, displaced_to, displaced_offset, header, data_vector;
	Primitive_Variable_Args();

	if (Number_Of_Args < 8)
		error_too_few_args("CL-MAKE-ARRAY");
	else if (Number_Of_Args > 8)
		error_too_many_args("CL-MAKE-ARRY");

	/* snarf the args */
	dimensions = Primitive_Variable_Arg(1);
	Touch_In_Primitive(dimensions, dimensions);
	et = Primitive_Variable_Arg(2);
	Touch_In_Primitive(et, et);
	init_element = Primitive_Variable_Arg(3);
	Touch_In_Primitive(init_element, init_element);
	init_contents = Primitive_Variable_Arg(4);
	Touch_In_Primitive(init_contents, init_contents);
	adjustable = Primitive_Variable_Arg(5);
	Touch_In_Primitive(adjustable, adjustable);
	fill_ptr = Primitive_Variable_Arg(6);
	Touch_In_Primitive(fill_ptr, fill_ptr);
	displaced_to = Primitive_Variable_Arg(7);
	Touch_In_Primitive(displaced_to, displaced_to);
	displaced_offset = Primitive_Variable_Arg(8);
	Touch_In_Primitive(displaced_offset, displaced_offset);

	return do_make_array(dimensions, et, init_element, init_contents,
			     adjustable, fill_ptr, displaced_to,
			     displaced_offset);
}

/* This goes through all the hair of calculating the linear offset
   given the array and indices.  Since array-in-bounds-p has to do
   the same exact thing, aref/aset and array-in-bounds-p sort of
   share this procedure, the only difference being the value of
   the argument IS_BOUNDS_CHECK.  If one of the indices is out of
   bounds and is_bounds_check is true, then -1 is returned.  If
   is_bounds_check is false in this case, and error is signaled.
   Seems reasonable to me. */

private long
calculate_linear_offset(array, n_indices, indices, is_bounds_check)
long	n_indices,
	indices[];
{
	Pointer	dims;
	long	d_total = 1,
		offset = 0,
		n_dims,
		i;

	/* make sure the number of indices matches the number
	   if dimensions */
	n_dims = array_rank(array);
	if (n_dims > n_indices)
	  CL_Error("Too few indices: ~A", 1, citsi(n_indices));
	else if (n_dims < n_indices)
	  CL_Error("Too many indices: ~A", 1, citsi(n_indices));

	/* Now calculate the offset, checking to make sure that
	   each index is < its corresponding dimension.  First
	   check for a simple vector. */
	if (Type_Code(array) != TC_CL_ARRAY) {
		if (indices[1] >= data_vector_length(array)) {
			if (is_bounds_check)
				return -1;
			CL_Error("Index out of range: ~A", 1, citsi(indices[1]));
		}
		offset = indices[1];
	} else {
	    dims = header_ref(array, DIMS_SLOT);

	    /* This starts from the last index and works its
	       way to the first one.  An unfortunate side-affect
	       of this is that if there's more than one subscript
	       out of range, the one that is actually reported is
	       the LAST one instead of the first one.  Sort of
	       unintuitive. */

	    for (i = n_indices; i > 0; i--) {
		long	index_i = indices[i],
			dim_i = (long) Vector_Ref(dims, i);

		if (index_i >= dim_i) {
			if (is_bounds_check)
				return -1;
			CL_Error("Index ~A is out of range: ~A", 2, citsi(i), citsi(index_i));
		}
		offset += (index_i * d_total);
		d_total *= dim_i;
	    }
	}
	return offset;
}

/* Given N_SUBS subcripts passed to a variable argument primitive, a C
   array SUBSCRIPTS, and an OFFSET specifying the argument which is the
   first subscript, gather all the subscripts into SUBSCRIPTS. */

private
gather_argument_subscripts(n_subs, subscripts, offset)
long	n_subs,
	subscripts[],
	offset;
{
	int	i;

	for (i = 1; i <= n_subs; i++) {
		Pointer	sub_i = Primitive_Variable_Arg(i + offset);
		Touch_In_Primitive(sub_i, sub_i);

		if ((Type_Code(sub_i) != TC_FIXNUM) ||
		    (FIXNUM_NEGATIVE_P(sub_i)))
		  CL_Error("Bad subscript: ~A", 1, citsi(sub_i));
		subscripts[i] = scheme_to_c_integer(sub_i);
	}
}

/* Called by the aref() primitive.  The primitive has already gathered
   the N_INDICES indices and passed them in the C array INDICES.  ARRAY
   is the array we're aref'ing. */ 

private Pointer
aref_aux(array, n_indices, indices)
Pointer	array;
long	n_indices,
	indices[];
{
	long	offset,
		disp_offset;
	Pointer	data_vector;

	offset = calculate_linear_offset(array, n_indices, indices, false);

	/* there's no need to make sure the offset plus displacement
	   (if there is any) is in range, since make-array would have
	   made sure that wasn't possible */
	if (Type_Code(array) == TC_CL_ARRAY) {
		disp_offset = header_ref(array, DISPLACED_OFFSET_SLOT);
		if (disp_offset != NIL)
			offset += scheme_to_c_integer(disp_offset);
	}
	data_vector = get_data_vector(array);

	return data_vector_ref(data_vector, offset);
}

Define_Primitive(prim_cl_aref, -1, "cl-aref")
{
	Pointer	array;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs;
	Primitive_Variable_Args();

	if (Number_Of_Args < 1)
		error_too_few_args("CL-AREF");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!array_like(array))
	  CL_Error("Bad argument to aref: ~A not an array", 1, array);
	n_subs = Number_Of_Args - 1;
	gather_argument_subscripts(n_subs, subscripts, 1);
	return aref_aux(array, n_subs, subscripts);
}

Define_Primitive(prim_cl_bit, -1, "cl-bit")
{
	Pointer	array;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs;
	Primitive_Variable_Args();

	if (Number_Of_Args < 1)
		error_too_few_args("CL-BIT");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!array_like(array) || data_vector_type(get_data_vector(array)) != Sym_bit)
	  CL_Error("Bad argument to bit: ~A not a bit-array", 1, array);
	n_subs = Number_Of_Args - 1;
	gather_argument_subscripts(n_subs, subscripts, 1);
	return aref_aux(array, n_subs, subscripts);
}

Define_Primitive(prim_cl_sbit, -1, "cl-sbit")
{
	Pointer	array;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs;
	Primitive_Variable_Args();

	if (Number_Of_Args < 1)
		error_too_few_args("CL-SBIT");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!simple_array(array) || data_vector_type(get_data_vector(array)) != Sym_bit)
	  CL_Error("Bad argument to sbit: ~A not a simple-bit-array", 1, array);
	n_subs = Number_Of_Args - 1;
	gather_argument_subscripts(n_subs, subscripts, 1);
	return aref_aux(array, n_subs, subscripts);
}

private Pointer
aset_aux(array, value, n_indices, indices)
Pointer	array,
	value;
long	n_indices,
	indices[];
{
	long	offset,
		disp_offset;
	Pointer	data_vector;

	offset = calculate_linear_offset(array, n_indices, indices, false);

	/* there's no need to make sure the offset plus displacement
	   (if there is any) is in range, since make-array would have
	   made sure that wasn't possible */
	if (Type_Code(array) == TC_CL_ARRAY) {
		disp_offset = header_ref(array, DISPLACED_OFFSET_SLOT);
		if (disp_offset != NIL)
			offset += scheme_to_c_integer(disp_offset);
	}
	data_vector = get_data_vector(array);

	return data_vector_set(data_vector, offset, value);
}

Define_Primitive(prim_cl_aset, -1, "cl-aset")
{
	Pointer	array,
		value;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs,
		i;
	Primitive_Variable_Args();

	if (Number_Of_Args < 2)
		error_too_few_args("CL-ASET");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!array_like(array))
	  CL_Error("Bad argument to aset: ~A not an array", array);
	value = Primitive_Variable_Arg(Number_Of_Args);
	n_subs = Number_Of_Args - 2;
	gather_argument_subscripts(n_subs, subscripts, 1);

	return aset_aux(array, value, n_subs, subscripts);
}

Define_Primitive(prim_cl_bitset, -1, "cl-bitset")
{
	Pointer	array,
		value;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs,
		i;
	Primitive_Variable_Args();

	if (Number_Of_Args < 2)
		error_too_few_args("CL_BITSET");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!array_like(array) || data_vector_type(get_data_vector(array)) != Sym_bit)
	  CL_Error("Bad argument to bitset: ~A not a bit-array", 1, array);
	value = Primitive_Variable_Arg(Number_Of_Args);
	n_subs = Number_Of_Args - 2;
	gather_argument_subscripts(n_subs, subscripts, 1);

	return aset_aux(array, value, n_subs, subscripts);
}

Define_Primitive(prim_cl_sbitset, -1, "cl-sbitset")
{
	Pointer	array,
		value;
	long	subscripts[ARRAY_RANK_LIMIT + 1],
		n_subs,
		i;
	Primitive_Variable_Args();

	if (Number_Of_Args < 2)
		error_too_few_args("CL-SBITSET");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	if (!simple_array(array) || data_vector_type(get_data_vector(array)) != Sym_bit)
	  CL_Error("Bad argument to sbitset: ~A not a simple-bit-array", 1, array);
	value = Primitive_Variable_Arg(Number_Of_Args);
	n_subs = Number_Of_Args - 2;
	gather_argument_subscripts(n_subs, subscripts, 1);

	return aset_aux(array, value, n_subs, subscripts);
}

/* A bunch of primitives for which the meat of the code is already
   written. */

private
check_is_array_like(array, n)
Pointer	array;
{
	if (!array_like(array))
		error_wrong_type_arg(n);
}

Define_Primitive(prim_cl_array_element_type, 1, "cl-array-element-type")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	return data_vector_type(get_data_vector(Arg1));
}

Define_Primitive(prim_cl_array_rank, 1, "cl-array-rank")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	return citsi(array_rank(Arg1));
}

Define_Primitive(prim_cl_array_dimension, 2, "cl-array-dimension")
{
	long	n_dimensions,
		axis_number;
	Primitive_2_Args();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	if (Type_Code(Arg2) != TC_FIXNUM)
		error_wrong_type_arg(2);
	axis_number = scheme_to_c_integer(Arg2);
	n_dimensions = array_rank(Arg1);
	if (axis_number < 0 || axis_number >= n_dimensions)
	  CL_Error("Axis-dimension ~A out of range [0, ~A]", 2, citsi(axis_number), citsi(n_dimensions - 1));
	if (n_dimensions == 1)
		return citsi(data_vector_length(Arg1));
	else {
		Pointer	dims;

		dims = header_ref(Arg1, DIMS_SLOT);
		return citsi(Vector_Ref(dims, axis_number));
	}
}

Define_Primitive(prim_cl_array_dimensions, 1, "cl-array-dimensions")
{
	long	n_dimensions,
		i;
	Primitive_1_Args();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	n_dimensions = array_rank(Arg1);
	if (n_dimensions == 1)
		return make_cons(citsi(data_vector_length(Arg1)), NIL);
	else {
		Pointer	dims,
			list_head = NIL,
			list_tail = NIL;
		
		dims = header_ref(Arg1, DIMS_SLOT);
		for (i = 1; i <= n_dimensions; i++) {
			Pointer	dim = citsi(Vector_Ref(dims, i)),
				new_cons;

			new_cons = make_cons(dim, NIL);
			if (list_head == NIL)
				list_head = new_cons;
			else
				Vector_Set(list_tail, CONS_CDR, new_cons);
			list_tail = new_cons;
		}
		return list_head;
	}
}

Define_Primitive(prim_cl_array_total_size, 1, "cl-array-total-size")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	return citsi(data_vector_length(get_data_vector(Arg1)));
}

Define_Primitive(prim_cl_array_in_bounds_p, -1, "cl-array-in-bounds-p")
{
	long	n_subs,
		subscripts[ARRAY_RANK_LIMIT];
	Pointer	array;
	Primitive_Variable_Args();

	if (Number_Of_Args < 1)
		error_too_few_args("CL-ARRAY-IN-BOUNDS-P");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	check_is_array_like(array, 1);
	n_subs = Number_Of_Args - 1;
	gather_argument_subscripts(n_subs, subscripts, 1);
	if (calculate_linear_offset(array, n_subs, subscripts, true) < 0)
		return NIL;
	return TRUTH;
}

Define_Primitive(prim_cl_array_row_major_index, -1, "cl-array-row-major-index")
{
	long	n_subs,
		subscripts[ARRAY_RANK_LIMIT];
	Pointer	array;
	Primitive_Variable_Args();

	if (Number_Of_Args < 1)
		error_too_few_args("CL-ARRAY-ROW-MAJOR-INDEX");
	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);
	check_is_array_like(array, 1);
	n_subs = Number_Of_Args - 1;
	gather_argument_subscripts(n_subs, subscripts, 1);
	return citsi(calculate_linear_offset(array, n_subs, subscripts, false));
}

private
array_is_adjustable(array)
Pointer	array;
{
	if ((Type_Code(array) == TC_CL_ARRAY) &&
	    (Vector_Ref(array, ADJUSTABLE_SLOT) != NIL))
		return TRUTH;
	return NIL;
}

Define_Primitive(prim_cl_adjustable_array_p, 1, "cl-adjustable-array-p")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	return array_is_adjustable(Arg1);
}

/* Fill pointers! */

private Pointer
array_has_fill_pointer(array)
Pointer	array;
{
	if ((Type_Code(array) != TC_CL_ARRAY) ||
	    (header_ref(array, FILL_PTR_SLOT) == NIL))
		return NIL;
	return TRUTH;
}

Define_Primitive(prim_cl_array_has_fill_ptr, 1, "cl-array-has-fill-pointer-p")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	return array_has_fill_pointer(Arg1);
}

private Pointer
cl_fill_pointer(array)
Pointer	array;
{
	Pointer	fill_ptr;

	check_is_array_like(array, 1);
	if ((Type_Code(array) != TC_CL_ARRAY) ||
	    ((fill_ptr = header_ref(array, FILL_PTR_SLOT)) == NIL))
	  CL_Error("Array has no fill pointer: ~A", 1, array);

	return fill_ptr;
}


Define_Primitive(prim_cl_fill_pointer, 1, "cl-fill-pointer")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	return cl_fill_pointer(Arg1);
}

Define_Primitive(prim_cl_set_fill_pointer, 2, "cl-set-fill-pointer")
{
	Pointer	fill_ptr;
	long	c_fill_ptr,
		max_fill;
	Primitive_2_Args();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	if ((Type_Code(Arg1) != TC_CL_ARRAY) ||
	    ((fill_ptr = header_ref(Arg1, FILL_PTR_SLOT)) == NIL))
	  CL_Error("Array has no fill pointer: ~A", 1, Arg1);
	if (Type_Code(Arg2) != TC_FIXNUM)
		error_wrong_type_arg(2);
	c_fill_ptr = scheme_to_c_integer(Arg2);
	if (c_fill_ptr < 0 || c_fill_ptr > (max_fill = data_vector_length(Arg1)))
	  CL_Error("Fill-pointer ~A out of range [0, ~A]", 2, Arg2, citsi(max_fill));
	fill_ptr = citsi(c_fill_ptr);
	header_set(Arg1, FILL_PTR_SLOT, fill_ptr);

	return fill_ptr;
}

Pointer
cl_vector_push(Arg1, Arg2)
fast Pointer	Arg1,
		Arg2;
{
	Pointer	fill_ptr;		/* no pun intended */
	long	c_fill_ptr;

	check_is_array_like(Arg2, 2);
	fill_ptr = cl_fill_pointer(Arg2);
	c_fill_ptr = scheme_to_c_integer(fill_ptr);
	if (c_fill_ptr >= data_vector_length(Arg2))
		return NIL;
	data_vector_set(get_data_vector(Arg2), c_fill_ptr, Arg1);

	/* incrementing a scheme integer should work */
	header_set(Arg2, FILL_PTR_SLOT, fill_ptr + 1);

	return fill_ptr;
}

Define_Primitive(prim_cl_vector_push, 2, "cl-vector-push")
{
	Primitive_2_Args();
	Touch_In_Primitive(Arg1, Arg1);

	return cl_vector_push(Arg1, Arg2);
}

Pointer
cl_vector_push_extend(element, vector, extension)
Pointer	element,
	vector,
	extension;
{
	long	old_length,
		new_length;
	Pointer	value,
		old_vector,
		new_vector;

	/* maybe this is simple! */
	value = cl_vector_push(element, vector);
	if (value != NIL || !array_is_adjustable(vector))
		return value;
	/* okay, it's not simple - we've got to extend the array */
	old_vector = get_data_vector(vector);
	old_length = data_vector_length(old_vector);
	new_length = old_length + scheme_to_c_integer(extension);
	/* this call to do_make_array() should return a simple-vector */
	new_vector = do_make_array(citsi(new_length),
				   data_vector_type(old_vector),
				   NIL, NIL, NIL, NIL, NIL, NIL);
	copy_vector_contents(old_vector, new_vector, pointer_overhead(old_vector));
	/* put the new vector in the data slot, and then change the
	   dimension to reflect the new length */
	header_set(vector, DATA_SLOT, new_vector);
	Vector_Set(header_ref(vector, DIMS_SLOT), 1, new_length);
	return cl_vector_push(element, vector);
}

Define_Primitive(prim_cl_vector_push_extend, -1, "cl-vector-push-extend")
{
	Pointer	element,
		vector,
		extension;
	Primitive_Variable_Args();

	if (Number_Of_Args < 2)
	  error_too_few_args("CL-VECTOR-PUSH-EXTEND");
	if (Number_Of_Args > 3)
	  error_too_many_args("CL-VECTOR-PUSH-EXTEND");
	element = Primitive_Variable_Arg(1);
	vector = Primitive_Variable_Arg(2);
	Touch_In_Primitive(vector, vector);
	if (Number_Of_Args == 3)
	{
	  extension = Primitive_Variable_Arg(3);
	  Touch_In_Primitive(extension, extension);
	}
	else
		extension = citsi(DEFAULT_EXTENSION);
	return cl_vector_push_extend(element, vector, extension);
}

Define_Primitive(prim_vector_pop, 1, "cl-vector-pop")
{
	Pointer	fill_ptr,
		value;
	long	c_fill_ptr;
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1, 1);
	fill_ptr = cl_fill_pointer(Arg1);
	c_fill_ptr = scheme_to_c_integer(fill_ptr);
	if (c_fill_ptr == 0)
	  CL_Error("Fill-pointer is 0; cannot vector-pop", 0);
	c_fill_ptr -= 1;
	value = data_vector_ref(get_data_vector(Arg1), c_fill_ptr);
	header_set(Arg1, FILL_PTR_SLOT, fill_ptr - 1);

	return value;
}

array_is_displaced(array)
Pointer	array;
{
	if ((Type_Code(array) == TC_CL_ARRAY) &&
	    ((header_ref(array, DISPLACED_OFFSET_SLOT)) != NIL))
		return TRUTH;
	return NIL;
}


Define_Primitive(prim_array_is_displaced_p, 1, "cl-array-is-displaced-p")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	return array_is_displaced(Arg1);
}

Define_Primitive(prim_cl_arrayp, 1, "cl-arrayp")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	if (array_like(Arg1))
		return TRUTH;
	return NIL;
}

Define_Primitive(prim_cl_find_data_vector, 1, "cl-find-data-vector")
{
	Primitive_1_Arg();
	Touch_In_Primitive(Arg1, Arg1);

	check_is_array_like(Arg1);

	Multiple_Value_Return(1, 2, get_data_vector(Arg1),
			            { Pointer disp_offset;
				      if (simple_array(Arg1))
				      {
					Push(Make_Non_Pointer(TC_FIXNUM, 0));
				      }
				      else
				      {
					disp_offset = header_ref(Arg1, DISPLACED_OFFSET_SLOT);
					if (disp_offset == NIL)
					  Push(Make_Non_Pointer(TC_FIXNUM, 0));
					else
					  Push(disp_offset);
				      }
				      Push(get_data_vector(Arg1));
				    });
}

private Pointer
copy_vector_contents(v_src, v_dest, overhead)
Pointer	v_src,
	v_dest;
{
	fast Pointer	*src,
			*dest;
	fast int	count;

	src = Get_Pointer(v_src) + overhead;
	dest = Get_Pointer(v_dest) + overhead;
	count = Vector_Length(v_src);
	while (--count >= 0)
		*dest++ = *src++;

	return TRUTH;				/* why not? */
}


private Pointer
list_nth_cdr(l, n, status)
fast Pointer	l;
fast long	n,
		*status;
{
	long	save_n = n;

	if (n < 0) {
err:		if (status) {
			*status = ELT_NOT_IN_RANGE;
			return NIL;
		} else
		  CL_Error("Elt: ~A is not a valid index", 1, citsi(save_n));
	}
	if ((n == 0) && (l == NIL)) goto err;
	while (--n >= 0) {
	        if (l == NIL) goto err;
		l = Vector_Ref(l, CONS_CDR);
		if (Type_Code(l) != TC_LIST)
		{ if (Type_Code(l) == TC_FUTURE)
		  { Touch_In_Primitive(l, l);
		  }
		  goto err;
		}
	}
	if (status)
		*status = ELT_OKAY;
	return l;
}

private Pointer
cl_elt(sequence, index, status)
Pointer	sequence;
long	index,
	*status;
{
	Pointer	data_vector;
	long	length;

	if (sequence == NIL || Type_Code(sequence) == TC_LIST) {
		Pointer	value;

		value = list_nth_cdr(sequence, index, status);
		if (status && *status != ELT_OKAY)
			return NIL;
		return Vector_Ref(value, CONS_CAR);
	}
	if (!array_like(sequence) || array_rank(sequence) != 1)
		if (status) {
			*status = ELT_NOT_SEQUENCE;
			return NIL;
		} else
		  CL_Error("Argument not a sequence: ~A", 1, sequence);
	data_vector = get_data_vector(sequence);
	if (array_has_fill_pointer(sequence))
		length = scheme_to_c_integer(cl_fill_pointer(sequence));
	else
		length = data_vector_length(data_vector);
	if (index >= length || index < 0)
		CL_Error("Elt: index ~A is not in the range [0, ~A)", 2, citsi(index), citsi(length));

	if (status)
		*status = ELT_OKAY;
	return data_vector_ref(data_vector, index);
}

private Pointer
cl_elt_set(sequence, index, value, status)
Pointer	sequence,
	value;
long	index,
	*status;
{
	Pointer	data_vector;
	long	length;

	if (sequence == NIL || Type_Code(sequence) == TC_LIST)
		return Vector_Set(list_nth_cdr(sequence, index, status), CONS_CAR, value);
	if (!array_like(sequence) || array_rank(sequence) != 1)
		if (status) {
			*status = ELT_NOT_SEQUENCE;
			return NIL;	/* or something */
		} else
		  CL_Error("Argument not a sequence: ~A", 1, sequence);
	data_vector = get_data_vector(sequence);
	if (array_has_fill_pointer(sequence))
		length = scheme_to_c_integer(cl_fill_pointer(sequence));
	else
		length = data_vector_length(data_vector);
	if (index >= length || index < 0) {
		if (status) {
			*status = ELT_NOT_IN_RANGE;
			return NIL;
		}
		CL_Error("Index ~A is not in the range [0, ~A)", citsi(index), citsi(length));
	}
	if (status)
		*status = ELT_OKAY;
	return data_vector_set(data_vector, index, value);
}

Define_Primitive(prim_cl_elt, 2, "cl-elt")
{
	Primitive_2_Args();
	Touch_In_Primitive(Arg1, Arg1);

	if (Type_Code(Arg2) != TC_FIXNUM)
		error_wrong_type_arg(2);
	return cl_elt(Arg1, scheme_to_c_integer(Arg2), (long *) 0);
}

Define_Primitive(prim_cl_elt_set, 3, "cl-elt-set")
{
	Primitive_3_Args();
	Touch_In_Primitive(Arg1, Arg1);

	if (Type_Code(Arg2) != TC_FIXNUM)
		error_wrong_type_arg(2);
	return cl_elt_set(Arg1, scheme_to_c_integer(Arg2), Arg3, (long *) 0);
}

#undef min
private long
min(a, b)
fast long	a,
		 b;
{
	return (a < b) ? a : b;
}

n_product(numbers, n)
long	numbers[];
{
	fast long	total = 1;

	if (n == 0)
		return 0;
	while (--n >= 0)
		total *= numbers[n];
	return total;
}

private
copy_array_contents(ndims, dims1, dims2, base1, base2, vector1, vector2)
long	*dims1,
	*dims2,
	base1,
	base2;
Pointer	vector1,
	vector2;
{
	long	min_dim = min(*dims1, *dims2),
		dim1_incr = n_product(dims1 + 1, ndims - 1),
		dim2_incr = n_product(dims2 + 1, ndims - 1),
		dim1_offset = 0,
		dim2_offset = 0;
	fast long	i;

	if (ndims == 1) {
		for (i = min_dim; --i >= 0; )
			data_vector_set(vector2, base2++, data_vector_ref(vector1, base1++));
	} else {
		for (i = 0; i < min_dim; i++) {
			copy_array_contents(ndims - 1, dims1 + 1, dims2 + 1,
					    base1 + dim1_offset,
					    base2 + dim2_offset,
					    vector1, vector2);
			dim1_offset += dim1_incr;
			dim2_offset += dim2_incr;
		}
	}
}

Define_Primitive(prim_adjust_array, -1, "cl-adjust-array")
{
	Pointer	array, new_dims, et, init_element, init_contents,
		fill_ptr, displaced_to, displaced_offset,
		new_array;
	Primitive_Variable_Args();

	if (Number_Of_Args < 8)
		error_too_few_args("CL-ADJUST-ARRAY");
	if (Number_Of_Args > 8)
		error_too_many_args("CL-ADJUST-ARRAY");

	array = Primitive_Variable_Arg(1);
	Touch_In_Primitive(array, array);	
	new_dims = Primitive_Variable_Arg(2);
	Touch_In_Primitive(new_dims, new_dims);
	et = Primitive_Variable_Arg(3);
	Touch_In_Primitive(et, et);
	init_element = Primitive_Variable_Arg(4);
	Touch_In_Primitive(init_element, init_element);
	init_contents = Primitive_Variable_Arg(5);
	Touch_In_Primitive(init_contents, init_contents);
	fill_ptr = Primitive_Variable_Arg(6);
	Touch_In_Primitive(fill_ptr, fill_ptr);
	displaced_to = Primitive_Variable_Arg(7);
	Touch_In_Primitive(displaced_to, displaced_to);
	displaced_offset = Primitive_Variable_Arg(8);
	Touch_In_Primitive(displaced_offset, displaced_offset);
	/* make the new array, if possible - do_make_error will
	   error if something's wrong, and we'll never get back
	   here */
	new_array = do_make_array(new_dims, et, init_element,
				  init_contents, TRUTH, fill_ptr,
				  displaced_to, displaced_offset);

	if (displaced_to == NIL && init_contents == NIL)
		copy_array_contents(array_rank(array),
				    (Get_Pointer(header_ref(array, DIMS_SLOT)) + 1),
				    (Get_Pointer(header_ref(new_array, DIMS_SLOT)) + 1),
				    0, 0, get_data_vector(array),
				    get_data_vector(new_array));
	header_set(array, DIMS_SLOT, header_ref(new_array, DIMS_SLOT));
	header_set(array, ET_SLOT, header_ref(new_array, ET_SLOT));
	header_set(array, FILL_PTR_SLOT, header_ref(new_array, FILL_PTR_SLOT));
	header_set(array, ADJUSTABLE_SLOT, header_ref(new_array, ADJUSTABLE_SLOT));
	header_set(array, DISPLACED_OFFSET_SLOT, header_ref(new_array, DISPLACED_OFFSET_SLOT));
	header_set(array, DATA_SLOT, header_ref(new_array, DATA_SLOT));

	return array;
}

private Pointer
init_contents_elt(sequence, i)
Pointer	sequence;
long	i;
{
	Pointer	value;
	long	status;

	value = cl_elt(sequence, i, &status);
	if (status == ELT_OKAY)
		return value;
	switch (status) {
	case ELT_NOT_IN_RANGE:
	  CL_Error("Index ~A is not in range of sequence ~A", citsi(i), sequence);

	case ELT_NOT_SEQUENCE:
	  CL_Error("Initial-contents argument is not a proper sequence: ~A", sequence);
	}
	/* NOTREACHED */
}

do_initial_contents(ndims, dims, base, vector, sequence)
long	ndims,
	*dims,
	base;
Pointer	vector,
	sequence;
{
  long	dim = *dims,
  dim_incr = n_product(dims + 1, ndims - 1),
  dim_offset = 0;
  fast long	i;

  if (ndims == 0)
  {
    data_vector_set(vector, base++, sequence);
  }
  else if (ndims == 1)
  {
    for (i = 0; i < dim; i++)
      data_vector_set(vector, base++,
		      init_contents_elt(sequence, i));
  }
  else
  {
    for (i = 0; i < dim; i++)
    {
      do_initial_contents(ndims - 1, dims + 1,
			  base + dim_offset,
			  vector,
			  init_contents_elt(sequence, i));
      dim_offset += dim_incr;
    }
  }
}
