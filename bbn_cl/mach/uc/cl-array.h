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

#define ARRAY_RANK_LIMIT	16
#define ARRAY_DIMENSION_LIMIT	BIGGEST_FIXNUM
#define ARRAY_TOTAL_SIZE_LIMIT	BIGGEST_FIXNUM

#define DEFAULT_EXTENSION	64	/* seems reasonable to me */

#define LENGTH_SLOT		0
#define DIMS_SLOT		1
#define ET_SLOT			2
#define FILL_PTR_SLOT		3
#define ADJUSTABLE_SLOT		4
#define DISPLACED_OFFSET_SLOT	5
#define DATA_SLOT		6
#define ARRAY_HEADER_SIZE	6

#define ELT_OKAY		0
#define ELT_NOT_SEQUENCE	1
#define ELT_NOT_IN_RANGE	2
