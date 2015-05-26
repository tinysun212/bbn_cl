;;;                                ********
;;;
;;; Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
;;; Beranek and Newman Inc.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice and this permission appear in all copies and in
;;; supporting documentation, and that the name Bolt, Beranek and Newman
;;; Inc. not be used in advertising or publicity pertaining to distribution
;;; of the software without specific, written prior permission.  In
;;; addition, BBN makes no respresentation about the suitability of this
;;; software for any purposes.  It is provided "AS IS" without express or
;;; implied warranties including (but not limited to) all implied warranties
;;; of merchantability and fitness.  In no event shall BBN be liable for any
;;; special, indirect or consequential damages whatsoever resulting from
;;; loss of use, data or profits, whether in an action of contract,
;;; negligence or other tortuous action, arising out of or in connection
;;; with the use or performance of this software.
;;; 
;;;                                 ********
;;; 
;;;
(proclaim '(insert-touches nil))

(export '(most-positive-fixnum most-negative-fixnum

	  most-positive-short-float most-negative-short-float
	  most-positive-single-float most-negative-single-float
	  most-positive-double-float  most-negative-double-float
	  most-positive-long-float most-negative-long-float

	  least-positive-short-float least-negative-short-float 
	  least-positive-single-float least-negative-single-float
	  least-positive-double-float least-negative-double-float
	  least-positive-long-float least-negative-long-float

	  short-float-epsilon short-float-negative-epsilon 
	  single-float-epsilon single-float-negative-epsilon 
	  double-float-epsilon double-float-negative-epsilon
	  long-float-epsilon long-float-negative-epsilon

	  pi))

;;;
;;; Implementation-dependent constants
;;;

(defconstant %fixnum-length 24)

(defconstant most-positive-fixnum (-1+ (expt 2 (-1+ %fixnum-length))))
(defconstant most-negative-fixnum (- (expt 2 (-1+ %fixnum-length))))

;;; 
;;; Constants for trig etc.
;;;

(defconstant %short-float-exponent-length 7)
(defconstant %short-float-mantissa-length 55)
(defconstant %single-float-exponent-length 7)
(defconstant %single-float-mantissa-length 55)
(defconstant %double-float-exponent-length 7)
(defconstant %double-float-mantissa-length 55)
(defconstant %long-float-exponent-length 7)
(defconstant %long-float-mantissa-length 55)

(defconstant pi ((make-primitive-procedure 'generic-pi)))


;;;================================================================
;;;12.10 Implementation Parameters
;;;================================================================

;;;+++These are machine dependent

(eval-when (compile load eval)
	   (cl-define float-parameter (make-primitive-procedure 'float-parameter))
)

(defconstant most-positive-short-float    (float-parameter 0 0))
(defconstant most-negative-short-float    (float-parameter 0 1))
(defconstant least-positive-short-float   (float-parameter 0 2))
(defconstant least-negative-short-float   (float-parameter 0 3))
(defconstant short-float-epsilon          (float-parameter 0 4))
(defconstant short-float-negative-epsilon (float-parameter 0 5))

(defconstant most-positive-single-float    (float-parameter 1 0))
(defconstant most-negative-single-float    (float-parameter 1 1))
(defconstant least-positive-single-float   (float-parameter 1 2))
(defconstant least-negative-single-float   (float-parameter 1 3))
(defconstant single-float-epsilon          (float-parameter 1 4))
(defconstant single-float-negative-epsilon (float-parameter 1 5))

(defconstant most-positive-double-float    (float-parameter 2 0))
(defconstant most-negative-double-float    (float-parameter 2 1))
(defconstant least-positive-double-float   (float-parameter 2 2))
(defconstant least-negative-double-float   (float-parameter 2 3))
(defconstant double-float-epsilon          (float-parameter 2 4))
(defconstant double-float-negative-epsilon (float-parameter 2 5))

(defconstant most-positive-long-float    (float-parameter 3 0))
(defconstant most-negative-long-float    (float-parameter 3 1))
(defconstant least-positive-long-float   (float-parameter 3 2))
(defconstant least-negative-long-float   (float-parameter 3 3))
(defconstant long-float-epsilon          (float-parameter 3 4))
(defconstant long-float-negative-epsilon (float-parameter 3 5))
