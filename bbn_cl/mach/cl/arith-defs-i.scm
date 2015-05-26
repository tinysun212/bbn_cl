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

(export '(decode-float 
	  scale-float
	  float-digits float-precision float-radix float-sign
	  integer-decode-float))

;;;================
;;;  DECODE-FLOAT
;;;================

;;;Returns float integer float

(cl-define DECODE-FLOAT-SIGN
	(make-primitive-procedure 'DECODE-FLOAT-SIGN))

(cl-define DECODE-FLOAT-MANTISSA
	(make-primitive-procedure 'DECODE-FLOAT-MANTISSA))

(cl-define DECODE-FLOAT-EXPONENT
	(make-primitive-procedure 'DECODE-FLOAT-EXPONENT))


(defun decode-float  (float)
  (check-type float float)
  (values
    (decode-float-mantissa float)
    (decode-float-exponent float)
    (decode-float-sign float)))

;;;================
;;;  SCALE-FLOAT
;;;================

;;;+++This not efficient
;;;+++LAS should write this one
(defun scale-float (float integer)
  (check-type float float)
  (check-type integer integer)
  (* float (expt (float (float-radix float) float) integer)))

;;;================
;;;  FLOAT-RADIX
;;;================
(defun float-radix (f)
  (check-type f float)
  2)

;;;================
;;;  FLOAT-SIGN
;;;================
(defun float-sign (float1 &optional (float2 (float 1 float1)))
  (check-type float1 float)
  (check-type float2 float)
  (if (eq (minusp float1) (minusp float2))
      float2
      (- float2)))


;;;================
;;;  FLOAT-DIGITS
;;;================

;; This should do someting with different types of floats.

(defun float-digits (f)
  (check-type f float)
  %long-float-mantissa-length)

;;;================
;;; FLOAT-PRECISION
;;;================
(defun float-precision (f)
  (check-type f float)
  (if (zerop f)
      0
      (float-digits f)))


;;;================
;;;INTEGER-DECODE-FLOAT
;;;================

;;;Returns integer integer integer

(cl-define INTEGER-DECODE-FLOAT-MANTISSA
	(make-primitive-procedure 'INTEGER-DECODE-FLOAT-MANTISSA))

(cl-define INTEGER-DECODE-FLOAT-EXPONENT
	(make-primitive-procedure 'INTEGER-DECODE-FLOAT-EXPONENT))


(defun integer-decode-float  (float)
  (check-type float float)
  (values
    (INTEGER-DECODE-FLOAT-MANTISSA float)
    (INTEGER-DECODE-FLOAT-EXPONENT float)
    (decode-float-sign float)))



