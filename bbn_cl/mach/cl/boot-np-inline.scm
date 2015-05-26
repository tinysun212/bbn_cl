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
;;;
;;; Procedural code for functions that generate inline
;;;  code without a corresponding primitive.
;;; Each of these has an entry in the compiler, in rtlgen/opncod.scm.
;;;

;;;
;;; This is the bootcl version of np-inline-defs, i.e., 
;;;  no fundefsym, and we let the compiler insert touches
;;;

(define coerce-fixnum-to-flonum
  (let ((coerce-integer-to-flonum (make-primitive-procedure 'coerce-integer-to-flonum)))
    (named-lambda (corece-fixnum-to-flonum x)
      (if (primitive-type? (microcode-type 'fixnum) x)
	  (coerce-integer-to-flonum x)
	  (error "Argument ~a to coerce-fixnum-to-flonum is not a fixnum" x)))))
