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

(cl-define substring-equal? (make-primitive-procedure 'substring-equal?))

(cl-define g-vector-alloc (make-primitive-procedure 'g-vector-alloc))
(cl-define g-vector-set! (make-primitive-procedure 'g-vector-set!))
(cl-define g-vector-ref (make-primitive-procedure 'g-vector-ref))
(cl-define g-vector-length (make-primitive-procedure 'g-vector-length))
(cl-define g-vector? (microcode-type-predicate 'G-VECTOR))
(cl-define get-g-vector-subtype (make-primitive-procedure 'get-g-vector-subtype))
(cl-define set-g-vector-subtype! (make-primitive-procedure 'set-g-vector-subtype!))
