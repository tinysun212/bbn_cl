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
;; These procedure stubs allow the automatic integration of primitives
;; to not integrate these scheme primitives.  During the build of
;; common lisp, these will get redefined as commonlisp primitives,
;; and automatic integration will then resume for these primitives.

(define-macro (make-integration-stub x)
  (let ((prim (make-primitive-procedure x)))
  `(cl-define ,x (lambda args (apply ,prim args)))))

(make-integration-stub sqrt)
(make-integration-stub 1+)
(make-integration-stub truncate)
(make-integration-stub ceiling)
(make-integration-stub floor)
(make-integration-stub round)
(make-integration-stub cos)
(make-integration-stub sin)
(make-integration-stub exp)
(make-integration-stub log)

(make-integration-stub make-char)
(make-integration-stub char-code)
(make-integration-stub char-bits)

(make-integration-stub length)
