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
;;;; Very compact errset, but isn't it elegant!

;;; First we need a way to evaluate a thunk with a different
;;; error hook

(define with-error-hook)

(in-package error-system
  (set! with-error-hook
	(lambda (new-error-hook thunk)
	  (fluid-let ((*error-hook* new-error-hook))
	    (thunk)))))

;;; Errset just calls a thunk with an error hook that
;;; will print the error message and throw back from the
;;; errset and return the unique error indicator.

(define *the-errset-error-flag* (list '*the-errset-error-flag*))

(define (scheme-errset thunk)
  (call-with-current-continuation
   (lambda (errset-throw)
     (define (cl-valid-error-hook 
	      environment message irritant substitute-environment?)
       (((access make-error-message error-system) message irritant))
       (errset-throw *the-errset-error-flag*))
     (with-error-hook cl-valid-error-hook thunk))))
