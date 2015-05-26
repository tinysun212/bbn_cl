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
;;; Primitive support for multiple-values that can also work across continuations.

(declare (usual-integrations))

(define prim-values (make-primitive-procedure 'values))
(define prim-values-list (make-primitive-procedure 'values-list))
(define prim-with-values (make-primitive-procedure 'with-values))
(define cl-call/cc)
(define cl-nr-call/cc)

(in-package continuation-package

(define with-saved-fluid-bindings 
  (make-primitive-procedure 'with-saved-fluid-bindings))

(define (cl-catch-maker catch-primitive restore-state-primitive one-time-only?)
  (let ((tag!! tag-procedure))
    (lambda (receiver)
      (prim-values-list
       (with-saved-fluid-bindings
	(lambda ()
	  (catch-primitive
	   (lambda (control-point)
	     (let ((dynamic-state (current-dynamic-state))
		   (used-up? false))
	       (define (the-raw-continuation . vals)
		 (if used-up?
		     (let loop ()
		       (error "RESTART: Continuation has been destroyed."
			      control-point)
		       (loop)))
		 (set! used-up? one-time-only?)
		 (restore-state-primitive dynamic-state)
		 (within-control-point control-point
				       (lambda () vals)))
	       (prim-with-values (lambda ()
				   (receiver the-raw-continuation))
				 the-raw-continuation))))))))))

(set! cl-nr-call/cc
      (cl-catch-maker primitive-nr-cwcc translate-to-state-point true))

(set! cl-call/cc
      (cl-catch-maker primitive-cwcc translate-to-state-point false))

)
