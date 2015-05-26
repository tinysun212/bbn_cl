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
;;;;;;; Patches to standard distribution file ERROR.SCM.157

(in-package error-system
  (declare (usual-integrations)
	   (integrate-primitive-procedures set-fixed-objects-vector!))

;; On error, all other processes are halted.  When PROCEEDed, the
;; processes are started up again.

(set! error-procedure-handler
  (named-lambda (error-procedure message irritant environment)
    (with-tasks-suspended
     (lambda ()
       (with-proceed-point
	proceed-value-filter
	(lambda ()
	  (fluid-let ((error-message message)
		      (error-irritant irritant))
	    (*error-hook* environment message irritant #!FALSE))))))))

(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'ERROR-PROCEDURE)
	     error-procedure-handler)
(set-fixed-objects-vector! (get-fixed-objects-vector))

(define (start-error-rep message irritant)
  (fluid-let ((error-message (if (string? message) message (message)))
	      (error-irritant irritant))
    (let ((environment (continuation-environment (rep-continuation))))
      (with-tasks-suspended
       (lambda ()
	 (if (continuation-undefined-environment? environment)
	     (*error-hook* (rep-environment) error-message irritant #!TRUE)
	     (*error-hook* environment error-message irritant #!FALSE)))))))

(define-total-error-handler 'WRITE-INTO-PURE-SPACE
  (make-error-handler '()
		      '()
		      default-error-handler
		      default-error-handler))

(define-operation-specific-error 'WRITE-INTO-PURE-SPACE
  (list (make-primitive-procedure 'SET-CAR!)
	(make-primitive-procedure 'SET-CDR!)
	(make-primitive-procedure 'VECTOR-SET!)
	(make-primitive-procedure 'SYSTEM-PAIR-SET-CAR!)
	(make-primitive-procedure 'SYSTEM-PAIR-SET-CDR!)
	(make-primitive-procedure 'HUNK3-SET-CXR!)
	(make-primitive-procedure 'SYSTEM-HUNK3-SET-CXR0!)
	(make-primitive-procedure 'SYSTEM-HUNK3-SET-CXR1!)
	(make-primitive-procedure 'SYSTEM-HUNK3-SET-CXR2!)
	(make-primitive-procedure 'SET-CELL-CONTENTS!)
	(make-primitive-procedure 'SYSTEM-VECTOR-SET!))
  "Side-effect on object in pure space"
  combination-first-operand)
)

