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
;;; Here, we replace scheme's error macro with a function since:
;;;  1. We want references to error to pick up cl's more elaborate version when it is defined later
;;;       in the build.
;;;  2. There seems to be no reason that it NEEDS to be a macro, except 
;;;       to pick up current env, which cl does not require.
;;;
;;; Scheme's original error macro is retained, calling it scm-error.
;;;

(syntax-table-shadow *rep-current-syntax-table* 'error)

(syntax-table-shadow system-global-syntax-table 'error)

;;;
;;; Make the error procedure a little more elaborate
;;;

(define (clear-input #!optional stream) #f)

(define error-procedure
  (let ((real-error-procedure (make-primitive-procedure 'error-procedure)))
    (named-lambda (error-procedure message object environment)
      (clear-input)
      (real-error-procedure message object environment))))

(define (error error-format-string . args)
  (error-procedure 
   (apply (access cl-format cl-format-package)
	  `(#f ,error-format-string ,@args))
   *the-non-printing-object*
   (the-environment)))

