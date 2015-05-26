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
;; Chapter 8 -- Macros

(proclaim '(insert-touches nil))

(def-special-form (macrolet bindings . body)
  (fluid-let ((*syntax-time-env* (push-contour *syntax-time-env*)))
    (mapcar define-a-local-macro bindings)
    ;; A let wrapped here insures that we can place decls
    ;; and that we are emitting a fixed-point to the syntax
    ;; function.
    ((access syntax-expression syntaxer-package) `(let () ,@body))))

(define (define-a-local-macro binding)
    (let ((name (car binding))
	  (arglist (cadr binding))
	  (body (cddr binding))
	  (whole 'whole) 
	  (environment 'env))
      (multiple-value-bind
       (body local-decs doc)
       (parse-defmacro
	arglist whole body name
	:environment environment
	:error-string 'defmacro-error-string)
       (let ((macro-entry (cons 'commonlisp-macro
				((access eval system-global-environment)
				 `(lambda (whole env) (block ,name ,body))
				 system-global-environment)))) ; should be *commonlisp-user-environment*
	 (local-assignment *syntax-time-env* (fundefsym name) macro-entry)))))


