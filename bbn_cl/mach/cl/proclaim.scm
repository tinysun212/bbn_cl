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
(export '(proclaim))

(cl-define (proclaim decl-spec)
  ;; Handle specials locally
  (special-proclaim decl-spec)
  ;; Process others via the syntaxer
  (syntaxer-proclaim! decl-spec)
  '()) ; return something nice

;;; Make proclaim be implicit eval-when compile load & eval.
;;; NOTE: We desire this behavior for building the system, but not to export to users.
;;;       Users get this processed at compile-time when at top-level, but not when embedded.
;;;       This is done by the compile-file loop in cl-compile-file.scm.
;;;       This macro def is removed from the syntax table at the end of the build.

(define-macro (proclaim decl-spec)
  `(eval-when (compile load eval)
	      ((symbol-function 'proclaim) ,decl-spec)))

(cl-define (special-proclaim decl-spec)
  (if (and (pair? decl-spec)
	   (eq? (car decl-spec) 'special))
      (mapc (lambda (sym)
	      (if (not (memq sym specials))
		  (set! specials (cons sym specials))))
	    (cdr decl-spec))))

;;;
;;; Syntaxing a declare is an error, since all valid declares should be
;;;   parsed out by parse-body and placed in code as a :declaration form.
;;;

(def-special-form (declare . args)
  (error "Out-of-place declaration: ~a" `(declare ,@args)))
