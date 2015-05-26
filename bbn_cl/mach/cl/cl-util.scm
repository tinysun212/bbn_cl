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
(export '(rplaca rplacd))

;;;
;;; Definitions for the error functions during build process.
;;; They are replaced later in the build by the more elaborate cl versions.
;;;

(cl-define (cerror continue-format-string error-format-string . args)
  (newline)
  (princ "Continuable error")
  (newline)
  (error continue-format-string error-format-string args))

(cl-define (warn format-string . args)
  (newline)
  (princ ";;; Warning: ")
  (prin1 format-string)
  (princ " ")
  (prin1 args))

(cl-define (commonlisp-nyi form)
  (error "~A in not implemented in ~a version ~a"
	 form
	 (lisp-implementation-type)
	 (lisp-implementation-version)))

(cl-define (rplaca x y) 
  (set-car! x y)
  x)

(cl-define (rplacd x y)
  (set-cdr! x y)
  x)

;; temp while booting

(cl-define nreverse reverse!)

;; Conditional definition printer (users don't see it).

(define *print-defs?* #t)

(cl-define (print-def definer name)
  (if *print-defs?*
      (begin
	(newline)
	(princ ";;; Expanding (")
	(princ definer)
	(princ " ")
	(princ (symbol->string name))
	(princ ")"))))
