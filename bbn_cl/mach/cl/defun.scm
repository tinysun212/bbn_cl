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
;;; Chicken-and-egg problems force us to put defun in its own file. Cluck Cluck Squawk.
;;;

(proclaim '(insert-touches nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Defun             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *save-commonlisp-source* '())

(boot-defmacro defun (name pattern &rest body)
  (print-def "defun" name)
  (prim-with-values
   (lambda () (make-cl-lambda #t name pattern body #f #t #t))
   (lambda (lambda-expr doc fun-name)
     `(defun-internal
	',fun-name 
	(letrec ((,fun-name ,lambda-expr)) ,fun-name)
	',doc
	,(if *save-commonlisp-source*
	     `'(defun ,name ,pattern ,@body)
	     nil)))))

(cl-define (defun-internal name lambda-expr doc source)
 (let ((sname (unfundefsym name)))
   (if (not (lexical-unreferenceable? system-global-environment
				      name))
       (cond ((procedure?
	       (lexical-reference
		system-global-environment
		name))			;already a function?
	      (newline)
	      (princ (string-append
		      ";;; Warning: redefining function "
		      (symbol-name sname))))
	     ((commonlisp-macro? name system-global-environment) ;a macro?
	      (newline)
	      (princ (string-append
		      ";;; Warning: redefining macro "
		      (symbol-name sname)
		      " as a function")))))
   (local-assignment system-global-environment
		     name lambda-expr)
   (if doc (%system-put sname '%fun-documentation doc))
   (%system-put sname 
		'%fun-source
		(if (or (compiled-procedure? lambda-expr)
			(not *save-commonlisp-source*))
		    '()
		    source))
   sname))
