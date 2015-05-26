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
;;;(declare (usual-integrations))		; *ONE WORD INTEGERS


;
;	Modifications to the syntaxer and file syntaxer.

(define default-declarations		; Default declarations.
  '(declare (usual-integrations)
	    (integrate-primitive-procedures %spawn-process)))

(define *macro-reserved-words*)
(define macro-allow-redefinitions #f)

(define (load-syntaxer)
  (let ((wd (%pwd)))
    (%cd "../sf")
    (load "make.bin")
    (in-package (access package/top-level package/scode-optimizer)

      (define (phase:syntax s-expression #!optional syntax-table)
	(if (or (unassigned? syntax-table) (not syntax-table))
	    (set! syntax-table (*get-default-syntax-table*)))
	(mark-phase "Syntax")
	(syntax* s-expression syntax-table))

      (define (phase:read filename)
	(mark-phase "Read")
	(if default-declarations
	    (cons default-declarations (*read-file* filename))
	    (*read-file* filename))))

    (set! *macro-reserved-words*
	  (map car (caddr user-initial-syntax-table)))

    (deep-fluid-let!)
    (set! sfu? #f)
    (%cd wd)))

;
;	Define a set of macros for more intuitive usage.

(in-package syntaxer-package

(syntax-table-define system-global-syntax-table
    'DEFINE-GLOBAL-MACRO
  (macro (pattern . body)
    (let ((the-macro `(macro ,(cdr pattern) ,@body)))
      (if (or macro-allow-redefinitions
	      (not (memq (car pattern) *macro-reserved-words*))
	      (error
	       "User macro redefining a system special form, check your manual"
	       (car pattern)))
	  `(begin
	     (define-syntax! ,(car pattern) ,the-macro)
	     (syntax-table-define system-global-syntax-table
		 ',(car pattern) ,the-macro)
	     ',(car pattern))
	  'DEFINITION-IGNORED))))

(define syntax-DEFINE-SYNTAX!-form
  (spread-arguments
   (lambda (name value)
     (cond ((symbol? name)
	    (syntax-table-define system-global-syntax-table name
	      (syntax-eval (syntax-expression value)))
	    name)
	   ((and (pair? name) (symbol? (car name)))
	    (syntax-table-define system-global-syntax-table (car name)
	      (let ((transformer
		     (syntax-eval (syntax-NAMED-LAMBDA-form
				   `(NAMED-LAMBDA ,name ,value)))))
		(lambda (expression)
		  (apply transformer (cdr expression)))))
	    (car name))
	   (else (syntax-error "Bad syntax description" name))))))


(define (syntax-DEFINE-MACRO!-form expression)
  (syntax-table-define system-global-syntax-table (caadr expression)
    (macro-spreader (syntax-eval (syntax-NAMED-LAMBDA-form expression))))
  (caadr expression))


(set-car! (cdr system-global-syntax-table)
	  (cons* (cons 'DEFINE-SYNTAX! syntax-DEFINE-SYNTAX!-form)
		 (cons 'DEFINE-MACRO! syntax-DEFINE-MACRO!-form)
		 (cadr system-global-syntax-table)))

)					; End of syntaxer-package patches
