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
;;;;;;; This file provides the utilities that allows Commonlisp source
;;;; to be displayed when scheme-translated scode is pretty-printed.
;;;;

;;; Commonlisp source code is attached to scode with COMMENTs.
;;; Two flags control 3 options which are detected when about to
;;; pretty-print a source-code comment form:
;;;   1: Display the full stuff, comment and all
;;;   2: Display only the Commonlisp source code
;;;   3: Display only the scode

#|
(in-package scheme-pretty-printer

  (define (print-comment nodes column depth)
    (let ((tag (first nodes))
	  (info (second nodes)))
      (if (and *detect-commonlisp-source*
	       (list-node? info)
	       (eq? (first (node-subnodes info)) '%commonlisp-source-code))
	  (if *use-commonlisp-source*
	      ; Print the source
	      (print-node (second (node-subnodes info)) column depth)
	      ; Print the scode
	      (print-node (third nodes) column depth))
	  (print-combination nodes column depth))))

  (define (print-list-node node column depth)
    (if (fits-within? node column depth)
	(if (eq? 'COMMENT (car (node-subnodes node)))
	    (print-comment (node-subnodes node) column depth)
	    (print-guaranteed-list-node node))
	(let ((subnodes (node-subnodes node)))
	  ((or (let ((association (assq (car subnodes) dispatch-list)))
		 (and association (cdr association)))
	       print-combination)
	   subnodes column depth))))


  (set! dispatch-list (cons `(COMMENT . ,print-comment)
			    dispatch-list))
)

(define *detect-commonlisp-source* #t)
(define *use-commonlisp-source* #t)

(define (wrap-with-source-code source code)
  (list 'cl-comment
	(list '%commonlisp-source-code source)
	code))

(define (system-pp . args)
  (fluid-let ((*detect-commonlisp-source* #f))
    (apply pp args)))
|#

(in-package debug-package

(define (pretty-print-reduction-function)
  (if-valid-environment (if current-reduction
			    (reduction-environment current-reduction)
			    current-environment)
			(lambda (env)
			  (pp (environment-procedure env))
			  (let ((top-level (find-top-level-procedure env)))
			    (if top-level
				(begin
				  (newline)
				  (princ "This is within the top-level procedure: ")
				  (show top-level)))))))

(define (show procedure-name)
  (let ((sname (unfundefsym procedure-name)))
    (let ((source (get sname '%fun-source)))
      (if source 
	  (pp source 'as-code)
	  (princ sname)))))

(define (find-top-level-procedure env)
  (let ((proc (environment-procedure env)))
    (let ((proc-name (lambda-components (procedure-lambda proc) (lambda args (car args)))))
      (if (and (not (lexical-unreferenceable? *commonlisp-user-environment* proc-name))
	       (eq? (lexical-reference *commonlisp-user-environment* proc-name) proc))
	  proc-name
	  (let ((new-env (procedure-environment proc)))
	    (if (not (null? new-env))
		(find-top-level-procedure new-env)
		'()))))))

(define-debug-command #\P pretty-print-reduction-function
  "Pretty print current procedure and it's top-level parent")

)



	  


  
