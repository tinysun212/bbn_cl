;;; -*-Scheme-*-
;;;
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Optional Special Forms

(declare (usual-integrations))

(syntax-table-define system-global-syntax-table 'CASE
  (macro (expr . clauses)
    (let ((need-temp? (not (symbol? expr))))
      (let ((the-expression (if need-temp? (generate-uninterned-symbol) expr)))
	(define (process-clauses clauses)
	  (if (null? clauses)
	      '()
	      `((,(let ((selector (caar clauses)))
		    (cond ((eq? selector 'ELSE)
			   (if (null? (cdr clauses))
			       'ELSE
			       (error "CASE SYNTAX: ELSE not last clause"
				      clauses)))
			  ((eq-testable? selector)
			   `(EQ? ,the-expression ',selector))
			  ((not (pair? selector))
			   `(EQV? ,the-expression ',selector))
			  ((check-selector selector)
			   (transform-selector selector 'EQ? 'MEMQ))
			  (else
			   (transform-selector selector 'EQV? 'MEMV))))
		 ,@(cdar clauses))
		,@(process-clauses (cdr clauses)))))

	(define (check-selector selector)
	  (or (null? selector)
	      (and (eq-testable? (car selector))
		   (check-selector (cdr selector)))))

	(define (eq-testable? selector)
	  (or (symbol? selector)
	      (char? selector)		;**** implementation dependent.
	      (eq? selector #F)
	      (eq? selector #T)))

	(define (transform-selector selector eq-name memq-name)
	  ;; Optimized for speed in compiled code.
	  (cond ((null? (cdr selector))
		 `(,eq-name ,the-expression ',(car selector)))
		((null? (cddr selector))
		 `(OR (,eq-name ,the-expression ',(car selector))
		      (,eq-name ,the-expression ',(cadr selector))))
		((null? (cdddr selector))
		 `(OR (,eq-name ,the-expression ',(car selector))
		      (,eq-name ,the-expression ',(cadr selector))
		      (,eq-name ,the-expression ',(caddr selector))))
		(else
		 `(,memq-name ,the-expression ',selector))))

	(let ((body `(COND ,@(process-clauses clauses))))
	  (if need-temp?
	      `(let ((,the-expression ,expr))
		 ,body)
	      body))))))

(syntax-table-define system-global-syntax-table 'LET*
  (macro (bindings . body)
    (define (do-one bindings)
      (if (null? bindings)
	  `(BEGIN ,@body)
	  `(LET (,(car bindings))
	     ,(do-one (cdr bindings)))))
    (do-one bindings)))

(syntax-table-define system-global-syntax-table 'LETREC
  (macro (bindings . body)
    `(LET ()
       ,@(map (lambda (binding) `(DEFINE ,@binding))
	      bindings)
       ,@body)))

(syntax-table-define system-global-syntax-table 'DO
  (macro (bindings test . body)
    (let ((the-name (string->uninterned-symbol "do-loop")))
      `(LET ,the-name
	    ,(map (lambda (binding)
		    (if (or (null? (cdr binding))
			    (null? (cddr binding)))
			binding
			`(,(car binding) ,(cadr binding))))
		  bindings)
	 (IF ,(car test)
	     ,(if (null? (cdr test))
		  `#T
		  `(BEGIN ,@(cdr test)))
	     (BEGIN
	       ,@body
	       (,the-name
		,@(map (lambda (binding)
			 (cond ((or (null? (cdr binding))
				    (null? (cddr binding)))
				(car binding))
			       ((null? (cdddr binding))
				(caddr binding))
			       (else
				(error "DO SYNTAX: Bad binding" binding))))
		       bindings))))))))
