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
;;;#| -*-Scheme-*-

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Top Level

(declare (usual-integrations))

;;;; User Interface

(define (integrate/procedure procedure declarations)
  (if (compound-procedure? procedure)
      (procedure-components procedure
	(lambda (*lambda environment)
	  (scode-eval (integrate/scode *lambda declarations false)
		      environment)))
      (error "INTEGRATE/PROCEDURE: Not a compound procedure" procedure)))

(define (integrate/sexp s-expression syntax-table declarations receiver)
  (integrate/simple (lambda (s-expressions)
		      (phase:syntax s-expressions syntax-table))
		    (list s-expression) declarations receiver))

(define (integrate/scode scode declarations receiver)
  (integrate/simple identity-procedure scode declarations receiver))

(in-package system-global-environment
  (define *eval-when-mode* 'evaling))

(define (sf input-string #!optional bin-string spec-string)
  (if (unassigned? bin-string) (set! bin-string false))
  (if (unassigned? spec-string) (set! spec-string false))
  (fluid-let ((*eval-when-mode* 'compiling)
	      (*package* *package*)
	      (*syntax-time-env* (push-contour *syntax-time-env*)))
    (fluid-let ((*syntax-time-global-env* *syntax-time-env*))
      (let ((old-mode (touch-mode)))
	(dynamic-wind
	 (lambda () #f)
	 (lambda ()
	   (syntax-file input-string bin-string spec-string))
	 (lambda ()
	   (syntaxer-proclaim! (list 'insert-touches old-mode))))))))

(define (scold input-string #!optional bin-string spec-string)
  "Use this only for syntaxing the cold-load root file.
Currently only the 68000 implementation needs this."
  (if (unassigned? bin-string) (set! bin-string false))
  (if (unassigned? spec-string) (set! spec-string false))
  (fluid-let ((wrapping-hook wrap-with-control-point))
    (syntax-file input-string bin-string spec-string)))

(define (sf/set-default-syntax-table! syntax-table)
  (if (or (false? syntax-table)
	  (syntax-table? syntax-table))
      (set! default-syntax-table syntax-table)
      (error "Illegal syntax table" syntax-table)))

(define (sf/set-file-syntax-table! pathname syntax-table)
  (pathname-map/insert! file-info/syntax-table
			(pathname/normalize pathname)
			syntax-table))

(define (sf/add-file-declarations! pathname declarations)
  (let ((pathname (pathname/normalize pathname)))
    (pathname-map/insert! file-info/declarations
			  pathname
			  (append! (file-info/get-declarations pathname)
				   (list-copy declarations)))))

(define (file-info/find pathname)
  (let ((pathname (pathname/normalize pathname)))
    (return-2 (pathname-map/lookup file-info/syntax-table
				   pathname
				   identity-procedure
				   (lambda () default-syntax-table))
	      (file-info/get-declarations pathname))))

(define (file-info/get-declarations pathname)
  (pathname-map/lookup file-info/declarations
		       pathname
		       identity-procedure
		       (lambda () '())))

(define (pathname/normalize pathname)
  (pathname-new-version
   (merge-pathnames (pathname->absolute-pathname (->pathname pathname))
		    sf/default-input-pathname)
   false))

(define file-info/syntax-table
  (pathname-map/make))

(define default-syntax-table
  false)

(define file-info/declarations
  (pathname-map/make))

;;;; File Syntaxer

(define sf/default-input-pathname
  (make-pathname false false false false "scm" 'NEWEST))

(define sf/default-externs-pathname
  (make-pathname false false false false "ext" 'NEWEST))

(define sf/output-pathname-type "bin")
(define sf/unfasl-pathname-type "unf")

(define (syntax-file input-string bin-string spec-string)
  (for-each
   (lambda (pathname)
     (let ((input-path (pathname->input-truename pathname)))
       (if (not input-path)
	   (error "SF: File does not exist" pathname))
       (let ((bin-path
	      ((lambda (x) (and x (pathname->absolute-pathname x)))
	       (let ((bin-path
		      (pathname-new-type input-path
					 sf/output-pathname-type)))
		 (if bin-string
		     (merge-pathnames-no-dir-append (->pathname bin-string) bin-path)
		     bin-path)))))
	 (let ((spec-path
	      ((lambda (x) (and x (pathname->absolute-pathname x)))
	       (and (or spec-string sfu?)
		    (let ((spec-path
			   (pathname-new-type bin-path
					      sf/unfasl-pathname-type)))
		      (if spec-string
			  (merge-pathnames-no-dir-append (->pathname spec-string)
							 spec-path)
			  spec-path))))))
	   (syntax-file* input-path bin-path spec-path)))))
   (stickify-input-filenames input-string sf/default-input-pathname)))

(define (syntax-file* input-pathname bin-pathname spec-pathname)
  (let ((start-date (date))
	(start-time (time))
	(input-filename (pathname->string input-pathname))
	(bin-filename (pathname->string bin-pathname))
	(spec-filename (and spec-pathname (pathname->string spec-pathname))))
    (newline)
    (write-string "Syntax file: ")
    (write input-filename)
    (write-string " ")
    (write bin-filename)
    (write-string " ")
    (write spec-filename)
    (transmit-values
     (transmit-values (file-info/find input-pathname)
      (lambda (syntax-table declarations)
	(integrate/file input-pathname syntax-table declarations
			spec-pathname)))
     (lambda (expression externs events)
       (let ((expression-list (separate-expression expression)))
	 (call-with-fasl-output-file bin-pathname
	   (lambda (fd)
	     (fasdump-fd 
	      (wrapping-hook
	       (make-comment `((SOURCE-FILE . ,input-filename)
			       (DATE . ,start-date)
			       (TIME . ,start-time)
			       (FLUID-LET . ,*fluid-let-type*))
			     (car expression-list)))
	      fd)
	     (let loop ((exprs (cdr expression-list)))
	       (if exprs
		   (begin
		     (fasdump-fd (car exprs) fd)
		     (loop (cdr exprs))))))))
       (write-externs-file (pathname-new-type
			    bin-pathname
			    (pathname-type sf/default-externs-pathname))
			   (set! externs false))
       (if spec-pathname
	   (begin (newline)
		  (write-string "Writing ")
		  (write spec-filename)
		  (with-output-to-file spec-pathname
		    (lambda ()
		      (newline)
		      (write `(DATE ,start-date ,start-time))
		      (newline)
		      (write `(FLUID-LET ,*fluid-let-type*))
		      (newline)
		      (write `(SOURCE-FILE ,input-filename))
		      (newline)
		      (write `(BINARY-FILE ,bin-filename))
		      (for-each (lambda (event)
				  (newline)
				  (write `(,(car event)
					   (RUNTIME ,(cdr event)))))
				events)))
		  (write-string " -- done")))))))

(define (separate-expression expression)
  (if (not (scode-sequence? expression))
      (list expression)
      (let ((l (trim-leading-splits (sequence-actions expression))))
	(cond
	 ((null? l)
	  (error "No valid forms in sequence"))
	 ((null? (cdr l))
	  (make-sequence (car l)))	; only one expr
	 (else
	  (let loop ((current l)
		     (orig l)
		     (prev '()))
	    (cond
	     ((null? orig)
	      '())
	     ((null? current)
	      (list (make-sequence orig)))
	     ((file-split-marker? (car current))
	      (let ((remainder (trim-leading-splits (cdr current))))
		(set-cdr! prev '())
		(cons (make-sequence orig)
		      (loop remainder remainder '()))))
	     (else (loop (cdr current) orig current)))))))))

(define (trim-leading-splits l)
  (cond
   ((null? l)
    '())
   ((file-split-marker? (car l))
    (trim-leading-splits (cdr l)))
   (else l)))

(define (file-split-marker? expr)
   (and (symbol? expr)
	(string=? (symbol->string expr) "$SPLIT-FILE")))
	  
(define scode-combination? (access combination? system-global-environment))
(define scode-variable? (access variable? system-global-environment))


(define (read-externs-file pathname)
  (let ((pathname
	 (merge-pathnames (->pathname pathname) sf/default-externs-pathname)))
    (if (file-exists? pathname)
	(fasload pathname)
	(begin (warn "Nonexistent externs file" (pathname->string pathname))
	       '()))))

(define (write-externs-file pathname externs)
  (cond ((not (null? externs))
	 (fasdump externs pathname))
	((file-exists? pathname)
	 (delete-file pathname))))

(define (print-spec identifier names)
  (newline)
  (newline)
  (write-string "(")
  (write identifier)
  (let loop
      ((names
	(sort names
	      (lambda (x y)
		(string<? (symbol->string x)
			  (symbol->string y))))))
    (if (not (null? names))
	(begin (newline)
	       (write (car names))
	       (loop (cdr names)))))
  (write-string ")"))

(define (wrapping-hook scode)
  scode)

(define control-point-tail
  `(3 ,(primitive-set-type (microcode-type 'NULL) (* 4 4))
      () () () () () () () () () () () () () () ()))

(define (wrap-with-control-point scode)
  (system-list-to-vector type-code-control-point
			 `(,return-address-restart-execution
			   ,scode
			   ,system-global-environment
			   ,return-address-non-existent-continuation
			   ,@control-point-tail)))

(define type-code-control-point
  (microcode-type 'CONTROL-POINT))

(define return-address-restart-execution
  (make-return-address (microcode-return 'RESTART-EXECUTION)))

(define return-address-non-existent-continuation
  (make-return-address (microcode-return 'NON-EXISTENT-CONTINUATION)))

;;;; Optimizer Top Level

(define (integrate/file file-name syntax-table declarations compute-free?)
  (integrate/kernel (lambda ()
		      (phase:syntax (phase:read file-name) syntax-table))
		    declarations))

(define (integrate/simple preprocessor input declarations receiver)
  (transmit-values
      (integrate/kernel (lambda () (preprocessor input)) declarations)
    (or receiver
	(lambda (expression externs events)
	  expression))))

(define (integrate/kernel get-scode declarations)
  (fluid-let ((previous-time false)
	      (previous-name false)
	      (events '()))
    (transmit-values
	(transmit-values
	    (transmit-values
		(phase:transform (canonicalize-scode (get-scode) declarations))
	      phase:optimize)
	  phase:generate-scode)
      (lambda (externs expression)
	(end-phase)
	(return-3 expression externs (reverse! events))))))

(define (canonicalize-scode scode declarations)
  (let ((declarations
	 ((access process-declarations syntaxer-package) declarations)))
    (if (null? declarations)
	scode
	(scan-defines (make-sequence
		       (list (make-block-declaration declarations)
			     scode))
		      make-open-block))))

(define (phase:read filename)
  (mark-phase "Read")
  (*read-file* filename))

(define (phase:syntax s-expression #!optional syntax-table)
  (if (or (unassigned? syntax-table) (not syntax-table))
      (set! syntax-table (*get-default-syntax-table*)))
  (mark-phase "Syntax")
  (syntax* s-expression syntax-table))

(define (phase:transform scode)
  (mark-phase "Transform")
  (transform/expression scode))

(define (phase:optimize block expression)
  (mark-phase "Optimize")
  (integrate/expression block expression))

(define (phase:generate-scode operations environment expression)
  (mark-phase "Generate SCode")
  (return-2 (operations->external operations environment)
	    (cgen/expression expression)))

(define previous-time)
(define previous-name)
(define events)

(define (mark-phase this-name)
  (end-phase)
  (newline)
  (write-string "    ")
  (write-string this-name)
  (write-string "...")
  (set! previous-name this-name))

(define (end-phase)
  (let ((this-time (runtime)))
    (if previous-time
	(let ((dt (- this-time previous-time)))
	  (set! events (cons (cons previous-name dt) events))
	  (newline)
	  (write-string "    Time: ")
	  (write dt)
	  (write-string " seconds.")))
    (set! previous-time this-time)))

