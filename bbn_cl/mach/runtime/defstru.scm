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
;;;;;;
;;; An implementation of DEFSTRUCT for Scheme
;;; Hacked up by: Seth Steinberg
;;;
;;; (defstruct <struct-name> <options> or <fields>)
;;;
;;;	<options> include: :UNTYPED, :LOCKABLE :CONC-NAME "name-"
;;;
;;;	It defines accessors (type-field object)
;;;		    mutators (set-type-field! object value)
;;;	      if-eq-mutators (set-type-field-if-eq?! object new-value old-value)
;;;		add-mutators (atomic-add-type-field! object increment)
;;;		constructors (make-type field1 value1 field2 value2 ... )
;;;		     lockers (lock-type! object)
;;;	      test & lockers (try-locking-type! object)
;;;		   unlockers (unlock-type! object)
;;;		lock testers (type-locked? object)
;;;		     copiers (copy-type object)
;;;		  predicates (type? object)
;;;
;;; (defstruct-printer type (argument) printing code ... )

(declare (usual-integrations))

(define defstruct-quotify)
(define defstruct-routine)
(define make-defstruct-object)
(define lock-vector!)
(define add-unparser-special-object!
  (access add-unparser-special-object! unparser-package))

(define bfly-defstruct-package
  (make-environment

(define no-arg-options '(:UNTYPED :LOCKABLE))
(define one-arg-options '(:CONC-NAME))

(set! defstruct-quotify
      (named-lambda (defstruct-quotify arguments)
	(let quotify-loop ((item arguments)
			   (result (cons 'defstruct-routine '())))
	  (if (null? item)
	      (begin
		(apply defstruct-routine arguments)
		(reverse result))
	      (cond ((memq (car item) no-arg-options)
		     (quotify-loop (cdr item)
				   (cons (list 'quote (car item)) result)))
		    ((memq (car item) one-arg-options)
		     (if (not (pair? (cdr item)))
			 (error "Missing option argument" (car item)))
		     (quotify-loop (cddr item)
				   (cons (cadr item)
					 (cons (list 'quote (car item))
					       result))))
		    (else
		     (quotify-loop (cdr item)
				   (cons (list 'quote (car item)) result))))))))

(define (parse-defstruct arguments)
  (let ((fields '()) (options '()))
    (let parse-loop ((item arguments))
      (if (null? item)
	  (cons (reverse fields) options)
	  (cond ((not (symbol? (car item)))
		 (error "Invalid defstruct component" (car item)))
		((memq (car item) no-arg-options)
		 (set! options (cons (car item) (cons #t options)))
		 (parse-loop (cdr item)))
		((memq (car item) one-arg-options)
		 (set! options (cons (car item) (cons (cadr item) options)))
		 (parse-loop (cddr item)))
		((char=? (string-ref (symbol->string (car item)) 0) #\:)
		 (error "Unknown option" (car item)))
		(else
		 (set! fields (cons (car item) fields))
		 (parse-loop (cdr item))))))))

(define (default-option option default options)
  (let ((result (memq option options)))
    (if result 
	(cadr result)
	default)))

(set! defstruct-routine
      (named-lambda (defstruct-routine . arguments)
	(let ((parsing (parse-defstruct (cdr arguments)))
	      (struct-name (car arguments)))
	  (let ((field-names (car parsing))
		(options (cdr parsing)))
	    (let ((struct-prefix (default-option ':CONC-NAME
				   (string-append (symbol->string struct-name) "-")
				   options))
		  (field-start 0)
		  (lockable? (memq ':LOCKABLE options))
		  (typed? (not (memq ':UNTYPED options))))
	      (if typed?
		  (set! field-start (1+ field-start)))
	      (if lockable?
		  (set! field-start (1+ field-start)))
	      (defstruct-selectors
		(make-selectors struct-prefix field-names) 
		field-start)
	      (defstruct-mutators
		(make-mutators struct-prefix field-names)
		field-start)
	      (defstruct-mutators-if-eq
		(make-mutators-if-eq struct-prefix field-names)
		field-start)
	      (defstruct-atomic-mutators
		(make-atomic-mutators struct-prefix field-names)
		field-start)
	      (defstruct-constructor
		struct-name field-names
		(if typed? struct-name '())
		field-start)
	      (if lockable?
		  (defstruct-locker struct-name (if typed? 1 0)))
	      (if typed?
		  (defstruct-predicate struct-name))
	      (defstruct-copier struct-name)))
	  struct-name)))

(define (defstruct-locker type-name lock-offset)
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append "LOCK-"
				     (symbol->string type-name) "!"))
      (macro (x)
	`(lock-vector! ,x ,lock-offset)))
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append "TRY-LOCKING-"
				     (symbol->string type-name) "!"))
      (macro (x)
	`(vector-set-if-eq?! ,x ,lock-offset #t #f)))
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append "UNLOCK-"
				     (symbol->string type-name) "!"))
      (macro (x)
	`(vector-set! ,x ,lock-offset #f)))
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append (symbol->string type-name) "-LOCKED?"))
      (macro (x)
	`(vector-ref ,x ,lock-offset))))

(set! lock-vector!
      (named-lambda (lock-vector! vector offset)
	(if (vector-set-if-eq?! vector offset #t #f)
	    'locked
	    (lock-vector! vector offset))))

(define (defstruct-copier type-name)
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append "COPY-" (symbol->string type-name)))
    (macro (x)
      `(vector-copy ,x))))

(define (defstruct-predicate type-marker)
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append (symbol->string type-marker) "?"))
    (macro (x) `(and (vector? ,x)
		     (eq? (vector-ref ,x 0) ',type-marker)))))

(define (make-constructor-alist fields offset)
  (if (null? fields)
      '()
      (cons (cons (car fields) offset)
	    (make-constructor-alist (cdr fields) (1+ offset)))))

(define (defstruct-constructor
	  type-name
	  type-fields
	  type-marker
	  type-overhead)
  (syntax-table-define
      system-global-syntax-table
      (string->symbol (string-append "MAKE-" (symbol->string type-name)))
    (macro x `(make-defstruct-object
	       ',type-name
	       ,(+ (length type-fields) type-overhead)
	       ',type-marker
	       ',(make-constructor-alist type-fields type-overhead)
	       ,(cons 'list (flatten-arglist x))))))

(define (flatten-arglist al)
  (cond ((null? al)
	 '())
	((or (not (pair? al)) (not (pair? (cdr al))))
	 (error "Invalid make form" al))
	(else
	 (cons (list 'quote (car al))
	       (cons (cadr al)
		     (flatten-arglist (cddr al)))))))

(set! make-defstruct-object
      (named-lambda (make-defstruct-object
		      type-name
		      type-size
		      type-marker
		      type-alist
		      type-arglist)
	(let ((result (make-vector type-size '())))
	  (if type-marker
	      (vector-set! result 0 type-marker))
	  (let loop ((arg type-arglist))
	    (if (null? arg)
		result
		(begin
		  (let ((offset (assq (car arg) type-alist)))
		    (if offset
			(vector-set! result (cdr offset) (cadr arg))
			(error "Invalid structure field" type-name (car arg))))
		  (loop (cddr arg))))))))

(define (make-selectors prefix fields)
  (if (null? fields)
      '()
      (cons (string->symbol (string-append prefix
					   (symbol->string (car fields))))
	    (make-selectors prefix (cdr fields)))))

(define (defstruct-selectors selectors offset)
  (if (null? selectors)
      'done
      (begin
	(syntax-table-define
	    system-global-syntax-table
	    (car selectors)
	  (macro (x) `(vector-ref ,x ,offset)))
	(defstruct-selectors (cdr selectors) (1+ offset)))))

(define (make-mutators prefix fields)
  (if (null? fields)
      '()
      (cons (string->symbol (string-append "SET-" prefix
					   (symbol->string (car fields)) "!"))
	    (make-mutators prefix (cdr fields)))))

(define (make-mutators-if-eq prefix fields)
  (if (null? fields)
      '()
      (cons (string->symbol (string-append "SET-" prefix
					   (symbol->string (car fields))
					   "-IF-EQ?!"))
	    (make-mutators-if-eq prefix (cdr fields)))))

(define (make-atomic-mutators prefix fields)
  (if (null? fields)
      '()
      (cons (string->symbol (string-append "ATOMIC-ADD-" prefix
					   (symbol->string (car fields))
					   "!"))
	    (make-atomic-mutators prefix (cdr fields)))))

(define (defstruct-mutators selectors offset)
  (if (null? selectors)
      'done
      (begin
	(syntax-table-define
	    system-global-syntax-table
	    (car selectors)
	  (macro (x y) `(vector-set! ,x ,offset ,y)))
	(defstruct-mutators (cdr selectors) (1+ offset)))))

(define (defstruct-mutators-if-eq selectors offset)
  (if (null? selectors)
      'done
      (begin
	(syntax-table-define
	    system-global-syntax-table
	    (car selectors)
	  (macro (x y) `(vector-set-if-eq?! ,x ,offset ,y)))
	(defstruct-mutators-if-eq (cdr selectors) (1+ offset)))))

(define (defstruct-atomic-mutators selectors offset)
  (if (null? selectors)
      'done
      (begin
	(syntax-table-define
	    system-global-syntax-table
	    (car selectors)
	  (macro (x y) `(atomic-add-vector! ,x ,offset ,y)))
	(defstruct-atomic-mutators (cdr selectors) (1+ offset)))))

))

(syntax-table-define
    system-global-syntax-table
    'DEFINE-A-STRUCTURE
    (macro the-form (defstruct-quotify the-form)))

(syntax-table-define
    system-global-syntax-table
    'DEFINE-A-STRUCTURE-PRINTER
    (macro (type-marker arglist . code)
      `((access add-unparser-special-object! unparser-package)
	',type-marker
	(named-lambda (,(string->symbol
			 (string-append "PRINT-"
					(symbol->string type-marker)))
		       ,(if (pair? arglist) (car arglist) arglist))
	  ,@code))))

'define-a-structure
