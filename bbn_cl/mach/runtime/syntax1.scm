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
;;;;;;; Dispatch 

(define syntax-expression)

(let ()

;;; A very tiny scode walker which tries to determine if 
;;; a piece of scode can ever return a future.  The default
;;; action is to take a look at the syntax-type-of the scode,
;;; but for comment IN-PACKAGE, CONDITIONAL and DISJUNCTION,
;;; we can do better.

(define never-a-future-methods 
  (vector-cons 128
	       (lambda (scode)
		 (let ((t (syntax-type-of scode)))
		   (if (eq? t unknown-keyword)
		       #f
		       (null? (normalize-type-expr `(and future ,t))))))))

(define (set-never! types handler)
  (for-each
   (lambda (type)
     (vector-set! never-a-future-methods (microcode-type type) handler))
   (if (pair? types) types (cons types '()))))

(define (never-a-future scode)
  (let ((handler (vector-ref never-a-future-methods
			     (primitive-type scode))))
    (cond ((eq? handler #t) #t)
	  ((eq? handler #f) #f)
	  (else (handler scode)))))

(define (never-comment-handler scode)
  (never-a-future (comment-expression scode)))

(set-never! 'COMMENT never-comment-handler)

(define (never-in-package-handler scode)
  (never-a-future (in-package-expression scode)))

(set-never! 'IN-PACKAGE never-in-package-handler)

(define (never-conditional-handler scode)
  (and (never-a-future (conditional-consequent scode))
       (never-a-future (conditional-alternative scode))))

(set-never! 'CONDITIONAL never-conditional-handler)

(define (never-disjunction-handler scode)
  (and (never-a-future (disjunction-predicate scode))
       (never-a-future (disjunction-alternative scode))))

(set-never! 'DISJUNCTION never-disjunction-handler)

;;; Touches are inserted automatically for primitive combinations by
;;; the commonlisp optimizers.  We need to check for the scode cases
;;; of conditional and disjunction, which must have touches inserted in
;;; thier predicates.

(define touch-primitive (make-primitive-procedure 'touch))

(define (add-predicate-touch predicate)
  (if (never-a-future predicate)
      predicate
      (let ((p-type (syntax-type-of predicate)))
	(make-cl-combination
	 `(,touch-primitive (syntax-quote ,predicate))
	 (if (eq? p-type unknown-keyword)
	     (list 'non-future #t)
	     (subtract-futures p-type))))))

(define (insert-touch scode type)
  (cond ((not (touch-mode))
	 scode)
	((conditional? scode)
	 (let ((result
		(make-conditional (add-predicate-touch (conditional-predicate scode))
				  (conditional-consequent scode)
				  (let ((alt (conditional-alternative scode)))
				    ; This is needed to handle CONDS
				    (insert-touch alt (syntax-type-of alt))))))
	   (set-scode-type! result type)
	   result))
	((disjunction? scode)
	 (let ((result
		(make-disjunction (add-predicate-touch (disjunction-predicate scode))
				  (disjunction-alternative scode))))
	   (set-scode-type! result type)
	   result))
	(else
	 scode)))

(define (check-if-ignored expression)
  (let ((ignored (lookup-attribute expression ignore-keyword)))
    (if (and (not (eq? ignored unknown-keyword)) ignored)
	(begin
	  (warn "Variable ~A is referenced but is declared ignored." expression)))))

(define (needs-special-access var)
  (and (shadowed? var)
       (special? var)))

(define (syntax-symbol expression required-type)
  (if *process-declarations*
      (begin
	(check-if-ignored expression)
	(cond ((get expression '%constant)
	       (let ((value (lexical-reference *commonlisp-user-environment* expression)))
		 (if (eq? required-type unknown-keyword)
		     value
		     (begin 
		       (if (not (typep value required-type))
			   (warn "The defconstant ~A has value ~A but is being declared to be of type ~A"
				 expression value required-type))
		       value))))
	      ((needs-special-access expression)
	       (syntax-expression `(access ,expression *commonlisp-user-environment*) required-type))
	      (else
	       (let ((result (make-variable expression)))
		 (if (not (eq? required-type unknown-keyword))
		     (set-scode-type! result required-type))
		 result))))
      (make-variable expression)))

;;; All USER expressions should go through here -- thus the calls
;;;  to make-possibly-lr-combination: if the (lr-evaluation-mode?) is on, we get lr even for
;;;  scheme code -- this allows a lambda in the car of a form
;;;  to be lr, and we get lr when *process-declarations* is off.
;;; If user code can get to other make-combination calls in the system,
;;;  we may have a bug in that lr order will not be preserved.

(define (syntax-pair expression required-type)
  (cond ((symbol? (car expression))
	 (let ((fcn-sym (*get-fcn-name* (car expression))))
	   (if (commonlisp-macro? fcn-sym *syntax-time-env*)
	       (syntax-expression
		((lexical-reference system-global-environment (*get-fcn-name* 'macroexpand)) 
		 expression
		 *syntax-time-env*))
	       (let ((quantum (syntax-table-ref syntax-table (car expression))))
		 (if quantum
		     (if *process-declarations*
			 (fluid-let ((saved-keyword (car expression)))
			   (let* ((result (quantum expression))
				  (type (syntax-type-of result))
				  (processed-result (insert-touch result type)))
			     (cond ((eq? required-type unknown-keyword)
				    processed-result)
				   ((eq? type unknown-keyword)
				    (set-scode-type! processed-result required-type)
				    result)
				   (else processed-result))))
			 (fluid-let ((saved-keyword (car expression)))
			   (quantum expression)))
		     (if *process-declarations*
			 (make-cl-combination expression required-type)
			 (make-possibly-lr-combination (make-variable fcn-sym)
						       (syntax-expressions (cdr expression)))))))))
	((and *process-declarations*
	      (primitive-procedure? (car expression)))
	 (make-cl-combination expression required-type))
	((and (pair? (car expression))
	      (eq? (car (car expression)) 'no-fundefsym))
	 (make-typed-combination required-type
				 (car expression)
				 (syntax-expressions (cdr expression))))
	(else
	 (make-possibly-lr-combination (syntax-expression (car expression))
				       (syntax-expressions (cdr expression))))))

(set! syntax-expression
      (named-lambda (syntax-expression expression #!optional required-type)
	(if (unassigned? required-type)
	    (set! required-type unknown-keyword))
	(cond ((symbol? expression)
	       (if *process-declarations*
		   (let ((declared-type (declared-variable-type expression)))
		     (cond ((eq? required-type unknown-keyword)
			    (syntax-symbol expression declared-type))
			   ((eq? declared-type unknown-keyword)
			    (syntax-symbol expression required-type))
			   (else
			    (if (not (or (subtypep required-type declared-type)
					 (subtypep declared-type required-type)))
				(warn "~A is declared to be of type ~A but is required~%to be of type ~A. ~
                                       Types are possibly disjoint."
				      expression declared-type required-type))
			    (syntax-symbol expression required-type))))
		   (syntax-symbol expression required-type)))
	      ((pair? expression)
	       (syntax-pair expression required-type))
	      (else
	       (if (and *process-declarations*
			(not (eq? required-type unknown-keyword))
			(not (typep expression required-type)))
		   (warn "~A is being required to be of type ~A"
			 expression required-type))
	       expression))))
)

(define (syntax-expressions expressions)
  (if (null? expressions)
      '()
      (if (pair? expressions)
	  (cons (syntax-expression (car expressions))
		(syntax-expressions (cdr expressions)))
	  (error "Bad expression list syntax"))))

(define ((spread-arguments kernel) expression)
  (apply kernel (cdr expression)))

(define saved-keyword
  (make-interned-symbol ""))

(define (syntax-error message . irritant)
  (error (string-append message
			": "
			(symbol->string saved-keyword)
			" SYNTAX")
	 (cond ((null? irritant) *the-non-printing-object*)
	       ((null? (cdr irritant)) (car irritant))
	       (else irritant))))

(define (syntax-sequence subexpressions)
  (cond ((null? subexpressions)
	 (syntax-error "No subforms in sequence"))
	((and *process-declarations*
	      (commonlisp-declaration? (car subexpressions)))
	 (if *decl-debug* (print `("Setting up decls:" ,(car subexpressions))))
	 (setup-decls! (car subexpressions) 'declare)
	 (make-optimized-sequence 
	  (syntax-sequentially (cdr subexpressions))))
	(else (make-optimized-sequence (syntax-sequentially subexpressions)))))

(define (syntax-sequentially expressions)
  (if (null? expressions)
      '()
      ;; force eval order.
      (let ((first (syntax-expression (car expressions))))
	(cons first
	      (syntax-sequentially (force-cdr expressions))))))

;;;
;;; Force-cdr -- will force cdr of list if is a delay object
;;;		 so that incremental side effects during the
;;;		 read can take place.

;;; [Temp fix: senses procedure in addition to delay
;;;   so that compiled code will work. Can be removed when 
;;;   compiler generates right stuff for delay -las 9/15/87]

(define (force-cdr x)
  (let ((y (cdr x)))
    (cond
     ((delayed? y) (force y))
     ((procedure? y) (y))
     (else y))))

(define (syntax-bindings bindings receiver)
  (cond ((null? bindings)
	 (receiver '() '()))
	((and (pair? (car bindings))
	      (symbol? (caar bindings)))
	 (syntax-bindings (cdr bindings)
	   (lambda (names values)
	     (receiver (cons (caar bindings) names)
		       (cons (expand-binding-value (cdar bindings)) values)))))
	(else
	 (syntax-error "Badly-formed binding" (car bindings)))))

;;;; Expanders

(define (expand-access chain cont)
  (if (symbol? (car chain))
      (cont (if (null? (cddr chain))
		(syntax-expression (cadr chain))
		(expand-access (cdr chain) make-access))
	    (car chain))
      (syntax-error "Non-symbolic variable" (car chain))))

(define (expand-binding-value rest)
  (cond ((null? rest) unassigned-object)
	((null? (cdr rest)) (syntax-expression (car rest)))
	(else (syntax-error "Too many forms in value" rest))))

(define expand-conjunction
  (let ()
    (define (expander forms)
      (if (null? (cdr forms))
	  (syntax-expression (car forms))
	  (make-conjunction (syntax-expression (car forms))
			    (expander (cdr forms)))))
    (named-lambda (expand-conjunction forms)
      (if (null? forms)
	  true
	  (expander forms)))))

(define expand-disjunction
  (let ()
    (define (expander forms)
      (if (null? (cdr forms))
	  (syntax-expression (car forms))
	  (make-disjunction (syntax-expression (car forms))
			    (expander (cdr forms)))))
    (named-lambda (expand-disjunction forms)
      (if (null? forms)
	  false
	  (expander forms)))))

(define (expand-lambda pattern actions receiver)
  (define (loop pattern body)
    (if (pair? (car pattern))
	(loop (car pattern)
	      `((lambda ,(cdr pattern) ,@body)))
	(receiver pattern body)))
  ((if (pair? pattern) loop receiver) pattern actions))

(define (syntax-lambda-body body)
  (syntax-sequence
   (if (and (not (null? body))
	    (not (null? (cdr body)))
	    (string? (car body)))
       (cdr body)		;discard documentation string.
       body)))

'$split-file

;;;; Quasiquote

(define expand-quasiquote)
(define expand-quasiquote-no-syntax)
(let ()

(define (descend-quasiquote x level return)
  (cond ((pair? x) (descend-quasiquote-pair x level return))
	((vector? x) (descend-quasiquote-vector x level return))
	(else (return 'QUOTE x))))

(define (descend-quasiquote-pair x level return)
  (define (descend-quasiquote-pair* level)
    (descend-quasiquote (car x) level
      (lambda (car-mode car-arg)
	(descend-quasiquote (cdr x) level
	  (lambda (cdr-mode cdr-arg)
	    (cond ((and (eq? car-mode 'QUOTE)
			(eq? cdr-mode 'QUOTE))
		   (return 'QUOTE x))
		  ((eq? car-mode 'UNQUOTE-SPLICING)
		   (if (and (eq? cdr-mode 'QUOTE)
			    (null? cdr-arg))
		       (return 'UNQUOTE car-arg)
		       (return (system 'APPEND)
			       (list car-arg
				     (finalize-quasiquote cdr-mode cdr-arg)))))
		  ((and (eq? cdr-mode 'QUOTE)
			(null? cdr-arg))
		   (return 'LIST
			   (list (finalize-quasiquote car-mode car-arg))))
		  ((and (eq? cdr-mode 'QUOTE)
			(list? cdr-arg))
		   (return 'LIST
			   (cons (finalize-quasiquote car-mode car-arg)
				 (map (lambda (el)
					(finalize-quasiquote 'QUOTE el))
				      cdr-arg))))
		  ((memq cdr-mode '(LIST CONS))
		   (return cdr-mode
			   (cons (finalize-quasiquote car-mode car-arg)
				 cdr-arg)))
		  (else
		   (return
		    'CONS
		    (list (finalize-quasiquote car-mode car-arg)
			  (finalize-quasiquote cdr-mode cdr-arg))))))))))
  (case (car x)
    ((QUASIQUOTE) (descend-quasiquote-pair* (1+ level)))
    ((UNQUOTE UNQUOTE-SPLICING)
     (if (zero? level)
	 (return (car x) (cadr x))
	 (descend-quasiquote-pair* (- level 1))))
    (else (descend-quasiquote-pair* level))))

(define (descend-quasiquote-vector x level return)
  (descend-quasiquote (vector->list x) level
    (lambda (mode arg)
      (case mode
	((QUOTE)
	 (return 'QUOTE x))
	((LIST)
	 (return (system 'VECTOR) arg))
	(else
	 (return (system 'LIST->VECTOR)
		 (list (finalize-quasiquote mode arg))))))))

(define (finalize-quasiquote mode arg)
  (case mode
    ((QUOTE) `',arg)
    ((UNQUOTE) arg)
    ((UNQUOTE-SPLICING) (error ",@ in illegal context" arg))
    ((LIST) `(,(system 'LIST) ,@arg))
    ((CONS)
     (if (= (length arg) 2)
	 `(,(system 'CONS) ,@arg)
	 `(,(system 'CONS*) ,@arg)))
    (else `(,mode ,@arg))))

(define (system name)
  `(ACCESS ,name #F))

(set! expand-quasiquote
  (named-lambda (expand-quasiquote expression)
    (syntax-expression 
     (expand-quasiquote-no-syntax expression))))

(set! expand-quasiquote-no-syntax
  (named-lambda (expand-quasiquote-no-syntax expression)
    (descend-quasiquote expression 0 finalize-quasiquote)))

)

;;;; Basic Syntax

;;; SYNTAX-QUOTE allows us to mark an object in a list to be syntaxed as already
;;; syntaxed.  This solved the problem that scode is not a fixed-point of the
;;; syntaxer.  Namely, syntaxing 'A gives us A, but then resyntaxing A gives
;;; us <Variable A>.

(define syntax-SYNTAX-QUOTE-form
  (spread-arguments
   (lambda (expr)
     expr)))

;;; (LR-COMBINATION <expr>)
;;;
;;; If <expr> syntaxes to a combo, then remake the combo using make-lr-combo,
;;;  to force left-to-right eval order (of the top-level combo only).
;;;  This is so we can selectively force lr order, e.g., in CL binding forms.

(define syntax-LR-COMBINATION-form
  (spread-arguments
   (lambda (expr)
     (let ((scode (syntax-expression expr)))
       (if (and (not (lr-evaluation-mode?))
		(combination? scode))
	   (make-lr-combination (combination-operator scode)
				(combination-operands scode))
	   scode)))))

(define syntax-SCODE-QUOTE-form
  (spread-arguments
   (lambda (expression)
     (make-quotation (syntax-expression expression)))))

(define syntax-QUOTE-form
  (spread-arguments identity-procedure))

(define syntax-THE-ENVIRONMENT-form
  (spread-arguments make-the-environment))

(define syntax-UNASSIGNED?-form
  (spread-arguments
   (lambda (name)
     (if *process-declarations*
	 (let ((result (make-unassigned? name)))
	   (set-scode-type! result '(member #t #f))
	   result)
	 (make-unassigned? name)))))

(define syntax-UNBOUND?-form
  (spread-arguments
   (lambda (name)
     (if *process-declarations*
	(let ((result (make-unbound? name)))
	   (set-scode-type! result '(member #t #f))
	   result)
	 (make-unbound? name)))))

(define syntax-ACCESS-form
  (spread-arguments
   (lambda chain
     (expand-access chain make-access))))

(define syntax-SET!-form
  (spread-arguments
   (lambda (name . rest)
     (let ((target (syntax-expression name)))
       (if (and *process-declarations*
		(or (and (symbol? name)
			 (or (keyword? name)
			     (get name '%constant)))
		    (and (symbol? target)
			 (or (keyword? target)
			     (get target '%constant)))
		    (eq? target #f)
		    (eq? target #t)))
	 (error "Attempt to side-effect constant" name))
       ((invert-expression target)
	(expand-binding-value rest))))))

(define syntax-DEFINE-form
  (spread-arguments
   (lambda (pattern . rest)
     (cond ((symbol? pattern)
	    (make-definition pattern
			     (expand-binding-value
			      (if (and (= (length rest) 2)
				       (string? (cadr rest)))
				  (list (car rest))
				  rest))))
	   ((pair? pattern)
	    (expand-lambda pattern rest
	      (lambda (pattern body)
		(make-definition (car pattern)
				 (make-named-lambda (car pattern) (cdr pattern)
						    (syntax-lambda-body body))))))
	   (else
	    (syntax-error "Bad pattern" pattern))))))

(define syntax-SEQUENCE-form
  (spread-arguments
   (lambda actions
     (syntax-sequence actions))))

(define syntax-IN-PACKAGE-form
  (spread-arguments
   (lambda (environment . body)
     (make-in-package (syntax-expression environment)
		      (syntax-sequence body)))))

(define syntax-DELAY-form
  (spread-arguments
   (lambda (expression)
     (make-delay (syntax-expression expression)))))

(define syntax-CONS-STREAM-form
  (spread-arguments
   (lambda (head tail)
     (make-combination* cons
			(syntax-expression head)
			(make-delay (syntax-expression tail))))))

;;;; Conditionals

(define syntax-IF-form
  (spread-arguments
   (lambda (predicate consequent . rest)
     (make-conditional (syntax-expression predicate)
		       (syntax-expression consequent)
		       (cond ((null? rest) undefined-conditional-branch)
			     ((null? (cdr rest))
			      (syntax-expression (car rest)))
			     (else
			      (syntax-error "Too many forms" (cdr rest))))))))

(define syntax-CONJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-conjunction forms))))

(define syntax-DISJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-disjunction forms))))

(define syntax-COND-form
  (let ()
    (define (process-cond-clauses clause rest)
      (cond ((eq? (car clause) 'ELSE)
	     (if (null? rest)
		 (syntax-sequence (cdr clause))
		 (syntax-error "ELSE not last clause" rest)))
	    ((null? (cdr clause))
	     (make-disjunction (syntax-expression (car clause))
			       (if (null? rest)
				   undefined-conditional-branch
				   (process-cond-clauses (car rest)
							 (cdr rest)))))
	    ((and (pair? (cdr clause))
		  (eq? (cadr clause) '=>))
	     (syntax-expression
	      `((ACCESS COND-=>-HELPER SYNTAXER-PACKAGE '())
		,(car clause)
		(LAMBDA () ,@(cddr clause))
		(LAMBDA ()
		  ,(if (null? rest)
		       undefined-conditional-branch
		       `(COND ,@rest))))))
	    (else
	     (make-conditional (syntax-expression (car clause))
			       (syntax-sequence (cdr clause))
			       (if (null? rest)
				   undefined-conditional-branch
				   (process-cond-clauses (car rest)
							 (cdr rest)))))))
    (spread-arguments
     (lambda (clause . rest)
       (process-cond-clauses clause rest)))))

(define (cond-=>-helper form1-result thunk2 thunk3)
  (if form1-result
      ((thunk2) form1-result)
      (thunk3)))

(define syntax-LAMBDA-form
  (spread-arguments
   (lambda (pattern . body)
     (parse-lambda-list pattern
       (lambda (req opt rest)
	 (if *decl-debug* (print `("Syntaxing lambda:" ,req ,opt ,rest ,body)))
	 (with-shadowed-variables
	  (append req opt rest)
	  (lambda ()
	    (if *process-declarations*
		(let ((decls (get-body-decls (car body)))
		      (real-body (get-body-body (car body))))
		  (if decls
		      (let ((unquoted-decls (get-unquoted-decls decls)))
			(let ((bvl (get-bvl unquoted-decls))
			      (linearized-decls (linearize-decls (get-list-of-decls unquoted-decls))))
			  (make-lambda pattern
				       (syntax-lambda-body
					(add-normal-declarations linearized-decls bvl real-body)))))
		      (make-lambda pattern (syntax-lambda-body body))))
		(make-lambda pattern (syntax-lambda-body body))))))))))

(define syntax-NAMED-LAMBDA-form
  (spread-arguments
   (lambda (pattern . body)
     (expand-lambda pattern body
      (lambda (pattern body)
	(parse-lambda-list pattern
	 (lambda (req opt rest)
	   (if *decl-debug*
	       (print `("Syntaxing named-lambda:" ,req ,opt ,rest ,body)))
	   (with-shadowed-variables
	    (append req opt rest)
	    (lambda ()
	      (if *process-declarations*
		  (let ((decls (get-body-decls (car body)))
			(real-body (get-body-body (car body)))
			(real-name (if (%function-symbol? (car pattern))
				       (unfundefsym (car pattern))
				       (car pattern))))
		    (if decls
			(let ((unquoted-decls (get-unquoted-decls decls)))
			  (let ((bvl (get-bvl unquoted-decls)))
			    (let ((ordered-linearized-decls
				   (order-decls bvl 
						(linearize-decls
						 (get-list-of-decls unquoted-decls)))))
			      (if (ok-to-add-function-declaration?
				   ordered-linearized-decls real-name)
				  (make-named-lambda 
				   (car pattern) (cdr pattern)
				   (syntax-lambda-body
				    (add-function-declaration
				     ordered-linearized-decls bvl real-name real-body)))
				  (make-named-lambda 
				   (car pattern) (cdr pattern)
				   (syntax-lambda-body
				    (add-normal-declarations
				     ordered-linearized-decls bvl real-body)))))))
			(make-named-lambda (car pattern) (cdr pattern)
					   (syntax-lambda-body body))))
		  (make-named-lambda (car pattern) (cdr pattern)
				     (syntax-lambda-body body))))))))))))

(define syntax-LET-form
  (spread-arguments
   (lambda (name-or-pattern pattern-or-first . rest)
     (if (symbol? name-or-pattern)
	 (syntax-bindings pattern-or-first
	   (lambda (names values)
	     (make-letrec (list name-or-pattern)
			  (list (make-named-lambda name-or-pattern names
						   (syntax-sequence rest)))
			  (make-combination (make-variable name-or-pattern)
					    values))))
	 (syntax-bindings name-or-pattern
	   (lambda (names values)
	     (make-closed-block
	      lambda-tag:let names values
	      (syntax-sequence (cons pattern-or-first rest)))))))))

(define syntax-MAKE-ENVIRONMENT-form
  (spread-arguments
   (lambda body
     (make-closed-block
      lambda-tag:make-environment '() '()
      (if (null? body)
	  the-environment-object
	  (make-sequence* (syntax-sequence body) the-environment-object))))))

;;;; Syntax Extensions

(define syntax-LET-SYNTAX-form
  (spread-arguments
   (lambda (bindings . body)
     (syntax-bindings bindings
       (lambda (names values)
	 (fluid-let ((syntax-table
		      (extend-syntax-table
		       (map (lambda (name value)
			      (cons name (syntax-eval value)))
			    names
			    values)
		       syntax-table)))
	   (syntax-sequence body)))))))

(define syntax-USING-SYNTAX-form
  (spread-arguments
   (lambda (table . body)
     (let ((table* (syntax-eval (syntax-expression table))))
       (if (not (syntax-table? table*))
	   (syntax-error "Not a syntax table" table))
       (fluid-let ((syntax-table table*))
	 (syntax-sequence body))))))

(define syntax-DEFINE-SYNTAX-form
  (spread-arguments
   (lambda (name value)
     (cond ((symbol? name)
	    (syntax-table-define syntax-table name
	      (syntax-eval (syntax-expression value)))
	    name)
	   ((and (pair? name) (symbol? (car name)))
	    (syntax-table-define syntax-table (car name)
	      (let ((transformer
		     (syntax-eval (syntax-NAMED-LAMBDA-form
				   `(NAMED-LAMBDA ,name ,value)))))
		(lambda (expression)
		  (apply transformer (cdr expression)))))
	    (car name))
	   (else (syntax-error "Bad syntax description" name))))))

(define (syntax-MACRO-form expression)
  (make-combination* (make-absolute-reference 'MACRO-SPREADER)
		     (syntax-LAMBDA-form expression)))

(define (syntax-DEFINE-MACRO-form expression)
  (syntax-table-define syntax-table (caadr expression)
    (macro-spreader (syntax-eval (syntax-NAMED-LAMBDA-form expression))))
  (caadr expression))

(set! macro-spreader
  (named-lambda ((macro-spreader transformer) expression)
    (syntax-expression (apply transformer (cdr expression)))))

;;;; Grab Bag

(define (syntax-ERROR-LIKE-form procedure-name)
  (spread-arguments
   (lambda (message . rest)
     (make-combination* (make-absolute-reference procedure-name)
			(syntax-expression message)
			(cond ((null? rest)
			       (make-absolute-reference
				'*THE-NON-PRINTING-OBJECT*))
			      ((null? (cdr rest))
			       (syntax-expression (car rest)))
			      (else
			       (make-combination
				(make-absolute-reference 'LIST)
				(syntax-expressions rest))))
			(make-the-environment)))))

(define syntax-ERROR-form
  (syntax-ERROR-LIKE-form 'ERROR-PROCEDURE))

(define syntax-BKPT-form
  (syntax-ERROR-LIKE-form 'BREAKPOINT-PROCEDURE))

(define syntax-QUASIQUOTE-form
  (spread-arguments expand-quasiquote))

;;;; FLUID-LET

(define syntax-FLUID-LET-form-shallow
  (let ()

    (define (syntax-fluid-bindings bindings receiver)
      (if (null? bindings)
	  (receiver '() '() '() '())
	  (syntax-fluid-bindings (cdr bindings)
	    (lambda (names values transfers-in transfers-out)
	      (let ((binding (car bindings)))
		(if (pair? binding)
		    (let ((transfer
			   (let ((reference (syntax-expression (car binding))))
			     (let ((assignment (invert-expression reference)))
			       (lambda (target source)
				 (make-assignment
				  target
				  (assignment
				   (make-assignment source
						    unassigned-object)))))))
			  (value (expand-binding-value (cdr binding)))
			  (inside-name
			   (string->uninterned-symbol "INSIDE-PLACEHOLDER"))
			  (outside-name
			   (string->uninterned-symbol "OUTSIDE-PLACEHOLDER")))
		      (receiver (cons* inside-name outside-name names)
				(cons* value unassigned-object values)
				(cons (transfer outside-name inside-name)
				      transfers-in)
				(cons (transfer inside-name outside-name)
				      transfers-out)))
		    (syntax-error "Binding not a pair" binding)))))))

    (spread-arguments
     (lambda (bindings . body)
       (if (null? bindings)
	   (syntax-sequence body)
	   (syntax-fluid-bindings bindings
	     (lambda (names values transfers-in transfers-out)
	       (make-closed-block
		lambda-tag:shallow-fluid-let names values
		(make-combination*
		 (make-variable 'DYNAMIC-WIND)
		 (make-thunk (make-sequence transfers-in))
		 (make-thunk (syntax-sequence body))
		 (make-thunk (make-sequence transfers-out)))))))))))

(define syntax-FLUID-LET-form-deep)
(define syntax-FLUID-LET-form-common-lisp)
(let ()

(define (make-fluid-let primitive procedure-tag)
  ;; (FLUID-LET ((<access-or-symbol> <value>) ...) . <body>) =>
  ;;    (WITH-SAVED-FLUID-BINDINGS
  ;;      (LAMBDA ()
  ;;        (ADD-FLUID! (THE-ENVIRONMENT) <access-or-symbol> <value>)
  ;;        ...
  ;;        <body>))
  (let ((with-saved-fluid-bindings
	 (make-primitive-procedure 'WITH-SAVED-FLUID-BINDINGS 1)))
    (spread-arguments
     (lambda (bindings . body)
       (with-variables
	;; Get all the commonlisp fluid variables
	(mapcar (lambda (binding)
		  (second (first binding)))
		(list-transform-positive bindings
		  (lambda (binding)
		    (let ((var (car binding)))
		      (and (pair? var)
			   (eq? (car var) 'ACCESS)
			   (eq? (third var) '*COMMONLISP-USER-ENVIRONMENT*))))))
	(lambda ()
	  (syntax-fluid-bindings bindings
	    (lambda (names values)
	      (make-combination
	       (internal-make-lambda procedure-tag '() '() '()
		 (make-combination
		  with-saved-fluid-bindings
		  (list
		   (make-thunk
		    (make-sequence 
		     (map*
		      (list (syntax-sequence body))
		      (lambda (name-or-access value)
			(cond ((variable? name-or-access)
			       (make-combination
				primitive
				(list the-environment-object
				      (cons name-or-access ())
				      value)))
			      ((access? name-or-access)
			       (access-components name-or-access
				 (lambda (env name)
				   (make-combination primitive
						     (list env name value)))))
			      (else
			       (syntax-error
				"Target of FLUID-LET not a symbol or ACCESS form"
				name-or-access))))
		      names values))))))
	       '())))))))))

(define (syntax-fluid-bindings bindings receiver)
  (if (null? bindings)
      (receiver '() '())
      (syntax-fluid-bindings
       (cdr bindings)
       (lambda (names values)
	 (let ((binding (car bindings)))
	   (if (pair? binding)
	       (receiver (cons (let ((name (syntax-expression (car binding))))
				 (if (or (variable? name)
					 (access? name))
				     name
				     (syntax-error "Binding name illegal"
						   (car binding))))
			       names)
			 (cons (expand-binding-value (cdr binding)) values))
	       (syntax-error "Binding not a pair" binding)))))))

(set! syntax-FLUID-LET-form-deep
  (make-fluid-let (make-primitive-procedure 'ADD-FLUID-BINDING! 3)
		  lambda-tag:deep-fluid-let))

(set! syntax-FLUID-LET-form-common-lisp
  ;; This -- groan -- is for Common Lisp support
  (make-fluid-let (make-primitive-procedure 'MAKE-FLUID-BINDING! 3)
		  lambda-tag:common-lisp-fluid-let))

;;; end special FLUID-LETs.
)


;;;; Extended Assignment Syntax

(define (invert-expression target)
  (cond ((variable? target)
	 (invert-variable (variable-name target)))
	((access? target)
	 (access-components target invert-access))
	(else
	 (syntax-error "Bad target" target))))

(define ((invert-variable name) value)
  (make-assignment name value))

(define ((invert-access environment name) value)
  (make-combination* lexical-assignment environment name value))

;;;; Declarations

;;; All declarations are syntactically checked; the resulting
;;; DECLARATION objects all contain lists of standard declarations.
;;; Each standard declaration is a proper list with symbolic keyword.

(define syntax-LOCAL-DECLARE-form
  (spread-arguments
   (lambda (declarations . body)
     (make-declaration (process-declarations declarations)
		       (syntax-sequence body)))))

(define syntax-DECLARE-form
  (spread-arguments
   (lambda declarations
     (make-block-declaration (map process-declaration declarations)))))

;;; These two procedures use `error' instead of `syntax-error' because
;;; they are called when the syntaxer is not running.

(define (process-declarations declarations)
  (if (list? declarations)
      (map process-declaration declarations)
      (error "SYNTAX: Illegal declaration list" declarations)))

(define (process-declaration declaration)
  (cond ((symbol? declaration)
	 (list declaration))
	((and (list? declaration)
	      (not (null? declaration))
	      (symbol? (car declaration)))
	 declaration)
	(else
	 (error "SYNTAX: Illegal declaration" declaration))))

;;;; SCODE Constructors

(define unassigned-object
  (make-unassigned-object))

(define the-environment-object
  (make-the-environment))

(define (make-conjunction first second)
  (make-conditional first second false))

(define (make-combination* operator . operands)
  (make-combination operator operands))

(define (make-sequence* . operands)
  (make-sequence operands))

(define (make-sequence operands)
  (internal-make-sequence operands))

'$split-file

(define (make-absolute-reference name . rest)
  (let loop ((reference (make-access (make-null) name)) (rest rest))
    (if (null? rest)
	reference
	(loop (make-access reference (car rest)) (cdr rest)))))

(define (make-thunk body)
  (make-lambda '() body))

(define (make-lambda pattern body)
  (make-named-lambda lambda-tag:unnamed pattern body))

(define (make-named-lambda name pattern body)
  (if (not (symbol? name))
      (syntax-error "Name of lambda expression must be a symbol" name))
  (parse-lambda-list pattern
    (lambda (required optional rest)
      (internal-make-lambda name required optional rest body))))

(define (make-closed-block tag names values body)
  (make-combination (internal-make-lambda tag names '() '() body)
		    values))

(define (make-letrec names values body)
  (make-closed-block lambda-tag:let '() '()
		     (make-sequence (append! (map make-definition names values)
					     (list body)))))

;;;; Lambda List Parser

(define (parse-lambda-list lambda-list receiver)
  (let ((required (list '()))
	(optional (list '())))
    (define (parse-parameters cell)
      (define (loop pattern)
	(cond ((null? pattern) (finish false))
	      ((symbol? pattern) (finish pattern))
	      ((not (pair? pattern)) (bad-lambda-list pattern))
	      ((eq? (car pattern) (access lambda-rest-tag lambda-package))
	       (if (and (pair? (cdr pattern)) (null? (cddr pattern)))
		   (cond ((symbol? (cadr pattern)) (finish (cadr pattern)))
			 ((and (pair? (cadr pattern))
			       (symbol? (caadr pattern)))
			  (finish (caadr pattern)))
			 (else (bad-lambda-list (cdr pattern))))
		   (bad-lambda-list (cdr pattern))))
	      ((eq? (car pattern) (access lambda-optional-tag lambda-package))
	       (if (eq? cell required)
		   ((parse-parameters optional) (cdr pattern))
		   (bad-lambda-list pattern)))
	      ((symbol? (car pattern))
	       (set-car! cell (cons (car pattern) (car cell)))
	       (loop (cdr pattern)))
	      ((and (pair? (car pattern)) (symbol? (caar pattern)))
	       (set-car! cell (cons (caar pattern) (car cell)))
	       (loop (cdr pattern)))
	      (else (bad-lambda-list pattern))))
      loop)

    (define (finish rest)
      (receiver (reverse! (car required))
		(reverse! (car optional))
		rest))

    (define (bad-lambda-list pattern)
      (syntax-error "Illegally-formed lambda-list" pattern))

    ((parse-parameters required) lambda-list)))

;;;; Scan Defines

(define no-scan-make-sequence
  external-make-sequence)

(define (scanning-make-sequence actions)
  (scan-defines (external-make-sequence actions)
    make-open-block))

(define (no-scan-make-lambda name required optional rest body)
  (external-make-lambda name required optional rest '() '() body))

(define scanning-make-lambda
  make-lambda*)

(define internal-make-sequence)
(define internal-make-lambda)

(set! enable-scan-defines!
  (named-lambda (enable-scan-defines!)
    (set! internal-make-sequence scanning-make-sequence)
    (set! internal-make-lambda scanning-make-lambda)))

(set! with-scan-defines-enabled
  (named-lambda (with-scan-defines-enabled thunk)
    (fluid-let ((internal-make-sequence scanning-make-sequence)
		(internal-make-lambda scanning-make-lambda))
      (thunk))))

(set! disable-scan-defines!
  (named-lambda (disable-scan-defines!)
    (set! internal-make-sequence no-scan-make-sequence)
    (set! internal-make-lambda no-scan-make-lambda)))

(set! with-scan-defines-disabled
  (named-lambda (with-scan-defines-disabled thunk)
    (fluid-let ((internal-make-sequence no-scan-make-sequence)
		(internal-make-lambda no-scan-make-lambda))
      (thunk))))

(define ((fluid-let-maker marker which-kind) #!optional name)
  (if (unassigned? name) (set! name 'FLUID-LET))
  (if (eq? name 'FLUID-LET) (set! *fluid-let-type* marker))
  (syntax-table-define system-global-syntax-table name which-kind))
  
(set! shallow-fluid-let!
  (fluid-let-maker 'SHALLOW syntax-fluid-let-form-shallow))

(set! deep-fluid-let!
  (fluid-let-maker 'DEEP syntax-fluid-let-form-deep))

(set! common-lisp-fluid-let!
  (fluid-let-maker 'COMMON-LISP syntax-fluid-let-form-common-lisp))

;;;; Top Level Syntaxers

(define syntax-table)

(define syntax-environment
  (in-package system-global-environment
    (make-environment)))

;;; The top level procedures, when not given an argument, use whatever
;;; the current syntax table is.  This is reasonable only while inside
;;; a syntaxer quantum, since at other times there is current table.

(define ((make-syntax-top-level syntaxer) expression #!optional table)
  (fluid-let ((*syntax-time-env* (push-contour *syntax-time-env*))
	      (*scode-types* (make-scode-type-table)))
    (fluid-let ((*syntax-time-global-env* *syntax-time-env*))
      (if (unassigned? table)
	  (syntaxer expression)
	  (begin (check-syntax-table table 'SYNTAX)
		 (fluid-let ((syntax-table table))
		   (syntaxer expression)))))))

(set! syntax
  (make-syntax-top-level syntax-expression))

(set! syntax*
  (make-syntax-top-level syntax-sequence))

(define (syntax-eval scode)
  (scode-eval scode syntax-environment))


;;;; Syntax Table

(define syntax-table-tag
  '(SYNTAX-TABLE))

(set! syntax-table?
  (named-lambda (syntax-table? object)
    (and (pair? object)
	 (eq? (car object) syntax-table-tag))))

(define (check-syntax-table table name)
  (if (not (syntax-table? table))
      (error "Not a syntax table" name table)))

(set! make-syntax-table
  (named-lambda (make-syntax-table #!optional parent)
    (cons syntax-table-tag
	  (cons '()
		(if (unassigned? parent)
		    '()
		    (cdr parent))))))

(set! extend-syntax-table
  (named-lambda (extend-syntax-table alist #!optional table)
    (if (unassigned? table) (set! table (current-syntax-table)))
    (check-syntax-table table 'EXTEND-SYNTAX-TABLE)
    (cons syntax-table-tag (cons alist (cdr table)))))

(set! copy-syntax-table
  (named-lambda (copy-syntax-table #!optional table)
    (if (unassigned? table) (set! table (current-syntax-table)))
    (check-syntax-table table 'COPY-SYNTAX-TABLE)
    (cons syntax-table-tag
	  (map (lambda (alist)
		 (map (lambda (pair)
			(cons (car pair) (cdr pair)))
		      alist))
	       (cdr table)))))

(set! syntax-table-ref
  (named-lambda (syntax-table-ref table name)
    (define (loop frames)
      (and (not (null? frames))
	   (let ((entry (assq name (car frames))))
	     (if entry
		 (cdr entry)
		 (loop (cdr frames))))))
    (check-syntax-table table 'SYNTAX-TABLE-REF)
    (loop (cdr table))))

(set! syntax-table-define
  (named-lambda (syntax-table-define table name quantum)
    (check-syntax-table table 'SYNTAX-TABLE-DEFINE)
    (let ((entry (assq name (cadr table))))
      (if entry
	  (set-cdr! entry quantum)
	  (set-car! (cdr table)
		    (cons (cons name quantum)
			  (cadr table)))))))

(set! syntax-table-shadow
  (named-lambda (syntax-table-shadow table name)
    (check-syntax-table table 'SYNTAX-TABLE-SHADOW)
    (let ((entry (assq name (cadr table))))
      (if entry
	  (set-cdr! entry false)
	  (set-car! (cdr table)
		    (cons (cons name false)
			  (cadr table)))))))

(set! syntax-table-undefine
  (named-lambda (syntax-table-undefine table name)
    (check-syntax-table table 'SYNTAX-TABLE-UNDEFINE)
    (if (assq name (cadr table))
	(set-car! (cdr table) 
		  (del-assq! name (cadr table))))))

;;;; Default Syntax

(enable-scan-defines!)

(set! system-global-syntax-table
  (cons syntax-table-tag
	`(((ACCESS           . ,syntax-ACCESS-form)
	   (AND              . ,syntax-CONJUNCTION-form)
	   (BEGIN            . ,syntax-SEQUENCE-form)
	   (BKPT             . ,syntax-BKPT-form)
	   (COND             . ,syntax-COND-form)
	   (CONS-STREAM      . ,syntax-CONS-STREAM-form)
	   (DECLARE          . ,syntax-DECLARE-form)
	   (DEFINE           . ,syntax-DEFINE-form)
	   (DEFINE-SYNTAX    . ,syntax-DEFINE-SYNTAX-form)
	   (DEFINE-MACRO     . ,syntax-DEFINE-MACRO-form)
	   (DELAY            . ,syntax-DELAY-form)
	   (ERROR            . ,syntax-ERROR-form)
	   (FLUID-LET        . ,syntax-FLUID-LET-form-shallow)
	   (IF               . ,syntax-IF-form)
	   (IN-PACKAGE       . ,syntax-IN-PACKAGE-form)
	   (LAMBDA           . ,syntax-LAMBDA-form)
	   (LET              . ,syntax-LET-form)
	   (LET-SYNTAX       . ,syntax-LET-SYNTAX-form)
	   (LOCAL-DECLARE    . ,syntax-LOCAL-DECLARE-form)
	   (MACRO            . ,syntax-MACRO-form)
	   (MAKE-ENVIRONMENT . ,syntax-MAKE-ENVIRONMENT-form)
	   (NAMED-LAMBDA     . ,syntax-NAMED-LAMBDA-form)
	   (OR               . ,syntax-DISJUNCTION-form)
	   ;; The funniness here prevents QUASIQUOTE from being
	   ;; seen as a nested backquote.
	   (,'QUASIQUOTE       . ,syntax-QUASIQUOTE-form)
	   (QUOTE            . ,syntax-QUOTE-form)
	   (SCODE-QUOTE      . ,syntax-SCODE-QUOTE-form)
	   (SEQUENCE         . ,syntax-SEQUENCE-form)
	   (SET!             . ,syntax-SET!-form)
	   (THE-ENVIRONMENT  . ,syntax-THE-ENVIRONMENT-form)
	   (UNASSIGNED?      . ,syntax-UNASSIGNED?-form)
	   (UNBOUND?         . ,syntax-UNBOUND?-form)
	   (USING-SYNTAX     . ,syntax-USING-SYNTAX-form)
	   ))))

