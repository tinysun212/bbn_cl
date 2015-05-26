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

;;;; Stack Parser

(declare (usual-integrations))

(define raw-continuation?)
(define continuation?)
(define call-with-current-continuation)
(define non-reentrant-call-with-current-continuation)
(define within-continuation)
(define null-continuation?)
(define continuation-expression)
(define continuation-environment)
(define continuation-reductions)
(define continuation-dynamic-state)
(define continuation-fluid-bindings)
(define continuation-return-code)
(define continuation-next-continuation)
(define continuation-annotation)
(define continuation-undefined-environment?)
(define continuation-undefined-expression?)
(define continuation-evaluated-object?)
(define continuation-evaluated-object-value)
(define continuation-package)
(define raw-continuation->continuation)

(let ((type-code:control-point (microcode-type 'CONTROL-POINT))
      (type-code:reference-trap (microcode-type 'REFERENCE-TRAP))
      (return-address-restore-history
       (make-return-address (microcode-return 'RESTORE-HISTORY)))
      (return-address-restore-dont-copy-history
       (make-return-address (microcode-return 'RESTORE-DONT-COPY-HISTORY)))
      (return-address-restore-fluids
       (make-return-address (microcode-return 'RESTORE-FLUIDS)))
      (return-address-restore-interrupt-mask
       (make-return-address (microcode-return 'RESTORE-INTERRUPT-MASK)))
      (return-address-reenter-compiled-code
       (make-return-address (microcode-return 'REENTER-COMPILED-CODE)))
      (primitive-cwcc
       (make-primitive-procedure 'CALL-WITH-CURRENT-CONTINUATION))
      (primitive-nr-cwcc
       (make-primitive-procedure
	'NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION))
      (translate-to-state-point
       (make-primitive-procedure 'TRANSLATE-TO-STATE-POINT))
      (within-control-point (make-primitive-procedure 'WITHIN-CONTROL-POINT))
      (evaluated-object-tag '(EVALUATED))
      (undefined-expression '(UNDEFINED-EXPRESSION))
      (compiled-code '(COMPILED-CODE))
      (undefined-environment '(UNDEFINED-ENVIRONMENT))
      (undefined-reductions '()))

(declare (integrate-primitive-procedures
	  (primitive-cwcc call-with-current-continuation)
	  (primitive-nr-cwcc non-reentrant-call-with-current-continuation)
	  translate-to-state-point
	  within-control-point))

(define (tag-procedure)
  '())

(set! continuation-package (the-environment))

(set! raw-continuation?
  (named-lambda (raw-continuation? object)
    (and (compound-procedure? object)
	 (not (lexical-unbound? (procedure-environment object) 'TAG!!))
	 (eq? (access tag!! (procedure-environment object)) tag-procedure))))

(set! continuation?
  (named-lambda (continuation? object)
    (or (raw-continuation? object)
	(and (compound-procedure? object)
	     (let ((environment (procedure-environment object)))
	       (and (environment? environment)
		    (eq? (environment-procedure
			  (environment-parent environment))
			 make-continuation)))))))

;;;; User Interface

(define (catch-maker catch-primitive restore-state-primitive one-time-only?)
  (let ((tag!! tag-procedure))
    (lambda (receiver)
      (fluid-let ()
	(catch-primitive
	 (lambda (control-point)
	   (let ((dynamic-state (current-dynamic-state))
		 (used-up? false))
	     (define (the-raw-continuation value)
	       (if used-up?
		   (let loop ()
		     (error "RESTART: Continuation has been destroyed."
			    control-point)
		     (loop)))
	       (set! used-up? one-time-only?)
	       (restore-state-primitive dynamic-state)
	       (control-point value))
	     (the-raw-continuation (receiver the-raw-continuation)))))))))

(set! call-with-current-continuation
  (catch-maker primitive-cwcc translate-to-state-point false))

(set! non-reentrant-call-with-current-continuation
  (catch-maker primitive-nr-cwcc translate-to-state-point true))

(set! raw-continuation->continuation
  (named-lambda (raw-continuation->continuation raw-continuation)
    (let ((env (procedure-environment raw-continuation)))
      (let ((control-point (access control-point env))
	    (dynamic-state (access dynamic-state env)))
	(make-continuation
	 (access one-time-only? env)
	 (delay control-point)
	 dynamic-state
	 '()				;Initial Fluid state???
	 (delay (parse-stack (control-point->stack control-point)
			     (access the-empty-history history-package)
			     dynamic-state
			     '()	;Initial Fluid state???
			     the-null-restore-history
			     the-null-interrupt-mask)))))))

(set! within-continuation
  (named-lambda (within-continuation continuation thunk)
    (if (not (continuation? continuation))
	(error "WITHIN-CONTINUATION: Not a continuation" continuation)
	(let ((env
	       (procedure-environment (upgrade-continuation continuation))))
	  (if (access used-up? env)
	      (error "WITHIN-CONTINUATION: continuation has been destroyed."
		     continuation)
	      (begin (translate-to-state-point (access dynamic-state env))
		     (within-control-point
		      (force (access promised-control-point env))
		      thunk)))))))

(set! null-continuation?
      (named-lambda (null-continuation? continuation)
	(null? (parser-output continuation))))

(set! continuation-expression
      (named-lambda (continuation-expression continuation)
	(parsed-expression (parser-output continuation))))

(set! continuation-environment
      (named-lambda (continuation-environment continuation)
	(parsed-environment (parser-output continuation))))

(set! continuation-reductions
      (named-lambda (continuation-reductions continuation)
	(parsed-reductions (parser-output continuation))))

(set! continuation-dynamic-state
      (named-lambda (continuation-dynamic-state continuation)
	(parsed-dynamic-state (parser-output continuation))))

(set! continuation-fluid-bindings
      (named-lambda (continuation-fluid-bindings continuation)
	(parsed-fluid-bindings (parser-output continuation))))

(set! continuation-next-continuation
  (named-lambda (continuation-next-continuation continuation)
    (let ((parser-output (parser-output continuation)))
      (make-continuation
       false
       (delay (stack->control-point
	       (parsed-stack parser-output)
	       (parsed-fluid-bindings parser-output)
	       ((access history-untransform history-package)
		(parsed-history parser-output))
	       (parsed-previous-restore-history parser-output)
	       (parsed-interrupt-mask parser-output)))
       (parsed-dynamic-state parser-output)
       (parsed-fluid-bindings parser-output)
       (delay (parse-stack (parsed-stack parser-output)
			   (parsed-history parser-output)
			   (parsed-dynamic-state parser-output)
			   (parsed-fluid-bindings parser-output)
			   (parsed-previous-restore-history parser-output)
			   (parsed-interrupt-mask parser-output)))))))

(set! continuation-return-code
  (named-lambda (continuation-return-code continuation)
    (parsed-return-code (parser-output continuation))))

(set! continuation-annotation
  (named-lambda (continuation-annotation continuation)
    (parsed-annotation (parser-output continuation))))

;;; Evaluated objects.

(define (make-evaluated-object expression)
  (list evaluated-object-tag expression))

(set! continuation-evaluated-object?
  (named-lambda (continuation-evaluated-object? object)
    (and (pair? object)
	 (eq? (car object) evaluated-object-tag))))

(set! continuation-evaluated-object-value cadr)

;;; Undefined objects.

(set! continuation-undefined-expression?
  (named-lambda (continuation-undefined-expression? object)
    (or (eq? undefined-expression object)
	(eq? compiled-code object))))

(set! continuation-undefined-environment?
  (named-lambda (continuation-undefined-environment? object)
    (eq? undefined-environment object)))

;;;; Stack Parser

(define (parse-stack stack history dynamic-state fluids
		     previous-restore-history interrupt-mask)
  (if (return-address? (car stack))
      ((vector-ref stack-parser-table (return-address-code (car stack)))
       stack history dynamic-state fluids
       previous-restore-history interrupt-mask)
      (error "PARSE-STACK needs a valid return code")))

(define stack-parser-table
  (vector-cons number-of-microcode-returns
	       (lambda (stack history dynamic-state fluids
			previous-restore-history interrupt-mask)
		 (error "Undefined return address"
			parse-stack
                  	(stack-ref stack 0)))))

(define (define-stack-parser name parser)
  (vector-set! stack-parser-table
	       (or (microcode-return name)
		   (error "Unknown return address name" name))
	       parser))

(define (define-standard-parser name parser)
  (define-stack-parser name
    (lambda (stack history dynamic-state fluids
	     previous-restore-history interrupt-mask)
      (parser (stack-tail stack 1)
	      history
	(lambda (expression environment count . annotation)
	  (apply make-parser-output
		 (stack-ref stack 0)
		 expression
		 environment
		 ((access history-reductions history-package) history)
		 (stack-tail stack (1+ count))
		 ((access history-superproblem history-package) history)
		 dynamic-state
		 fluids
		 (monus-restore-history previous-restore-history (1+ count))
		 interrupt-mask
		 annotation))))))

(define (parse-standard-frame stack history cont)
  (cont (stack-ref stack 0)
	(stack-ref stack 1)
	2))

(define (parse-expression-only-frame stack history cont)
  (cont (stack-ref stack 0)
        undefined-environment
	1))

;;;; Parser Output Abstraction

;;If you change MAKE-CONTINUATION, you must also change CONTINUATION?

(define set-fluid-bindings!)

(define (make-continuation one-time-only? promised-control-point
			   dynamic-state fluids promised-parser-output)
  (let ((used-up? false))
    (named-lambda (continuation value)
      (if used-up?
	  (let loop ()
	    (error "CONTINUATION: Continuation has been destroyed"
		   continuation)
	    (loop)))
      (set! used-up? one-time-only?)
      (translate-to-state-point dynamic-state)
      (set-fluid-bindings! fluids)
      ((force promised-control-point) value))))

(define (upgrade-continuation probably-continuation)
  (if (raw-continuation? probably-continuation)
      (raw-continuation->continuation probably-continuation)
      probably-continuation))

(define (control-point probably-continuation)
  (let ((continuation (upgrade-continuation probably-continuation)))
    (let ((the-env (procedure-environment continuation)))
      (if (access used-up? the-env)
	  (error "CONTROL-POINT: Continuation has been destroyed"
		 continuation)
	  (force (access promised-control-point the-env))))))

(define (fake-set-fluid-bindings! fluids)
  true)

(define (reset!)
  (let ((prim (make-primitive-procedure 'SET-FLUID-BINDINGS! 1)))
    (set! set-fluid-bindings!
	  (if (implemented-primitive-procedure? prim)
	      prim
	      fake-set-fluid-bindings!)))
  true)

(define (parser-output probably-continuation)
  (let ((continuation (upgrade-continuation probably-continuation)))
    (let ((the-env (procedure-environment continuation)))
      (if (access used-up? the-env)
	  (error "PARSER: Continuation has been destroyed" continuation)
	  (force (access promised-parser-output the-env))))))

(define (make-parser-output return-address expression environment
                            reductions stack history dynamic-state
			    fluids previous-restore-history
			    interrupt-mask . annotation)
  (list->vector
   (cons* expression environment reductions stack
	  history dynamic-state fluids
	  previous-restore-history interrupt-mask
          (if (return-address? return-address)
	      (return-address-code return-address)
	      '())
          annotation)))

(define (parsed-expression parser-output)
  (vector-ref parser-output 0))

(define (parsed-environment parser-output)
  (vector-ref parser-output 1))

(define (parsed-reductions parser-output)
  (vector-ref parser-output 2))

(define (parsed-stack parser-output)
  (vector-ref parser-output 3))

(define (parsed-history parser-output)
  (vector-ref parser-output 4))

(define (parsed-dynamic-state parser-output)
  (vector-ref parser-output 5))

(define (parsed-fluid-bindings parser-output)
  (vector-ref parser-output 6))

(define (parsed-previous-restore-history parser-output)
  (vector-ref parser-output 7))

(define (parsed-interrupt-mask parser-output)
  (vector-ref parser-output 8))

(define (parsed-return-code parser-output)
  (vector-ref parser-output 9))

(define (parsed-annotation parser-output)
  (subvector->list parser-output 10 (vector-length parser-output)))

;;;; Stack Abstraction

(define (stack-head stack)
  (if (non-touching-primitive-type? type-code:reference-trap
				    (lambda () (car stack)))
      (map-reference-trap (lambda () (car stack)))
      (car stack)))

(define (stack-ref stack n)
  (stack-head (stack-tail stack n)))

(define (stack-tail stack n)
  (if (zero? n)
      stack
      (stack-tail (force (cdr stack))
                  (-1+ n))))

(define (stack-cons x stack)
  (cons x (delay stack)))

(define (stack-list stack n)
  (if (zero? n)
      '()
      (cons (stack-head stack)
            (stack-list (force (cdr stack))
                        (-1+ n)))))

(define (stack-split stack n receiver)
  (if (zero? n)
      (receiver '() stack)
      (stack-split (force (cdr stack)) (-1+ n)
        (lambda (head tail)
          (receiver (cons (stack-head stack) head)
                    tail)))))

;;; Previous-Restore-History and Interrupt-mask abstraction

(define the-null-restore-history (cons '() 0))
(define make-restore-history cons)
(define restore-history-control-point car)
(define restore-history-offset cdr)

(define (new-restore-history control-point restore-history)
  (if (eq? control-point (restore-history-control-point restore-history))
      (make-restore-history '() (restore-history-offset restore-history))
      restore-history))   

(define (monus-restore-history restore-history amount)
  (if (null? (restore-history-control-point restore-history))
      (make-restore-history
       '()
       (max 0 (- (restore-history-offset restore-history) amount)))
      restore-history))

(define the-null-interrupt-mask 0)

;;; end CONTINUATION package.
)
((access reset! continuation-package))
