#| -*-Scheme-*-

$Header: opncod.scm,v 1.8 88/08/31 10:43:02 jinx Exp $
$MIT-Header: opncod.scm,v 4.4 88/03/31 21:35:23 GMT mhwu Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; RTL Generation: Inline Combinations

(declare (usual-integrations))

;; *** NOTE *** this is a horrible kludge!
;; This section should be split into a separate file and loaded into
;; the appropriate environment to avoid (THE-ENVIRONMENT) in code.

(define open-coder-package)

(package (open-coding-analysis combination/inline)

(set! open-coder-package (the-environment))

;;;; Analysis

(define-export (open-coding-analysis applications)
  (for-each (if compiler:open-code-primitives?
		(lambda (application)
		  (if (eq? (application-type application) 'COMBINATION)
		      (set-combination/inliner!
		       application
		       (analyze-combination application))))
		(lambda (application)
		  (if (eq? (application-type application) 'COMBINATION)
		      (set-combination/inliner! application false))))
	    applications))

(define (analyze-combination combination)
  (let ((operator (combination/operator combination)))
    (let ((entry (reference-open-coder operator)))
      (if entry
	  (try-reference-handler combination entry)
	  (let ((callee (rvalue-known-value operator)))
	    (and callee
		 (rvalue/constant? callee)
		 (let ((value (constant-value callee)))
		   (and (scode/primitive-procedure? value)
			(let ((entry
			       (assq (primitive-procedure-name value)
				     name->open-coders)))
			  (and entry
			       (try-handler combination value (cdr entry))))))))))))

(define (reference-open-coder node)
  (and (rvalue/reference? node)
       (let ((name (variable-name (reference-lvalue node))))
	 (assq name name->open-coders))))

(define (reference-open-code-arity name)
  (let ((entry (assq name reference-open-coder-name->arity)))
    (and entry
	 (cdr entry))))

(define (try-reference-handler combination entry)
  (let* ((operands (combination/operands combination))
	 (name (car entry))
	 (open-coder-info (cdr entry))
	 (arity (reference-open-code-arity name)))
    (if (and arity
	     (or (= arity -1)
		 (= arity (length operands))))
	(let ((result ((vector-ref open-coder-info 0) operands)))
	  (and result
	       (transmit-values result
		(lambda (generator indices)
		  (make-inliner open-coder-info generator indices))))))))

(define (try-handler combination primitive entry)
  (let ((operands (combination/operands combination)))
    (and (primitive-arity-correct? primitive (length operands))
	 (let ((result ((vector-ref entry 0) operands)))
	   (and result
		(transmit-values result
		  (lambda (generator indices)
		    (make-inliner entry generator indices))))))))

;;;; Code Generator

(define-export (combination/inline combination)
  (let ((offset (node/offset combination)))
    (generate/return* (combination/block combination)
		      (combination/continuation combination)
		      (let ((inliner (combination/inliner combination)))
			(let ((handler (inliner/handler inliner))
			      (generator (inliner/generator inliner))
			      (expressions
			       (map (subproblem->expression offset)
				    (inliner/operands inliner))))
			  (make-return-operand
			   (lambda (offset)
			     ((vector-ref handler 1) generator expressions))
			   (lambda (offset finish)
			     ((vector-ref handler 2) generator
						     expressions
						     finish))
			   (lambda (offset finish)
			     ((vector-ref handler 3) generator
						     expressions
						     finish))
			   false)))
		      offset)))

(define (subproblem->expression offset)
  (lambda (subproblem)
    (let ((rvalue (subproblem-rvalue subproblem)))
      (let ((value (rvalue-known-value rvalue)))
	(cond ((and value (rvalue/constant? value))
	       (rtl:make-constant (constant-value value)))
	      ((and value
		    (rvalue/procedure? value)
		    (procedure/closure? value)
		    (procedure/trivial-closure? value))
	       (make-trivial-closure-cons value))
	      ((and (rvalue/reference? rvalue)
		    (not (variable/value-variable? (reference-lvalue rvalue)))
		    (reference-to-known-location? rvalue))
	       (rtl:make-fetch
		(find-known-variable (reference-block rvalue)
				     (reference-lvalue rvalue)
				     offset)))
	      (else
	       (rtl:make-fetch
		(continuation*/register
		 (subproblem-continuation subproblem)))))))))

(define (invoke/effect->effect generator expressions)
  (generator expressions false))

(define (invoke/predicate->value generator expressions finish)
  (generator expressions
    (lambda (pcfg)
      (let ((temporary (rtl:make-pseudo-register)))
	(scfg*scfg->scfg!
	 (pcfg*scfg->scfg!
	  pcfg
	  (rtl:make-assignment temporary (rtl:make-constant true))
	  (rtl:make-assignment temporary (rtl:make-constant false)))
	 (finish (rtl:make-fetch temporary)))))))

(define (invoke/value->effect generator expressions)
  (make-null-cfg))

(define (invoke/value->predicate generator expressions finish)
  (generator expressions
    (lambda (expression)
      (finish (rtl:make-true-test expression)))))

(define (invoke/value->value generator expressions finish)
  (generator expressions finish))

;;;; Definers

(define (open-coder-definer ->effect ->predicate ->value)
  (let ((per-name
	 (lambda (name handler arity)
	   (if arity
	       (let ((arity-entry (assq name reference-open-coder-name->arity)))
		 (if arity-entry
		     (set-cdr! arity-entry arity)
		     (set! reference-open-coder-name->arity
			   (cons (cons name arity) reference-open-coder-name->arity)))))
	   (let ((entry (assq name name->open-coders))
		 (item (vector handler ->effect ->predicate ->value)))
	     (if entry
		 (set-cdr! entry item)
		 (set! name->open-coders
		       (cons (cons name item) name->open-coders)))))))
    (lambda (name handler #!optional arity)
      (if (unassigned? arity) (set! arity #f))
      (if (pair? name)
	  (for-each (lambda (name)
		      (per-name name handler arity))
		    name)
	  (per-name name handler arity))
      name)))

(define name->open-coders
  '())

(define reference-open-coder-name->arity
  '())

(in-package system-global-environment
  (define compiler:change-name-of-open-coder!)
  (define compiler:copy-open-coder!))

(set! compiler:change-name-of-open-coder!
      (named-lambda (compiler:change-name-of-open-coder! from-name to-name)
	(let loop ((l name->open-coders))
	  (cond
	   ((null? l)
	    #f)
	   ((eq? (caar l) from-name)
	    (set-car! (car l) to-name))
	   (else
	    (loop (cdr l)))))
	(let loop ((l reference-open-coder-name->arity))
	  (cond
	   ((null? l)
	    #f)
	   ((eq? (caar l) from-name)
	    (set-car! (car l) to-name))
	   (else
	    (loop (cdr l)))))))

(set! compiler:copy-open-coder!
      (named-lambda (compiler:copy-open-coder! from-name to-name)
	(let ((entry (assq from-name name->open-coders)))
	  (if entry
	      (set! name->open-coders
		    (cons (cons to-name (cdr entry)) name->open-coders))))
	(let ((entry (assq from-name reference-open-coder-name->arity)))
	  (if entry
	      (set! reference-open-coder-name->arity
		    (cons (cons to-name (cdr entry)) reference-open-coder-name->arity))))
	#f))

(define define-open-coder/effect
  (open-coder-definer invoke/effect->effect
		      invoke/value->predicate
		      invoke/value->value))

(define define-open-coder/predicate
  (open-coder-definer invoke/value->effect
		      invoke/value->value
		      invoke/predicate->value))

(define define-open-coder/value
  (open-coder-definer invoke/value->effect
		      invoke/value->predicate
		      invoke/value->value))

;;;; Operand Filters

(define (filter/constant rvalue predicate generator)
  (let ((operand (rvalue-known-value rvalue)))
    (and operand
	 (rvalue/constant? operand)
	 (let ((value (constant-value operand)))
	   (and (predicate value)
		(generator value))))))

(define (filter/nonnegative-integer operand generator)
  (filter/constant operand
		   (lambda (value)
		     (and (integer? value)
			  (not (negative? value))))
		   generator))

(define (filter/positive-integer operand generator)
  (filter/constant operand
		   (lambda (value)
		     (and (integer? value)
			  (positive? value)))
		   generator))

;;;; Open Coders

(define-open-coder/predicate 'NULL?
  (lambda (operands)
    (return-2 (lambda (expressions finish)
		(finish (pcfg-invert (rtl:make-true-test (car expressions)))))
	      '(0))))

(let ((open-code/type-test
       (lambda (type)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-type-test (rtl:make-object->type (car expressions))
				type))))))

  (let ((define/type-test
	  (lambda (name type)
	    (define-open-coder/predicate name
	      (lambda (operands)
		(return-2 (open-code/type-test type) '(0)))))))
    (define/type-test 'PAIR? (ucode-type pair))
    (define/type-test 'STRING? (ucode-type string))
    (define/type-test 'BIT-STRING? (ucode-type vector-1b)))

  (define-open-coder/predicate 'PRIMITIVE-TYPE?
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/type-test type) '(1))))))

  (define-open-coder/predicate 'NON-TOUCHING-PRIMITIVE-TYPE?
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/type-test type) '(1)))))))

(let ((open-code/eq-test
       (lambda (expressions finish)
	 (finish (rtl:make-eq-test (car expressions) (cadr expressions))))))
  (define-open-coder/predicate 'EQ?
    (lambda (operands)
      (return-2 open-code/eq-test '(0 1)))))

(let ((open-code/pair-cons
       (lambda (type)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-typed-cons:pair (rtl:make-constant type)
				      (car expressions)
				      (cadr expressions)))))))

  (define-open-coder/value 'CONS
    (lambda (operands)
      (return-2 (open-code/pair-cons (ucode-type pair)) '(0 1))))

  (define-open-coder/value 'SYSTEM-PAIR-CONS
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/pair-cons type) '(1 2)))))))

(define-open-coder/value 'VECTOR
  (lambda (operands)
    (and (< (length operands) 32)
	 (return-2 (lambda (expressions finish)
		     (finish
		      (rtl:make-typed-cons:vector
		       (rtl:make-constant (ucode-type vector))
		       expressions)))
		   (all-operand-indices operands)))))

(define (all-operand-indices operands)
  (let loop ((operands operands) (index 0))
    (if (null? operands)
	'()
	(cons index (loop (cdr operands) (1+ index))))))

(let ((open-code/memory-length
       (lambda (index)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-cons-pointer
	     (rtl:make-constant (ucode-type fixnum))
	     (rtl:make-fetch
	      (rtl:locative-offset (car expressions) index))))))))
  (let ((define/length
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		(return-2 (open-code/memory-length index) '(0)))))))
    (define/length '(VECTOR-LENGTH SYSTEM-VECTOR-SIZE) 0)
    (define/length '(STRING-LENGTH BIT-STRING-LENGTH) 1)))

(let ((open-code/memory-ref
       (lambda (index name)
	 (lambda (expressions finish)
	   (let ((expression (car expressions)))
	     (if (rtl:constant? expression)
		 (finish
		  (list 'CONSTANT 
			((make-primitive-procedure name) (cadr expression))))
		 (finish
		  (rtl:make-fetch (rtl:locative-offset expression index)))))))))

  (let ((define/ref
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		(return-2 (open-code/memory-ref index name) '(0)))))))
    (define/ref 'CAR 0)
    (define/ref 'SYSTEM-PAIR-CAR 0)
    (define/ref 'CELL-CONTENTS 0)
    (define/ref 'SYSTEM-HUNK3-CXR0 0)
    (define/ref 'CDR 1)
    (define/ref 'SYSTEM-PAIR-CDR 1)
    (define/ref 'SYSTEM-HUNK3-CXR1 1)
    (define/ref 'SYSTEM-HUNK3-CXR2 2))

  (for-each (lambda (name)
	      (define-open-coder/value name
		(lambda (operands)
		  (filter/nonnegative-integer (cadr operands)
                    (lambda (index)
		      (return-2 (open-code/memory-ref (1+ index) name) '(0)))))))
	    '(VECTOR-REF SYSTEM-VECTOR-REF)))

;;;
;;; Reference (non-primitive) inline code for commonlisp car/cdr
;;;

(let ((open-code/safe-memory-ref
       (lambda (index)
	 (lambda (expressions finish)
	   (let ((temporary (rtl:make-pseudo-register))
		 (expression (car expressions)))
	     (if (rtl:constant? expression)
		 (finish 
		  (let ((constant (cadr expression)))
		    (cond 
		     ((null? constant)
		      '(CONSTANT ()))
		     ((pair? constant)
		      `(CONSTANT
			,((if (= index 0) car cdr)
			  constant)))
		     (else (error "CAR/CDR of non-list" constant)))))
		 (scfg*scfg->scfg!
		  (pcfg*scfg->scfg!
		   (rtl:make-true-test expression)
		   (rtl:make-assignment temporary
					(rtl:make-fetch
					 (rtl:locative-offset expression index)))
		   (rtl:make-assignment temporary expression))
		  (finish (rtl:make-fetch temporary)))))))))

  (let ((define/ref
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		(return-2 (open-code/safe-memory-ref index) '(0)))
	      1))))
    (define/ref '(CL-CAR) 0)
    (define/ref '(CL-CDR) 1)))


(let ((open-code/general-car-cdr
       (lambda (pattern)
	 (lambda (expressions finish)
	   (finish
	    (let loop ((pattern pattern) (expression (car expressions)))
	      (if (= pattern 1)
		  expression
		  (let ((qr (integer-divide pattern 2)))
		    (loop (integer-divide-quotient qr)
			  (rtl:make-fetch
			   (rtl:locative-offset
			    expression
			    (- 1 (integer-divide-remainder qr)))))))))))))
  (define-open-coder/value 'GENERAL-CAR-CDR
    (lambda (operands)
      (filter/positive-integer (cadr operands)
	(lambda (pattern)
	  (return-2 (open-code/general-car-cdr pattern) '(0)))))))

(let ((open-code/memory-assignment
       (lambda (index)
	 (lambda (expressions finish)
	   (let ((locative (rtl:locative-offset (car expressions) index)))
	     (let ((assignment
		    (rtl:make-assignment locative (cadr expressions))))
	       (if finish
		   (let ((temporary (rtl:make-pseudo-register)))
		     (scfg-append!
		      (rtl:make-assignment temporary (rtl:make-fetch locative))
		      assignment
		      (finish (rtl:make-fetch temporary))))
		   assignment)))))))

  (let ((define/set!
	  (lambda (name index)
	    (define-open-coder/effect name
	      (lambda (operands)
		(return-2 (open-code/memory-assignment index) '(0 1)))))))
    (define/set! '(SET-CAR! SYSTEM-PAIR-SET-CAR!
			    SET-CELL-CONTENTS!
			    SYSTEM-HUNK3-SET-CXR0!)
      0)
    (define/set! '(SET-CDR! SYSTEM-PAIR-SET-CDR! SYSTEM-HUNK3-SET-CXR1!) 1)
    (define/set! 'SYSTEM-HUNK3-SET-CXR2! 2))

  (define-open-coder/effect '(VECTOR-SET! SYSTEM-VECTOR-SET!)
    (lambda (operands)
      (filter/nonnegative-integer (cadr operands)
	(lambda (index)
	  (return-2 (open-code/memory-assignment (1+ index)) '(0 2)))))))

;;; end COMBINATION/INLINE
)
