#| -*-Scheme-*-

$Header: rgrval.scm,v 1.2 88/08/31 10:43:21 jinx Exp $
$MIT-Header: rgrval.scm,v 4.4.1.1 88/08/05 15:15:25 GMT jinx Exp $

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

;;;; RTL Generation: RValues

(declare (usual-integrations))

(package (generate/rvalue load-closure-environment)

(define-export (generate/rvalue operand offset scfg*cfg->cfg! generator)
  (transmit-values (generate/rvalue* operand offset)
    (lambda (prefix expression)
      (scfg*cfg->cfg! prefix (generator expression)))))

(define (generate/rvalue* operand offset)
  ((method-table-lookup rvalue-methods (tagged-vector/index operand))
   operand
   offset))

(define rvalue-methods
  (make-method-table rvalue-types false))

(define-integrable (expression-value/simple expression)
  (return-2 (make-null-cfg) expression))

(define (expression-value/temporary prefix result)
  (let ((register (rtl:make-pseudo-register)))
    (return-2 (scfg*scfg->scfg! prefix (rtl:make-assignment register result))
	      (rtl:make-fetch register))))

(define-integrable (expression-value/transform expression-value transform)
  (transmit-values expression-value
    (lambda (prefix expression)
      (return-2 prefix (transform expression)))))

(define-method-table-entry 'CONSTANT rvalue-methods
  (lambda (constant offset)
    (generate/constant constant)))

(define (generate/constant constant)
  (expression-value/simple (rtl:make-constant (constant-value constant))))

(define-method-table-entry 'BLOCK rvalue-methods
  (lambda (block offset)
    (expression-value/simple (rtl:make-fetch register:environment))))

(define-method-table-entry 'REFERENCE rvalue-methods
  (lambda (reference offset)
    (let ((block (reference-block reference))
	  (lvalue (reference-lvalue reference))
	  (safe? (reference-safe? reference)))
      (let ((value (lvalue-known-value lvalue))
	    (perform-fetch
	     (lambda ()
	       (find-variable block lvalue offset
		(lambda (locative)
		  (expression-value/simple (rtl:make-fetch locative)))
		(lambda (environment name)
		  (expression-value/temporary
		   (rtl:make-interpreter-call:lookup
		    environment
		    (intern-scode-variable! block name)
		    safe?)
		   (rtl:interpreter-call-result:lookup)))
		(lambda (name)
		  (generate/cached-reference name safe?))))))
	(cond ((not value) (perform-fetch))
	      ((not (rvalue/procedure? value))
	       (generate/rvalue* value offset))
	      ((and (procedure/closure? value)
		    (procedure/trivial-closure? value))
	       (expression-value/simple (make-trivial-closure-cons value)))
	      (else (perform-fetch)))))))

(define (generate/cached-reference name safe?)
  (let ((temp (rtl:make-pseudo-register))
	(result (rtl:make-pseudo-register)))
    (return-2
     (let* ((cell (rtl:make-fetch temp))
	    (reference (rtl:make-fetch cell))
	    (n1 (rtl:make-assignment temp (rtl:make-variable-cache name))))
       ;; n1 MUST be bound before the rest.  It flags temp as a
       ;; register that contains an address.
       (let ((n2 (rtl:make-type-test (rtl:make-object->type reference)
				     (ucode-type reference-trap)))
	     (n3 (rtl:make-assignment result reference))
	     (n4 (rtl:make-interpreter-call:cache-reference cell safe?))
	     (n5
	      (rtl:make-assignment
	       result
	       (rtl:interpreter-call-result:cache-reference))))
	 (scfg-next-connect! n1 n2)
	 (pcfg-alternative-connect! n2 n3)
	 (scfg-next-connect! n4 n5)
	 (if safe?
	     (let ((n6 (rtl:make-unassigned-test reference))
		   ;; Make new copy of n3 to keep CSE happy.
		   ;; Otherwise control merge will confuse it.
		   (n7 (rtl:make-assignment result reference)))
	       (pcfg-consequent-connect! n2 n6)
	       (pcfg-consequent-connect! n6 n7)
	       (pcfg-alternative-connect! n6 n4)
	       (make-scfg (cfg-entry-node n1)
			  (hooks-union (scfg-next-hooks n3)
				       (hooks-union (scfg-next-hooks n5)
						    (scfg-next-hooks n7)))))
	     (begin
	       (pcfg-consequent-connect! n2 n4)
	       (make-scfg (cfg-entry-node n1)
			  (hooks-union (scfg-next-hooks n3)
				       (scfg-next-hooks n5)))))))
     (rtl:make-fetch result))))

(define-method-table-entry 'PROCEDURE rvalue-methods
  (lambda (procedure offset)
    (enqueue-procedure! procedure)
    (case (procedure/type procedure)
      ((CLOSURE)
       (if (procedure/trivial-closure? procedure)
	   (expression-value/simple (make-trivial-closure-cons procedure))
	   (let ((register (rtl:make-pseudo-register)))
	     (return-2
	      (scfg*scfg->scfg!
	       (make-non-trivial-closure-cons procedure)
	       (scfg*scfg->scfg!
		(rtl:make-assignment register
				    (rtl:interpreter-call-result:enclose))
		(load-closure-environment procedure offset
					 (rtl:make-fetch register))))
	      (rtl:make-fetch register)))))
      ((IC)
       (expression-value/simple (make-ic-cons procedure)))
      ((OPEN-EXTERNAL OPEN-INTERNAL)
       (error "Reference to open procedure" procedure))
      (else
       (error "Unknown procedure type" procedure)))))

(define-export (load-closure-environment procedure offset closure-locative)
  (let ((block (procedure-closing-block procedure)))
    (cond ((not block)
	   (make-null-cfg))
	  ((ic-block? block)
	   (rtl:make-assignment
	    (rtl:locative-offset closure-locative closure-block-first-offset)
	    (if (ic-block/use-lookup? block)
		(let ((closure-block (procedure-closure-block procedure)))
		  (if (ic-block? closure-block)
		      (rtl:make-fetch register:environment)
		      (closure-ic-locative closure-block block offset)))
		(rtl:make-constant false))))
	  ((closure-block? block)
	   (let ((closure-block (procedure-closure-block procedure)))
	     (define (loop entries code)
	       (if (null? entries)
		   code
		   (loop (cdr entries)
			 (scfg*scfg->scfg!
			  (rtl:make-assignment
			   (rtl:locative-offset closure-locative
						(cdar entries))
			   (let* ((variable (caar entries))
				  (value (lvalue-known-value variable)))
			     (cond ;; This is a waste.  It should be integrated.
				   ((and value
					 (rvalue/procedure? value)
					 (procedure/closure? value)
					 (procedure/trivial-closure? value))
				    (make-trivial-closure-cons value))
				   ((not (eq? value (block-procedure
						     closure-block)))
				    (rtl:make-fetch
				     (find-closure-variable closure-block
							    variable
							    offset)))
				   (else
				    (rtl:make-fetch
				     (block-closure-locative closure-block
							     offset))))))
			  code))))

	     (loop
	      (block-closure-offsets block)
	      (if (let ((parent (block-parent block)))
		    (and parent (ic-block/use-lookup? parent)))
		  (rtl:make-assignment
		   (rtl:locative-offset closure-locative
					closure-block-first-offset)
		   (if (ic-block? closure-block)
		       (rtl:make-fetch register:environment)
		       (closure-ic-locative closure-block block offset)))
		  (make-null-cfg)))))
	  (else
	   (error "Unknown block type" block)))))

;;; end GENERATE/RVALUE
)

(define (make-ic-cons procedure)
  ;; IC procedures have their entry points linked into their headers
  ;; at load time by the linker.
  (let ((header
	 (scode/make-lambda (procedure-name procedure)
			    (map variable-name
				 (procedure-required-arguments procedure))
			    (map variable-name (procedure-optional procedure))
			    (let ((rest (procedure-rest procedure)))
			      (and rest (variable-name rest)))
			    (map variable-name (procedure-names procedure))
			    '()
			    false)))
    (set! *ic-procedure-headers*
	  (cons (cons header (procedure-label procedure))
		*ic-procedure-headers*))
    (rtl:make-typed-cons:pair
     (rtl:make-constant (scode/procedure-type-code header))
     (rtl:make-constant header)
     ;; Is this right if the procedure is being closed
     ;; inside another IC procedure?
     (rtl:make-fetch register:environment))))

(define (make-trivial-closure-cons procedure)
  (rtl:make-cons-pointer
   (rtl:make-constant type-code:compiled-entry)
   (rtl:make-entry:procedure (procedure-label procedure))))

(define (make-non-trivial-closure-cons procedure)
  (with-procedure-arity-encoding procedure
   (lambda (min max)
     (rtl:make-cons-closure
      (rtl:make-entry:procedure (procedure-label procedure))
      min
      max
      (procedure-closure-size procedure)))))

(define (with-procedure-arity-encoding procedure receiver)
  (let* ((min (1+ (length (procedure-required-arguments procedure))))
	 (max (+ min (length (procedure-optional procedure)))))
    (receiver min
	      (if (procedure-rest procedure)
		  (- (1+ max))
		  max))))
