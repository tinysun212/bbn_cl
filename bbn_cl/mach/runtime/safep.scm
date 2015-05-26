#| -*-Scheme-*-

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

;;;; SCode Optimizer: Safety Analysis

(declare (usual-integrations))

(define (safe?/expressions expressions)
  (if (null? expressions)
      true
      (and (safe?/expression (car expressions))
	   (safe?/expressions (cdr expressions)))))

(define (safe?/expression expression)
  ((expression/method dispatch-vector expression) expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/safe?
  (expression/make-method-definer dispatch-vector))

(define-method/safe? 'ACCESS
  (lambda (expression)
    (safe?/expression (access/environment expression))))

(define-method/safe? 'ASSIGNMENT
  (lambda (expression)
    (safe?/expression (assignment/value expression))))

(define-method/safe? 'COMBINATION
  (lambda (expression)
    (and (safe?/expression (combination/operator expression))
	 (safe?/expressions (combination/operands expression)))))

(define-method/safe? 'CONDITIONAL
  (lambda (expression)
    (and (safe?/expression (conditional/predicate expression))
	 (safe?/expression (conditional/consequent expression))
	 (safe?/expression (conditional/alternative expression)))))

(define-method/safe? 'CONSTANT
  (lambda (expression)
    true))

(define-method/safe? 'DECLARATION
  (lambda (expression)
    (safe?/expression (declaration/expression expression))))

(define-method/safe? 'DELAY
  (lambda (expression)
    (safe?/expression (delay/expression expression))))

(define-method/safe? 'DISJUNCTION
  (lambda (expression)
    (and (safe?/expression (disjunction/predicate expression))
	 (safe?/expression (disjunction/alternative expression)))))

(define-method/safe? 'IN-PACKAGE
  (lambda (expression)
    (safe?/expression (in-package/environment expression))))

(define-method/safe? 'PROCEDURE
  (lambda (expression)
    (block/safe? (procedure/block expression))))

(define-method/safe? 'OPEN-BLOCK
  (lambda (expression)
    (and (safe?/expressions (open-block/values expression))
	 (let loop ((actions (open-block/actions expression)))
	   (or (null? actions)
	       (and (or (eq? (car actions) open-block/value-marker)
			(safe?/expression (car actions)))
		    (loop (cdr actions))))))))

(define-method/safe? 'QUOTATION
  (lambda (expression)
    true))

(define-method/safe? 'REFERENCE
  (lambda (expression)
    true))

(define-method/safe? 'SEQUENCE
  (lambda (expression)
    (safe?/expressions (sequence/actions expression))))

(define-method/safe? 'THE-ENVIRONMENT
  (lambda (expression)
    false))
