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
;;;;;; (sf/set-file-syntax-table! "bbn-inline-code.scm" lap-generator-syntax-table)
;;;
;;;	(%ge compiler-package)
;;;	(%gst lap-generator-syntax-table)

;;; NOTE!!!!!!! The $split-file markers in this file are NOT
;;;             for size reasons but are to divide the file into parts that may
;;;             be loaded into individual environments.
;;;             The reason for this is to keep these changes
;;;             centralized until they stabilize. See machines/butterfly/make.scm
;;;             for the loading protocol. -las

;;; This first piece is loaded into the compiler-package

  (define (rtl:make-fixnum:equal expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-fixnum:equal expression-1 expression-2))))))

  (define (rtl:make-fixnum:greater-than expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-fixnum:greater-than expression-1 expression-2))))))

  (define (rtl:make-fixnum:less-than expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-fixnum:less-than expression-1 expression-2))))))

  (define (rtl:make-fixnum:zero? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-fixnum:zero? expression))))

  (define (rtl:make-fixnum:negative? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-fixnum:negative? expression))))

  (define (rtl:make-fixnum:positive? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-fixnum:positive? expression))))

  (define (rtl:make-flonum:equal expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-flonum:equal expression-1 expression-2))))))

  (define (rtl:make-flonum:greater-than expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-flonum:greater-than expression-1 expression-2))))))

  (define (rtl:make-flonum:less-than expression-1 expression-2)
    (expression-simplify-for-predicate
     expression-1
     (lambda (expression-1)
       (expression-simplify-for-predicate
	expression-2
	(lambda (expression-2)
	  (%make-flonum:less-than expression-1 expression-2))))))

  (define (rtl:make-flonum:zero? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-flonum:zero? expression))))

  (define (rtl:make-flonum:negative? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-flonum:negative? expression))))

  (define (rtl:make-flonum:positive? expression)
    (expression-simplify-for-predicate
     expression
     (lambda (expression)
       (%make-flonum:positive? expression))))

  (define (rtl:expression-cost expression)
    ;; Returns an estimate of the cost of evaluating the expression.
    ;; For simplicity, we try to estimate the actual number of cycles
    ;; that a typical code sequence would produce.
    (case (rtl:expression-type expression)
      ((ASSIGNMENT-CACHE VARIABLE-CACHE) 16) ;move.l d(pc),reg
      ((CONS-POINTER)
       ;; Best case = 12 cycles, worst =  44
       ;; move.l reg,d(reg) = 16
       ;; move.b reg,d(reg) = 12
       ;; move.l d(reg),reg = 16
       (+ 30
	  (rtl:expression-cost (rtl:cons-pointer-type expression))
	  (rtl:expression-cost (rtl:cons-pointer-datum expression))))
      ((CONSTANT)
       (let ((value (cadr expression)))
	 (cond ((false? value) 4)		;clr.l reg
	       ((or (eq? value true)
		    (char? value)
		    (and (integer? value)
			 (<= -#x80000000 value #x7FFFFFFF)))
		12)			;move.l #...,reg
	       (else 16))))		;move.l d(pc),reg
      ;; lea d(pc),reg       =  8
      ;; move.l reg,d(reg)   = 16
      ;; move.b #type,d(reg) = 16
      ;; move.l d(reg),reg   = 16

      ((FIXNUM:MULTIPLY)  ; I question whether this is correct
       (+ 16 (rtl:expression-cost (rtl:fixnum:multiply-multiplier expression))
	  (rtl:expression-cost (rtl:fixnum:multiply-multiplicand expression))))
      ((FIXNUM:PLUS)
       (+ 16 (rtl:expression-cost (rtl:fixnum:plus-addend expression))
	  (rtl:expression-cost (rtl:fixnum:plus-addend expression))))
      ((FIXNUM:MINUS)
       (+ 16 (rtl:expression-cost (rtl:fixnum:minus-addend expression))
	  (rtl:expression-cost (rtl:fixnum:minus-addend expression))))
      ((FLONUM:PLUS)
       (+ 16 (rtl:expression-cost (rtl:flonum:plus-addend expression))
	  (rtl:expression-cost (rtl:flonum:plus-addend expression))))
      ((FLONUM:MINUS)
       (+ 16 (rtl:expression-cost (rtl:flonum:minus-addend expression))
	  (rtl:expression-cost (rtl:flonum:minus-addend expression))))
      ((FLONUM:TIMES)
       (+ 16 (rtl:expression-cost (rtl:flonum:times-addend expression))
	  (rtl:expression-cost (rtl:flonum:times-addend expression))))
      ((FLONUM:DIVIDE)
       (+ 16 (rtl:expression-cost (rtl:flonum:divide-addend expression))
	  (rtl:expression-cost (rtl:flonum:divide-addend expression))))
      ((FIXNUM:-1+)
       (+ 16 (rtl:expression-cost (rtl:fixnum:-1+-value expression))))
      ((FIXNUM:1+)
       (+ 16 (rtl:expression-cost (rtl:fixnum:1+-value expression))))
      ((FIXNUM:LESS-THAN)
       (+ 16 (rtl:expression-cost (rtl:fixnum:less-than-first expression))
	  (rtl:expression-cost (rtl:fixnum:less-than-second expression))))
      ((FIXNUM:GREATER-THAN)
       (+ 16 (rtl:expression-cost (rtl:fixnum:greater-than-first expression))
	  (rtl:expression-cost (rtl:fixnum:greater-than-second expression))))
      ((FIXNUM:EQUAL)
       (+ 16 (rtl:expression-cost (rtl:fixnum:equal-first expression))
	  (rtl:expression-cost (rtl:fixnum:equal-second expression))))
      ((FLONUM:LESS-THAN)
       (+ 16 (rtl:expression-cost (rtl:flonum:less-than-first expression))
	  (rtl:expression-cost (rtl:flonum:less-than-second expression))))
      ((FLONUM:GREATER-THAN)
       (+ 16 (rtl:expression-cost (rtl:flonum:greater-than-first expression))
	  (rtl:expression-cost (rtl:flonum:greater-than-second expression))))
      ((FLONUM:EQUAL)
       (+ 16 (rtl:expression-cost (rtl:flonum:equal-first expression))
	  (rtl:expression-cost (rtl:flonum:equal-second expression))))
      ((FIXNUM:ZERO?)
       (+ 16 (rtl:expression-cost (rtl:fixnum:zero?-value expression))))
      ((FIXNUM:POSITIVE?)
       (+ 16 (rtl:expression-cost (rtl:fixnum:positive?-value expression))))
      ((FIXNUM:NEGATIVE?)
       (+ 16 (rtl:expression-cost (rtl:fixnum:negative?-value expression))))
      ((FLONUM:ZERO?)
       (+ 16 (rtl:expression-cost (rtl:flonum:zero?-value expression))))
      ((FLONUM:NEGATIVE?)
       (+ 16 (rtl:expression-cost (rtl:flonum:negative?-value expression))))
      ((FLONUM:POSITIVE?)
       (+ 16 (rtl:expression-cost (rtl:flonum:positive?-value expression))))
      ((FAST-VECTOR-REF)
       (+ 24 (rtl:expression-cost (rtl:fast-vector-ref-vector expression))
	  (rtl:expression-cost (rtl:fast-vector-ref-index expression))))
      ((CL-FAST-VECTOR-SET!)
       (+ 24 (rtl:expression-cost (rtl:cl-fast-vector-set!-vector expression))
	  (rtl:expression-cost (rtl:cl-fast-vector-set!-index expression))
	  (rtl:expression-cost (rtl:cl-fast-vector-set!-value expression))))
      ((FLONUM:SINE)
       (+ 16 (rtl:expression-cost (rtl:flonum:sine-arg expression))))
      ((FLONUM:COSINE)
       (+ 16 (rtl:expression-cost (rtl:flonum:cosine-arg expression))))
      ((FLONUM:ATAN)
       (+ 16 (rtl:expression-cost (rtl:flonum:atan-arg expression))))
      ((FLONUM:EXP)
       (+ 16 (rtl:expression-cost (rtl:flonum:exp-arg expression))))
      ((FLONUM:LN)
       (+ 16 (rtl:expression-cost (rtl:flonum:ln-arg expression))))
      ((FLONUM:SQRT)
       (+ 16 (rtl:expression-cost (rtl:flonum:sqrt-arg expression))))
      ((FLONUM:TRUNCATE)
       (+ 16 (rtl:expression-cost (rtl:flonum:truncate-arg expression))))
      ((FLONUM:ROUND)
       (+ 16 (rtl:expression-cost (rtl:flonum:round-arg expression))))
      ((FLONUM:COERCE-FIXNUM)
       (+ 16 (rtl:expression-cost (rtl:flonum:coerce-fixnum-arg expression))))
      ((ENTRY:CONTINUATION ENTRY:PROCEDURE) 56)
      ((OBJECT->ADDRESS OBJECT->DATUM) 6)	;and.l d7,reg
      ;; move.l reg,d(reg) = 16
      ;; move.b d(reg),reg = 12
      ((OBJECT->TYPE) 28)
      ((OFFSET) 16)			;move.l d(reg),reg
      ((OFFSET-ADDRESS) 8)		;lea d(an),reg
      ((POST-INCREMENT) 12)		;move.l (reg)+,reg
      ((PRE-INCREMENT) 14)		;move.l -(reg),reg
      ((REGISTER) 4)			;move.l reg,reg
      ((UNASSIGNED) 12)			;move.l #data,reg
      (else (error "Unknown expression type" expression))))

  (define-rtl-expression fixnum:plus % addend augend)
  (define-rtl-expression fixnum:minus % addend augend)
  (define-rtl-expression fixnum:multiply % multiplier multiplicand)
  (define-rtl-expression fixnum:-1+ % value)
  (define-rtl-expression fixnum:1+ % value)
  (define-rtl-expression fast-vector-ref % vector index)
  (define-rtl-expression cl-fast-vector-set! % vector index value)

  (define-rtl-predicate fixnum:less-than % first second)
  (define-rtl-predicate fixnum:greater-than % first second)
  (define-rtl-predicate fixnum:equal % first second)
  (define-rtl-predicate fixnum:zero? % value)
  (define-rtl-predicate fixnum:negative? % value)
  (define-rtl-predicate fixnum:positive? % value)

  (define-rtl-expression flonum:plus % addend augend)
  (define-rtl-expression flonum:minus % addend augend)
  (define-rtl-expression flonum:times % addend augend)
  (define-rtl-expression flonum:divide % addend augend)
  (define-rtl-expression flonum:sine % arg)
  (define-rtl-expression flonum:cosine % arg)
  (define-rtl-expression flonum:atan % arg)
  (define-rtl-expression flonum:exp % arg)
  (define-rtl-expression flonum:ln % arg)
  (define-rtl-expression flonum:sqrt % arg)
  (define-rtl-expression flonum:truncate % arg)
  (define-rtl-expression flonum:round % arg)
  (define-rtl-expression flonum:coerce-fixnum % arg)

  (define-rtl-predicate flonum:less-than % first second)
  (define-rtl-predicate flonum:greater-than % first second)
  (define-rtl-predicate flonum:equal % first second)
  (define-rtl-predicate flonum:zero? % value)
  (define-rtl-predicate flonum:negative? % value)
  (define-rtl-predicate flonum:positive? % value)

'$split-file ;;; This piece is loaded into the rtl-cse-package

  (define-trivial-two-arg-method 'FIXNUM:LESS-THAN
    rtl:fixnum:less-than-first rtl:set-fixnum:less-than-first!
    rtl:fixnum:less-than-second rtl:set-fixnum:less-than-second!)

  (define-trivial-two-arg-method 'FIXNUM:GREATER-THAN
    rtl:fixnum:greater-than-first rtl:set-fixnum:greater-than-first!
    rtl:fixnum:greater-than-second rtl:set-fixnum:greater-than-second!)

  (define-trivial-two-arg-method 'FIXNUM:EQUAL
    rtl:fixnum:equal-first rtl:set-fixnum:equal-first!
    rtl:fixnum:equal-second rtl:set-fixnum:equal-second!)

  (define-trivial-one-arg-method 'FIXNUM:ZERO?
    rtl:fixnum:zero?-value rtl:set-fixnum:zero?-value!)

  (define-trivial-one-arg-method 'FIXNUM:NEGATIVE?
    rtl:fixnum:negative?-value rtl:set-fixnum:negative?-value!)

  (define-trivial-one-arg-method 'FIXNUM:POSITIVE?
    rtl:fixnum:positive?-value rtl:set-fixnum:positive?-value!)

  (define-trivial-two-arg-method 'FLONUM:LESS-THAN
    rtl:flonum:less-than-first rtl:set-flonum:less-than-first!
    rtl:flonum:less-than-second rtl:set-flonum:less-than-second!)

  (define-trivial-two-arg-method 'FLONUM:GREATER-THAN
    rtl:flonum:greater-than-first rtl:set-flonum:greater-than-first!
    rtl:flonum:greater-than-second rtl:set-flonum:greater-than-second!)

  (define-trivial-two-arg-method 'FLONUM:EQUAL
    rtl:flonum:equal-first rtl:set-flonum:equal-first!
    rtl:flonum:equal-second rtl:set-flonum:equal-second!)

  (define-trivial-one-arg-method 'FLONUM:ZERO?
    rtl:flonum:zero?-value rtl:set-flonum:zero?-value!)

  (define-trivial-one-arg-method 'FLONUM:NEGATIVE?
    rtl:flonum:negative?-value rtl:set-flonum:negative?-value!)

  (define-trivial-one-arg-method 'FLONUM:POSITIVE?
    rtl:flonum:positive?-value rtl:set-flonum:positive?-value!)

  (define-cse-method 'CL-FAST-VECTOR-SET! method/noop)

'$split-file ;;; This piece is loaded into the rtl-method-package

  (define-expression-method 'FIXNUM:-1+
    (lambda (receiver scfg-append! value)
      (expression-simplify*
       value scfg-append!
       (lambda (value)
	 (receiver (%make-fixnum:-1+ value))))))

  (define-expression-method 'FIXNUM:1+
    (lambda (receiver scfg-append! value)
      (expression-simplify*
       value scfg-append!
       (lambda (value)
	 (receiver (%make-fixnum:1+ value))))))

  (define-expression-method 'FIXNUM:PLUS
    (lambda (receiver scfg-append! addend augend)
      (expression-simplify*
       addend scfg-append!
       (lambda (addend)
	 (expression-simplify*
	  augend scfg-append!
	  (lambda (augend)
	    (receiver (%make-fixnum:plus addend augend))))))))

  (define-expression-method 'FIXNUM:MINUS
    (lambda (receiver scfg-append! addend augend)
      (expression-simplify*
       addend scfg-append!
       (lambda (addend)
	 (expression-simplify*
	  augend scfg-append!
	  (lambda (augend)
	    (receiver (%make-fixnum:minus addend augend))))))))

  (define-expression-method 'FIXNUM:MULTIPLY
    (lambda (receiver scfg-append! multiplier multiplicand)
      (expression-simplify*
       multiplier scfg-append!
       (lambda (multiplier)
	 (expression-simplify*
	  multiplicand scfg-append!
	  (lambda (multiplicand)
	    (receiver (%make-fixnum:multiply multiplier multiplicand))))))))

  (define-expression-method 'FAST-VECTOR-REF
    (lambda (receiver scfg-append! vector index)
      (expression-simplify*
       vector scfg-append!
       (lambda (vector)
	 (expression-simplify*
	  index scfg-append!
	  (lambda (index)
	    (receiver (%make-fast-vector-ref vector index))))))))

  (define-expression-method 'CL-FAST-VECTOR-SET!
    (lambda (receiver scfg-append! vector index value)
      (expression-simplify*
       vector scfg-append!
       (lambda (vector)
	 (expression-simplify*
	  index scfg-append!
	  (lambda (index)
	    (expression-simplify*
	     value scfg-append!
	     (lambda (value)
	       (receiver (%make-cl-fast-vector-set! vector index value))))))))))

  (define (define-binary-flonum-method name maker)
    (define-expression-method name
      (lambda (receiver scfg-append! first second)
	(expression-simplify*
	 first scfg-append!
	 (lambda (first)
	   (expression-simplify*
	    second scfg-append!
	    (lambda (second)
	      (receiver (maker first second)))))))))

  (define-binary-flonum-method 'FLONUM:PLUS %make-flonum:plus)
  (define-binary-flonum-method 'FLONUM:MINUS %make-flonum:minus)
  (define-binary-flonum-method 'FLONUM:TIMES %make-flonum:times)
  (define-binary-flonum-method 'FLONUM:DIVIDE %make-flonum:divide)

  (define (define-unary-flonum-method name maker)
    (define-expression-method name
      (lambda (receiver scfg-append! first)
	(expression-simplify*
	 first scfg-append!
	 (lambda (first)
	   (receiver (maker first)))))))

  (define-unary-flonum-method 'FLONUM:SINE %make-flonum:sine)
  (define-unary-flonum-method 'FLONUM:COSINE %make-flonum:cosine)
  (define-unary-flonum-method 'FLONUM:ATAN %make-flonum:atan)
  (define-unary-flonum-method 'FLONUM:EXP %make-flonum:exp)
  (define-unary-flonum-method 'FLONUM:LN %make-flonum:ln)
  (define-unary-flonum-method 'FLONUM:SQRT %make-flonum:sqrt)
  (define-unary-flonum-method 'FLONUM:COERCE-FIXNUM %make-flonum:coerce-fixnum)
  (define-unary-flonum-method 'FLONUM:TRUNCATE %make-flonum:truncate)
  (define-unary-flonum-method 'FLONUM:ROUND %make-flonum:round)

'$split-file ;;; This piece is loaded into (access open-coder-package rtl-generator-package)

  (define-open-coder/predicate 'ZERO-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:zero? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'POSITIVE-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:positive? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'NEGATIVE-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:negative? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'LESS-THAN-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:less-than (car expressions)
					    (cadr expressions))))
       '(0 1))))

  (define-open-coder/predicate 'GREATER-THAN-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:greater-than (car expressions)
					       (cadr expressions))))
       '(0 1))))

  (define-open-coder/predicate 'EQUAL-FIXNUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-fixnum:equal (car expressions)
					(cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'PLUS-FIXNUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-fixnum:plus (car expressions)
				    (cadr expressions))))
       '(0 1))))
  
  (define-open-coder/value 'MINUS-FIXNUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-fixnum:minus (car expressions)
				     (cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'MULTIPLY-FIXNUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-fixnum:multiply (car expressions)
					(cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'MINUS-ONE-PLUS-FIXNUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-fixnum:-1+ (car expressions))))
       '(0))))

  (define-open-coder/value 'ONE-PLUS-FIXNUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-fixnum:1+ (car expressions))))
       '(0))))

  (define-open-coder/value 'SYSTEM-VECTOR-REF
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (let ((temp (rtl:make-pseudo-register)))
	   (scfg-append!
	    (rtl:make-assignment temp 
				 (%make-fast-vector-ref (car expressions)
							(cadr expressions)))
	    (finish (rtl:make-fetch temp)))))
       '(0 1))))

  (define-open-coder/effect '(CL-SYSTEM-VECTOR-SET! CL-VECTOR-SET!)
    (lambda (operands)
      (let ((proc 
	     (lambda (expressions finish)
	       (let ((temp1 (rtl:make-pseudo-register))
		     (temp2 (rtl:make-pseudo-register))
		     (temp3 (rtl:make-pseudo-register)))
		 (if finish
		     (scfg-append!
			(rtl:make-assignment temp1 (car expressions))
			(rtl:make-assignment temp2 (cadr expressions))
			(rtl:make-assignment temp3 (caddr expressions))
			(node->scfg
			 (make-sblock
			  (make-rtl-instruction
			   `(cl-fast-vector-set!
			     ,temp1
			     ,temp2
			     ,temp3)))
			 set-snode-next-edge!)
			(finish (rtl:make-fetch temp3)))
		     (scfg-append!
			(rtl:make-assignment temp1 (car expressions))
			(rtl:make-assignment temp2 (cadr expressions))
			(rtl:make-assignment temp3 (caddr expressions))
			(node->scfg
			 (make-sblock
			  (make-rtl-instruction
			   `(cl-fast-vector-set!
			     ,temp1
			     ,temp2
			     ,temp3)))
			 set-snode-next-edge!)))))))
	(return-2 proc '(0 1 2)))))

  (define-open-coder/predicate 'ZERO-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:zero? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'POSITIVE-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:positive? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'NEGATIVE-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:negative? (car expressions))))
       '(0))))

  (define-open-coder/predicate 'LESS-THAN-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:less-than (car expressions)
					    (cadr expressions))))
       '(0 1))))

  (define-open-coder/predicate 'GREATER-THAN-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:greater-than (car expressions)
					       (cadr expressions))))
       '(0 1))))

  (define-open-coder/predicate 'EQUAL-FLONUM?
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (rtl:make-flonum:equal (car expressions)
					(cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'PLUS-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:plus (car expressions)
				    (cadr expressions))))
       '(0 1))))
  
  (define-open-coder/value 'MINUS-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:minus (car expressions)
				     (cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'MULTIPLY-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:times (car expressions)
				     (cadr expressions))))
       '(0 1))))

  (define-open-coder/value 'DIVIDE-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:divide (car expressions)
				      (cadr expressions))))
       '(0 1)))) 

  (define-open-coder/value 'SINE-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:sine (car expressions))))
       '(0))))

  (define-open-coder/value 'COSINE-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:cosine (car expressions))))
       '(0))))

  (define-open-coder/value 'ATAN-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:atan (car expressions))))
       '(0))))

  (define-open-coder/value 'EXP-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:exp (car expressions))))
       '(0))))

  (define-open-coder/value 'LN-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:ln (car expressions))))
       '(0))))

  ;; No primitive for this; pass arity

  (define-open-coder/value 'COERCE-FIXNUM-TO-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:coerce-fixnum (car expressions))))
       '(0)))
    1)

  (define-open-coder/value 'TRUNCATE-FLONUM
    (lambda (operands)
      (return-2
       (lambda (expressions finish)
	 (finish (%make-flonum:truncate (car expressions))))
       '(0))))

'$split-file ;;; This piece is loaded into lap-syntax-package

  (define *fixnum-prefix-instructions*)

  (define (appending-prefix-instructions action)
    (fluid-let ((*fixnum-prefix-instructions* (LAP)))
      (let ((new-code (action)))
	(LAP ,@*fixnum-prefix-instructions*
	     ,@new-code))))

  (define (rtl->inst-ea rtl)
    (cond ((eq? (car rtl) 'OFFSET)
	   (indirect-reference! (cadadr rtl) (caddr rtl)))
	  ((eq? (car rtl) 'REGISTER)
	   (coerce->any (cadr rtl)))
	  ((eq? (car rtl) 'CONS-POINTER)
	   (coerce->any (cadr (caddr rtl))))
	  ((eq? (car rtl) 'CONSTANT)
	   (INST-EA (& ,(cadr rtl))))
	  ((eq? (car rtl) 'PRE-INCREMENT)
	   (INST-EA (@-A ,(- (cadadr rtl) 8))))
	  ((eq? (car rtl) 'POST-INCREMENT)
	   (INST-EA (@A+ ,(- (cadadr rtl) 8))))
	  ((eq? (car rtl) 'FAST-VECTOR-REF)
	   (vector-ref->inst-ea (cadr rtl) (caddr rtl)))))

  (define (sign-test/memory test-mode flip-sign? rtl)
    (set-standard-branches! test-mode)
    (let ((temp (reference-temporary-register! 'DATA)))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea rtl) ,temp)
	      (LS L L (& 8) ,temp)
	      ,@(if flip-sign? (LAP (NEG L , temp))))))))

;;;;	-> Zero

  (define-rule predicate
    (FIXNUM:ZERO? (? rtl))
    (sign-test/memory 'EQ #f rtl))

;;;;	-> Positive

  (define-rule predicate
    (FIXNUM:POSITIVE? (? rtl))
    (sign-test/memory 'MI #t rtl))

;;;;	-> Negative

  (define-rule predicate
    (FIXNUM:NEGATIVE? (? rtl))
    (sign-test/memory 'MI #f rtl))

;;;;	FIXNUM COMPARISON

  (define (fixnum-comparison-test test-mode item1 item2)
    (set-standard-branches! test-mode)
    (let ((temp1 (reference-temporary-register! 'DATA))
	  (temp2 (reference-temporary-register! 'DATA)))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea item1) ,temp1)
	      (LS L L (& 8) ,temp1)
	      (MOV L ,(rtl->inst-ea item2) ,temp2)
	      (LS L L (& 8) ,temp2)
	      (CMP L ,temp2 ,temp1))))))

;;;;	-> Less Than

  (define-rule predicate
    (FIXNUM:LESS-THAN (? part1) (? part2))
    (fixnum-comparison-test 'LT part1 part2))

;;;;	-> Greater Than

  (define-rule predicate
    (FIXNUM:GREATER-THAN (? part1) (? part2))
    (fixnum-comparison-test 'GT part1 part2))

;;;;	-> Equal To

  (define-rule predicate
    (FIXNUM:EQUAL (? part1) (? part2))
    (fixnum-comparison-test 'EQ part1 part2))

;;;; INCREMENT AND DECREMENT

  (define (unary-operate-with-one operation source target1 target2)
    (appending-prefix-instructions
     (lambda ()
       (LAP (MOV L ,(rtl->inst-ea source) ,target1)
	    (,operation L (& 1) ,target2)
	    (MOV B (& #x1a) ,target2)))))

;;;;	-> Plus One

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FIXNUM:1+ (? source)))
    (unary-operate-with-one 'ADDQ source
			    (INST-EA (@-A ,(- r 8)))
			    (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FIXNUM:1+ (? source)))
    (unary-operate-with-one 'ADDQ source
			    (INST-EA (@A+ ,(- r 8)))
			    (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FIXNUM:1+ (? source)))
    (unary-operate-with-one 'ADDQ source
			    (indirect-reference! r n)
			    (indirect-reference! r n)))

  (define-rule statement
    (ASSIGN (REGISTER (? target))
	    (FIXNUM:1+ (? source)))
    (QUALIFIER (pseudo-register? target))
    (let ((operation
	   (unary-operate-with-one 'ADDQ source reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp ,(coerce->any target)))))

;;;;	-> Minus One

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FIXNUM:-1+ (? source)))
    (unary-operate-with-one 'SUBQ source
			    (INST-EA (@-A ,(- r 8)))
			    (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FIXNUM:-1+ (? source)))
    (unary-operate-with-one 'SUBQ source
			    (INST-EA (@A+ ,(- r 8)))
			    (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FIXNUM:-1+ (? source)))
    (unary-operate-with-one 'SUBQ source
			    (indirect-reference! r n)
			    (indirect-reference! r n)))

  (define-rule statement
    (ASSIGN (REGISTER (? target))
	    (FIXNUM:-1+ (? source)))
    (QUALIFIER (pseudo-register? target))
    (let ((operation
	   (unary-operate-with-one 'SUBQ source reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp ,(coerce->any target)))))

;;;;   ADDITION AND SUBTRACTION

  (define (binary-op-to-memory operation first second target1 target2)
    (let ((temp (register-reference (allocate-temporary-register! 'DATA))))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea first) ,temp)
	      (,operation L ,(rtl->inst-ea second) ,temp)
	      (MOV L ,temp ,target1)
	      (MOV B (& #x1a) ,target2))))))

;;;;	-> Addition

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FIXNUM:PLUS (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (binary-op-to-memory 'ADD part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FIXNUM:PLUS (? part1) (? part2)))
    (binary-op-to-memory 'ADD part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FIXNUM:PLUS (? part1) (? part2)))
    (binary-op-to-memory 'ADD part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FIXNUM:PLUS (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (binary-op-to-memory 'ADD part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp ,(coerce->any r)))))

;;;;	-> Subtraction

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FIXNUM:MINUS (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (binary-op-to-memory 'SUB part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FIXNUM:MINUS (? part1) (? part2)))
    (binary-op-to-memory 'SUB part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FIXNUM:MINUS (? part1) (? part2)))
    (binary-op-to-memory 'SUB part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FIXNUM:MINUS (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (binary-op-to-memory 'SUB part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp ,(coerce->any r)))))

;;;;   MULTIPLICATION

  (define (multiply-to-memory first second target1 target2)
    (let ((temp1 (register-reference (allocate-temporary-register! 'DATA)))
	  (temp2 (register-reference (allocate-temporary-register! 'DATA))))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea first) ,temp1)
	      (MOV L ,(rtl->inst-ea second) ,temp2)
	      (LS L L (& 8) ,temp1)
	      (LS L L (& 8) ,temp2)
	      (AS R L (& 8) ,temp1)
	      (AS R L (& 8) ,temp2)
	      (MUL S L ,temp1 ,temp2)
	      (MOV L ,temp1 ,target1)
	      (MOV B (& #x1a) ,target2))))))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FIXNUM:MULTIPLY (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (multiply-to-memory part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FIXNUM:MULTIPLY (? part1) (? part2)))
    (multiply-to-memory part1 part2
			(INST-EA (@-A ,(- r 8)))
			(INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FIXNUM:MULTIPLY (? part1) (? part2)))
    (multiply-to-memory part1 part2
			(INST-EA (@A+ ,(- r 8)))
			(INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FIXNUM:MULTIPLY (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (multiply-to-memory part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp ,(coerce->any r)))))

;;; -> CL-FAST-VECTOR-SET!

(define (emit-cl-fast-vector-set! vector index value)
  (let ((value 
	 (if (rtl:constant? value)
	     (let ((temp (reference-temporary-register! 'DATA)))
	       (set! *fixnum-prefix-instructions*
		     (LAP ,@*fixnum-prefix-instructions*
			  ,(load-constant (cadr value) temp)))
	       temp)
	     (rtl->inst-ea value))))
    (let ((temp1 (reference-temporary-register! 'DATA)))
      (if (rtl:constant? index)
	  (if (rtl:constant? vector)
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   ,(load-constant (cadr vector) temp1)
			   (AND L ,mask-reference ,temp1)
			   (MOV L ,value
				  (@DO ,(cadr temp1) ,(* (1+ (cadr index)) 4)))))
		'())
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea vector) ,temp1)
			   (AND L ,mask-reference ,temp1)
			   (MOV L ,value
				  (@DO ,(cadr temp1) ,(* (1+ (cadr index)) 4)))))
		'()))
	  (if (rtl:constant? vector)
	      (let ((temp2 (reference-temporary-register! 'DATA)))
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea index) ,temp1)
			   (LS L L (& 2) ,temp1)
			   ,(load-constant (cadr vector) temp2)
			   (ADD L ,temp2 ,temp1)
			   (AND L ,mask-reference ,temp1)
			   (MOV L ,value (@DO ,(cadr temp1) 4))))
		'())
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea index) ,temp1)
			   (LS L L (& 2) ,temp1)
			   (ADD L ,(rtl->inst-ea vector) ,temp1)
			   (AND L ,mask-reference ,temp1)
			   (MOV L ,value (@DO ,(cadr temp1) 4))))
		'()))))))

  (define-rule statement
    (CL-FAST-VECTOR-SET! (? vector) (? index) (? value))
    (appending-prefix-instructions
     (lambda ()
       (emit-cl-fast-vector-set! vector index value))))

;;;;	FAST VECTOR REF via SYSTEM-VECTOR-REF

(define (vector-ref->inst-ea vector index)
    (let ((temp1 (reference-temporary-register! 'DATA)))
      (if (eq? (car index) 'CONSTANT)
	  (if (eq? (car vector) 'CONSTANT)
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   ,(load-constant (cadr vector) temp1)
			   (AND L ,mask-reference ,temp1)))
		(INST-EA (@DO ,(cadr temp1) ,(* (1+ (cadr index)) 4))))
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea vector) ,temp1)
			   (AND L ,mask-reference ,temp1)))
		(INST-EA (@DO ,(cadr temp1) ,(* (1+ (cadr index)) 4)))))
	  (if (eq? (car vector) 'CONSTANT)
	      (let ((temp2 (reference-temporary-register! 'DATA)))
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea index) ,temp1)
			   (LS L L (& 2) ,temp1)
			   ,(load-constant (cadr vector) temp2)
			   (ADD L ,temp2 ,temp1)
			   (AND L ,mask-reference ,temp1)))
		(INST-EA (@DO ,(cadr temp1) 4)))
	      (begin
		(set! *fixnum-prefix-instructions*
		      (LAP ,@*fixnum-prefix-instructions*
			   (MOV L ,(rtl->inst-ea index) ,temp1)
			   (LS L L (& 2) ,temp1)
			   (ADD L ,(rtl->inst-ea vector) ,temp1)
			   (AND L ,mask-reference ,temp1)))
		(INST-EA (@DO ,(cadr temp1) 4)))))))

;;;  (@AOXS <a-reg-#> <offset> ((D <d-reg-#>) L <factor>))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FAST-VECTOR-REF (? vector) (? index)))
    (appending-prefix-instructions
     (lambda ()
       (LAP (MOV L
		 ,(vector-ref->inst-ea vector index)
		 ,(indirect-reference! r n))))))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FAST-VECTOR-REF (? vector) (? index)))
    (appending-prefix-instructions
     (lambda ()
       (LAP (MOV L
		 ,(vector-ref->inst-ea vector index)
		 (@-A ,(- r 8)))))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FAST-VECTOR-REF (? vector) (? index)))
    (appending-prefix-instructions
     (lambda ()
       (LAP (MOV L
		 ,(vector-ref->inst-ea vector index)
		 (@A+ ,(- r 8)))))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FAST-VECTOR-REF (? vector) (? index)))
    (QUALIFIER (pseudo-register? r))
    (appending-prefix-instructions
     (lambda ()
       (let ((source (vector-ref->inst-ea vector index)))
	 (delete-dead-registers!)
	 (LAP (MOV L
		   ,source
		   ,(coerce->any r)))))))

  (define-rule predicate
    (TRUE-TEST (FAST-VECTOR-REF (? vector) (? index)))
    (set-standard-branches! 'NE)
    (appending-prefix-instructions
     (lambda ()
       (LAP (TST L ,(vector-ref->inst-ea vector index))))))

  (define (set-floating-branches! cc)
    (set-current-branches!
     (lambda (label)
       (LAP (FB ,cc (@PCR ,label))))
     (lambda (label)
       (LAP (FB ,(invert-cc cc) (@PCR ,label))))))

  (define (floating-sign-test/memory test-mode flip-sign? rtl)
    (set-floating-branches! test-mode)
    (let ((temp (reference-temporary-register! 'DATA)))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea rtl) ,temp)
	      (AND L ,mask-reference ,temp)
	      (FMOVE D (@DO ,(cadr temp) 4) FP0)
	      ,@(if flip-sign? (LAP (FNEG FP0)) ()))))))
  
;;;;	-> Zero

  (define-rule predicate
    (FLONUM:ZERO? (? rtl))
    (floating-sign-test/memory 'EQ #f rtl))

;;;;	-> Positive

  (define-rule predicate
    (FLONUM:POSITIVE? (? rtl))
    (floating-sign-test/memory 'MI #t rtl))

;;;;	-> Negative

  (define-rule predicate
    (FLONUM:NEGATIVE? (? rtl))
    (floating-sign-test/memory 'MI #f rtl))

;;;;	FLONUM COMPARISON

  (define (flonum-comparison-test test-mode item1 item2)
    (set-floating-branches! test-mode)
    (let ((temp (reference-temporary-register! 'DATA)))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea item1) ,temp)
	      (AND L ,mask-reference ,temp)
	      (FMOVE D (@DO ,(cadr temp) 4) FP0)
	      (MOV L ,(rtl->inst-ea item2) ,temp)
	      (AND L ,mask-reference ,temp)
	      (FCMP D (@DO ,(cadr temp) 4) FP0))))))

;;;;	-> Less Than

  (define-rule predicate
    (FLONUM:LESS-THAN (? part1) (? part2))
    (flonum-comparison-test 'LT part1 part2))

;;;;	-> Greater Than

  (define-rule predicate
    (FLONUM:GREATER-THAN (? part1) (? part2))
    (flonum-comparison-test 'GT part1 part2))

;;;;	-> Equal To

  (define-rule predicate
    (FLONUM:EQUAL (? part1) (? part2))
    (flonum-comparison-test 'EQ part1 part2))

;;;;	SINE, COSINE, ATAN, EXP, LN, SQRT

  (define (store-flonum freg target1 target2)
    (LAP (MOV L (A 5) ,target1)
	 (MOV B (& #x06) ,target2)
	 (MOV L (& #x27000002) (@A+ 5))
	 (FMOVE D ,freg (@A 5))
	 (ADDQ L (& 8) (A 5))))

  (define (floating-unary-op-to-memory operation arg target1 target2)
    (let ((temp (register-reference (allocate-temporary-register! 'DATA))))
      (appending-prefix-instructions
       (lambda ()
	 (LAP (MOV L ,(rtl->inst-ea arg) ,temp)
	      (AND L ,mask-reference ,temp)
	      (,operation D (@DO ,(cadr temp) 4) FP0)
	      ,@(store-flonum 'FP0 target1 target2))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:SINE (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FSIN rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:SINE (? rtl)))
    (floating-unary-op-to-memory 'FSIN rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:SINE (? rtl)))
    (floating-unary-op-to-memory 'FSIN rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:SINE (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FSIN rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:COSINE (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FCOS rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:COSINE (? rtl)))
    (floating-unary-op-to-memory 'FCOS rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:COSINE (? rtl)))
    (floating-unary-op-to-memory 'FCOS rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:COSINE (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FCOS rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:ATAN (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FATAN rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:ATAN (? rtl)))
    (floating-unary-op-to-memory 'FATAN rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:ATAN (? rtl)))
    (floating-unary-op-to-memory 'FATAN rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:ATAN (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FATAN rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:EXP (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FETOX rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:EXP (? rtl)))
    (floating-unary-op-to-memory 'FETOX rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:EXP (? rtl)))
    (floating-unary-op-to-memory 'FETOX rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:EXP (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FETOX rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:LN (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FLOGX rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:LN (? rtl)))
    (floating-unary-op-to-memory 'FLOGX rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:LN (? rtl)))
    (floating-unary-op-to-memory 'FLOGX rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:LN (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FLOGX rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
  
  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:SQRT (? rtl)))
    (let ((target (indirect-reference! r n)))
      (floating-unary-op-to-memory 'FSQRT rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:SQRT (? rtl)))
    (floating-unary-op-to-memory 'FSQRT rtl
				 (INST-EA (@-A ,(- r 8)))
				 (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:SQRT (? rtl)))
    (floating-unary-op-to-memory 'FSQRT rtl
				 (INST-EA (@A+ ,(- r 8)))
				 (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:SQRT (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-unary-op-to-memory 'FSQRT rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))

;;;;	TRUNCATE-FLONUM

  (define (do-truncate-flonum rtl target1 target2)
    (let ((temp (register-reference (allocate-temporary-register! 'DATA))))
      (LAP (MOV L ,(rtl->inst-ea rtl) ,temp)
	   (AND L ,mask-reference ,temp)
	   (FMOVE D (@DO ,(cadr temp) 4) FP0)
	   (FINTRZ FP0)
	   (FMOVE L FP0 ,target1)
	   (MOV B (& #x1a) ,target2))))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:TRUNCATE (? rtl)))
    (let ((target (indirect-reference! r n)))
      (do-truncate-flonum rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:TRUNCATE (? rtl)))
    (do-truncate-flonum rtl
		       (INST-EA (@-A ,(- r 8)))
		       (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:TRUNCATE (? rtl)))
    (do-truncate-flonum rtl
		       (INST-EA (@A+ ,(- r 8)))
		       (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:TRUNCATE (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (do-truncate-flonum rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))


;;;;	COERCE-FIXNUM-TO-FLONUM

  (define (do-coerce-fixnum rtl target1 target2)
    (let ((temp (register-reference (allocate-temporary-register! 'DATA))))
      (LAP (MOV L ,(rtl->inst-ea rtl) ,temp)
	   (AND L ,mask-reference ,temp)
	   (LS L L (& 8) ,temp)
	   (AS R L (& 8) ,temp)
	   (FMOVE L ,temp  FP0)
	   ,@(store-flonum 'FP0 target1 target2))))

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:COERCE-FIXNUM (? rtl)))
    (let ((target (indirect-reference! r n)))
      (do-coerce-fixnum rtl target target)))
  
  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:COERCE-FIXNUM (? rtl)))
    (do-coerce-fixnum rtl
		      (INST-EA (@-A ,(- r 8)))
		      (INST-EA (@A ,(- r 8)))))
  
  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:COERCE-FIXNUM (? rtl)))
    (do-coerce-fixnum rtl
		      (INST-EA (@A+ ,(- r 8)))
		      (INST-EA (@AO ,(- r 8) -4))))
  
  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:COERCE-FIXNUM (? rtl)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (do-coerce-fixnum rtl reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))

;;;;   ADDITION, SUBTRACTION, MULTIPLICATION, DIVISION

  (define (floating-binary-op-to-memory operation first second target1 target2)
    (let ((temp (register-reference (allocate-temporary-register! 'DATA))))
      (appending-prefix-instructions
       (lambda ()
	 (LAP ,(if (and (pair? first)
			(eq? (car first) 'CONSTANT))
		   (load-constant (cadr first) temp)
		   (INST (MOV L ,(rtl->inst-ea first) ,temp)))
	      (AND L ,mask-reference ,temp)
	      (FMOVE D (@DO ,(cadr temp) 4) FP0)
	      ,(if (and (pair? second)
			(eq? (car second) 'CONSTANT))
		   (load-constant (cadr second) temp)
		   (INST (MOV L ,(rtl->inst-ea second) ,temp)))
	      (AND L ,mask-reference ,temp)
	      (,operation D (@DO ,(cadr temp) 4) FP0)
	      ,@(store-flonum 'FP0 target1 target2))))))

;;;;	-> Addition

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:PLUS (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (floating-binary-op-to-memory 'FADD part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:PLUS (? part1) (? part2)))
    (floating-binary-op-to-memory 'FADD part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:PLUS (? part1) (? part2)))
    (floating-binary-op-to-memory 'FADD part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:PLUS (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-binary-op-to-memory 'FADD part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))

;;;;	-> Subtraction

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:MINUS (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (floating-binary-op-to-memory 'FSUB part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:MINUS (? part1) (? part2)))
    (floating-binary-op-to-memory 'FSUB part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:MINUS (? part1) (? part2)))
    (floating-binary-op-to-memory 'FSUB part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:MINUS (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-binary-op-to-memory 'FSUB part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))

;;;;	-> Multiplication

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:TIMES (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (floating-binary-op-to-memory 'FMUL part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:TIMES (? part1) (? part2)))
    (floating-binary-op-to-memory 'FMUL part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:TIMES (? part1) (? part2)))
    (floating-binary-op-to-memory 'FMUL part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:TIMES (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-binary-op-to-memory 'FMUL part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))

;;;;	-> Divison

  (define-rule statement
    (ASSIGN (OFFSET (REGISTER (? r)) (? n))
	    (FLONUM:DIVIDE (? part1) (? part2)))
    (let ((target (indirect-reference! r n)))
      (floating-binary-op-to-memory 'FDIV part1 part2 target target)))

  (define-rule statement
    (ASSIGN (PRE-INCREMENT (REGISTER (? r)) -1)
	    (FLONUM:DIVIDE (? part1) (? part2)))
    (floating-binary-op-to-memory 'FDIV part1 part2
			 (INST-EA (@-A ,(- r 8)))
			 (INST-EA (@A ,(- r 8)))))

  (define-rule statement
    (ASSIGN (POST-INCREMENT (REGISTER (? r)) 1)
	    (FLONUM:DIVIDE (? part1) (? part2)))
    (floating-binary-op-to-memory 'FDIV part1 part2
			 (INST-EA (@A+ ,(- r 8)))
			 (INST-EA (@AO ,(- r 8) -4))))

  (define-rule statement
    (ASSIGN (REGISTER (? r))
	    (FLONUM:DIVIDE (? part1) (? part2)))
    (QUALIFIER (pseudo-register? r))
    (let ((operation 
	   (floating-binary-op-to-memory 'FDIV part1 part2 reg:temp reg:temp)))
      (delete-dead-registers!)
      (LAP ,@operation
	   (MOV L ,reg:temp
		,(register-reference (allocate-alias-register! r 'DATA))))))
