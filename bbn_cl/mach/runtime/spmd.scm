;;; -*-Scheme-*-
;;;
;;;	$Header: spmd.scm,v 13.93 88/08/31 09:13:15 jinx Exp $
;;;	$MIT-Header: spmd.scm,v 13.50 88/03/14 16:36:05 GMT jinx Rel $
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

;;;; Stack Parser Quanta for CScheme Implementation

(in-package continuation-package

(declare (usual-integrations))

;;; In CScheme a control point contains a pointer to the stacklet
;;; which is the continuation of some computation.  This continuation
;;; contains restore history and restore interrupt frames so that
;;; these operations will take place when a throw is done to the given
;;; continuation.  (Registers are no longer stored since this is a
;;; vestage to the past.)

(define type-code:manifest-nm-vector
  (microcode-type 'MANIFEST-NM-VECTOR))

(define control-point:reuse-flag 0)
(define control-point:unused-size 1)
(define control-point:first-offset 2)
;; The interpreter requires at least this much padding
(define control-point:magic-reserve-size 7)

;; This procedure implicitely knows the format of a control point.

(define (make-control-point-header)
  `(;; Reuse flag
    ()
    ;; unused size
    ,control-point:magic-reserve-size
    ;; space for expansion
    ,@(make-list control-point:magic-reserve-size '())))

#|
(define non-touching-primitive-type?
  (let ((future? (make-primitive-procedure 'FUTURE? 1)))
    (lambda (type getter)
      (if (implemented-primitive-procedure? future?)
	  (and (not (future? (getter)))
	       (primitive-type? type (getter)))
	  (primitive-type? type (getter))))))
|#

(define non-touching-primitive-type?
  (make-primitive-procedure 'non-touching-primitive-type?))

(define (control-point->stack control-point)

  (define (make-nils n tail)
    (if (= n 0)
	tail
	(delay (cons '() (make-nils (-1+ n) tail)))))

  (if (not (primitive-type? type-code:control-point control-point))
      (error "control-point->stack: Not a control-point" control-point)
      (let ((size (system-vector-size control-point)))
        (let loop ((index
		    (+ control-point:first-offset
		       (primitive-datum
			(system-vector-ref control-point
					   control-point:unused-size)))))
          (cond ((= index size) '())
		((non-touching-primitive-type?
		  type-code:manifest-nm-vector
		  (system-vector-ref control-point index))
		 (let ((n-skips
			(primitive-datum
			 (system-vector-ref control-point index))))
		   (cons n-skips
			 (make-nils n-skips
				    (delay (loop (+ index n-skips 1)))))))
		(else
		 (cons (system-vector-ref control-point index)
		       (delay (loop (1+ index))))))))))

(define (stack->control-point stack fluids history
			      previous-restore-history interrupt-mask)

  (define (stack->list stack)
    (if (null? stack)
	'()
	(cons (car stack)
	      (stack->list (force (cdr stack))))))

  (system-list-to-vector 
   type-code:control-point
   `(,@(make-control-point-header)
     ,return-address-restore-fluids
     ,fluids
     ,return-address-restore-interrupt-mask
     ,interrupt-mask
     ,return-address-restore-history
     ,history
     ,(restore-history-control-point previous-restore-history)
     ,(restore-history-offset previous-restore-history)
     ,@(stack->list stack))))

;; Known to be omitted return codes:
;; PURIFY-GC-1, PURIFY-GC-2, NORMAL-GC-DONE, RESTORE-CONTINUATION,
;; RESTART-EXECUTION, COMPLETE-GC-DONE,
;; AFTER-MEMORY-UPDATE, RESTARTABLE-EXIT

(define-stack-parser 'NON-EXISTENT-CONTINUATION
  (lambda (stack history dynamic-state fluids
           previous-restore-history interrupt-mask)
    '()))

(define-stack-parser 'HALT
  (lambda (stack history dynamic-state fluids
           previous-restore-history interrupt-mask)
    '()))

(define-stack-parser 'JOIN-STACKLETS
  (lambda (stack history dynamic-state fluids
	   previous-restore-history interrupt-mask)
    (parse-stack (control-point->stack (stack-ref stack 1))
		 history
		 dynamic-state
		 fluids
		 (new-restore-history
		  (stack-ref stack 1)
		  previous-restore-history)
		 interrupt-mask)))

(define-stack-parser 'INVOKE-STACK-THREAD
  (lambda (stack history dynamic-state fluids
	   previous-restore-history interrupt-mask)
    (make-parser-output (stack-ref stack 0)	   ; Return Address
			(stack-ref stack 1)	   ; Expression
			undefined-environment
			undefined-reductions
			(stack-tail stack 2)	   ; Stack
			history
			dynamic-state
			fluids
			(monus-restore-history previous-restore-history 2)
			interrupt-mask)))

(define-standard-parser 'ASSIGNMENT-CONTINUE
  parse-standard-frame)

(define-standard-parser 'DEFINITION-CONTINUE
  parse-standard-frame)

(define-standard-parser 'SEQUENCE-2-SECOND
  parse-standard-frame)

(define-standard-parser 'SEQUENCE-3-SECOND
  parse-standard-frame)

(define-standard-parser 'SEQUENCE-3-THIRD
  parse-standard-frame)

(define-standard-parser 'CONDITIONAL-DECIDE
  parse-standard-frame)

(define-standard-parser 'DISJUNCTION-DECIDE
  parse-standard-frame)

(define-standard-parser 'COMBINATION-1-PROCEDURE
  parse-standard-frame)

(define-standard-parser 'COMBINATION-2-FIRST-OPERAND
  parse-standard-frame)

(define-standard-parser 'COMBINATION-2-PROCEDURE
  (lambda (stack history cont)
    (cont (stack-ref stack 0)
	  (stack-ref stack 1)
	  3
	  ;; Second operand.
	  (stack-ref stack 2))))

(define-standard-parser 'EVAL-ERROR
  parse-standard-frame)

(define-standard-parser 'IN-PACKAGE-CONTINUE
  parse-expression-only-frame)

(define-standard-parser 'ACCESS-CONTINUE
  parse-expression-only-frame)

(define-standard-parser 'PRIMITIVE-COMBINATION-1-APPLY
  parse-expression-only-frame)

#|
(define-standard-parser 'REPEAT-DISPATCH ;Newly added for Morry
  (lambda (stack history cont)
    (cont undefined-expression
	  (stack-ref stack 2)
	  3
	  (stack-ref stack 1)		;Dispatch value
	  (stack-ref stack 3)		;Val
	  )))
|#

(define-standard-parser 'PRIMITIVE-COMBINATION-2-FIRST-OPERAND
  parse-standard-frame)

(define-standard-parser 'PRIMITIVE-COMBINATION-2-APPLY
  (lambda (stack history cont)
    (cont (stack-ref stack 0)
	  undefined-environment
	  2
	  (stack-ref stack 1)		;Second operand.
	  )))

(define-standard-parser 'PRIMITIVE-COMBINATION-3-FIRST-OPERAND
  (lambda (stack history cont)
    (cont (stack-ref stack 0)
	  (stack-ref stack 2)
	  3
	  (stack-ref stack 1)		;Third operand.
	  )))

(define-standard-parser 'FORCE-SNAP-THUNK
  (lambda (stack history cont)
    (cont (stack-ref stack 0)
	  undefined-environment
	  1)))

(define-standard-parser 'GC-CHECK
  (lambda (stack history cont)
    (cont undefined-expression
	  undefined-environment
	  1)))

(define (restore-history-parser
	 stack history dynamic-state fluids
	 previous-restore-history interrupt-mask)
  (parse-stack (stack-tail stack 4)
	       ((access history-transform history-package) (stack-ref stack 1))
	       dynamic-state
	       fluids
	       (make-restore-history (stack-ref stack 3) (stack-ref stack 2))
	       interrupt-mask))

(define-stack-parser 'RESTORE-HISTORY restore-history-parser)
(define-stack-parser 'RESTORE-DONT-COPY-HISTORY restore-history-parser)

(define-stack-parser 'RESTORE-TO-STATE-POINT
  (lambda (stack history dynamic-state fluids
           previous-restore-history interrupt-mask)
    (parse-stack (stack-tail stack 2)
		 history
		 (stack-ref stack 1)
		 fluids
		 (monus-restore-history previous-restore-history 2)
		 interrupt-mask)))

(define-standard-parser 'MOVE-TO-ADJACENT-POINT
  (lambda (stack history cont)
    (cont undefined-expression
	  undefined-environment
	  5
	  (stack-ref stack 1)		;State Space
	  (stack-ref stack 2)		;From Point
	  (stack-ref stack 3)		;From Depth
	  (stack-ref stack 4)		;To Point
	  (stack-ref stack 5)		;To Depth
	  )))

(define-stack-parser 'RESTORE-INTERRUPT-MASK
  (lambda (stack history dynamic-state fluids
           previous-restore-history interrupt-mask)
    (parse-stack (stack-tail stack 2)
		 history
		 dynamic-state
		 fluids
		 (monus-restore-history previous-restore-history 2)
		 (stack-ref stack 1)	;New mask
		 )))

(define-stack-parser 'RESTORE-FLUIDS
  (lambda (stack history dynamic-state fluids
           previous-restore-history interrupt-mask)
    (parse-stack (stack-tail stack 2)
		 history
		 dynamic-state
		 (stack-ref stack 1)	;New fluids
		 (monus-restore-history previous-restore-history 2)
		 interrupt-mask)))

(define-standard-parser 'RESTORE-VALUE
  (lambda (stack history cont)
    (cont undefined-expression
	  undefined-environment
	  1
	  (stack-ref stack 1)		;The value
	  )))

(define-standard-parser 'RECEIVE-VALUES
  (lambda (stack history cont)
    (cont (list 'RECEIVE-MULTIPLE-VALUES (stack-ref stack 0)) ;The receiver
	  undefined-environment
	  1
	  )))

(define-stack-parser 'POP-RETURN-ERROR	;Basically ignore this frame
  (lambda (stack history dynamic-state fluids
	   previous-restore-history interrupt-mask)
    (parse-stack (stack-tail stack 2)
		 history
		 dynamic-state
		 fluids
		 (monus-restore-history previous-restore-history 2)
		 interrupt-mask)))

(define (cscheme-combination-parser count-at missing parser)
  (lambda (stack history cont)
    (let* ((count (primitive-datum (stack-ref stack count-at)))
	   (real-count (- count missing)))
      (parser cont
	      (stack-list (stack-tail stack (1+ count-at)) real-count)
	      count
	      (+ real-count count-at 1)
	      stack))))

(define-standard-parser 'COMBINATION-SAVE-VALUE
  (lambda (stack history cont)
    (let ((combination (stack-ref stack 0)))
      (let ((size (system-vector-size combination)))
	(cont combination
	      (stack-ref stack 1)
	      (+ size 2)
	      ;; evaluated arguments
	      (let ((count (1+ (primitive-datum (stack-ref stack 2)))))
		(stack-list (stack-tail stack (+ 2 count))
			    (- size count))))))))

(define-standard-parser 'COMBINATION-APPLY
  (CScheme-Combination-Parser 1 0
    (lambda (cont frame frame-length skip stack)
      (cont undefined-expression
	    undefined-environment
	    skip
	    (list->vector (cdr frame))	;evaluated arguments
	    ))))

(define-standard-parser 'INTERNAL-APPLY
  (CScheme-Combination-Parser 1 0
    (lambda (cont frame frame-length skip stack)
      (cont (make-combination (make-evaluated-object (car frame))
			      (map make-evaluated-object (cdr frame)))
	    undefined-environment
	    skip))))

(define-standard-parser 'PRIMITIVE-COMBINATION-3-SECOND-OPERAND
  parse-standard-frame)

(define-standard-parser 'PRIMITIVE-COMBINATION-3-APPLY
  (lambda (stack history cont)
    (cont (stack-ref stack 0)
	  undefined-environment
	  3
	  (stack-ref stack 2)		;third operand
	  (stack-ref stack 1)		;second operand
	  )))

(define-standard-parser 'REPEAT-PRIMITIVE

  ;; Reconstruct a fully evaluated combination which would have called
  ;; this primitive with these arguments.  The primitive itself is
  ;; where the expression would normally appear in a stack frame,
  ;; followed by its arguments.

  (lambda (stack history cont)
    (let ((primitive (stack-ref stack 0)))
      (let ((arity (primitive-procedure-arity primitive)))
	(if (= arity -1)  ;; Handle the variable argument case
	    (let ((real-arity (primitive-datum (stack-ref stack 1))))
	      (cont (make-combination
		     (make-evaluated-object primitive)
		     (map make-evaluated-object
			  (stack-list (stack-tail stack 2) real-arity)))
		    undefined-environment
		    (+ real-arity 2)))
	    (cont (make-combination (make-evaluated-object primitive)
				    (map make-evaluated-object
					 (stack-list (stack-tail stack 1) arity)))
		  undefined-environment
		  (1+ arity)))))))

;;;; Compiled Code Frames

(define-standard-parser 'REENTER-COMPILED-CODE
  (lambda (stack history cont)
    (cont compiled-code
	  undefined-environment
	  (1+ (stack-ref stack 0))	;offset
	  )))

(define (define-compiled-parser name parser)
  (define-stack-parser name
    (lambda (stack history dynamic-state fluids
	     previous-restore-history interrupt-mask)
      (parser (stack-tail stack 2)
	      history
	      (lambda (expression environment count . annotation)
		(apply make-parser-output
		       (cons* (stack-ref stack 0)
			      expression
			      environment
			      ((access history-reductions history-package)
			       history)
			      (compiler-stack-tail stack count)
			      ((access history-superproblem history-package)
			       history)
			      dynamic-state
			      fluids
			      (monus-restore-history previous-restore-history
						     count)
			      interrupt-mask
			      annotation)))))))

(define (ignore-compiler-frame n)
  (lambda (stack history dynamic-state fluids
		 previous-restore-history interrupt-mask)
    (parse-stack (compiler-stack-tail stack n)
		 history
		 dynamic-state
		 fluids
		 (monus-restore-history previous-restore-history n)
		 interrupt-mask)))

(define (compiler-stack-tail stack n)
  (stack-cons return-address-reenter-compiled-code
	      (stack-cons (- (stack-ref stack 1) n)
			  (stack-tail stack (+ 2 n)))))

(define-stack-parser 'COMPILER-INTERRUPT-RESTART
  (ignore-compiler-frame 1))

(define-stack-parser 'COMPILER-LINK-CACHES-RESTART
  (ignore-compiler-frame 6))

(define (parse-compiler-reference scode-maker)
  (lambda (stack history cont)
    (cont (scode-maker (stack-ref stack 1))	; name
	  (stack-ref stack 0)			; env
	  2)))

(let ((reference-parser (parse-compiler-reference identity-procedure)))
  (define-compiled-parser 'COMPILER-REFERENCE-RESTART reference-parser)
  (define-compiled-parser 'COMPILER-SAFE-REFERENCE-RESTART reference-parser))

(define-compiled-parser 'COMPILER-ACCESS-RESTART
  (parse-compiler-reference make-variable))

(define-compiled-parser 'COMPILER-UNASSIGNED?-RESTART
  (parse-compiler-reference make-unassigned?))

(define-compiled-parser 'COMPILER-UNBOUND?-RESTART
  (parse-compiler-reference make-unbound?))

(define-compiled-parser 'COMPILER-LOOKUP-APPLY-RESTART
  (cscheme-combination-parser 2 1
   (lambda (cont frame frame-length skip stack)
     (cont (make-combination (stack-ref stack 1) ;var
			     (map make-evaluated-object frame))
	   (stack-ref stack 0)			; env
	   (+ 2 frame-length)))))

(define (parse-compiler-assignment scode-maker)
  (lambda (stack history cont)
    (cont (scode-maker (stack-ref stack 1)	; name
		       (make-evaluated-object
			(stack-ref stack 2)))	; value
	  (stack-ref stack 0)			; env
	  3)))

(define-compiled-parser 'COMPILER-ASSIGNMENT-RESTART
  (parse-compiler-assignment make-assignment-from-variable))

(define-compiled-parser 'COMPILER-DEFINITION-RESTART
  (parse-compiler-assignment make-definition))

(define (parse-compiler-reference-trap scode-maker)
  (lambda (stack history cont)
    (cont (scode-maker (stack-ref stack 0))			; name
	  (stack-ref stack 1)					; env
	  2)))

(let ((parser (parse-compiler-reference-trap make-variable)))
  (define-compiled-parser 'COMPILER-REFERENCE-TRAP-RESTART parser)
  (define-compiled-parser 'COMPILER-SAFE-REFERENCE-TRAP-RESTART parser))

(define-compiled-parser 'COMPILER-UNASSIGNED?-TRAP-RESTART
  (parse-compiler-reference-trap make-unassigned?))

(define-compiled-parser 'COMPILER-ASSIGNMENT-TRAP-RESTART
  (lambda (stack history cont)
    (cont (make-assignment (stack-ref stack 0)			; name
			   (make-evaluated-object
			    (stack-ref stack 2)))		; value
	  (stack-ref stack 1)					; env
	  3)))

(let ((parser
       (lambda (stack history cont)
	 (let* ((number-of-arguments
		 (-1+ (primitive-datum (stack-ref stack 2))))
		(arguments (stack-list (stack-tail stack 4)
				       number-of-arguments)))
	   (cont (make-combination (make-variable
				    (stack-ref stack 0))	; name
				   (map make-evaluated-object
					arguments))
		 (stack-ref stack 1)				; env
		 (+ 4 number-of-arguments))))))
  (define-compiled-parser 'COMPILER-LOOKUP-APPLY-TRAP-RESTART parser)
  (define-compiled-parser 'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART parser))

(define-stack-parser 'DETERMINE-SELF
  (lambda (stack history dynamic-state fluids
		 previous-restore-history interrupt-mask)
    '()))

(define-stack-parser 'RESTORE-PROCESS-STATE
  (lambda (stack history dynamic-state fluids
		 previous-restore-history interrupt-mask)
    (parse-stack (stack-tail stack 3)
 		 history
 		 (stack-ref stack 1)
 		 (stack-ref stack 2)
 		 previous-restore-history 
 		 interrupt-mask)))

;;; end IN-PACKAGE PRIMITIVE-CONTINUATION.
)
