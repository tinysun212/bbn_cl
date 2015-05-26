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
;;;;;; Program to insert calls to the primitive touch in a piece of S-Code.
;;;
;;;	- The inner pass explores a piece of S-Code to see if it can never
;;;		evaluate to a future.
;;;	- The outer pass looks to see where a future would have to be
;;;		touched in in-line code and if the inner pass cannot
;;;		guarantee that a touch is not needed, it inserts the touch.
;;;
;;; $Header: touchify.scm,v 1.1 89/02/01 13:06:13 las Exp $
;;; $Log:	touchify.scm,v $
Revision 1.1  89/02/01  13:06:13  las
Initial revision

;; Revision 1.4  88/08/31  10:48:35  jinx
;; New compiled procedure and uuo-link format.
;; 

(declare (usual-integrations))		; *ONE WORD INTEGERS

(define touch-debug #f)
(define touch-count 0)
(define touch-dcount 0)
(define touch-declare-vbl '())
(define touch-declare-fcn '())
(define touch-declare-vec '())
(define touch-spliced-out (cons 'splice 'out))

(define (touchify scode)
  (fluid-let ((touch-count 0))
    (let ((result (touchify1 scode)))
      (if (access compiler:show-phases? compiler-package)
	  (write-string
	   (with-output-to-string
	     (lambda ()
	       (write-string "      ")
	       (write touch-count)
	       (write-string " touches inserted")
	       (newline)))))
      result)))

(define (touchify1 scode)
  ((vector-ref touchify-methods (primitive-type scode)) scode))

(define (insert-touch scode)
  (if (never-a-future scode)
      (touchify1 scode)
      (begin
	(set! touch-count (1+ touch-count))
	(wrap-touch (touchify1 scode)))))

(define wrap-touch
  (let* ((touch (make-primitive-procedure 'touch))
	 (primitive-type? (make-primitive-procedure 'primitive-type?))
	 (var (generate-uninterned-symbol 'g))
	 (future-tag (microcode-type 'future))
	 (touch-scode-template
	  (syntax `(let ((,var #f))
		     (if (,primitive-type? ,future-tag ,var)
			 (,touch ,var)
			 ,var))
		  system-global-syntax-table)))
    (named-lambda (wrap-touch scode)
      (make-combination (combination-operator touch-scode-template)
			(list scode)))))

(define (touchify-verbatim scode)
  scode)

(define touchify-methods (vector-cons 128 touchify-verbatim))

(define (set-touchifier! types handler)
  (for-each
   (lambda (type)
     (vector-set! touchify-methods (microcode-type type) handler))
   (if (pair? types) types (cons types '()))))

(define touching-setters-list		; Compiled inline, setting AND touching!
  (list system-vector-set!
	vector-set!
	system-hunk3-set-cxr2!
	system-hunk3-set-cxr1!
	system-hunk3-set-cxr0!
	system-pair-set-car!
	system-pair-set-cdr!
	set-car!
	set-cdr!
	set-cell-contents!))

(define touching-primitives-list	; Compiled inline AND touching!
  (mapcar make-primitive-procedure
	  '(
            system-vector-ref   vector-ref         system-hunk3-cxr2     system-hunk3-cxr1 
            system-hunk3-cxr0   system-pair-car    system-pair-cdr	     car
	    cdr                 cell-contents      bit-string-length     string-length
	    system-vector-size  vector-length      eq?                   primitive-type?
	    bit-string?         string?            null?                 pair?
	    zero-fixnum?        positive-fixnum?   negative-fixnum?      zero-flonum? 
	    positive-flonum?    negative-flonum?   less-than-fixnum?     greater-than-fixnum? 
	    equal-fixnum?       less-than-flonum?  greater-than-flonum?  equal-flonum?
	    plus-fixnum         minus-fixnum       multiply-fixnum       plus-flonum 
	    minus-flonum        multiply-flonum    divide-flonum         minus-one-plus-fixnum
	    one-plus-fixnum     sine-flonum        cosine-flonum         arctan-flonum
	    exp-flonum          ln-flonum          truncate-flonum       coerce-integer-to-flonum
	    )))

(define (touchify-primitive-handler scode)
   (let ((operator (combination-operator scode))
	 (operands (combination-operands scode)))
     (cond ((memq operator touching-primitives-list)
	    (make-combination operator (mapcar insert-touch operands)))
	   ((memq operator touching-setters-list)
	    (make-combination operator (insert-touch-but-last operands)))
	   ((eq? operator general-car-cdr)
	    (expand-car-cdr (car operands) (touchify1 (cadr operands))))
	   (else
	    (make-combination operator (mapcar touchify1 operands))))))

(define (expand-car-cdr expression pattern)
  (if (= pattern 1)
      expression
      (let ((qr (integer-divide pattern 2)))
	(expand-car-cdr
	 (make-combination
	  (if (= (integer-divide-remainder qr) 0) cdr car)
	  (list (wrap-touch expression)))
	 (integer-divide-quotient qr)))))

(set-touchifier! '(PRIMITIVE-COMBINATION-0 PRIMITIVE-COMBINATION-1
					   PRIMITIVE-COMBINATION-2
					   PRIMITIVE-COMBINATION-3)
		 touchify-primitive-handler)

(define (insert-touch-but-last operands)
  (cond ((null? operands)
	 '())
	((null? (cdr operands))
	 operands)
	(else
	 (cons (insert-touch (car operands))
	       (insert-touch-but-last (cdr operands))))))

(define (touchify-combination-handler scode)
  (let ((operator (combination-operator scode))
	(operands (combination-operands scode)))
    (if (variable? operator)
	(let ((vbl-name (variable-name operator)))
	  (cond ((eq? vbl-name '*NON-FUTURE-VBL*)
		 (set! touch-declare-vbl
		       (append (car operands) touch-declare-vbl))
		 touch-spliced-out)
		((eq? vbl-name '*NON-FUTURE-FCN*)
		 (set! touch-declare-fcn
		       (append (car operands) touch-declare-fcn))
		 touch-spliced-out)
		((eq? vbl-name '*NON-FUTURE-VEC*)
		 (set! touch-declare-vec
		       (append (car operands) touch-declare-vec))
		 touch-spliced-out)
		((eq? vbl-name 'WITHOUT-TOUCHING)
		 (if (lambda? (car operands))
		     (lambda-body (car operands))
		     (error "TOUCHIFY: without-touching needs a lambda"
			    (car operands))))
		(else
		 (make-combination operator
				   (mapcar touchify1 operands)))))
	(make-combination (touchify1 operator)
			  (mapcar touchify1 operands)))))

(set-touchifier! '(COMBINATION-1 COMBINATION-2 COMBINATION)
		 touchify-combination-handler)

(define (touchify-sequence-handler scode)
  (make-sequence (mapcar-in-order touchify1 (sequence-actions scode))))

(set-touchifier! '(SEQUENCE-2 SEQUENCE-3) touchify-sequence-handler)

(define (touchify-comment-handler scode)
  (make-comment (comment-text scode)
		(touchify1 (comment-expression scode))))

(set-touchifier! 'COMMENT touchify-comment-handler)

(define (touchify-define-handler scode)
  (make-definition (definition-name scode)
		   (touchify1 (definition-value scode))))

(set-touchifier! 'DEFINITION touchify-define-handler)

(define (touchify-access-handler scode)
  (make-access (touchify1 (access-environment scode))
	       (access-name scode)))

(set-touchifier! 'ACCESS touchify-access-handler)

(define (touchify-assign-handler scode)
  (let ((target (assignment-variable scode)))
    (make-assignment (if (access? target)
			 (touchify-access-handler target)
			 (variable-name target))
		     (touchify1 (assignment-value scode)))))

(set-touchifier! 'ASSIGNMENT touchify-assign-handler)

(define (touchify-in-package-handler scode)
  (make-in-package (in-package-environment scode)
		   (touchify1 (in-package-expression scode))))

(set-touchifier! 'IN-PACKAGE touchify-in-package-handler)

(define (touchify-variable-handler scode)
  scode)

(set-touchifier! 'VARIABLE touchify-variable-handler)

(define (touchify-conditional-handler scode)
  (make-conditional (insert-touch (conditional-predicate scode))
		    (touchify1 (conditional-consequent scode))
		    (touchify1 (conditional-alternative scode))))

(set-touchifier! 'CONDITIONAL touchify-conditional-handler)

(define (touchify-disjunction-handler scode)
  (make-disjunction (insert-touch (disjunction-predicate scode))
		    (touchify1 (disjunction-alternative scode))))

(set-touchifier! 'DISJUNCTION touchify-disjunction-handler) 

(define (touchify-lambda-handler scode)
  (fluid-let ((touch-declare-vbl touch-declare-vbl)
	      (touch-declare-fcn touch-declare-fcn)
	      (touch-declare-vec touch-declare-vec))
    (lambda-components
	scode
      (lambda (name params opts rest aux decls body)
	(make-lambda name params opts rest aux decls (touchify1 body))))))

(set-touchifier! '(LAMBDA EXTENDED-LAMBDA) touchify-lambda-handler)

(define never-a-future-methods (vector-cons 128 #t))

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

(define (never-primitive-handler scode)
  (let ((prim (combination-operator scode)))
    (if (eq? prim vector-ref)
	(let ((vector-hmm (car (combination-operands scode))))
	  (if (and (variable? vector-hmm)
		   (memq (variable-name vector-hmm) touch-declare-vec))
	      (begin
		(set! touch-dcount (1+ touch-dcount))
		(if touch-debug
		    (begin
		      (newline)
		      (display (list 'vector
				     (variable-name vector-hmm)
				     (cadr (combination-operands scode))))))
		#t)
	      #f))
	(if (memq prim may-return-futures-list)
	    #f
	    #t))))

(set-never! '(PRIMITIVE-COMBINATION-0 PRIMITIVE-COMBINATION-1
				      PRIMITIVE-COMBINATION-2
				      PRIMITIVE-COMBINATION-3)
	    never-primitive-handler)

(define local-reference (make-primitive-procedure 'local-reference))
(define within-control-point (make-primitive-procedure 'within-control-point))
(define hunk3-set-cxr! (make-primitive-procedure 'hunk3-set-cxr!))
(define hunk3-cxr (make-primitive-procedure 'hunk3-cxr))

(define may-return-futures-list
  (list lexical-assignment
	local-reference
	local-assignment
	call-with-current-continuation
	scode-eval
	apply
	non-reentrant-call-with-current-continuation
	lexical-reference
	car
	cdr
	set-car!
	set-cdr!
	general-car-cdr
	hunk3-cxr
	hunk3-set-cxr!
	vector-ref
	vector-set!
	assq
	cell-contents
	system-pair-car
	system-pair-cdr
	system-pair-set-car!
	system-pair-set-cdr!
	set-cell-contents!
	system-hunk3-cxr0
	system-hunk3-set-cxr0!
	system-hunk3-cxr1
	system-hunk3-set-cxr1!
	system-hunk3-cxr2
	system-hunk3-set-cxr2!
	system-vector-ref
	system-vector-set!
	with-history-disabled
	with-threaded-continuation
	within-control-point
	with-interrupts-reduced
	%spawn-process))

(set-never! '(ACCESS ASSIGNMENT COMPILED-ENTRY FUTURE)
	    #f)

(define (never-combination-handler scode)
  (let ((operator (combination-operator scode)))
    (if (and (variable? operator)
	     (memq (variable-name operator) touch-declare-fcn))
	(begin
	  (set! touch-dcount (1+ touch-dcount))
	  (if touch-debug
	      (begin
		(newline)
		(display (list 'combination
			       (variable-name operator)
			       (combination-operands scode)))))
	  #t)
	#f)))

(set-never! '(COMBINATION-1 COMBINATION-2 COMBINATION)
	    never-combination-handler)

(define (never-variable-handler scode)
  (if (memq (variable-name scode) touch-declare-vbl)
      (begin
	(set! touch-dcount (1+ touch-dcount))
	(if touch-debug
	    (begin
	      (newline)
	      (display (list 'variable (variable-name scode)))))
	#t)
      #f))

(set-never! 'VARIABLE never-variable-handler)

(define (never-sequence-handler scode)
  (let ((actions (sequence-actions scode)))
    (if (null? actions)
	#t
	(never-a-future (car (last-pair actions))))))

(set-never! '(SEQUENCE-2 SEQUENCE-3)
	    never-sequence-handler)

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

;;;	Make it easy to access lambda components.

(define (lambda-name scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       name)))

(define (lambda-params scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       params)))

(define (lambda-opts scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       opts)))

(define (lambda-rest scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       (if rest
			   rest
			   'NO-REST))))

(define (lambda-aux scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       aux)))

(define (lambda-body scode)
  (lambda-components scode
		     (lambda (name params opts rest aux decls body)
		       body)))

;;;	Mapcar in order

(define (mapcar-in-order fcn args)
  (if (null? args)
      '()
      (let ((result (fcn (car args))))
	(cons result (mapcar-in-order fcn (cdr args))))))
