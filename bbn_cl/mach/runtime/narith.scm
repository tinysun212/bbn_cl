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

;;;; Scheme Arithmetic

(declare (usual-integrations))

(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(LOCAL-ASSIGNMENT
		       system-global-environment
		       ',name ,(make-primitive-procedure name)))
		   names)))

  (define-primitives
    zero? positive? negative? 1+ -1+ integer-divide
    sqrt exp log sin cos truncate round floor ceiling
    )

  (define-macro (make-primitive name)
    (make-primitive-procedure name))

  (local-assignment system-global-environment
		    'integer-divide-quotient
		    (make-primitive car))

  (local-assignment system-global-environment
		    'integer-divide-remainder
		    (make-primitive cdr))

  )

(declare (integrate quotient remainder))

(define (quotient x y)
  (declare (integrate x y))
  (integer-divide-quotient (integer-divide x y)))

(define (remainder x y)
  (declare (integrate x y))
  (integer-divide-remainder (integer-divide x y)))

(define (modulo x y)
  (let ((rem (remainder x y)))
    (if (negative? x)
	(if (or (negative? y) (zero? rem))
	    rem
	    (+ y rem))
	(if (and (negative? y) (not (zero? rem)))
	    (+ y rem)
	    rem))))

(define (even? x)
  (zero? (remainder x 2)))

(define (odd? x)
  (not (zero? (remainder x 2))))

(define (exact? x)
  (if (number? x)
      false
      (error "EXACT?: Not a number" x)))

(define (inexact? x)
  (if (number? x)
      true
      (error "INEXACT?: Not a number" x)))

(define (exact->inexact x)
  (if (number? x)
      x
      (error "EXACT->INEXACT: Not a number" x)))

(define (inexact->exact x)
  (if (number? x)
      (error "INEXACT->EXACT: Exact numbers not supported" x)
      (error "INEXACT->EXACT: Not a number" x)))

(define integer?)
(define real?)
(define complex?)
(define make-rectangular)
(define make-polar)
(define real-part)
(define imag-part)
(define magnitude)
(define angle)
(define =)
(define <)
(define >)
(define <=)
(define >=)
(define =?)
(define <?)
(define >?)
(define <=?)
(define >=?)
(define +)
(define -)
(define *)
(define /)
(define gcd)
(define lcm)
(define abs)
(define integer-expt)
(define expt)
(define tan)
(define atan)
(define acos)
(define asin)
(define min)
(define max)
(define random)
(define randomize)
(let ()

(define fixnum-type (microcode-type 'FIXNUM))
(define bignum-type (microcode-type 'BIGNUM))
(define flonum-type (microcode-type 'FLONUM))
(define complex-type (microcode-type 'COMPLEX))

;; fake out broken declarations
(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(LOCAL-ASSIGNMENT
		       (the-environment)
		       ',name
		       ,(make-primitive-procedure name)))
		   names)))
  (define-primitives &= &< &> &+ &- &* &/ &atan))

(declare (integrate-primitive-procedures
	  &= &< &> &+ &- &* &/ &atan))

(define (&gcd x y)
  (cond ((not (zero? y))
	 (&gcd y (remainder x y)))
	((negative? x) (- 0 x))
	(else x)))

(define (&lcm x y)
  (if (or (zero? x)
	  (zero? y))
      0
      (quotient (abs (* x y))
		(&gcd x y))))

(set! integer?
(named-lambda (integer? object)
  (or (primitive-type? fixnum-type object)
      (primitive-type? bignum-type object))))

(set! real?
(named-lambda (real? object)
  (or (primitive-type? fixnum-type object)
      (primitive-type? bignum-type object)
      (primitive-type? flonum-type object))))

(set! complex?
(named-lambda (complex? object)
  (or (primitive-type? fixnum-type object)
      (primitive-type? bignum-type object)
      (primitive-type? flonum-type object)
      (primitive-type? complex-type object))))

;;;; Complex Numbers
;;; Simple version to allow reader & printer to work ok.

(set! make-rectangular
(lambda (x y)
  (if (and (real? x) (real? y))
      (&make-rectangular x y)
      (error "Make-retangular: bad arguments" x y))))

(set! make-polar
(lambda (r theta)
  (if (and (real? r) (real? theta))
      (if (zero? r)
	  0
	  (&make-rectangular (* r (cos theta))
			     (* r (sin theta))))
      (error "Make-polar: bad arguments" r theta))))

(define (&make-rectangular re im)
  (if (zero? im)
      re
      (system-pair-cons complex-type re im)))

(set! real-part
(lambda (z)
  (cond ((real? z) z)
	((primitive-type? complex-type z) (system-pair-car z))
	(else (error "Real-part: bad number" z)))))

(set! imag-part
(lambda (z)
  (cond ((real? z) 0)
	((primitive-type? complex-type z) (system-pair-cdr z))
	(else (error "Imag-part: bad number" z)))))

(set! magnitude
(lambda (z)
  (cond ((real? z) (abs z))
	((primitive-type? complex-type z)
	 (sqrt (+ (square (system-pair-car z))
		  (square (system-pair-cdr z)))))
	(else (error "Magnitude: bad number" z)))))

(define (square x)
  (* x x))

(set! angle
(lambda (z)
  (cond ((real? z) (if (negative? z) pi 0))
	((primitive-type? complex-type z)
	 (let ((re (system-pair-car z))
	       (im (system-pair-cdr z)))
	   (if (and (zero? re) (zero? im))
	       0
	       (atan im re))))
	(else (error "Angle: bad number" z)))))

;;;; Order Operations

(let-syntax
 ((define-pairwise-test
   (macro (name p)
     `(SET! ,name
	    (NAMED-LAMBDA (,name . ARGUMENTS)
	      (COND ((NULL? ARGUMENTS) #F)
		    ((NULL? (CDR ARGUMENTS)) #F)
		    ((NULL? (CDDR ARGUMENTS))
		     (,p (CAR ARGUMENTS)
			 (CADR ARGUMENTS)))
		    (ELSE
		     (LET TEST-LOOP
		       ((RESULT (,p (CAR ARGUMENTS)
				    (CADR ARGUMENTS)))
			(CDR1 (CDR ARGUMENTS))
			(CDR2 (CDDR ARGUMENTS)))
		       (IF RESULT
			   (IF (NULL? CDR2)
			       #T
			       (TEST-LOOP (,p (CAR CDR1) (CAR CDR2))
					  CDR2
					  (CDR CDR2)))
			   #F)))))))))
 (define-pairwise-test = &=)
 (define-pairwise-test < &<)
 (define-pairwise-test > &>))

(set! =? =)
(set! <? <)
(set! >? >)

(let-syntax
 ((define-pairwise-test
   (macro (name p)
     `(SET! ,name
	    (NAMED-LAMBDA (,name . ARGUMENTS)
	      (COND ((NULL? ARGUMENTS) #F)
		    ((NULL? (CDR ARGUMENTS)) #F)
		    ((NULL? (CDDR ARGUMENTS))
		     (NOT (,p (CAR ARGUMENTS)
			      (CADR ARGUMENTS))))
		    (ELSE
		     (LET TEST-LOOP
		       ((RESULT (,p (CAR ARGUMENTS)
				    (CADR ARGUMENTS)))
			(CDR1 (CDR ARGUMENTS))
			(CDR2 (CDDR ARGUMENTS)))
		       (IF RESULT
			   #F
			   (IF (NULL? CDR2)
			       #T
			       (TEST-LOOP (,p (CAR CDR1) (CAR CDR2))
					  CDR2
					  (CDR CDR2))))))))))))
 (define-pairwise-test <= &>)
 (define-pairwise-test >= &<))

(set! <=? <=)
(set! >=? >=)


;;;; Field Operations

(let-syntax
 ((define-right-accumulation
   (macro (name f i)
     `(SET! ,name
	    (NAMED-LAMBDA (,name . ARGUMENTS)
	      (COND ((NULL? ARGUMENTS)
		     ,i)
		    ((NULL? (CDR ARGUMENTS))
		     (CAR ARGUMENTS))
		    ((NULL? (CDDR ARGUMENTS))
		     (,f (CAR ARGUMENTS)
			 (CADR ARGUMENTS)))
		    (ELSE
		     (LET ACCUMULATION-LOOP
		       ((ARGUMENTS (CDDR ARGUMENTS))
			(ACCUMULATOR (,f (CAR ARGUMENTS)
					 (CADR ARGUMENTS))))
		       (IF (NULL? ARGUMENTS)
			   ACCUMULATOR
			   (ACCUMULATION-LOOP (CDR ARGUMENTS)
					      (,f (CAR ARGUMENTS)
						  ACCUMULATOR)))))))))))
 (define-right-accumulation + &+ 0)
 (define-right-accumulation * &* 1)
 (define-right-accumulation gcd &gcd 0)
 (define-right-accumulation lcm &lcm 1))

(let-syntax
 ((define-right-accumulation
    (macro (name f f-1 i)
      `(SET! ,name
	     (NAMED-LAMBDA (,name X . REST)
	       (COND ((NULL? REST)
		      (,f ,i X))
		     ((NULL? (CDR REST))
		      (,f X (CAR REST)))
		     (ELSE
		      (LET ACCUMULATION-LOOP
			((ARGUMENTS (CDDR REST))
			 (ACCUMULATOR (,f-1 (CAR REST)
					    (CADR REST))))
			(IF (NULL? ARGUMENTS)
			    (,f X ACCUMULATOR)
			    (ACCUMULATION-LOOP (CDR ARGUMENTS)
					       (,f-1 (CAR ARGUMENTS)
						     ACCUMULATOR)))))))))))
 (define-right-accumulation - &- &+ 0)
 (define-right-accumulation / &/ &* 1))

;;;; Exponentiation

(set! expt
(named-lambda (expt x y)
  (if (integer? y)
      (integer-expt x y)
      (exp (&* (log x) y)))))

(set! integer-expt
(named-lambda (integer-expt base exponent)
  (if (not (integer? exponent))
      (error "Non-integer exponent" 'INTEGER-EXPT exponent))
  (if (negative? exponent)
      (&/ 1 (expt-iter base (&- 0 exponent) 1))
      (expt-iter base exponent 1))))

(define (expt-iter x count answer)
  (if (zero? count)
      answer
      (let ((qr (integer-divide count 2)))
	(if (zero? (integer-divide-remainder qr))
	    (expt-iter (&* x x) (integer-divide-quotient qr) answer)
	    (expt-iter x (-1+ count) (&* x answer))))))

;;;; Trigonometric Operations

(set! tan
(named-lambda (tan x)
  (&/ (sin x) (cos x))))

(set! atan
(named-lambda (atan y x)
  (if (zero? x)
      (if (negative? y) -pi/2 pi/2)
      (let ((atan1 (&atan (&/ y x))))
	(cond ((positive? x) atan1)
	      ((negative? y) (&- atan1 pi))
	      (else (&+ atan1 pi)))))))

(set! acos
(named-lambda (acos x)
  (if (zero? x)
      pi/2
      (let ((sq (&* x x)))
	(cond ((&> sq 1)
	       (error "ACOS: argument out of range" x))
	      ((positive? x)
	       (&atan (&/ (sqrt (&- 1 sq))
			  x)))
	      (else
	       (&- pi
		   (&atan (&/ (sqrt (&- 1 sq))
			      (&- 0 x))))))))))

(set! asin
(named-lambda (asin x)
  (let ((sq (&* x x)))
    (cond ((&= sq 1)
	   (if (positive? x) pi/2 (&- 0 pi/2)))
	  ((&> sq 1)
	   (error "ASIN: argument out of range" x))
	  ((positive? x)
	   (&atan (&/ x
		      (sqrt (&- 1 sq)))))
	  (else
	   (&- 0
	       (&atan (&/ (&- 0 x)
			  (sqrt (&- 1 sq))))))))))

(define pi/4  (&atan 1))
(define pi/2  (&* pi/4 2))
(define -pi/2 (&- 0 pi/2))
(define pi    (&* pi/4 4))
(define 3pi/2 (&+ pi pi/2))
(define 2pi   (&* pi/4 8))

;;;; Miscellaneous

(set! abs
(named-lambda (abs x)
  (if (negative? x)
      (&- 0 x)
      x)))

(let-syntax
 ((min-max-generator
   (macro (name predicate)
     `(SET! ,name
	    (NAMED-LAMBDA (,name FIRST . REST)
	      (LET LOOP ((RESULT FIRST)
			 (VALUE-LIST REST))
		   (IF (NULL? VALUE-LIST)
		       RESULT
		       (LOOP (IF (,predicate RESULT (CAR VALUE-LIST))
				 RESULT
				 (CAR VALUE-LIST))
			     (CDR VALUE-LIST)))))))))
  (min-max-generator min &<)
  (min-max-generator max &>))

(let ((seed 1)
      (a (+ (* 3141 1000 1000)
	    (* 592  1000)
	    621))
      (m (integer-expt 2 63))
      (c 1))

(set! random
(named-lambda (random k)
  (cond ((not (integer? k))
	 (error "RANDOM is valid only for integers" k))
	((<= 1 k m)
	 (set! seed
	       (remainder (&+ (&* a seed) c)
			  m))
	 (quotient (&* seed k) m))
	(else
	 (error "RANDOM is valid only for integers from 1 to" m)))))

(set! randomize
(named-lambda (randomize new-seed)
  (set! seed new-seed)))

)

)

(define rational? integer?)
(define number? complex?)
