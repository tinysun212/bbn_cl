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
;;;
;;; Access to common lisp generic arithmetic primitives.

(export '(* + - / /= 1+ 1- < <= = > >= abs acos acosh
	  asin asinh atan atanh ceiling cis conjugate cos cosh complex denominator
	  evenp exp expt fceiling ffloor floor fround ftruncate gcd imagpart
	  isqrt log lcm max min minusp mod numerator oddp phase plusp rational rationalize
	  realpart rem round signum sin sinh sqrt tan tanh truncate zerop))

(cl-define complex (make-primitive-procedure 'make-complex))
(cl-define realpart (make-primitive-procedure 'realpart))
(cl-define imagpart (make-primitive-procedure 'imagpart))
(cl-define numerator (make-primitive-procedure 'ratio-numerator))
(cl-define denominator (make-primitive-procedure 'ratio-denominator))
(cl-define zerop (make-primitive-procedure 'generic-zerop))
(cl-define plusp (make-primitive-procedure 'generic-plusp))
(cl-define minusp (make-primitive-procedure 'generic-minusp))
(cl-define oddp (make-primitive-procedure 'generic-oddp))
(cl-define evenp (make-primitive-procedure 'generic-evenp))
(cl-define = (make-primitive-procedure 'generic-=))
(cl-define /= (make-primitive-procedure 'generic-/=))
(cl-define < (make-primitive-procedure 'generic-<))
(cl-define > (make-primitive-procedure 'generic->))
(cl-define <= (make-primitive-procedure 'generic-<=))
(cl-define >= (make-primitive-procedure 'generic->=))
(cl-define max (make-primitive-procedure 'generic-max))
(cl-define min (make-primitive-procedure 'generic-min))
(cl-define + (make-primitive-procedure 'generic-+))
(cl-define - (make-primitive-procedure 'generic--))
(cl-define * (make-primitive-procedure 'generic-*))
(cl-define / (make-primitive-procedure 'generic-/))
(cl-define 1+ (make-primitive-procedure 'generic-1+))
(cl-define 1- (make-primitive-procedure 'generic-1-))
(cl-define conjugate (make-primitive-procedure 'generic-conjugate))
(cl-define gcd (make-primitive-procedure 'generic-gcd))
(cl-define lcm (make-primitive-procedure 'generic-lcm))
(cl-define exp (make-primitive-procedure 'generic-exp))
(cl-define expt (make-primitive-procedure 'generic-expt))
(cl-define log (make-primitive-procedure 'generic-log))
(cl-define sqrt (make-primitive-procedure 'generic-sqrt))
(cl-define isqrt (make-primitive-procedure 'generic-isqrt))
(cl-define abs (make-primitive-procedure 'generic-abs))
(cl-define phase (make-primitive-procedure 'generic-phase))
(cl-define signum (make-primitive-procedure 'generic-signum))
(cl-define sin (make-primitive-procedure 'generic-sin))
(cl-define cos (make-primitive-procedure 'generic-cos))
(cl-define tan (make-primitive-procedure 'generic-tan))
(cl-define cis (make-primitive-procedure 'generic-cis))
(cl-define asin (make-primitive-procedure 'generic-asin))
(cl-define acos (make-primitive-procedure 'generic-acos))
(cl-define atan (make-primitive-procedure 'generic-atan))
(cl-define sinh (make-primitive-procedure 'generic-sinh))
(cl-define cosh (make-primitive-procedure 'generic-cosh))
(cl-define tanh (make-primitive-procedure 'generic-tanh))
(cl-define asinh (make-primitive-procedure 'generic-asinh))
(cl-define acosh (make-primitive-procedure 'generic-acosh))
(cl-define atanh (make-primitive-procedure 'generic-atanh))
;;; Defined with coerce
;;; (cl-define float (make-primitive-procedure 'generic-float)) 
(cl-define rational (make-primitive-procedure 'generic-rational))
(cl-define rationalize (make-primitive-procedure 'generic-rationalize))
(cl-define floor (make-primitive-procedure 'generic-floor))
(cl-define ceiling (make-primitive-procedure 'generic-ceiling))
(cl-define truncate (make-primitive-procedure 'generic-truncate))
(cl-define round (make-primitive-procedure 'generic-round))

;;; These should be redone in microcode

(cl-define (ffloor x) (float (floor x)))
(cl-define (fceiling x) (float (ceiling x)))
(cl-define (ftruncate x) (float (truncate x)))
(cl-define (fround x) (float (round x)))

(cl-define (mod number divisor)
  (prim-with-values (lambda () (floor number divisor)) (lambda (f remainder) remainder)))

(cl-define (rem number divisor)
  (prim-with-values (lambda () (truncate number divisor)) (lambda (f remainder) remainder)))

;;;; Arithmetic optimizers

;;; Needed primitives

(cl-define plus-fixnum (make-primitive-procedure 'plus-fixnum))
(cl-define minus-fixnum (make-primitive-procedure 'minus-fixnum))
(cl-define multiply-fixnum (make-primitive-procedure 'multiply-fixnum))
(cl-define divide-flonum (make-primitive-procedure 'divide-flonum))
(cl-define plus-flonum (make-primitive-procedure 'plus-flonum))
(cl-define minus-flonum (make-primitive-procedure 'minus-flonum))
(cl-define multiply-flonum (make-primitive-procedure 'multiply-flonum))
(cl-define one-plus-fixnum (make-primitive-procedure 'one-plus-fixnum))
(cl-define minus-one-plus-fixnum (make-primitive-procedure 'minus-one-plus-fixnum))

(cl-define zero-fixnum? (make-primitive-procedure 'zero-fixnum?))
(cl-define negative-fixnum? (make-primitive-procedure 'negative-fixnum?))
(cl-define positive-fixnum? (make-primitive-procedure 'positive-fixnum?))
(cl-define equal-fixnum? (make-primitive-procedure 'equal-fixnum?))
(cl-define less-than-fixnum? (make-primitive-procedure 'less-than-fixnum?))
(cl-define greater-than-fixnum? (make-primitive-procedure 'greater-than-fixnum?))

(cl-define zero-flonum? (make-primitive-procedure 'zero-flonum?))
(cl-define negative-flonum? (make-primitive-procedure 'negative-flonum?))
(cl-define positive-flonum? (make-primitive-procedure 'positive-flonum?))
(cl-define less-than-flonum? (make-primitive-procedure 'less-than-flonum?))
(cl-define greater-than-flonum? (make-primitive-procedure 'greater-than-flonum?))
(cl-define equal-flonum? (make-primitive-procedure 'equal-flonum?))

(cl-define exp-flonum (make-primitive-procedure 'exp-flonum))
(cl-define sine-flonum (make-primitive-procedure 'sine-flonum))
(cl-define cosine-flonum (make-primitive-procedure 'cosine-flonum))

;;; Binary arithmetic ops

(generate-type-optimizer
 (+ a b)
 (#t #t)
 ((((fixnum a) (fixnum b))    fixnum   fixnum            (plus-fixnum a b)               #t)
  (((float a) (float b))      *        float             (plus-flonum a b)               #t)
  ((((complex float) a) 
    ((complex float) b))      *        (complex float)   

    (let (((:gensym the-a) a)
	  ((:gensym the-b) b))
      (declare (type (complex float) (:gensym the-a) (:gensym the-b)))
      (complex (+ (realpart (:gensym the-a))
		  (realpart (:gensym the-b)))
	       (+ (imagpart (:gensym the-a))
		  (imagpart (:gensym the-b)))))     #t)))

(generate-type-optimizer
 (- a b)
 (#t #t)
 ((((fixnum a) (fixnum b))    fixnum   fixnum            (minus-fixnum a b)              #t)
  (((float a) (float b))      *        float             (minus-flonum a b)              #t)
  ((((complex float) a) 
    ((complex float) b))      *        (complex float)   

(let (((:gensym the-a) a)
      ((:gensym the-b) b))
  (declare (type (complex float) (:gensym the-a) (:gensym the-b)))
  (complex (- (realpart (:gensym the-a))
	      (realpart (:gensym the-b)))
	   (- (imagpart (:gensym the-a))
	      (imagpart (:gensym the-b)))))      #t)))

(generate-type-optimizer
 (* a b)
 (#t #t)
 ((((fixnum a) (fixnum b)) fixnum fixnum           (multiply-fixnum a b)               #t)
  (((float a) (float b))   *      float            (multiply-flonum a b)               #t)
  ((((imaginary float) a)
    (real b))              *      (imaginary float)    (optimize-*-imag-real a b)      #t)
  (((real a)
    ((imaginary float) b)) *      (imaginary float)    (optimize-*-imag-real b a)      #t)
  ((((imaginary float) a)
     ((imaginary float) b)) *     float            (- (* (imagpart a) (imagpart b)))   #t) 
  ((((complex float) a)
    (real b))              *      (complex float)  (optimize-*-real-complex b a)   #t)
  (((real a)
    ((complex float) b))   *      (complex float)  (optimize-*-real-complex a b)   #t)
  ((((complex float) a)
    ((complex float) b))   *      (complex float)  

    (let* (((:gensym the-a) a)
	   ((:gensym the-b) b)
	   ((:gensym ra) (realpart (:gensym the-a)))
	   ((:gensym ia) (imagpart (:gensym the-a)))
	   ((:gensym rb) (realpart (:gensym the-b)))
	   ((:gensym ib) (imagpart (:gensym the-b))))
      (declare (float (:gensym ra) (:gensym ia)
		      (:gensym rb) (:gensym ib))
	       (type (complex float) (:gensym the-a) (:gensym the-b)))
      (complex (- (* (:gensym ra) (:gensym rb))
		  (* (:gensym ia) (:gensym ib)))
	       (+ (* (:gensym ra) (:gensym ib))
		  (* (:gensym ia) (:gensym rb)))))   #t)))

(define-macro (optimize-*-real-complex xreal xcomplex)
  (let ((the-real (gensym)))
    `(let ((,the-real (float ,xreal)))
       (declare (float ,the-real))
       (complex (* ,the-real (realpart ,xcomplex))
		(* ,the-real (imagpart ,xcomplex))))))

(define float-zero ((make-primitive-procedure 'coerce-integer-to-flonum) 0))

(define-macro (optimize-*-imag-real ximag xreal)
  `(complex ,float-zero (* (imagpart ,ximag) ,xreal)))

(generate-type-optimizer
 (/ a b)
 (#t #t)
 ((((float a) (float b))   *      float         (divide-flonum a b)             #t)))

(generate-type-optimizer
 (complex a b)
 (#t #t)
 (((((and float (satisfies zerop)) a) (float b)) * (imaginary float) (optimize-complex-float a b) #t)
  (((float a) (float b))                         * (complex float)   (optimize-complex-float a b) #t)))

(define complex-tag (microcode-type 'complex))

(define-macro (optimize-complex-float a b)
  `(locally
    (declare (function system-pair-cons (#t #t #t) (complex float)))
    (system-pair-cons ,complex-tag ,a ,b)))


;;; Unary arithmentic ops

(generate-type-optimizer
 (1+ a)
 (#t)
 ((((fixnum a))   fixnum          fixnum        (one-plus-fixnum a)         #t)))

(generate-type-optimizer
 (1- a)
 (#t)
 ((((fixnum a))   fixnum          fixnum        (minus-one-plus-fixnum a)   #t)))

;; Catch some of the SCHEME cases

(generate-type-optimizer
 (-1+ a)
 (#t)
 ((((fixnum a))   fixnum          fixnum        (minus-one-plus-fixnum a)   #t)))

(generate-type-optimizer
 (realpart a)
 (#t)
 (((((complex float) a))  *        float             (system-pair-car a)        #t)
  (((complex a))          *        :required-type    (system-pair-car a)        #t)))

(generate-type-optimizer
 (imagpart a)
 (#t)
 (((((complex float) a))  *        float             (system-pair-cdr a)        #t)
  (((complex a))          *        :required-type    (system-pair-cdr a)        #t)))

(generate-type-optimizer
 (float a)
 (#t)
 ((((float a))            *        float             a                                          #t)
  (((fixnum a))           *        float             ((no-fundefsym coerce-fixnum-to-flonum) a) #t)
  (((integer a))          *        float             (coerce a 'float)                          #t)))


;;; Unary arithmetic predicates

(define-macro (std-unary-arith-pred-optimizer name fixnum-prim-name flonum-prim-name)
  (let ((temp (generate-uninterned-symbol 'g)))
    `(generate-type-optimizer
      (,name a)
      (#t)
      ((((fixnum a)) * (member t nil) 
		     (let ((,temp (the fixnum a))) 
		       (,fixnum-prim-name (the fixnum ,temp)))        #t)
       (((float a))  * (member t nil) 
		     (let ((,temp (the float a))) 
		       (,flonum-prim-name (the float ,temp)))        #t)))))

(std-unary-arith-pred-optimizer zerop zero-fixnum? zero-flonum?)
(std-unary-arith-pred-optimizer plusp positive-fixnum? positive-flonum?)
(std-unary-arith-pred-optimizer minusp negative-fixnum? negative-flonum?)


;;; Binary arithmetic predicates

(define-macro (std-binary-arith-pred-optimizer name fixnum-prim-name flonum-prim-name negate?)
  (let ((temp1 (generate-uninterned-symbol 'g))
	(temp2 (generate-uninterned-symbol 'g)))
    (if negate?
	`(generate-type-optimizer
	  (,name a b)
	  (#t #t)
	  ((((fixnum a) (fixnum b)) * (member t nil) 
				    (let ((,temp1 (the fixnum a))
					  (,temp2 (the fixnum b)))
				      (not (the (member t nil)
						(,fixnum-prim-name (the fixnum ,temp1) (the fixnum ,temp2)))))  #t)
	   (((float a) (float b))   * (member t nil) 
				    (let ((,temp1 (the float a))
					  (,temp2 (the float b)))
				      (not (the (member t nil)
						(,flonum-prim-name (the float ,temp1) (the float ,temp2)))))  #t)))
	`(generate-type-optimizer
	  (,name a b)
	  (#t #t)
	  ((((fixnum a) (fixnum b)) * (member t nil) 
				    (let ((,temp1 (the fixnum a))
					  (,temp2 (the fixnum b)))
				      (,fixnum-prim-name (the fixnum ,temp1) (the fixnum ,temp2)))  #t)
	   (((float a) (float b))   * (member t nil) 
				    (let ((,temp1 (the float a))
					  (,temp2 (the float b)))
				      (,flonum-prim-name (the float ,temp1) (the float ,temp2)))  #t))))))


(std-binary-arith-pred-optimizer <  less-than-fixnum? less-than-flonum? #f)
(std-binary-arith-pred-optimizer >= less-than-fixnum? less-than-flonum? #t)
(std-binary-arith-pred-optimizer >  greater-than-fixnum? greater-than-flonum? #f)
(std-binary-arith-pred-optimizer <= greater-than-fixnum? greater-than-flonum? #t)
(std-binary-arith-pred-optimizer =  equal-fixnum? equal-flonum? #f)
(std-binary-arith-pred-optimizer /= equal-fixnum? equal-flonum? #t)


;;; Trancendental ops

(generate-type-optimizer
 (exp arg)
 (#t)
 ((((float arg))                * float                (exp-flonum arg) #t)
  ((((imaginary float) arg))    * (complex float)
				(let (((:gensym theta) (imagpart arg)))
				  (declare (float (:gensym theta)))
				  (complex (cos (:gensym theta))
					   (sin (:gensym theta))))       #t)
  ((((complex float) arg)) * (complex float)      (let (((:gensym real-part) (realpart arg))
							((:gensym imag-part) (imagpart arg)))
						    (declare (float (:gensym real-part)
								    (:gensym imag-part)))
						    (let (((:gensym multiplier) (exp (:gensym real-part))))
						      (declare (float (:gensym multiplier)))
						      (complex (* (cos (:gensym imag-part))
								  (:gensym multiplier))
							       (* (sin (:gensym imag-part))
								  (:gensym multiplier))))) #t)))

(generate-type-optimizer
 (cis arg)
 (#t)
 ((((float arg)) * (complex float) (complex (the float (cos arg))
					    (the float (sin arg)))  #t)))

(generate-type-optimizer
 (sin a)
 (#t)
 ((((float a))   * float (sine-flonum a)                                #t)))

(generate-type-optimizer
 (cos a)
 (#t)
 ((((float a))   * float (cosine-flonum a)                                #t)))

