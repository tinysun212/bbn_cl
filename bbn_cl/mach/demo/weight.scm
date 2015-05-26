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
;;;; 	This file calculates the highest and lowest exponents of each variable in
;	the bag.  It then calculates the weights for each term.

(declare (usual-integrations))

;	1) Canonicalize so that the factors in each term are identical.
;	2) Reduce to take the maximum and minimum of the powers of each factor.
;	3) Apply the weighting formula to each term building a new poly.

(define (weight-poly poly)
  (let ((canonical-poly (canon-poly poly)))
    (let ((min-max (bag-reduce max-min-merge find-max-and-min
			       (poly-terms canonical-poly))))
      (make-poly (poly-variables poly)
		 (bag-mapcar (lambda (x) (weight-term x min-max)) 
			     (poly-terms canonical-poly))))))

;	Weight a term by doing the following to each factor:
;		- multiply the value so far by max minus min
;		- add the current power minus min

(define (weight-term term mmlist)
  (let loop ((factor (term-factors term))
	     (result 0) (mm mmlist))
       (if (term-last-factor? factor)
	   (make-weighted-term (term-coeff term) result (term-factors term))
	   (loop (term-next-factor factor)
		 (+ (* result (1+ (- (cdar mm) (caar mm))))
		    (- (factor-power (term-this-factor factor))
		       (caar mm)))
		 (cdr mm)))))

;	Find the largest possible weight value.

(define (max-min-product mml)
  (if (null? mml)
      1
      (* (1+ (- (cdar mml) (caar mml)))
	 (max-min-product (cdr mml)))))

;	Combine two ((min . max) (min . max) .. ) lists.

(define (max-min-merge mma mmb)
  (cond ((null? mma) mmb)
	((null? mmb) mma)
	(else (do-max-min-merge mma mmb))))

(define (do-max-min-merge mma mmb)
  (if (null? mma)
      ()
      (cons (cons (min (caar mma) (caar mmb))
		  (max (cdar mma) (cdar mmb)))
	    (max-min-merge (cdr mma) (cdr mmb)))))

;	The result of this structure is of the form:
;		((min . max) (min . max) ... )

(define (find-max-and-min termlist)
  (let ((result (make-max-min-template (term-factors (car termlist)))))
    (mapc (lambda (x) (do-max-min (term-factors x) result)) termlist)
    result))

;	Given the result template and a list of factors do the appropriate
;	max-mins and side effect the template when necessary.

(define (do-max-min factor max-min)
  (if (null? max-min)
      ()
      (let ((power (factor-power (term-this-factor factor)))
	    (mm (car max-min)))
	(if (< power (car mm))
	    (set-car! mm power))
	(if (> power (cdr mm))
	    (set-cdr! mm power))
	(do-max-min (cdr factor) (cdr max-min)))))

;	Make the initial template: ((999999 . -999999) (999999 . -999999) ... )

(define (make-max-min-template factor)
  (if (term-last-factor? factor)
      ()
      (cons (cons 999999 -999999) 
	    (make-max-min-template (term-next-factor factor)))))

;	Canonicalizing a polynomial is done by reordering w.r.t. each
;	of its variables.

(define (canon-poly poly)
  (make-poly (poly-variables poly)
	     (bag-mapcar (lambda (term) (canon-term term (poly-variables poly)))
			 (poly-terms poly))))

; 	First build the resulting term with zero powers, then for each factor
;	in the term add its power to the appropriate "bucket" in the result.

(define (canon-term term vbls)
  (let ((result (make-term (term-coeff term) (canon-setup-result vbls))))
    (let loop ((factor (term-factors term)))
	 (if (term-last-factor? factor)
	     result
	     (sequence
	       (let ((bucket (assq (factor-variable (term-this-factor factor))
				   (term-factors result))))
		 (set-factor-power! bucket (+ (factor-power bucket)
					      (factor-power (term-this-factor factor)))))
	       (loop (term-next-factor factor)))))))

;	Given the list (a b c), this creates the list ((a . 0) (b . 0) (c . 0))
;	which is a list of trivial factors for the canonicalized term.

(define (canon-setup-result vbls)
  (if (null? vbls)
      ()
      (cons (make-factor (car vbls) 0)
	    (canon-setup-result (cdr vbls)))))

