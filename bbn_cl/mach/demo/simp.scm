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
;;;;	Simplification works by canonicalizing the polynomial and
;	then sorting it by the powers of its factors.  A final merge
;	step combines terms with the same coefficients.

(declare (usual-integrations))

;	Sort a polynomial using the vector sort utility below.

(define (sort-poly poly)
  (make-poly (poly-variables poly)
	     (list->bag (sort-poly-terms poly))))

(define (sort-poly-terms poly)
  (let ((newpoly (weight-poly poly)))
    (bag-reduce merge-terms
		(lambda (tlist)
		  (collect-terms
		    (sort-terms tlist)))
		(poly-terms newpoly))))

;	Sort the terms assuming that they have been weighted.

(define (sort-terms tlist)
  (sort tlist term-greater?))

(define (term-greater? x y)
  (> (term-weight x) (term-weight y)))

;	Merge two term lists by comparing their weights.  This will merge
;	terms if necessary.

(define (merge-terms trma trmb)
  (cond ((null? trma) trmb)
	((null? trmb) trma)
	((= (term-weight (car trma))
	    (term-weight (car trmb)))
	 (cons (make-weighted-term
		 (+ (term-coeff (car trma)) (term-coeff (car trmb)))
		 (term-weight (car trma))
		 (term-factors (car trma)))
	       (merge-terms (cdr trma) (cdr trmb))))
	((> (term-weight (car trma))
	    (term-weight (car trmb)))
	 (cons (car trma) (merge-terms (cdr trma) trmb)))
	(else
	 (cons (car trmb) (merge-terms trma (cdr trmb))))))

;	Merge adjacent terms with the same weight by adding their coefficients.

(define (collect-terms terms)
  (cond ((null? terms) ())
	((null? (cdr terms)) terms)
	((= (term-weight (car terms))
	    (term-weight (cadr terms)))
	 (collect-terms
	  (cons (make-weighted-term
		  (+ (term-coeff (car terms)) (term-coeff (cadr terms)))
		  (term-weight (car terms))
		  (term-factors (car terms)))
		(cddr terms))))
	(else
	 (cons (car terms) (collect-terms (cdr terms))))))
