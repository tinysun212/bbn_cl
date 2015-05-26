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
;;;;	Polynomial Arithmetic

(declare (usual-integrations))

;	To add two polynomials, just append them.

(define (add-poly p q)
  (make-poly (set-union (poly-variables p) (poly-variables q))
	     (bag-mapcar2 append p q)))

;	To multiply two polynomials we must cross multiply the terms.

(define (mult-poly p q)
  (let ((q-terms (bag->list (poly-terms q))))
    (make-poly (set-union (poly-variables p) (poly-variables q))
	       (bag-mapcan (lambda (tp)
			     (mapcar (lambda (tq)
				       (mult-term-term tp tq))
				     q-terms))
			   (poly-terms p)))))

(define (mult-term-term ta tb)
  (make-term (* (term-coeff ta) (term-coeff tb))
	     (append (term-factors ta) (term-factors tb))))

;	Merge variable lists!

(define (set-union a b)
  (cond ((null? a) b)
	((memq (car a) b) (set-union (cdr a) b))
	(else (set-union (cdr a) (cons (car a) b)))))
