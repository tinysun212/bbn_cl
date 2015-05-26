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
;;;;	Polynomial abstractions.

(declare (usual-integrations))

(define poly-variables car)
(define (poly-terms poly)
  (if (vector? (cdr poly))
      (cdr poly)
      (list->bag (cdr poly))))
(define make-poly cons)

(define term-coeff car)
(define term-weight cadr)
(define term-factors cddr)
(define term-last-factor? null?)
(define term-next-factor cdr)
(define term-this-factor car)
(define (make-term coeff factors)
  (cons coeff (cons () factors)))
(define (make-weighted-term coeff weight factors)
  (cons coeff (cons weight factors)))

(define factor-variable car)
(define factor-power cdr)
(define set-factor-power! set-cdr!)
(define factor-last-part? null?)
(define make-factor cons)

(define (print-poly poly)
  (mapc print-term (bag->list (poly-terms poly)))
  (newline))

(define (print-term term)
  (princ " ")
  (cond ((= (term-coeff term) 1)
	 (princ "+"))
	((> (term-coeff term) 0)
	 (princ "+")
	 (princ (term-coeff term)))
	(else
	 (princ (term-coeff term))))
  (mapc (lambda (factor)
	  (cond ((eq? (factor-power factor) 0) 'nothing)
		((eq? (factor-power factor) 1)
		 (princ " ")
		 (princ (factor-variable factor)))
		(else
		 (princ " ")
		 (princ (factor-variable factor))
		 (princ "^")
		 (princ (factor-power factor)))))
	(term-factors term)))


;	Make a test polynomial of a given length.

(define (build-poly len vbls)
  (let ((vbl-vector (list->vector vbls)))
    (let loop ((i 0) (result (bag-nil)))
      (if (< i len)
	  (loop (1+ i) (bag-cons (build-term vbl-vector) result))
	  (make-poly vbls result)))))

(define (build-term vbls)
  (let loop ((i 0) (result ()))
    (if (< i (vector-length vbls))
	(loop (1+ i)
	      (cons (make-factor (vector-ref vbls i) (random 10))
		    result))
	(make-term (- (random 100) 49) result))))
