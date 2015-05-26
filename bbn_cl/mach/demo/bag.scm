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
;;;;	This package implements the bag abstraction which
;	gets us our parallelism.

(declare (usual-integrations))

(define *bag-size* (n-interpreters))

(define (bag-nil)
  (let ((new-bag (vector-cons (1+ *bag-size*) ())))
    (vector-set! new-bag 0 1)
    new-bag))

(define (bag-cons item bag)
  (let ((idx (vector-ref bag 0)))
    (vector-set! bag idx (cons item (vector-ref bag idx)))
    (vector-set! bag 0
		 (if (= (1+ idx) (vector-length bag))
		     1 (1+ idx)))
    bag))

(define (bag-length bag)
  (let loop ((idx 1) (sum 0))
    (if (= idx (vector-length bag))
	sum
	(loop (1+ idx) (+ sum (length (vector-ref bag idx)))))))

(define (bag-mapcar fcn bag)
  (let ((new-bag (bag-nil)))
    (let loop ((idx 1))
      (if (= idx (vector-length bag))
	  new-bag
	  (sequence
	    (vector-set! new-bag idx
			 (future (mapcar fcn (vector-ref bag idx))))
	    (loop (1+ idx)))))))

(define (bag-mapcan fcn bag)
  (let ((new-bag (bag-nil)))
    (let loop ((idx 1))
      (if (= idx (vector-length bag))
	  new-bag
	  (sequence
	    (vector-set! new-bag idx
			  (future (mapcan fcn (vector-ref bag idx))))
	    (loop (1+ idx)))))))

(define (bag-reduce mrg-fcn bot-fcn bag)
  (let recurse ((lb 1) (hb (-1+ (vector-length bag))))
    (if (= lb hb)
	(future (bot-fcn (vector-ref bag lb)))
	(if (= (1+ lb) hb)
	    (mrg-fcn (future (bot-fcn (vector-ref bag lb)))
		     (future (bot-fcn (vector-ref bag hb))))
	    (let ((half (floor (/ (- hb lb) 2))))
	      (mrg-fcn (future (recurse lb (+ lb (-1+ half))))
		       (future (recurse (+ lb half) hb))))))))

(define (bag-mapcar2 fcn baga bagb)
  (let ((newbag (bag-nil)) (bag-end (vector-length baga)))
    (let loop ((idx 1))
      (if (= idx bag-end)
	  newbag
	  (sequence
	    (vector-set! newbag idx
			 (future (fcn (vector-ref baga idx)
				      (vector-ref bagb idx))))
	    (loop (1+ idx)))))))

(define (bag-foreach-slot fcn bag)
  (let ((bag-end (vector-length bag)))
    (let loop ((idx 1) (result ()))
      (if (= idx bag-end)
	  result
	  (loop (1+ idx) 
		(cons (future (fcn (vector-ref bag idx)))
		      result))))))

;	Convert a bag to a list or a list to a bag.

(define (list->bag lst)
  (let ((result (bag-nil)))
    (mapc (lambda (x) (bag-cons x result)) lst)
    result))

(define (bag->list bag)
  (let loop ((idx 1) (result ()))
    (if (= idx (vector-length bag))
	result
	(loop (1+ idx) (append (vector-ref bag idx) result)))))
