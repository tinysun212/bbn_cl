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
;;;;	Bucket sort algorithm.
;
;	- Calculate the max-min list.
;	- Calculate the value of the highest weight.
;	- Weight each term and put it in the right bucket.
;	- Merge all the n-th buckets.
;	- Sort each bucket and then collect.
;	- Append all the buckets.

(declare (usual-integrations))


(define (better-sort poly)
  (let ((canonical-poly (canon-poly poly)))
    (let ((min-max (bag-reduce max-min-merge find-max-and-min
			       (poly-terms canonical-poly))))
      (let ((bucket-size (ceiling (/ (1+ (max-min-product min-max))
				     *bag-size*))))
	(make-poly
	 (poly-variables poly)
	 (bag-reduce
	  barely-append!
	  (lambda (termlist)
	    (let ((result (collect-terms (sort termlist term-greater?))))
	      (cons result (last result))))
	  (transpose-buckets
	   (bag-foreach-slot (lambda (termlist)
			       (put-in-buckets termlist bucket-size))
			     (bag-mapcar (lambda (term)
					   (weight-term term min-max))
					 (poly-terms canonical-poly))))))))))

(define (barely-append! xx yy)
  (cond ((null? (car xx)) yy)
	((null? (car yy)) xx)
	(else 
	 (set-cdr! (cdr xx) (car yy))
	 (cons (car xx) (cdr yy)))))

(define (last x)
  (if (null? x)
      ()
      (let loop ((xx x))
	   (if (null? (cdr xx))
	       xx
	       (loop (cdr xx))))))

(define (old-bucket-sort poly)

  (let ((canonical-poly (canon-poly poly)))
    (let ((min-max (bag-reduce max-min-merge find-max-and-min
			       (poly-terms canonical-poly))))
      (let ((bucket-size (ceiling (/ (1+ (max-min-product min-max))
				     *bag-size*))))
	(make-poly
	 (poly-variables poly)
	 (bag-reduce
	  cheap-append!
	  (lambda (termlist) (collect-terms (sort termlist term-greater?)))
	  (transpose-buckets
	   (bag-foreach-slot (lambda (termlist)
			       (put-in-buckets termlist bucket-size))
			     (bag-mapcar (lambda (term)
					   (weight-term term min-max))
					 (poly-terms canonical-poly))))))))))

;	Given a list of terms return a vector of partially sorted terms.

(define (put-in-buckets termlist bucket-size)
  (let ((result (vector-cons *bag-size* ())))
    (mapc (lambda (term)
	    (let ((buck-num (floor (/ (term-weight term) bucket-size))))
	      (vector-set! result buck-num
			   (cons term (vector-ref result buck-num)))))
	  termlist)
    result))


;	Given a list of vectors, each containing the partially sorted terms,
;	build a bag so that each slot of the bag contains a list of terms
;	belonging to a particular bucket.

(define (transpose-buckets buck-list)
  (let ((result (bag-nil)) (bag-end *bag-size*))
    (let loop ((idx 1))
      (if (= idx bag-end)
	  result
	  (sequence
	    (vector-set! result idx
			 (future (combine-nth-bucket buck-list (-1+ idx))))
	    (loop (1+ idx)))))))

(define (combine-nth-bucket buck-list nth)
  (let loop ((buck buck-list) (result ()))
    (if (null? buck)
	result
	(loop (cdr buck) (cheap-append! (vector-ref (car buck) nth) result)))))

; 	Cheap append - bare minimum in the middle.

(define (cheap-append! a b)
  (if (null? a)
      b
      (let loop ((aa a))
	(if (null? (cdr aa))
	    (sequence
	      (set-cdr! aa b)
	      a)
	    (loop (cdr aa))))))
