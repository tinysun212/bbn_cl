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
;;;;	Various ways to multiply matrices.


(declare (usual-integrations)
	 (integrate-primitive-procedures
		(+ &+) (- &-) (/ &/) (* &*) (< &<) (= eq?)
		-1+ floor 1+))

(define (matrix-cons n m val)
  (let ((matrix (vector-cons (+ (* m n) 2) val)))
    (vector-set! matrix 0 n)
    (vector-set! matrix 1 m)
    matrix))

(define (matrix-fill n m values)
  (let ((mat (matrix-cons n m 0)) (bound (* n m)))
    (let fill-loop ((i 0) (v values))
      (if (null? v) (set! v values))
      (if (>= i bound)
          mat
	  (sequence
	    (vector-set! mat (+ i 2) (car v))
	    (fill-loop (1+ i) (cdr v)))))))

(define-macro (matrix-ref matrix i j)
    `(vector-ref ,matrix (+ (+ (* ,i (vector-ref ,matrix 1)) ,j) 2)))

(define-macro (matrix-set! matrix i j value)
  `(vector-set! ,matrix (+ (+ (* ,i (vector-ref ,matrix 1)) ,j) 2) ,value))

(define-macro (matrix-rows matrix)
  `(vector-ref ,matrix 1))

(define-macro (matrix-cols matrix)
    `(vector-ref ,matrix 0))

(define (inner-product mat1 i mat2 k)
  (let inner-loop ((j (-1+ (matrix-rows mat1))) (result 0))
    (if (< j 0)
        result
        (inner-loop
          (-1+ j)
          (+ result (* (matrix-ref mat1 i j) (matrix-ref mat2 j k)))))))

(define (matrix-multiply a b)
  (let ((n (matrix-cols a)) (m (matrix-rows b)))
    (let ((c (matrix-cons n m 0)))
      (let outer-loop ((i (-1+ n)))
        (if (< i 0)
	  c
	  (sequence
	    (future
	     (let inner-loop ((j (-1+ m)))
	       (if (< j 0)
		   'finished-inner-loop
		   (sequence
		     (matrix-set! c i j (future (inner-product a i b j)))
		     (inner-loop (-1+ j))))))
	    (outer-loop (-1+ i))))))))


(define (bad-matrix-multiply a b)
  (let ((n (matrix-cols a)) (m (matrix-rows b)))
    (let ((c (matrix-cons n m 0)))
      (let outer-loop ((i (-1+ n)))
        (if (< i 0)
	  c
	  (sequence
	    (let inner-loop ((j (-1+ m)))
		 (if (< j 0)
		     'finished-inner-loop
		     (sequence
		       (matrix-set! c i j (future (inner-product a i b j)))
		       (inner-loop (-1+ j)))))
	    (outer-loop (-1+ i))))))))

(define (time-matrix n a b)
  (if (> n 0)
    (let ((start (runtime))
	  (result (matrix-multiply a b)))
      (let wait-loop ()
	(if (eq? (matrix-ref result 0 0) 0)
	    (wait-loop)))
      (touch (matrix-ref result 0 0))
      (print (list 'elapsed (- (runtime) start)))
      (time-matrix (-1+ n) a b))))
