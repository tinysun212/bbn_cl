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
;;;;;;; Emacs, this is Multi -*- Scheme -*- code
;;;
;;;  Gaussian Elimination
;;;  Reduces square matrix to upper-triangular.

(declare (usual-integrations))		; *ONE WORD INTEGERS for IBM 1130 hackers.

(define gauss-timer runtime)

(define (gtime method matrix)
  (display (list 'order (length matrix)))
  (let ((start (gauss-timer)))
    (method matrix)
    (list 'elapsed (- (gauss-timer) start))))

(define gauss-matrix-a '((2 3 5 33) (1 1 2 13) (5 4 1 26)))
(define gauss-matrix-b '((1 2 3 4 5 6 7 8 9 4)
			 (2 3 1 5 2 3 1 3 5 7)
			 (3 4 0 2 5 1 7 5 7 1)
			 (4 8 7 0 1 3 2 4 3 2)
			 (5 4 2 1 9 8 3 2 1 9)
			 (6 3 6 8 5 7 8 1 9 2)
			 (7 4 6 5 1 2 8 9 0 4)
			 (8 1 5 4 6 7 2 3 8 0)
			 (9 4 5 7 8 1 2 3 4 2)))

(define (make-gauss-matrix m)
  (define (make-matrix-row n)
    (if (= n 0)
	(cons (random 1000) '())
	(cons (random 1000)  (make-matrix-row (-1+ n)))))
  (define (make-n-rows n)
    (if (= n 0)
	'()
	(cons (make-matrix-row m) (make-n-rows (-1+ n)))))
  (make-n-rows m))

;; Sequential definition
(define (gauss matrix)

  (define (-rows a b) (mapcar - a b))
  (define (scale n a) (mapcar (lambda (x) (* n x)) a))

  (if (or (not matrix) (not (cdr matrix)))
      matrix
      (let ((first-row (car matrix)))
	(cons first-row
	      (mapcar (lambda (row) (cons 0 row))
		      (gauss
		       (mapcar
			(lambda (this-row)
			  (-rows (cdr this-row)
				 (scale (/ (car this-row)
					   (car first-row))
					(cdr first-row))))
			(cdr matrix))))))))


;; Parallel Gauss, with futures almost everywhere
(define (fffgauss matrix)

  (define (pmapcar f l)
    (if l
	(cons (future (f (car l)))
	      (future (pmapcar f (cdr l))))
	'()))

  (define (pmapcar2 f l1 l2)
    (if l1
	(cons (future (f (car l1) (car l2)))
	      (future (pmapcar2 f (cdr l1) (cdr l2))))
	'()))

  (define (-rows a b) (pmapcar2 - a b))
  (define (scale n a) (pmapcar (lambda (x) (* n x)) a))

  (if (or (not matrix) (not (cdr matrix)))
      matrix
      (let ((first-row (car matrix)))
	(cons first-row
	      (pmapcar (lambda (row) (cons 0 row))
		       (fffgauss
			(pmapcar
			 (lambda (this-row)
			   (-rows (cdr this-row)
				  (future
				   (scale (/ (car this-row)
					     (car first-row))
					  (cdr first-row)))))
			 (cdr matrix))))))))


;; Parallel Gauss, with fewer Futures (better)
(define (ffgauss matrix)

  (define (pmapcar f l)
    (if l
	(cons (f (car l)) (future (pmapcar f (cdr l))))
	'()))

  (define (pmapcar2 f l1 l2)
    (if l1
	(cons (f (car l1) (car l2))
		 (future (pmapcar2 f (cdr l1) (cdr l2))))
	'()))

  (define (-rows a b) (pmapcar2 - a b))
  (define (scale n a) (pmapcar (lambda (x) (* n x)) a))

  (if (or (not matrix) (not (cdr matrix)))
      matrix
      (let ((first-row (car matrix)))
	(cons first-row
	      (pmapcar (lambda (row) (cons 0 row))
		       (ffgauss
			(pmapcar
			 (lambda (this-row)
			   (-rows (cdr this-row)
				  (future
				   (scale (/ (car this-row)
					     (car first-row))
					  (cdr first-row)))))
			 (cdr matrix))))))))


;; Parallel Gauss, with some thought put to Future placement

(define (fgauss matrix)

  (define (pmapcar f l)
    (if l
	(cons (f (car l)) (future (pmapcar f (cdr l))))
	'()))

  (define (pmapcar2 f l1 l2)
    (if l1
	(cons (f (car l1) (car l2))
	      (future (pmapcar2 f (cdr l1) (cdr l2))))
	'()))

  (define (-rows a b) (future (pmapcar2 - a b)))
  (define (scale n a) (future (pmapcar (lambda (x) (* n x)) a)))

  (if (or (not matrix) (not (cdr matrix)))
      matrix
      (let ((first-row (car matrix)))
	(cons first-row
	      (pmapcar (lambda (row) (cons 0 row))
		       (future
			(fgauss
			 (pmapcar
			  (lambda (this-row)
			    (-rows (cdr this-row)
				   (scale (/ (car this-row)
					     (car first-row))
					  (cdr first-row))))
			  (cdr matrix)))))))))


(define (gauss-help)
  (newline) (newline)
  (display "Gauss's Method") (newline)
  (display "    (gtime gauss <matrix>)    - without futures") (newline)
  (display "    (gtime fgauss <matrix>)   - with minimal futures") (newline)
  (display "    (gtime ffgauss <matrix>)  - with some futures") (newline)
  (display "    (gtime fffgauss <matrix>) - with too many futures") (newline)
  (display "Matrices include:") (newline)
  (display "    gauss-matrix-a    3x4") (newline)
  (display "    gauss-matrix-b    9x10") (newline)
  (display "    (make-gauss-matrix <n>)") (newline)
  (newline))

(gauss-help)
