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
;;;(declare (usual-integrations)
	 (integrate-primitive-procedures
	  (+ &+)
	  (- &-)
	  (= eq?)
	  (> &>)))

;
;	Usage and pointers to other things.
;

(define gauss-loaded? #f)
(define fft-loaded? #f)
(define matrix-loaded? #f)
(define poly-loaded? #f)
(define boyer-loaded? #f)

(define (help)
  (newline)
  (princ "Demo programs include:") (newline)
  (princ "  (fib <integer e.g. 11>)    computes Fibonacci") (newline)
  (princ "  (pfib <integer>)           parallel version of fib") (newline)
  (princ "  (qsort <list of numbers e.g. qsort-data>)") (newline)
  (princ "                  Quicksorts a list on n processors") (newline)
  (princ "  (hsort <list of numbers e.g. qsort-data>)") (newline)
  (princ "                  Parallel insertion sort") (newline)
  (princ "                  The variables best-case and worst-case") (newline)
  (princ "                      are short optimal and deoptimal lists") (newline)
  (newline)
  (if poly-loaded?
      (poly-help)
      (begin
	(princ "  (load-poly)     Loads the polynomial sort demo")
	(newline)))
  (if boyer-loaded?
      (boyer-help)
      (begin
	(princ "  (load-boyer)    Loads the Boyer-Moore theorem prover")
	(newline)))
  (if matrix-loaded?
      (begin
	(matrix-help)
	(gauss-help))
      (begin
	(princ "  (load-matrix)   Loads the matrix multiply")
	(newline)
	(princ "                      and the Gaussian demos")
	(newline)))
  (if fft-loaded?
      (fft-help)
      (begin
	(princ "  (load-fft)      Loads the FFT demo")
	(newline))))

(help)

;;; Fibonacci demos

(define (fib n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (pfib n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else (+ (future (pfib (- n 1)))
		 (future (pfib (- n 2)))))))

;;; Boyer Moore theorem prover

(define modus-ponens '(implies (and (implies a b) a) b))
(define transitivity '(implies (and (implies (f x) (g x))
				    (implies (g x) (h x)))
			       (implies (f x) (h x))))
(define three-deep '(implies (and (implies (f x) (g x))
				  (and (implies (g x) (h x))
				       (implies (h x) (i x))))
			     (implies (f x) (i x))))

(define (load-boyer)
  (load "fboyer.com")
  (load "boyer-data.bin")
  (set! boyer-loaded? #t)
  (boyer-help))

(define (boyer-help)
  (newline)
  (princ "Boyer-Moore Theorem Prover")
  (newline)
  (princ "  (taut? <tautology e.g. test-case>)")
  (newline)
  (princ "                  Determines if something is a tautology.")
  (newline)
  (princ "  (rewrite <tautology>)      rewrite portion of taut?")
  (newline)
  (newline)
  (princ "Examples include:")
  (newline)
  (princ "  modus-ponens    (a & a->b) -> b")
  (newline)
  (princ "  transitivity    (Fx->Gx & Gx->Hx) -> (Fx->Hx)")
  (newline)
  (princ "  three-deep      (Fx->Gx & Gx->Hx & Hx->Ix) -> (Fx->Ix)")
  (newline))


;;; Matrix and Gauss

(define mat-a)
(define mat-b)

(define (load-matrix)
  (load "matrix.com")
  (matrix-help)
  (load "gauss.com")
  (set! mat-a (matrix-fill 20 20 '(1 2 3)))
  (princ "Matrix 20x20 mat-a defined") (newline)
  (set! mat-b (matrix-fill 20 20 '(4 5 6)))
  (princ "Matrix 20x20 mat-b defined") (newline)
  (set! matrix-loaded? #t)
  (set! gauss-loaded? #t))

(define (matrix-help)
  (newline)
  (princ "  (matrix-fill N N <list of numbers>)") (newline)
  (princ "                  Builds a matrix of size NxN") (newline)
  (princ "  (time-matrix <count> <matrix-1> <matrix-2>)") (newline)
  (princ "                  Multiplies and times the two matrices") (newline)
  (princ "                  The variables mat-a and mat-b are 20x20") (newline)
  (princ "  (do-matrix)     multiplies mat-a by mat-b") (newline))
 
(define (do-matrix)
  (princ "Multiplying two 20x20 matrices") (newline)
  (time-matrix 1 mat-a mat-b)
  'done)

;;; FFT
(define (load-fft)
  (load "fft.com")
  (set! fft-loaded? #t))

;;; Polynomial sorting package

(define sample-polynomial)

(define (load-poly)
  (load "bag.com")
  (load "poly.com")
  (load "bucket.com")
  (load "weight.com")
  (load "polymath.com")
  (load "simp.com")
  (newline)
  (princ "Building a 400 element polynomial") (newline)
  (newline)
  (let ((baby-polynomial (build-poly 20 '(x y z))))
    (set! sample-polynomial (mult-poly baby-polynomial baby-polynomial)))
  (poly-help)
  (set! poly-loaded? #t))

(define (poly-help)
  (princ "Polynomial sort package:") (newline)
  (princ "    Inefficient sort/merge - (sort-merge)") (newline)
  (princ "    More efficient bucket sort - (bucket-sort)") (newline))

(define (sort-merge)
  (let ((start (runtime)))
    (princ "Sorting a 400 element polynomial") (newline)
    (princ "---- Notice the slow falloff in utilization") (newline)
    (touch (sort-poly sample-polynomial))
    (princ "Time elapsed ") (print (- (runtime) start)))
  'done)

(define (bucket-sort)
  (let ((start (runtime)))
    (princ "Sorting a 400 element polynomial") (newline)
    (princ "---- Notice relatively rapid utilization falloff") (newline)
    (touch (better-sort sample-polynomial))
    (princ "Time elapsed ") (print (- (runtime) start)))
  'done)

(load "qsort.com")

;;; Eight queens

;;; Various mapcars

(define (pmapcar1 fcn lst)
  (if (null? lst)
      '()
      (cons (future (fcn (car lst)))
	    (pmapcar1 fcn (cdr lst)))))

(define (pmapcar2 fcn lst)
  (if (null? lst)
      '()
      (cons (fcn (car lst))
	    (future (pmapcar2 fcn (cdr lst))))))

(define (pmapcar3 fcn lst)
  (if (null? lst)
      '()
      (future (cons (fcn (car lst))
		    (pmapcar3 fcn (cdr lst))))))

(define (mapcar-help)
  (newline)
  (princ "(pmapcar1 fcn lst)    Bad mapcar, spawns after traversal")
  (newline)
  (princ "(pmapcar2 fcn lst)    Returns the car, future the cdr")
  (newline)
  (princ "(pmapcar3 fcn lst)    Returns a future")
  (newline)
  (newline))

;;; Two ways to append!

(define append-result '(head))

(define (try-append times lens appender)
  (set! append-result (cons 'head '()))
  (let loop ((index 0))
    (if (= index times)
	'done
	(begin
	  (future
	   (let append-loop ((which 0))
	     (if (= which lens)
		 'done
		 (begin
		   (appender append-result
			     (cons (cons index which) '()))
		   (append-loop (1+ which))))))
	  (loop (1+ index))))))

(define (append!! result item)
  (if (set-cdr-if-eq?! result item '())
      item
      (append!! (cdr result) item)))

(define (append-help)
  (princ "(try-append <ntasks> <nelemts> append! or append!!)")
  (newline)
  (princ "    append! will lose elements")
  (newline)
  (princ "    append!! will collect all <ntasks>*<nelemts>+1")
  (newline)
  (princ "(length append-result) will show the length")
  (newline))

