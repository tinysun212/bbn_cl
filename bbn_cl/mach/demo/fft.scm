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
;;;;;;
;;; Call fft-demo to get a graph of the magnitude of the Discrete Fourier Transform
;;;   of a rectangular pulse of total duration "duration" and width "width".
;;;

(declare (usual-integrations))

(define (fft-demo #!optional duration width yunits)
  (if (unassigned? duration)
      (set! duration 64))
  (if (unassigned? width)
      (set! width 5))
  (if (unassigned? yunits)
      (set! yunits 0.25))
  (if (not (is-power-of-two? duration))
      (error "Duration is not a power of two." duration))
  (display (list 'duration duration 'pulse-width width))
  (newline)
  (graphit (vmag (fft (make-rect-pulse duration width))) yunits))

(define *fft-limit* 8)

(define (fft-test x)
  (with-event-list event-list
    (lambda ()
      (fft x))))

(define (fft x)
  (log-event start-fft)
  (let ((x-length (vector-length x)))
    (let ((r
	   (cond ((= x-length 1)
		  x)
		 ((<= x-length *fft-limit*)
		  (combine-ffts (fft (even-case x)) (fft (odd-case x))))
		 (else
		  (combine-ffts (future (fft (even-case x)))
				(future (fft (odd-case x))))))))
      (log-event end-fft)
      r)))

;;; wrap "future" around these two fft 
;;;   calls to generate basic recursive parallelism

(define (even-case x)
  (let* ((N (vector-length x))
         (r (make-vector (quotient N 2))))
    (do ((i 0 (+ i 1))
         (j 0 (+ j 2)))
        ((= j N) r)
      (vector-set! r i (vector-ref x j)))))
      
(define (odd-case x)
  (let* ((N (vector-length x))
         (r (make-vector (quotient N 2))))
    (do ((i 0 (+ i 1))
         (j 1 (+ j 2)))
        ((> j (- N 1)) r)
      (vector-set! r i (vector-ref x j)))))

(define (combine-ffts g h)
  (let* ((N/2 (vector-length g))
         (N (* N/2 2))
         (r (make-vector N)))
    (do ((k 0 (+ k 1))
         (l 0 (modulo (+ l 1) N/2)))
        ((= k N) r)
      (vector-set! r k (c+ (vector-ref g l)
			   (c* (wexp N k)
			       (vector-ref h l)))))))

(define -2pi (-  0 (* 2 3.14159)))

;;;
;;; e**((-j*2*pi*k)/N)
;;;

(define (wexp N k)
  (cexp (* k (/ -2pi N))))

(define (cexp x)
  (cons (cos x) (sin x)))

(define (c+ x y)
  (cons (+ (car x) (car y))
        (+ (cdr x) (cdr y))))

(define (c* x y)
  (let ((a (car x))
        (b (cdr x))
        (c (car y))
        (d (cdr y)))
    (cons (- (* a c) (* b d))
          (+ (* a d) (* b c)))))

(define (make-rect-pulse duration width)
  (let ((x (make-vector duration '(0.0 . 0.0))))
    (do ((i 0 (+ i 1))) ((>= i width) #f) (vector-set! x i '(1.0 . 0.0)))
    x))

(define (vmag v)
  (let ((a (make-vector (vector-length v))))
    (do ((j 0 (1+ j)))
	((= j (vector-length v)) a)
      (let ((r (car (vector-ref v j)))
	    (i (cdr (vector-ref v j))))
	(vector-set! a j (sqrt (+ (* r r) (* i i))))))
    a))

(define (vreal v)
  (let ((a (make-vector (vector-length v))))
    (do ((j 0 (1+ j)))
	((= j (vector-length v)) a)
      (let ((r (car (vector-ref v j)))
	    (i (cdr (vector-ref v j))))
	(vector-set! a j r)))
    a))

(define (vimag v)
  (let ((a (make-vector (vector-length v))))
    (do ((j 0 (1+ j)))
	((= j (vector-length v)) a)
      (let ((r (car (vector-ref v j)))
	    (i (cdr (vector-ref v j))))
	(vector-set! a j i)))
    a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Draw a graph of the result.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphit v #!optional yunits)
  (if (unassigned? yunits) (set! yunits 0.1))
  (vector-min-max v
   (lambda (min max)
     (let* ((l (vector-length v))
	    (y-span (- max min))
	    (y-size (ceiling (/ y-span yunits)))
	    (bias (- min))
	    (y-vector (make-vector (1+ y-size)))
	    (str (make-string l #\space)))
       (do ((x-val 0 (1+ x-val)))
	   ((= x-val l) #f)
	 (let ((y-val (round (* (/ (+ (vector-ref v x-val) bias) 
				   y-span)
				y-size))))
	   (vector-set! y-vector y-val
			(cons x-val (vector-ref y-vector y-val)))))
       (do ((i (-1+ (vector-length y-vector)) (-1+ i)))
	   ((< i 0) #f)
	 (do ((x (vector-ref y-vector i) (cdr x)))
	     ((null? x) #f)
	   (string-set! str (car x) #\*))
	 (princ str)
	 (newline)
	 (do ((x (vector-ref y-vector i) (cdr x)))
	     ((null? x) #f)
	   (string-set! str (car x) #\space)))))))

(define (vector-min-max v cont)
  (let ((l (vector-length v)))
    (if (= l 1) 
	(cont (vector-ref v 0) (vector-ref v 0))
	(do ((i 1 (1+ i))
	     (min (vector-ref v 0) (if (< (vector-ref v i) min) (vector-ref v i) min))
	     (max (vector-ref v 0) (if (> (vector-ref v i) max) (vector-ref v i) max)))
	    ((= i l) (cont min max))))))
  
(define (is-power-of-two? n)
  (and (integer? n)
       (or (= n 1)
	   (is-power-of-two? (/ n 2)))))

(define-event-list event-list
  (
   (start-fft)
   (end-fft)
))



(define (fft-help)
  (newline)
  (display "FFT Demo - Transform a Pulse, Draw Graph of Result") (newline)
  (display "    (fft-demo duration pulse-width)") (newline)
  (display "        duration - power of two # elements") (newline)
  (display "        pulse-width - width of pulse being transformed") (newline)
  (display "    *fft-limit* - vector size processed within a single task (default 8)") (newline)
  (display "    Try (fft-demo) -> defaults to duration 64, width 5") (newline)
  (newline))

(fft-help)


