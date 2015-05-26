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
;;;
;;; -*- Scheme -*-

(proclaim '(insert-touches t))
(proclaim '(lr-evaluation nil))

(in-package 'fft)

(export '(fft-test))

(use-package 'fft 'user)

(defvar *debug* nil)

(defmacro dfuture (e)
  `(possibly-spawn-process (lambda () ,e)))

(defun possibly-spawn-process (thunk)
  (if (= (work-queue-length) 0)
      (%spawn-process thunk "" t)
      (funcall thunk)))

(defun fft-test (n m &key (part :real) (scale 0.1) (debug nil))
  (let ((*debug* debug))
    (graphit (funcall
	      (case part
		(:mag #'vmag)
		(:real #'vreal)
		(:imag #'vimag))
	      (fft (f n m)))
	     scale)))

(defun fft (x)
    (fft1 x))

(defun fft1 (x)
  (declare (simple-vector x))
  (let ((N (length x)))
    (declare (fixnum N))
    (if (= N 1)
	x
	(multiple-value-bind
	 (even odd)
	 (even-odd x)
	 (combine-ffts
	  (dfuture (fft1 even))
	  (dfuture (fft1 odd)))))))

(let ((lock (make-lock)))
  (defun atomic-inform-user (N key)
    (with-lock lock
	       (format t "Creating ~a task for N = ~a~%" key N))))

(defun even-odd (x)
  (declare (simple-vector x))
  (let* ((N (length x))
	 (N/2 (/ N 2))
	 (even (make-array N/2))
	 (odd (make-array N/2)))
    (declare (fixnum N N/2)
	     (simple-vector even odd))
    (do ((i 0 (the fixnum (+ i 2)))
	 (j 0 (the fixnum (+ j 1))))
	((= i N) (values even odd))
      (declare (fixnum i j))
      (setf (svref even j) (svref x i))
      (setf (svref odd j) (svref x (the fixnum (1+ i)))))))

(defun combine-ffts (g h)
  (declare (simple-vector g h))
  (let* ((N/2 (length g))
         (N (* N/2 2))
         (r (make-array N)))
    (declare (fixnum N N/2)
	     (simple-vector r))
    (do ((k 0 (the fixnum (+ k 1)))
         (l 0 (the fixnum (mod (the fixnum (+ l 1)) N/2))))
        ((= k N) r)
      (declare (fixnum k l))
      (let ()
	(declare (function + ((complex float) (complex float)) (complex float))
		 (function * ((complex float) (complex float)) (complex float))
		 (function wexp (fixnum fixnum) (complex float)))
	(setf (svref r k)
	      (+ (svref g l)
		 (* (wexp N k)
		    (the (complex float) (svref h l)))))))))

;;;
;;; e**((-j*2pi*k)/N)
;;;


(defconstant the-j (sqrt -1))
;;(defvar the-j (sqrt -1))
(proclaim '(type (imaginary float) the-j))
(defconstant 2pi (* 2 pi))
;;(defvar 2pi (* 2 pi))
(proclaim '(float 2pi))

(defun wexp (N k)
  (declare (fixnum N k))
  (exp (* the-j (* 2pi (/ (float k) (float N))))))

;;; The functions below never get futures

(proclaim '(insert-touches nil))
      
(defun f (n m)
  (declare (fixnum n m))
  (let ((x (make-array n :initial-element #c(0.0 0.0))))
    (do ((i 0 (+ i 1))) 
	((>= i m) nil)
      (declare (fixnum i))
      (setf (svref x i) #c(1.0 0.0)))
    x))

(defun vmag (v)
  (let ((r (make-array (length v))))
    (dotimes (i (length v))
	     (setf (svref r i) (abs (svref v i))))
    r))

(defun vreal (v)
  (let ((r (make-array (length v))))
    (dotimes (i (length v))
	     (setf (svref r i) (realpart (svref v i))))
    r))

(defun vimag (v)
  (let ((r (make-array (length v))))
    (dotimes (i (length v))
	     (setf (svref r i) (imagpart (svref v i))))
    r))

(defun graphit (v &optional (yunits 0.1))
  (multiple-value-bind
   (min max)
   (vector-min-max v)
   (let* ((l (length v))
	  (y-span (abs (- max min)))
	  (y-size (ceiling (/ y-span yunits)))
	  (bias (- min))
	  (y-vector (make-array (1+ y-size)))
	  (str (make-string l :initial-element #\space)))
     (dotimes (x-val l)
	      (let ((y-val (round (* (/ (+ (svref v x-val) bias) y-span) y-size))))
		(push x-val (svref y-vector y-val))))
     (do ((i (1- (length y-vector)) (1- i)))
	 ((< i 0) nil)
       (if (null (svref y-vector i))
	   (terpri)
	   (progn
	    (dolist (x (svref y-vector i))
		    (setf (schar str x) #\*))
	    (write-line str)
	    (dolist (x (svref y-vector i))
		    (setf (schar str x) #\space))))))))

(defun vector-min-max (v)
  (let ((l (length v)))
    (if (= l 1) 
	(values (svref v 0) (svref v 0))
	(let ((v0 (svref v 0)))
	  (do ((i 1 (1+ i))
	       (min v0 (let ((x (svref v i)))
			 (if (< x min) x min)))
	       (max v0 (let ((x (svref v i)))
			 (if (> x max) x max))))
	      ((= i l) (values min max)))))))
