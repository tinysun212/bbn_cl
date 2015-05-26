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
(proclaim '(insert-touches t))

;;;; Equal

(defun equal (x y)
  (or
   (eq? x y)
   (and (consp x) 
	(consp y)
	(equal (car x) (car y))
	(equal (cdr x) (cdr y)))
   (and
    (or (characterp x) (numberp x))
    (eql x y))
   (and (stringp x)
	(stringp y)
	(string= x y))
   (and (bit-vector-p x)
	(bit-vector-p y)
	(bit-vector-equal x (length x) y (length y)))
   (and (pathnamep x)
	(pathnamep y)
	(equal (pathname-components x #'list)
	       (pathname-components y #'list)))))

(proclaim '(function equal (t t) (member t nil)))

(defun bit-vector-equal (x x-length y y-length)
  (declare (insert-touches nil)
	   (fixnum x-length y-length))
  (labels ((element-equal (index)
             (declare (fixnum index))
	     (if (= index x-length)
		 t
		 (and (equal (aref x index) (aref y index))
		      (element-equal (1+ index))))))
    (and (= x-length y-length)
	 (element-equal 0))))

;;;; Equalp

(cl-define ivector-equal (make-primitive-procedure 'cl-ivector-equal))

(defun equalp (x y)
  (or 
   (eq? x y)
   (and (consp x) 
	(consp y)
	(equalp (car x) (car y))
	(equalp (cdr x) (cdr y)))
   (and (characterp x) 
	(characterp y)
	(char-equal x y))
   (and (stringp x)
	(stringp y)
	(string= x y))
   (and (bit-vector-p x)
	(bit-vector-p y)
	(bit-vector-equal x (length x) y (length y)))
   (and (eq? (object-type x) 'cl-ivector)
	(eq? (object-type y) 'cl-ivector)
	(ivector-equal x y))
   (and (numberp x) 
	(numberp y)
	(= x y))
   (and (pathnamep x)
	(pathnamep y)
	(equal (pathname-components x #'list)
	       (pathname-components y #'list)))
   (and (arrayp x)
	(arrayp y)
	(array-equalp x y))))

(proclaim '(function equalp (t t) (member t nil)))

(defun array-equalp (x y)
  (let ((x (touch x))
	(y (touch y)))
    (declare (insert-touches nil)
	     (array x y))
    (let ((dimsx (array-dimensions x))
	  (dimsy (array-dimensions y)))
      (declare (list dimsx dimsy))
      (let ((countx (apply #'* dimsx))
	    (county (apply #'* dimsy)))
	(declare (fixnum countx county))
	(and (= (length dimsx) (length dimsy))
	     (= countx county)
	     (prim-with-values
	      (lambda () (find-data-vector x))
	      (lambda (xx x-offset)
		(prim-with-values
		 (lambda () (find-data-vector y))
		 (lambda (yy y-offset)
		   (do-vector-equalp (touch xx) x-offset (touch yy) y-offset countx))))))))))

(defun do-vector-equalp (x x-index y y-index count)
  (declare (insert-touches nil)
	   (fixnum x-index y-index count)
	   (simple-vector x y))
  (if (zero? count)
      t
      (and (equalp (aref x x-index) (aref y y-index))
	   (do-vector-equalp x (1+ x-index) y (1+ y-index) (-1+ count)))))

