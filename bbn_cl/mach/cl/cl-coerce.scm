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
(proclaim '(insert-touches nil))

(export '(coerce float))

;;; Coerce:

(cl-define integer->float 
	   (let ()
	     (cl-define f (make-primitive-procedure 'coerce-integer-to-flonum))
	     (cl-named-lambda (integer->float x)
	       (let ((r (f x)))
		 (if (eq r x)
		     (error "Integer ~a too large to convert to float" x)
		     r)))))

(defun coerce (object output-type-spec)
  "Coerces the Object to an object of type Output-Type-Spec."
  (setq object (touch object))
  (setq output-type-spec (touch output-type-spec))
  (flet ((coerce-error () (error "~S can't be converted to type ~S." object output-type-spec)))
	(cond
	 ((typep object output-type-spec)
	  object)
	 ((eq output-type-spec 'character)
	  (character object))
	 ((integerp object)
	  (cond
	   ((subtypep output-type-spec 'float)
	    (integer->float object))
	   ((memq (type-specifier output-type-spec) '(complex ratio)) ; Kcl doesn't allow ratio here, 
	                                                              ;  but the rule of complex and rational 
         	                                                      ;  canonicalization seems to permit both.
	    object)
	   (t (coerce-error))))
	 ((ratiop object)
	  (cond
	   ((subtypep output-type-spec 'float)
	    (/ (integer->float (numerator object))
	       (integer->float (denominator object))))
	   ((eq (type-specifier output-type-spec) 'complex)
	    object)
	   (t (coerce-error))))
	 ((floatp object)
	  (if (eq (type-specifier output-type-spec) 'complex)
	      (complex object (integer->float 0))  ; Scheme reader would reduce 0.0 to 0
	      (coerce-error)))
	 ((complexp object) (coerce-error))
	 (t
	  (typecase object
		    (list
		     (case (type-specifier output-type-spec)
		       ((simple-string string) (list-to-string* object))
		       ((simple-bit-vector bit-vector) (list-to-bit-vector* object))
		       ((simple-vector vector array simple-array)
			(list-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (simple-string
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       ;; Can't coerce a string to a bit-vector!
		       ((simple-vector vector array simple-array)
			(vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (simple-bit-vector
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       ;; Can't coerce a bit-vector to a string!
		       ((simple-vector vector array simple-array)
			(vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (simple-vector
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       ((simple-string string) (vector-to-string* object))
		       ((simple-bit-vector bit-vector) (vector-to-bit-vector* object))
		       ((vector array simple-array) (vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (string
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       (simple-string (string-to-simple-string* object))
		       ;; Can't coerce a string to a bit-vector!
		       ((simple-vector vector simple-array array)
			(vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (bit-vector
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       ;; Can't coerce a bit-vector to a string!
		       (simple-bit-vector (bit-vector-to-simple-bit-vector* object))
		       ((simple-vector vector array simple-array)
			(vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (vector
		     (case (type-specifier output-type-spec)
		       (list (vector-to-list* object))
		       ((simple-string string) (vector-to-string* object))
		       ((simple-bit-vector bit-vector) (vector-to-bit-vector* object))
		       ((simple-vector vector array simple-array)
			(vector-to-vector* object output-type-spec))
		       (t (coerce-error))))
		    (t (coerce-error)))))))

(defun float (x &optional (type-obj 1e-1))
  (setq x (touch x))
  (setq type-obj (touch type-obj))
  (check-type type-obj float)
  (coerce x (type-of type-obj)))

;;; Internal Frobs:

(defun list-to-string* (object)
  (do* ((index 0 (1+ index))
	(length (list-length object))
	(result (make-string length)))
       ((= index length) result)
    (setf (schar result index) 
	  (locally
	   (declare (insert-touches t))
	   (pop object)))))

(defun list-to-bit-vector* (object)
  (do* ((index 0 (1+ index))
	(length (list-length object))
	(result (make-array length :element-type '(mod 2))))
       ((= index length) result)
       (declare (fixnum length))
       (setf (sbit result index)
	     (locally
	      (declare (insert-touches t))
	      (pop object)))))

;;; Has to touch cdrs, but not cars.

(defun list-to-vector* (object type)
  (do* ((index 0 (1+ index))
	(length (list-length object))
	(result (make-sequence-of-type type length)))
       ((= index length) result)
       (setf (aref result index)
	     (prog1
	      (car object)
	      (setq object (touch (cdr object)))))))

;;; Don't have to touch anything.

(defun vector-to-list* (object)
  (let ((result (list nil))
	(length (length object)))
    (do ((index 0 (1+ index))
	 (splice result (cdr splice)))
	((= index length) (cdr result))
      (rplacd splice (list (aref object index))))))

;;; Don't have to touch anything.

(defun vector-to-vector* (object type)
  (do* ((index 0 (1+ index))
	(length (length object))
	(result (make-sequence-of-type type length)))
       ((= index length) result)
    (setf (aref result index) (aref object index))))

(defun vector-to-string* (object)
  (do* ((index 0 (1+ index))
	(length (length object))
	(result (make-string length)))
       ((= index length) result)
    (setf (schar result index) (touch (aref object index)))))

(defun vector-to-bit-vector* (object)
  (do* ((index 0 (1+ index))
	(length (length object))
	(result (make-array length :element-type '(mod 2))))
       ((= index length) result)
    (declare (fixnum length))
    (setf (sbit result index) (touch (aref object index)))))

(defun string-to-simple-string* (object)
  (if (simple-string-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (fill-pointer object)))
	(declare (simple-string data))
	(subseq data start end))))

(defun bit-vector-to-simple-bit-vector* (object)
  (if (simple-bit-vector-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (fill-pointer object)))
	(declare (simple-bit-vector data))
	(subseq data start end))))


