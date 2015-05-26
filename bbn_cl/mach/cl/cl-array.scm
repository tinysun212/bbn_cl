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

(export '(array-rank-limit array-dimension-limit array-total-size-limit
	  make-array vector adjust-array
	  bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
	  bit-orc1 bit-orc2 bit-not
	  vectorp stringp bit-vector-p
	  simple-vector-p simple-string-p simple-bit-vector-p))

(eval-when (compile load eval)
	   (cl-define %array-rank-limit (make-primitive-procedure 'cl-array-rank-limit))
	   (cl-define %array-dimension-limit (make-primitive-procedure 'cl-array-dimension-limit))
	   (cl-define %array-total-size-limit (make-primitive-procedure 'cl-array-total-size-limit))
)

(defconstant array-rank-limit (%array-rank-limit)
  "The upper bound on the rank of an array.")
(defconstant array-dimension-limit (%array-dimension-limit)
  "The upper bound on any given dimension of an array.")
(defconstant array-total-size-limit (%array-total-size-limit)
  "The upper bound on the total number of element in an array.")

;; This currently does NOT make sure that element-type, if specified,
;; is actually a legitimate type!  As it stands, if it passes something
;; that's not a type in CommonLisp, it (the micro-code) blindly assumes
;; type T.
;;
(defun make-array (dimensions &key
			      (element-type t)
			      (initial-element nil initial-element-p)
			      initial-contents adjustable
			      fill-pointer displaced-to
			      displaced-index-offset)
  "Creates an array of the specified dimensions.  See manual for details."
  (setq element-type (canonicalize-array-element-type element-type))
  (if (and initial-element-p (not (typep initial-element element-type)))
      (error "The specified :initial-element ~A is incompatible with type ~S."
	     initial-element element-type))
  (if (and initial-element-p (or initial-contents displaced-to))
      (error "The :initial-element option may not be used with the :initial-contents~%or :displaced-to option."))
  (if (and initial-contents (or initial-element-p displaced-to))
      (error "The :initial-contents option may not be used with the :initial-element~%or :displaced-to option."))
  (cond (displaced-to
	 (if (not (arrayp displaced-to))
      	     (error ":displaced-to argument must be an array."))
  	 (if (not (subtypep element-type (array-element-type displaced-to)))
      	     (error "One can't displace an array of type ~S into another of type ~S"
	     	    element-type (array-element-type displaced-to)))))
  (%make-array dimensions element-type initial-element initial-contents
	       adjustable fill-pointer displaced-to displaced-index-offset))


(defun canonicalize-array-element-type (type-spec)
  (if (not (consp type-spec))
      type-spec
      (cond
       ((eq (car type-spec) 'mod)
	(canonicalize-array-element-type `(integer 0 ,(1- (cadr type-spec)))))
       ((eq (car type-spec) 'integer)
	(let ((low (cadr type-spec))
	      (high (caddr type-spec)))
	  (if (not (and
		    (integerp low)
		    (integerp high)
		    (<= low high)))
	      type-spec
	      (cond
	       ((<= 0 low high)
		`(unsigned-byte ,(ceiling (log (1+ high) 2))))
	       ((< low 0 high)
		`(signed-byte ,(1+ (max (ceiling (log (- low) 2))
					(ceiling (log (1+ high) 2))))))
	       (t type-spec)))))
       (t type-spec))))

(defun vector (&rest objects)
  "Creates a simple general vector with specified initial contents.
  It is analogous to the function LIST."
  (make-array (length objects) :initial-contents objects))

(defun adjust-array (array new-dimensions
			   &key
			   (element-type t element-type-p)
			   (initial-element nil initial-element-p)
			   initial-contents fill-pointer displaced-to
			   displaced-index-offset)
  "Adjusts an array in the specified manner.  See manual for details."
  (if (not (listp new-dimensions))
      (setq new-dimensions (list new-dimensions)))
  (if (not (adjustable-array-p array))
      (error "~S is not an adjustable array." array))
  (if (not (= (array-rank array) (length new-dimensions)))
      (error "Number of dimensions not equal to the rank of array."))
  (if (not element-type-p)
      (setq element-type (array-element-type array))
      (if (not (subtypep element-type (array-element-type array)))
	  (error "New element type ~S is incompatible with old." element-type)))
  (if (and fill-pointer (not (array-has-fill-pointer-p array)))
      (error "Array has no fill-pointer to adjust.")
      (if (and (not fill-pointer) (array-has-fill-pointer-p array))
	  (setq fill-pointer (fill-pointer array))))
  (if (and initial-element-p (not (typep initial-element element-type)))
      (error "The specified :initial-element is incompatible with type ~S." initial-element element-type))
  (if (and initial-element-p (or initial-contents displaced-to))
      (error "The :initial-element option may not be used with the :initial-contents~%or :displaced-to option."))
  (if (and initial-contents (or initial-element-p displaced-to))
      (error "The :initial-contents option may not be used with the :initial-element~%or :displaced-to option."))
  (cond (displaced-to
	 (if (not (arrayp displaced-to))
      	     (error ":displaced-to argument must be an array."))
  	 (if (not (subtypep element-type (array-element-type displaced-to)))
      	     (error "One can't displace an array of type ~S into another of type ~S"
	     	    element-type (array-element-type displaced-to)))))
  (%adjust-array array new-dimensions element-type initial-element initial-contents
	       fill-pointer displaced-to displaced-index-offset))

(defun stringp (s)
  (and (arrayp s)
       (= (array-rank s) 1)
       (eq (array-element-type s) 'string-char)))

(defun simple-string-p (s)
  (eq (object-type s) 'string))

(defun bit-vector-p (b)
  (and (arrayp b)
       (= (array-rank b) 1)
       (eq (array-element-type b) 'bit)))

(defun simple-bit-vector-p (b)
  (eq (object-type b) 'vector-1b))

(defun vectorp (v)
  (and (arrayp v)
       (= (array-rank v) 1)))

(defun simple-vector-p (v)
  (eq (object-type v) 'vector))

(defun bit-array-same-dimensions-p (array1 array2)
  (equal (array-dimensions array1)
	 (array-dimensions array2)))

;;; WITH-ARRAY-DATA follows an arbitrarily long chain of displaced arrays
;;; binding data-var to the data vector, offset-var to the cumulative
;;; displacement offset, start-var to the actual start index in the data
;;; vector, and end-var to the actual end of the data vector.
;;;

(defmacro with-array-data (((data-var array &key (offset-var (gensym)))
			    (start-var &optional (svalue 0))
			    (end-var &optional (evalue nil)))
			   &rest forms)
  "Bind data-var to the data-vector eventually reached by following displacement
   links from array, offset-var to a cumulative offset, start-var to the first
   index in the data vector, and end-var to the total length of the array plus
   the cumulative offset."
  `(multiple-value-bind (,data-var ,offset-var)
			(find-data-vector ,array)
     (let* ((,start-var (+ ,svalue ,offset-var))
	    (,end-var (+ ,offset-var (or ,evalue (array-total-size ,array)))))
       ,@forms)))

