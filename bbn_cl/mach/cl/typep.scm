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

(export '(typep))

;;;; Typep auxiliary functions.

;;; This is called if the type-specifier is a symbol and is not one of the
;;; built-in Lisp types.  If it's an instance, see if it's either that type
;;; or call the class object for the result.  If it's a structure, see if it's
;;; that type, or if it includes that type.

(defun structure-typep (object type)
  (let ((type (type-expand type)))
    (if (and (symbolp type) (system-get type '%structure-definition))
	(and (structurep object)
	     (let ((obj-name (g-vector-ref object 0)))
	       (or (eq obj-name type)
		   (not (null (memq type
				    (dd-includes (system-get obj-name '%structure-definition))))))))
	(error "~S is an unknown type specifier." type))))
	       
;;; Given that the object is a vector of some sort, and that we've already
;;; verified that it matches CAR of TYPE, see if the rest of the type
;;; specifier wins.  Mild hack: Eltype Nil means either type not supplied
;;; or was Nil.  Any vector can hold objects of type Nil, since there aren't
;;; any, so (vector nil) is the same as (vector *).
;;;

(defun vector-eltype (object eltype)
  (let ((data (find-data-vector object))
	(eltype (type-expand eltype)))
    (case eltype
      ((t) (simple-vector-p data))
      (string-char (simple-string-p data))
      (bit (simple-bit-vector-p data))
      ((* nil) t)
      (t
       (subtypep eltype
		 (array-element-type data))))))

;;; See if object satisfies the specifier for an array.

(defun array-typep (object type)
  (and (arrayp object)
       (vector-eltype object (cadr type))
       (if (cddr type)
	   (let ((dims (third type)))
	     (cond ((eq dims '*) t)
		   ((numberp dims)
		    (and (vectorp object)
			 (= (length (the vector object)) dims)))
		   (t
		    (dotimes (i (array-rank object) (null dims))
		      (when (null dims) (return nil))
		      (let ((dim (pop dims)))
			(unless (or (eq dim '*)
				    (= dim (array-dimension object i)))
			  (return nil)))))))
	   t)))

;;; Test sequence for specified length.

(defun test-length (object length)
  (or (null length)
      (eq length '*)
      (= length (length object))))

;;; Test whether a number falls within the specified limits.

(defun test-limits (object type)
  (let ((low (cadr type))
	(high (caddr type)))
    (and (cond ((null low) t)
	       ((eq low '*) t)
	       ((numberp low) (>= object low))
	       ((and (consp low) (numberp (car low)))
		(> object (car low)))
	       (t nil))
	 (cond ((null high) t)
	       ((eq high '*) t)
	       ((numberp high) (<= object high))
	       ((and (consp high) (numberp (car high)))
		(< object (car high)))
	       (t nil)))))

;;; Data type predicates.

;;; Translation from type keywords to specific predicates.  Assumes that
;;; the following are named structures and need no special type hackery:
;;; HASHTABLE, RANDOM-STATE.

(defconstant type-pred-alist
  `((common              . commonp)
    (null                . null)
    (cons                . pair?)
    (list                . pair?)
    (symbol              . symbolp)
    (array               . arrayp)
    (vector              . vectorp)
    (bit-vector          . bit-vector-p)
    (string              . stringp)
    (sequence            . sequencep)
    (simple-array        . simple-array-p)
    (simple-vector       . simple-vector-p)
    (simple-string       . simple-string-p)
    (simple-bit-vector   . simple-bit-vector-p)
    (function            . functionp)
    (compiled-function   . compiled-function-p)
    (character           . characterp)
    (number              . numberp)
    (float               . floatp)
    (string-char         . string-char-p)
    (integer             . integerp)
    (short-float         . short-floatp)
    (standard-char       . standard-charp)
    (fixnum              . fixnump)
    (single-float        . single-floatp)
    (bignum              . bignump)
    (double-float        . double-floatp)
    (bit                 . bitp)
    (long-float          . long-floatp)
    (structure           . structurep)
    (atom                . atom)
    (pathname            . pathnamep)
    (package             . packagep)
    (complex             . complexp)
    (ratio               . ratiop)
    (stream              . streamp)
    (readtable           . readtablep)
    (future              . future?)
    ))

;;;; TYPEP

(defun typep (object type)
  "Returns T if OBJECT is of the specified TYPE, otherwise NIL."
  (let* ((type (type-expand (touch type)))
	 (object (if (eq type 'future)
		     object
		     (touch object)))
	 temp)
    (cond ((symbolp type)
	   (cond ((eq type 't) t)
		 ((eq type 'nil) nil)
		 ((setq temp (assq type type-pred-alist))
		  (funcall (cdr temp) object))
		 (t (structure-typep object type))))
	  ((listp type) 
	   ;; This handles list-style type specifiers.
	   (case (car type)
	     (vector (and (vectorp object)
			  (vector-eltype object (cadr type))
			  (test-length object (caddr type))))
	     (simple-vector (and (simple-vector-p object)
				 (test-length object (cadr type))))
	     (string (and (stringp object)
			  (test-length object (cadr type))))
	     (simple-string (and (simple-string-p object)
				 (test-length object (cadr type))))
	     (bit-vector (and (bit-vector-p object)
			      (test-length object (cadr type))))
	     (simple-bit-vector (and (simple-bit-vector-p object)
				     (test-length object (cadr type))))
	     (array (array-typep object type))
	     (simple-array (and (simple-array-p object)
				(array-typep object type)))
	     (satisfies (funcall (cadr type) object))
	     (member (member object (cdr type)))
	     (not (not (typep object (cadr type))))
	     (or (dolist (x (cdr type) nil)
			 (if (typep object x) (return t))))
	     (and (dolist (x (cdr type) t)
			  (if (not (typep object x)) (return nil))))
	     (integer (and (integerp object) (test-limits object type)))
	     (rational (and (rationalp object) (test-limits object type)))
	     (float (and (floatp object) (test-limits object type)))
	     (short-float (and (short-floatp object)
			       (test-limits object type)))
	     (single-float (and (single-floatp object)
				(test-limits object type)))
	     (double-float (and (double-floatp object)
				(test-limits object type)))
	     (long-float (and (long-floatp object)
			      (test-limits object type)))
	     (mod (and (integerp object)
		       (>= object 0)
		       (< object (cadr type))))
	     (signed-byte
	      (and (integerp object)
		   (let ((n (cadr type)))
		     (or (not n) (eq n '*)
			 (> n (integer-length object))))))
	     (unsigned-byte
	      (and (integerp object)
		   (not (minusp object))
		   (let ((n (cadr type)))
		     (or (not n) (eq n '*)
			 (>= n (integer-length object))))))
	     (complex (and (complexp object)
			   (or (not (cdr type))
			       (typep (realpart object) (cadr type)))))
	     (t (error "~S -- Illegal type specifier to TYPEP."  type))))
	  (t (error "~S -- Illegal type specifier to TYPEP."  type)))))

;;; For use by the syntaxer

(set! typep (symbol-function 'typep))
