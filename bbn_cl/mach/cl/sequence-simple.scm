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

(export '(elt subseq copy-seq length reverse nreverse make-sequence))

;;; General utility functions

(defun elt-slice (sequences n)
  (mapcar (lambda (seq) (elt seq n)) sequences))

(defun type-specifier (type)
  (if (atom type) type (car type)))

;;; Subseq:

(defun subseq (sequence start &optional end)
  "Returns a copy of a subsequence of SEQUENCE starting with element number 
   START and continuing to the end of SEQUENCE or the optional END."
  (if (listp sequence)
      (list-subseq sequence start end)
      (vector-subseq sequence start (or end (length sequence)))))

(defun vector-subseq (sequence start end)
  (declare (fixnum start end))
  (do ((old start (1+ old))
       (new 0 (1+ new))
       (copy (make-sequence-like sequence (- end start))))
      ((= old end) copy)
    (declare (fixnum old new))
    (setf (aref copy new) (aref sequence old))))

(defun list-subseq (sequence start end)
  (declare (fixnum start end))
  (labels ((subseq-loop (l i)
             (declare (fixnum i))
	     (if (or (null? l) (and end (>= i end)))
		 '()
		 (cons (car l)
		       (subseq-loop (cdr l) (1+ i))))))
    (subseq-loop (list-tail sequence start) start)))

;;; Copy-seq:

(defun copy-seq (sequence)
  "Returns a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ."
  (if (listp sequence)
      (list-copy sequence)
      (vector-copy-seq sequence (length sequence))))

;;; List-copy inherited from Scheme

(defun vector-copy-seq (sequence length)
  (declare (fixnum length))
  (do ((copy (make-sequence-of-type (type-of sequence) length))
       (index 0 (1+ index)))
      ((= index length) copy)
    (declare (fixnum index))
    (setf (aref copy index) (aref sequence index))))

;;; Length

(cl-define simple-list-length length)

(defun length (x)
  (cond ((null x) 0)
	((consp x) (simple-list-length x))
	((vectorp x) (%cl-vector-length x))
      	(t (error "Argument ~a to LENGTH is not a sequence~%" x))))

;;; Reverse:

;; List-Reverse inherited from Scheme.
(cl-define list-reverse reverse)

(defun reverse (sequence)
  "Returns a new sequence containing the same elements but in reverse order."
  (if (listp sequence)
      (list-reverse sequence)
      (vector-reverse sequence)))

(defun vector-reverse (sequence)
  (let ((length (length sequence)))
    (vector-reverse-aux sequence
			(make-sequence-like sequence length)
			length)))

(defun vector-reverse-aux (source dest length)
  (declare (fixnum length))
  (do ((souce-index 0 (1+ source-index))
       (dest-index (-1+ length) (-1+ dest-index)))
      ((= source-index length) dest)
    (declare (fixnum source-index dest-index))
    (setf (aref dest dest-index)
	  (aref source source-index))))
		
;;; Nreverse:

(defun nreverse (sequence)
  "Returns a sequence of the same elements in reverse order; the argument
   is destroyed."
  (if (listp sequence)
      (reverse! sequence)
      (vector-nreverse sequence)))

;; Reverse! inherited from Scheme

(defun vector-nreverse (sequence)
  (vector-reverse-aux (copy-seq sequence) sequence (length sequence)))

;;; Make-sequence

(defun make-sequence-of-type (type length)
  "Returns a sequence of the given TYPE and LENGTH."
  (declare (fixnum length))
  (case (type-specifier type)
    ((cons list) (make-list length))
    ((bit-vector simple-bit-vector) (make-array length :element-type '(mod 2)))
    ((string simple-string) (make-string length))
    (simple-vector (make-array length))
    ((array simple-array vector)
     (if (listp type)
	 (make-array length :element-type (cadr type))
	 (make-array length)))
    ((bit-vector simple-bit-vector)
     (make-array length :element-type '(mod 2)))
    (t (error "~S is a bad type specifier for sequence functions." type))))

(defun make-sequence (type length &key initial-element)
  "Returns a sequence of the given Type and Length, with elements initialized
  to :Initial-Element."
  (declare (fixnum length))
  (case (type-specifier type)
    (list (make-list length :initial-element initial-element))
    ((simple-string string)
     (if initial-element
	 (do ((index 0 (1+ index))
	      (string (make-string length)))
	     ((= index length) string)
	   (setf (char (the simple-string string) index) initial-element))
	 (make-string length)))
    (simple-vector
     (make-array length :initial-element initial-element))
    ((array vector simple-array)
     (if (listp type)
	 (make-array length :element-type (cadr type)
		     :initial-element initial-element)
	 (make-array length :initial-element initial-element)))
    ((bit-vector simple-bit-vector)
     (make-array length :element-type '(mod 2)
		 :initial-element (or initial-element 1)))
    (t (error "~S is a bad type specifier for sequences." type))))

(defun make-sequence-like (sequence length)
  (make-sequence-of-type (type-of sequence) length))
