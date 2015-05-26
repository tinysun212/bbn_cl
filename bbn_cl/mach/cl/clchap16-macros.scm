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
;;; Hashing functions for the three kinds of hash tables:
(defmacro eq-hash (object)
  "Gives us a hashing of an object such that (eq a b) implies
   (= (eq-hash a) (eq-hash b))"
;;      `(%sp-make-fixnum ,object)
	`(hash ,object))

(defmacro eql-hash (object)
  "Gives us a hashing of an object such that (eql a b) implies
   (= (eql-hash a) (eql-hash b))"
	`(if (numberp ,object) 
	     (sxhash ,object)
	     (hash ,object)))

(defmacro equal-hash (object)
  "Gives us a hashing of an object such that (equal a b) implies
   (= (equal-hash a) (equal-hash b))"
  `(sxhash ,object))

(defmacro grow-size (table)
  "Returns a fixnum for the next size of a growing hash-table."
  `(let ((rehash-size (hash-table-rehash-size ,table)))
     (if (floatp rehash-size)
	 (ceiling (* rehash-size (hash-table-size ,table)))
	 (+ rehash-size (hash-table-size ,table)))))

(defmacro grow-rehash-threshold (table new-length)
  "Returns the next rehash threshold for the table."
  table
  `,new-length
;  `(ceiling (* (hash-table-rehash-threshold ,table)
;	       (/ ,new-length (hash-table-size ,table))))
  )

(defmacro hash-set (vector key value length hashing-function)
  "Used for rehashing.  Enters the value for the key into the vector
   by hashing.  Never grows the vector.  Assumes the key is not yet
   entered."
  `(let ((index (rem (the fixnum (funcall ,hashing-function ,key))
		     (the fixnum ,length))))
     (declare (fixnum index))
     (setf (aref (the simple-vector ,vector) index)
	   (cons (cons ,key ,value)
		 (aref (the simple-vector ,vector) index)))))

;;; Macros for Gethash, %Puthash, and Remhash:


;;; Hashop dispatches on the kind of hash table we've got, rehashes if
;;; necessary, and binds Vector to the hash vector, Index to the index
;;; into that vector that the Key points to, and Size to the size of the
;;; hash vector.  Since Equal hash tables only need to be maybe rehashed
;;; sometimes, one can tell it if it's one of those times with the
;;; Equal-Needs-To-Rehash-P argument.

(defmacro hashop (equal-needs-to-rehash-p eq-body eql-body equal-body)
  `(let* ((vector (hash-table-table hash-table))
	  (size (length vector)))
     (declare (simple-vector vector))
     (declare (fixnum size))
     (case (hash-table-kind hash-table)
       (equal
	,@(if equal-needs-to-rehash-p `((equal-rehash-if-needed)))
	(let ((index (rem (the fixnum (equal-hash key)) size)))
	  (declare (fixnum index))
	  ,equal-body))
       (eq
	(eq-rehash-if-needed)
	(let ((index (rem (the fixnum (eq-hash key)) size)))
	  (declare (fixnum index))
	  ,eq-body))
       (eql
	(eq-rehash-if-needed)
	(let ((index (rem (the fixnum (eql-hash key)) size)))
	  (declare (fixnum index))
	  ,eql-body)))))

(defmacro eq-rehash-if-needed ()
  `(cond 
    ((> (hash-table-number-entries hash-table)
	(hash-table-rehash-threshold hash-table))
     (rehash hash-table vector (grow-size hash-table))
     (setq vector (hash-table-table hash-table))
     (setq size (length vector)))))

(defmacro equal-rehash-if-needed ()
  `(cond ((> (hash-table-number-entries hash-table)
	     (hash-table-rehash-threshold hash-table))
	  (rehash hash-table vector (grow-size hash-table))
	  (setq vector (hash-table-table hash-table))
	  (setq size (length vector)))))

(defmacro rehash-if-needed ()
  `(cond 
    ((> (hash-table-number-entries hash-table)
	(hash-table-rehash-threshold hash-table))
     (rehash hash-table vector (grow-size hash-table))
     (setq vector (hash-table-table hash-table))
     (setq size (length vector)))))

;;;+++Actually the case insensitive hasing (see sxhash-simple-strin in
;;;+++bool.c) isn't necessary since for equal strings are *NOT* equal if
;;;+++there is a difference in case. (AGB)
(defmacro sxhash-string (sequence)
  (let ((data (gensym))
	(start (gensym))
	(end (gensym)))
    `(with-array-data ((,data ,sequence)
		       (,start)
		       (,end (fill-pointer ,sequence)))
		      (if (zerop ,start)
			  (sxhash-simple-string ,data 0 ,end)
			  (sxhash-simple-string (coerce (the string ,sequence)
							'simple-string)
						nil nil)))))

