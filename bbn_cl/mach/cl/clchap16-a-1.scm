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
(eval-when (compile)
	   (load "clchap16-macros.bin")
	   (load "clchap16-comm.bin"))


(export '(hash-table hash-table-p make-hash-table
	  gethash remhash maphash clrhash
	  hash-table-count sxhash))

(cl-define sxhash-simple-string (make-primitive-procedure 'sx-hash-simple-string))

;;; A hash-table-table is a vector of association lists.  When an
;;; entry is made in a hash table, a pair of (key . value) is consed onto
;;; the element in the vector arrived at by hashing.

;;; How to print one:

(defvar *print-base*)
(defvar *print-radix*)

(defun %print-hash-table (structure stream depth)
  depth
  (write-string "#<" stream)
  (write-string (symbol-name (hash-table-kind structure)) stream)
  (write-string " Hash Table, " stream)
  (let ((n (hash-table-number-entries structure)))
    (write n :stream stream)
    (write-string (if (= n 1) " entry, " " entries, ") stream))
  (let ((*print-base* 16)
	(*print-radix* t))
    (write (primitive-datum structure) :stream stream))
  (write-string ">"))


;;; Rehashing functions:

(defun almost-primify (num)
  (declare (fixnum num))
  "Almost-Primify returns an almost prime number greater than or equal
   to NUM."
  (if (= (rem num 2) 0)
      (setq num (+ 1 num)))
  (if (= (rem num 3) 0)
      (setq num (+ 2 num)))
  (if (= (rem num 7) 0)
      (setq num (+ 4 num)))
  num)



(defun rehash (structure hash-vector new-length)
  (declare (simple-vector hash-vector))
  (declare (fixnum new-length))
  "Rehashes a hash table and replaces the TABLE entry in the structure if
   someone hasn't done so already.  New vector is of NEW-LENGTH."
  (do ((new-vector (make-array new-length))
       (i 0 (1+ i))
       (size (hash-table-size structure))
       (hashing-function (case (hash-table-kind structure)
			   (eq #'(lambda (x) (eq-hash x)))
			   (eql #'(lambda (x) (eql-hash x)))
			   (equal #'(lambda (x) (equal-hash x))))))
      ((= i size)
       (cond ((eq hash-vector (hash-table-table structure))
	      (cond ((> new-length size)
		     (setf (hash-table-table structure) new-vector)
		     (setf (hash-table-rehash-threshold structure)
			   (grow-rehash-threshold structure new-length))
		     (setf (hash-table-size structure) new-length))
		    (t
		     (setf (hash-table-table structure) new-vector)))
;;don't think this is needed for bfly lisp (dca), since scheme "hash"
;;is gc-independent
;;	      (if (not (eq (hash-table-kind structure) 'equal))
;;		  (%sp-set-vector-subtype new-vector
;;					  (+ 2 (%sp-get-newspace-bit))))
	)))
    (declare (fixnum i size))
    (do ((bucket (aref hash-vector i) (cdr bucket)))
	((null bucket))
      (hash-set new-vector (caar bucket) (cdar bucket) new-length
		hashing-function))))


;;; Making hash tables:

(defun make-hash-table (&key (test 'eql) (size 65) (rehash-size 101)
			     rehash-threshold)
  "Creates and returns a hash table.  See manual for details."
  (declare (fixnum size))
  (cond ((eq test #'eq) (setq test 'eq))
	((eq test #'eql) (setq test 'eql))
	((eq test #'equal) (setq test 'equal)))
  (if (not (memq test '(eq eql equal)))
      (error "~S is an illegal :Test for hash tables." test))
  (setq size (if (<= size 37) 37 (almost-primify size)))
  (cond ((null rehash-threshold)
	 (setq rehash-threshold size))
	((floatp rehash-threshold)
	 (setq rehash-threshold (ceiling (* rehash-threshold size)))))
  (make-hash-table-structure :size size
			     :rehash-size rehash-size
			     :rehash-threshold rehash-threshold
			     :table
			      (make-array size)
			     :kind test))
