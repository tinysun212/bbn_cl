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

(export '(find find-if find-if-not position position-if
	  position-if-not count count-if count-if-not mismatch search))

;;; Utilities for find and position

(defun find-terminator (index item)
  item)

(defun position-terminator (index item)
  index)

(defun list-find/position-if (predicate l from-end start end key terminator)
  (if from-end
      (let ((size (length l)))
	(if (not end) (set! end size))
	(list-find/position-if-aux predicate (reverse l) 0 (- size end) (- size start) key 
				   (lambda (index item) (funcall terminator (- (-1+ size) index) item))))
      (list-find/position-if-aux predicate l 0 start end key terminator)))

(defun list-find/position-if-aux (predicate l index start end key terminator)
  (cond ((< index start)
	 (list-find/position-if-aux predicate (cdr l) (1+ index) start end key terminator))
	((or (null? l)
	     (and end (>= start end)))
	 nil)
	((funcall predicate (funcall key (car l)))
	 (funcall terminator index (car l)))
	(else (list-find/position-if-aux predicate (cdr l) (1+ index) start end key terminator))))

(defun vector-find/position-if (predicate v from-end start end key terminator)
  (let ((size (length v)))
    (if (null? end) (set! end size))
    (if from-end
	(vector-find/position-if-aux predicate v (-1+ end) (-1+ start) key -1 terminator)
	(vector-find/position-if-aux predicate v start end key 1 terminator))))

(defun vector-find/position-if-aux (predicate v start end key increment terminator)
  (cond ((= start end)
	 nil)
	((funcall predicate (funcall key (aref v start)))
	 (funcall terminator start (aref v start)))
	(else (vector-find/position-if-aux predicate v (+ increment start) end key increment terminator))))

;;; Find:

(defun find (item sequence &key from-end (test (function eql)) test-not (start 0)
		  end (key (function identity)))
  "Returns the first element in SEQUENCE satisfying the test (default
   is EQL) with the given ITEM"
  (find-if (if test-not 
	       (lambda (x) (not (funcall test-not x item)))
	       (lambda (x) (funcall test x item)))
	   sequence :from-end from-end :start start :end end :key key))

;;; Find-if:

(defun find-if (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the zero-origin index of the first element satisfying the test."
  (if (listp sequence)
      (list-find/position-if test sequence from-end start end key #'find-terminator)
      (vector-find/position-if test sequence from-end start end key #'find-terminator)))

;;; Find-if-not:

(defun find-if-not (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the zero-origin index of the first element not satisfying the test."
  (find-if (lambda (x) (not (funcall test x))) sequence :from-end from-end :start start :end end :key key))

;;; Position:

(defun position (item sequence &key from-end (test (function eql)) test-not (start 0)
		  end (key (function identity)))
  "Returns the zero-origin index of the first element in SEQUENCE
   satisfying the test (default is EQL) with the given ITEM"
  (position-if (if test-not 
		   (lambda (x) (not (funcall test-not x item)))
		   (lambda (x) (funcall test x item)))
	       sequence :from-end from-end :start start :end end :key key))

;;; Position-if:

(defun position-if (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the zero-origin index of the first element satisfying test(el)"
  (if (listp sequence)
      (list-find/position-if test sequence from-end start end key #'position-terminator)
      (vector-find/position-if test sequence from-end start end key #'position-terminator)))

;;; Position-if-not:

(defun position-if-not (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the zero-origin index of the first element not satisfying test(el)"
  (position-if (lambda (x) (not (funcall test x))) sequence
	       :from-end from-end :start start :end end :key key))

;;; Count:

(defun count (item sequence &key from-end (test (function eql)) test-not (start 0)
		   end (key (function identity)))
  "Returns the number of elements in SEQUENCE satisfying a test with ITEM,
   which defaults to EQL."
  (count-if (if test-not 
		(lambda (x) (not (funcall test-not x item))) 
		(lambda (x) (funcall test x item)))
	    sequence :start start :end end :key key))

;;; Count-if:

(defun count-if (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the number of elements in SEQUENCE satisfying TEST(el)."
  (if (listp sequence)
      (list-count-if test sequence 0 start end key)
      (vector-count-if test sequence start end key)))

;;; Count-if-not:

(defun count-if-not (test sequence &key from-end (start 0) end (key (function identity)))
  "Returns the number of elements in SEQUENCE not satisfying TEST(el)."
  (count-if (lambda (x) (not (funcall test x))) sequence :start start :end end :key key))

(defun list-count-if (test sequence index start end key)
  (cond ((< index start)
	 (list-count-if test (cdr sequence) (1+ index) start end key))
	((or (null? sequence)
	     (and end (>= index end)))
	 0)
	((funcall test (funcall key (car sequence)))
	 (1+ (list-count-if test (cdr sequence) (1+ index) start end key)))
	(else
	 (list-count-if test (cdr sequence) (1+ index) start end key))))

(defun vector-count-if (test sequence start end key)
  (if (null? end) (set! end (length sequence)))
  (cl-name-let count-loop ((index start))
	       (cond ((>= index end)
		      0)
		     ((funcall test (funcall key (aref sequence index)))
		      (1+ (count-loop (1+ index))))
		     (else (count-loop (1+ index))))))		     

'$split-file
;;; Mismatch utilities:

(defmacro match-vars (&rest body)
  `(let ((inc (if from-end -1 1))
	 (start1 (if from-end (1- end1) start1))
	 (start2 (if from-end (1- end2) start2))
	 (end1 (if from-end (1- start1) end1))
	 (end2 (if from-end (1- start2) end2)))
     ,@body))

(defmacro matchify-list (sequence start length end)
  `(setq ,sequence
	 (if from-end
	     (nthcdr (- ,length ,start 1) (reverse (the list ,sequence)))
	     (nthcdr ,start ,sequence))))

(defmacro if-mismatch (elt1 elt2)
  `(cond ((= index1 end1)
	  (return (if (= index2 end2) nil (if from-end (1+ index1) index1))))
	 ((= index2 end2)
	  (return (if from-end (1+ index1) index1)))
	 (test-not
	  (if (funcall test-not (funcall key ,elt1) (funcall key ,elt2))
	      (return (if from-end (1+ index1) index1))))
	 (t (if (not (funcall test (funcall key ,elt1) (funcall key ,elt2)))
		(return (if from-end (1+ index1) index1))))))

(defmacro mumble-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 inc))
	(index2 start2 (+ index2 inc)))
       (())
     (if-mismatch (aref sequence1 index1) (aref sequence2 index2))))

(defmacro mumble-list-mismatch ()
  `(do ((index1 start1 (+ index1 inc))
	(index2 start2 (+ index2 inc)))
       (())
     (if-mismatch (aref sequence1 index1) (pop sequence2))))

(defmacro list-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 inc))
	(index2 start2 (+ index2 inc)))
       (())
     (if-mismatch (pop sequence1) (aref sequence2 index2))))

(defmacro list-list-mismatch ()
  `(do ((index1 start1 (+ index1 inc))
	(index2 start2 (+ index2 inc)))
       (())
     (if-mismatch (pop sequence1) (pop sequence2))))

;;; Compare two elements and return if they don't match:

(defmacro compare-elements (elt1 elt2)
  `(if test-not
       (if (funcall test-not (funcall key ,elt1) (funcall key ,elt2))
	   (return nil)
	   t)
       (if (not (funcall test (funcall key ,elt1) (funcall key ,elt2)))
	   (return nil)
	   t)))

;;;; The stuff below this line should one day be rewritten

;;; Mismatch:

(defun mismatch (sequence1 sequence2 &key from-end (test (function eql)) test-not 
		   (start1 0) (end1 (length sequence1)) (start2 0)
		   (end2 (length sequence2)) (key (function identity)))
  "The specified subsequences of Sequence1 and Sequence2 are compared
element-wise.  If they are of equal length and match in every element, the
result is Nil.  Otherwise, the result is a non-negative integer, the index
within Sequence1 of the leftmost position at which they fail to match; or, if
one is shorter than and a matching prefix of the other, the index within
Sequence1 beyond the last position tested is returned.  If a non-Nil :From-End
keyword argument is given, then one plus the index of the rightmost position in
which the sequences differ is returned."
  (let ((length1 (length sequence1))
	(length2 (length sequence2)))
    (match-vars (if (listp sequence1)
		    (progn (matchify-list sequence1 start1 length1 end1)
			   (if (listp sequence2)
			       (progn (matchify-list sequence2 start2 length2 end2)
				      (list-list-mismatch))
			       (list-mumble-mismatch)))
		    (if (listp sequence2)
			(progn (matchify-list sequence2 start2 length2 end2)
			       (mumble-list-mismatch))
			(mumble-mumble-mismatch))))))

'$split-file
;;; Search comparison functions:

(defmacro search-compare-list-list (main sub)
  `(do ((main ,main (cdr main))
	(jndex start1 (1+ jndex))
	(sub (nthcdr start1 ,sub) (cdr sub)))
       ((or (null main) (null sub) (= end1 jndex)) t)
     (compare-elements (car main) (car sub))))

(defmacro search-compare-list-vector (main sub)
  `(do ((main ,main (cdr main))
	(index start1 (1+ index)))
       ((or (null main) (= index end1)) t)
     (compare-elements (car main) (aref ,sub index))))

(defmacro search-compare-vector-list (main sub index)
  `(do ((sub (nthcdr start1 ,sub) (cdr sub))
	(jndex start1 (1+ jndex))
	(index ,index (1+ index)))
       ((or (= end1 jndex) (null sub)) t)
     (compare-elements (aref ,main index) (car sub))))

(defmacro search-compare-vector-vector (main sub index)
  `(do ((index ,index (1+ index))
	(sub-index start1 (1+ sub-index)))
       ((= sub-index end1) t)
     (compare-elements (aref ,main index) (aref ,sub sub-index))))

(defmacro search-compare (main-type main sub index)
  (if (eq main-type 'list)
      `(if (listp ,sub)
	   (search-compare-list-list ,main ,sub)
	   (search-compare-list-vector ,main ,sub))
      `(if (listp ,sub)
	   (search-compare-vector-list ,main ,sub ,index)
	   (search-compare-vector-vector ,main ,sub ,index))))

(defmacro list-search (main sub)
  `(do ((main (nthcdr start2 ,main) (cdr main))
	(index2 start2 (1+ index2))
	(terminus (- end2 (- end1 start1)))
	(last-match ()))
       ((> index2 terminus) last-match)
     (if (search-compare list main ,sub index2)
	 (if from-end
	     (setq last-match index2)
	     (return index2)))))

(defmacro vector-search (main sub)
  `(do ((index2 start2 (1+ index2))
	(terminus (- end2 (- end1 start1)))
	(last-match ()))
       ((> index2 terminus) last-match)
     (if (search-compare vector ,main ,sub index2)
	 (if from-end
	     (setq last-match index2)
	     (return index2)))))

;;; Search

(defun search (sequence1 sequence2 &key from-end (test (function eql)) test-not 
		(start1 0) (end1 (length sequence1))
		(start2 0) (end2 (length sequence2))
		(key (function identity)))
  "A search is conducted using EQL for the first subsequence of sequence2 
   which element-wise matches sequence1.  If there is such a subsequence in 
   sequence2, the index of the its leftmost element is returned; 
   otherwise () is returned."
  (if (listp sequence2)
      (list-search sequence2 sequence1)
      (vector-search sequence2 sequence1)))
