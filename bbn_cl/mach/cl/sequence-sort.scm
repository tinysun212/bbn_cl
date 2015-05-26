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

(export '(sort stable-sort merge))

;;; Quick sort from simple-vectors to simple-vectors (an optimized case).

(defun sv-sort (vec cmp key)
  (declare (simple-vector vec))
  (let ((n (length vec)))
    (declare (fixnum n))
    (cond ((= n 0) vec)
          ((= n 1) vec)
          (t
	   (let ((half (floor n 2)))
	     (declare (fixnum half))
	     (let ((rest (the fixnum (- n half))))
	       (declare (fixnum rest))
	       (let ((a (make-array half))
		     (b (make-array rest)))
		 (copy-sv! vec a 0 0 half)
		 (copy-sv! vec b half 0 rest)
		 (sv-merge (sv-sort a cmp key) (sv-sort b cmp key) cmp key))))))))

;;; Copies from source to dest starting at source-index and dest-index
;;; and ending at dest-end.

(defun copy-sv! (source dest source-index dest-index dest-end)
  (declare (simple-vector source dest)
	   (fixnum source-index dest-index dest-end))
  (if (>= dest-index dest-end)
      dest
      (progn
        (setf (svref dest dest-index) (svref source source-index))
        (copy-sv! source
		  dest
		  (1+ source-index)
		  (1+ dest-index)
		  dest-end))))

(defun sv-merge (a b cmp key)
  (declare (simple-vector a b))
  (let ((length-a (length a))
        (length-b (length b)))
    (declare (fixnum length-a length-b))
    (let ((new (make-array (the fixnum (+ length-a length-b)))))
      (declare (simple-vector new))
      (labels ((outer-loop (index-a index-b index-new)
		 (declare (fixnum index-a index-b index-new))
		 (cond ((>= index-a length-a)
			(do ((index-b index-b (1+ index-b))
			     (index-new index-new (1+ index-new)))
			    ((>= index-b length-b) new)
			  (declare (fixnum index-b index-new))		 
			  (setf (svref new index-new) (funcall key (svref b index-b)))))
		       ((>= index-b length-b)
			(do ((index-a index-a (1+ index-a))
			     (index-new index-new (1+ index-new)))
			    ((>= index-a length-a) new)
			  (declare (fixnum index-a index-new))		 
			  (setf (svref new index-new) (funcall key (svref a index-a)))))
		       (t
			;; The order of the comparison below guarentees a
			;; STABLE merge.
			(let ((aa (funcall key (svref a index-a)))
			      (bb (funcall key (svref b index-b))))
			  (if (funcall cmp bb aa)
			      (progn (setf (svref new index-new) bb)
				     (outer-loop index-a (1+ index-b) (1+ index-new)))
			      (progn (setf (svref new index-new) aa)
				     (outer-loop (1+ index-a) index-b (1+ index-new)))))))))
	(outer-loop 0 0 0)))))

;;;  Quick sort from lists to lists.

(defun list-sort (list cmp key)
  (declare (list list))
  (let ((n (length list)))
    (declare (fixnum n))
    (cond ((= n 0) list)
          ((= n 1) list)
          (t
	   (let ((half (floor n 2)))
	     (declare (fixnum half))
	     (let* ((b (nthcdr half list))
		    (a list))
		 (rplacd (nthcdr (the fixnum (1- half)) a) nil)
		 (list-merge (list-sort a cmp key) (list-sort b cmp key) cmp key)))))))

;;; This is destructive to save conses

(defun list-merge (a b cmp key)
  (declare (list a b))
  (labels ((outer-loop! (head a b)
	     (declare (list head a b))
	     (cond ((null a)
		    (rplacd head b))		   
		   ((null b)
		    (rplacd head a))
		   (t
		    (let ((aa (funcall key (car a)))
			  (bb (funcall key (car b))))
		      (if (funcall cmp bb aa)
			  (progn (rplacd head b)
				 (outer-loop! b a (cdr b)))
			  (progn (rplacd head a)
				 (outer-loop! a (cdr a) b))))))))
    (cond ((null a)
	   b)
	  ((null b)
	   a)
	  (t
	   (let ((aa (funcall key (car a)))
		 (bb (funcall key (car b))))
	     (if (funcall cmp bb aa)
		 (progn (outer-loop! b a (cdr b))
			b)
		 (progn (outer-loop! a (cdr a) b)
			a)))))))

;;;; The merge and sort functions

(defun merge (result-type sequence1 sequence2 predicate &key (key #'identity))
  "The sequences Sequence1 and Sequence2 are destructively merged into
a sequence of type Result-Type using the Predicate to order the elements."
  ; For the time being we do all merges through lists.
  ; This might be optimized for simple vectors.
  (coerce (list-merge (coerce sequence1 'list)
		      (coerce sequence2 'list)
		      predicate
		      key)
	  result-type))

(defun copy-to (sequence list)
  (do ((index 0 (1+ index))
       (list list (cdr list)))
      ((null list) sequence)
    (setf (aref sequence index) (car list))))

(defun sort (sequence predicate &key (key #'identity))
  "Destructively sorts sequence.  Predicate should returns non-Nil if
Arg1 is to precede Arg2."
  ; For the time being we do all sorts though lists.
  ; This might be optimized for simple vectors.
  (cond ((listp sequence)
	 (list-sort sequence predicate key))
	((vectorp sequence)
	 (copy-to sequence
		  (list-sort (coerce sequence 'list) predicate key)))
	(t
	 (error "~S is not a sequence." sequence))))

;; Stable sort is the same as sort

(cl-define stable-sort #'sort)
