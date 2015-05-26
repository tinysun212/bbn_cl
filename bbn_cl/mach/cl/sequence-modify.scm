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

(export '(fill replace remove remove-if remove-if-not
	  delete delete-if delete-if-not remove-duplicates delete-duplicates
	  substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
	  nsubstitute-if-not))

;;; Fill:

(defun fill (sequence item &key (start 0) end)
  "Replace the specified elements of SEQUENCE with ITEM."
  (if (listp sequence)
      (list-fill sequence item start end)
      (vector-fill sequence 
		   item
		   start
		   (or end (length sequence)))))

(defun vector-fill (sequence item index end)
  (if (>= index end)
      sequence
      (begin (setf (aref sequence index) item)
	     (vector-fill sequence item (1+ index) end))))

(defun list-fill (sequence item start end)
  (do ((l (list-tail sequence start) (cdr l))
       (index start (1+ start)))
      ((or (null? l) (and end (>= index end)))
       sequence)
    (set-car! l item)))

;;; Replace:

(defun replace (seq1 seq2 &key (start1 0) (end1 (length seq1)) (start2 0) (end2 (length seq2)))
  "The target sequence is destructively modified by copying successive
   elements into it from the source sequence."
  (do ((s1 start1 (1+ s1))
       (s2 start2 (1+ s2)))
      ((or (>= s1 end1) (>= s2 end2))
       seq1)
    (setf (elt seq1 s1) (elt seq2 s2))))

;;; Remove:

(defun remove (item sequence &key from-end (test (function eql)) test-not
               	    (start 0) end count (key (function identity)))
  "Returns a copy of SEQUENCE with elements satisfying the test
   (default is EQL) with ITEM removed."
  (remove-if (if test-not (lambda (x) (funcall test-not item x)) (lambda (x) (funcall test item x)))
	     sequence :from-end from-end :start start :end end :count count :key key))

;;; Remove-if

(defun remove-if (predicate sequence &key from-end (start 0)
		    	    end count (key (function identity)))
  "Returns a copy of sequence with elements such that predicate(element)
   is non-null are removed"
  (let ((size (length sequence)))
    (if (not end) (set! end size))
    (if (not from-end)
	(sequence-copy-if predicate 
			  sequence
			  (make-sequence-like sequence size)
			  start
			  end
			  size
			  count
			  key)
	(reverse 
	 (sequence-copy-if predicate 
			   (reverse sequence)
			   (make-sequence-like sequence size)
			   (- size end)
			   (- size start)
			   size
			   count
			   key)))))

;;; Remove-if-not

(defun remove-if-not (predicate sequence &key from-end (start 0)
		    	    end count (key (function identity)))
  "Returns a copy of sequence with elements such that predicate(element)
   is null are removed"
  (remove-if (lambda (x) (not (funcall predicate x)))
	     sequence :from-end from-end :start start :end end :count count :key key))

(defun sequence-copy-if (predicate source dest start end size count key)
  (let ((final-size 
	 (labels ((copy-loop (source-index dest-index c)
                    (cond ((< source-index start)
			   (setf (elt dest dest-index) (elt source source-index))
			   (copy-loop (1+ source-index) (1+ dest-index) c))
			  ((or (>= source-index end)
			       (and c (zero? c)))
			   (do ((source-index source-index (1+ source-index))
				(dest-index dest-index (1+ dest-index)))
			       ((= source-index size)
				dest-index)
			     (setf (elt dest dest-index) (elt source source-index))))
			  ((funcall predicate (funcall key (elt source source-index)))
			   (copy-loop (1+ source-index) dest-index (if c (-1+ c) c)))
			  (else
			   (setf (elt dest dest-index) (elt source source-index))
			   (copy-loop (1+ source-index) (1+ dest-index) c)))))
	   (copy-loop 0 0 count))))
    (sequence-truncate dest final-size)))

(defun sequence-truncate (seq size)
  (if (listp seq)
      (if (zero? size)
	  '()
	  (begin (set-cdr! (list-tail seq (-1+ size)) nil)
		 seq))
      (subseq seq 0 size)))

;;; Delete:

(defun delete (item sequence &key from-end (test (function eql)) test-not
               	    (start 0) end count (key (function identity)))
  "Returns a sequence formed by destructively removing the specified Item from
  the given Sequence."
  (delete-if (if test-not (lambda (x) (funcall test-not item x)) (lambda (x) (funcall test item x)))
	     sequence :from-end from-end :start start :end end :count count :key key))

;;; Delete-if

(defun delete-if (predicate sequence &key from-end (start 0)
		    	    end count (key (function identity)))
  "Returns a sequence formed by destructively removing the elements satisfying
  the specified Predicate from the given Sequence."
  (let ((size (length sequence)))
    (if (not end) (set! end size))
    (if (not from-end)
	(sequence-copy-if predicate 
			  (copy-seq sequence)
			  sequence
			  start
			  end
			  size
			  count
			  key)
	(nreverse
	 (sequence-copy-if predicate 
			   (reverse sequence)
			   sequence
			   (- size end)
			   (- size start)
			   size
			   count
			   key)))))

;;; Delete-if-not

(defun delete-if-not (predicate sequence &key from-end (start 0)
				end count (key (function identity)))
  "Returns a sequence formed by destructively removing the elements not
  satisfying the specified Predicate from the given Sequence."
  (delete-if (lambda (x) (not (funcall predicate x)))
	     sequence :from-end from-end :start start :end end :count count :key key))

'$split-file
;;; Remove-Duplicates:

(defun remove-duplicates (sequence &key (test (function eql)) test-not (start 0) from-end
				   end (key (function identity)))
  "The elements of Sequence are examined, and if any two match, one is
   discarded.  The resulting sequence is returned."
  (remove-duplicates-if (if test-not (lambda (x y) (not (funcall test-not x y))) test)
			sequence start end from-end key))

(defun remove-duplicates-if (test sequence start end from-end key)
  (let ((size (length sequence)))
    (if (not end) (set! end size))
    (if (not from-end)
	(sequence-duplicate-copy-if test 
				    sequence
				    (make-sequence-like sequence size)
				    start
				    end
				    size
				    key)
	(reverse 
	 (sequence-duplicate-copy-if test 
				     (reverse sequence)
				     (make-sequence-like sequence size)
				     (- size end)
				     (- size start)
				     size
				     key)))))

'$split-file

(defun sequence-duplicate-copy-if (predicate source dest start end size key)
  (let ((final-size 
	 (labels ((copy-loop (source-index dest-index)
		    (cond ((< source-index start)
			   (setf (elt dest dest-index) (elt source source-index))
			   (copy-loop (1+ source-index) (1+ dest-index)))
			  ((>= source-index end)
			   (do ((source-index source-index (1+ source-index))
				(dest-index dest-index (1+ dest-index)))
			       ((= source-index size)
				dest-index)
			     (setf (elt dest dest-index) (elt source source-index))))
			  ((in-set? (funcall key (elt source source-index)) 
				    source (1+ source-index) end predicate key)
			   (copy-loop (1+ source-index) dest-index))
			  (else
			   (setf (elt dest dest-index) (elt source source-index))
			   (copy-loop (1+ source-index) (1+ dest-index))))))
	   (copy-loop 0 0))))
    (sequence-truncate dest final-size)))

(defun in-set? (element sequence start end predicate key)
  (cond ((>= start end)
	 nil)
	((funcall predicate element (funcall key (elt sequence start)))
	 t)
	(else
	 (in-set? element sequence (1+ start) end predicate key))))

;;; Delete-Duplicates:

(defun delete-duplicates (sequence &key (test (function eql)) test-not (start 0) from-end
			    (end (length sequence)) (key (function identity)))
  "The elements of Sequence are examined, and if any two match, one is
   discarded.  The resulting sequence, which may be formed by destroying the
   given sequence, is returned."
  (delete-duplicates-if (if test-not (lambda (x y) (not (funcall test-not x y))) test)
			sequence start end from-end key))

(defun delete-duplicates-if (test sequence start end from-end key)
  (let ((size (length sequence)))
    (if (not end) (set! end size))
    (if (not from-end)
	(sequence-duplicate-copy-if test 
				    sequence
				    sequence
				    start
				    end
				    size
				    key)
	(let ((r (nreverse sequence)))
	  (sequence-duplicate-copy-if test 
				      r
				      r
				      (- size end)
				      (- size start)
				      size
				      key)
	  (nreverse r)))))

;;; Substitute:

(defun substitute (new old sequence &key from-end (test (function eql)) test-not
		   (start 0) count end (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements equal to Old are replaced with New.  See manual
  for details."
  (substitute-if new (if test-not
			 (lambda (x) (not (funcall test-not old x))) 
			 (lambda (x) (funcall test old x)))
		 sequence :from-end from-end :start start :end end :count count :key key))

;;; Substitute-If:

(defun substitute-if (new test sequence &key from-end (start 0)
			  end count (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements satisfying the Test are replaced with New.  See
  manual for details."
  (if (not end) (set! end (length sequence)))
  (if from-end
      (substitute-aux sequence (copy-seq sequence) new test (-1+ end) (-1+ start) count -1 key)
      (substitute-aux sequence (copy-seq sequence) new test start end count 1 key)))

;;; Substitute-If-Not:

(defun substitute-if-not (new test sequence &key from-end (start 0)
			      end count (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements not satisfying the Test are replaced with New.  See
  manual for details."
  (substitute-if new (lambda (x) (not (funcall test x)))
		 sequence :from-end from-end :start start :end end :count count :key key))

(defun substitute-aux (source dest new test index end count increment key)
  (if (or (= index end)
	  (and count (zero? count)))
      dest
      (let ((item (elt source index)))
	(if (funcall test (funcall key (elt source index)))
	    (begin 
	      (setf (elt dest index) new)
	      (substitute-aux source dest new test (+ index increment) end (if count (-1+ count) count)
			      increment key))
	      (substitute-aux source dest new test (+ index increment) end count increment key)))))
	 
;;; NSubstitute:

(defun nsubstitute (new old sequence &key from-end (test (function eql)) test-not
			(start 0) count end (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements equal to Old are replaced with New.  The Sequence
  may be destroyed.  See manual for details."
  (nsubstitute-if new (if test-not 
			  (lambda (x) (not (funcall test-not old x)))
			  (lambda (x) (funcall test old x)))
		  sequence :from-end from-end :start start :end end :count count :key key))

;;; NSubstitute-If:

(defun nsubstitute-if (new test sequence &key from-end (start 0)
			   end (count (length sequence)) (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements satisfying the Test are replaced with New.  The
  Sequence may be destroyed.  See manual for details."
  (if (not end) (set! end (length sequence)))
  (if from-end
      (substitute-aux sequence sequence new test (-1+ end) (-1+ start) count -1 key)
      (substitute-aux sequence sequence new test start end count 1 key)))

;;; NSubstitute-If-Not:

(defun nsubstitute-if-not (new test sequence &key from-end (start 0)
			       end count (key (function identity)))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements not satisfying the Test are replaced with New.
  The Sequence may be destroyed.  See manual for details."
  (nsubstitute-if new (lambda (x) (not (test x)))
		  sequence :from-end from-end :start start :end end :count count :key key))
