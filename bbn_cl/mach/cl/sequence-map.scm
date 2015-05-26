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

(export '(concatenate map some every notany notevery reduce))

;;; Concatenate:

(defun concatenate (output-type-spec &rest sequences)
  "Returns a new sequence of all the argument sequences concatenated together
   which shares no structure with the original argument sequences of the
   specified OUTPUT-TYPE-SPEC."
  (case (type-specifier output-type-spec)
    (list (concatenate-to-list sequences))
    ((simple-vector simple-string vector string array simple-array
		    bit-vector simple-bit-vector)
     (concatenate-to-vector (make-sequence-of-type
			     output-type-spec
			     (apply #'+ (mapcar #'length sequences)))
		       	    0
		       	    sequences))
    (t (error "~S: invalid output type specification." output-type-spec))))

(defun concatenate-to-list-from-list (l sequences)
  (if (null l)
      (concatenate-to-list sequences)
      (cons (car l)
	    (concatenate-to-list-from-list (cdr l) sequences))))

(defun concatenate-to-list-from-vector (v index end sequences)
  (if (= index end)
      (concatenate-to-list sequences)
      (cons (aref v index)
	    (concatenate-to-list-from-vector v (1+ index) end sequences))))

(defun concatenate-to-list (sequences)
  (if (null sequences)
      '()
      (let ((first (car sequences)))
	(if (listp first)
	    (concatenate-to-list-from-list first (cdr sequences))
	    (concatenate-to-list-from-vector first 0 (length first) (cdr sequences))))))

(defun concatenate-to-vector-from-list (l v index sequences)
  (if (null? l)
      (concatenate-to-vector v index sequences)
      (begin (setf (aref v index) (car l))
	     (concatenate-to-vector-from-list (cdr l) v (1+ index) sequences))))

(defun concatenate-to-vector-from-vector (source source-index end v index sequences)
  (if (= source-index end)
      (concatenate-to-vector v index sequences)
      (begin (setf (aref v index) (aref source source-index))
	     (concatenate-to-vector-from-vector source (1+ source-index) end v (1+ index) sequences))))

(defun concatenate-to-vector (v index sequences)
  (if (null? sequences)
      v
      (let ((first (car sequences))
	    (rest (cdr sequences)))
	(if (listp first)
	    (concatenate-to-vector-from-list first v index rest)
	    (concatenate-to-vector-from-vector first 0 (length first) v index rest)))))

;;; Map:

(defun map (output-type-spec function first-sequence &rest more-sequences)
  "FUNCTION must take as many arguments as there are sequences provided.  The 
   result is a sequence such that element i is the result of applying FUNCTION
   to element i of each of the argument sequences."
  (let ((sequences (cons first-sequence more-sequences)))
    (case (type-specifier output-type-spec)
      ((nil) (map-for-effect function sequences (min-length sequences)))
      (list (map-to-list function sequences (min-length sequences)))
      ((simple-vector simple-string vector string array simple-array
		    bit-vector simple-bit-vector)
       (map-to-vector output-type-spec function sequences (min-length sequences)))
      (t (error "~S: invalid output type specifier." output-type-spec)))))

(defun min-length (sequences)
  (apply #'min (mapcar #'length sequences)))

(defun map-for-effect (function sequences min-length)
  (do ((index 0 (1+ index)))
      ((= index min-length)
       nil)
    (apply function (elt-slice sequences index))))

(defun map-to-list (function sequences min-length)
  (labels ((map-loop (index)
	     (if (= index min-length)
		 nil
		 (let ((result (apply function (elt-slice sequences index))))
		   (cons result
			 (map-loop (1+ index)))))))
    (map-loop 0)))

(defun map-to-vector (type function sequences min-length)
  (let ((v (make-sequence-of-type type min-length)))
    (do ((index 0 (1+ index)))
	((= index min-length)
	  v)
      (setf (aref v index) (apply function (elt-slice sequences index))))))

;;; Quantifiers: Some, every, notany, and notevery.

(defun some (predicate first-sequence &rest more-sequences)
  "PREDICATE is applied to the elements with index 0 of the sequences, then 
   possibly to those with index 1, and so on.  SOME returns the first 
   non-() value encountered, or () if the end of a sequence is reached."
  (let ((seqs (cons first-sequence more-sequences)))
    (let ((min-length (min-length seqs)))
      (labels ((some-loop (index)
                 (if (= index min-length)
		     nil
		     (or (apply predicate (elt-slice seqs index))
			 (some-loop (1+ index))))))
	(some-loop 0)))))

(defun every (predicate first-sequence &rest more-sequences)
  "PREDICATE is applied to the elements with index 0 of the sequences, then
   possibly to those with index 1, and so on.  EVERY returns () as soon
   as any invocation of PREDICATE returns (), or T if every invocation
   is non-()."
  (let ((seqs (cons first-sequence more-sequences)))
    (let ((min-length (min-length seqs)))
      (labels ((every-loop (index)
                 (if (= index min-length)
		     t
		     (and (apply predicate (elt-slice seqs index))
			  (every-loop (1+ index))))))
	(every-loop 0)))))

(defun notany (predicate first-sequence &rest more-sequences)
  "PREDICATE is applied to the elements with index 0 of the sequences, then 
   possibly to those with index 1, and so on.  NOTANY returns () as soon
   as any invocation of PREDICATE returns a non-() value, or T if the end
   of a sequence is reached."
  (let ((seqs (cons first-sequence more-sequences)))
    (let ((min-length (min-length seqs)))
      (labels ((notany-loop (index)
		 (if (= index min-length)
		     T
		     (and (not (apply predicate (elt-slice seqs index)))
			  (notany-loop (1+ index))))))
	(notany-loop 0)))))

(defun notevery (predicate first-sequence &rest more-sequences)
  "PREDICATE is applied to the elements with index 0 of the sequences, then
   possibly to those with index 1, and so on.  NOTEVERY returns T as soon
   as any invocation of PREDICATE returns (), or () if every invocation
   is non-()."
  (let ((seqs (cons first-sequence more-sequences)))
    (let ((min-length (min-length seqs)))
      (labels ((notevery-loop (index)
		  (if (= index min-length)
		      nil
		      (or (not (apply predicate (elt-slice seqs index)))
			  (notevery-loop (1+ index))))))
	(notevery-loop 0)))))

;;; Reduce:

(defun reduce (function seq &key from-end (start 0) (end (length seq)) (initial-value nil ivp))
  (cond ((= end start)
	 (if ivp
	     initial-value
	     (funcall function)))
	((and (= 1 (- end start)) (not ivp))
	 (elt seq start))
	(from-end
	 (if ivp
	     (reduce-right function seq (-1+ end) start initial-value)
	     (reduce-right function seq (- end 2) start (elt seq (-1+ end)))))
	(else
	 (if ivp
	     (reduce-left function seq start end initial-value)
	     (reduce-left function seq (1+ start) end (elt seq start))))))

(defun reduce-left (function sequence index end initial-value)
  (if (= (1+ index) end)
      (funcall function initial-value (elt sequence index))
      (reduce-left function
		   sequence
		   (1+ index)
		   end
		   (funcall function initial-value (elt sequence index)))))

(defun reduce-right (function sequence index start initial-value)
  (if (= index start)
      (funcall function (elt sequence start) initial-value)
      (reduce-right function
		    sequence
		    (-1+ index)
		    start
		    (funcall function (elt sequence index) initial-value))))

