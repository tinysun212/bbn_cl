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

;;; Manipulating hash tables:

(defun gethash (key hash-table &optional default)
  "Finds the entry in Hash-Table whose key is Key and returns the associated
   value and T as multiple values, or returns Default and Nil if there is no
   such entry."
  (macrolet ((lookup (test)
	       `(let ((cons (assoc key (aref vector index) :test #',test)))
		  (if cons
		      (values (cdr cons) t)
		      (values default nil)))))
    (hashop nil
      (lookup eq)
      (lookup eql)
      (lookup equal))))

(defun %puthash (key hash-table value)
  "Create an entry in HASH-TABLE associating KEY with VALUE; if there already
   is an entry for KEY, replace it.  Returns VALUE."
  (macrolet ((store (test)
	       `(let ((cons (assoc key (aref vector index) :test #',test)))
		  (cond (cons (setf (cdr cons) value))
			(t
			 (push (cons key value) (aref vector index))
			 (incf (hash-table-number-entries hash-table))
			 value)))))
    (hashop t
      (store eq)
      (store eql)
      (store equal))))

(defun remhash (key hash-table)
  "Remove any entry for KEY in HASH-TABLE.  Returns T if such an entry
   existed; () otherwise."
  (hashop nil
   (let ((bucket (aref vector index)))		; EQ case
     (cond ((and bucket (eq (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (eq (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))
   (let ((bucket (aref vector index)))		; EQL case
     (cond ((and bucket (eql (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (eql (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))
   (let ((bucket (aref vector index)))		; EQUAL case
     (cond ((and bucket (equal (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (equal (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))))
