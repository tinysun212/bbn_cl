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

;;load macros, defvars, etc.
(eval-when (compile eval)
	   (load "clchap16-macros.bin")
           (load "clchap16-comm.bin"))

(defun maphash (function hash-table)
  "For each entry in HASH-TABLE, calls FUNCTION on the key and value of the
   entry; returns T."
  (let ((vector (hash-table-table hash-table)))
    (declare (simple-vector vector))
    (rehash-if-needed)
    (do ((i 0 (1+ i))
	 (size (hash-table-size hash-table)))
	((= i size))
      (declare (fixnum i size))
      (do ((bucket (aref vector i) (cdr bucket)))
	  ((null bucket))
	(funcall function (caar bucket) (cdar bucket))))))

(defun clrhash (hash-table)
  "Removes all entries of HASH-TABLE and returns the hash table itself."
  (let ((vector (hash-table-table hash-table)))
    (declare (simple-vector vector))
    (setf (hash-table-number-entries hash-table) 0)
    (do ((i 0 (1+ i))
	 (size (hash-table-size hash-table)))
	((= i size) hash-table)
      (declare (fixnum i size))
      (setf (aref vector i) nil))))

(defun hash-table-count (hash-table)
  "Returns the number of entries in the given Hash-Table."
  (hash-table-number-entries hash-table))


(defun make-dispatch-vector (default-function &rest l)
  (let ((v (make-array 256 :initial-element default-function)))
    (do ((x l (cddr x)))
	((null x) nil)
      (setf (svref v (microcode-type (car x))) (cadr x)))
    v))

(defmacro make-primitive-type-dispatch-vector (arg &rest clauses)
  (labels ((find-default-function (clauses)
	     (if (null clauses)
		 `(lambda (,arg) (error "No entry in dispatch table for type"))
		 (if (eq (caar clauses) t)
		     `(lambda (,arg) ,(cadr (car clauses)))
		     (find-default-function (cdr clauses))))))
    (let ((dispatch-vector-list '()))
      (dolist (clause clauses)
	(let ((keys (if (consp (car clause))
			(car clause)
			(list (car clause))))
	      (body (cdr clause)))
	  (let ((proc `(lambda (,arg) ,@body)))
	    (dolist (key keys)
	      (if (not (eq key t))
		  (progn
		    (push proc dispatch-vector-list)
		    (push (list 'quote key) dispatch-vector-list)))))))
      `(make-dispatch-vector ,(find-default-function clauses) ,@dispatch-vector-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Hash Function ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fm+ == "fixnum-mod-+"
;;;
;;; Add two fixnums, mod the fixnum size. This is cheating
;;;  at its best, since we just take advantage of the inline code
;;;  code emitted by declaring the vars as shown.
;;;

(let ((modulus (* 2 (1+ most-positive-fixnum))))
  (defun fm+ (x y)
    (let ((r (mod (+ x y) modulus)))
      (if (> r most-positive-fixnum)
	  (- (- modulus r))
	  r))))

(compiler:copy-open-coder! 'plus-fixnum (fundefsym 'fm+))

(defun sxhash (sexpr)
  (let ((h (sxhash1 sexpr)))
    (declare (fixnum h))
    (if (< h 0)
	(- 0 h)
	h)))

(cl-define sxhash-float
	   (make-primitive-procedure 'sxhash-float))
(cl-define sxhash-compiled
	   (make-primitive-procedure 'compiled-code-address->offset))

(defvar sxhash-dispatch-vector)
(setq sxhash-dispatch-vector
      (make-primitive-type-dispatch-vector sexpr
       (cl-array
	(if (stringp sexpr)
	    (sxhash-string sexpr)
	    (array-rank sexpr)))
       (character-string (sxhash-simple-string sexpr nil nil))
       ((interned-symbol
	 uninterned-symbol)
	(sxhash-simple-string (symbol-name sexpr) nil nil))
       ((null)
	0)
       ((pair)
	(sxhash-list sexpr))
       (fixnum 
	sexpr)
       (bignum
	(mod sexpr most-positive-fixnum))
       (flonum
	(sxhash-float sexpr))
       (ratio
	(fm+ (sxhash1 (numerator sexpr)) (sxhash1 (denominator sexpr))))
       (complex
	(fm+ (sxhash1 (realpart sexpr)) (sxhash1 (imagpart sexpr))))
       (compiled-entry
	(sxhash-compiled sexpr))
       (vector
	;;"Two pathname objects are equal if all corresponding components
	;;are equivalent" - Silver book
	;; [a nice idea, but what is "equivalent"? CLtL mentions
	;; "functionally equivalent", but this sounds rather expensive --
	;; one would probably have to do file system calls. So, the following will have to do -las]
	(if (pathnamep sexpr)
	    (sxhash1 (namestring sexpr))
	    (array-rank sexpr)))
       ;; structure -- includes hash tables
       (g-vector (g-vector-length sexpr))
       (i-vector (length sexpr))
       (environment (hash sexpr))	;Includes readtable
       (cl-package (hash sexpr))
       (cl-stream (hash sexpr))
       (t (hash sexpr))))

(defmacro primitive-type-dispatch (dispatch-vector var)
  `((svref ,dispatch-vector (primitive-type ,var)) ,var))

(defun sxhash1 (sexpr)
  (primitive-type-dispatch sxhash-dispatch-vector sexpr))

(defun sxhash-list (x)
  (declare (insert-touches nil))
  (let ((x (touch x)))
    (if (not (pair? x))
	(sxhash x)
	(fm+ 1
	     (fm+ (sxhash-list (car x))
		  (sxhash-list (cdr x)))))))
