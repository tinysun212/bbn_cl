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
;;;
;;; The CL part of the reader; mostly interfaces
;;;   into (and out from) cl-parse.scm
;;;

(proclaim '(insert-touches nil))

(defvar *readtable*)

(export '(*readtable* *read-suppress* *features* *read-base* *read-default-float-format*
	  copy-readtable readtablep set-syntax-from-char 
	  set-macro-character get-macro-character make-dispatch-macro-character
	  set-dispatch-macro-character get-dispatch-macro-character
	  read *read-default-float-format* read-preserving-whitespace
	  read-delimited-list read-from-string parse-integer
))

;;; Already defined at top-leval in cl-parse:
;;;
;;; *readtable* *read-suppress* *features* *read-base*
;;; readtablep set-syntax-from-char 
;;; set-macro-character get-macro-character make-dispatch-macro-character
;;; set-dispatch-macro-character get-dispatch-macro-character

(defvar *read-default-float-format*)
(setq *read-default-float-format* 'single-float)

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  ((access *cl-parse-object cl-parser-package) 
   stream nil eof-error-p eof-value recursive-p nil))

(defun read-preserving-whitespace (&optional (stream *standard-input*) eof-error-p eof-value recursive-p)
  ((access *cl-parse-object cl-parser-package) 
   stream nil eof-error-p eof-value recursive-p t))

(defun read-from-string (string &optional eof-error-p eof-value 
				&key (start 0) (end (length string)) preserve-whitespace)
  (let* ((stream (string-open string :input start end))
	 (result ((access *cl-parse-object cl-parser-package) 
		  stream nil eof-error-p eof-value nil preserve-whitespace))
	 (index (string-input-stream-index stream)))
    (close stream)
    (values result index)))

(defun copy-readtable (&optional from-readtable to-readtable)
  (if (null from-readtable)
      (setq from-readtable *readtable*))
  (if (null to-readtable)
      (setq to-readtable (make-readtable)))
  (check-type from-readtable readtable)
  (check-type to-readtable readtable)
  (dolist (binding (environment-bindings from-readtable))
    (let ((var (car binding))
	  (value (cadr binding)))
      (if (and (sequencep value)
	       (not (memq var '(base-parser-table macro-parser-table
				system-#-dispatch-vector))))
	  (local-assignment to-readtable var
			    (copy-seq value)))))
  to-readtable)

(defun read-delimited-list (char &optional stream recursive-p)
  (labels ((xloop ()
		  (let ((c (peek-char t stream t nil recursive-p)))
		    (if (char= c char)
			(progn
			 (read-char stream t nil recursive-p)
			 nil)
			(cons (read stream t nil recursive-p)
			      (xloop))))))
	  (xloop)))

(defun parse-integer (string &key (start 0) (end (length string)) (radix 10) (junk-allowed nil))
  (labels ((integer-char-p (c radix)
			   (or (char= c #\+)
			       (char= c #\-)
			       (digit-char-p c radix)))
	   (xloop (acc sign i)
		  (if (= i end)
		      (values (* sign acc) i)
		      (let ((c (schar string i)))
			(if (not (integer-char-p c radix))
			    (if junk-allowed
				(values (* sign acc) i)
				(error "Junk found in string ~a by parse-integer" string))
			    (progn
			     (cond
			      ((char= c #\+))
			      ((char= c #\-) (setq sign -1))
			      (t
			       (setq acc (+ (* acc radix) (char->digit c radix)))))
			     (xloop acc sign (1+ i))))))))
	  (xloop 0 1 start)))

;;;
;;; Called from cl-parse, in #S code.
;;;

(defun make-#s-structure (l)
  (let* ((struct-name (car l))
	 (slots nil)
	 (constructor 
	  (let ((dd (system-get struct-name '%structure-definition)))
	    (if (null dd)
		(error "No such structure as ~a" struct-name)
		(dd-constructor dd)))))
    (do ((x (cdr l) (cddr x)))
	((null x) nil)
      (push (cadr x) slots)
      (push (intern (symbol-name (car x)) *keyword-package*) slots))
    (if (null constructor)
	(error "No constructor function for structure named ~a" struct-name)
	(apply constructor slots))))
;;;
;;; Called from cl-parse, in #( code
;;;

(defun list-with-length->vector (xlist xlength)
  (let ((l xlist)
	(vec-length xlength))
    (if (null vec-length)
	(coerce l 'vector)
	(let ((list-len (length l)))
	  (cond
	   ((> list-len vec-length)
	    (error "supplied vector data ~a greater than given length ~a" l vec-length))
	   ((= list-len vec-length)
	    (coerce l 'vector))
	   (else
	    (let ((v (make-array vec-length :initial-element (car (last-pair l)))))
	      (do ((i 0 (1+ i)))
		  ((= i (1- list-len))
		   v)
		(setf (aref v i) (car l))
		(setq l (cdr l))))))))))

'$split-file

;;;
;;; Called from cl-parse, in #nA code
;;;

(defun make-#a-array (ndims initial-contents)
  (if (not (verify-shape initial-contents ndims))
      (error "Improper shape to array's initial-contents")
      (labels ((xloop (i dim-list n)
	         (cond
		  ((and (not (sequencep i))
			(> n 0))
		   (error "Structure of initial contents of array does not match given number of dimensions"))
		  ((or (not (sequencep i))
		       (= n 0))
		   (let ((dim-list (nreverse dim-list)))
		     (make-array dim-list :initial-contents initial-contents)))
		  (t
		   (xloop (elt i 0)
			  (cons (length i) dim-list)
			  (1- n))))))
	(xloop initial-contents '() ndims))))

(defun verify-shape (s ndims)
  (labels ((weight (s ndims)
		   (if (or (not (sequencep s))
			   (= ndims 0))
		       1
		       (let* ((w (weight (elt s 0) (1- ndims)))
			      (l (length s))
			      (acc w))
			 (do ((i 1 (1+ i)))
			     ((= i l) acc)
			   (declare (fixnum i m w))
			   (let ((m (weight (elt s i) (1- ndims))))
			     (if (not (= m w))
				 (return-from verify-shape nil)
				 (incf acc m))))))))
	  (weight s ndims)))

(defun sequencep (x)
  (or (listp x)
      (vectorp x)))

;;;
;;; Called from cl-parse, in #n= and #n# code.
;;;

(defvar sharp-cons-table (make-hash-table :test #'eq)) ; Holds the cons cells seen already by circle-subst

;;; This function is the same as nsubst, except that it checks for circular
;;; lists. the first arg is an alist of the things to be replaced assoc'd with
;;; the things to replace them.

(defun circle-subst (old-new-alist tree)
  (clrhash sharp-cons-table)
  (circle-subst1 old-new-alist tree))

(defun circle-subst1 (old-new-alist tree)
  (cond
   ((consp tree)
    (unless (gethash tree sharp-cons-table)
	    (setf (gethash tree sharp-cons-table) t)
	    (let ((a (circle-subst1 old-new-alist (car tree)))
		  (d (circle-subst1 old-new-alist (cdr tree))))
	      (if (eq a (car tree))
		  tree
		  (rplaca tree a))
	      (if (eq d (cdr tree))
		  tree
		  (rplacd tree d))))
    tree)
   ((vectorp tree)
    (unless (gethash tree sharp-cons-table)
	    (setf (gethash tree sharp-cons-table) t)
	    (dotimes (i (length tree))
		     (let ((v (circle-subst1 old-new-alist (aref tree i))))
		       (when (not (eq v (aref tree i)))
			     (setf (aref tree i) v)))))
    tree)
   ((and (g-vector? tree)
	 (= (get-g-vector-subtype tree) 1)) ; structure
    (unless (gethash tree sharp-cons-table)
	    (setf (gethash tree sharp-cons-table) t)
	    (dotimes (i (1- (g-vector-length tree)))
		     (let* ((j (1+ i))
			    (v (circle-subst1 old-new-alist (g-vector-ref tree j))))
		       (when (not (eq v (g-vector-ref tree j)))
			     (g-vector-set! tree j v)))))
    tree)
   (t 
    (let ((pair (assq tree old-new-alist)))
      (if pair (cdr pair) tree)))))
