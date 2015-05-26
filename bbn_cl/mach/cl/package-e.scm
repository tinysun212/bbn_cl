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
(proclaim '(insert-touches nil))

(eval-when (compile)
	   (load "package-common.bin"))

;;; Apropos and Apropos-List.

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (write-string ", value: ")
    (prin1 (symbol-value symbol)))
  (when (fboundp symbol)
	(write-string ", function value: ")
	(prin1 (symbol-function symbol))))

(defun apropos-search (symbol string)
  (declare (simple-string string))
  (let* ((name (symbol-name symbol))
	 (first-char (schar string 0))
	 (string-length (length string))
	 (symbol-length (length name))
	 (terminus (the fixnum (- symbol-length string-length))))
    (declare (simple-string name)
	     (string-char first-char)
	     (fixnum string-length symbol-length terminus))
    (labels ((string-search (index)
	        (declare (fixnum index))
		(cond ((> index terminus)
		       nil)
		      ((char-equal first-char (schar name index))
		       (match-search 1 (the fixnum (1+ index))))
		      (else (string-search (the fixnum (1+ index))))))
	     
	     (match-search (string-index name-index)
		(declare (fixnum string-index name-index))
		(cond ((>= string-index string-length)
		       t)
		      ((char-equal (schar string string-index) (schar name name-index))
		       (match-search (the fixnum (1+ string-index))
				     (the fixnum (1+ name-index))))
		      (else (string-search name-index)))))
	    (string-search 0))))

(defun apropos (string &optional package external-only)
  "Briefly describe all symbols which contain the specified String.
  If Package is supplied then only describe symbols present in
  that package.  If External-Only is true then only describe
  external symbols in the specified package."
  (setq package (touch package))
  (setq external-only (touch external-only))
  (let ((string (string-upcase string)))
    (declare (simple-string string))
    (if (null package)
	(do-all-symbols (symbol)
	   (if (apropos-search symbol string)
	       (briefly-describe-symbol symbol)))
	(let ((package (package-or-lose package)))
	  (if external-only
	      (do-external-symbols (symbol package)
		(if (apropos-search symbol string)
		    (briefly-describe-symbol symbol)))
	      (do-symbols (symbol package)
		(if (apropos-search symbol string)
		    (briefly-describe-symbol symbol))))))
    (values)))

(defun apropos-list (string &optional package external-only)
  "Identical to Apropos, except that it returns a list of the symbols
  found instead of describing them."
  (setq package (touch package))
  (setq external-only (touch external-only))
  (let ((string (string-upcase string))
	(list '()))
    (declare (simple-string string))
    (if (null package)
	(do-all-symbols (symbol)
	   (if (apropos-search symbol string)
	       (push symbol list)))
	(let ((package (package-or-lose package)))
	  (if external-only
	      (do-external-symbols (symbol package)
		(if (apropos-search symbol string)
		    (push symbol list)))
	      (do-symbols (symbol package)
		(if (apropos-search symbol string)
		    (push symbol list))))))
    list))
