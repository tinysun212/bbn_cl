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
;; Chapter 7 -- Control Structure

(proclaim '(insert-touches nil))

(export '(case ccase ecase typecase ctypecase etypecase)) 

;runtime

;;;; CASE, TYPECASE, & Friends.

(eval-when (compile load eval)

;;; Case-Body  --  Internal
;;;
;;;    This function is used to implement all of the case-like macros.
;;;
(defun case-body (name keyform cases multi-p test error-string proceed-string)
  (let ((kv (gensym)) (AGAIN (gensym)) (BLOCK (gensym))
	(clauses ())
	(keys ()))
    (dolist (case cases)
      (cond ((atom case)
	     (error "~S -- Bad clause in ~S." case name))
	    ((memq (car case) '(t otherwise))
	     (if error-string
		 (error "No default clause allowed in ~S: ~S" name case)
		 (push `(t nil ,@(rest case)) clauses)))
	    ((and multi-p (listp (first case)))
	     (setq keys (append (first case) keys))
	     (push `((or ,@(mapcar (function (lambda (key) `(,test ,kv ',key)))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (push (first case) keys)
	     (push `((,test ,kv ',(first case)) nil ,@(rest case)) clauses))))
    (if proceed-string
	`(let ((,kv ,keyform))
	   (block ,BLOCK
	     (tagbody
	      ,AGAIN
	      (return-from
	       ,BLOCK
	       (cond ,@(nreverse clauses)
		     (t
		      (cerror proceed-string error-string ,kv ',keys)
		      (write-string "Expression for new key value: " *query-io*)
		      (setq ,kv (setf ,keyform (eval (read *query-io*))))
		      (go ,AGAIN)))))))
	`(let ((,kv ,keyform))
	   (cond ,@(nreverse clauses)
		 ,@(if error-string
		       `((t (error ,error-string ,kv ',keys)))))))))

); Eval-When (Compile Load Eval)

(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil))

(defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql
	     "CCASE key ~S is not any of the following:~% ~S"
	     "prompt for a new key value to use in its place."))

(defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql
	     "ECASE key ~S is not any of the following:~% ~S"
	     nil))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep
	     "CTYPECASE key ~S is not of any of the following types:~% ~S."
	     "prompt for a new object to use in its place."))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep
	     "ETYPECASE key ~S is not of any of the following types:~% ~S."
	     nil))
