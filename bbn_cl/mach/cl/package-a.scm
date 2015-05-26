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

(export '(package packagep *package* make-package in-package find-package
	  package-name package-nicknames rename-package
	  package-use-list package-used-by-list package-shadowing-symbols
	  list-all-packages intern find-symbol symbol-package unintern export
	  unexport import shadowing-import shadow use-package
	  unuse-package find-all-symbols do-symbols
	  do-external-symbols do-all-symbols apropos apropos-list
	  ))

(eval-when (compile)
  (load "package-common.scm"))

(eval-when (compile load)
  (if (lookup-syntax 'make-package)
      (add-syntax! 'scm-make-package (lookup-syntax 'make-package)))
  (shadow-syntax! 'make-package)
  (if (lookup-syntax 'in-package)
      (add-syntax! 'scm-in-package (lookup-syntax 'in-package)))
  (shadow-syntax! 'in-package)
  (shadow-syntax! 'packagep)
  )

(defun make-package-hashtable (size)
  (make-obarray))
(defun internal-make-package (&key name internal-symbols external-symbols)
  (let ((pkg (make-cl-package name)))
    (setf (package-internal-symbols pkg) internal-symbols)
    (setf (package-external-symbols pkg) external-symbols)
    (setf (package-tables pkg) (list '()))
    pkg))

(defun packagep (thing) (cl-package? thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *package* () "The current package.")

(defvar *bbnaci-package* nil)
(defvar *lisp-package* nil)
(defvar *keyword-package* nil)

;;; Find-Package  --  Public
;;;
;;;
(defun find-package (name)
  "Find the package having the specified name."
  (pkg-gethash (string name) (get-package-names)))

;;; Package-Listify  --  Internal
;;;
;;;    Return a list of packages given a package-or-string-or-symbol or
;;; list thereof, or die trying.
;;;
(defun package-listify (thing)
  (let ((res ()))
    (dolist (thing (if (listp thing) thing (list thing)) res)
      (push (package-or-lose thing) res))))

;;; Package-Or-Lose  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package.
;;;
(defun package-or-lose (thing)
  (if (packagep thing)
      (touch thing)
      (let ((thing (string thing)))
	(cond ((pkg-gethash thing (get-package-names)))
	      (t
	       (cerror "Make this package."
		       "~S is not the name of a package." thing)
	       (make-package thing))))))  

;;;; Iteration macros.

(defmacro do-symbols ((var &optional (package '*package*) result-form)
		      &body (code decls))
  "Do-Symbols (Var [Package [Result-Form]]) {Declaration}* {Tag | Statement}*
  Executes the Forms at least once for each symbol accessible in the given
  Package with Var bound to the current symbol."
  (let ((n-package (gensym "N-PACKAGE"))
	(shadowed (gensym "SHADOWED"))
	(inherits (gensym "INHERITS")))
    `(prog ()
       ,@decls
       (symbol-table-iterator (lambda (,var) ,@code)
			      (package-internal-symbols ,package))
       (symbol-table-iterator (lambda (,var) ,@code)
			      (package-external-symbols ,package))
       (let* ((,n-package (package-or-lose ,package))
	      (,shadowed (package-shadowing-symbols ,n-package)))
	 (do ((,inherits (cdr (package-tables ,n-package)) (cdr ,inherits)))
	     ((null ,inherits)
	      (let ((,var nil))
		,result-form))
	   (symbol-table-iterator
	    (lambda (,var) 
	      (if (or (not ,shadowed)
		      (eq (find-symbol (symbol-name ,var) ,n-package) ,var))
		  ,@code))
	    (car ,inherits)))))))

(defmacro do-external-symbols ((var &optional (package '*package*) result-form)
			       &body (code decls))
  "Do-External-Symbols (Var [Package [Result-Form]])
                       {Declaration}* {Tag | Statement}*
  Executes the Forms once for each external symbol in the given Package with
  Var bound to the current symbol."
  (let ((n-package (gensym)))
    `(prog ((,n-package (package-or-lose ,package)))
       ,@decls
       (symbol-table-iterator
	(lambda (,var) ,@code)
	(package-external-symbols ,n-package))
       (let ((,var nil))
	 ,result-form))))

(defmacro do-all-symbols ((var &optional result-form)
			  &body (code decls))
  "Do-All-Symbols (Var [Package [Result-Form]])
  		  {Declaration}* {Tag | Statement}*
  Executes the Forms once for each symbol in each package with Var bound
  to the current symbol."
  (let ((package-list (gensym)))
    `(do ((,package-list (list-all-packages) (cdr ,package-list)))
	 ((null ,package-list)
	  (let ((,var nil))
		,result-form))
       ,@decls
       (symbol-table-iterator
	(lambda (,var) ,@code)
	(package-internal-symbols (car ,package-list)))
       (symbol-table-iterator
	(lambda (,var) ,@code)
	(package-external-symbols (car ,package-list))))))
