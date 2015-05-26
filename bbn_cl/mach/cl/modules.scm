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

(export '(*modules* provide require))

(defvar *modules* ()
"This is a list of module names that have been loaded into Lisp so far.
It is used by PROVIDE and REQUIRE.")

;;;; Provide and Require.

(defun provide (module-name)
"Adds a new module name to *modules* indicating that it has been loaded.
Module-name may be either a case-sensitive string or a symbol; if it is
a symbol, its print name is used (downcased if it is not escaped)."
  (pushnew (module-name-string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathname)
"Loads a module when it has not been already.  Pathname, if supplied,
is a single pathname or list of pathnames to be loaded if the module
needs to be.  If pathname is not supplied, then a file will be loaded whose
name is formed by merging \"\\modules\\\" and module-name (downcased if it
is a symbol)."
  (setf module-name (module-name-string module-name))
  (if (not (member module-name *modules* :test #'string=))
      (progn
	(cond ((null pathname)
	       (setf pathname (list (merge-pathnames "modules/" module-name))))
	      ((not (consp pathname))
	       (setf pathname (list pathname))))		    
	(dolist (ele pathname t)
	  (load ele)))))

;;;; Misc.

(defun module-name-string (name)
  (typecase name
    (string name)
    (symbol (symbol->filename-string name))
    (t (error "Module name must be a string or symbol -- ~S."
	      name))))
