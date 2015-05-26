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
	   (load "package-common.bin"))

(proclaim '(insert-touches nil))

;;; Enter-New-Nicknames  --  Internal
;;;
;;;    Enter any new Nicknames for Package into (get-package-names).
;;; If there is a conflict then give the user a chance to do
;;; something about it.
;;;
(defun enter-new-nicknames (package nicknames)
  (check-type nicknames list)
  (dolist (n nicknames)
    (let* ((n (touch (string n)))
	   (found (pkg-gethash n (get-package-names))))
      (cond ((not found)
	     (setf (pkg-gethash n (get-package-names)) package)
	     (push n (package-nicknames package)))
	    ((eq found package))
	    ((string= (package-name found) n)
	     (cerror "Ignore this nickname."
		     "~S is a package name, so it cannot be a nickname for ~S."
		     n (package-name package)))
	    (t
	     (cerror "Redefine this nickname."
		     "~S is already a nickname for ~S."
		     n (package-name found))
	     (setf (pkg-gethash n (get-package-names)) package)
	     (push n (package-nicknames package)))))))


;;; Make-Package  --  Public
;;;
;;;    Check for package name conflicts in name and nicknames, then
;;; make the package.  Do a use-package for each thing in the use list
;;; so that checking for conflicting exports among used packages is done.
;;;
(defun make-package (name &key (use '("LISP" "SYSTEM")) (nicknames ())
			  (internal-symbols 10) (external-symbols 10))
  "Makes a new package having the specified Name and Nicknames.  The
  package will inherit all external symbols from each package in
  the use list.  :Internal-Symbols and :External-Symbols are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package."
  (when (find-package name)
    (error "A package named ~S already exists" name))
  (let* ((name (touch (string name)))
	 (package (internal-make-package
		   :name name
		   :internal-symbols (make-package-hashtable internal-symbols)
		   :external-symbols (make-package-hashtable external-symbols))))
    (use-package use package)
    (enter-new-nicknames package nicknames)
    (setf (pkg-gethash name (get-package-names)) package)))

;;; In-Package  --  Public
;;;
;;;    Like Make-Package, only different.
;;;
(defun in-package (name &rest keys &key nicknames use)
  "Sets *package* to package with given name, creating the package if
  it does not exist.  If the package already exists then it is modified
  to agree with the :Use and :Nicknames arguments.  Any new nicknames
  are added without removing any old ones not specified.  If any package
  in the :Use list is not currently used, then it is added to the use
  list."
  (let ((package (find-package name)))
    (cond
     (package
      (use-package use package)
      (enter-new-nicknames package nicknames)
      (setq *package* package))
     (t
      (setq *package* (apply #'make-package name keys))))))

;;; Rename-Package  --  Public
;;;
;;;    Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
(defun rename-package (package name &optional (nicknames ()))
  "Changes the name and nicknames for a package."
  (check-type package package)
  (let* ((package (package-or-lose package))
	 (name (string name))
	 (found (find-package name)))
    (unless (or (not found) (eq found package))
      (error "A package named ~S already exists." name))
    (pkg-remhash (package-name package) (get-package-names))
    (setf (package-name package) name)
    (setf (pkg-gethash name (get-package-names)) package)
    (dolist (n (package-nicknames package))
      (pkg-remhash n (get-package-names)))
    (setf (package-nicknames package) ())
    (enter-new-nicknames package nicknames)
    package))

;;; List-All-Packages  --  Public
;;;
;;;

;;Each bucket in the hash table of package names looks like this:
;; ((name . package) (name . package) (name . package))
;;

(defun list-all-packages ()
  "Returns a list of all existing packages."
  (remove-duplicates
   (do ((i 0 (1+ i))
	(hash-table (get-package-names))
       	(package-list '()))
       ((= i (vector-length hash-table))
       	package-list)
     (declare (fixnum i))
     (dolist (j (vector-ref hash-table i))
       (if (not (null? j))
	   (push (cdr j) package-list))))))

