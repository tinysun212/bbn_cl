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
	   (load "package-common.bin")
	   (load "package-a.bin"))

;;; Export  --  Public
;;;
;;;
(defun export (symbols &optional (package *package*))
  "Exports Symbols from Package, checking that no name conflicts result."
  (let ((package (package-or-lose package))
	(syms ()))
    ;;
    ;; Punt any symbols that are already external.
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-external-symbol (symbol-name sym) package)
	(declare (ignore s))
	(unless (or w (member sym syms)) (push sym syms))))
    ;;
    ;; Find symbols and packages with conflicts.
    (let ((used-by (package-used-by-list package))
	  (cpackages ())
	  (cset ()))
      (dolist (sym syms)
	(let ((name (symbol-name sym)))
	  (dolist (p used-by)
	    (multiple-value-bind (s w) (find-symbol name p)
	      (when (and w (not (eq s sym))
			 (not (member s (package-shadowing-symbols p))))
		(pushnew sym cset)
		(pushnew p cpackages))))))
      (when cset
	(cerror "skip exporting these symbols or unintern all conflicting ones."
		"Exporting these symbols from the ~A package:~%~S~%~
		results in name conflicts with these packages:~%~{~A ~}"
		(package-name package) cset (mapcar #'package-name cpackages))
	(if (y-or-n-p "Unintern all conflicting symbols? ")
	    (dolist (p cpackages)
	      (dolist (sym cset)
		(moby-unintern sym p)))
	    (setq syms (nset-difference syms cset)))))
    ;;
    ;; Check that all symbols are accessible.  If not, ask to import them.
    (let ((missing ())
	  (imports ()))
      (dolist (sym syms)
	(multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	  (cond ((not (and w (eq s sym))) (push sym missing))
		((eq w :inherited) (push sym imports)))))
      (when missing
	(cerror "Import these symbols into the ~A package."
		"These symbols are not accessible in the ~A package:~%~S"
		(package-name package) missing)
	(import missing package))
      (import imports package))
    ;;
    ;; And now, three pages later, we export the suckers.
    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(nuke-symbol internal (symbol-name sym))
	(add-symbol external sym)))
    t))

;;; Unexport  --  Public
;;;
;;;    Check that all symbols are accessible, then move from external to
;;; internal.
;;;
(defun unexport (symbols &optional (package *package*))
  "Makes Symbols no longer exported from Package."
  (let ((package (package-or-lose package))
	(syms ()))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((or (not w) (not (eq s sym)))
	       (error "~S is not accessible in the ~A package."
		      sym (package-name package)))
	      ((eq w :external) (pushnew sym syms)))))

    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)
	(nuke-symbol external (symbol-name sym))))
    t))

;;; Import  --  Public
;;;
;;;    Check for name conflic caused by the import and let the user 
;;; shadowing-import if there is.
;;;
(defun import (symbols &optional (package *package*))
  "Make Symbols accessible as internal symbols in Package.  If a symbol
  is already accessible then it has no effect.  If a name conflict
  would result from the importation, then a correctable error is signalled."
  (let ((package (package-or-lose package))
	(symbols (symbol-listify symbols))
	(syms ())
	(cset ()))
    (dolist (sym symbols)
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((not w)
	       (let ((found (member sym syms :test #'string=)))
		 (if found
		     (when (not (eq (car found) sym))
		       (push sym cset))
		     (push sym syms))))
	      ((not (eq s sym)) (push sym cset))
	      ((eq w :inherited) (push sym syms)))))
    (when cset
      (cerror
       "Import these symbols with Shadowing-Import."
       "Importing these symbols into the ~A package causes a name conflict:~%~S"
       (package-name package) cset))
    ;;
    ;; Add the new symbols to the internal hashtable.
    (let ((internal (package-internal-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)))
    ;;
    ;; If any of the symbols are uninterned, make them be owned by Package.
    (dolist (sym symbols)
      (unless (symbol-package sym) (%set-symbol-package! sym package)))
    (shadowing-import cset package)))

;;; Shadowing-Import  --  Public
;;;
;;;    If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
;;;
(defun shadowing-import (symbols &optional (package *package*))
  "Import Symbols into package, disregarding any name conflict.  If
  a symbol of the same name is present, then it is uninterned.
  The symbols are added to the Package-Shadowing-Symbols."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (sym (symbol-listify symbols))
	    (multiple-value-bind 
	     (s w) 
	     (find-symbol (symbol-name sym) package)
	     (unless (and w (not (eq w :inherited)) (eq s sym))
		     (when (or (eq w :internal) (eq w :external))
			   ;;
			   ;; If it was shadowed, we don't want Unintern to flame out...
			   (setf (package-shadowing-symbols package)
				 (delete s (the list (package-shadowing-symbols package))))
			   (unintern s package))
		     (add-symbol internal sym))
	     (pushnew sym (package-shadowing-symbols package)))))
  t)

'$split-file

;;; Shadow  --  Public
;;;
;;;
(defun shadow (symbols &optional (package *package*))
  "Make an internal symbol in Package with the same name as each of the
  specified symbols, adding the new symbols to the Package-Shadowing-Symbols.
  If a symbol with the given name is already present in Package, then
  the existing symbol is placed in the shadowing symbols list if it is
  not already present."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (sym (symbol-listify symbols))
      (let ((name (symbol-name sym)))
	(multiple-value-bind (s w) (find-symbol name package)
	  (when (or (not w) (eq w :inherited))
	    (setq s (make-symbol name))
	    (set-package s package)
	    (add-symbol internal s))
	  (pushnew s (package-shadowing-symbols package))))))
  t)

;;; Use-Package  --  Public
;;;
;;;    Do stuff to use a package, with all kinds of fun name-conflict
;;; checking.
;;;
(defun use-package (packages-to-use &optional (package *package*))
  "Add all the Package-To-Use to the use list for Package so that
  the external symbols of the used packages are accessible as internal
  symbols in Package."
  (let ((packages (package-listify packages-to-use))
	(package (package-or-lose package)))
    ;;
    ;; Loop over each package, use-ing one at a time...
    (dolist (pkg packages)
      (unless (member pkg (package-use-list package))
	(let ((cset ())
	      (shadowing-symbols (package-shadowing-symbols package))
	      (use-list (package-use-list package)))
	  ;;
	  ;;   If the number of symbols already accessible is less than the
	  ;; number to be inherited then it is faster to run the test the
	  ;; other way.  This is particularly valuable in the case of
	  ;; a new package use-ing Lisp.

	  ;; I deleted the special case code because our implementation
	  ;; does not keep a symbol count - JP 6/23/87
	  ;;
	  (do-external-symbols (sym pkg)
	    (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	      (when (and w (not (eq s sym))
			 (not (member s shadowing-symbols)))
		    (push s cset))))
	  
	  (when cset
	    (cerror
	     "The symbols will be replaced by the symbols in ~A~%~
              by uninterning the conflicting symbols in ~1*~A."              
	     "Use-package of package ~A results in name conflicts for these symbols:~%~S"
	     (package-name pkg) cset (package-name package))
	    (dolist (s cset) (moby-unintern s package))))

	(push pkg (package-use-list package))
	(push (package-external-symbols pkg) (cdr (package-tables package)))
	(push package (package-used-by-list pkg)))))
  t)

;;; Unuse-Package  --  Public
;;;
;;;
(defun unuse-package (packages-to-unuse &optional (package *package*))
  "Remove Packages-To-Unuse from the use list for Package."
  (let ((package (package-or-lose package)))
    (dolist (p (package-listify packages-to-unuse))
      (setf (package-use-list package)
	    (delete p (the list (package-use-list package))))
      (setf (package-tables package)
	    (delete (package-external-symbols p)
		    (the list (package-tables package))))
      (setf (package-used-by-list p)
	    (delete package (the list (package-used-by-list p)))))
    t))

;;; Find-All-Symbols --  Public
;;;
;;;
(defun find-all-symbols (string-or-symbol)
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
	(res ()))
    (mapc #'(lambda (package)
	      (multiple-value-bind (s w) (find-symbol string package)
		(when w (pushnew s res))))
	  (list-all-packages))
    res))
