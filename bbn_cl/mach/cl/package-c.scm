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

;;; Intern
;;;
;;;    Simple-stringify the name and call pkg-intern-string.
;;;
(defun intern (name &optional package)
  "Returns a symbol having the specified name, creating it if necessary."
  (setq name (touch name))
  (check-type name string)
  (let ((name (coerce name 'simple-string)))
    (declare (simple-string name))
    (pkg-intern-string name (length name) (if package (package-or-lose package) *package*))))

;;; Find-Symbol  --  Public
;;;
;;;    Ditto.
;;;
(defun find-symbol (name &optional package)
  "Returns the symbol named String in Package.  If such a symbol is found
  then the second value is :intern, :external or :inherited to indicate
  how the symbol is accessible.  If no symbol is found then both values
  are NIL."
  (setq name (touch name))
  (check-type name string)
  (let ((name (coerce name 'simple-string)))
    (declare (simple-string name))
    (pkg-find-symbol name (length name) (if package (package-or-lose package) *package*))))

;;; Find-External-Symbol  --  Internal
;;;
;;;    Similar to Find-Symbol, but only looks for an external symbol.
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
;;;
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let ((symbol (get-symbol (package-external-symbols package) string (string-length string))))
    (values symbol (and symbol t))))

;;; For unparser

(set! (access *find-symbol* cl-unparser-package)
      #'find-symbol)
(set! (access *find-external-symbol* cl-unparser-package)
      #'find-external-symbol)

;;; Unintern  --  Public
;;;
;;;    If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
;;;
(defun unintern (symbol &optional (package *package*))
  "Makes Symbol no longer present in Package.  If Symbol was present
  then T is returned, otherwise NIL.  If Package is Symbol's home
  package, then it is made uninterned."
  (let* ((package (package-or-lose package))
	 (name (symbol-name symbol))
	 (shadowing-symbols (package-shadowing-symbols package)))
    (declare (list shadowing-symbols) (simple-string name))
    ;;
    ;; If a name conflict is revealed, give user a chance to shadowing-import
    ;; one of the accessible symbols.
    (when (member symbol shadowing-symbols)
      (let ((cset ()))
	(dolist (p (package-use-list package))
	  (multiple-value-bind (s w) (find-external-symbol name p)
	    (when w (pushnew s cset))))
	(when (cdr cset)
	  (loop
	   (cerror
	    "prompt for a symbol to shadowing-import."
	    "Uninterning symbol ~S causes name conflict among these symbols:~%~S"
	    symbol cset)
	   (write-string "Symbol to shadowing-import: " *query-io*)
	   (let ((sym (read *query-io*)))
	     (cond
	      ((not (symbolp sym))
	       (format *query-io* "~S is not a symbol."))
	      ((not (member sym cset))
	       (format *query-io* "~S is not one of the conflicting symbols."))
	      (t
	       (shadowing-import sym package)
	       (return-from unintern t)))))))
      (setf (package-shadowing-symbols package)
	    (delete symbol shadowing-symbols)))

    (multiple-value-bind (s w) (find-symbol name package)
      (declare (ignore s))
      (cond ((or (eq w :internal) (eq w :external))
	     (nuke-symbol (if (eq w :internal)
			      (package-internal-symbols package)
			      (package-external-symbols package))
			  name)
	     (if (eq (symbol-package symbol) package)
		 (%set-symbol-package! symbol nil))
	     t)
	    (t nil)))))

;;; Symbol-Listify  --  Internal
;;;
;;;    Take a symbol-or-list-of-symbols and return a list, checking types.
;;;
(defun symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s) (error "~S is not a symbol." s)))
	 thing)
	((symbolp thing) (list thing))
	(t
	 (error "~S is neither a symbol nor a list of symbols." thing))))

;;; Moby-Unintern  --  Internal
;;;
;;;    Like Unintern, but if symbol is inherited chases down the
;;; package it is inherited from and uninterns it there.  Used
;;; for name-conflict resolution.  Shadowing symbols are not
;;; uninterned since they do not cause conflicts.
;;;
(defun moby-unintern (symbol package)
  (unless (member symbol (package-shadowing-symbols package))
    (or (unintern symbol package)
	(let ((name (symbol-name symbol)))
	  (multiple-value-bind (s w) (find-symbol name package)
	    (declare (ignore s))
	    (when (eq w :inherited)
	      (dolist (q (package-use-list package))
		(multiple-value-bind (u x) (find-external-symbol name q)
		  (declare (ignore u))
		  (when x
		    (unintern symbol q)
		    (return t))))))))))
