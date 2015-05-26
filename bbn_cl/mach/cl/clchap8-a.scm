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
;; Chapter 8 -- Macros

;;load macros, defvars, etc.
(eval-when (compile)
	   (load "clchap8-macros.bin")
	   (load "clchap8-comm.bin"))

(proclaim '(insert-touches nil))

(export '(macro-function parse-body defmacro macroexpand macroexpand-1 *macroexpand-hook*))


;;8.1 -- Macro Definition

(cl-define macro-function
  (lambda (symbol)
    (if (symbolp symbol)
	(if  (commonlisp-macro? (fundefsym symbol) *syntax-time-env*)
	    (lookup-macro-definition symbol *syntax-time-env*)
	    nil)
	(error "Macro-function: ~a not a symbol" symbol))))

;;defmacro goes here -- temporarily defined in clchap5.scm



;;; Parse-Body  --  Public
;;;
;;;    Macros that may be declarations are expanded and looked at, but are 
;;; returned unexpanded.  We don't try to expand things which are special
;;; forms since Steve says he needs it, and it saves time anyway.  This guy
;;; uses macroexpand rather than %macroexpand since it must do the right
;;; thing in the compiler.
;;;
(defun parse-body (body environment &optional (doc-string-allowed t))
  "This function is to parse the declarations and doc-string out of the body
  of a defun-like form.  Body is the list of stuff which is to be parsed.
  Environment is the lexical environment to expand macros in.  If
  Doc-String-Allowed is true, then a doc string will be parsed out of the body
  and returned.  If it is false then a string will terminate the search for
  declarations.  Three values are returned: the tail of Body after the
  declarations and doc strings, a list of declare forms, and the doc-string,
  or NIL if none."
  (parse-body-internal body environment doc-string-allowed))

;;;
;;; Boot version of this in clchap5
;;;

(defun parse-body-internal (body environment doc-string-allowed)
  (let* ((decls ())
	 (doc nil))
	  (do ((tail body (cdr tail)))
	      ((endp tail)
	       (values (normalize-body tail) (nreverse decls) doc))
	    (let ((form (car tail)))
	      (cond ((and (string? form) (cdr tail))
		     (if doc-string-allowed
			 (setq doc form)
			 (return (values (normalize-body tail) (nreverse decls) doc))))
		    ((not (and (consp form) (symbolp (car form))))
		     (return (values (normalize-body tail) (nreverse decls) doc)))
		    ((eq (car form) 'declare)
		     (push form decls))
		    ((special-form-p (car form))
		     (return (values (normalize-body tail) (nreverse decls) doc)))
		    (t
		     (multiple-value-bind (res win)
		       (macroexpand form environment)
		       (if (and win (consp res) (eq (car res) 'declare))
			   (push res decls)
			   (return (values (normalize-body tail) (nreverse decls) doc))))))))))


(defun normalize-body (body)
  (if (null? body)
      '(())
      body))

;;; Parse-Defmacro  --  Semi-Public
;;;
;;;    Provides a clean interface to ANALYZE1
;;;
(defun parse-defmacro (arglist whole code errloc &key (path `(cdr ,whole))
				((:environment %env-arg-name)) error-string
				(doc-string-allowed t)
				((:default-default *default-default*) nil)
				((:key-finder *key-finder*) 'find-keyword))
  "For use by macros and macro-like forms that must parse some form
  according to a defmacro-like argument list, ARGLIST.  The first value
  returned is a LET* form which binds things and then evalutes the
  specified CODE.  WHOLE is the variable which is bound to the entire
  arglist, or NIL if &whole is illegal.  ERRLOC is the name of the function
  being worked on, for use in error messages.  The second value is a list
  of ignore declarations for the WHOLE and ENVIRONMENT vars, if appropriate.

  PATH is an access expression for getting to the object to be parsed,
  which defaults to the CDR of WHOLE.
  
  ENVIRONMENT is the place where the macroexpansion environment
  may be found.  If not supplied, then no &environment arg is allowed.

  ERROR-STRING is used as the argument to error if an incorrect number of
  arguments are supplied.  The additional error arguments are ERRLOC and
  the number of arguments supplied.  If not supplied, then no argument count
  error checking is done.

  DOC-STRING-ALLOWED indicates whether a doc-string should be parsed out of
  the body.  If one is found, it is returned as the third value.
  
  DEFAULT-DEFAULT is the default value for unsupplied arguments, which defaults
  to NIL.

  KEY-FINDER the function used to do keyword lookup.  It defaults to a function
  that does the right thing.

  The fourth and fifth values are the minimum and maximum number of arguments
  allowed, in case you care about that kind of thing.  The fifth value is NIL
  if there is no upper limit."
  (multiple-value-bind (body local-decs doc)
		       (parse-body code nil doc-string-allowed)

    (let ((%arg-count 0) (%min-args 0)
	  (%restp nil) (%let-list nil)
	  (%keyword-tests nil)
	  (%env-arg-used nil))
      (analyze1 arglist path errloc whole)
    
      (let ((arg-test (if error-string (defmacro-arg-test whole)))
	    (body
	     `(let* ,(nreverse %let-list)
		,@local-decs
		(progn
		 ,@%keyword-tests
		 ,@body))))
	(values
	 (if arg-test
	     `(if ,arg-test
		  (error ,error-string ',errloc (length ,path))
		  ,body)
	     body)
	 ;; Wrong if no error check and arglist composed entirely of &environment
	 ;; args, but anyone who does that deserves to lose...
	 `(,@(unless (or arg-test arglist) `((declare (ignore ,whole))))
	   ,@(when (and %env-arg-name (not %env-arg-used))
	       `((declare (ignore ,%env-arg-name)))))
	 doc
	 %min-args
	 (if %restp nil %arg-count))))))
