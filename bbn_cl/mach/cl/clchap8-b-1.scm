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
;;load macros, defvars, etc.
(eval-when (compile)
	   (load "clchap8-macros.bin")
	   (load "clchap8-comm.bin"))

(proclaim '(insert-touches nil))

;;; ANALYZE1 is implemented as a finite-state machine that steps
;;; through the legal parts of an arglist in order: required, optional,
;;; rest, key, and aux.  The results are accumulated in a set of special
;;; variables: %let-list, %arg-count, %min-args, %restp, and %keyword-tests.
;;;
;;; ANALYZE1 is called by ANALYZE-ARGLIST to do the work for required and
;;; optional args.  It calls other functions if &rest, &key, or &aux are
;;; encountered.

(defun analyze1 (arglist path errloc whole)
  (do ((args arglist (cdr args))
       (optionalp nil)
       (a)
       (temp))
      ((atom args)
       (cond ((null args) nil)
	     ;; Varlist is dotted, treat as &rest arg and exit.
	     (t (push (list args path) %let-list)
		(setq %restp t))))
    (setq a (car args))
    (cond ((eq a '&whole)
	   (cond ((and whole (cdr args) (symbolp (cadr args)))
		  (push (list (cadr args) whole) %let-list)
		  (setq %restp t)
		  (setq args (cdr args)))
		 (t (error "Illegal or ill-formed &whole arg in ~S." errloc))))
	  ((eq a '&environment)
	   (cond ((and %env-arg-name (cdr args) (symbolp (cadr args)))
		  (push `(,(cadr args) ,%env-arg-name) %let-list)
		  (setq %env-arg-used t)
		  (setq args (cdr args)))
		 (t (error "Illegal or ill-formed &environment arg in ~S."
			   errloc))))
	  ((eq a '&optional)
	   (and optionalp
		(cerror "Ignore it."
			"Redundant &optional flag in varlist of ~S." errloc))
	   (setq optionalp t))
	  ((or (eq a '&rest) (eq a '&body))
	   (return (analyze-rest (cdr args) path errloc whole)))
	  ((eq a '&key)
	   ;; Create a rest-arg, then do keyword analysis.
	   (setq temp (gensym))
	   (setq %restp t)
	   (push (list temp path) %let-list)
	   (return (analyze-key (cdr args) temp errloc)))
	  ((eq a '&allow-other-keys)
	   (cerror "Ignore it."
		   "Stray &ALLOW-OTHER-KEYS in arglist of ~S." errloc))
	  ((eq a '&aux)
	   (return (analyze-aux (cdr args) errloc)))
	  ((not optionalp)
	   (setq %min-args (1+ %min-args))
	   (setq %arg-count (1+ %arg-count))
	   (cond ((symbolp a)
		  (push `(,a (car ,path)) %let-list))
		 ((atom a)
		  (cerror "Ignore this item."
			  "Non-symbol variable name in ~S." errloc))
		 (t (let ((%min-args 0) (%arg-count 0) (%restp nil))
		      (analyze1 a `(car ,path) errloc nil))))
	   (setq path `(cdr ,path)))
	  ;; It's an optional arg.
	  (t (setq %arg-count (1+ %arg-count))
	     (cond ((symbolp a)
		    ;; Just a symbol.  Bind to car of path or default.
		    (push `(,a (cond (,path (car ,path))
				     (t ,*default-default*)))
			  %let-list))
		   ((atom a)
		    (cerror "Ignore this item."
			    "Non-symbol variable name in ~S." errloc))
		   ((symbolp (car a))
		    ;; Car of list is a symbol.  Bind to car of path or
		    ;; to default value.
		    (push `(,(car a)
			    (cond (,path (car ,path))
				  (t ,(cond ((> (length a) 1) (cadr a))
					    (t *default-default*)))))
			  %let-list)
		    ;; Handle supplied-p variable, if any.
		    (and (> (length a) 2)
			 (push `(,(caddr a) (not (null ,path))) %let-list)))
		   ;; Then destructure arg against contents of this gensym.
		   (t (setq temp (gensym))
		      (push `(,temp
			      (cond (,path (car ,path))
				    (t ,(cond ((cddr a) (cadr a))
					      (t *default-default*)))))
			    %let-list)
		      (let ((%min-args 0) (%arg-count 0) (%restp nil))
			(analyze1 (car a) temp errloc nil))
		      ;; Handle supplied-p variable if any.
		      (and (> (length a) 2)
			   (push `(,(caddr a) (not (null ,path))) %let-list))))
	     (setq path `(cdr ,path))))))

