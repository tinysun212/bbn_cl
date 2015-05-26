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

;;; This deals with the portion of the arglist following any &rest flag.

(defun analyze-rest (arglist path errloc whole)
  (when (atom arglist)
    (error "Bad &rest or &body arg in ~S." errloc))
  (prog ((rest-arg (car arglist))
	 (more (cdr arglist)))
    (cond ((symbolp rest-arg)
	   (push (list rest-arg path) %let-list))
	  ((and (consp rest-arg) (> (length rest-arg) 1))
	   (unless %env-arg-name
	     (error "Hairy &body not allowed when no environment available."))
	   (let ((decls-var (second rest-arg))
		 (doc-var (third rest-arg))
		 (n-body (gensym)) (n-decls (gensym)) (n-doc (gensym)))
	     (setq rest-arg (first rest-arg))
	     (when doc-var (push doc-var %let-list))
	     (push decls-var %let-list)
	     (push `(,rest-arg
		     (multiple-value-bind (,n-body ,n-decls ,n-doc)
					  (parse-body ,path ,%env-arg-name
						      ,(not (null doc-var)))
		       (setq ,decls-var ,n-decls)
		       ,(if doc-var `(setq ,doc-var ,n-doc) n-doc)
		       ,n-body))
		   %let-list)))
	  (t
	   (error "Bad &rest or &body arg in ~S." errloc)))
		 
    (setq %restp t)
    TRY-AGAIN
    (cond ((null more) nil)
	  ((atom more)
	   (cerror "Ignore the illegal terminator."
		   "Dotted arglist terminator after &rest arg in ~S." errloc))
	  ((eq (car more) '&key)
	   (analyze-key (cdr more) rest-arg errloc))
	  ((eq (car more) '&aux)
	   (analyze-aux (cdr more) errloc))
	  ((eq (car more) '&allow-other-keys)
	   (cerror "Ignore it."
		   "Stray &ALLOW-OTHER-KEYS in arglist of ~S." errloc))
	  ((eq (cadr arglist) '&whole)
	   (cond ((and whole (cdr more) (symbolp (cadr more)))
		  (push (list (cadr more) whole) %let-list)
		  (setq more (cddr more))
		  (go try-again))
		 (t (error "Ill-formed or illegal &whole arg in ~S."
			   errloc))))
	  ((eq (cadr arglist) '&environment)
	   (cond ((and %env-arg-name (cdr more) (symbolp (cadr more)))
		  (push `(,(cadr more) ,%env-arg-name) %let-list)
		  (setq %env-arg-used t)
		  (setq more (cddr more))
		  (go try-again))
		 (t (error "Ill-formed or illegal &environment arg in ~S."
			   errloc)))))))

;;; Analyze stuff following &aux.

(defun analyze-aux (arglist errloc)
  (do ((args arglist (cdr args)))
      ((null args))
    (cond ((atom args)
	   (cerror "Ignore the illegal terminator."
		   "Dotted arglist after &AUX in ~S." errloc)
	   (return nil))
	  ((atom (car args))
	   (push (list (car args) nil) %let-list))
	  (t (push (list (caar args) (cadar args)) %let-list)))))


;;; Handle analysis of keywords, perhaps with destructuring over the keyword
;;; variable.  Assumes the remainder of the calling form has already been
;;; bound to the variable passed in as RESTVAR.

(defun analyze-key (arglist restvar errloc)
  (let ((temp (gensym))
	(check-keywords t)
	(keywords-seen nil))
    (push temp %let-list)
    (do ((args arglist (cdr args))
	 (a) (k) (sp-var) (temp1))
	((atom args)
	 (cond ((null args) nil)
	       (t (cerror "Ignore the illegal terminator."
			  "Dotted arglist after &key in ~S." errloc))))
      (setq a (car args))
      (cond ((eq a '&allow-other-keys)
	     (setq check-keywords nil))
	    ((eq a '&aux)
	     (return (analyze-aux (cdr args) errloc)))
	    ;; Just a top-level variable.  Make matching keyword.
	    ((symbolp a)
	     (setq k (symbol->keyword a))
	     (push `(,a (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			       (car ,temp))
			      (t nil)))
		   %let-list)
	     (push k keywords-seen))
	    ;; Filter out error that might choke defmacro.
	    ((atom a)
	     (cerror "Ignore this item."
		     "~S -- non-symbol variable name in arglist of ~S."
		     a errloc))
	    ;; Deal with the common case: (var [init [svar]]) 
	    ((symbolp (car a))
	     (setq k (symbol->keyword (car a)))
	     ;; Deal with supplied-p variable, if any.
	     (cond ((and (cddr a) (symbolp (caddr a)))
		    (setq sp-var (caddr a))
		    (push (list sp-var nil) %let-list))
		   (t (setq sp-var nil)))
	     (push `(,(car a)
		     (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			    ,@(and sp-var `((setq ,sp-var t)))
			    (car ,temp))
			   (t ,(cadr a))))
		   %let-list)
	     (push k keywords-seen))
	    ;; Filter out more error cases that might kill defmacro.
	    ((or (atom (car a)) (not (keywordp (caar a))) (atom (cdar a)))
	     (cerror "Ignore this item."
		     "~S -- ill-formed keyword arg in ~S." (car a) errloc))
	    ;; Next case is ((:key var) [init [supplied-p]]).
	    ((symbolp (cadar a))
	     (setq k (caar a))
	     ;; Deal with supplied-p variable, if any.
	     (cond ((and (cddr a) (symbolp (caddr a)))
		    (setq sp-var (caddr a))
		    (push (list sp-var nil) %let-list))
		   (t (setq sp-var nil)))
	     (push `(,(cadar a)
		     (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			    ,@(and sp-var `((setq ,sp-var t)))
			    (car ,temp))
			   (t ,(cadr a))))
		   %let-list)
	     (push k keywords-seen))
	    ;; Same case, but must destructure the "variable".
	    (t (setq k (caar a))
	       (setq temp1 (gensym))
	       (cond ((and (cddr a) (symbolp (caddr a)))
		      (setq sp-var (caddr a))
		      (push (list sp-var nil) %let-list))
		     (t (setq sp-var nil)))
	       (push `(,temp1
		       (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			      ,@(and sp-var `((setq ,sp-var t)))
			      (car ,temp))
			     (t ,(cadr a))))
		     %let-list)
	       (push k keywords-seen)
	       (let ((%min-args 0) (%arg-count 0) (%restp nil))
		      (analyze1 (cadar a) temp1 errloc nil)))))
    (and check-keywords
	 (push `(keyword-test ,restvar ',keywords-seen) %keyword-tests))))
	    
