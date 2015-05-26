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

;;; Functions that must be around when the macros produced by DEFMACRO are
;;; expanded.

(defun find-keyword (keyword keylist)
  "If keyword is present in the keylist, return a list of its argument.
  Else, return NIL."
  (do ((l keylist (cddr l)))
      ((atom l) nil)
    (cond ((atom (cdr l))
	   (cerror "Stick a NIL on the end and go on."
		   "Unpaired item in keyword portion of macro call.")
	   (rplacd l (list nil))
	   (return nil))
	  ((eq (car l) keyword) (return (list (cadr l)))))))


(defun keyword-test (keylist legal)
  "Check whether all keywords in a form are legal.  KEYLIST is the portion
  of the calling form containing keywords.  LEGAL is the list of legal
  keywords.  If the keyword :allow-other-keyws is present in KEYLIST,
  just return without complaining about anything."
  (cond ((memq :allow-other-keys keylist) nil)
	(t (do ((kl keylist (cddr kl)))
	       ((atom kl) nil)
	     (cond ((memq (car kl) legal))
		   (t (cerror "Ignore it."
			      "~S illegal or unknown keyword." (car kl))))))))

;;; Return a form which tests whether an illegal number of arguments 
;;; have been supplied.  Args is the name of the variable to which
;;; the arglist is bound.
;;;
(defun defmacro-arg-test (args)
  (cond ((and (zerop %min-args) %restp) nil)
	((zerop %min-args)
	 `(> (length ,args) ,(1+ %arg-count)))
	(%restp
	 `(< (length ,args) ,(1+ %min-args)))
	((= %min-args %arg-count)
	 `(not (= (length ,args) ,(1+ %min-args))))
	(t
	 `(or (> (length ,args) ,(1+ %arg-count))
	      (< (length ,args) ,(1+ %min-args))))))


(boot-defmacro defmacro (name arglist &rest body)
  (unless (symbolp name)
	  (error "~S -- Macro name not a symbol." name))
  (print-def "defmacro" name)
  (if *build-time-eval*
      (warn-if-macro-redefined name *syntax-time-global-env*)) ; takes care of checking global env, too.
  (let ((funcellsym (fundefsym name)))
    (let ((whole 'whole) 
	  (environment 'env))		; why are these vars bound like this?
      (multiple-value-bind
       (body local-decs doc)
       (parse-defmacro
	arglist whole body name
	:environment environment
	:error-string 'defmacro-error-string)
       (if *build-time-eval* 
	   (let ((expander ((access eval system-global-environment)
			    `(lambda (whole env) (block ,name ,body))
			    *commonlisp-user-environment*)))
	     (local-assignment *syntax-time-global-env* funcellsym 
			       (cons 'commonlisp-macro expander))))
       ;; The emitted expander is closed in the wrong env
       ;;  (but the one above is correct) -- need to be able 
       ;;  to compile (scheme) in-package to correct this problem
       `(defmacro-internal
	  ',name
	  ',doc
	  (lambda (whole env) (block ,name ,body)))))))

;;;
;;; Remove quasiquote as a special form and reinstall it as a defmacro.
;;;  This is so macroexpand works properly, and thus so do code walkers.
;;; This function is called at the bottom of sfcl.scm.
;;;

(cl-define (quasiquote->defmacro)
	   (remove-syntax! 'quasiquote)
	   ((access eval system-global-environment)
	    '(defmacro quasiquote (x) ((access expand-quasiquote-no-syntax syntaxer-package) x))
	    system-global-environment))


;;8.2 Macro Expansion

(defvar *macroexpand-hook*)

(setq *macroexpand-hook* #'funcall)

(defun macroexpand (form &optional (env *syntax-time-global-env*))
  "If Form is a macro call, then the form is expanded until the result is not
a macro.  Returns as multiple values, the form after any expansion has
been done and T if expansion was done, or NIL otherwise.  Env is the
lexical environment to expand in, which defaults to the null environment."
  (if (and (pair? form) 
	   (symbol? (car form)))
      (let ((expander (lookup-macro-definition
		       (car form) env)))
	(if expander
	    (values
	     (macroexpand (funcall *macroexpand-hook* expander form env) env)
	     t)
	    (values form nil)))
      (values form nil)))

(defun macroexpand-1 (form &optional (env *syntax-time-global-env*))
  (if (and (pair? form) 
	   (symbol? (car form)))
      (let ((expander (lookup-macro-definition
			     (car form) env)))
	(if expander		;a commonlisp-macro?
	    (values (funcall *macroexpand-hook* expander form env)
		    t)			;yes
	    (values form nil)))		;no
      (values form nil)))

