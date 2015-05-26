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

(export '(function symbol-value symbol-function boundp fboundp special-form-p 
		   setq psetq set makunbound fmakunbound apply funcall
		   progn prog1 prog2 let let* compiler-let progv flet labels macrolet
		   when unless case block return-from return loop do do* dolist dotimes))
		   
;runtime

;;7.1 constants and variables

;;quote works the same as in scheme

(define *user-lambda-symbol* #f) ; assigned by setup-packages

(def-special-form (function fn)
  (cond ((symbolp fn)
	 (fundefsym fn))
	((pair? fn)
	 (if (or (eq? (car fn) 'lambda)
		 (eq? (car fn) *user-lambda-symbol*)) ;got to be a lambda form
	     `(cl-lambda ,@(cdr fn))
	     (error "Improper arg to 'function': ~A" fn)))
	(else
	 (error "Improper arg to 'function': ~A" fn))))

;;get global value of a variable
(defun symbol-value (symbol)
  (lexical-reference *commonlisp-user-environment*
		     (un-nil-or-t-ify symbol)))

(defun boundp (sym)
  (set! sym (touch sym))
  (or (eq? sym #f)
      (eq? sym #t)
      (not (lexical-unreferenceable? *commonlisp-user-environment*
				     sym))))

(cl-define fboundp
  (lambda (symbol)
    (not (lexical-unreferenceable?
	  system-global-environment
	  (fundefsym symbol)))))

(defun special-form-p (symbol)
  (memq symbol '(block case catch compiler-let declare eval-when flet function
		       go if labels let let* macrolet multiple-value-call
		       multiple-value-prog1 progn progv quote
		       return-from setq tagbody the throw unwind-protect
		       ;; Some incompatibles:
		       cond or and)))

;;assignment
(cl-define (make-n-sets pairs)
  (if (null? (cddr pairs))
      `((sequence (set! ,(first pairs) ,(second pairs))
		  ,(first pairs)))
      `((set! ,(first pairs) ,(second pairs))
	,@(make-n-sets (cddr pairs)))))

(def-special-form (setq . args)
  (if (null? args)
      nil
      `(sequence
	 ,@(make-n-sets args))))

(boot-defmacro psetq (&rest pairs)
  (if (null? pairs)
      nil
      (if (odd? (length pairs))
	  (error "Odd number of args to psetq: ~a" pairs)
	  (let* ((letvars (make-letvars pairs))
		 (set!args (make-set!args pairs letvars)))
		`(let ,letvars
		     (setq-set! ,@set!args)	
		   nil)))))

;;support function for psetq
(cl-define (make-letvars pairs)
  (if (null? pairs)
      nil
      `((,(generate-uninterned-symbol 'psetq-) ,(cadr pairs)) ,@(make-letvars
							 (cddr pairs)))))

;;support function for psetq
(cl-define (make-set!args pairs letvars)
  (if (null? pairs)
      nil
      `(,(car pairs) ,(caar letvars) ,@(make-set!args
					(cddr pairs)
					(cdr letvars)))))

(defun set (symbol value)
  (if (or (eq? symbol #f)
	  (eq? symbol #t)
	  (keyword? symbol)
	  (system-get symbol '%constant))
      (error "Cannot SET constant ~A" symbol)
      (begin
	(lexical-assignment *commonlisp-user-environment* symbol value)
	value)))

(defun makunbound (symbol)
  (lexical-assignment *commonlisp-user-environment*
		      symbol
		      (make-unassigned-object))
  symbol)

(cl-define fmakunbound 
  (lambda (symbol)
    (lexical-assignment system-global-environment
			(fundefsym symbol)
			(make-unassigned-object))
    symbol))

;;generalized variables

;;see files clchap7c, 7d, 7e for generalized variables goodies

;;7.3 function invocation

;;apply -- scheme's apply touches all args, so we should too.
;;         we can be selective about it, though.

(cl-define scheme-apply (access apply system-global-environment))

(cl-define (apply fn . args)
  (set! fn (touch fn))
  (let ((args (cond 
	       ((null? args) '())
	       ((null? (cdr args)) (car args))
	       (else (scheme-apply cons* args)))))
    (cond ((symbolp fn)			;somebody just quote a symbol (ugh)?
	   (scheme-apply (symbol-function fn) args))
	  ((pair? fn)
	   (if (eq? (car fn) 'lambda)	;got to be a lambda form
	       (scheme-apply (eval `(cl-lambda ,@(cdr fn))) args) 
	       (error "Improper arg to 'apply': ~A" fn)))
	  (else
	   (scheme-apply fn args)))))

(cl-define (funcall fn . arguments)
    (apply fn arguments))

;;7.4 simple sequencing

(def-special-form (progn . forms)
  (if (null? forms)
      'nil
      (if (eq? (length forms) 1)
	  (car forms)
	  `(begin ,@forms))))

(boot-defmacro prog1 (first &rest forms)
  (let ((temp (generate-uninterned-symbol 'prog1-)))
    `(let ((,temp ,first))
       ,@forms
       ,temp)))

(boot-defmacro prog2 (first second &rest forms)
  (let ((temp (generate-uninterned-symbol 'prog2-)))
    `(let ((,temp nil))
       ,first
       (setq ,temp ,second)
       ,@forms
       ,temp)))

;;7.5 Establishing new variable bindings

;;;
;;; Save scheme's versions just to be true and blue.
;;;

(add-syntax! 'scm-let (lookup-syntax 'let))
(add-syntax! 'scm-let* (lookup-syntax 'let*))

;;; Why we need to use cl-lambda:
;;;  1. Temps are generated for specials so that we get parallel binding.
;;;  2. Specials are processed by the central mechanism in parse-param-list.
;;; lr-combination is used to force left-to-right order, even if (lr-evaluation-mode?)
;;;  is off, as we specify for binding forms.

(def-special-form (let vars . body)
  `(lr-combination 
    ((cl-let-lambda ,(mapcar (lambda (x)
			       (if (pair? x) (car x) x))
			     vars)
		    ,@(if (null? body) '(()) body))
     ,@(mapcar (lambda (x)
		 (if (pair? x) (cadr x) nil))
	       vars))))

(cl-define parse-declarations-by-bvl 
  (access parse-declarations-by-bvl syntaxer-package))
(cl-define get-lambda-body
  (access get-lambda-body syntaxer-package))
(cl-define get-lambda-decls
  (access get-lambda-decls syntaxer-package))

(def-special-form (let* bindings . body)
  (let ((vars (mapcar (lambda (x)
			(if (pair? x) (car x) x))
		      bindings))
	(values (mapcar (lambda (x)
			  (if (pair? x) (cadr x) nil))
			bindings)))
    (prim-with-values
     (lambda () (parse-body-internal body *syntax-time-env* nil))
     (lambda (body-forms decl-forms ignore)
       (let ((ordered-decls (parse-declarations-by-bvl
			     `(quote (:declarations ,vars ,@decl-forms)))))
	 (car (make-let* vars values ordered-decls body-forms)))))))

(defun make-let* (vars values decls body)
  (if (null? vars)
      body
      (if (car decls)
	  `((let ((,(car vars) ,(car values)))
	      (quote ,(car decls))
	      ,@(make-let* (cdr vars) (cdr values) (cdr decls) body)))
	  `((let ((,(car vars) ,(car values)))
	      ,@(make-let* (cdr vars) (cdr values) (cdr decls) body))))))

(def-special-form (progv symbols values . body)
  `(progv-aux ,symbols ,values (lambda () ,@body)))

;;; We use eval because the fluid-let
;;;   needs to be set up at runtime, and we don't wish to fool
;;;   with the procedures that actually make up fluid-let.
;;;   Performance should be ok since the thunk will be compiled,
;;;   so only the binding will be interpreted.

(cl-define (progv-aux symbols values thunk)
  (cl-define (pair x y)
    (if (null? x) 
	nil
	(if (null? y) 
	    (mapcar list x)
	    (cons `((access ,(car x) *commonlisp-user-environment*) ',(car y))
		  (pair (cdr x) (cdr y))))))
  (if (null? symbols)
      (funcall thunk)
      (eval `(values-list
	      (fluid-let ,(pair symbols values)
	       (multiple-value-list
		(funcall ,thunk)))))))

(def-special-form (flet bindings . body)
  `(let (,@(mapcar (lambda (pair)
		     `(,(fundefsym
			 (car pair))
		       ,`(cl-lambda ,@(cdr pair))))
		   bindings))
     ,@body))

(def-special-form (labels bindings . body)
  `(scm-let (,@(mapcar (lambda (pair) (list (fundefsym (car pair))))
		       bindings))
    ,@(mapcar (lambda (pair)
		`(set! ,(fundefsym (car pair))
		       (cl-named-lambda ,(cons (car pair) (cadr pair))
					,@(cddr pair))))
	      bindings)
	    ,@body))

(cl-define with-values (make-primitive-procedure 'with-values))

'$split-file

;;7.6 Conditionals

;;; First arg is a predicate.  If it is non-null, the rest of the forms are
;;; evaluated as a PROGN.

(boot-defmacro when (test &rest forms)
  `(cond (,test nil ,@forms)))

;;; First arg is a predicate.  If it is null, the rest of the forms are
;;; evaluated as a PROGN.

(boot-defmacro unless (test &rest forms)
  `(cond ((not ,test) nil ,@forms)))

;;;cond is the same as scheme

(def-special-form (case expr . clauses)
  (let ((need-temp? (not (or (symbol? expr) (scode-constant? (syntax expr))))))
    (let ((the-expression (if need-temp?
			      (generate-uninterned-symbol 'case-) expr)))
      (cl-define (transform-clause original single multi)
	(cl-define (process-selector exp)
	  `(,single ,the-expression ',exp))
	(let ((selector (car original)))
	  (cons
	   ;; This is done like this only for speed.
	   (cond ((null? (cdr selector))
		  (process-selector (car selector)))
		 ((< (length selector) 4)
		  `(or ,@(mapcar process-selector selector)))
		 (else `(,multi ,the-expression ',selector)))
	   (cdr original))))
      (cl-define (check-selector selector)
	(or (null? selector)
	    (and (not (number? (car selector)))
		 (not (string? (car selector)))
		 (check-selector (cdr selector)))))
      (cl-define (process-clause clause)
	(cond ((not (pair? clause))
	       (error "Case: Bad clause: ~a" clause))
	      ((not (pair? (car clause)))
	       (if (memq (car clause) '(t otherwise))
		   `(else ,@(cdr clause))
		   (if (not (null? (car clause)))
		       (process-clause `((,(car clause)) ,@(cdr clause)))
		       (error "Case: Bad clause: ~a" clause))))
	      ((check-selector (car clause))
	       (transform-clause clause 'eq? 'memq))
	      (else (transform-clause clause 'eqv? 'memv))))
      (if need-temp?
	  `(let ((,the-expression ,expr))
	     (cond ,@(mapcar (function process-clause) clauses)))
	  `(cond ,@(mapcar (function process-clause) clauses))))))

;; 7.7 Blocks and Exits

;;;; BLOCK and RETURN-FROM are now built in to the syntaxer -las

(boot-defmacro return (&rest result)
  `(return-from nil ,@result))   ; whole result is passed to return-from to pickup #-args check.


;;7.8 Iteration

(boot-defmacro loop (&rest body)
  (let ((loopname (generate-uninterned-symbol 'loop-)))
    `(block nil
	    (cl-iterate ,loopname () ,@body (,loopname)))))

;;; We use the syntax LR-COMBINATION to force left-to-right
;;;  ordering in calling the loopname. This is also done in passing the
;;;  initial values, in make-iterator (cl-syntax.scm).

(boot-defmacro do (vars end &rest body)
  (let ((loopname (generate-uninterned-symbol 'loop)))
    (prim-with-values
     (lambda () (parse-body-internal body *syntax-time-env* nil))
     (lambda (body decls docs)
       (let ((ddecls decls))		;**** this is a compiler bug work around -ajc *****
	 `(block nil 
		 (cl-iterate
		     ,loopname ,(mapcar (lambda (var)
					  (if (pair? var)
					      `(,(car var) ,(cadr var))
					      `(,var nil)))
					vars)
		   ,@ddecls
		   (if ,(car end) 
		       (progn ,@(cdr end))
		       (progn
			 (tagbody ,@body)
			 (lr-combination 
			  (,loopname ,@(mapcar (lambda (var)
						 (if (pair? var)
						     (if (not (null? (cddr var)))
							 (caddr var)
							 (car var))
						     var))
					       vars))))))))))))

(boot-defmacro do* (vars end &rest body)
  (let ((loopname (generate-uninterned-symbol 'loop)))
    (prim-with-values
     (lambda () (parse-body-internal body *syntax-time-env* #f))
     (lambda (body decls docs)
       (let ((ddecls decls))		;**** this is a compiler bug work around -ajc *****
	 `(block nil 
		 (let* ,(mapcar (lambda (var)
				  (if (pair? var)
				      `(,(car var) ,(cadr var))
				      `(,var nil)))
				vars)
		   ,@ddecls
		   (cl-iterate ,loopname ()
		     (if ,(car end) 
			 (progn ,@(cdr end))
			 (progn
			   (tagbody ,@body)
			   (setq ,@(mapcan
				    (lambda (var)
				      (if (and (pair? var)
					       (not (null? (cddr var))))
					  `(,(car var) ,(caddr var))
					  '()))
				    vars))
			   (,loopname)))))))))))

;;;
;;; Dolist and Dotimes could be (and were for awhile) based
;;;  directly on Do. But, this generates suboptimal code,
;;;  so instead we have code special to these macros
;;;  that maps into efficient Scheme binding and iteration constructs.
;;;

(boot-defmacro dolist (arg &rest body)
  (let ((loopname (generate-uninterned-symbol 'dolist-loop))
	(var (generate-uninterned-symbol 'dolist-var))
	(x (car arg))
	(l (cadr arg))
	(r (caddr arg)))
    (prim-with-values
     (lambda () (parse-body-internal body *syntax-time-env* #f))
     (lambda (body decls docs)
       `(cl-iterate ,loopname ((,var ,l))
	  (block nil
		 (if (not (pair? ,var))
		     ,r
		     (let ((,x (car ,var)))
		       ,@decls
		       (tagbody
			,@body)
		       (,loopname (cdr ,var))))))))))

(boot-defmacro dotimes (arg &rest body)
  (let ((loopname (generate-uninterned-symbol 'dotimes-loop))
	(i (car arg))
	(c (cadr arg))
	(r (caddr arg)))
    (prim-with-values
     (lambda () (parse-body-internal body *syntax-time-env* #f))
     (lambda (body decls docs)
       (let ((ddecls decls))		;**** this is a compiler bug work around -ajc *****
	 (if (not (pair? c))
	     `(cl-iterate ,loopname ((,i 0))
		,@ddecls
		(block nil
		       (if (>= ,i ,c)
			   ,r
			   (begin
			     (tagbody
			      ,@body)
			     (,loopname (1+ ,i))))))

	     (let ((v1 (generate-uninterned-symbol 'dotimes-)))
	       `(let ((,v1 ,c))
		  (cl-iterate ,loopname ((,i 0))
		    ,@ddecls
		    (block nil
			   (if (>= ,i ,v1)
			       ,r
			       (begin
				 (tagbody
				  ,@body)
				 (,loopname (1+ ,i))))))))))))))
