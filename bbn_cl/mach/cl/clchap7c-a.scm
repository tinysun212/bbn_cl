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

;; load macros, defvars, etc.
(eval-when (compile)
	   (load "clchap7c-macros.bin")
	   (load "clchap7c-comm.bin"))

(proclaim '(insert-touches nil))

(export '(push get-setf-method get-setf-method-multiple-value defsetf setf
	       psetf shiftf rotatef define-modify-macro pushnew pop incf decf
	       remf))



;;7.2 Generalized Variables

;;;; SETF and friends.

;;; Note: The expansions for SETF and friends sometimes create needless
;;; LET-bindings of argument values.  The compiler will remove most of
;;; these spurious bindings, so SETF doesn't worry too much about creating
;;; them. 

;;; The inverse for a generalized-variable reference function is stored in
;;; one of two ways:
;;;
;;; A SETF-INVERSE property corresponds to the short form of DEFSETF.  It is
;;; the name of a function takes the same args as the reference form, plus a
;;; new-value arg at the end.
;;;
;;; A SETF-METHOD-EXPANDER property is created by the long form of DEFSETF or
;;; by DEFINE-SETF-METHOD.  It is a function that is called on the reference
;;; form and that produces five values: a list of temporary variables, a list
;;; of value forms, a list of the single store-value form, a storing function,
;;; and an accessing function.


(defun defsetf-huge-aux-fcn (setting-form-generator)
  (function
   (lambda (access-form environment)
     (declare (ignore environment))
     (do* ((args (cdr access-form) (cdr args))
	   (dummies nil (cons (gensym) dummies))
	   (newval-var (gensym))
	   (new-access-form nil))
	  ((atom args)
	   (setq new-access-form 
	       	 (cons (car access-form) dummies))
	   (values
	    dummies
	    (cdr access-form)
	    (list newval-var)
	    (funcall setting-form-generator new-access-form newval-var)
	    new-access-form))))))

(defmacro defsetf (access-fn &rest rest &environment env)
  "Associates a SETF update function or macro with the specified access
  function or macro.  The format is complex.  See the manual for
  details."
  (print-def "defsetf" access-fn)
  (cond ((not (listp (car rest)))
	 `(eval-when (load compile eval)
		     (system-remprop ',access-fn 'setf-method-expander); SKH 4/17/84
		     (%system-put ',access-fn 'setf-inverse ',(car rest))
		     ,@(if (and (car rest) (string? (cadr rest)))
			   `((eval-when (load eval)
					(%system-put ',access-fn '%setf-documentation ,(cadr rest)))))
		     ',access-fn))
	((and (listp (car rest)) (cdr rest) (listp (cadr rest)))
	 (if (not (= (length (cadr rest)) 1))
	     (cerror "Ignore the extra items in the list."
		     "Only one new-value variable allowed in DEFSETF."))
	 (multiple-value-bind (setting-form-generator doc)
			      (defsetter
				access-fn rest env)
			      `(eval-when
				(load compile eval)
				(system-remprop ',access-fn 'setf-inverse);SKH 4/17/84
				(%system-put ',access-fn 'setf-method-expander
				      (defsetf-huge-aux-fcn ,setting-form-generator))
				,@(if doc
				      `((eval-when (load eval)
						   (%system-put ',access-fn '%setf-documentation ',doc)))
				      `((eval-when (load eval);SKH 4/17/84
						   (system-remprop ',access-fn '%setf-documentation))))
				',access-fn)))
	(t (error "Ill-formed DEFSETF for ~S." access-fn))))

(defmacro setf (&rest args &environment env)
  "Takes pairs of arguments like SETQ.  The first is a place and the second
  is the value that is supposed to go into that place.  Returns the last
  value.  The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((temp (length args)))
    (cond ((= temp 2)
	   (cond ((atom (car args))
		  `(setq ,(car args) ,(cadr args)))
		 ((setq temp (system-get (caar args) 'setf-inverse))
		  `(,temp ,@(cdar args) ,(cadr args)))
		 (t (multiple-value-bind (dummies vals newval setter getter)
					 (get-setf-method (car args) env)
		      (declare (ignore getter))
		      (do* ((d dummies (cdr d))
			    (v vals (cdr v))
			    (let-list nil))
			   ((null d)
			    (setq let-list
				  (nreverse (cons (list (car newval)
							(cadr args))
						  let-list)))
			    `(let* ,let-list ,setter))
			(setq let-list
			      (cons (list (car d) (car v)) let-list)))))))
	  ((oddp temp) 
	   (error "Odd number of args to SETF."))
	  (t (do ((a args (cddr a)) (l nil))
		 ((null a) `(progn ,@(nreverse l)))
	       (setq l (cons (list 'setf (car a) (cadr a)) l)))))))

(defmacro psetf (&rest args &environment env)
  "This is to SETF as PSETQ is to SETQ.  Args are alternating place
  expressions and values to go into those places.  All of the subforms and
  values are determined, left to right, and only then are the locations
  updated.  Returns NIL."
  (do ((a args (cddr a))
       (let-list nil)
       (setf-list nil))
      ((atom a)
       `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
    (if (atom (cdr a))
	(error "Odd number of args to PSETF."))
    (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method (car a) env)
      (declare (ignore getter))
      (do* ((d dummies (cdr d))
	    (v vals (cdr v)))
	   ((null d))
	(push (list (car d) (car v)) let-list))
      (push (list (car newval) (cadr a)) let-list)
      (push setter setf-list))))

(defmacro shiftf (&rest args &environment env)
  "One or more SETF-style place expressions, followed by a single
  value expression.  Evaluates all of the expressions in turn, then
  assigns the value of each expression to the place on its left,
  returning the value of the leftmost."
  (if (< (length args) 2)
      (error "Too few argument forms to a SHIFTF."))
  (let ((leftmost (gensym)))
    (do ((a args (cdr a))
	 (let-list nil)
	 (setf-list nil)
	 (next-var leftmost))
	((atom (cdr a))
	 (push (list next-var (car a)) let-list)
	 `(let* ,(nreverse let-list) ,@(nreverse setf-list) ,leftmost))
      (multiple-value-bind (dummies vals newval setter getter)
	(get-setf-method (car a) env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v)))
	     ((null d))
	  (push (list (car d) (car v)) let-list))
	(push (list next-var getter) let-list)
	(push setter setf-list)
	(setq next-var (car newval))))))

(defmacro rotatef (&rest args &environment env)
  "Takes any number of SETF-style place expressions.  Evaluates all of the
  expressions in turn, then assigns to each place the value of the form to
  its right.  The rightmost form gets the value of the leftmost.  Returns NIL."
  (cond ((null args) nil)
	((null (cdr args)) `(progn ,(car args) nil))
	(t (do ((a args (cdr a))
		(let-list nil)
		(setf-list nil)
		(next-var nil)
		(fix-me nil))
	       ((atom a)
		  (rplaca fix-me next-var)
		  `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
	       (multiple-value-bind (dummies vals newval setter getter)
                 (get-setf-method (car a) env)
		 (do ((d dummies (cdr d))
		      (v vals (cdr v)))
		     ((null d))
		   (push (list (car d) (car v)) let-list))
		 (push (list next-var getter) let-list)
		 ;; We don't know the newval variable for the last form yet,
		 ;; so fake it for the first getter and fix it at the end.
		 (unless fix-me (setq fix-me (car let-list)))
		 (push setter setf-list)
		 (setq next-var (car newval)))))))

