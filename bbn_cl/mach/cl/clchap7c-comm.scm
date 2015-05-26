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

(eval-when (compile load eval)

;Returns five values needed by the SETF machinery: a list of temporary
;variables, a list of values with which to fill them, the temporary for the
;new value in a list, the setting function, and the accessing function.
(defun get-setf-method (form &optional environment)
  (let (temp)
    (cond ((symbolp form)
	   (let ((new-var (gensym)))
	     (values nil nil (list new-var) `(setq ,form ,new-var) form)))
	  ((atom form)
	   (error "~S illegal atomic form for GET-SETF-METHOD." form))
	  ((setq temp (system-get (car form) 'setf-inverse))
	   (let ((new-var (gensym))
		 (vars nil)
		 (vals nil))
	     (dolist (x (cdr form))
	       (push (gensym) vars)
	       (push x vals))
	     (setq vals (nreverse vals))
	     (values vars vals (list new-var)
		     `(,temp ,@vars ,new-var)
		     `(,(car form) ,@vars))))
	  ((setq temp (system-get (car form) 'setf-method-expander))
	   (funcall temp form environment))
	  (t
	   (multiple-value-bind (res win)
				(macroexpand-1 form environment)
	     (if win
		 (get-setf-method res environment)
		 (error "~S is not a known location specifier for SETF."
			(car form))))))))

(defun get-setf-method-multiple-value (form &optional environment)
  "Like Get-Setf-Method, but may return multiple new-value variables."
  (get-setf-method form environment))

(defun defsetter (fn rest env)
  (let* ((arglist (car rest))
	 (new-var (car (cadr rest)))
	 (%arg-count 0)
	 (%min-args 0)
	 (%restp nil)
	 (%let-list nil)
	 (%keyword-tests nil))
    (declare (special %arg-count %min-args %restp %let-list %keyword-tests))
    (multiple-value-bind (body local-decs doc)
			 (parse-body (cddr rest) env)
      ;; Analyze the defmacro argument list.
      (analyze1 arglist '(cdr %access-arglist) fn '%access-arglist)
      ;; Now build the body of the transform.
      (when (null arglist) (push '(declare (ignore %access-arglist)) local-decs))
      (values 
       `(lambda (%access-arglist ,new-var)
	  (let* ,(nreverse %let-list)
	    ,@ local-decs
	    ,@ %keyword-tests
	    ,@ body))
       doc))))

) ; End of Eval-When.
