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

(export '(tagbody prog prog* go values values-list multiple-value-list multiple-value-call
		  multiple-value-prog1 multiple-value-bind multiple-value-setq catch
		  unwind-protect throw))

;;prog and prog*

(boot-defmacro prog (vars &rest body)
  (prim-with-values
   (lambda () (parse-body-internal body *syntax-time-env* #f))
   (lambda (code decls doc)
     `(block nil
	     (let ,vars
	       ,@decls
	       (tagbody ,@code))))))

(boot-defmacro prog* (vars &rest body)
  (prim-with-values
   (lambda () (parse-body-internal body *syntax-time-env* #f))
   (lambda (code decls doc)
     `(block nil
	     (let* ,vars
	       ,@decls
	       (tagbody ,@code))))))

;;7.9 Multiple Values

(cl-define values prim-values)

(cl-define values-list prim-values-list)

(boot-defmacro multiple-value-list (form)
  `(prim-with-values
    (lambda () ,form)
    (lambda all all)))

(def-special-form (multiple-value-call function . forms)
  `(apply ,function 
	  (append ,@((access mapcar ())
		     (lambda (form) `(multiple-value-list ,form))
		     forms))))

(def-special-form (multiple-value-prog1 form . forms)
  (let ((result (generate-uninterned-symbol 'mvprog1)))
    `(let ((,result (multiple-value-list ,form)))
       ,@forms
       (values-list ,result))))

(boot-defmacro multiple-value-bind (vars values-form &rest body)
  (let ((ignore (generate-uninterned-symbol 'mvbignore)))
    `(prim-with-values
      (lambda () ,values-form)
      (cl-lambda (&optional ,@vars &rest ,ignore)
		 ,@body))))

(boot-defmacro multiple-value-setq (variables form)
  (let ((result (generate-uninterned-symbol 'mvsetq)))
    (let ((binding-list
	   (labels ((make-binding-loop (n v)
				       (if (null? v)
					   '()
					   (cons (car v)
						 (cons (cons 'nth (list n result))
						       (make-binding-loop (1+ n) (cdr v)))))))
	     (make-binding-loop 0 variables))))
      `(let ((,result (multiple-value-list ,form)))
	 (setq ,@binding-list)
	 (car ,result)))))

;;7.10 Dynamic non-local exits

(define *common-lisp-catch-throw-stack* '())

(def-special-form (catch tag . forms)
  (let ((cont (fundefsym (generate-uninterned-symbol 'catch-))))
    `(cl-nr-call/cc;; This is the non-reenterant kind since
      (lambda (,cont);; catches are not reused.
	(values-list
	 (fluid-let ((*common-lisp-catch-throw-stack* 
		      (cons (list ,tag ,cont)
			    *common-lisp-catch-throw-stack*)))
	   (multiple-value-list (progn ,@forms))))))))

(def-special-form (unwind-protect protected-form . cleanup-forms)
  `(values-list
    (dynamic-wind
     (lambda () #!null)
     (lambda () (multiple-value-list ,protected-form))
     (lambda () ,@cleanup-forms))))

(def-special-form (throw tag form)
  (let ((assoc-tag (generate-uninterned-symbol 'assoc-tag))
	(result (generate-uninterned-symbol 'throw-result)))
    `(let ((,assoc-tag (assq ,tag *common-lisp-catch-throw-stack*)))
       (if (null? ,assoc-tag)
	   (error "Throw to unknown tag: ~a" ,tag)
	   (let ((,result (multiple-value-list ,form)))
	     (apply (second ,assoc-tag) ,result))))));; Apply the continuation to the values to be returned by the form.
