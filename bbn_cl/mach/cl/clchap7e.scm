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

(export '(define-setf-method))

;;define-setf-method

(defparameter defsetf-error-string "Setf expander for ~S cannot be called with ~S args.")

(defmacro define-setf-method (access-fn lambda-list &body body)
  "Syntax like DEFMACRO, but creates a Setf-Method generator.  The body
  must be a form that returns the five magical values."
  (unless (symbolp access-fn)
	  (error "~S -- Access-function name not a symbol in DEFINE-SETF-METHOD."
		 access-fn))

  (let ((whole (gensym)) (environment (gensym)))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro lambda-list whole body access-fn
					 :environment environment
					 :error-string 'defsetf-error-string)
			 `(eval-when (load compile eval)
				     (system-remprop ',access-fn 'setf-inverse)
				     (setf (system-get ',access-fn 'setf-method-expander)
					   (function (lambda (,whole ,environment) ,@local-decs (block ,access-fn ,body))))
				     ,@(when doc
					     `((setf (documentation ',access-fn 'setf) ,doc)))
				     ',access-fn))))


(define-setf-method getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
		       (get-setf-method place env)
    (let ((newval (gensym))
	  (ptemp (gensym))
	  (def-temp (gensym)))
      (values `(,@temps ,(car stores) ,ptemp ,@(if default `(,def-temp)))
	      `(,@values ,get ,prop ,@(if default `(,default)))
	      `(,newval)
	      `(progn (setq ,(car stores)
			    (putf ,(car stores) ,ptemp ,newval))
		      ,set
		      ,newval)
	      `(getf ,(car stores) ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-method get (symbol prop &optional default)
  "Get turns into %put. Don't put in the default unless it really is supplied and 
  non-nil, so that we can transform into the get instruction whenever possible."
  (let ((symbol-temp (gensym))
	(prop-temp (gensym))
	(def-temp (gensym))
	(newval (gensym)))
    (values `(,symbol-temp ,prop-temp ,@(if default `(,def-temp)))
	    `(,symbol ,prop ,@(if default `(,default)))
	    (list newval)
	    `(%put ,symbol-temp ,prop-temp ,newval)
	    `(get ,symbol-temp ,prop-temp ,@(if default `(,def-temp))))))

(define-setf-method system-get (symbol prop &optional default)
  "System-get turns into %system-put. Don't put in the default unless it really is supplied and 
  non-nil, so that we can transform into the get instruction whenever possible."
  (let ((symbol-temp (gensym))
	(prop-temp (gensym))
	(def-temp (gensym))
	(newval (gensym)))
    (values `(,symbol-temp ,prop-temp ,@(if default `(,def-temp)))
	    `(,symbol ,prop ,@(if default `(,default)))
	    (list newval)
	    `(%system-put ,symbol-temp ,prop-temp ,newval)
	    `(system-get ,symbol-temp ,prop-temp ,@(if default `(,def-temp))))))

(define-setf-method gethash (key hashtable &optional default)
  (let ((key-temp (gensym))
	(hashtable-temp (gensym))
	(default-temp (gensym))
	(new-value-temp (gensym)))
    (values
     `(,key-temp ,hashtable-temp ,@(if default `(,default-temp)))
     `(,key ,hashtable ,@(if default `(,default)))
     `(,new-value-temp)
     `(%puthash ,key-temp ,hashtable-temp ,new-value-temp)
     `(gethash ,key-temp ,hashtable-temp ,@(if default `(,default-temp))))))

(defsetf subseq (sequence start &optional (end nil)) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
	  ,v))


;;; Evil hack invented by the gnomes of Vassar Street.  The function
;;; arg must be constant.  Get a setf method for this function, pretending
;;; that the final (list) arg to apply is just a normal arg.  If the
;;; setting and access forms produced in this way reference this arg at
;;; the end, then just splice the APPLY back onto the front and the right
;;; thing happens.

(define-setf-method apply (function &rest args &environment env)
  (if (and (listp function)
	   (= (list-length function) 2)
	   (eq (first function) 'function)
	   (symbolp (second function)))
      (setq function (second function))
      (error
       "Setf of Apply is only defined for function args of form #'symbol."))
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method (cons function args) env)
    ;; Make sure the place is one that we can handle.
    (unless (and (eq (car (last args)) (car (last vals)))
		 (eq (car (last getter)) (car (last dummies)))
		 (eq (car (last setter)) (car (last dummies))))
	    (error "Apply of ~S not understood as a location for Setf."
		   function))
    (values dummies vals newval
	    `(apply (function ,(car setter)) ,@(cdr setter))
	    `(apply (function ,(car getter)) ,@(cdr setter)))))

'$split-file


(define-setf-method ldb (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this
  place with bits from the low-order end of the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values (cons btemp dummies)
	      (cons bytespec vals)
	      (list gnuval)
	      `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		 ,setter
		 ,gnuval)
	      `(ldb ,btemp ,getter)))))


(define-setf-method mask-field (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this place
  with bits from the corresponding position in the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values (cons btemp dummies)
	      (cons bytespec vals)
	      (list gnuval)
	      `(let ((,(car newval) (deposit-field ,gnuval ,btemp ,getter)))
		 ,setter
		 ,gnuval)
	      `(mask-field ,btemp ,getter)))))


(define-setf-method char-bit (place bit-name &environment env)
  "The first argument is any place form acceptable to SETF.  Replaces the
  specified bit of the character in this place with the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values `(,@dummies ,btemp)
	      `(,@vals ,bit-name)
	      (list gnuval)
	      `(let ((,(car newval)
		      (set-char-bit ,getter ,btemp ,gnuval)))
		 ,setter
		 ,gnuval)
	      `(char-bit ,getter ,btemp)))))


(define-setf-method the (type place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
      (values dummies
	      vals
	      newval
	      (subst `(the ,type ,(car newval)) (car newval) setter)
	      `(the ,type ,getter))))

