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

(export '(deftype type-of

	  ;; Names of types...
	  array atom bignum bit bit-vector character common package
	  compiled-function complex cons double-float fixnum float
	  function integer keyword list long-float nil null number ratio
	  rational sequence short-float signed-byte simple-array
	  simple-bit-vector simple-string simple-vector single-float
	  standard-char string string-char symbol t unsigned-byte vector
	  structure satisfies))

;;; Create the list-style description of a G-vector.

(defun describe-g-vector (object)
  (g-vector-ref object 0))

;;; Create the list-style description of an I-vector.
	
(defun describe-i-vector (object)
  `(simple-array ,(i-vector-element-type object)
		 ,(array-total-size object)))

;;; Create the list-style description of an array.

(defun describe-array (object)
  (let ((data-vector (find-data-vector object)))
    (let ((rank (array-rank object))
	  (length (array-total-size object)))
      (if (= rank 1)
	  (typecase data-vector
		    (simple-bit-vector `(bit-vector ,length))
		    (simple-string `(string ,length))
		    (simple-vector `(vector t ,length))
		    (t `(vector ,(array-element-type object) ,length)))
	  `(array
	    ,(typecase data-vector
		       (simple-bit-vector '(mod 2))
		       (simple-string 'string-char)
		       (simple-vector 't)
		       (t ,(array-element-type object)))
	    ,(array-dimensions object))))))


(defun type-of-env (env)
  (cond
   ((readtablep env) 'readtable)
   (t (object-type env))))

(defun env-commonp (env)
  (cond
   ((readtablep env) t)
   (t nil)))

(defun type-of-vector (vector)
  (cond
   ((pathname? vector) 'pathname)
   (t (object-type vector))))

(defun vector-commonp (vector)
  (cond
   ((pathname? vector) t)
   (t nil)))

(defun type-of-entry (entry)
  (let ((entry-type (compiled-entry-type entry)))
    (cond ((eq entry-type 'COMPILED-PROCEDURE)
	   'COMPILED-FUNCTION)
	  (t entry-type))))

;;;
;;; Similar to scheme's microcode-types-vector
;;;
;;; NOTE!!!: A scheme vector is used here due to 
;;;          infinite recursion when using svref and other cl array functions
;;;          that themselves call upon the type inquiry mechanisms.
;;;
;;; Note the list notation is used to avoid forward ref problems as can occur when using #'.
;;;
;;; In the commonp column, nil or t is used to indicate true or false;
;;; another symbol denotes the name of a function to run to determine commonp.
;;;

(defconstant commonlisp-types-vector
  ((access vector ())

;   cl type name or 
;      determinant fcn     commonp        scheme type name  

   '(null                  t)               ;   null
   '(cons                  t)               ;   pair
   '(character             t)               ;   character 
   '((object-type)       nil)               ;   quotation 
   '((object-type)       nil)               ;   primitive-combination-2 
   '(symbol                t)               ;   uninterned-symbol 
   '(float                 t)               ;   flonum
   '((object-type)       nil)               ;   combination-1 
   '((object-type)         t)               ;   true 
   '(function              t)               ;   extended-procedure 
   '((type-of-vector)    vector-commonp)    ;   vector 
   '((object-type)       nil)               ;   return-address 
   '((object-type)       nil)               ;   combination-2 
   '((object-type)       nil)               ;   manifest-closure
   '(bignum                t)               ;   bignum
   '(function              t)               ;   procedure 
   '(function		   t)               ;   entity
   '((object-type)       nil)               ;   delay 
   '((type-of-env)       env-commonp)       ;   environment 
   '((object-type)       nil)               ;   delayed 
   '((object-type)       nil)               ;   extended-lambda 
   '((object-type)       nil)               ;   comment 
   '((object-type)       nil)               ;   non-marked-vector 
   '((object-type)       nil)               ;   lambda 
   '((object-type)       nil)               ;   primitive 
   '((object-type)       nil)               ;   sequence-2 
   '(fixnum                t)               ;   fixnum
   '((object-type)       nil)               ;   primitive-combination-1 
   '((object-type)       nil)               ;   control-point 
   '(symbol                t)               ;   interned-symbol 
   '(simple-string         t)               ;   string
   '((object-type)       nil)               ;   access 
   '((object-type)       nil)               ;   extended-fixnum 
   '((object-type)       nil)               ;   definition 
   '((object-type)       nil)               ;   broken-heart 
   '((object-type)       nil)               ;   assignment 
   '((object-type)       nil)               ;   triple
   '((object-type)       nil)               ;   in-package 
   '((object-type)       nil)               ;   combination 
   '((object-type)       nil)               ;   manifest-nm-vector 
   '((type-of-entry)     compiled-function-p)     ;   compiled-entry
   '((object-type)       nil)               ;   lexpr 
   '((object-type)       nil)               ;   primitive-combination-3 
   '((object-type)       nil)               ;   manifest-special-nm-vector 
   '((object-type)       nil)               ;   variable 
   '((object-type)       nil)               ;   the-environment 
   '((object-type)       nil)               ;   future 
   '(simple-bit-vector     t)               ;   vector-1b
   '((object-type)       nil)               ;   primitive-combination-0 
   '((object-type)       nil)               ;   vector-16b 
   '((object-type)       nil)               ;   unassigned 
   '((object-type)       nil)               ;   sequence-3 
   '((object-type)       nil)               ;   conditional 
   '((object-type)       nil)               ;   disjunction
   '((object-type)       nil)               ;   cell
   '((object-type)       nil)               ;   weak-cons
   '((object-type)       nil)               ;   quad
   '((object-type)       nil)               ;   compiler-return-address
   '((object-type)       nil)               ;   compiler-link
   '((object-type)       nil)               ;   stack-environment
   '(complex               t)               ;   complex
   '((object-type)       nil)               ;   compiled-code-block
   '((object-type)       nil)               ;   **obsolete** (was header)
   '((object-type)       nil)               ;   **obsolete** (was i-vector)
   '((describe-g-vector)   t)               ;   g-vector 
   '((object-type)       nil)               ;   io-error-code
   '(package		 nil)               ;   cl-package
   '((object-type)       nil)               ;   clsav
   '(ratio                 t)               ;   ratio
   '(stream                t)               ;   stream
   '((object-type)       nil)               ;   vector-32b
   '((describe-array)      t)	            ;   cl-array
   '((describe-i-vector)   t)	            ;   cl-i-vector
))

;;;; TYPE-OF and auxiliary functions.

;;;
;;; Note that type-of should really take into account alterations made to the type
;;;  lattice made by deftype, by searching the extended lattice and classifying the
;;;  given datum. I don't know of any implementations that do this, but to do so
;;;  would add a distinctive mark of elegance and consistency.
;;;
;;; Because of this short-cut approach, use of type-of should be avoided, especially
;;;  in user code. Instead, typep, typecase, etc., should be used.
;;;
;;; When the need arises in system code to use type-of, use basic-type-of instead.
;;;  Presumably, type-of can then change without our having to change all other code.
;;;

(defun type-of (object)
  (basic-type-of object))

(defun basic-type-of (object)
  "Returns the type of OBJECT as a type-specifier.
  Since objects may be of more than one type, the choice is somewhat
  arbitrary and may be implementation-dependent."
  (let* ((scheme-tag (primitive-type object))
	 (entry (car (vector-ref commonlisp-types-vector scheme-tag))))
    (if (symbolp entry) entry 
	(if (pair? entry) (funcall (car entry) object)
	    (funcall entry object)))))

(defun commonp (x)
  (let ((r (cadr (vector-ref commonlisp-types-vector (primitive-type x)))))
    (if (memq r '(t nil))
	r
	(funcall r x))))

;;; Type-Expand  --  Internal
;;;
;;;    Similar to Macroexpand, but expands deftypes.  We don't bother returning
;;; a second value.
;;;
(defun type-expand (form)
  (let ((def (cond ((symbolp form)
		    (system-get form 'deftype-expander))
		   ((and (consp form) (symbolp (car form)))
		    (system-get (car form) 'deftype-expander))
		   (t nil))))
    (if def
	(type-expand (funcall def (if (consp form) form (list form))))
	form)))

;;;
;;; deftype
;;;
(eval-when (compile load eval)
	   (cl-define (clear-subtype-cache) '())   ;; boot def
)

(defparameter deftype-error-string "Type ~S cannot be used with ~S args.")

(defmacro deftype (name arglist &body body)
  "Syntax like DEFMACRO, but defines a new type."
  (unless (symbolp name)
	  (error "~S -- Type name not a symbol." name))
  
  (let ((whole (gensym)))
    (multiple-value-bind 
     (body local-decs doc)
     (parse-defmacro arglist whole body name
		     :default-default ''*
		     :error-string 'deftype-error-string)
     `(eval-when (compile load eval)
		 (clear-subtype-cache)
		 (setf (system-get ',name 'deftype-expander)
		       #'(lambda (,whole) ,@local-decs (block ,name ,body)))
		 ,@(when doc
			 `((setf (documentation ',name 'type) ,doc)))
		 ',name))))

;;;
;;; Important built-in compound types
;;;

(deftype list () '(or null cons))

(deftype rational () '(or integer ratio))

(deftype non-future (&optional x) 
  (if x 
      `(and (not future) ,x)
      `(and (not future) #t)))

(deftype imaginary (&optional (type '(or rational float)))
  `(and (complex ,type) (satisfies realpart-zero-p)))


(deftype real () 
  '(or rational float
       (and (complex float)
	    (satisfies imagpart-zero-p))))

(defun realpart-zero-p (x)
  (zerop (realpart x)))

(defun imagpart-zero-p (x)
  (zerop (imagpart x)))
