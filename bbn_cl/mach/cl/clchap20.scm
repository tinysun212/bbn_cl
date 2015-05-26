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
;; Chapter 20 -- The Evaluator

(proclaim '(insert-touches nil))

;;20.1 Run-Time Evaluation of Forms

(export '(eval *evalhook* *applyhook* evalhook applyhook constantp
	  ** *** ++ +++ // ///))

(cl-define scm-eval (access eval ()))

(cl-define (eval form)
	   (scm-eval form *commonlisp-user-environment*))

(defvar *evalhook*)
(setq *evalhook* nil)

(defvar *applyhook*)
(setq *applyhook* nil)

(defun evalhook (form evalhookfcn applyhookfn &optional env)
  (commonlisp-nyi 'evalhook))

(defun applyhook (function args evalhookfcn applyhookfn &optional env)
  (commonlisp-nyi 'applyhook))

(defconstant commonlisp-constant-vector
  ((access vector ())

;   constantp        scheme type name  

   t               ;   null
   'dont-know      ;   pair
   t               ;   character 
   t               ;   quotation 
   nil             ;   primitive-combination-2 
   'dont-know      ;   uninterned-symbol 
   t               ;   flonum
   nil             ;   combination-1 
   t               ;   true 
   t               ;   extended-procedure 
   t               ;   vector 
   t               ;   return-address 
   nil             ;   combination-2 
   t               ;   manifest-closure
   t               ;   bignum
   t               ;   procedure 
   t               ;   primitive-external 
   t               ;   delay 
   t               ;   environment 
   t               ;   delayed 
   t               ;   extended-lambda 
   t               ;   comment 
   t               ;   non-marked-vector 
   t               ;   lambda 
   t               ;   primitive 
   nil             ;   sequence-2 
   t               ;   fixnum
   nil             ;   primitive-combination-1 
   t               ;   control-point 
   'dont-know      ;   interned-symbol 
   t               ;   string
   nil             ;   access 
   t               ;   extended-fixnum 
   nil             ;   definition 
   t               ;   broken-heart 
   nil             ;   assignment 
   t               ;   triple
   nil             ;   in-package 
   nil             ;   combination 
   t               ;   manifest-nm-vector 
   nil             ;   compiled-expression 
   t               ;   lexpr 
   nil             ;   primitive-combination-3 
   t               ;   manifest-special-nm-vector 
   nil             ;   variable 
   t               ;   the-environment 
   t               ;   future 
   t               ;   vector-1b
   nil             ;   primitive-combination-0 
   t               ;   vector-16b 
   t               ;   unassigned 
   nil             ;   sequence-3 
   nil             ;   conditional 
   nil             ;   disjunction
   t               ;   cell
   t               ;   weak-cons
   t               ;   quad
   t               ;   linkage-section
   t               ;   compiler-link
   t               ;   stack-environment
   t               ;   complex
   t               ;   compiled-code-block
   t               ;   **obsolete** (was header)
   t               ;   **obsolete** (was i-vector)
   t               ;   g-vector 
   t               ;   io-error-code
   t               ;   cl-package
   t               ;   clsav
   t               ;   ratio
   t               ;   stream
   t               ;   vector-32b
   t               ;   cl-array
   t	           ;   cl-i-vector
))

(defun constantp (object)
  (let ((first-guess (vector-ref commonlisp-constant-vector (primitive-type object))))
    (if (eq first-guess 'dont-know)
	(cond ((pair? object)
	       (eq (car object) 'quote))
	      ((symbolp object)
	       (cond ((keywordp object)
		      t)
		     ((system-get object '%constant)
		      t)
		     (else nil)))
	      (else nil))
	first-guess)))

	    

