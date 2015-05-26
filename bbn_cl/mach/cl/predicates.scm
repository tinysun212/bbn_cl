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
;;;; Common Lisp predicates (typep and subtypep are in their own files)

(proclaim '(insert-touches nil))

(export '(null symbolp atom consp listp numberp integerp rationalp
	  floatp complexp characterp
	  functionp compiled-function-p commonp eq eql equal equalp not))

(cl-define null null?)

(defun symbolp (x)
  (set! x (touch x))
  (or (null? x) 
      (eq? x #t)
      (symbol? x)))

(proclaim '(function symbolp (t) (member t nil)))

(defun atom (x)
  (set! x (touch x))
  (not (pair? x)))

(proclaim '(function atom (t) (member t nil)))

(cl-define consp pair?)

(defun listp (x)
  (set! x (touch x))
  (or (pair? x)
      (null? x)))

(proclaim '(function listp (t) (member t nil)))

(defun numberp (x)
  (set! x (touch x))
  (or (number? x)
      (complexp x)
      (ratiop x)))

(proclaim '(function numberp (t) (member t nil)))

(cl-define integerp integer?)

(proclaim '(function integerp (t) (member t nil)))

(defun rationalp (x)
  (set! x (touch x))
  (or (integer? x)
      (ratiop x)))

(proclaim '(function rationalp (t) (member t nil)))

;;; primitive-type touches, and is not inline.

(defun floatp (x)
  (eq? (microcode-type-name (primitive-type x)) 'flonum))

(proclaim '(function floatp (t) (member t nil)))

(cl-define characterp char?)

(proclaim '(function characterp (t) (member t nil)))

;;; PACKAGEP is in package-a.scm

;;; STRINGP, BIT-VECTOR-P, VECTORP, SIMPLE-VECTOR-P, SIMPLE-STRING-P,
;;; SIMPLE-BIT-VECTOR-P and ARRAYP is-defined in cl-array.scm

(define *user-lambda-symbol* #f) ; assigned in setup-packages

(cl-define (functionp x)
  (set! x (touch x))
  (or (procedure? x)
      (symbolp x)
      (and (pair? x)
	   (or (eq? (car x) 'lambda)
	       (eq? (car x) *user-lambda-symbol*)))))

(proclaim '(function functionp (t) (member t nil)))

(defun compiled-function-p (x)
  (compiled-procedure? x))

(proclaim '(function compiled-function-p (t) (member t nil)))

;;; COMMONP is in type-specifier.scm

;;6.3 equality predicates

(cl-define eq eq?)

(defun eql (x y)
  (set! x (touch x))
  (set! y (touch y))
  (cond ((characterp x)
	 (and (characterp y)
	      (char= x y)))
	((numberp x)
	 (cond ((complexp x)
		(and (complexp y)
		     (eql (real-part x) (real-part y))
		     (eql (imag-part x) (imag-part y))))
	       ((floatp x)
		(and (floatp y)
		     (= x y)))
	       ((integerp x)
		(and (integerp y)
		     (= x y)))
	       (else              ;; Ratio case
		(= x y))))
	(else
	 (eq? x y))))

(proclaim '(function eql (t t) (member t nil)))

;;6.4 logical operators

;;not, and, or same as rrrs scheme

;;; An early definition needed for equal

(cl-define pathnamep pathname?) 

(proclaim '(function pathnamep (t) (member t nil)))

;;; Some needed additions

;;; primitive-type touches, and is not inline.

(defun fixnump (x)
  (eq? (microcode-type-name (primitive-type x)) 'fixnum))

(proclaim '(function fixnump (t) (member t nil)))

(defun bignump (x)
  (eq? (microcode-type-name (primitive-type x)) 'bignum))

(proclaim '(function bignump (t) (member t nil)))

(defun complexp (n)
  (eq? (microcode-type-name (primitive-type n)) 'recnum))

(proclaim '(function complexp (t) (member t nil)))

(defun ratiop (n)
  (eq? (microcode-type-name (primitive-type n)) 'ratio))

(proclaim '(function ratiop (t) (member t nil)))

;;; object-type calls primitive-type, which touches

(defun structurep (s)
  (eq? (object-type s) 'g-vector))

(proclaim '(function structurep (t) (member t nil)))

(defun bitp (object)
  (set! object (touch object))
  (and (fixnump object) 
       (or (= (the fixnum object) 0) (= (the fixnum object) 1))))

(proclaim '(function bitp (t) (member t nil)))

(cl-define short-floatp (symbol-function 'floatp))
(cl-define single-floatp (symbol-function 'floatp))
(cl-define long-floatp (symbol-function 'floatp))
(cl-define double-floatp (symbol-function 'floatp))

(proclaim '(function short-floatp (t) (member t nil)))
(proclaim '(function single-floatp (t) (member t nil)))
(proclaim '(function long-floatp (t) (member t nil)))
(proclaim '(function double-floatp (t) (member t nil)))
