;;; -*-Scheme-*-
;;;
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Fixnum Arithmetic

(declare (usual-integrations))

(let-syntax
    ((define-primitive
       (macro (name primitive)
	 `(LOCAL-ASSIGNMENT SYSTEM-GLOBAL-ENVIRONMENT
			    ',name
			    ,(make-primitive-procedure primitive)))))

(define-primitive fix:zero? zero-fixnum?)
(define-primitive fix:negative? negative-fixnum?)
(define-primitive fix:positive? positive-fixnum?)
(define-primitive fix:= equal-fixnum?)
(define-primitive fix:< less-than-fixnum?)
(define-primitive fix:> greater-than-fixnum?)
(define-primitive fix:1+ one-plus-fixnum)
(define-primitive fix:-1+ minus-one-plus-fixnum)
(define-primitive fix:+ plus-fixnum)
(define-primitive fix:- minus-fixnum)
(define-primitive fix:* multiply-fixnum)
(define-primitive fix:divide divide-fixnum)
(define-primitive fix:gcd gcd-fixnum)

)

(define (fix:quotient x y)
  (car (fix:divide x y)))

(define (fix:remainder x y)
  (cdr (fix:divide x y)))
