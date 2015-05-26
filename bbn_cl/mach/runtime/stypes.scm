;;; -*-Scheme-*-
;;;
;;;	$Header: stypes.scm,v 13.91 88/08/31 09:13:19 jinx Exp $
;;;	$MIT-Header: stypes.scm,v 13.47 88/03/14 16:36:24 GMT jinx Rel $
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

;;;; SCODE Type Setup

(declare (usual-integrations))

;;;; Constants

(define unbound-type
  (make-sub-type 'UNBOUND (microcode-type-object 'LIST) unbound-object?))

(define unassigned-type
  (make-sub-type 'UNASSIGNED (microcode-type-object 'LIST) unassigned-object?))

(define null-type
  (microcode-type-object 'NULL))

(define false-type
  (microcode-type-object 'FALSE))

(define true-type
  (microcode-type-object 'TRUE))

(define cell-type
  (microcode-type-object 'CELL))

(define symbol-type 
  (make-union-type 'SYMBOL
		   (microcode-type-object 'UNINTERNED-SYMBOL)
		   (microcode-type-object 'INTERNED-SYMBOL)))

(define number-type
  (make-union-type 'NUMBER
		   (microcode-type-object 'FIXNUM)
		   (microcode-type-object 'BIGNUM)
		   (microcode-type-object 'FLONUM)
		   (microcode-type-object 'COMPLEX)))

;;;; Active Data

(define primitive-procedure-type
  (microcode-type-object 'PRIMITIVE))

(define compound-procedure-type
  (make-union-type 'COMPOUND-PROCEDURE
		   (microcode-type-object 'PROCEDURE)
		   (microcode-type-object 'EXTENDED-PROCEDURE)))

;; This is bogus.  See ustruc.scm for the real thing.

(define compiled-procedure-type
  (microcode-type-object 'COMPILED-ENTRY))

(define procedure-type
  (make-union-type 'PROCEDURE
		   primitive-procedure-type
		   compound-procedure-type
		   compiled-procedure-type))

(define environment-type
  (microcode-type-object 'ENVIRONMENT))

(define promise-type
  (microcode-type-object 'DELAYED))

(define continuation-type
  (make-sub-type 'CONTINUATION compound-procedure-type continuation?))

(define unparser-special-object-type
  (make-sub-type 'UNPARSER-SPECIAL-OBJECT (microcode-type-object 'VECTOR)
    (lambda (vector)
      (and (not (zero? (vector-length vector)))
	   (assq (vector-ref vector 0)
		 (access *unparser-special-objects* unparser-package))))))

;;;; SCode

(define quotation-type
  (microcode-type-object 'QUOTATION))

(define variable-type
  (microcode-type-object 'VARIABLE))

(define definition-type
  (microcode-type-object 'DEFINITION))

(define assignment-type
  (microcode-type-object 'ASSIGNMENT))

(define comment-type
  (microcode-type-object 'COMMENT))

(define declaration-type
  (make-sub-type 'DECLARATION comment-type declaration?))

(define the-environment-type
  (microcode-type-object 'THE-ENVIRONMENT))

(define access-type
  (microcode-type-object 'ACCESS))

(define in-package-type
  (microcode-type-object 'IN-PACKAGE))

(define delay-type
  (microcode-type-object 'DELAY))

(define sequence-type
  (make-union-type 'SEQUENCE
		   (microcode-type-object 'SEQUENCE-2)
		   (microcode-type-object 'SEQUENCE-3)))

(define open-block-type
  (make-sub-type 'OPEN-BLOCK
		 (microcode-type-object 'SEQUENCE-3)
		 open-block?))

(define conditional-type
  (microcode-type-object 'CONDITIONAL))

(define disjunction-type
  (microcode-type-object 'DISJUNCTION))

(define lambda-type
  (make-union-type 'LAMBDA
		   (microcode-type-object 'LAMBDA)
		   (microcode-type-object 'LEXPR)
		   (microcode-type-object 'EXTENDED-LAMBDA)))

(define combination-type
  (make-union-type 'COMBINATION
		   (microcode-type-object 'COMBINATION-1)
		   (microcode-type-object 'COMBINATION-2)
		   (microcode-type-object 'COMBINATION)
		   (microcode-type-object 'PRIMITIVE-COMBINATION-0)
		   (microcode-type-object 'PRIMITIVE-COMBINATION-1)
		   (microcode-type-object 'PRIMITIVE-COMBINATION-2)
		   (microcode-type-object 'PRIMITIVE-COMBINATION-3)))

(define unassigned?-type
  (make-sub-type 'UNASSIGNED? combination-type unassigned??))

(define unbound?-type
  (make-sub-type 'UNBOUND? combination-type unbound??))

(define error-combination-type
  (make-sub-type 'ERROR-COMBINATION combination-type
    (lambda (combination)
      (let ((operator (combination-operator combination)))
	(or (eq? operator error-procedure)
	    (and (access? operator)
		 (eq? (access-environment operator) system-global-environment)
		 (eq? (access-name operator) 'ERROR-PROCEDURE)))))))
