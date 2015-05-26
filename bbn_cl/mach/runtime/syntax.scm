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

;;;; SYNTAX: S-Expressions -> SCODE

(declare (usual-integrations))

(define lambda-tag:unnamed
  (make-named-tag "UNNAMED-PROCEDURE"))

(define *fluid-let-type*
  'SHALLOW)

(define lambda-tag:shallow-fluid-let
  (make-named-tag "SHALLOW-FLUID-LET-PROCEDURE"))

(define lambda-tag:deep-fluid-let
  (make-named-tag "DEEP-FLUID-LET-PROCEDURE"))

(define lambda-tag:common-lisp-fluid-let
  (make-named-tag "COMMON-LISP-FLUID-LET-PROCEDURE"))

(define lambda-tag:let
  (make-named-tag "LET-PROCEDURE"))

(define lambda-tag:make-environment
  (make-named-tag "MAKE-ENVIRONMENT-PROCEDURE"))

(define syntax)
(define syntax*)
(define macro-spreader)

(define enable-scan-defines!)
(define with-scan-defines-enabled)
(define disable-scan-defines!)
(define with-scan-defines-disabled)

;; Enable shallow vs fluid binding for FLUID-LET
(define shallow-fluid-let!)
(define deep-fluid-let!)
(define common-lisp-fluid-let!)

(define system-global-syntax-table)
(define syntax-table?)
(define make-syntax-table)
(define extend-syntax-table)
(define copy-syntax-table)
(define syntax-table-ref)
(define syntax-table-define)
(define syntax-table-shadow)
(define syntax-table-undefine)

;;; Debug flags for commonlisp declarations

(define *decl-debug* #f)

(define *optimization-debug* #f)

;;; Whether or not to process commonlisp declarations

(define *process-declarations* #f)

;;; Optimizer interface

(define syntax-type-of)
(define syntax-constant?)
(define add-optimizer!)

;;; Declaration interface

(define touch-mode)
(define syntaxer-proclaim!)
(define lr-evaluation-mode?)

;;; Structure operation interface

(define structure-operation-info) ; defined in cl/clchap19.scm

;;; This gets swapped to fundefsym when cl-mode is on

(define *get-fcn-name* (lambda (x) x))

;;; Some simplified commonlisp utilities that are needed
;;; to handle declarations

(define (subtypep x y)
  (if (eq? y #t)
      #t
      (eq? x y)))

(define (typep x y)
  (define (cl-typify x)
    (cond ((eq? x 'string)
	   'simple-string)
	  ((eq? x 'vector)
	   'simple-vector)
	  (else x)))
  (let ((type (cl-typify (microcode-type-name (primitive-type x)))))
    (subtypep type y)))

(define (normalize-type-expr x)
  (if (and (pair? x)
	   (eq? (first x) 'and))
      (cond ((and (equal? (second x) '(not future))
		  (not (eq? (third x) 'future)))
	     (third x))
	    ((and (equal? (second x) '(not future))
		  (eq? (third x) 'future))
	     '())
	    ((and (eq? (second x) 'future)
		  (not (eq? (third x) 'future)))
	     '())
	    (else x))
      x))

(define (warn message . args)
  (newline)
  (princ message)
  (princ args))

(define (get x y)
  nil)

(define (subtract-futures x)
  x)

;;; Binding environment for lambda-abstraction.  The environments here
;;; are used to track shawdowing of macros as well as for keeping
;;; track of optimization information added through commonlisp
;;; declarations.

;; The top-level bindings of the *syntax-time-global-env* will be optimizer
;; functions for global functions.  It can be globally enhanced by
;; adding optimizers through ADD-OPTIMIZER! or by declarations made
;; by PROCLAIM.

;;;  The environment stack and binding abstraction

(define *syntax-time-global-env*
  (in-package '()
    (make-environment)))

(define *syntax-time-env* *syntax-time-global-env*)

(define (push-contour environment)
  (in-package environment
    (make-environment)))

(define (commonlisp-macro? fcn-sym env)
  (and (not (lexical-unreferenceable? env fcn-sym))
       (let ((def (lexical-reference env fcn-sym)))
	 (and (pair? def)
	      (eq? (car def) 'commonlisp-macro)))))

;;; The start of the syntaxer package

(define syntaxer-package)

(let ((external-make-sequence make-sequence)
      (external-make-lambda make-lambda))

  (set! syntaxer-package (the-environment))
)

;;; The commonlisp imperative optimizer package

(define imperative-optimizer-package)
