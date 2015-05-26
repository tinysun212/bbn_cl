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

;;;; REP User Interface

(declare (usual-integrations))

;;; Standard Format

(define rep-message-hook false)
(define rep-prompt-hook false)

(define ((standard-rep-message string))
  (if rep-message-hook
      (rep-message-hook string)
      (begin (newline)
	     (write-string string))))

(define ((standard-rep-prompt string))
  (if rep-prompt-hook
      (rep-prompt-hook (rep-level) string)
      (begin (newline)
	     (newline)
	     (write (rep-level))
	     (write-char #\Space)
	     (write-string string)
	     (write-char #\Space))))

;;; Standard Drivers

(define (read-eval-print environment message prompt)
  (with-standard-proceed-point
   (lambda ()
     (push-rep environment
	       (standard-rep-message message)
	       (standard-rep-prompt prompt)))))

(define (abort-to-nearest-driver message)
  (abort->nearest (standard-rep-message message)))

(define (abort-to-previous-driver message)
  (abort->previous (standard-rep-message message)))

(define (abort-to-top-level-driver message)
  (abort->top-level (standard-rep-message message)))

(define (breakpoint message environment)
  (push-rep environment
	    (standard-rep-message message)
	    (standard-rep-prompt breakpoint-prompt)))

(define breakpoint-prompt
  "Bkpt->")

(define (breakpoint-procedure message irritant environment)
  (with-history-disabled
   (lambda ()
     (with-standard-proceed-point
      (lambda ()
	(breakpoint message environment))))))

;;; PROCEED

(define with-proceed-point)
(define with-standard-proceed-point)
(define proceed)
(let ()

(define (standard-value-filter arguments)
  (if (null? arguments)
      *the-non-printing-object*
      (car arguments)))

(define proceed-value-filter
  standard-value-filter)

(set! with-proceed-point
  (named-lambda (with-proceed-point value-filter thunk)
    (with-rep-continuation
     (lambda (continuation)
       (fluid-let ((proceed-value-filter value-filter))
	 (thunk))))))

(set! with-standard-proceed-point
  (named-lambda (with-standard-proceed-point thunk)
    (with-proceed-point standard-value-filter thunk)))

(set! proceed
  (named-lambda (proceed . args)
    (continue-rep (proceed-value-filter args))))

)

;;; User Interface Stuff

(define (%ge environment)
  (set-rep-base-environment! (coerce-to-environment environment))
  *rep-current-environment*)

(define (%ve environment)
  (set-rep-environment! (coerce-to-environment environment))
  (set-rep-prompt! (standard-rep-prompt %ve-prompt))
  *rep-current-environment*)

(define %ve-prompt
  "Visiting->")

(define (%gst syntax-table)
  (if (not (syntax-table? syntax-table))
      (error "Not a syntax table" syntax-table))
  (set-rep-base-syntax-table! syntax-table)
  *the-non-printing-object*)

(define (%vst syntax-table)
  (if (not (syntax-table? syntax-table))
      (error "Not a syntax table" syntax-table))
  (set-rep-syntax-table! syntax-table)
  *the-non-printing-object*)

(define (%in #!optional index)
  (if (unassigned? index) (set! index 1))
  (reader-history index))

(define (%out #!optional index)
  (if (unassigned? index) (set! index 1))
  ((make-primitive-procedure 'values-list)
   (printer-history (-1+ index))))

(define (coerce-to-environment object)
  (cond ((or (eq? object system-global-environment)
	     (environment? object))
	 object)
	((compound-procedure? object)
	 (procedure-environment object))
	(else
	 (error "COERCE-TO-ENVIRONMENT: Not an environment" object))))

(define (%pwd)
  (working-directory-pathname))

(define (%cd pathname)
  (set-working-directory-pathname! pathname))

(define (eval expression environment)
  (scode-eval (syntax expression *rep-current-syntax-table*)
	      environment))
