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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Keyboard Interrupt Control

(declare (usual-integrations))

(define keyboard-interrupt-dispatch-table)
(define set-keyboard-interrupt-dispatch-table!)
(define make-keyboard-interrupt-dispatch-table)
(define reset-keyboard-interrupt-dispatch-table!)

(let ()

#| THIS CODE DOESN'T WORK UNDER UNIX(TM) .....................

(define substring-read! (make-primitive-procedure 'SUBSTRING-READ!))
(define substring-write! (make-primitive-procedure 'SUBSTRING-WRITE!))
(define lookup-system-symbol (make-primitive-procedure 'LOOKUP-SYSTEM-SYMBOL))

(define table-lock-address)
(define table-address)

(set! reset-keyboard-interrupt-dispatch-table!
(named-lambda (reset-keyboard-interrupt-dispatch-table!)
  (set! table-lock-address
	(lookup-system-symbol "SCHEME_KEYBOARD_INTERRUPT_DISPATCH_TABLE"))
  (set! table-address (+ 2 table-lock-address))))

(reset-keyboard-interrupt-dispatch-table!)

(define table-unlock-code
  (list->string (map ascii->char '(#x63 #xC9))))

(define table-lock-code
  (list->string (map ascii->char '(#x00 #x00))))

(define table-size
  (* 2 #x400))

(define (string-read memory-address n)
  (let ((result (string-allocate n)))
    (substring-read! result 0 n memory-address)
    result))

(define (string-write! memory-address string)
  (substring-write! string 0 (string-length string) memory-address))

(set! keyboard-interrupt-dispatch-table
(named-lambda (keyboard-interrupt-dispatch-table)
  (string-read table-address table-size)))

(set! set-keyboard-interrupt-dispatch-table!
(named-lambda (set-keyboard-interrupt-dispatch-table! new-table)
  (if (not (= (string-length new-table) table-size))
      (error "Bad keyboard interrupt dispatch table"))
  (string-write! table-lock-address table-lock-code)
  (string-write! table-address new-table)
  (string-write! table-lock-address table-unlock-code)))


(set! make-keyboard-interrupt-dispatch-table
(named-lambda (make-keyboard-interrupt-dispatch-table)
  (let ((table (string-allocate table-size)))
    (vector-8b-fill! table 0 table-size 0)
    table)))
............................... |#

(set! reset-keyboard-interrupt-dispatch-table!
      (named-lambda (reset-keyboard-interrupt-dispatch-table!)
	'KEYBOARD-INTERRUPT-DISPATCH-TABLE-RESET!))

(set! keyboard-interrupt-dispatch-table
      (named-lambda (keyboard-interrupt-dispatch-table)
	'NO-DISPATCH-TABLE))

(set! set-keyboard-interrupt-dispatch-table!
      (named-lambda (set-keyboard-interrupt-dispatch-table! new-table)
	'YOU-BETTER-NOT-THINK-I-DID-THIS))
      
(set! make-keyboard-interrupt-dispatch-table
      (named-lambda (make-keyboard-interrupt-dispatch-table)
	'HA-HA-HA!!))
)

(define (with-keyboard-interrupt-dispatch-table new-table thunk)
  (define old-table)
  (dynamic-wind (lambda ()
		  (set! old-table (keyboard-interrupt-dispatch-table))
		  (set-keyboard-interrupt-dispatch-table! new-table))
		thunk
		(lambda ()
		  (set! new-table (keyboard-interrupt-dispatch-table))
		  (set-keyboard-interrupt-dispatch-table! old-table))))

