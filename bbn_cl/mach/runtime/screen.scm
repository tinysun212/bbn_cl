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

;;;; 9836 Alpha Screen Hacks

(declare (usual-integrations))

(define alpha-screen)
(define (saving-alpha-screen thunk)
  (define old-screen)
  (dynamic-wind (lambda ()
		  (set! old-screen
			(set! alpha-screen (save-alpha-screen))))
		thunk
		(lambda ()
		  (restore-alpha-screen!
		   (set! alpha-screen (set! old-screen))))))

(define save-alpha-screen)
(define restore-alpha-screen!)
(let ()

(define primitive-save
  (make-primitive-procedure 'SAVE-SCREEN))

(define primitive-restore!
  (make-primitive-procedure 'RESTORE-SCREEN!))

(define get-cursor
  (make-primitive-procedure 'TTY-GET-CURSOR))

(define move-cursor
  (make-primitive-procedure 'TTY-MOVE-CURSOR))

(set! save-alpha-screen
(named-lambda (save-alpha-screen)
  (cons (get-cursor) (primitive-save))))

(set! restore-alpha-screen!
(named-lambda (restore-alpha-screen! saved-screen)
  (primitive-restore! (cdr saved-screen))
  (move-cursor (caar saved-screen) (cdar saved-screen))))

)
