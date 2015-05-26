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

;;;; Transcripts

(declare (usual-integrations))

(define transcript-default-filename ":OUTPUT.TEXT")
(define transcript-on)
(define transcript-off)
(let ()

(define photo-open
  (make-primitive-procedure 'PHOTO-OPEN))

(define photo-close
  (make-primitive-procedure 'PHOTO-CLOSE))

(set! transcript-on
(named-lambda (transcript-on #!optional filename)
  (if (unassigned? filename)
      (begin (set! filename transcript-default-filename)
	     (newline)
	     (write-string "Transcript file is ")
	     (write filename)))
  (if (not (photo-open filename))
      (error "Transcript file already open: TRANSCRIPT-ON" filename))
  *the-non-printing-object*))

(set! transcript-off
(named-lambda (transcript-off)
  (if (not (photo-close))
      (error "Transcript file already closed: TRANSCRIPT-OFF"))
  *the-non-printing-object*))

)
