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

;;;; Operations on the World

(declare (usual-integrations))

(define %exit
  (let ((primitive (make-primitive-procedure 'EXIT)))
    (named-lambda (exit)
      (close-all-open-files)
      (primitive))))

(define quit
  (let ((primitive (make-primitive-procedure 'HALT)))
    (named-lambda (quit)
      (with-interrupt-mask interrupt-mask-none
	(lambda (interrupt-enables)
	  (primitive)))
      *the-non-printing-object*)))

(define save-world
  (let ((dump-band (make-primitive-procedure 'DUMP-BAND)))
    (named-lambda (save-world filename #!optional after-suspend after-restore)
      (if (unassigned? after-suspend)
	  (set! after-suspend set-interrupt-enables!))
      (if (unassigned? after-restore)
	  (set! after-restore set-interrupt-enables!))
      (define (loop restart)
	(if (not (dump-band restart (canonicalize-output-filename filename)))
	    (begin (error "Band dump failed: (PROCEED 0) to retry")
		   (loop restart))))
      (suspend-world loop after-suspend after-restore))))

(define disk-restore
  (let ((load-band (make-primitive-procedure 'LOAD-BAND))
	(reload-band-name (make-primitive-procedure 'RELOAD-BAND-NAME)))
    (named-lambda (disk-restore #!optional filename)
      (if (unassigned? filename)
	  (set! filename
		(or (reload-band-name)
		    (error "DISK-RESTORE: No default band name available"))))
      (close-all-open-files)
      (load-band (canonicalize-input-filename filename)))))
