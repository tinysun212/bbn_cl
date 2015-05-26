;;; -*-Scheme-*-
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

;;;; Debugger Command Loop Support

(declare (usual-integrations))

(define (make-command-set name)
  (list name))

(define (define-letter-command command-set new-command function help-text)
  (let ((vcell (assv new-command (cdr command-set))))
    (if (null? vcell)
	(let loop ((command-set command-set))
	  (if (or (null? (cdr command-set))
		  (char<? new-command (caadr command-set)))
	      (set-cdr! command-set
			(cons (list new-command function help-text)
			      (cdr command-set)))
	      (loop (cdr command-set))))
	(set-cdr! vcell (list function help-text)))))

(define (letter-commands command-set message prompt)
  (with-standard-proceed-point
   (lambda ()
     (push-command-loop
      message
      (named-lambda (do-letter-command state)
	(let ((char (char-upcase (prompt-for-command-char prompt))))
	  (let ((entry (assv char (cdr command-set))))
	    (if entry
		(let ((value ((cadr entry))))
		  (if (not (eq? value *the-non-printing-object*))
		      (begin (newline) (write value))))))))
      '()))))

(define ((standard-help-command command-set))
  (for-each (lambda (entry)
	      (newline)
	      (write-string "   ")
	      (write-char (car entry))
	      (write-string "   ")
	      (write-string (caddr entry)))
	    (cdr command-set))
  *the-non-printing-object*)

(define (standard-exit-command)
  (proceed))

(define (environment-name environment)
  (lambda-components* (procedure-lambda (environment-procedure environment))
    (lambda (name required optional rest body)
      name)))

(define (prompt-for-command-char/default prompt)
  ((standard-rep-prompt prompt))
  (let ((obj (read)))
    (let ((obj-characters
	   (with-output-to-string (lambda () (write obj)))))
      (string-ref obj-characters 0))))

(define prompt-for-command-char
  prompt-for-command-char/default)

(define (prompt-for-confirmation/default prompt)
  (let loop ()
    (newline)
    (write-string prompt)
    (write-string "(y or n) ")
    (case (read)
      ((Y) true)
      ((N) false)
      (else (loop)))))

(define prompt-for-confirmation
  prompt-for-confirmation/default)

(define (prompt-for-expression/default prompt)
  (newline)
  (write-string prompt)
  (read))

(define prompt-for-expression
  prompt-for-expression/default)

(define (leaving-command-loop/default thunk)
  (thunk))

(define leaving-command-loop
  leaving-command-loop/default)

(define (debug/read-eval-print environment message prompt)
  (leaving-command-loop
   (lambda ()
     (read-eval-print environment message prompt))))

(define (debug/eval expression environment)
  (leaving-command-loop
   (lambda ()
     (eval expression environment))))

(define (debug/where environment)
  (leaving-command-loop
   (lambda ()
     (where environment))))

