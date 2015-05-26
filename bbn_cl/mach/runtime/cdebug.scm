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
;;;;;; -*-Scheme-*-

(declare (usual-integrations))

(define compiled-stack-tag '(*compiled-stack*))

((access add-unparser-special-object! unparser-package)
 compiled-stack-tag
 (lambda (vec)
   (write-char #\#)
   (write-char #\[)
   (write-string "COMPILED-CODE ")
   (write-string (number->string (primitive-datum vec) 16))
   (write-char #\space)
   (write-string (number->string (vector-length vec)))
   (write-char #\])))

(in-package continuation-package

  (define-standard-parser 'REENTER-COMPILED-CODE
    (lambda (stack history cont)
      (cont (list->vector
	     (cons compiled-stack-tag
		   (stack-list (stack-tail stack 1)
			       (stack-ref stack 0))))
	    undefined-environment
	    (1+ (stack-ref stack 0))		;offset
	    )))
)					; End CONTINUATION-PACKAGE

(define compiled-debug-package
  (make-environment

    (define command-set (make-command-set 'compiled-debug-commands))

    (define (define-debug-command letter function help-text)
      (define-letter-command command-set letter function help-text))

    (define-debug-command #\? (standard-help-command command-set)
      "Help, list command letters")

    (define-debug-command #\Q standard-exit-command "Quit (exit DEBUG)")

    (define-debug-command #\B
      (named-lambda (backwards-frame)
	(if (> current-frame 0)
	    (begin
	      (set! current-frame (-1+ current-frame))
	      (print-current-frame))
	    *the-non-printing-object*))
      "Go back one frame")

    (define-debug-command #\F
      (named-lambda (forwards-frame)
	(if (< current-frame (-1+ (vector-length frame-vector)))
	    (begin
	      (set! current-frame (1+ current-frame))
	      (print-current-frame))
	    *the-non-printing-object*))
      "Go forward one frame")

    (define frame-vector)
    (define current-frame)

    (set! (access debug-compiled-continuation debug-package)
	  (named-lambda (debug-compiled-continuation cont)
	    (if (and (continuation? cont)
		     (setup-compiled-debug cont))
		(letter-commands
		   command-set
		   (lambda ()
		     ((standard-rep-message "Compiled Code Debugger")))
		   "CDB -->")
		(begin
		  (newline)
		  (display "Not a compiled stack frame")
		  (newline)))))
	    
    (define (setup-compiled-debug cont)
      (let ((stack (continuation-expression cont)))
	(if (and (vector? stack)
		 (eq? (vector-ref stack 0) compiled-stack-tag))
	    (begin
	      (set! frame-vector
		    (list->vector (reverse (parse-compiled-stuff stack))))
	      (newline)
	      (newline)
	      (display (vector-length frame-vector))
	      (display " Frames")
	      (newline)
	      (set! current-frame (-1+ (vector-length frame-vector)))
	      (print-current-frame)
	      #t)
	    #f)))

    (define (parse-compiled-stuff stuff)
      (let loop ((index (-1+ (vector-length stuff)))
		 (last (-1+ (vector-length stuff)))
		 (result '()))
	(cond ((= index 1)
	       (if (= index last)
		   result
		   (cons (subvector->list stuff index last)
			 result)))
	      ((primitive-type? #x28 (vector-ref stuff index))
	       (loop (-1+ index)
		     (-1+ index)
		     (cons (subvector->list stuff index last)
			   result)))
	      (else
	       (loop (-1+ index)
		     last
		     result)))))

    (define (print-current-frame)
      (newline)
      (if (and (>= current-frame 0)
	       (< current-frame (vector-length frame-vector)))
	  (let ((frame (vector-ref frame-vector current-frame)))
	    (if (null? frame)
		(begin
		  (display "*** THE RETURN FRAME ***")
		  (newline))
		(for-each
		 (lambda (x)
		   (display "    ")
		   (display-compiled x)
		   (newline))
		 frame)))
	  (begin
	    (display "*** NO STACK FRAME ***")
	    (newline))))


    (define get-place-in-code
      (make-primitive-procedure 'get-place-in-code))

    (define (display-compiled x)
      (cond ((primitive-type? #x28 x)
	     (let ((place (get-place-in-code x)))
	       (if (pair? place)
		   (begin
		     (display "In file ")
		     (display (car place))
		     (display " at ")
		     (display (number->string (cdr place) 16)))
		   (begin
		     (display "Not in compiled code at ")
		     (display (number->string place 16))))))
	    (else
	     (display x))))

    (define (print-stack-summary)
      (newline)
      (let loop ((index (-1+ (vector-length frame-vector))))
	(cond ((< index 0)
	       *the-non-printing-object*)
	      ((null? (vector-ref frame-vector index))
	       (display "*** THE RETURN FRAME ***")
	       (newline))
	      ((primitive-type? #x28 (car (vector-ref frame-vector index)))
	       (display-compiled (car (vector-ref frame-vector index)))
	       (newline)
	       (loop (-1+ index)))
	      (else
	       (display
		(cdr
		 (with-output-to-truncated-string
		  75
		  (lambda ()
		    (display-compiled (vector-ref frame-vector index))))))
	       (newline)
	       (loop (-1+ index))))))


    (define-debug-command #\H print-stack-summary
      "Print the stack briefly")

    ))					; End COMPILED-DEBUG-PACKAGE
