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
;;;(declare (usual-integrations)
	 (integrate-primitive-procedures
	  set-fixed-objects-vector! get-fixed-objects-vector))

(define describe-hardware-trap)
(define hardware-stack-trace)
(define code-place->string)
(define get-place-in-code
  (make-primitive-procedure 'get-place-in-code))
(define set-debug-flag!
  (make-primitive-procedure 'set-debug-flag!))

(define hardware-trap-package
  (make-environment

(define get-hardware-trap-info
  (make-primitive-procedure 'get-hardware-trap-info))
(define microcode-tables-filename
  (make-primitive-procedure 'microcode-tables-filename))
(define get-hardware-stack-slot
  (make-primitive-procedure 'get-hardware-stack-slot))

(define saved-hardware-stack '())

(define (hardware-trap-handler ignored)
  (save-hardware-stack)
  (newline)
  (describe-hardware-trap)
  (newline)
  (abort->nearest
   (standard-rep-message "Aborting: The hardware trapped.")))

(define code-messages
  '#(("Unknown code - 0")
     ("Hangup")
     ("Interrupt")
     ("Quit")
     ("Illegal instruction"
      (0 . "Reserved addressing fault")
      (1 . "Privileged instruction fault")
      (2 . "Reserved operand fault"))
     ("Trace trap")
     ("IOT instruction")
     ("EMT instruction")
     ("Floating point exception"
      (#x1c . "TRAPV instruction")
      (#xc0 . "Branch or set unordered")
      (#xc4 . "Floating inexact result")
      (#xc8 . "Floating divide by zero")
      (#xcc . "Floating underflow")
      (#xd0 . "Floating operand error")
      (#xd4 . "Floating overflow")
      (#xd8 . "Floating not-a-number"))
     ("Kill - You are dead")
     ("Bus error"
      (#x18 . "CHK or CHK2 instruction"))
     ("Segmentation violation")
     ("Bad argument to system call")
     ("Write to pipe w/no readers")
     ("Alarm clock")
     ("Termination")
     ("Urgent I/O condition")
     ("Stop signal")
     ("TTY stop signal")
     ("Continue")
     ("Child stopped or exitted")
     ("Background TTY read")
     ("Background TTY write")
     ("I/O possible")
     ("Too much CPU time")
     ("File size exceeded")
     ("Virtual time alarm")
     ("Profile time alarm")
     ("Window size changed")
     ("User signal 1")
     ("User signal 2")))

(define (get-appropriate-message code)
  (let ((split (integer-divide code 65536)))
    (let ((primary (car split))
	  (secondary (cdr split)))
      (if (or (< primary 1) (> primary 32))
	  (string-append "Unknown code - " (number->string code 16))
	  (let ((info (vector-ref code-messages primary)))
	    (let ((best (assq secondary (cdr info))))
	      (if best
		  (cdr best)
		  (car info))))))))

(set! describe-hardware-trap
      (named-lambda (describe-hardware-trap)
	(newline)
	(let ((trap-info (get-hardware-trap-info)))
	  (if (null? trap-info)
	      (display "There was no recent hardware trap")
	      (let ((trap-pc (vector-ref trap-info 0))
		    (trap-irritant (vector-ref trap-info 1))
		    (trap-code (vector-ref trap-info 2))
		    (task-func (vector-ref trap-info 3))
		    (task-name (vector-ref trap-info 4))
		    (task-number (vector-ref trap-info 5)))
		(display (get-appropriate-message trap-code))
		(newline)
		(if task-name
		    (begin
		      (display "    in task: ")
		      (display task-name)
		      (newline)))
		(let ((code-place (get-place-in-code trap-pc)))
		  (display "    at: ")
		  (display (code-place->string code-place))
		  (newline)))))
	(display "Type: (hardware-stack-trace) for an approximate stack trace")
	(newline)))

(define symbol-pathname-loaded '())
(define symbol-table-info '())

(set! code-place->string
      (named-lambda (code-place->string place)
	(cond ((number? place)
	       (number->string place 16))
	      ((and (pair? place) (string? (car place)))
	       (let ((fname
		      (or (file-exists? (->pathname (car place)))
			  (file-exists?
			   (merge-pathnames
			    (pathname-new-directory (->pathname (car place)) ())
			    (->pathname (microcode-tables-filename)))))))
		 (if fname
		     (begin
		       (if (not (equal? path symbol-pathname-loaded))
			   (begin
			     (set! symbol-table-info (vector-ref (fasload (path)) 3))
			     (set! symbol-pathname-loaded path)))
		       (let ((slot (find-symbol-slot (cdr place) symbol-table-info)))
			 (cond ((null? slot)
				(string-append (car place) " + "
					       (number->string (cdr code-place) 16)))
			       (else
				(string-append
				 (vector-ref (vector-ref symbol-table-info slot) 0)
				 " + " (- (cdr place)
					  (vector-ref (vector-ref symbol-table-info
								  slot) 1)))))))
		     (string-append (car place) " + "
				    (number->string (cdr code-place) 16)))))
	      (else
	       (with-output-to-string (lambda () (display place)))))))

(define (find-symbol-slot offset table)
  (let recurse ((lo 0)
		(hi (-1+ (vector-length table))))
    (cond ((< (- hi lo) 2)
	   (cond ((> offset (vector-ref (vector-ref table hi) 1))
		  hi)
		 ((> offset (vector-ref (vector-ref table lo) 1))
		  lo)
		 ((> lo 0)
		  (-1+ lo))
		 (else
		  #f)))
	  (else
	   (let ((mid (floor (/ (+ lo hi) 2))))
	     (let ((value (vector-ref (vector-ref table mid) 1)))
	       (cond ((= offset value)
		      mid)
		     ((< offset value)
		      (recurse lo mid))
		     (else
		      (recurse mid hi)))))))))

(define (terminal-source-name? symbol)
  (let ((name (if (symbol? symbol)
		  (symbol->string symbol)
		  symbol)))
    (cond ((= (string-match-forward name "LABEL-") 6) #f)
	  ((= (string-match-forward name "CONTINUATION-") 13) #f)
	  ((= (string-match-forward name "LET-") 4) #f)
	  ((= (string-match-forward name "QUOTATION-") 10) #f)
	  ((= (string-match-forward name "LAMBDA-") 7) #f)
	  ((= (string-match-forward name "LOOP-") 5) #f)
	  (else #t))))

(define (save-hardware-stack)
  (let loop ((i 0) (result ()))
    (let ((value (get-hardware-stack-slot i)))
      (if (null? value)
	  (set! saved-hardware-stack (list->vector (reverse result)))
	  (loop (1+ i) (cons value result)))))
  'saved)

(set! hardware-stack-trace
      (named-lambda (hardware-stack-trace)
	(if saved-hardware-stack
	    (begin
	      (display "Stack trace:")
	      (newline)
	      (let loop ((slot 0))
		(if (< slot (vector-length saved-hardware-stack))
		    (let ((value (vector-ref saved-hardware-stack slot)))
		      (display "  ")
		      (display slot)
		      (display "  ")
		      (if (number? (car value))
			  (let ((type (microcode-type-name (car value))))
			    (display "<< ")
			    (if type
				(display type)
				(display (car value)))
			    (display " | ")
			    (display (number->string (cdr value) 16))
			    (display " >>") )
			  (display
			   (cdr 
			    (with-output-to-truncated-string 70
			     (lambda () (display (cdr value)))))))
		      (newline)
		      (loop (1+ slot))))))
	    (begin
	      (display "No hardware stack saved")
	      (newline)))))

(let ((fov (get-fixed-objects-vector)))
  (vector-set! fov #x0C hardware-trap-handler)
  (set-fixed-objects-vector! fov))

))

