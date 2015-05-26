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
;;;;	Butterfly I/O System Code
;
;	There are two levels of I/O:
;
;		1) The cold-load stream is what is used by the system before
;		any of the Butterfly stuff starts up and is initially defined
;		by whatever is in standard-input or standard-output.
;
;		2) When Butterfly I/O starts these old values are stashed in
;		cold-load-input and cold-load-output and they are replaced by
;		a pair of streams which know about the tasking system.  When
;		I/O is performed they see if there is a current task and if so
;		they see if it has a local stream identifier, usually just a
;		set of integers.  If it does, then I/O is done using the
;		Butterfly Lisp I/O system.  If it does not or there is no
;		task running I/O is sent to the cold-load stream.
;

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  (channel-buffer system-hunk3-cxr2)
	  (message-tag system-hunk3-cxr0)
	  (message-stream system-hunk3-cxr1)
	  (message-buffer system-hunk3-cxr2)))


(define return-external-message
  (make-primitive-procedure 'return-external-message))
(define tty-read-char
  (make-primitive-procedure 'tty-read-char))


;	The Butterfly I/O Package

(define butterfly-io
  (make-environment

(define get-butterfly-port-number (make-primitive-procedure 'bfio-get-stream-number))
(define io-next-hash-number (make-primitive-procedure 'bfio-next-hash-number))
(define io-task-hash (make-primitive-procedure 'bfio-task-hash))
(define io-task-unhash (make-primitive-procedure 'bfio-task-unhash))
(define bfio-read-string (make-primitive-procedure 'bfio-read-string))
(define bfio-read-char (make-primitive-procedure 'bfio-read-char))
(define bfio-kick-input-server (make-primitive-procedure 'bfio-kick-input-server))


(define butterfly-input-port
  (make-environment
    (define :type input-port-tag)
    (define (:print-self)
      (unparse-with-brackets
      (lambda () (write-string "Butterfly input port"))))
    (define (:close) 'done)
   
    (define character-buffer ())
    (define (:discard-char)
      (set! character-buffer ()))
    
    (define (:peek-char)
      (or character-buffer
	  (begin (set! character-buffer (:read-char))
		 character-buffer)))
    (define (:peek-char-immediate)
      (or character-buffer
	  (begin (set! character-buffer (:read-char-immediate))
		 character-buffer)))
    
    (define (:read-char)
      (if character-buffer
	  (set! character-buffer ())
	  (bfio-read-char #f #f)))
    (define (:read-char-immediate)
      (if character-buffer
	  (set! character-buffer ())
	  (bfio-read-char #f #t)))
   
   (define (:read-start!) 'done)
   (define (:read-finish!) 'done)
   (define (:char-ready?) #t) 

   (define (:read-string delimiters)
    (define (loop)
      (if (char-set-member? delimiters (:peek-char))
	  '()
	  (let ((char (:read-char)))
	    (cons char (loop)))))
    (list->string (loop)))
   (define (:discard-chars delimiters)
     (define (loop)
       (if (not (char-set-member? delimiters (:peek-char)))
	   (begin (:discard-char)
		  (loop))))
     (loop))))

;;;	Handle the buffer arrived from INSER interrupt

(define (buffer-arrived-handler int-code int-enabled)
  (let ((raw-message (return-external-message)))
    (let ((request (message-tag raw-message))
	  (buffer (message-buffer raw-message)))
      (case request
	((#o102)
	 (let* ((number (message-stream raw-message))
		(task (io-task-unhash number))
		(channel
		 (if (future? task)
		     (vector-ref (future-ref task
					     FUTURE-PROCESS-PRIVATE-SLOT) 0)
		     '())))
	   (if channel
	       (determine! (channel-buffer channel) (cons 0 buffer))
	       (using-rep-state
		(current-level-rep-state)
		(lambda ()
		  (error "Message received for unknown stream" number))))))
	((#o103)
	 ((vector-ref (access keyboard-interrupts interrupt-system)
		      buffer)
	  buffer int-enabled))
	(else
	 (using-rep-state
	  (current-level-rep-state)
	  (lambda ()
	    (error "Unknown external message type" request))))))))

(define (install)
  
  (vector-set!
   (vector-ref (get-fixed-objects-vector)
	       (fixed-objects-vector-slot 'system-interrupt-vector))
   (access CHARACTER-SLOT interrupt-system)
   buffer-arrived-handler)
  
  (set! (access :print-self console-input-port)
	(access :print-self butterfly-input-port))
  (set! (access :close console-input-port)
	(access :close butterfly-input-port))
  (set! (access :read-start! console-input-port)
	(access :read-start! butterfly-input-port))
  (set! (access :read-finish! console-input-port)
	(access :read-finish! butterfly-input-port))
  (set! (access :discard-char console-input-port)
	(access :discard-char butterfly-input-port))
  (set! (access :peek-char console-input-port)
	(access :peek-char butterfly-input-port))
  (set! (access :peek-char-immediate console-input-port)
	(access :peek-char-immediate butterfly-input-port))
  (set! (access :read-char console-input-port)
	(access :read-char butterfly-input-port))
  (set! (access :read-char-immediate console-input-port)
	(access :read-char-immediate butterfly-input-port))

  (bfio-kick-input-server)

  )


))	; End of the butterfly-io package.
