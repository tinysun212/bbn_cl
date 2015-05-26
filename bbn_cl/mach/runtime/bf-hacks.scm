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
;;;; -*- Mode: Scheme
;
;	This file contains of a series of hacks which can be
;	used on a variety of parallel machines.

(declare (usual-integrations))

(define make-queue)
(define enqueue-head!)
(define enqueue-tail!)
(define dequeue-head!)
(define dequeue-tail!)
(define queue-length)
(define queue-contents)

(define hunk3-cons (make-primitive-procedure 'hunk3-cons))
(define hunk3-cxr (make-primitive-procedure 'hunk3-cxr))
(define hunk3-set-cxr! (make-primitive-procedure 'hunk3-set-cxr!))

(let ((prim-enqueue-head (make-primitive-procedure 'enqueue-head!))
      (prim-enqueue-tail (make-primitive-procedure 'enqueue-tail!))
      (prim-dequeue-head (make-primitive-procedure 'dequeue-head!))
      (prim-dequeue-tail (make-primitive-procedure 'dequeue-tail!)))

  (set! make-queue
	(named-lambda (make-queue)
	  (hunk3-cons 0 '() '())))

  (set! enqueue-head!
	(named-lambda (enqueue-head! queue item)
	  (prim-enqueue-head queue (cons item '()))))

  (set! enqueue-tail!
	(named-lambda (enqueue-tail! queue item)
	  (prim-enqueue-tail queue (cons item '()))))

  (set! dequeue-head!
	(named-lambda (dequeue-head! queue)
	  (let ((item (prim-dequeue-head queue)))
	    (if (pair? item)
		(car item)
		item))))

  (set! dequeue-tail!
	(named-lambda (dequeue-tail! queue)
	  (let ((item (prim-dequeue-tail queue)))
	    (if (pair? item)
		(car item)
		item))))

  (set! queue-length (make-primitive-procedure 'system-hunk3-cxr0))
  (set! queue-contents (make-primitive-procedure 'system-hunk3-cxr1))
  )


;	The locking facility is tied in with the future mechanism
;	but does NOT use ordinary future waits.  It uses explicit
;	calls to the scheduler.
;
;	A lock is a cxr which has the following fields:
;		0:	State		'LOCKED, 'UNLOCKED, 'ATOMIC
;		1:	Waiters		A queue of waiting futures.
;		2:	Number		A unique lock number for serialization.

(define set-cxr-if-eq?! (make-primitive-procedure 'set-cxr-if-eq?!))

(define make-lock)
(define lock-lock!)
(define lock-multiple!)
(define unlock-lock!)
(define unlock-multiple!)

(define atomic-add-car! (make-primitive-procedure 'atomic-add-car!))
(define atomic-add-cdr! (make-primitive-procedure 'atomic-add-cdr!))
(define atomic-add-vector! (make-primitive-procedure 'atomic-add-vector!))

(let ((lock-counter (cons 0 '())))

  (set! make-lock
	(named-lambda (make-lock)
	  (hunk3-cons 'UNLOCKED (make-queue) (atomic-add-car! lock-counter 1))))

  (define set-lock-state! (make-primitive-procedure 'system-hunk3-set-cxr0!))
  (define lock-waiters (make-primitive-procedure 'system-hunk3-cxr1))
  (define lock-number (make-primitive-procedure 'system-hunk3-cxr2))
  (define lock-state (make-primitive-procedure 'system-hunk3-cxr0))

  (set! lock-lock!
	(named-lambda (lock-lock! lock)
	  (if (not (set-cxr-if-eq?! lock 0 'locked 'unlocked))
	      (if (set-cxr-if-eq?! lock 0 'atomic 'locked)
		  (suspending-execution
		   (lambda (self)
		     (enqueue-tail! (lock-waiters lock) self)
		     (set-lock-state! lock 'locked)))
		  (lock-lock! lock)))))

  (set! lock-multiple!
	(named-lambda (lock-multiple! . locks)
	  (let ((sorted-locks
		 (sort locks (lambda (a b) (< (lock-number a) (lock-number b))))))
	    (let locking-loop ((locks sorted-locks))
	      (if (null? locks)
		  sorted-locks
		  (begin
		    (lock-lock! (car locks))
		    (locking-loop (cdr locks))))))))

  (set! unlock-lock!
	(named-lambda (unlock-lock! lock)
	  (if (set-cxr-if-eq?! lock 0 'atomic 'locked)
	      (let ((waiter (dequeue-head! (lock-waiters lock))))
		(if (future? waiter)
		    (begin
		      (set-lock-state! lock 'locked)
		      (reschedule-future waiter))
		    (set-lock-state! lock 'unlocked)))
	      (if (eq? (hunk3-cxr lock 0) 'unlocked)
		  (error "Unlocking an unlocked lock!" lock)
		  (unlock-lock! lock)))))

  (set! unlock-multiple!
	(named-lambda (unlock-multiple! sorted-locks)
	  (let unlocking-loop ((locks sorted-locks))
	    (if (null? locks)
		'unlocked
		(begin
		  (unlock-lock! (car locks))
		  (unlocking-loop (cdr locks)))))))
  )

(define (print-lock lock)
  (print (list 'STATE (hunk3-cxr lock 0)))
  (print (list 'NUMBER (hunk3-cxr lock 2)))
  (print-queue (hunk3-cxr lock 1))
  lock)

(define (print-queue queue)
  (print (list 'LENGTH (hunk3-cxr queue 0)))
  (print 'CONTENTS)
  (mapcar print-future (hunk3-cxr queue 1))
  queue)

(define (print-future future)
  (print (list 'FUTURE-NAME (future->string future)))
  (print (list 'FUTURE-DETERMINED (future-ref future FUTURE-DETERMINED-SLOT)))
  (print (list 'FUTURE-LOCK (future-ref future FUTURE-LOCK-SLOT)))
  (print (list 'FUTURE-VALUE/QUEUE (future-ref future FUTURE-VALUE-SLOT)))
  (print (list 'FUTURE-STATUS (future-ref future FUTURE-STATUS-SLOT)))
  (print (list 'FUTURE-METERING (future-ref future FUTURE-METERING-SLOT)))
  future)

(define (future->string future)
  (if (future? future)
      (let ((private (future-ref future FUTURE-PROCESS-PRIVATE-SLOT)))
	(if (vector? private)
	    (let ((channel (vector-ref private 0)))
	      (if channel
		  (hunk3-cxr channel 1)
		  "#[UNNAMED]"))
	    private))
      "#[NOT A FUTURE]"))

(define (set-queue-overflow-mode! error-p)
  (vector-set! (vector-ref (get-fixed-objects-vector) #x2b) 2 error-p))

(define set-queue-overflow-level!
  (if is-a-butterfly?
      (make-primitive-procedure 'set-queue-overflow-level!)
      (lambda (ignored) 'ignored)))

(define n-idle-processors
  (if is-a-butterfly?
      (make-primitive-procedure 'n-idle-processors)
      (lambda () 0)))

(define work-queue-length
  (if is-a-butterfly?
      (make-primitive-procedure 'work-queue-length)
      (lambda ()
	(let ((the-queue (vector-ref (get-fixed-objects-vector) #x19)))
	  (if the-queue
	      (system-list-length (car the-queue))
	      0)))))

(define work-queue-sizes
  (if is-a-butterfly?
      (make-primitive-procedure 'work-queue-sizes)
      (write "This is not supported")))

(define (system-list-length list)
  (let length-loop ((this list) (len 0))
    (if (system-pair? this)
	(length-loop (system-pair-cdr this) (1+ len))
	len)))

(define (reschedule-self)
  (suspending-execution reschedule-future))

(define (wait-until-system-idle)
  (if (and (= (work-queue-length) 0)
	   (= (n-idle-processors) (-1+ (n-interpreters))))
      'idle
      (begin
	(reschedule-self)
	(wait-until-system-idle))))
