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
; This code is responsible for metering the behavior of futures
; and for the interface to the metering package in general.

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  get-fixed-objects-vector
	  set-fixed-objects-vector!))

(define *meter-list* ())

(define *future-creation-meter* ())
(define *future-start-meter* ())
(define *future-await-meter* ())
(define *future-suspend-meter* ())
(define *future-restart-meter* ())
(define *future-determine-meter* ())
(define *gc-start-meter* ())
(define *gc-finish-meter* ())

(define (lookup-meter name)
  (let ((found (assoc name *meter-list*)))
    (if (null? found)
	(let ((number ((access io-next-hash-number butterfly-io))))
	  (set! *meter-list* (cons (cons name number) *meter-list*))
	  (bfly-send-output-string 5 number (string-append "METER:" name))
	  number)
	(let ((number (cdr found)))
	  (bfly-send-output-string 5 number (string-append "METER:" name))
	  number))))

(define send-metering (make-primitive-procedure 'send-metering))
(define bfio-start-metering (make-primitive-procedure 'bfio-start-metering))
(define bfio-end-metering (make-primitive-procedure 'bfio-end-metering))

(define atomic-add-car! (make-primitive-procedure 'atomic-add-car!))
(define atomic-add-cdr! (make-primitive-procedure 'atomic-add-cdr!))
(define atomic-add-vector! (make-primitive-procedure 'atomic-add-vector!))

(define *future-counter* (cons 'future-number 0))

(define (reset-metering)
  (set! *future-counter* (cons 'future-number 0)))

(define (*future-creation-handler* process name)
  (send-metering *future-creation-meter*
		 (vector (future-ref process FUTURE-METERING-SLOT)
			 (future-ref (current-future) FUTURE-METERING-SLOT))
		 name))

(define (*future-determine-handler* future)
  (if (not (eq? (future-ref future FUTURE-PROCESS-SLOT) 'input-wait))
      (send-metering *future-determine-meter*
		     (future-ref future FUTURE-METERING-SLOT)
		     0)))

(define (*future-start-handler* future)
  (if (not (eq? (future-ref future FUTURE-PROCESS-SLOT) 'input-wait))
      (send-metering *future-start-meter*
		     (future-ref future FUTURE-METERING-SLOT)
		     0)))

(define (*future-restart-handler* started starter)
  (if (not (eq? (future-ref starter FUTURE-PROCESS-SLOT) 'input-wait))
      (send-metering *future-restart-meter*
		     (future-ref started FUTURE-METERING-SLOT)
		     (future-ref starter FUTURE-METERING-SLOT))))

(define (*future-await-handler* waiter awaited)
  (if (not (eq? (future-ref awaited FUTURE-PROCESS-SLOT) 'input-wait))
      (send-metering *future-await-meter*
		     (future-ref waiter FUTURE-METERING-SLOT)
		     (future-ref awaited FUTURE-METERING-SLOT))))

(define (*future-suspend-handler* waiter)
  (send-metering *future-await-meter*
		 (future-ref waiter FUTURE-METERING-SLOT) 0))

(define (*gc-start-handler*)
  (send-metering *gc-start-meter* 0 0))

(define (*gc-finish-handler*)
  (send-metering *gc-finish-meter* 0 0))

(define (metering-on #!optional filename)

  (if (not (unassigned? filename))
      (bfio-start-metering filename))

  (set! *future-creation-meter* (lookup-meter "FUTURE-CREATION"))
  (set! *future-start-meter* (lookup-meter "FUTURE-START"))
  (set! *future-await-meter* (lookup-meter "FUTURE-AWAIT"))
  (set! *future-restart-meter* (lookup-meter "FUTURE-RESTART"))
  (set! *future-determine-meter* (lookup-meter "FUTURE-DETERMINE"))
  (set! *gc-start-meter* (lookup-meter "GC-STARTING"))
  (set! *gc-finish-meter* (lookup-meter "GC-FINISHED"))

  (set-microcode-metering! (vector *future-creation-meter*
				   *future-start-meter*
				   *future-await-meter*
				   *future-restart-meter*
				   *future-determine-meter*))
  
  (set! *future-creation-hook* *future-creation-handler*)
  (set! *future-start-hook* *future-start-handler*)
  (set! *future-determine-hook* *future-determine-handler*)
  (set! *future-restart-hook* *future-restart-handler*)
  (set! *future-await-hook* *future-await-handler*)
  (set! *future-suspend-hook* *future-suspend-handler*)
  (set! *gc-start-hook* *gc-start-handler*)
  (set! *gc-finish-hook* *gc-finish-handler*))

(define (metering-off)

  (bfio-end-metering)

  (set-microcode-metering! #f)

  (set! *future-creation-hook* ())
  (set! *future-start-hook* ())
  (set! *future-determine-hook* ())
  (set! *future-restart-hook* ())
  (set! *future-await-hook* ())
  (set! *future-suspend-hook* ())
  (set! *gc-start-hook* ())
  (set! *gc-finish-hook* ()))

(define (set-microcode-metering! state)
  (let ((fov (get-fixed-objects-vector)))
    (vector-set! fov (fixed-objects-vector-slot 'metering-on) state)
    (set-fixed-objects-vector! fov)))

(define (with-metering file thunk)
  (dynamic-wind
   (lambda () (metering-on file))
   thunk
   (lambda () (metering-off))))

