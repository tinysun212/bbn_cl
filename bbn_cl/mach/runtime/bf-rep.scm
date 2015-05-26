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
;;;;;;; Patches to standard distribution file REP.SCM.341

;;; Modification added: WITH-PROCEED-POINT pauses all other processes.

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  translate-to-state-point first car))

(define debug-rep? #!false)		; Debug Flag
(define rep-output-window #f)
(define rep-future-id #f)

(define-macro (atomic . expressions)
  `(WITHOUT-INTERRUPTS
    (LAMBDA () . ,expressions)))


;;; The rep-futures data structure records information about each rep level.

;;; The structure is grown as new levels are pushed and is explicitly pruned
;;; when a level is popped.  In particular, the newest and highest numbered
;;; levels are put at the start of the list and the last element of the
;;; list is associated with level zero, the top level.

;;; The global variable rep-futures contains a lock cell (for spin locking)
;;; and a mutatable cell which contains an association list which associates
;;; each level with a FUTURE and a FLUID BINDINGS set (which may or may not be
;;; accessible from the FUTURE's FUTURE-PROCESS-SLOT).

(define get-fluid-bindings (make-primitive-procedure 'get-fluid-bindings))
(define set-fluid-bindings! (make-primitive-procedure 'set-fluid-bindings!))
(define rep-futures (cons 'UNLOCKED '()))
(define using-rep-state)
(define clear-rep-future-list!)
(define with-rep-lock)
(define top-level-process)
(define current-level-process)
(define current-level-rep-state)
(define previous-level-process)
(define push-new-rep-level!)
(define pop-old-rep-level!)

(define bf-rep-package
  (make-environment

    (define rep-info-level first)	; The level number.
    (define rep-info-process second)	; The task to execute in (if possible).
    (define rep-info-fluids third)	; The fluid bindings needed.
    
    (set! push-new-rep-level!
	  (named-lambda (push-new-rep-level! level)
	    (with-rep-lock
	     (lambda ()
	       (let ((old-slot (assq level (cdr rep-futures))))
		 (if old-slot
		     (sequence
		       (set-car! (cdr old-slot) (current-future))
		       (set-car! (cddr old-slot) (get-fluid-bindings)))
		     (let ((result (list level (current-future)
					 (get-fluid-bindings))))
		       (set-cdr! rep-futures (cons result (cdr rep-futures)))
		       result)))))))

    (set! pop-old-rep-level!
	  (named-lambda (pop-old-rep-level! level)
	    (with-rep-lock
	     (lambda ()
	       (let pop-old-level ((rep-list (cdr rep-futures)))
		 (if (and rep-list
			  (>= (rep-info-level (car rep-list)) level))
		     (pop-old-level (cdr rep-list))
		     (set-cdr! rep-futures rep-list)))))))

    (set! clear-rep-future-list!
	  (named-lambda (clear-rep-future-list!)
	    (set-cdr! rep-futures '())
	    'CLEARED))
    
    (set! with-rep-lock			
	  (named-lambda (with-rep-lock thunk)
	    (atomic
	     (if (set-car-if-eq?! rep-futures 'LOCKED 'UNLOCKED)
		 (let ((result (thunk)))
		   (set-car! rep-futures 'UNLOCKED)
		   result)
		 (with-rep-lock thunk)))))
    
    (set! top-level-process
	  (named-lambda (top-level-process)
	    (with-rep-lock
	     (lambda ()
	       (let ((0-level (assq 0 (cdr rep-futures))))
		 (if 0-level
		     (rep-info-process 0-level)
		     '()))))))

    (set! current-level-rep-state
	  (named-lambda (current-level-rep-state)
	    (with-rep-lock
	     (lambda ()
	       (if (cdr rep-futures)
		   (rep-info-fluids (cadr rep-futures)) ; Highest level
		   '())))))
	    
    (set! current-level-process
	  (named-lambda (current-level-process)
	    (with-rep-lock
	     (lambda ()
	       (if (cdr rep-futures)
		   (rep-info-process (cadr rep-futures)) ; Highest level
		   '())))))

    (set! previous-level-process
	  (named-lambda (previous-level-process)
	    (with-rep-lock
	     (lambda ()
		 (if (cdr rep-futures)
		     (if (cddr rep-futures)
			 (rep-info-process (caddr rep-futures))
			 (rep-info-process (cadr rep-futures)))
		     '())))))
))

;;; Patch the REP code to support parallelism.

(in-package (procedure-environment abort->previous)

;;; This is different from the serial version in that:
;;;	- It calls discard-recently-suspended-tasks! and
;;;		prevent-discarding-processes!
;;;	- It forces the requisite state point translation before
;;;		running the continuation.

  (set! abort->previous
	(named-lambda (abort->previous message)
	  (let ((to-where (if (null? previous-driver-hook)
			      nearest-driver-hook
			      previous-driver-hook)))
	    (discard-recently-suspended-tasks!)
	    (within-continuation
	     to-where
	     (lambda ()
	       (translate-to-state-point (continuation-dynamic-state to-where))
	       (prevent-discarding-processes!)
	       message)))))

;;; This is different from the serial version in that:
;;;	- It calls clear-rep-future-list! and discard-recently-suspended-tasks!.
;;;	- It forces the requisite state translation before calling
;;;		the top level driver hook.
;;;	- On a Butterfly it restarts the zap-stream task.

  (set! abort->top-level
    (named-lambda (abort->top-level message)
      (clear-rep-future-list!)
      (discard-recently-suspended-tasks!)
      (within-continuation top-level-driver-hook
	 (lambda ()
	   (translate-to-state-point (continuation-dynamic-state
				      top-level-driver-hook))
	   (prevent-discarding-processes!)
	   (if is-a-butterfly?
	       (begin
		 (restart-zap-stream)))
	   message))))

  ;; Push-Command-Hook allows you to customize the read-eval-print loop behavior.
  ;; In particular, it is called when the loop is CREATED and is
  ;; passed the same arguments which are supplied to PUSH-COMMAND-LOOP.

  (set! push-command-hook

  ;; Useful Subroutines

    (let ((set-current-future! (access set-current-future! scheduler)))

  ;; Now To Push The Command Loop

      (named-lambda (push-command-hook start-hook driver
				       initial-state continuation)
	(if (not (futures-on?))
	    (continuation start-hook driver initial-state (lambda () '()))

	    (let ((original-level (-1+ current-level)))
					; We subtract one since current-level
					; has already been incremented by the
					; time the hook is called.

	      (if (not (future? (current-future)))
		  (set-current-future!
		   (make-future 'BREAKPOINT 'BREAKPOINT "Breakpoint Handler")))
	      
	      (let ((old-future-id rep-future-id)
		    (old-output-window rep-output-window))

		(dynamic-wind
	       
		 (lambda ()		; On The Way IN
		   (if (futures-on?)
		       (set! rep-future-id 
			     (future-ref (current-future) future-metering-slot)))
		   (if (and is-a-butterfly? butterfly-io-active?)
		       (set! rep-output-window (current-window->output-port)))
		   (push-new-rep-level! original-level))
	       
		 (lambda ()		; The Actual Command Loop
		   (continuation start-hook driver initial-state
				 (lambda () '())))
	       
		 (lambda ()		; On The Way OUT
		   (set! rep-future-id old-future-id)
		   (set! rep-output-window old-output-window)
		   (pop-old-rep-level! original-level)))))))))

  (set! using-rep-state
	(named-lambda (using-rep-state rep-state thunk)
	  (let ((old-fluids rep-state)
		(tag '(NO REP STATE))
		(my-fluids (get-fluid-bindings)))
	    (set-fluid-bindings! old-fluids)
	    (let ((rbe *rep-base-environment*)
		  (rce *rep-current-environment*)
		  (rbst *rep-base-syntax-table*)
		  (rcst *rep-current-syntax-table*)
		  (rbp *rep-base-prompt*)
		  (rcp *rep-current-prompt*)
		  (rbip *rep-base-input-port*)
		  (rcip *rep-current-input-port*)
		  (rbop *rep-base-output-port*)
		  (rcop *rep-current-output-port*)
		  (rkm *rep-keyboard-map*)
		  (reh *rep-error-hook*)
		  (cl current-level)
		  (ndh nearest-driver-hook)
		  (pdh (if (unassigned? previous-driver-hook)
			   tag previous-driver-hook))
		  (cs (if (unassigned? current-state)
			  tag
			  current-state)))
	      (set-fluid-bindings! my-fluids)
	      (fluid-let ((*rep-base-environment* rbe)
			  (*rep-current-environment* rce)
			  (*rep-base-syntax-table* rbst)
			  (*rep-current-syntax-table* rcst)
			  (*rep-base-prompt* rbp)
			  (*rep-current-prompt* rcp)
			  (*rep-base-input-port* rbip)
			  (*rep-current-input-port* rcip)
			  (*rep-base-output-port* rbop)
			  (*rep-current-output-port* rcop)
			  (*rep-keyboard-map* rkm)
			  (*rep-error-hook* reh)
			  (current-level cl)
			  (previous-driver-hook pdh)
			  (nearest-driver-hook ndh)
			  (current-state cs))
		(if (eq? current-state tag)
		    (set! current-state))
		(if (eq? previous-driver-hook tag)
		    (set! previous-driver-hook))
		(thunk))))))

) ; in-package


