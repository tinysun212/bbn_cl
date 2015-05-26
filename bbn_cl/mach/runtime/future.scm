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
;;;; This is -*- SCHEME -*- code

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  (weak-car system-pair-car)
	  (weak-cdr system-pair-cdr)
	  (weak-set-car! system-pair-set-car!)
	  (weak-set-cdr! system-pair-set-cdr!)
	  set-fixed-objects-vector!
	  vector-set!
	  get-work
	  within-control-point
          future-ref
          future-set!))

; Some useful macros for dealing with atomicity.  Notice that
;;DEFINE-MACRO happens when the text is turned into code (i.e.
;;at syntax time), while ADD-SYNTAX! happens only when the program
;;is actually executed.  So both are used when this file uses the
;;macro, but only ADD-SYNTAX! is used for user macros which
;;are not referenced here.

(define-macro (add-syntax! name expander)
  `(SYNTAX-TABLE-DEFINE SYSTEM-GLOBAL-SYNTAX-TABLE ,name ,expander))

; atomic takes a list of expression and guarantees that they
;;are done without interrupts.

(define-macro (atomic . expressions)
  `(WITHOUT-INTERRUPTS
    (LAMBDA () . ,expressions)))

(add-syntax! 'atomic
  (macro expressions
  `(WITHOUT-INTERRUPTS
    (LAMBDA () . ,expressions))))

; define-atomic is like the procedural version of DEFINE, except
;;that the body is wrapped in WITHOUT-INTERRUPTS.

(define-macro (define-atomic arg-template . body)
  `(DEFINE ,arg-template (atomic . ,body)))

(add-syntax! 'define-atomic
  (macro (arg-template . body)
    `(DEFINE ,arg-template (atomic . ,body))))

; LOCKING-FUTURE is the same as atomic except that it also wraps
;;a LOCK-FUTURE! and UNLOCK-FUTURE! around the expression(s).
;;LOCKED? is a flag which can be used in BODY -- it will be #t
;;if the future is still valid (you hang until you can lock it),
;;or #f if it has been spliced out.

(define-macro (LOCKING-FUTURE FUTURE LOCKED? . BODY)
  `(WITH-FUTURE-LOCKED ,future
      (LAMBDA (,locked?) . ,body)))

(add-syntax! 'LOCKING-FUTURE
  (macro (FUTURE LOCKED? . BODY)
    `(WITH-FUTURE-LOCKED ,future
       (LAMBDA (,locked?) . ,body))))

(define-macro (WITH-STATE STATE . BODY)
  `(NON-REENTRANT-TASK-CATCH (LAMBDA (,state) . ,body)))

(define (with-future-locked future thunk)
  (with-interrupt-mask INTERRUPT-MASK-GC-OK
    (lambda (old-mask)
      (if (lock-future! future)
	  (let ((result (thunk #t)))
	    (unlock-future! future)
	    result)
	  (thunk #f)))))

(define %await-future (make-primitive-procedure '%await-future))
(define %reschedule (make-primitive-procedure '%reschedule))
(define %spawn-process (make-primitive-procedure '%spawn-process))
(define %change-priority (make-primitive-procedure '%change-priority))
(define %inherit-priority-up (make-primitive-procedure '%inherit-priority-up))
(define %inherit-priority-down (make-primitive-procedure '%inherit-priority-down))


(define (inherit-priority-up future priority)
  (if (lock-future! future)
      (%inherit-priority-up future priority)))

(define (inherit-priority-down future priority)
  (if (lock-future! future)
      (%inherit-priority-down future priority)))

(define (change-priority future priority) 
  (if (lock-future! future)
      (%change-priority future priority)))

(define debug-paused-tasks)
(define timer-interrupt)		; We must define this here!
(define put-work (make-primitive-procedure 'PUT-WORK))
(define global-interrupt (make-primitive-procedure 'GLOBAL-INTERRUPT))

(define touch (make-primitive-procedure 'TOUCH))
(define set-car-if-eq?! (make-primitive-procedure 'SET-CAR-IF-EQ?!))
(define set-cdr-if-eq?! (make-primitive-procedure 'SET-CDR-IF-EQ?!))
(define vector-set-if-eq?! (make-primitive-procedure 'VECTOR-SET-IF-EQ?!))
(define set-cxr-if-eq?! (make-primitive-procedure 'SET-CXR-IF-EQ?!))

(define future-ref (make-primitive-procedure 'FUTURE-REF))
(define future-set! (make-primitive-procedure 'FUTURE-SET!))
(define future-size (make-primitive-procedure 'FUTURE-SIZE))
(define lock-future! (make-primitive-procedure 'LOCK-FUTURE!))
(define unlock-future! (make-primitive-procedure 'UNLOCK-FUTURE!))

(define non-touching-eq? (make-primitive-procedure 'NON-TOUCHING-EQ?))

(define n-interpreters (make-primitive-procedure 'N-INTERPRETERS))
(define my-processor-number (make-primitive-procedure 'MY-PROCESSOR-NUMBER))
(define my-interpreter-number
  (make-primitive-procedure 'MY-INTERPRETER-NUMBER))
(define make-future (make-primitive-procedure 'MAKE-CHEAP-FUTURE))
(define spawn-process)
(define suspending-execution)
(define reschedule-future)

;; Slots in a future

;; MICROCODE KNOWS ABOUT THESE

;; FUTURE-DETERMINED-SLOT is #!TRUE if the value is known and immutable
;; #!FALSE if not yet know, else known but mutable (i.e. KEEP-SLOT) 
(define FUTURE-DETERMINED-SLOT 0)

;; FUTURE-LOCK-SLOT is #!TRUE if the future is locked by a process
(define FUTURE-LOCK-SLOT 1)

;; The next two are mutually exclusive.  The VALUE is used if
;; DETERMINED is not #!FALSE. The QUEUE contains a WEAK queue of
;; processes waiting for a value to appear if DETERMINED is #!FALSE.
(define FUTURE-VALUE-SLOT 2)
(define FUTURE-QUEUE-SLOT 2)

; REFERENCED ONLY BY THE RUNTIME SYSTEM

;; Code to run to re-activate this process
(define FUTURE-PROCESS-SLOT 3)

;; The FUTURE-STATUS-SLOT contains one of:
;;   RUNNING:    Actually in possession of a processor
;;   WAITING:    Stopped waiting for the value of a future
;;   PAUSED:     Stopped by PAUSE-EVERYTHING
;;   DELAYED:    Created by delay scheduler and not yet run
;;   RUNNABLE:   Available for execution
;;   DETERMINED: Value has been set and process is finished
;;   CREATED:    Future newly created
(define FUTURE-STATUS-SLOT 4)

;; For debugging purposes, the original thunk to be executed.
(define FUTURE-ORIG-CODE-SLOT 5)

(define FUTURE-PROCESS-PRIVATE-SLOT 6)

;; If this process has status WAITING this is a (strong) list of the
;; futures on which it is waiting.
(define FUTURE-WAITING-ON-SLOT 7)

;; Used by the external metering system - a systemwide unique process number.
(define FUTURE-METERING-SLOT 8)

;; Contains the number of subtasks spawned by this future.
(define FUTURE-SUBTASK-COUNT-SLOT 9)

;; Contains a pointer to the spawn tree list structure.
(define FUTURE-SPAWN-TREE-SLOT 10)

;; For users:
(define FUTURE-USER-SLOT 11)

;; For priority queues
(define FUTURE-PRIORITY-SLOT 12)

;; Metering Hooks - Called if not NIL

(define *future-creation-hook* ())
(define *future-start-hook* ())
(define *future-await-hook* ())
(define *future-restart-hook* ())
(define *future-determine-hook* ())
(define *future-suspend-hook* ())

(define (futures-on #!optional slice)
  (if (unassigned? slice) (set! slice '()))
  (initialize-scheduler! slice))

(define (non-touching-memq element list)
  (cond ((null? list) #f)
	((non-touching-eq? element (car list)) list)
	(else (non-touching-memq element (cdr list)))))

(define (non-touching-assq element list)
  (cond ((null? list) #f)
	((non-touching-eq? element (caar list)) (car list))
	(else (non-touching-assq element (cdr list)))))

(add-syntax!
 'FUTURE
 (macro (expression #!optional doc priority)
   (let ((the-doc) 
	 (this-priority))
     (cond ((unassigned? doc)
	    (set! the-doc (with-output-to-string
			    (lambda () (display expression))))
	    (set! this-priority (future-ref (current-future) FUTURE-PRIORITY-SLOT)))
	   ((and (unassigned? priority) (number? doc))
	    (set! this-priority doc)
	    (set! the-doc (with-output-to-string
			    (lambda () (display expression)))))
	   ((unassigned? priority)
	    (set! this-priority  (future-ref (current-future) FUTURE-PRIORITY-SLOT))
	    (set! the-doc doc))
	   (else (set! this-priority priority)
		 (set! the-doc doc)))
     `(%spawn-process (lambda () ,expression) ,the-doc #t #| ,this-priority |# ))))

(define scheduler
  (make-environment

    (define sti
      (make-primitive-procedure 'SETUP-TIMER-INTERRUPT #t))
    (define drain-work-queue!
      (make-primitive-procedure 'DRAIN-WORK-QUEUE!))
    (define set-fluid-bindings!
     (make-primitive-procedure 'SET-FLUID-BINDINGS!))
    (define non-reentrant-call/cc
      (make-primitive-procedure 'NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION))
    (define call/cc
      (make-primitive-procedure 'CALL-WITH-CURRENT-CONTINUATION))
    (define set-current-dynamic-state!
      (make-primitive-procedure 'SET-CURRENT-DYNAMIC-STATE!))
    (define current-future
      (make-primitive-procedure 'CURRENT-FUTURE))
    (define set-current-future!
      (make-primitive-procedure 'SET-CURRENT-FUTURE!))

    (define control-point-type (microcode-type 'CONTROL-POINT))

    (define catch-maker (access catch-maker continuation-package))
    (define non-reentrant-task-catch)
    (define task-catch)

    (define current-future-vector)	; Process currently running
    (define Idle-Future)		; Future to wait until idle on
    (define discard-the-paused-tasks? #f); Throw away tasks?

    (define scheduler-info (vector '() #f))	; Scheduler info shared w/microcode
    (define schinfo-delta 0)

(set! non-reentrant-task-catch
      (catch-maker non-reentrant-call/cc set-current-dynamic-state! #t))

(set! task-catch
      (catch-maker call/cc set-current-dynamic-state! #f))

(define (legitimate-process? object)
  (or (procedure? object) (primitive-type? control-point-type object)))
    

(define start-preempting
  (make-primitive-procedure '%Start-Preempting))

(define stop-preempting
  (make-primitive-procedure '%Stop-Preempting))

(define (end-preempting)
  (stop-preempting)
  (vector-set! scheduler-info schinfo-delta '()))


(define (more-work work)
  (future-set! work FUTURE-STATUS-SLOT 'RUNNABLE)
  (put-work work))

(set! reschedule-future
      (named-lambda (reschedule-future work)
	(if *future-restart-hook*
	    (*future-restart-hook* work (current-future)))
	(more-work work)))

(define primitive-determine! (make-primitive-procedure '%determine!))

(define (determine! fut val #!optional keep?)
  (if (unassigned? keep?)
      (primitive-determine! fut val #f)
      (primitive-determine! fut val keep?)))

(define (Futures-On?) (not (unassigned? Current-Future-Vector)))

(define (Futures-Off)
  (pause-everything)
  (set! Current-Future-Vector)
  'FUTURES-TURNED-OFF)

(define (initialize-scheduler!
	 #!optional interval non-aborting?)
  
  (pause-everything)		; Stop all processors & drain queue
  
  (new-current-future-vector 'NO-FUTURE-YET)
  
  (set! idle-routine
	(if is-a-butterfly? 
	    (lambda args
	      (display "Idle-routine called!")
	      (new-line)
	      (apply bfly-idle-routine args))
	    (lambda args
	      (display "Idle-routine called!")
	      (new-line)
	      (apply default-idle-routine args))))
  
  (let ((fov (get-fixed-objects-vector)))
    (vector-set! fov #x15 %await-future)
    (vector-set! fov #x24 (vector 'runnable 'created 'determined
				  'waiting 'your-turn
				  'waiting-for-work 'running
				  'delayed 'paused))
    (vector-set! fov #x26 %reschedule)
    (vector-set! fov #x27 idle-routine)
    (set! scheduler-info (vector '() #f))
    (vector-set! fov #x2b scheduler-info)
    (set-fixed-objects-vector! fov))
  
  (Set-Current-Future!
   (make-future 'INITIAL-PROCESS 'INITIAL-PROCESS "The Initial Process"))
  (future-set! (current-future) FUTURE-STATUS-SLOT 'RUNNING)
  
  (set! timer-interrupt		; Set up the timer interrupt handler.
    (lambda ()
      (if (not (vector-ref scheduler-info schinfo-delta))
	  (begin (stop-preempting)
		 (bkpt "TIMER: Illegal entry"))
	  (saving-state (lambda () 'nothing)))))
  
  (let ((termination-handlers
	 (vector-ref (get-fixed-objects-vector)
		     (fixed-objects-vector-slot
		      'MICROCODE-TERMINATIONS-PROCEDURES))))
    (if (= (vector-length termination-handlers) 0)
	(begin
	  (set! termination-handlers 
                (vector-cons number-of-microcode-terminations '()))
	  (vector-set! (get-fixed-objects-vector)
		       (fixed-objects-vector-slot
			'MICROCODE-TERMINATIONS-PROCEDURES)
		       termination-handlers)
	  (set-fixed-objects-vector! (get-fixed-objects-vector))))
    (vector-set! termination-handlers
		 (microcode-termination 'END-OF-CONTINUATION)
		 end-of-computation-handler))
  (set! Idle-Future (make-future 'NO-PROCESS 'NO-PROCESS "Idle-Loop"))
  
  (if (not (unassigned? interval))
      (begin
	(vector-set! scheduler-info schinfo-delta interval)
	(start-preempting))
      (vector-set! scheduler-info schinfo-delta '()))
  (global-interrupt
   1
   (lambda (IntCode IntEnb)
     (next))
   (lambda () #t))
  (if (or (unbound? abort-to-top-level-driver)
	  (and (not (unassigned? non-aborting?))
	       non-aborting?))
      (or (vector-ref scheduler-info schinfo-delta)
	  'NOT-PREEMPTIVE-SCHEDULING)
      (abort-to-top-level-driver
       (cond ((unbound? format) "^G to restart the futures")
	     ((not (vector-ref scheduler-info schinfo-delta))
	      "^G: no preemptive scheduling")
	     ((negative? (vector-ref scheduler-info schinfo-delta))
	      (let ((delta (vector-ref scheduler-info schinfo-delta)))
		(format () "^G: scheduling ~o.~o~o (real) secs."
			(quotient (abs delta) 100)
			(remainder (quotient (abs delta) 10) 10)
			(remainder (remainder (abs delta) 10) 10))))
	     (else
	      (let ((delta (vector-ref scheduler-info schinfo-delta)))
		(format () "^G: scheduling ~o.~o~o (runtime) secs."
			(quotient delta 100)
			(remainder (quotient delta 10) 10)
			(remainder (remainder delta 10) 10))))))))

(define (end-of-computation-handler expression environment value)
  (%reschedule))

(define (new-current-future-vector content)
  (set! Current-Future-Vector (make-vector (n-interpreters) content))
  (let ((fov (get-fixed-objects-vector)))
    (vector-set! fov #x25 Current-Future-Vector)
    (set-fixed-objects-vector! fov)))

; Scheduling support

(define (next)
  (stop-preempting)
  (Set-Current-Future! 'WAITING-FOR-WORK)
  (set-interrupt-enables! interrupt-mask-all)
  (run (get-work idle-routine)))

(define (default-idle-routine)
  (let loop ()
    (stop-preempting)
    (determine! Idle-Future 'DONE)
    (set! Idle-Future
	  (make-future 'NO-PROCESS 'NO-PROCESS "Idle Loop"))
    (run (get-work (lambda ()
		     (error "No work available!")
		     (loop))))))

(define (bfly-idle-routine)
  (stop-preempting)
  (determine! Idle-Future 'DONE)
  (set! Idle-Future
	(make-future 'NO-PROCESS 'NO-PROCESS "Idle Loop"))
  (run (get-work ())))

(define idle-routine default-idle-routine)

;; RUN starts a process running

(define (run future)
  (if *future-start-hook*
      (*future-start-hook* future))
  ((LOCKING-FUTURE future Still-A-Future?
     (if Still-A-Future?
	 (let ((new-process
		(future-set! future FUTURE-PROCESS-SLOT
			     (My-Interpreter-Number)))
	       (old-status (future-set! future FUTURE-STATUS-SLOT 'RUNNING)))
	   (if (and (legitimate-process? new-process)
		    (eq? old-status 'RUNNABLE))
	       (begin
		 (Set-Current-Future! future)
		 (lambda ()
		   (start-preempting)
;;;; JSM???		   (set-interrupt-enables! interrupt-mask-all) ; For preemption -sas
		   (new-process 'YOUR-TURN)))
	       (begin
		 (future-set! future FUTURE-STATUS-SLOT old-status)
		 (future-set! future FUTURE-PROCESS-SLOT new-process)
		 next)))
	 next))))

; Special scheduler operations

;; RESCHEDULE allows me to give up my processor slice and
;; wait until the scheduler gets back to me.

(define-atomic (reschedule)
  (let ((my-task (current-future)))
    (WITH-STATE me
      (if (LOCKING-FUTURE my-task am-I-running?
            (if am-I-running?
		(begin
		  (future-set! my-task FUTURE-PROCESS-SLOT me)
		  (more-work my-task)))
	    am-I-running?)
	  (next)
	  'NOT-CURRENTLY-RUNNING-A-FUTURE))))


;; WAIT-UNTIL-IDLE causes a process to just continue
;; going to sleep until there are no other active processes.

(define (wait-until-idle) (touch idle-future))

(define weak-cons
  (let ((weak-cons-type (microcode-type 'weak-cons)))
    (named-lambda (weak-cons a b)
      (system-pair-cons weak-cons-type a b))))
;(define weak-car system-pair-car)
;(define weak-cdr system-pair-cdr)
;(define weak-set-car! system-pair-set-car!)
;(define weak-set-cdr! system-pair-set-cdr!)


;; SAVING-STATE wraps up the current state of the system into the
;; current future and returns it to the work queue.  It then executes
;; the thunk.  If the current future is invoked the call to
;; SAVING-STATE is exitted; when the thunk returns, the processor will
;; wait for new work to perform.

(define (saving-state thunk)
  (suspending-execution
   (lambda (me)
     (if (future? me) (more-work me))
     (thunk))))

;; SUSPENDING-EXECUTION suspends the current task and calls the specified
;; thunk with the current future.  Then it returns to the scheduler.

(set! suspending-execution
      (named-lambda (suspending-execution thunk)
	(WITH-STATE
	 my-state
	 (let ((my-future (current-future)))
	   (LOCKING-FUTURE
	    my-future am-I-running?
	    (if am-I-running?
		(begin
		  (if *future-suspend-hook*
		      (*future-suspend-hook* my-future))
		  (future-set! my-future FUTURE-PROCESS-SLOT my-state)
		  (future-set! my-future FUTURE-STATUS-SLOT 'SUSPENDED))))
	   (stop-preempting)
	   (set-current-future! 'STATE-SAVED)
	   (set! my-state)
	   (thunk my-future)
	   (next)))
	'COMPLETED))


;; PAUSE-EVERYTHING is used to make every processor but the caller
;; save its state and go quiescent.  The value returned by
;; Pause-Everything is a procedure which will put the work queue 
;; back to its initial state (modulo order of futures on the queue).

(define-atomic (pause-everything)
  
  ;; RELEASE-STATE! takes a list of futures and puts them
  ;; on the work queue.
  (define (release-state! list)
    (if (null? list)
	'RESTARTED
	(let ((work-unit (car list)))
	  (LOCKING-FUTURE work-unit work-to-do?
			  (if (and work-to-do?
				   (legitimate-process?
				    (future-ref work-unit FUTURE-PROCESS-SLOT))
				   (eq? (future-ref work-unit FUTURE-STATUS-SLOT)
					'PAUSED))
			      (more-work work-unit)))
	  (release-state! (cdr list)))))
  
  ;; WEAK-LIST->LIST! takes a weak list of futures, as
  ;; returned by DRAIN-WORK-QUEUE! and converts it to a list of
  ;; the objects referenced.  The GC code needs the weak form,
  ;; hence the extra work here.  In the process, each future is
  ;; made to be PAUSED so it will automatically resume if touched
  
  (define (weak-list->list! weak-list)
    (let loop ((current weak-list)
	       (result '()))
      (if (null? current)
	  (reverse! result)
	  (let ((work-unit (weak-car current)))
	    (if (future? work-unit)
		(begin
		  (future-set! work-unit FUTURE-STATUS-SLOT 'PAUSED)
		  (loop (weak-cdr current)
			(cons work-unit result)))
		(loop (weak-cdr current) result))))))
  
  (define ((returned-object the-queue) #!optional message)
    (if (unassigned? message) (set! message 'Restart-tasks))
    (cond ((eq? message 'Any-Tasks?)
	   (and (not (eq? the-queue #t))
		(not (null? the-queue))))
	  ((eq? message 'Restart-tasks)
	   (if (not (eq? the-queue #t))
	       (release-state! the-queue)
	       (error "Attempt to re-use a pause object!"))
	   (set! the-queue #t))
	  ((eq? message 'The-Tasks)
	   (if (eq? the-queue #t)
	       '()
	       the-queue))
	  (else (error "Pause object: strange message" message))))

  (if (not (Futures-On?))
      (returned-object '())
      (let ((save-synch (make-synchronizer))
	    (drain-synch (make-synchronizer))
	    (proceed-synch (make-synchronizer)))
	(stop-preempting)
	(global-interrupt
	 1
	 (lambda (int-code int-mask)
	   (await-synchrony save-synch)
	   (saving-state
	    (lambda ()
	      (set-interrupt-enables! int-mask)
	      (await-synchrony drain-synch)
	      (await-synchrony proceed-synch))))
	 (lambda () #!TRUE))
	(await-synchrony save-synch)
	(await-synchrony drain-synch)
	(let ((me (current-future))
	      (the-queue (weak-list->list! (drain-work-queue!))))
	  (new-current-future-vector 'PAUSED)
	  (Set-Current-Future! me)
	  (await-synchrony proceed-synch)
	  (returned-object the-queue)))))

;; WITH-TASKS-SUSPENDED executes the thunk with all other processes
;; stopped. It returns the value of the thunk.

(define (with-tasks-suspended thunk)
  (if (not (Futures-On?))
      (thunk)
      (begin
	(let ((the-paused-tasks (pause-everything)))
	  (fluid-let ((debug-paused-tasks the-paused-tasks))
	    (dynamic-wind
	     (lambda ()
	       (if (the-paused-tasks 'any-tasks?)
		   (begin
		     (newline)
		     (display "[Suspending tasks]"))))
	     thunk
	     (lambda ()
	       (cond ((not (the-paused-tasks 'any-tasks?)) '())
		     (discard-the-paused-tasks?
		      (newline) (display "[Discarding tasks]") (newline))
		     (else
		      (newline) (display "[Resuming tasks]") (newline)
		      (the-paused-tasks 'Restart-tasks))))))))))

;; Dealing with recently suspended tasks

(define (discard-recently-suspended-tasks!)
  (set! discard-the-paused-tasks? #t))

(define (prevent-discarding-processes!) 
  (set! discard-the-paused-tasks? #f))

;; Execution within a selected task

(define (within-process future thunk)
  (define (loop noisy?)
    ((LOCKING-FUTURE future true-future?
       (if true-future?
	   (let ((status (future-ref future FUTURE-STATUS-SLOT))
		 (process (future-ref future FUTURE-PROCESS-SLOT)))
	     (cond ((non-touching-eq? future (current-future))
		    thunk)
		   ((eq? status 'RUNNING)
		    (lambda ()
		      (if noisy?
			  (bkpt "WITHIN-PROCESS: process is running"))
		      (loop #f)))
		   (else
		    (future-set! future FUTURE-PROCESS-SLOT
		      (if (continuation? process)
			  (let ((our-fluids
				 (continuation-fluid-bindings
				  (continuation-next-continuation
				   process))))
			    (if (not (null? our-fluids))
				(lambda (arg)
				  (set-fluid-bindings! our-fluids)
				  (within-continuation process thunk))
				(lambda (arg) (thunk) (process 'go))))
			  (lambda (arg) (thunk) (process 'go))))
		    (more-work future)
		    (lambda () (run future)))))
	   (begin
	     (error "WITHIN-PROCESS: Not a process" future)
	     (lambda () (next)))))))
  (loop #t))

(define (get-scheduling-delta)
  (vector-ref scheduler-info schinfo-delta))

))					; End of SCHEDULER
; Export definitions to the world outside the scheduler

(define initialize-scheduler! (access initialize-scheduler! scheduler))
(define determine! (access determine! scheduler))
(define next (access reschedule scheduler))
(define wait-until-idle (access wait-until-idle scheduler))
(define pause-everything (access pause-everything scheduler))
(define with-tasks-suspended (access with-tasks-suspended scheduler))
(define discard-recently-suspended-tasks!
  (access discard-recently-suspended-tasks! scheduler))
(define prevent-discarding-processes!
  (access prevent-discarding-processes! scheduler))
(define Current-Future (access Current-Future scheduler))
(define Futures-On? (access Futures-On? scheduler))
(define Futures-Off (access Futures-Off scheduler))
(define Saving-State (access Saving-State scheduler))
(define within-process (access within-process scheduler))
(define get-scheduling-delta (access get-scheduling-delta scheduler))
(define weak-cons (access weak-cons scheduler))
(define weak-car system-pair-car)
(define weak-cdr system-pair-cdr)
(define weak-set-car! system-pair-set-car!)
(define weak-set-cdr! system-pair-set-cdr!)

(futures-on)

