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
;;;
(declare (integrate-primitive-procedures
	  (procedure-environment system-pair-cdr)))

(define await-synchrony)
(define make-synchronizer)

(define *gc-start-hook* ())
(define *gc-finish-hook* ()) 

(define purify-code-space)
(define impurify-code-space)

(define bf-gc-package (procedure-environment gc-flip))

(in-package bf-gc-package
  (declare (usual-integrations)
	   (integrate-primitive-procedures
	    primitive-purify
	    primitive-impurify
	    primitive-fasdump
	    enable-interrupts!
	    with-interrupts-reduced
	    primitive-gc-type
	    pure?
	    get-fixed-objects-vector
	    set-fixed-objects-vector!
	    (< &<)
	    (= eq?)
	    (weak-car system-pair-car)
	    (weak-cdr system-pair-cdr)
	    drain-work-queue!
	    gc-needed?
	    slave-gc-before-sync
	    slave-gc-after-sync
	    master-gc-before-sync
	    master-gc-loop
	    n-interpreters
	    (primitive-await-synchrony  await-synchrony)
	    put-work
	    global-interrupt
	    lock-future!
	    unlock-future!
	    (primitive-purify-code-space purify-code-space)
	    (primitive-impurify-code-space impurify-code-space)
	    (procedure-environment system-pair-cdr)))

  ;; These get redefined by resources.com

  (define (prepare-to-snapshot-resources-before-gc!) #f)
  (define (prepare-to-snapshot-resources-after-gc!) #f)
  (define (master-snapshot-resources-before-gc!) #f)
  (define (slave-snapshot-resources-before-gc!) #f)
  (define (master-snapshot-resources-after-gc! remaining-free) #f)
  (define (slave-snapshot-resources-after-gc!) #f)

  (let ((Synchronizer-Marker   '(SYNCHRONIZER))
	(INTERRUPT-MASK-GLOBAL-GC 1))

    ;;MAKE-SYNCHRONIZER craetes an object which can be used with
    ;;AWAIT-SYNCHRONY to synchronize all processors.
    (set! make-synchronizer
	  (named-lambda (make-synchronizer)
	    (cons synchronizer-marker (n-interpreters))))

    (set! await-synchrony
	  (named-lambda (await-synchrony synchronizer)
	    (if (eq? (car synchronizer) synchronizer-marker)
		(primitive-await-synchrony synchronizer)
		(error "AWAIT-SYNCHRONY: Not a synchronizer." synchronizer))))


    ;;TEST-AND-GC actually causes the garbage collection to happen if the
    ;;thunk returns #!TRUE.  It can be called either from the interrupt
    ;;handler or a user's call to GC-FLIP.  It uses global-interrupt to
    ;;atomically call the thunk with the appropriate serializing lock
    ;;set.
    
    (define (test-and-gc thunk Action)
      (let ((GC-Synchronizer (make-synchronizer))
	    (GC-Starting-Sync (make-synchronizer))
	    (before-gc-resources-sync (make-synchronizer))
	    (after-gc-resources-sync (make-synchronizer)))

	;;GC-SLAVE is called at interrupt level with interrupts off
	(define (gc-slave interrupt-code interrupt-enables)
	  (await-synchrony GC-Starting-Sync)
	  ;;The master will drain the work queue between here ...
	  (slave-gc-before-sync)
	  (await-synchrony GC-Synchronizer)
	  (slave-snapshot-resources-before-gc!)
	  (await-synchrony before-gc-resources-sync) 
	  ;;... and here
	  (slave-gc-after-sync)
	  (await-synchrony after-gc-resources-sync)
	  (slave-snapshot-resources-after-gc!))

	(if *gc-start-hook* (*gc-start-hook*))

	(if (global-interrupt 0 gc-slave thunk)
	    (sequence
	      (await-synchrony GC-Starting-Sync)
	      ;;Everyone is starting GC-SLAVE now
	      (let ((the-queue (drain-work-queue!)))
		(master-gc-before-sync)
		(prepare-to-snapshot-resources-before-gc!)
		(await-synchrony GC-Synchronizer)
		(master-snapshot-resources-before-gc!)
		;; have to wait here so that no one gc's until resource data is allocated
		(await-synchrony before-gc-resources-sync) 
		(let ((result (Action))); Perform the protected action
		  (prepare-to-snapshot-resources-after-gc!)
		  ;; Everyone must wait for the master to prepare
		  (await-synchrony after-gc-resources-sync)
		  (master-snapshot-resources-after-gc! result)
		  (if *gc-finish-hook* (*gc-finish-hook*))
		  (let loop ((list the-queue))
		    (if (null? list)
			result
			(let ((work-unit (weak-car list)))
			  (if (future? work-unit)
			      (put-work work-unit))
			  (loop (weak-cdr list))))))))
	    8192)))			; GC-AVERTED

    (define (gc-without-interrupts thunk)
      ;; We enable GC global interrupts here in case someone
      ;; else is trying to reach us on this priority channel!
      (with-interrupts-reduced INTERRUPT-MASK-GLOBAL-GC
	(lambda (old-interrupt-mask) (thunk))))

    (define (reset)
      (enable-interrupts! INTERRUPT-MASK-GLOBAL-GC))

    ;; User call -- optionally overrides the default GC safety
    ;; margin for this flip only

    (set! gc-flip
	  (named-lambda (gc-flip #!Optional New-Safety-Margin Condition)
	    (gc-without-interrupts
	     (lambda ()
	       (test-and-gc
		(if (unassigned? Condition)
		    (lambda () #!true)
		    Condition)
		(lambda ()
		  (master-gc-loop
		   (if (unassigned? New-Safety-Margin)
		       Default-Safety-Margin
		       New-Safety-Margin))))))))

    (vector-set!
     (vector-ref (get-fixed-objects-vector) 1)
     2					; Local Garbage Collection Interrupt
     (named-lambda (gc-interrupt interrupt-code interrupt-enables)
       (gc-flip Default-Safety-Margin gc-needed?)))

    (set-fixed-objects-vector! (get-fixed-objects-vector))
					; Propagate the changes!

    (reset)

;;;; "GC-like" Primitives

    ;; Purify an item -- move it into pure space and clean everything
    ;; by doing a gc-flip

    (set! purify
	  (named-lambda (purify item #!optional really-pure?)
	    (special-gc (lambda ()
			  (primitive-purify item
					    (if (unassigned? really-pure?)
						#!FALSE; assume constant space
						really-pure?))))
	    item))

    ;; Purify all of code space
    
    (set! purify-code-space
	  (if is-a-butterfly?
	      (lambda ()
		(special-gc (lambda () (primitive-purify-code-space))))
	      (lambda () (gc-flip))))

    (set! impurify-code-space
	  (if is-a-butterfly?
	      (lambda () (primitive-impurify-code-space))
	      (lambda () 'done)))

   ;; Special call for use with purify and other operations which
    ;; must be performed next to a garbage collection.

    (define (special-gc Special-Action #!Optional New-Safety-Margin)
      (gc-without-interrupts
       (lambda ()
	 (test-and-gc (lambda () #!true) Special-Action))))

    (set! impurify
	  (named-lambda (impurify object)
	    (if (zero? (primitive-gc-type object))
		object
		(if (pure? object)
		    (primitive-impurify object)
		    object))))

    (let ((old-suspend-world suspend-world))
      (set! suspend-world
	(named-lambda
	  (bfly-suspend-world suspender after-suspend after-restore)
	  (if (futures-on?)
	      (let ((scheduling-interval (get-scheduling-delta)))
		(futures-off)
		(old-suspend-world
		 suspender
		 (lambda (ie)
		   (initialize-scheduler! scheduling-interval #!true)
		   (after-suspend ie))
		 (lambda (ie)
		   (initialize-scheduler! scheduling-interval #!true)
		   (after-restore ie))))
	      (old-suspend-world suspender after-suspend after-restore)))))
	  
    )					; End LET
  )					; End IN-PACKAGE
