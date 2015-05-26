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
;;;;;; Notation:
;;;   rv  = resources-vector
;;;   prv = pseudo-resources-vector
;;;   puv = pseudo-usage-vector

(define time-thunk)

(define gc-statistics-package)
(define print-gc-statistics)
(define gc-history-mode)
(define toggle-gc-notification!) ; defined for compability
(define gc-notification-mode)    ; the new, grand notifier interface

(define resource-usage-package
  (make-environment
    
(define system-vector-32b-ref (make-primitive-procedure 'system-vector-32b-ref))
(define system-vector-32b-set! (make-primitive-procedure 'system-vector-32b-set!))
(define system-vector-32b-length (make-primitive-procedure 'system-vector-32b-length))
(define get-resource-usage-vector (make-primitive-procedure 'get-resource-usage-vector))
(define make-resource-usage-vector (make-primitive-procedure 'get-resource-usage-vector)) ; for now.
(define with-values (make-primitive-procedure 'with-values))
(define values (make-primitive-procedure 'values))
(define truncate-string! (make-primitive-procedure 'truncate-string!))

(define (get-resources-slot vector slot-name)
  (vector-ref vector (resources-slot-index slot-name)))

(define (set-resources-slot! vector slot-name value)
  (vector-set! vector (resources-slot-index slot-name) value))

(define (resources-slot-index slot-name)
  (+ (n-interpreters)
     (cdr
      (assq slot-name rv-slot-names))))

(define rv-slot-names
  '(
    (kind          . 0)
    (gc-count      . 1)
    (gc-heap-space . 2)
    ))

;;; Access constants for uv's and puv's (just the ones we need)

(define real-uv-index 1)
(define ucpu-uv-index 2)
(define scpu-uv-index 3)

(define rv-size)
(define gc-rv)
(define gc-count)
(define after-gc-sync)   ; so that master knows all processors have computed their data vectors
(define print-gc-sync)   ; so that slaves wait while master prints, to avoid jumbled output

;;; This is allocated after a gc to be used in the next before-gc snapshot,
;;;  so we don't run out of space.
;;; First one is allocated when band is restored.

(define spare-rv)

(define (install!)
  ((access install! gc-statistics-package))
  (reset-resources-info-system!)
  (add-event-receiver! event:after-restore reset-resources-info-system!)
  (set! (access prepare-to-snapshot-resources-before-gc! bf-gc-package)
	prepare-to-snapshot-resources-before-gc!)
  (set! (access prepare-to-snapshot-resources-after-gc! bf-gc-package)
	prepare-to-snapshot-resources-after-gc!)
  (set! (access master-snapshot-resources-before-gc! bf-gc-package)
	master-snapshot-resources-before-gc!)
  (set! (access slave-snapshot-resources-before-gc! bf-gc-package)
	slave-snapshot-resources-before-gc!)
  (set! (access master-snapshot-resources-after-gc! bf-gc-package)
	master-snapshot-resources-after-gc!)
  (set! (access slave-snapshot-resources-after-gc! bf-gc-package)
	slave-snapshot-resources-after-gc!))

(define (reset-resources-info-system!)
  (set! gc-count 1)
  (set! rv-size (+ (n-interpreters) (length rv-slot-names)))
  (set! spare-rv (allocate-rv))
  #f)

(define (allocate-rv)
  (let ((rv (make-vector rv-size)))
    (let loop ((i (-1+ (n-interpreters))))
      (if (< i 0)
	  rv
	  (begin
	    (vector-set! rv i (make-resource-usage-vector))
	    (loop (-1+ i)))))))

(define (prepare-to-snapshot-resources-before-gc!)
  (set! gc-rv spare-rv))

(define (prepare-to-snapshot-resources-after-gc!)
  (set! gc-rv (make-vector rv-size))
  (set! spare-rv (allocate-rv))
  (set! after-gc-sync (make-synchronizer))
  (set! print-gc-sync (make-synchronizer)))

(define (master-snapshot-resources-before-gc!)
  (snapshot-gc-resources! 'before-gc #t #f))

(define (slave-snapshot-resources-before-gc!)
  (snapshot-gc-resources! 'before-gc #f #f))

(define (master-snapshot-resources-after-gc! remaining-free)
  (snapshot-gc-resources! 'after-gc #t remaining-free)
  (if (and (number? remaining-free) (< remaining-free 8192))
      (abort->nearest
       (standard-rep-message "Aborting: Out of memory!"))))

(define (slave-snapshot-resources-after-gc!)
  (snapshot-gc-resources! 'after-gc #f #f))

(define (snapshot-gc-resources! kind master? remaining-free)
  (case kind
    ((before-gc)  ; the gc-rv at this point is always pre-allocated  -- pass the uv to the ucode
     (get-resource-usage-vector (vector-ref gc-rv (my-interpreter-number)))
     (if master?
	 (begin
	   (set-resources-slot! gc-rv 'kind 'before-gc)
	   (set-resources-slot! gc-rv 'gc-count gc-count)
	   (record-in-gc-resources-list! gc-rv)
	   (record-gc-statistic! gc-rv)
	   (start-gc-notify gc-rv))))
    ((after-gc)  
     (vector-set! gc-rv (my-interpreter-number)
		  (get-resource-usage-vector))
     (await-synchrony after-gc-sync)
     (if (not master?)
	 (await-synchrony print-gc-sync)
	 (begin
	   (set-resources-slot! gc-rv 'kind 'after-gc)
	   (set-resources-slot! gc-rv 'gc-count gc-count)
	   (set! gc-count (1+ gc-count))
	   (set-resources-slot! gc-rv 'gc-heap-space remaining-free)
	   (record-in-gc-resources-list! gc-rv)
	   (record-gc-statistic! gc-rv)
	   (end-gc-notify gc-rv)
	   (await-synchrony print-gc-sync)))))
  #f)

;;; Returns a vector indexed by interpreter number.
;;; Each element of the vector is s vector-32b (see cl-time.c)

(define (snapshot-resources)
  (define (get-info-list info-vector)
    (vector-set! info-vector (my-interpreter-number)
		 (get-resource-usage-vector)))
  (let ((synchronizer (make-synchronizer)))
    (let ((info-vector (make-vector rv-size)))
      (global-interrupt 
       user-global-interrupt-level
       (lambda (a b)
	 (get-info-list info-vector)
	 (await-synchrony synchronizer))
       (lambda () #t))
      (get-info-list info-vector)
      (await-synchrony synchronizer)
      info-vector)))

(define resource-usage-vector-size 15) ; Must track cl-time.c

(define (make-prv)
  (let ((v (make-vector (n-interpreters))))
    (let loop ((n (-1+ (n-interpreters))))
      (if (< n 0)
	  v
	  (begin
	    (vector-set! v n (make-vector resource-usage-vector-size 0))
	    (loop (-1+ n)))))))

(define (accumulate-prv operation rv acc)
  (let loop1 ((i (-1+ (n-interpreters))))
    (if (< i 0)
	#f
	(let ((usage-vector (vector-ref rv i))
	      (acc-puv (vector-ref acc i)))
	  (let loop2 ((j (-1+ resource-usage-vector-size)))
	    (if (< j 0)
		(loop1 (-1+ i))
		(begin
		  (vector-set! acc-puv j
			       ((if (differential-datum? j)
				    operation
				    (lambda (x y) y))
				(vector-ref acc-puv j)
				(system-vector-32b-ref usage-vector j)
				))
		  (loop2 (-1+ j)))))))))

(define (prv-diff x y)
  (let ((z (make-prv)))
    (let loop1 ((i (-1+ (vector-length z))))
      (if (< i 0)
	  z
	  (let ((xpuv (vector-ref x i))
		(ypuv (vector-ref y i))
		(zpuv (vector-ref z i)))
	    (let loop2 ((j (-1+ (vector-length xpuv))))
	      (if (< j 0)
		  (loop1 (-1+ i))
		  (begin
		    (vector-set! zpuv j 
				 (if (differential-datum? j)
				     (- (vector-ref xpuv j)
					(vector-ref ypuv j))
				     (vector-ref xpuv j)))
		    (loop2 (-1+ j))))))))))

(define (rv->prv rv)
  (let ((prv (make-prv)))
    (accumulate-prv (lambda (x y) y) rv prv)
    prv))

(define (get-delta-gc-resources gc-resources-list cont)
  (let ((gc-acc (make-prv)))
    (let loop ((l gc-resources-list) (gc-count 0) (total-remaining-space 0))
      (if (null? l)
	  (cont gc-acc gc-count 
		(if (= gc-count 0) 0 (/ total-remaining-space gc-count)))
	  (let ((rv (car l)))
	    (case (get-resources-slot rv 'kind)
	      ((before-gc)
	       (accumulate-prv - rv gc-acc)
	       (loop (cdr l) (1+ gc-count) total-remaining-space))
	      ((after-gc)
	       (accumulate-prv + rv gc-acc)
	       (loop (cdr l) gc-count (+ total-remaining-space
					 (get-resources-slot rv 
							     'gc-heap-space))))))))))

(define (get-field-sizes prv)
  (let ((v (make-vector (vector-length prv))))
    (let loop ((i (-1+ (vector-length v))))
      (if (< i 0)
	  v
	  (begin
	    (vector-set! v i 
			 (+ 4 (string-length 
			       (number->string 
				(vector-max (vector-ref prv i))))))
	    (loop (-1+ i)))))))

(define (vector-max v)
  (if (= (vector-length v) 1)
      (vector-ref v 0)
      (let loop ((m (vector-ref v 0))
		 (i (-1+ (vector-length v))))
	(cond
	 ((= i 0) m)
	 ((> (vector-ref v i) m)
	  (loop (vector-ref v i)
		(-1+ i)))
	 (else
	  (loop m (-1+ i)))))))

;;; If not integer-format, n is assumed to be 1000 times larger than the representation we wish to print
;;; result-string is where we put the characters, beginning at start index si,
;;; of width w.
;;; it is assumed to be blank-filled.

(define (format-number n integer-format? w result-string si)
  (let* ((s (number->string n))
	 (l (string-length s))
	 (p (- (+ si w) 5)))
    (if (not integer-format?)
	(begin
	  (let loop ((i 0))
	    (if (= i 4)
		#f
		(begin
		  (string-set! result-string (+ p i) (string-ref "0.000" i))
		  (loop (1+ i)))))
	  (let loop ((i 1))
	    (if (or (= i 4)
		    (< (- l i) 0))
		#f
		(begin
		  (string-set! result-string (- (+ si w) i)
			       (string-ref s (- l i)))
		  (loop (1+ i)))))))
    (let loop ((i (- l (if integer-format? 1 4)))
	       (j (- (+ si w) 5)))
      (if (< i 0)
	  #f
	  (begin
	    (string-set! result-string j
			 (string-ref s i))
	    (loop (-1+ i) (-1+ j)))))
    result-string))

(define (prv->formatted-string prv cont)
  (let* ((field-sizes (get-field-sizes prv))
	 (l (vector-length prv))
	 (string (make-string (* (1+ l) resource-usage-vector-size (+ 10 (vector-max field-sizes)))
			      #\space)))
    (let loop1 ((i 0) (fi 0))
      (if (= i resource-usage-vector-size)
	  (cont string fi)
	  (let* ((info (vector-ref resource-usage-info-vector i))
		 (message-string (first info))
		 (message-string-length (string-length message-string))
		 (integer-format? (second info)))
	    (let ((new-fi
		   (let loop ((j 0) (fi fi))
		     (if (= j message-string-length)
			 (1+ fi)
			 (begin
			   (string-set! string fi (string-ref message-string j))
			   (loop (1+ j) (1+ fi)))))))
	      (let loop2 ((j 0) (fi new-fi))
		(if (= j l)
		    (begin
		      (string-set! string fi #\newline)
		      (loop1 (1+ i) (1+ fi)))
		    (begin
		      (format-number 
		       (vector-ref 
			(vector-ref prv j)
			i)
		       integer-format?
		       (vector-ref field-sizes j)
		       string
		       fi)
		      (loop2 (1+ j) (+ 1 fi (vector-ref field-sizes j))))))))))))

(define resource-usage-info-vector
  #( 
;;;  Name          integer-format?  differential-datum?
    ("Interp No: " #t               #f)
    ("Real:      " #f               #t)
    ("Ucpu:      " #f               #t)
    ("Scpu:      " #f               #t)
    ("Idle:      " #f               #t)
    ("Pg rec:    " #t               #t)
    ("Pg disk:   " #t               #t)
    ("Swap:      " #t               #t)
    ("Blk in:    " #t               #t)
    ("Blk out:   " #t               #t)
    ("Msg snd:   " #t               #t)
    ("Msg rcv:   " #t               #t)
    ("Sig:       " #t               #t)
    ("Vcsw:      " #t               #t)
    ("IVcsw:     " #t               #t)    
    ))

(define (differential-datum? uv-index)
  (caddr (vector-ref resource-usage-info-vector uv-index)))

;;; Convenient test function to generate random pseudo-resource-vectors
#|
(define (f n)
  (let ((v (make-vector n)))
    (let loop ((n (-1+ n)))
      (if (< n 0)
	  v
	  (begin
	    (vector-set! v n (apply vector 
				    (eval (cons 'list 
						(make-list 
						 resource-usage-vector-size 
						 '(random 100000000))) ())))
	    (loop (-1+ n)))))))
|#

(set! time-thunk
      (named-lambda (time-thunk thunk)
	;; we do this so that only top-level time-thunk creates
	;; the gc resources list; the data vanishes when it exits.
	(if (unassigned? gc-resources-list)
	    (fluid-let ((gc-resources-list))
	      (reset-gc-resources-list!)
	      (time-thunk-1 thunk))
	    (time-thunk-1 thunk))))

(define (time-thunk-1 thunk)
  (let* ((start-gc-resources-list-point gc-resources-list)
	 (start-normal-prv (rv->prv (snapshot-resources)))
	 (result-list (with-values thunk
				   list))
	 (end-normal-prv (rv->prv (snapshot-resources))))
    (get-delta-gc-resources (get-gc-resources-list start-gc-resources-list-point)
      (lambda (gc-prv gc-count avg-remaining-space)
	(let ((normal-prv (prv-diff (prv-diff end-normal-prv start-normal-prv)
				    gc-prv)))
	  (newline)
	  (prv->formatted-string 
	   normal-prv 
	   (lambda (norm-string norm-end-index)
	     (prv->formatted-string
	      gc-prv 
	      (lambda (gc-string gc-end-index)
		(truncate-string! norm-string norm-end-index)
		(truncate-string! gc-string gc-end-index)
		(write-string "*** Non-GC resources ***")
		(newline)
		(write-string norm-string)
		(newline)
		(write-string "*** GC resources ***")
		(newline)
		(write-string "GC count: ")
		(write gc-count)
		(if (not (= gc-count 0))
		    (begin
		      (newline)
		      (write-string "Average space remaining after each GC: ")
		      (write avg-remaining-space)))
		(newline)
		(write-string gc-string)
		(newline)
		(apply values result-list))))))))))

(set! print-gc-statistics
      (named-lambda (print-gc-statistics)
	(let loop ((l (gc-statistics)))
	  (if (null? l)
	      #f
	      (begin
		(print-gc-statistic (car l) (cadr l))
		(loop (cddr l)))))))

(define (print-gc-statistic start-rv end-rv)
  (newline)
  (write-string "GC ")
  (write (get-resources-slot start-rv 'gc-count))
  (newline)
  (write-string "Free space: ")
  (write (get-resources-slot end-rv 'gc-heap-space))
  (newline)
  (prv->formatted-string (prv-diff (rv->prv end-rv)
				   (rv->prv start-rv))
    (lambda (string fi)
      (truncate-string! string fi)
      (write-string string)
      (newline))))

;;; Only real time is meaningful here. We use that of interpreter 0.

(define (print-brief-gc-statistic start-rv end-rv)
  (let ((delta-prv (prv-diff (rv->prv end-rv)
			     (rv->prv start-rv))))
    (newline)
    (write-string "GC ")
    (write (get-resources-slot start-rv 'gc-count))
    (write-string " took ")
    (write (/ (vector-ref (vector-ref delta-prv 0) real-uv-index) 1000))
    (write-string " sec. (real) Free Space: ")
    (write (get-resources-slot end-rv 'gc-heap-space))))

(define gc-resources-list)

(define (reset-gc-resources-list!)
  (set! gc-resources-list '()))

(define (record-in-gc-resources-list! x)
  (if (not (unassigned? gc-resources-list))
      (set! gc-resources-list (cons x gc-resources-list))))

(define (get-gc-resources-list end-point)
  (let loop ((l gc-resources-list))
    (if (eq? l end-point)
	#f
	(cons (car l)
	      (loop (cdr l))))))

;;; The keeper of permanent gc statistics.
;;; The exported procedures below are called by the gc resource info collector and printer,
;;;  above.
;;; The other exported procedures, defined at the top of this file, 
;;;  are user-callable.

(define record-gc-statistic!)
(define gc-statistics)
(define start-gc-notify)
(define end-gc-notify)

(set! gc-statistics-package
  (make-environment

(define (statistics-reset!)
  (reset-recorder! '()))


;;;; Statistics Recorder

(define history)

(define (reset-recorder! old)
  (reset-history! old))

(set! record-gc-statistic! 
      (named-lambda (record-gc-statistic! statistic)
	(record-in-history! statistic)))

(set! gc-statistics
      (named-lambda (gc-statistics)
	(get-history)))

;;; GC Notification

(let ((mode 'quiet)
      (start-rv))
  (set! gc-notification-mode
	(named-lambda (gc-notification-mode #!optional new-mode)
	  (if (unassigned? new-mode)
	      (list mode
		    '(quiet bell brief verbose))
	      (let ((old-mode mode))
		(set! mode new-mode)
		old-mode))))
  (set! toggle-gc-notification!
	(named-lambda (toggle-gc-notification!)
	  (if (eq? (car (gc-notification-mode)) 'quiet)
	      (gc-notification-mode 'brief)
	      (gc-notification-mode 'quiet))))
  (set! start-gc-notify
	(named-lambda (start-gc-notify rv)
	  (set! start-rv rv)
	  (case (car (gc-notification-mode))
	    ((bell)
	     (write-char #\C-G))
	    ((verbose))
	    ((brief))
	    ((quiet)))))
  (set! end-gc-notify
	(named-lambda (end-gc-notify rv)
	  (case (car (gc-notification-mode))
	    ((bell)
	     (write-char #\C-G))
	    ((verbose)
	     (print-gc-statistic start-rv rv)
	     (set! start-rv '()))
	    ((brief)
	     (print-brief-gc-statistic start-rv rv)
	     (set! start-rv '()))
	    ((quiet))))))


;;;; History Modes

(define reset-history!)
(define record-in-history!)
(define get-history)
(define history-mode)

(set! gc-history-mode
      (named-lambda (gc-history-mode #!optional new-mode)
	(let ((old-mode history-mode))
	  (if (not (unassigned? new-mode))
	      (let ((old-history (get-history)))
		(set-history-mode! new-mode)
		(reset-history! old-history)))
	  old-mode)))

(define (set-history-mode! mode)
  (let ((entry (assq mode history-modes)))
    (if (not entry)
	(error "Bad mode name" 'SET-HISTORY-MODE! mode))
    ((cdr entry))
    (set! history-mode (car entry))))

(define history-modes
  `((NONE . ,(named-lambda (none:install-history!)
	       (set! reset-history! none:reset-history!)
	       (set! record-in-history! none:record-in-history!)
	       (set! get-history none:get-history)))
    (BOUNDED . ,(named-lambda (bounded:install-history!)
		  (set! reset-history! bounded:reset-history!)
		  (set! record-in-history! bounded:record-in-history!)
		  (set! get-history bounded:get-history)))
    (UNBOUNDED . ,(named-lambda (unbounded:install-history!)
		    (set! reset-history! unbounded:reset-history!)
		    (set! record-in-history! unbounded:record-in-history!)
		    (set! get-history unbounded:get-history)))))

;;; NONE

(define (none:reset-history! old)
  (set! history '()))

(define (none:record-in-history! item)
  'DONE)

(define (none:get-history)
  '())

;;; BOUNDED

(define history-size 16)

(define (copy-to-size l size)
  (let ((max (length l)))
    (if (>= max size)
	(initial-segment l size)
	(append (initial-segment l max)
		(make-list (- size max) '())))))

(define (bounded:reset-history! old)
  (set! history (apply circular-list (copy-to-size old history-size))))

(define (bounded:record-in-history! item)
  (set-car! history item)
  (set! history (cdr history)))

(define (bounded:get-history)
  (let ((first (car history)))
    (let ((copied-history
	   (let loop ((scan (cdr history)))
	     (cond ((eq? scan history) '())
		   ((null? (car scan)) (loop (cdr scan)))
		   (else (cons (car scan) (loop (cdr scan))))))))
      (if first
	  (cons first copied-history)
	  copied-history))))

;;; UNBOUNDED

(define (unbounded:reset-history! old)
  (set! history old))

(define (unbounded:record-in-history! item)
  (set! history (cons item history)))

(define (unbounded:get-history)
  (reverse history))

;;;; Initialization

(define (install!)
  (set-history-mode! 'BOUNDED)
  (statistics-reset!)
  (set! (access stack-overflow garbage-collector-package)
	(named-lambda (stack-overflow)
	  (abort->nearest
	   (standard-rep-message
	    "Aborting: Maximum recursion depth exceeded!"))))
  (set! (access hardware-trap garbage-collector-package)
	(named-lambda (hardware-trap)
	  (abort->nearest
	   (standard-rep-message
	    "Aborting: The hardware trapped!"))))
  (add-event-receiver! event:after-restore statistics-reset!))

;;; end GC-STATISTICS-PACKAGE.
))

)) ; end resource-usage-package
