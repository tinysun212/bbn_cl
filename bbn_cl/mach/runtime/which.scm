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
;;;;;;; This is Multi -*- Scheme -*- code
;;;; Which - Task Inspector
;;;;

;;; Some macros

(define-macro (PROG1 . exprs)
  `(LET ((FIRST ,(CAR exprs)))
     ,@(CDR exprs)
     FIRST))

(define-macro (if-binding binding consequent alternative)
  `(LET (,binding)
     (IF ,(CAR binding)
	 ,consequent
	 ,alternative)))

(define-macro (export-from-scheduler name)
  `(define ,name (access ,name scheduler)))



;;; Definitions

(declare (usual-integrations)
	   (integrate-operator
	    push-stack pop-stack top-of-stack stack-ref
	    clean-up-stack empty-stack? singleton-stack?)
	   )

;;; Stack Abstraction

  (define (make-stack)
    (let ((the-stack '()))
      (define (push-into-it x)
	(set! the-stack (cons x the-stack)))
      (define (pop-it)
	(if the-stack
	    (let ((it (car the-stack)))
	      (set! the-stack (cdr the-stack))
	      it)
	    (error "POP: Empty Stack" '())))
      (define (peek-at-it)
	(if the-stack
	    (car the-stack)
	    (error "PEEK: Empty Stack" '())))
      (define (ref n)
	(list-ref the-stack n))
      (define (clean-it) (set! the-stack '()))
      (named-lambda (stack m)
	(case m
	  ((push) push-into-it)
	  ((pop) (pop-it))
	  ((peek) (peek-at-it))
	  ((ref) ref)
	  ((clean) (clean-it))
	  ((empty?) (null? the-stack))
	  ((singleton?) (= 1 (length the-stack)))
	  ((contents) the-stack)
	  (else (error "STACK: invalid message" m))))))

  (define (push-stack stack)
    (declare (integrate stack)) (stack 'PUSH))
  (define (pop-stack stack)
    (declare (integrate stack)) (stack 'POP))
  (define (top-of-stack stack)
    (declare (integrate stack)) (stack 'PEEK))
  (define (stack-ref stack)
    (declare (integrate stack)) (stack 'REF))
  (define (clean-up-stack stack)
    (declare (integrate stack)) (stack 'CLEAN))
  (define (empty-stack? stack)
    (declare (integrate stack)) (stack 'EMPTY?))
  (define (singleton-stack? stack)
    (declare (integrate stack)) (stack 'SINGLETON?))

  

;;; The Actual Stuff

(define which)
(define which-display-tasks)
(define which-display-tasks-with-value)


(define which-package
  (make-environment

    (define :name "Which")
    (define :version 1)
    (define :modification 2)
    (define :files '("which.scm"))
    ))

;;;	Everything below the following $split-file is loaded into which-package!

'$split-file

  (declare (integrate-primitive-procedures
	    (rest cdr)
	    (set-first! set-car!)
	    (weak-car system-pair-car)
	    (weak-cdr system-pair-cdr)
	    (weak-set-car! system-pair-set-car!)
	    (weak-set-cdr! system-pair-set-cdr!)
	    (class-sample car)
	    (task-frame-number car)
	    (set-task-frame-number! set-car!)
	    )
	   (integrate-operator
	    push-stack pop-stack top-of-stack clean-up-stack empty-stack?
	    singleton-stack? noline
	    task-procedure task-process set-task-process! task-id-number
	    task-identity task-determined?
	    task-status set-task-status!
	    task-dependencies set-task-dependencies!
	    task-on-a-wait-queue? task-wait-queue
	    spaces task-frame-process make-task-frame
	    spawn-tree-node-task
	    spawn-tree-node-child spawn-tree-node-children
	    set-spawn-tree-node-child!
	    task-child-spawn-id task-parent-spawn-id)
	   )

(define command-set (make-command-set 'DEBUG-COMMANDS))

(define future-eq? (make-primitive-procedure 'NON-TOUCHING-EQ?))
(define future->vector (make-primitive-procedure 'FUTURE->VECTOR))
(define within-control-point (make-primitive-procedure 'WITHIN-CONTROL-POINT))

;;; Queues (stolen from Scheduler)
(define queue-head-ptr car)
(define queue-tail-ptr cdr)
(define set-queue-head-ptr! set-car!)
(define set-queue-tail-ptr! set-cdr!)

(define queue-contents queue-head-ptr)
(define queue-length (lambda (q) (weak-length (queue-contents q))))

(export-from-scheduler current-future)

;; WHICH State variables
(define calling-continuation)		; Process that called WHICH
(define suspended-task-equivalences)	; Processes suspended at call time
(define task-equiv-stack (make-stack))
(define task-stack (make-stack))
(define marked-tasks)


;; Constants

(define STATUS-FLAG:PAUSED 'PAUSED)
(define STATUS-FLAG:SUSPENDED 'DELAYED)
(define STATUS-FLAG:KILLED 'TERMINATE)
(define STATUS-FLAG:FORCED 'DETERMINE)
(define STATUS-FLAG:RUNNING 'RUNNING)

;; Aliases
(define rest cdr)			;LIST abstraction
(define set-first! set-car!)

;; Weak pairs
(define weak-cons-type (microcode-type 'WEAK-CONS))
(define weak-car system-pair-car)
(define weak-cdr system-pair-cdr)
(define weak-set-car! system-pair-set-car!)
(define weak-set-cdr! system-pair-set-cdr!)
(define weak-pair? (microcode-type-predicate 'weak-cons))

;;; Useful functions

(define (weak-length l)
  (if (null? l)
      0
      (1+ (weak-length (weak-cdr l)))))

(define (weak-cons a b)
  (system-pair-cons weak-cons-type a b))

;;; WLIST->LIST converts a weak list into a strong list, removing null
;;; elements as it goes along.  
(define (wlist->slist wlist)
  (cond ((null? wlist) '())
	((weak-pair? wlist)
	 (if (non-touching-eq? (weak-car wlist) '())
	     (wlist->slist (weak-cdr wlist))
	     (cons (weak-car wlist) (wlist->slist (weak-cdr wlist)))))
	(else
	 (error "WLIST->SLIST: not a weak list!"))))

(define (member-future f lyst)
  (cond ((null? lyst) '())
	((future-eq? f (car lyst)) lyst)
	(else (member-future f (cdr lyst)))))

(define (noline)
  (declare (integrate)) (display ""))

;;; Debugging
(define (traced-await-future x) (%await-future x))

;;; Basic Commands

(define (define-which-command letter function help-text)
  (define-letter-command command-set letter function help-text))


(define-which-command #\? (standard-help-command command-set)
  "Help, list command letters")

;;; WHICH Exit - Must clean up some internal things so that tasks can
;;; be GC'd if they are not otherwise referenced.

(define (which-exit-command)
  (clean-up-stack task-stack)
  (clean-up-stack task-equiv-stack)
  (set! calling-continuation)
  (set! suspended-task-equivalences)
  (standard-exit-command))

(define-which-command #\X which-exit-command
  "Exit (leave WHICH immediately)")

(define (unimplemented-which-command)
  "Oops, you found me out!  I haven't written that command yet.")


;;; Main definition

(define (which #!optional task)
  (dynamic-wind
   (lambda ()
     (vector-set! (get-fixed-objects-vector) #x15 traced-await-future))
   (lambda ()
     (cond ((lexical-unassigned? scheduler 'CURRENT-FUTURE-VECTOR)
	    (error "WHICH: Futures not activated."))
	   ((unassigned? task) (which (Current-Future)))
	   ((not (future? task))
	    (error "WHICH: Future has been determined:" task))
	   ;; Calling from top level (ie. no previous task suspension)
	   ((unassigned? debug-paused-tasks)
	    (with-tasks-suspended (lambda () (inner-which task))))
	   (else (inner-which task))))
   (lambda ()
     (vector-set! (get-fixed-objects-vector) #x15 %await-future))))

(define (inner-which task)

  (set! calling-continuation (rep-continuation))
  (set! marked-tasks '())

  (format "~2%Which -- Task Inspector")

  ;; Find where you are
  (which-get-bearings task)
    
  ;; Show where you are
  (print-waiters-on-current-task)

  (newline)
  (let ((number-of-runnable-tasks
	 (length (flatten-non-touching (get-suspended-tasks))))
	(number-of-tasks
	 (length (get-all-tasks))))
    (letter-commands command-set
		     (standard-rep-message 
		      (string-append
		       (number->string number-of-tasks)
		       " task"
		       (if (= number-of-tasks 1) "" "s")
		       ", "
		       (number->string number-of-runnable-tasks)
		       " runnable"))
		     "Which-->")))



;;; Task abstraction
;;; No FUTURE locking needed because all other processors are off, and
;;; we better not be touching in this code.

(define-macro (define-future-selector arg)
  `(define (,(first arg) task)
     (declare (integrate task))
     (future-ref task ,(second arg))))

(define-macro (define-future-mutator arg)
  `(define (,(first arg) task val)
     (declare (integrate task val))
     (future-set! task ,(second arg) val)))

(define-future-selector (task-procedure FUTURE-ORIG-CODE-SLOT))
(define-future-selector (task-process FUTURE-PROCESS-SLOT))
(define-future-mutator  (set-task-process! FUTURE-PROCESS-SLOT))

(define (task-continuation task)
  (cond ((the-calling-task? task) calling-continuation)
	((virgin-future? task)
	 (format "~%This future hasn't been run yet!")
	 #F)
	(else (control-point->continuation (task-process task)))))

(define task-identity future->string)

(define-future-selector (task-id-number FUTURE-METERING-SLOT))
(define-future-selector (task-determined? FUTURE-DETERMINED-SLOT))
(define-future-selector (task-status FUTURE-STATUS-SLOT))
(define-future-mutator  (set-task-status! FUTURE-STATUS-SLOT))
(define-future-selector (task-dependencies FUTURE-WAITING-ON-SLOT))
(define task-on-a-wait-queue? task-dependencies)
(define-future-mutator  (set-task-dependencies! FUTURE-WAITING-ON-SLOT))
(define-future-selector (task-wait-queue FUTURE-QUEUE-SLOT))

(define (waiters-on-task task)
  (if (not (task-determined? task))
      (let ((waiters
	     (wlist->slist (queue-contents (task-wait-queue task)))))
	(if (and waiters (not (future-eq? (car waiters) '())))
	    waiters
	    #f))))

(define-future-selector (task-spawn-count FUTURE-SUBTASK-COUNT-SLOT))
(define-future-selector (task-spawn-id FUTURE-SPAWN-TREE-SLOT))

;;; Status Bit - Display

(define process-status-bit
  (let ((status-alist `((,STATUS-FLAG:SUSPENDED . "S")
			(,STATUS-FLAG:KILLED . "K")
			(,STATUS-FLAG:FORCED . "F")
			(,STATUS-FLAG:RUNNING . "*"))))

    (named-lambda (process-status-bit process)
      (let ((status (task-status process)))
	(if-binding (flag (assq status status-alist))
		    (cdr flag)
		    "")))))



;; PATH-TO-TASK searches the entire suspended-task tree for TASK,
;; and returns a list of task-frames, representing a 'waiter-path'
;; from the original suspended queue to TASK.

(define (path-to-task task)

  (define (or-over f lyst)
    (if lyst
	(or (f (car lyst)) (or-over f (cdr lyst)))))

  (let search-loop ((tasks (get-suspended-tasks))
		    (path '()))
    (cond ((null? tasks) '())
	  ((member-future task tasks)
	   (reverse
	    (cons (lookup-task-and-generate-frame task tasks)
		  path)))
	  (else
	   (or-over
	    (lambda (task)
	      (search-loop
	       (waiters-on-task task)
	       (cons
		(lookup-task-and-generate-frame task tasks)
		path)))
	    tasks)))))

(define (lookup-task-and-generate-frame task task-list)
  (make-task-frame task
		   (1+ (list-position-future task task-list))))



;; TASK-PATH->EQUIV-PATH generates a list of process equivalence
;; classes from a 'waiter-path' of processes.

(define (task-path->equiv-path path)
  (let loop ((path path)
	     (classes (list suspended-task-equivalences)))
    (if (not (rest path))
	(reverse classes)
	(loop (rest path)
	      (cons
	       (process-equivalences
		(waiters-on-task (task-frame-process (first path))))
	       classes)))))


;; Useful Procedures

(define (list-position-future f lyst)
  (cond ((null? lyst) '())
	((future-eq? f (weak-car lyst)) 0)
	(else (1+ (list-position-future f (weak-cdr lyst))))))

(define (flatten-non-touching l)
  (cond ((not l) '())
	((not (list? l)) (list l))
	((future? (car l)) (cons (car l) (flatten-non-touching (cdr l))))
	(else (append (flatten-non-touching (car l))
		      (flatten-non-touching (cdr l))))))

;;; Used to remove a future from another's wait queue.
;;;
;;;	This queue is a cons, the car pointing at the head and the cdr at the tail.

(define (remove-future-from-wqueue! elt wq) 
  (let loop ((next (queue-contents wq))
	     (previous '()))
    (cond ((not next) '())
	  ((future-eq? (weak-car next) elt)
	   (if previous			; Always fix the previous element.
	       (weak-set-cdr! previous (weak-cdr next)))
	   (if (weak-cdr next)		; Last element?
	       (set-queue-tail-ptr! wq (weak-cdr next)))
	   (if (null? previous)		; First element?
	       (set-queue-head-ptr! wq (weak-cdr next)))) 
	  (else
	   (loop (weak-cdr next) next)))))
  

(define remove-future-from-alist!
  (delete-association-procedure list-deletor! future-eq? car))


;;; Command definition
'$split-file
(define (get-current-thunk)
  (if-binding (thunk (task-procedure (get-current-task)))
	      thunk
	      (begin
		(format "~%Undefined environment for this task.")
		#F)))

(define (pretty-print-expression)
  (if-binding (thunk (get-current-thunk))
	      (pp thunk)
	      '()))
		   
(define-which-command #\L pretty-print-expression
  "(list expression) Pretty-print the task expression")


(define (get-suspended-tasks)
  (flatten-non-touching suspended-task-equivalences))


;; GET-RECENTLY-SUSPENDED-TASKS inspects the tasks suspended by the
;; most recent WITH-TASKS-SUSPENDED.  All elements in the queue that
;; are not real futures are filtered out.  The current future (the
;; contintuation of the process running WHICH) is also discarded in
;; the filter, and so is put back in explicitly.

(define (get-recently-suspended-tasks)
  (cons
   (current-future)
   (debug-paused-tasks 'THE-TASKS)))


(define (print-future-slots f)
  (let ((it (future->vector f)))
    (if it
	(vector-set! it FUTURE-QUEUE-SLOT
		     (queue-contents (task-wait-queue it))))
    it))

(define (print-suspended-tasks)
  (show-processes suspended-task-equivalences))

(define-which-command #\A print-suspended-tasks
  "Print all tasks and their waiters")



;; Routine to be called from outside (tasks aren't sorted yet)
(define (display-recently-suspended-tasks)
  (if-binding (tasks (get-recently-suspended-tasks))
	      (show-processes (process-equivalences tasks))
	      'NO-SUSPENDED-TASKS))


;;; Get all tasks in system
(define (get-all-tasks)
  (let loop ((old-tasks '())
	     (new-tasks (get-recently-suspended-tasks)))
    (if new-tasks
	(loop (append new-tasks old-tasks)
	      (flatten-non-touching (map waiters-on-task new-tasks)))
	old-tasks)))


(define (spaces l)
  (declare (integrate l)) (make-string l #\space))

(define (print-waiters-on-current-task)
  (show-process-with-number
   (get-current-task)
   (get-current-task-number))
  (noline))

(define-which-command #\. print-waiters-on-current-task
  "Print the waiters on the current task")


(define (print-siblings-of-task)
  (show-processes (get-current-siblings)))
		   
(define-which-command #\C print-siblings-of-task
  "Print the co-waiters of the current task")

(define (which-get-bearings task)
  ;; Generate task-list (as equivalence classes)
  (set! suspended-task-equivalences
	(process-equivalences (get-recently-suspended-tasks)))
  ;; Clean up stacks
  (for-each clean-up-stack (list task-stack task-equiv-stack))
  ;; Reset stacks
  (if-binding (task-frames (path-to-task task))
	      (for-each which-down task-frames
			(task-path->equiv-path task-frames))
	      (begin
		(format "~%Future supplied as argument is neither active")
		(format "nor potentially active.")
		(standard-exit-command))))


(define (show-processes process-classes #!optional origin)

  (define (show-process process depth from to)
    (let* ((indent (* depth 2))
 	   (line (task-identity process))
	   (id (task-id-number process))
	   (status-bit (process-status-bit process))
	   (size-of-string (min (string-length line) (- 66 indent)))
	   (colon (if (and (not (unassigned? origin)) (virgin-future? process))
		      ";" ":")))
      (if (= from to)
	  (format "~%~s~1s~2x~o~1s~3x~@:vs ~o"
		  (spaces indent) status-bit from
		  colon size-of-string line id)
	  (format "~%~s~1s~o-~o~1s~2x~@:vs ~o"
		  (spaces indent) status-bit from to
		  colon size-of-string line id))))

  (newline)
  (let loop ((process-classes process-classes)
	     (depth 0)
	     (breadth (if (unassigned? origin) 1 origin)))
    (if process-classes
	(let ((process-class (first process-classes)))
	  (let ((next-breadth (+ breadth (length process-class)))
		(process (class-sample process-class)))
	    (show-process process depth breadth (-1+ next-breadth))
	    (loop (process-equivalences (waiters-on-task process))
		  (1+ depth) 1)
	    (loop (rest process-classes) depth next-breadth)))
	(noline))))

(define (show-process process)
  (show-processes (list (list process))))

(define (show-process-with-number process number)
  (show-processes (list (list process)) number))


;; Class abstraction

(define (make-class first-elt) (list first-elt))
(define class-sample car)
(define (append-to-class! class new-elt)
  (set-cdr! class (cons new-elt (cdr class))))

;; Map over class
(define map-over-class map)
(define for-each-element for-each)

;; Sorting over classes
(define (sort-classes classes p)
  (sort
   (map (lambda (class) (sort class p)) classes)
   (lambda (class1 class2)
     (p (class-sample class1) (class-sample class2)))))



;; MAKE-EQUIVALENCE-CLASSES takes an equality procedure, a hashing
;; function, and a list of elements, and builds a list of lists, each
;; sublist of which is an equivalence class (ie. all the elements of
;; each sublist are equivalent (wrt. to the given equality procedure)
;; to each other). 

(define (make-equivalence-classes equivalent? hash elts)

  (let* ((number-of-buckets 37)
	 (buckets (make-vector number-of-buckets)))

    (declare (integrate-operator elt->bucket existing-class?))

    (define (elt->bucket s)
      (declare (integrate s))
      (remainder (hash s) number-of-buckets))

    (define (existing-class? elt buckets)
      (declare (integrate elt buckets))
      (vector-ref buckets (elt->bucket elt)))
  
    (define (add-to-class-in-bucket! elt bucket)
      (let ((class
	     ((association-procedure equivalent? class-sample) elt bucket)))
	(if class
	    (append-to-class! class elt)
	    (append! bucket (list (make-class elt))))))
  
    (define (filled-buckets buckets)
      (let next-bucket ((result '())
			(index 0))
	(if (= index number-of-buckets)
	    result
	    (next-bucket
	     (append (vector-ref buckets index) result)
	     (1+ index)))))


    (for-each-element
     (lambda (elt)
       (let ((the-bucket (existing-class? elt buckets)))
	 (if the-bucket
	     (add-to-class-in-bucket! elt the-bucket)
	     (vector-set! buckets
			  (elt->bucket elt)
			  (list (make-class elt))))))
     elts)
    (filled-buckets buckets)))		; end of MAKE-EQUIVALENCE-CLASSES



'$split-file
;;; Process equivalences

(define (process-hash process) (string-hash (task-identity process)))

(define (process-equivalences processes)
  (make-equivalence-classes eqv-processes? process-hash processes))

(define (eqv-processes? process1 process2)
  (and (eq? (task-identity process1)
	    (task-identity process2))
       (eq? (task-status process1)
	    (task-status process2))
       (eqv-wait-queues? (waiters-on-task process1)
			 (waiters-on-task process2))))

;; Do the queues have equivalent waiters?
(define (eqv-wait-queues? set1 set2)
  (and (= (weak-length set1) (weak-length set2))
       (let loop ((set1 set1))
	 (if set1
	     (and (one-of-waiters? (weak-car set1) set2)
		  (loop (weak-cdr set1)))
	     #T))))

(define one-of-waiters? (member-procedure eqv-processes?))

(define (same-spawning-parent process1 process2)
  (eq? (task-parent-spawn-id process1)
       (task-parent-spawn-id process2)))

(define (same-spawning-parent-hash process)
  (object-hash (task-parent-spawn-id process)))


;;; Spawn Tree Abstraction

(define (make-spawn-tree-node task)
  (cons task
	(vector-cons (task-spawn-count task) 'DETERMINED-FUTURE)))

(define (spawn-tree? tree)
  (and (pair? tree)
       (vector? (cdr tree))))

(define (spawn-tree-node-task node)
  (declare (integrate node)) (car node))

(define (spawn-tree-node-children node)
  (declare (integrate node)) (vector->list (cdr node)))

(define (spawn-tree-node-child node pos)
  (declare (integrate node pos)) (vector-ref (cdr node) pos))

(define (set-spawn-tree-node-child! node child val)
  (declare (integrate node child val))
  (vector-set! (cdr node) (task-child-spawn-id child) val))


;;; Spawn ID Ordering

(define (task-child-spawn-id task)
  (declare (integrate task)) (first (task-spawn-id task)))

(define (task-parent-spawn-id task)
  (declare (integrate task)) (rest (task-spawn-id task)))

(define (spawn-id-< t1 t2)
  (id-< (task-spawn-id t1)
	(task-spawn-id t2)))


(define (id-< id1 id2)

  (define (<-loop id1 id2)
    (cond ((not id1) (if id2 #t #f))
	  ((not id2) #f)
	  ((= (first id1) (first id2))
	   (<-loop (rest id1) (rest id2)))
	  ((< (first id1) (first id2)) #t)
	  ((> (first id1) (first id2)) #f)
	  (else (error "Violation of trichotomy principle"))))

  (<-loop (reverse id1) (reverse id2)))


;;; Spawn Tree Generation

;; GENERATE-SPAWN-TREE generates a tree of the remnants of the task
;; spawn tree for the current set of (suspended) tasks.  It does this
;; by looking at the FUTURE-SPAWN-TREE-SLOT of each task.

(define (generate-remnant-spawn-tree)
  (sort (get-all-tasks) spawn-id-<))



;;; Display Spawn Tree

(define (display-spawn-tree sorted-tasks)

  (define (familial-relation from to)
    (let* ((diff (- (length to) (length from)))
	   (up (if (negative? diff)
		   (removal (list-tail from (- diff)) to)
		   (removal from (list-tail to diff)))))
      (if (negative? diff)
	  (cons up (+ up (abs diff)))
	  (cons (+ up (abs diff)) up))))


  (define (removal from to)
    (if (eq? from to)
	0
	(1+ (removal (rest from) (rest to)))))

  (define (display-node current-id display-string previous-id)
    (let ((relation (familial-relation current-id previous-id))
	  (level (length current-id)))
      (cond ((or (= (cdr relation) 1)
		 (equal? relation (cons 0 0))))
	    ((equal? (cdr relation) 0)	;ancestor?
	     (error "DISPLAY-SPAWN-TREE: task list not sorted"))
	    (else			;cousin
	     (display-node (rest current-id) "..." previous-id)))
      (display-line (length current-id)
		    (first current-id)
		    display-string)))

  (define (display-line level label line)
    (let ((indent (* 3 level)))
      (format "~%~vx~o:~3x~@:vs" indent label (- 66 indent) line)))

  (let ((task-ids (map task-spawn-id sorted-tasks)))
    (for-each
     display-node
     task-ids
     (map task-identity sorted-tasks)
     (cons () task-ids))))


;;; User Command

(define (display-remnant-spawn-tree)
  (format "~10x[Working...]")
  (display-spawn-tree (generate-remnant-spawn-tree)))

(define-which-command #\Y display-remnant-spawn-tree
  "Display spawn tree of existing tasks")



;;;; Movement commands

(define (select-task-frame task-list)
  (if task-list
      (let ((limit (length task-list)))
	(if (= limit 1)
	    (make-task-frame (first task-list) 1)
	    (let inputloop ()
	      (format "~%Enter task number (1-~o) or 0 to exit: "
		      limit)
	      (let ((red (read)))
		(cond ((not (number? red))
		       (format "~%Input number must be numeric!")
		       (inputloop))
		      ((zero? red) (format "~%Movement aborted!") '())
		      ((and (> red 0)
			    (<= red limit))
		       (make-task-frame
			(list-ref task-list (-1+ red))
			red))
		      (else
		       (format "~%Number out of range.!")
		       (inputloop)))))))
      (begin (format "~%There are no tasks to choose from!") '())))


(define (which-down task-frame family)
  ((push-stack task-stack) task-frame)
  ((push-stack task-equiv-stack) family))

(define (which-on-waiter)
  (let ((the-waiters
	 (process-equivalences (waiters-on-task (get-current-task)))))
    (let ((selected-frame
	   (select-task-frame (flatten-non-touching the-waiters))))
      (if (and selected-frame (future? (task-frame-process selected-frame)))
	  (begin
	    (which-down selected-frame the-waiters)
	    (print-waiters-on-current-task))
	  (noline)))))

(define-which-command #\D which-on-waiter
  "Go to one of waiters of the current task")


'$split-file
;;; Movement Commands (continued)

(define (back-up)
  (cond ((singleton-stack? task-stack)
	 (format "~%No task to back up to!"))
	(else
	 (pop-stack task-stack)
	 (pop-stack task-equiv-stack)
	 (print-waiters-on-current-task))))
	   

(define-which-command #\U back-up
  "Go to task that current task is waiting on")


(define (jump-across)
  (let ((selected-frame
	 (select-task-frame (flatten-non-touching (get-current-siblings)))))
    (if (and selected-frame (future? (task-frame-process selected-frame)))
	(begin
	  (set-current-task-frame! selected-frame)
	  (print-waiters-on-current-task))
	(noline))))

(define-which-command #\G jump-across
  "Go to a co-waiter of the current task")


;;; PREVIOUS-SIBLING and NEXT-SIBLING
(define (adjacent-sibling inc)
  (lambda ()
    (let ((index (inc (get-current-task-number)))
	  (siblings (flatten-non-touching (get-current-siblings))))
      (let* ((max (length siblings))
	     (new-index 
	      (cond ((zero? index)
		     (format "~10x[Wrapping around to end...]")
		     max)
		    ((> index max)
		     (format "~10x[Wrapping around to beginning...]")
		     1)
		    (else index))))
	(set-current-task-frame!
	 (make-task-frame (list-ref siblings (-1+ new-index)) new-index))
	(print-waiters-on-current-task)))))

(define-which-command #\P (adjacent-sibling -1+)
  "Go to previous sibling")

(define-which-command #\N (adjacent-sibling 1+)
  "Go to next sibling")


;;;; Task Frame Abstraction

(define (make-task-frame task number)
  (declare (integrate task number))
  (list number task))

(define (task-frame-process frame)
  (declare (integrate frame))
  (cadr frame))

(define task-frame-number car)

(define set-task-frame-number! set-car!)

;;;; Evaluation commands

(define (get-current-task)
  (task-frame-process (top-of-stack task-stack)))

(define (get-current-task-number)
  (task-frame-number (top-of-stack task-stack)))

(define (set-current-task-frame! new-task-frame)
  (pop-stack task-stack)
  ((push-stack task-stack) new-task-frame))


(define (get-waitee-of-current-task)
  (if (singleton-stack? task-stack)
      '()
      (let ((popped-task (pop-stack task-stack)))
	(PROG1
	 (get-current-task)
	 ((push-stack popped-task))))))

(define (get-current-siblings)
  (top-of-stack task-equiv-stack))

(define (get-current-class)
  (let ((the-task (get-current-task)))
    (let loop-over-classes ((classes (get-current-siblings)))
      (cond ((not classes) '())
	    ((member-future the-task (first classes)) (first classes))
	    (else (loop-over-classes (rest classes)))))))


(define (if-valid-environment env true-receiver false-receiver)
  (if (environment? env)
      (true-receiver env)
      (false-receiver env)))

(define (show-frame env)
  (if-valid-environment
   env
   (lambda (env) ((access show-frame env-package) env -1))
   (lambda (env) (noline))))


;; Spawning Environment

(define (get-task-spawning-environment task)
  (if (the-calling-task? task)
      (let ((env (continuation-environment calling-continuation)))
	(if (continuation-undefined-environment? env)
	    #F
	    env))
      (let ((it (task-procedure task)))
	(if (procedure? it)
	    (procedure-environment it)
	    #F))))

(define (get-spawning-environment)
  (get-task-spawning-environment (get-current-task)))

(define (get-spawning-or-other-environment)
  (if-binding (env (get-spawning-environment))
	      env
	      (begin
		(format "~%Using the read-eval-print environment instead!")
		(newline)
		(rep-environment))))
  
(define (enter-read-eval-print-loop)
  (read-eval-print (get-spawning-or-other-environment)
		   "You are now in the desired environment"
		   "Eval-in-env-->"))

(define-which-command #\E enter-read-eval-print-loop
  "Enter a read-eval-print loop in the spawning environment")

(define (eval-in-spawning-environment)
  (let ((env (get-spawning-or-other-environment)))
    (format "~%Enter expression to evaluate-> ")
    (eval (read) env)))

(define-which-command #\V eval-in-spawning-environment
  "Evaluate expression in spawning environment and print the result")

(define (enter-spawning-where)
  (where (get-spawning-or-other-environment)))
      
(define-which-command #\W enter-spawning-where
  "Enter WHERE on the spawning environment")


;; Inspection over Spawning Environment of Siblings

(define (show-processes-with-value quoted-expr processes)

  (define (show-process-with-value process depth count val)
    (let ((indent (* 2 depth))
	  (status-bit (process-status-bit process))
	  (colon (if (virgin-future? process) ";" ":")))
      (format "~%~s~1s~3o~@4s~@:vs~4x~@10o"
	      (spaces indent) status-bit count colon
	      (max (- 53 indent) 15) (task-identity process) val)))

  (format "~%  Task  Body~58s ~o" "Value of" quoted-expr)
  (let loop ((processes processes)
	     (vals (eval-in-tasks quoted-expr processes))
	     (depth 0)
	     (breadth 1))
    (if processes
	(let ((process (first processes))
	      (val (first vals)))
	  (let ((waiters (waiters-on-task process)))
	    (show-process-with-value process depth breadth val)
	    (loop waiters
		  (eval-in-tasks quoted-expr waiters)
		  (1+ depth) 1)
	    (loop (rest processes) (cdr vals) depth (1+ breadth))))
	(noline))))


(define (show-hashed-processes-with-value quoted-expr processes)

  (define (show-hashed-process-with-value process depth val)
    (let ((indent (* 2 depth))
	  (status-bit (process-status-bit process))
	  (colon (if (virgin-future? process) ";" ":"))
	  (hash-number (task-id-number process)))
      (format "~%~s~1s~3o~@4s~@:vs~4x~@10o"
	      (spaces indent) status-bit hash-number colon
	      (max (- 53 indent) 15) (task-identity process) val)))

  (format "~% Task  Body~58s ~o" "Value of" quoted-expr)
  (let loop ((processes processes)
	     (vals (eval-in-tasks quoted-expr processes))
	     (depth 0))
    (if processes
	(let ((process (first processes))
	      (val (first vals)))
	  (let ((waiters (waiters-on-task process)))
	    (show-hashed-process-with-value process depth val)
	    (loop waiters
		  (eval-in-tasks quoted-expr waiters)
		  (1+ depth))
	    (loop (rest processes) (cdr vals) depth)))
	(noline))))


'$split-file
;;; Evaluation in Task (Spawning) Environment

(define (eval-in-tasks quoted-expr tasks)
  (map (eval-in-task quoted-expr) tasks))

(define ((eval-in-task quoted-expr) task)
  (let ((symbols-list
	 (if (list? quoted-expr)
	     (flatten-non-touching quoted-expr)
	     (list quoted-expr))))
    (if-binding (env (get-task-spawning-environment task))
		(if (symbols-referenceable? env symbols-list)
		    (eval quoted-expr env)
		   *the-non-printing-object*)
		*the-non-printing-object*)))

(define (display-tasks-with-value quoted-expr)
  (show-processes-with-value
   quoted-expr
   (flatten-non-touching (get-current-siblings))))

(define (interactive-display-tasks-with-value)
  (format "~%Enter expression -> ")
  (display-tasks-with-value (read)))
 
(define (symbols-referenceable? env symbol-list)
  (or (not symbol-list)
      (let ((symbol (first symbol-list))
	    (rest-of-symbols (rest symbol-list)))
	(and (or (not (symbol? symbol))
		 (not (lexical-unreferenceable? env symbol)))
	     (symbols-referenceable? env rest-of-symbols)))))

(define-which-command #\@ interactive-display-tasks-with-value
  "Find value of expression over suspended tasks")


;; Routine to be called from outside (tasks aren't sorted yet)
(define (display-recently-suspended-tasks-with-value quoted-expr)
  (lambda ()
    (set! calling-continuation (rep-continuation))
    (if-binding (tasks (get-recently-suspended-tasks))
		(begin
		  (show-hashed-processes-with-value
		   quoted-expr
		   (flatten-non-touching (process-equivalences tasks))))
		'NO-SUSPENDED-TASKS)))


;; Current Environment

(define (get-current-environment)
  (let ((cont (get-continuation-from-current-task)))
    (if cont
	(let ((env (continuation-environment cont)))
	  (if (continuation-undefined-environment? env)
	      (begin
		(format "~%No continuation environment!")
		(format "~%Using the read-eval-print environment instead!~%")
		(rep-environment))
	      env))
	(begin
	  (format "~%Using the read-eval-print environment instead!~%")
	  (rep-environment)))))


;;; Hacking the Scheduler

(define (make-task-runnable! task)

  (define (remove-from-wait-queue! parent)
    (LOCKING-FUTURE parent parent-still-a-future?
      (if parent-still-a-future?		    
	  (remove-future-from-wqueue!
	   task
	   (task-wait-queue parent)))))

  (LOCKING-FUTURE task still-a-future?
    (if still-a-future?
	(let ((waitees (task-dependencies task)))
	  (if (list? waitees) (for-each remove-from-wait-queue! waitees))
	  (set-task-dependencies! task '())
	  (set-task-status! task STATUS-FLAG:PAUSED)
	  (append! (debug-paused-tasks 'THE-TASKS) (list task)))
	(error "MAKE-TASK-RUNNABLE: no longer a future"))))



;; Special Task Instructions -- Marking Tasks

(define (when-task-unmarked task thunk)
  (if (equal? (process-status-bit task) "")
      (thunk task)
      (format "~%Current future is already marked."))

  (if (not (future-eq? task (first (get-current-class))))
      (format "~10x[Reordering tasks...]"))

  (which-get-bearings task)
  (print-waiters-on-current-task))



;; Called from within future process
(define (which-force-task-value task value)
  (let* ((reschedule (vector-ref (get-fixed-objects-vector) #x26))
	 (force-task-continuation
	  (call-with-current-continuation
	   (lambda (exit)
	     (call-with-current-continuation
	      (lambda (return) 
		(exit return)))
	     (determine! (current-future) value #f)
	     (reschedule)
	     'FORCED))))
    (if (task-on-a-wait-queue? task) (make-task-runnable! task))
    (set-task-process! task force-task-continuation))
  'FORCED)


(define (get-value prompt)
  (let ((env (get-spawning-or-other-environment)))
    (format "~%~s--> " prompt)
    (eval (read) env)))

(define (mark-task-for-forced-determination)
  (when-task-unmarked (get-current-task)
    (lambda (task)
      (let ((value (get-value "Value to force to")))
	(set-task-status! task STATUS-FLAG:FORCED)
	(set! marked-tasks
	      (cons (list task which-force-task-value value) marked-tasks))))))

(define-which-command #\F mark-task-for-forced-determination
  "Forcibly determine a task to completion (after exit from WHICH)")


;;; Special Instructions (Continued)

;;; Kill
 (define (which-kill-task task)
  (which-force-task-value task (make-unassigned-object))
  'KILLED)


(define (mark-task-for-termination)
  (define (actually-mark-task-for-termination task)
    (for-each actually-mark-task-for-termination (waiters-on-task task))
    (set-task-status! task STATUS-FLAG:KILLED)
    (set! marked-tasks (cons (list task which-kill-task) marked-tasks)))

  (when-task-unmarked (get-current-task)
   (lambda (task)
     (actually-mark-task-for-termination task))))


(define-which-command #\K mark-task-for-termination
  "Kill a task and its waiters (after exit from WHICH)")


;;; Unmark
(define (unmark-task)
  (let do-unmark-task ((task (get-current-task)))

    (if (eq? (task-status task) STATUS-FLAG:KILLED)
	(for-each do-unmark-task (waiters-on-task task)))

    (set-task-status! task STATUS-FLAG:PAUSED)
    (set! marked-tasks (remove-future-from-alist! task marked-tasks))

    (which-get-bearings task)
    (if (not (future-eq? task (first (get-current-class))))
	(format "~10x[Reordering tasks...]")))

  (print-waiters-on-current-task))


(define-which-command #\R unmark-task
  "Restore a task that has been marked (Kill, Force, or Suspend)")


;; WHICH-SUSPEND-TASK has nothing to do, as the future will not be put
;; on the work queue on resumption, since its status-slot is set to
;; 'DELAYED.  This is exactly the functionality I want here.

(define (which-suspend-task task) 'DONE)

(define (mark-task-for-suspension)
  (when-task-unmarked (get-current-task)
    (lambda (task)
      (set-task-status! task STATUS-FLAG:SUSPENDED)
      (set! marked-tasks (cons (list task which-suspend-task) marked-tasks)))))

(define-which-command #\S mark-task-for-suspension
  "Suspend a task (after exit from WHICH)") 


;;;
;;; PERFORM-INSTRUCTIONS actually fulfills the promises laid out
;;; during the WHICH session, and then leaves WHICH altogether.
;;;

(define (perform-instructions)
  (for-each
   (lambda (instruction)
     (apply (second instruction)
	    (cons (first instruction) (cddr instruction))))
   marked-tasks))


(define (exit-and-perform-instructions)
  (perform-instructions)
  (which-exit-command))

(define-which-command #\Q exit-and-perform-instructions
  "Quit (leave WHICH) and perform actions on marked tasks")
      

'$split-file
;; Task Continuations

(define control-point?
  (microcode-type-predicate 'CONTROL-POINT))

(define (control-point->continuation cp)
  (call-with-current-continuation
   (lambda (c)
     (within-control-point
      cp
      (lambda ()
	(call-with-current-continuation (lambda (new-c) (c new-c))))))))

(define (virgin-future? f)
  (and (future? f)
       (control-point? (task-process f))
       (let ((expr (continuation-expression
		    (control-point->continuation (task-process f)))))
	 (and (not (future? expr)) (combination? expr)
	      (let ((op (combination-operator expr)))
		(and (pair? op) (cdr op) (second op)
		     (eq? (second op) (task-procedure f))))))))
	  
(define (the-calling-task? f)
  (future-eq? f (current-future)))

(define (debug-current-task)
  (if-binding (cont (task-continuation (get-current-task)))
	      (debug cont)
	      (noline)))

(define-which-command #\B debug-current-task
  "Enter DEBUG on current task")


;;;; Advanced hacking (ie. miscellaneous) commands

(define (rewhich)
  (perform-instructions)
  (within-continuation calling-continuation
		       (lambda ()
			 (next)
			 which)))

(define-which-command #\T rewhich
  "Let the other tasks run a bit, and then resume WHICH")


(define user-which-environment (make-environment))
      
(define (internal-command-procedure)
	(read-eval-print user-which-environment
			 "You are now in the task inspector environment"
			 "Debugging-Which-->"))
      
(define-which-command #\! internal-command-procedure
  "Create a read eval print loop in the WHICH debugger environment")


;;; Exports

(set! (access which system-global-environment)
      (access which which-package))
(set! (access which-display-tasks system-global-environment)
      (access display-recently-suspended-tasks which-package))
(set! (access which-display-tasks-with-value system-global-environment)
      (access display-recently-suspended-tasks-with-value which-package))
