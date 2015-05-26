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
(proclaim '(insert-touches nil))

;;; Parallel extensions drawn from Scheme

;; Redundant entries (non-renames) are omitted.
;; The functions and macros here are exported to the system
;; package in setup-packages.scm

(cl-define set-queue-overflow-level set-queue-overflow-level!)
(cl-define set-queue-overflow-mode set-queue-overflow-mode!)

;; Where did this go? It is missing
;;(cl-define set-lifo-not-fifo set-lifo-not-fifo!)

(cl-define futurep future?)

(cl-define lock-lock lock-lock!)
(cl-define unlock-lock unlock-lock!)
(cl-define lock-multiple lock-multiple!)
(cl-define unlock-multiple unlock-multiple!)

(cl-define atomic-add-car atomic-add-car!)
(cl-define atomic-add-cdr atomic-add-cdr!)
(cl-define atomic-add-simple-vector atomic-add-vector!)

(cl-define dequeue-head dequeue-head!)
(cl-define dequeue-tail dequeue-tail!)
(cl-define enqueue-head enqueue-head!)
(cl-define enqueue-tail enqueue-tail!)

(cl-define determine determine!)

(cl-define future-name future->string)
(cl-define non-touching-eq non-touching-eq?)

(defsetf future-ref cl-future-set!)

(cl-define (cl-future-set! x y z)
  (future-set! x y z)
  z)

(defmacro with-lock (lock &body body)
  `(unwind-protect
    (progn (lock-lock ,lock)
	   ,@body)
    (unlock-lock ,lock)))

(defmacro with-suspended-current-future (name &body body)
  `(suspending-execution
    #'(lambda (,name)
	,@body)))

;;; FUTURE must be a commonlisp macro,
;;;  or else it will be viewed as a 
;;;  special form by code walkers.

(defmacro future (expression)
  (if (null? (touch-mode))
      (warn "INSERT-TOUCHES is NIL, but FUTURE is used in ~S" expression))
  `(%spawn-process (lambda () ,expression) "" #t))

(cl-define rplaca-if-eq set-car-if-eq?!)
(cl-define rplacd-if-eq set-cdr-if-eq?!)

(defmacro setf-if-eq (exp new old)
  (cond ((eq (car exp) 'car)
	 `(set-car-if-eq?! ,(cadr exp) ,new ,old))
	((eq (car exp) 'cdr)
	 `(set-cdr-if-eq?! ,(cadr exp) ,new ,old))
	((eq (car exp) 'svref)
	 `(vector-set-if-eq?! ,(cadr exp) ,(caddr exp) ,new ,old))
	(else
	 (error "Unknown target for SETF-IF-EQ: ~A" exp))))

(cl-define prim-global-interrupt global-interrupt)

(defmacro global-interrupt (level test (interrupt-code interrupt-enables) &body work)
  `(prim-global-interrupt 
    ,level
    #'(lambda (,interrupt-code ,interrupt-enables)
	,@work)
    #'(lambda () ,test)))
