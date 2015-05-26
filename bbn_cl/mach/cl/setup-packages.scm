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

(defvar *lisp-package*)
(defvar *system-package*)
(defvar *system-symbols*)
(defvar *user-package*)

(setq *system-symbols*
      '(
	%spawn-process
	atomic-add-car
	atomic-add-cdr
	atomic-add-simple-vector
	await-synchrony
	cd
	current-future
	debug
	define-event-list
	dequeue-head
	dequeue-tail
	describe-hardware-trap
	describe-hardware-stack
	determine
	disassemble-file
	disk-restore
	disk-save
	enqueue-head
	enqueue-tail
	exit
	future
	future-determined-slot
	future-lock-slot
	future-metering-slot
	future-name
	future-orig-code-slot
	future-process-slot
	future-process-private-slot
	future-queue-slot
	future-ref
	future-spawn-tree-slot
	future-status-slot
	future-subtask-count-slot
	future-user-slot
	future-value-slot
	future-waiting-on-slot
	futurep
	gc-flip
	gc-history-mode 
	gc-notification-mode 
	global-interrupt
	imaginary
	insert-touches
	lock-lock
	lock-multiple
	log-event
	logging-events?
	lr-evaluation
	make-future
	make-lock
	make-queue
	make-synchronizer
	my-interpreter-number
	my-processor-number
	n-interpreters
	n-idle-processors
	n-processors
	non-future
	non-touching-eq
	pp
	print-gc-statistics
	proceed
	pwd
	queue-contents
	queue-length
	real
	reschedule-future
	reschedule-self
	rplaca-if-eq
	rplacd-if-eq
	set-lifo-not-fifo
	set-queue-overflow-level
	set-queue-overflow-mode
	setf-if-eq
	toggle-gc-notification 
	touch
	unlock-lock
	unlock-multiple
	user-global-interrupt-level
	wait-until-system-idle
	where
	which
	with-event-list
	with-event-log
	with-lock
	with-suspended-current-future
	without-errors
	work-queue-length
	))

(defun setup-packages ()
  (setq *lisp-package* (make-package "LISP" :use nil))
  (setq *system-package* (make-package "SYSTEM" :use nil))
  (setq *user-package* (make-package "USER" :use nil))
  ;;
  ;; Setup the lambda symbol that the user sees to 
  ;;  do the same thing as cl-lambda.
  ;;
  (setq *user-lambda-symbol* (intern "LAMBDA" *lisp-package*))
  (export *user-lambda-symbol* *lisp-package*)
  (add-syntax! *user-lambda-symbol* (lookup-syntax 'cl-lambda))
  (export-system-symbols)
  (let* ((start-time (runtime))
	 (r (export-lisp-symbols))
	 (end-time (runtime)))
    (newline)
    (princ ";;; Time to export LISP package symbols: ")
    (princ (- end-time start-time))
    (newline))
  (use-package *lisp-package* *user-package*)
  (use-package *system-package* *user-package*))

(defun export-system-symbols ()
  (dolist (x *system-symbols*)
	  (export-symbol x *system-package*)))

(defun export-lisp-symbols ()
  (dotimes (i (vector-length *boot-exported-symbols*))
	   (let ((exp-list (vector-ref *boot-exported-symbols* i)))
	     (dolist (x exp-list)
	       (when (and (fboundp x)
			  (not (macro-function x))
			  (boundp x)
			  (procedure? (symbol-value x)))
		 (local-assignment *commonlisp-user-environment* x (make-unassigned-object)))
	       (export-symbol x *lisp-package*))))
  (setq *boot-exported-symbols* nil))


;;;
;;; Below, note that we use some non-commonlisp fakery
;;;  to change a symbol's home package. CL does not allow
;;;  this, although it does seem as though IMPORT should set the home package
;;;  when importing an uninterned symbol.
;;;

(defun export-symbol (symbol to-package)
  (let ((x symbol))
    (import (if x x (list x)) to-package)
    (%set-symbol-package! x to-package) 
    (export (if x x (list x)) to-package)))
