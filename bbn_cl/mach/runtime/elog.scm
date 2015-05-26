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
(define log-event (make-primitive-procedure 'prim-elog-log))
(define with-event-log)
(define with-event-list)
(define logging-events?)

(define elog-package
  (make-environment

(define *elog-log-file*)
(define *elog-n-events*)

(define prim-elog-init   (make-primitive-procedure 'prim-elog-init))
(define prim-elog-setup  (make-primitive-procedure 'prim-elog-setup))
(define prim-elog-define (make-primitive-procedure 'prim-elog-define))
(define prim-elog-output (make-primitive-procedure 'prim-elog-output))
(define prim-with-values (make-primitive-procedure 'with-values))
(define prim-values      (make-primitive-procedure 'values))

(define (elog-init filename n-entries event-list)
  (prim-elog-init filename)
  (let loop ((n 0) (l event-list))
    (if (null? l)
	#f
	(let ((event-def (car l)))
	  (prim-elog-define n 
			    (if (symbol? (car event-def))
				(symbol->string (car event-def)) 
				(car event-def))
			    (if (cdr event-def)
				(cadr event-def)
				""))
	  (loop (1+ n) (cdr l)))))
  (let ((synchronizer (make-synchronizer)))
    (global-interrupt 
     user-global-interrupt-level
     (lambda (a b)
       (prim-elog-setup filename (my-interpreter-number) n-entries)
       (await-synchrony synchronizer))
     (lambda () #t))
    (prim-elog-setup filename (my-interpreter-number) n-entries)
    (await-synchrony synchronizer))
  #t)

(define (elog-output)
  (let ((synchronizer (make-synchronizer)))
    (global-interrupt 
     user-global-interrupt-level
     (lambda (a b)
       (prim-elog-output)
       (await-synchrony synchronizer))
     (lambda () #t))
    (prim-elog-output)
    (await-synchrony synchronizer)))

(set! with-event-log
      (named-lambda (with-event-log filename thunk #!optional n-events)
	(apply prim-values
	       (fluid-let ((*elog-log-file* filename))
		 (if (unassigned? n-events)
		     (prim-with-values thunk list)
		     (fluid-let ((*elog-n-events* n-events))
		       (prim-with-values thunk list)))))))

;;; (with-event-list event-list thunk #!optional n-events)
;;;
;;;   event-list -> ((<name> <format-string>) ...)

(set! with-event-list
      (named-lambda (with-event-list event-list thunk #!optional n-events)
	(if (unassigned? *elog-log-file*)
	    (thunk)
	    (let ((filename *elog-log-file*))
	      (if (unassigned? *elog-n-events*)
		  (if (unassigned? n-events)
		      (set! n-events 0))
		  (if (unassigned? n-events)
		      (set! n-events *elog-n-events*)
		      (begin
			(warn "Overriding default number of events ~a with ~a"
			      n-events *elog-n-events*)
			(set! n-events *elog-n-events*))))
	      (let ((result))
		(dynamic-wind
		 (lambda ()
		   (elog-init filename n-events event-list))
		 (lambda ()
		   (prim-with-values thunk
				     (lambda all (set! result all))))
		 (lambda ()
		   (elog-output)))
		(apply prim-values result))))))

(set! logging-events?
      (named-lambda (logging-events?)
	(not (unassigned? *elog-log-file*))))

;;; This should be done with define-global-macro -- but it seems 
;;; to have compilation problems.

(in-package syntaxer-package

(define (expand-define-event-list name event-list)
  `(begin
     ,@(let loop ((l event-list) (n 0))
	 (if (null? l)
	     '()
	     (cons `(define ,(caar l) ,n)
		   (loop (cdr l) (1+ n)))))
     (define ,name ',event-list)))

(syntax-table-define system-global-syntax-table 'define-event-list
  (spread-arguments
   (lambda (name event-list)
     (syntax-expression (expand-define-event-list name event-list)))))

)

)) ; end elog-package