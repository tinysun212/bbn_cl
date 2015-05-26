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
;;; We assume in this code that commonlisp mode is on,
;;;  in particular that *read-file* is the delayed version,
;;;  and that the default syntax table is the right one for cl
;;;  (this shouldn't matter since cl macros don't use
;;;  the syntax table anymore).
;;; In addition, it is assumed that no scheme declarations are present.

;;; This var is so that cl-compile-file may alter the environment
;;; used by defmacro-internal to effect non-sticky compile-time 
;;; macro definitions. See clchap5-b.scm.

(define *defmacro-internal-env* system-global-environment)

(define cl-compile-file-package
  (make-environment

(define debug? #f)

;;; Set this to true to cause the "nondeterministic" crash on bfly.
;;; Try compiling pfft-cl.scm; it will take a fault or pick up a bad
;;; gc type code or get silly unbound errors, in random places.
;;; Narrowed down to fact that we are evaluating compiled code.
;;; (check code space?). With the compiled evaluation on, placing
;;; an explicit gc before make-noise also cures the problem.

(define *compiler-bug* #f)  ; Set this to true to cause the "nondeterministic" crash on bfly

(define force-cdr (access force-cdr syntaxer-package))
(define transmit-values (access transmit-values package/scode-optimizer))
(define syntax (access syntax syntaxer-package))
(define transform/expression (access transform/expression package/scode-optimizer))
(define integrate/expression (access integrate/expression package/scode-optimizer))
(define cgen/expression (access cgen/expression package/scode-optimizer))
(define compile-scode (access compile-scode compiler-package))
(define prim-with-values (make-primitive-procedure 'with-values))

(define (macroexpand x)
  ((symbol-function 'macroexpand) x))

(define (cl-compile-file input-string output-string noisy? syntax? compile? brtl? binf?)
  (with-event-list event-list
    (lambda ()
      (clc-log-event start-compilation)
      (let ((result
	     (fluid-let ((*eval-when-mode* 'compiling)
			 (*package* *package*)
			 (*syntax-time-env* (push-contour *syntax-time-env*))
			 ((access compiler:show-phases? compiler-package) #f))
	       (fluid-let ((*syntax-time-global-env* *syntax-time-env*))
		 (let ((old-mode (touch-mode)))
		   (dynamic-wind
		    (lambda () #f)
		    (lambda ()
		      (if (and compile? (not syntax?))
			  (cf input-string 'output output-string 'noisy noisy? 'syntax #f 'touch #f
			      'compile #t 'b-files `(,@(and brtl? '(rtl)) ,@(and binf? '(binf))))
			  (compile-file input-string output-string noisy? compile? brtl? binf?)))
		    (lambda ()
		      (syntaxer-proclaim! (list 'insert-touches old-mode)))))))))
	(clc-log-event end-compilation)
	result))))

(define *write-file-threshold* 20)

(define (compile-file input-string output-string noisy? compile? brtl? binf?)
  (let ((input-path (pathname->input-truename (->pathname input-string))))

    (define (new-pathname type)
      (pathname->output-truename 
       (pathname-new-type (if output-string 
			      (->pathname output-string)
			      input-path)
			  type)))

    (if (not input-path)
	(error "File ~s does not exist" input-string))
    (if noisy?
	(begin
	  (newline)
	  (princ ";;; Compiling file ")
	  (princ (pathname->string input-path))))
    (let* ((com-path (and compile? (new-pathname "com")))
	   (bin-path (new-pathname "bin"))
	   (brtl-path (and brtl? (new-pathname "brtl")))
	   (binf-path (and binf? (new-pathname "binf")))
	   (sexpr-id 0)
	   (sexprs (*read-file* input-path))
	   (syntax-table (*get-default-syntax-table*))
	   (scode-exprs (tconc-create))
	   (brtl? (and brtl? #t))
	   (binf? (and binf? #t))
	   (compiled-code (and compile? (tconc-create)))
	   (compiled-brtl (and brtl? (tconc-create)))
	   (compiled-binf (and binf? (tconc-create))))

      (compile-file-ccwfofs com-path bin-path brtl-path binf-path
	(lambda (com-fd bin-fd brtl-fd binf-fd)

	  ;; The Compile Process (see below for our own process abstraction)
	  ;;
	  ;; Entries on the queue can be:
	  ;;  - A determined process, meaning a compiled object is available.
	  ;;  - A pending process.
	  ;;  - The symbol WRITE-FILES, a command to write the files
	  ;;    when it reaches the head of the queue.
	  ;;  - A procedure (thunk), a not-yet-started compilation.

	  (define cp-compile-scode)
	  (define cp-write-files)
	  (define cp-check)
	  (define cp-check-log-events? #t)

	  (let ()

	    (define queue (tconc-create))
	    (define n (max (* 3 (n-interpreters)) 1))

	    (define (cp-check-log-event event-name)
	      (if cp-check-log-events?
		  (clc-log-event event-name)))

	    (define (format-queue x)
	      (let ((x (tconc-list x)))
		(let loop ((l x))
		  (if (null? l)
		      #f
		      (cons
		       (let ((x (car l)))
			 (cond
			  ((determined-process? x)
			   (car (process-value x)))
			  (else
			   x)))
		       (loop (cdr l)))))))

	    (set! cp-check
		  (lambda ()
		    (cp-check-log-event cp-check-enter)
		    (if debug? (print (list 'cp-check (format-queue queue)
					    (the-work-queue))))
		    (let ((head (tconc-head queue)))
		      (cond
		       ((null? head)
			'empty)
		       ((eq? head 'write-files)
			(cp-check-log-event cp-check-write-files)
			(tconc-pull! queue)
			(write-files)
			(cp-check))
		       ((determined-process? head) ; A finished compilation
			(cp-check-log-event cp-check-determined-process)
			(tconc-pull! queue)
			(set! head (process-value head))
			(tconc-write-compiled-objects! head)
			(cp-check))
		       (else
			(let loop ((i 0)
				   (l (tconc-list queue)))
			  (if (or (null? l)
				  (= i n))
			      #f
			      (let ((entry (car l)))
				(cond
				 ((and (process? entry)
				       (not (determined-process? entry)))
				  (cp-check-log-event cp-check-pending-process)
				  (reschedule)
				  (loop (1+ i) (cdr l)))
				 ((procedure? entry)
				  (set-car! l (make-process entry))
				  (cp-check-log-event cp-check-start-process)
				  (reschedule)
				  (loop (1+ i) (cdr l)))
				 (else
				  (loop i (cdr l))))))))))))

	    (set! cp-compile-scode
		  (lambda (scode brtl? binf? com-path)
		    (clc-log-event cp-compile-scode-enter sexpr-id)
		    (let ((local-sexpr-id sexpr-id))
		      (tconc-push!
		       queue
		       (lambda ()
			 (clc-log-event start-compile-scode local-sexpr-id)
			 (if debug? (print (list 'enter-compile-scode scode (the-work-queue))))
			 (let ((result
				(prim-with-values
				 (lambda () (compile-scode scode brtl? binf? com-path))
				 make-compiled-object)))
			   (clc-log-event end-compile-scode local-sexpr-id)
			   (if debug? (print (list 'exit-compile-scode scode (the-work-queue))))
			   result))))
		    (cp-check)))

	    (set! cp-write-files
		  (lambda ()
		    (and (not (tconc-empty? scode-exprs))
			 (begin
			   (fasdump-fd (make-sequence (tconc-close! scode-exprs))
				       bin-fd)
			   (set! scode-exprs (tconc-create))))
		    (tconc-push! queue 'write-files)
		    (cp-check)))

	    )

	  (define (tconc-write-compiled-objects! compiled-object)
	    (tconc-write! compiled-code (compiled-object-code compiled-object))
	    (and brtl?
		 (tconc-write! compiled-brtl (compiled-object-brtl compiled-object)))
	    (and binf?
		 (tconc-write! compiled-binf (compiled-object-binf compiled-object))))

	  (define (write-files)
	    (clc-log-event start-write-files)
	    (and com-fd 
		 (not (tconc-empty? compiled-code))
		 (begin
		   (fasdump-fd (make-sequence (tconc-close! compiled-code))
			       com-fd)
		   (set! compiled-code (tconc-create))))
	    (and brtl-fd
		 (not (tconc-empty? compiled-brtl))
		 (begin
		   (fasdump-fd (cons 'brtl-list (tconc-close! compiled-brtl))
			       brtl-fd)
		   (set! compiled-brtl (tconc-create))))
	    (and binf-fd
		 (not (tconc-empty? compiled-binf))
		 (begin
		   (fasdump-fd (cons 'binf-list (tconc-close! compiled-binf))
			       binf-fd)
		   (set! compiled-binf (tconc-create))))
	    (clc-log-event end-write-files))

	  (let compile-loop ((sexprs sexprs)
			     (nobjects 0)
			     (explicit-eval-when? #f)
			     (eval-when-compile? #f)
			     (eval-when-load? #f))
	    (cond
	     ((null? sexprs)
	      (cp-write-files)
	      nobjects)
	     ((>= nobjects *write-file-threshold*)
	      (cp-write-files)
	      (compile-loop sexprs 0 explicit-eval-when?
			    eval-when-compile? eval-when-load?))
	     (else
	      (make-noise (car sexprs) noisy?)
	      (let ((expanded-sexpr 
		     (begin
		       (clc-log-event start-macroexpand)
		       (macroexpand (car sexprs)))))
		(clc-log-event end-macroexpand)
		(cond
		 ((file-split-marker? expanded-sexpr)
		  (cp-write-files)
		  nobjects)
		 ((progn? expanded-sexpr)
		  (let ((new-nobjects 
			 (compile-loop (cdr expanded-sexpr) 
				       nobjects 
				       explicit-eval-when?
				       eval-when-compile?
				       eval-when-load?)))
		    (compile-loop (force-cdr sexprs) new-nobjects
				  explicit-eval-when? 
				  eval-when-compile?
				  eval-when-load?)))
		 ((eval-when? expanded-sexpr)
		  (eval-when-components expanded-sexpr
		    (lambda (situation body)
		      (let ((new-nobjects
			     (compile-loop body nobjects #t					   
					   (or (memq 'compile situation)
					       (and eval-when-compile?
						    (memq 'eval situation)))
					   (memq 'load situation))))
			(compile-loop (force-cdr sexprs) new-nobjects
				      explicit-eval-when? 
				      eval-when-compile?
				      eval-when-load?)))))
		 (else
		  (let ((scode 
			 (begin
			   (if (logging-events?)
			       (begin
				 (princ " ")
				 (princ sexpr-id)
				 (princ " ")))
			   (clc-log-event start-syntaxing sexpr-id)
			   (let ((r (syntax-sexpr expanded-sexpr syntax-table sexpr-id)))
			     (clc-log-event end-syntaxing sexpr-id)
			     r)))
			(write? (or (not explicit-eval-when?)
				    eval-when-load?))
			(eval? (or (and (not explicit-eval-when?)
					(normally-compile-time-eval? expanded-sexpr))
				   (and explicit-eval-when?
					eval-when-compile?))))
		    (if write?
			(begin
			  (tconc-write! scode-exprs scode)
			  (if compile?
			      (cp-compile-scode scode brtl? binf? com-path))))
		    (if eval?
			(begin
			  (cp-write-files)
			  (fluid-let ((*defmacro-internal-env* *syntax-time-global-env*))
			    (scode-eval scode *commonlisp-user-environment*))))
		    (set! sexpr-id (1+ sexpr-id))
		    (compile-loop (force-cdr sexprs) (1+ nobjects)
				  explicit-eval-when?
				  eval-when-compile?
				  eval-when-load?))))))))
	  (fluid-let ((cp-check-log-events? #f))
	    (do () 
		((eq? (cp-check) 'empty))
	      (reschedule)))))
      com-path)))

;;; Reschedule-self is called multiple times so that work queued
;;;  as a result of one reschedule-self (e.g., the waiter of
;;;  compiled code (see make-process)), is guaranteed to get done
;;;  (in a serial processing environment) before we add more stuff to be done.
;;;  This avoids a big chunk of compilation stuff to do at the end.

(define (reschedule)
  (if (= (n-interpreters) 1)
      (begin
	(reschedule-self)
	(reschedule-self)
	(reschedule-self))))

(define (progn? sexpr)
  (and (pair? sexpr)
       (memq (car sexpr) '(progn begin))))

(define (eval-when? sexpr)
  (and (pair? sexpr)
       (eq? (car sexpr) 'eval-when)))

(define (eval-when-components sexpr cont)
  (if (null? (cdr sexpr))
      (error "Eval-when: bad format ~s" sexpr)
      (let ((situation (cadr sexpr)))
	(if (not (pair? situation))
	    (error "Eval-when: bad situation format ~s ~s" sexpr situation))
	(let ((body (cddr sexpr)))
	  (if (not (pair? body))
	      (error "Eval-when: bad body format ~s ~s" sexpr body))
	  (cont situation body)))))

(define (file-split-marker? sexpr)
  (eq? sexpr '$split-file))

(define (normally-compile-time-eval? sexpr)
  (and (pair? sexpr)
       (memq (car sexpr)
	     '(make-package in-package shadow shadowing-import
               export unexport use-package unuse-package import
	       proclaim defmacro-internal defvar-internal-decl 
	       defparameter-internal defconstant-internal
	       defstruct-internal))))

(define (make-noise expr noisy?)
  (if noisy?
      (begin
	(if (and (pair? expr)
		 (symbol? (car expr)))
	    (if (not (memq (car expr) no-noise-names))
		(let ((s (with-output-to-string
			   (lambda ()
			     (newline)
			     (write-string ";;; Compiling ")
			     (write-string "(")
			     (princ (car expr))
			     (if (null? (cdr expr))
				 (write-string ")")
				 (begin
				   (write-string " ")
				   (if (and (pair? (cdr expr))
					    (symbol? (cadr expr)))
				       (begin
					 (princ (cadr expr))
					 (write-string " ..."))
				       (write-string "..."))))))))
		  (write-string s)))
	    (begin
	      (newline)
	      (write-string ";;; Compiling top level form ..."))))))

(define no-noise-names
  '(
    defmacro-internal defvar-internal-decl defvar-internal
    defparameter-internal defconstant-internal
    defstruct-internal
    ))

(define (compile-file-ccwfofs com-name bin-name brtl-name binf-name proc)
  (ccwfof com-name
    (lambda (com-fd)
      (ccwfof bin-name
        (lambda (bin-fd)
	  (ccwfof brtl-name
            (lambda (brtl-fd)
	      (ccwfof binf-name
	        (lambda (binf-fd)
		  (proc com-fd bin-fd brtl-fd binf-fd))))))))))

;;; Conditional-Call-With-Fasl-Output-File
;;;
;;; Not only is it conditional, but contains bells and whistles
;;; for ensuring the integrity of the specified file.
;;; Proc is called with a temp file for output. If control
;;; leaves proc normally, the given file is deleted and the temp
;;; renamed to it. Otherwise, the temp file is deleted and the
;;; original is untouched. We attempt here to keep the operations
;;; as atomic as possible; other processes could still do some damage,
;;; though we feel it is worth the risk.
;;; This could be improved somewhat by sensing errors from the deletion
;;; and rename, and not removing the temp and leaving the original
;;; alone. This could allow the user to correct the situation by hand,
;;; e.g., if there were protection problems.

(define (ccwfof name proc)
  (if name
      (let* ((path (->pathname name))
	     (temp 
	      (pathname-new-type path 
				 (string-append 
				  (pathname-type path)
				  "-tmp-"
				  (number->string (real-time-clock)))))
	     (delete-and-rename-done? #f))
	(dynamic-wind
	 (lambda () #f)
	 (lambda ()
	   (let ((result
		  (call-with-fasl-output-file temp proc)))
	     (without-interrupts
	      (lambda ()
		(if (file-exists? name) 
		    (delete-file name))
		(rename-file temp name)
		(set! delete-and-rename-done? #t)))
	     result))
	 (lambda ()
	   (if (not delete-and-rename-done?)
	       (if (file-exists? temp)
		   (delete-file temp))))))
      (proc #f)))

;;; sexpr-id is passed only for debugging purposes

(define (syntax-sexpr sexpr syntax-table sexpr-id)
  (let ((r))
    (clc-log-event start-syntax-expr 0 sexpr-id)
    (set! r (syntax* (list sexpr) syntax-table))
    (clc-log-event end-syntax-expr 0 sexpr-id)
    (clc-log-event start-transform-expr 0 sexpr-id)
    (set! r (transform/expression r))
    (clc-log-event end-transform-expr 0 sexpr-id)
    (clc-log-event start-integrate-expr 0 sexpr-id)
    (set! r (transmit-values r integrate/expression))
    (clc-log-event end-integrate-expr 0 sexpr-id)
    (clc-log-event start-cgen-expr 0 sexpr-id)
    (set! r (transmit-values r (lambda (operations environment expression)
				 (cgen/expression expression))))
    (clc-log-event end-cgen-expr 0 sexpr-id)
    r))

;;; Ye Olde "tconc" abstraction.

(define (tconc-create)
  (let ((x (list #f)))
    (cons x x)))

(define (tconc-close! x)
  (let ((result (cdar x)))
    (set-car! x #f)
    (set-cdr! x #f)
    result))

(define (tconc-write! x data)
  (set-cdr! (cdr x) (list data))
  (set-cdr! x (cddr x))
  x)

(define (tconc-empty? x)
  (eq? (car x) (cdr x)))

(define (tconc-list x)
  (cdar x))

(define (tconc-head x)
  (if (tconc-empty? x)
      #f
      (cadar x)))

(define tconc-push! tconc-write!)

(define (tconc-pull! x)
  (let ((result (tconc-head x)))
    (and result
	 (begin
	   (set-cdr! (car x) (cddar x))
	   (if (null? (cdar x)) (set-cdr! x (car x)))
	   result))))


;;; Abstraction for compiled objects

;;; A compiled object is a list (code rtl inf)

(define make-compiled-object list)

(define compiled-object-code first)
(define compiled-object-brtl second)
(define compiled-object-binf third)

;;; Our own process abstraction, with value and atomicity properties
;;; appropriate to this application. We need not worry about touch;
;;; we must always inquire if a value is ready and explicitly 
;;; retrieve it if so.

;;; This is a variant on %spawn-process that allows us to 
;;; test for readiness of a value, without running into problems
;;; with splicing-out of values by GC. 
;;; Also, we hold onto the future itself in this structure, so it is not
;;; GCed away since nothing is waiting for it.

(define (make-process thunk)
  (let ((result (list 'process 'pending #f)))
    (set-car! (cddr result) (future (set-car! (cdr result) (thunk))))
    result))

(define (process? x)
  (and (pair? x)
       (eq? (car x) 'process)))

(define (determined-process? x)
  (and (process? x)
       (not (eq? (cadr x) 'pending))))

(define (process-value x)
  (cadr x))

;;; Gist Event list

(define-event-list event-list
  (
   (start-compilation)
   (end-compilation)
   (start-compile-scode "Expr %d")
   (end-compile-scode "Expr %d")
   (start-write-files)
   (end-write-files)
   (start-macroexpand)
   (end-macroexpand)
   (start-syntaxing "Expr %d")
   (end-syntaxing "Expr %d")
   (start-syntax-expr)
   (end-syntax-expr)
   (start-transform-expr)
   (end-transform-expr)
   (start-integrate-expr)
   (end-integrate-expr)
   (start-cgen-expr)
   (end-cgen-expr)
   (cp-check-start-process)
   (cp-check-enter)
   (cp-check-write-files)
   (cp-check-determined-process)
   (cp-check-pending-process)
   (cp-compile-scode-enter "Expr %d")
   ))

;;; Gist aids

(define (clc-log-event name #!optional data debug-info)
  (if (unassigned? data)
      (set! data 0))
  (if (unassigned? debug-info)
      (set! debug-info #f))
  (if debug?
      (print (list name (my-interpreter-number) (current-future) debug-info)))
  (log-event name data))

;;; Random

(define (the-work-queue)
  (weak-list->list (vector-ref (get-fixed-objects-vector) 
			       (fixed-objects-vector-slot 'the-work-queue))))

(define (weak-list->list x)
  (cond
   ((future? x)
    x)
   ((pair? x)
    (cons (weak-list->list (car x))
	  (weak-list->list (cdr x))))
   ((eq? (object-type x) 'weak-cons)
    (cons (weak-list->list (weak-car x))
	  (weak-list->list (weak-cdr x))))
   (else
    x)))

))
