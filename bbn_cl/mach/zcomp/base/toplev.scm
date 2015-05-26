#| -*-Scheme-*-

$Header: toplev.scm,v 1.11 89/02/07 15:09:32 las Exp $
$MIT-Header: toplev.scm,v 4.5 88/03/14 20:24:54 GMT jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Compiler Top Level

(declare (usual-integrations))

(define prim-values (make-primitive-procedure 'values))

(define code->code-space 
  (if is-a-butterfly?
      (make-primitive-procedure 'code->code-space)
      (lambda (x) x)))


;;; Global variables

(define *input-scode*)
(define *ic-procedure-headers*)
(define *root-block*)
(define *root-expression*)
(define *rtl-expression*)
(define *rtl-procedures*)
(define *rtl-continuations*)
(define *rtl-graphs*)
(define label->object)

;;; These variable names mistakenly use the format "compiler:..."
;;; instead of the correct format, which is "*...*".  Fix it sometime.
(define compiler:external-labels)
(define compiler:label-bindings)
(define compiler:block-label)
(define compiler:entry-label)
(define compiler:bits)
(define compiler:code-vector)
(define compiler:entry-points)
(define compiler:expression)

(define compiler:phase-wrapper (lambda (thunk) (thunk)))
(define compiler:process-time 0)
(define compiler:real-time 0)

(define (compiler:reset!)
  (set! *input-scode*)
  (set! *current-label-number*)
  (set! *constants*)
  (set! *blocks*)
  (set! *expressions*)
  (set! *procedures*)
  (set! *lvalues*)
  (set! *applications*)
  (set! *parallels*)
  (set! *assignments*)
  (set! *ic-procedure-headers*)
  (set! *root-expression*)
  (set! *root-block*)
  (set! *rtl-expression*)
  (set! *rtl-procedures*)
  (set! *rtl-continuations*)
  (set! *rtl-graphs*)
  (set! label->object)
  (set! *machine-register-map*)
  (set! compiler:external-labels)
  (set! compiler:label-bindings)
  (set! compiler:block-label)
  (set! compiler:entry-label)
  (set! compiler:bits)
  (set! compiler:code-vector)
  (set! compiler:entry-points)
  (set! compiler:expression))

(define (in-compiler thunk)
  (fluid-let ((compiler:process-time 0)
	      (compiler:real-time 0)
	      (*input-scode*)
	      (*current-label-number*)
	      (*constants*)
	      (*blocks*)
	      (*expressions*)
	      (*procedures*)
	      (*lvalues*)
	      (*applications*)
	      (*parallels*)
	      (*assignments*)
	      (*ic-procedure-headers*)
	      (*root-expression*)
	      (*root-block*)
	      (*rtl-expression*)
	      (*rtl-procedures*)
	      (*rtl-continuations*)
	      (*rtl-graphs*)
	      (label->object)
	      (*machine-register-map*)
	      (compiler:external-labels)
	      (compiler:label-bindings)
	      (compiler:block-label)
	      (compiler:entry-label)
	      (compiler:bits)
	      (compiler:code-vector)
	      (compiler:entry-points)
	      (compiler:expression))
    (compiler:reset!)
    (let ((value (thunk)))
      (if (not compiler:preserve-data-structures?)
	  (compiler:reset!))
      (if compiler:show-phases?
	  (compiler-time-report "Total compilation time"
				compiler:process-time
				compiler:real-time))
      value)))


;;;; The file compiler, its usual mode.

(define (cf input . options)
  (let ((syntax? #t)
	(noisy? #t)
	(output-file "")
	(b-files '(inf))
	(touchify? #t)
	(compile? #t))
    (let ((kernel
	   (lambda (source-file)
	     (let ((scode-file
		    (compiler-merge-pathnames
		     (pathname-new-type (->pathname output-file) "bin")
		     (->pathname source-file))))
	       ;; Maybe this should be done only if scode-file
	       ;; does not exist or is older than source-file.
	       (if syntax? 
		   (begin
		     (sf source-file scode-file)
		     (newline)))
	       (if compile?
		   (compile-bin-file-util scode-file output-file b-files))))))
      (if (and (pair? options)
	       (or (string? (car options))
		   (pathname? (car options))))
	  (begin
	    (set! output-file (car options))
	    (set! options (cdr options))))
      (let loop ((options options))
	(if (null? options)
	    #f
	    (let ((option (car options)))
	      (case option
		(touch
		 (set! touchify? (cadr options))
		 (set! options (cdr options)))
		(output
		 (set! output-file (cadr options))
		 (set! options (cdr options)))
		(syntax
		 (set! syntax? (cadr options))
		 (set! options (cdr options)))
		(noisy
		 (set! noisy? (cadr options))
		 (set! options (cdr options)))
		(b-files
		 (set! b-files (cadr options))
		 (if (eq? b-files #t) 
		     (set! b-files '(inf rtl)))
		 (set! options (cdr options)))
		(compile
		 (set! compile? (cadr options))
		 (set! options (cdr options)))
		(else (error "Bad option to cf" option)))
	      (loop (cdr options)))))
      (fluid-let ((compiler:touchify? touchify?)
		  (load-quietly? (not noisy?))
		  (compiler:show-phases? noisy?))
	(if (pair? input)
	    (for-each kernel input)
	    (kernel input))))))

(define (compile-bin-file input-string #!optional output-string)
  (compile-bin-file-util input-string
			 (and (not (unassigned? output-string)) output-string)
			 (if compiler:generate-rtl-files?
			     '(inf rtl)
			     '(inf))))

(define (compile-bin-file-util input-string output-string b-files)
  (compiler-pathnames input-string
		      output-string
		      (make-pathname false false false false "bin" 'NEWEST)
   (lambda (input-pathname output-pathname)
     (let ((input (compiler-fasload-multiple input-pathname)))
       (newline)
       (compile-scode-list input
			   (and (memq 'rtl b-files)
				(pathname-new-type output-pathname "brtl"))
			   (and (memq 'inf b-files)
				(pathname-new-type output-pathname "binf")))))))

;;;; Utilities for compiling in batch mode

(define compiler:batch-mode? false)
(define compiler:abort-handled? false)
(define compiler:abort-continuation)

(define (compiler:batch-compile input #!optional output)
  (fluid-let ((compiler:batch-mode? true)
	      ((access *error-hook* error-system)
	       (lambda (env mesg irr subst?)
		 (if compiler:abort-handled?
		     (begin
		       (write-string
			(with-output-to-string
			  (lambda ()
			    (newline)
			    (display "*** Error: ")
			    (display mesg)
			    (display " ***")
			    (newline)
			    (display "Irritant: ")
			    (write irr)
			    (newline))))
		       (compiler:abort false))
		     ((access standard-error-hook error-system)
		      env mesg irr subst?)))))
    (if (unassigned? output)
	(compile-bin-file input)
	(compile-bin-file input output))))

(define (compiler:abort value)
  (if compiler:abort-handled?
      (begin
	(write-string
	 (with-output-to-string
	   (lambda ()
	     (display "*** Aborting...")
	     (newline))))
	(compiler:abort-continuation value))
      (error "compiler:abort: Not set up to abort" value)))

(define (batch-kernel real-kernel)
  (lambda (input-string)
    (call-with-current-continuation
     (lambda (abort-compilation)
       (fluid-let ((compiler:abort-continuation abort-compilation)
		   (compiler:abort-handled? true))
	 (real-kernel input-string))))))

(define (compiler-pathnames input-string output-string default transform)
  (let* ((core
	  (lambda (input-string)
	    (let ((input-pathname
		  (pathname->input-truename
		   (compiler-merge-pathnames (->pathname input-string) default))))
	      (if (not input-pathname)
		  (error "File does not exist" input-string))
	      (let ((output-pathname
		     (compiler-merge-pathnames
		      (pathname-new-type (->pathname (or output-string "")) "com")
		      input-pathname)))
		(newline)
		(write-string
		 (with-output-to-string
		   (lambda ()
		     (write-string "Compile File: ")
		     (write (pathname->string input-pathname))
		     (write-string " => ")
		     (write (pathname->string output-pathname)))))
		(fasdump-multiple (transform input-pathname output-pathname)
				  output-pathname)))))
	 (kernel
	  (if compiler:batch-mode?
	      (batch-kernel core)
	      core)))
    (if (pair? input-string)
	(for-each kernel input-string)
	(kernel input-string))))

(define compiler-merge-pathnames merge-pathnames-no-dir-append)

(define (compiler-fasload-multiple pathname)
    (mapcar 
     (lambda (scode)
       (let ((scode
	      (if (scode/comment? scode)
		  (scode/comment-expression scode)
		  scode)))
	 (if (scode/open-block? scode)
	     (scode/open-block-components scode
               (lambda (names declarations body)
		 (if (null? names)
		     (scan-defines body
                       (lambda (names declarations* body)
			 (make-open-block names
					  (append declarations declarations*)
					  body)))
		     scode)))
	     (scan-defines scode make-open-block))))
     (fasload-multiple pathname)))


(define (compile-procedure procedure . options)
  (let ((touchify? #t)
	(noisy? #t))
    (let loop ((options options))
      (if (null? options)
	  #f
	  (let ((option (car options)))
	    (case option
	      (touch
	       (set! touchify? (cadr options))
	       (set! options (cdr options)))
	      (noisy
	       (set! noisy? (cadr options))
	       (set! options (cdr options)))
	      (else (error "Bad option to compile-procedure" option)))
	    (loop (cdr options)))))
    (fluid-let ((compiler:touchify? touchify?)
		(compiler:show-phases? noisy?))
      (scode-eval (code->code-space
		   (compile-scode 
		    (procedure-lambda procedure) false false))
		  (procedure-environment procedure)))))

(define (compile-scode-list scode-list
			    #!optional
			    rtl-output-pathname
			    info-output-pathname)
  (if (unassigned? rtl-output-pathname)
      (set! rtl-output-pathname false))
  (if (unassigned? info-output-pathname)
      (set! info-output-pathname false))
  (ccwfof rtl-output-pathname
    (lambda (rtl-fd)
      (ccwfof info-output-pathname
	(lambda (inf-fd)
	  (mapcar (lambda (scode) 
		    (compile-scode scode rtl-fd inf-fd info-output-pathname))
		  scode-list))))))

;;; conditional-call-with-fasl-output-file

(define (ccwfof name? proc)
  (if name?
      (call-with-fasl-output-file name? proc)
      (proc #f)))

(define (compile-scode scode #!optional rtl inf info-output-pathname)
  (if (unassigned? rtl) (set! rtl false))
  (if (unassigned? inf) (set! inf false))
  (let ((rtl-obj)
	(inf-obj))
    (let ((compiled-expression
	   (in-compiler
	    (lambda ()
	      (set! *input-scode* scode)
	      (if compiler:touchify?
		  (compiler-phase "Inserting touches"
		    (lambda ()
		      (set! *input-scode* ((access touchify touchifier-package) 
					   *input-scode*)))))
	      (phase/fg-generation)
	      (phase/fg-optimization)
	      (phase/rtl-generation)
	      (phase/rtl-optimization)
	      (set! rtl-obj (phase/rtl-file-output rtl))
	      (phase/bit-generation)
	      (phase/bit-linearization)
	      (phase/assemble)
	      (set! inf-obj (phase/info-generation-2 inf info-output-pathname))
	      (phase/link)
	      compiler:expression))))
      (prim-values compiled-expression rtl-obj inf-obj))))


(define (compiler-phase name thunk)
  (compiler-phase-control name thunk compiler:show-phases?))

(define (compiler-superphase name thunk)
  (compiler-phase-control name thunk compiler:show-subphases?))

(define (compiler-subphase name thunk)
  (compiler-phase-control name thunk compiler:show-subphases?))

(define (compiler-phase-control name thunk visible?)
  (if visible?
      (compiler-display-start-phase name))
  (compiler-time-phase thunk
    (lambda (process-delta real-delta)
      (if visible?
	  (compiler-time-report "      Time taken" process-delta real-delta)))))

(define (compiler-display-start-phase name)
  (write-string
   (with-output-to-string
     (lambda ()
       (display "    ")
       (display name)
       (display "...")
       (newline)))))

(define (compiler-time-phase thunk cont)
  (let ((process-start (process-time-clock))
	(real-start (real-time-clock)))
    (compiler:phase-wrapper thunk)
    (let ((process-delta (- (process-time-clock) process-start))
	  (real-delta (- (real-time-clock) real-start)))
      (set! compiler:process-time (+ process-delta compiler:process-time))
      (set! compiler:real-time (+ real-delta compiler:real-time))
      (cont process-delta real-delta))))

(define (compiler-time-report prefix process-time real-time)
  (write-string
   (with-output-to-string
     (lambda ()
       (write-string prefix)
       (write-string ": ")
       (write (/ process-time 1000))
       (write-string " (process time); ")
       (write (/ real-time 1000))
       (write-string " (real time)")
       (newline)))))

(define-macro (last-reference name)
  `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
       ,name
       (SET! ,name)))

(define (phase/fg-generation)
  (compiler-phase "Generating the Flow Graph"
    (lambda ()
      (set! *current-label-number* 0)
      (set! *constants* '())
      (set! *blocks* '())
      (set! *expressions* '())
      (set! *procedures* '())
      (set! *lvalues* '())
      (set! *applications* '())
      (set! *parallels* '())
      (set! *assignments* '())
      (set! *root-expression*
	    ((access construct-graph fg-generator-package)
	     (if compiler:preserve-data-structures?
		 *input-scode*
		 (set! *input-scode*))))
      (set! *root-block* (expression-block *root-expression*))
      (if (or (null? *expressions*)
	      (not (null? (cdr *expressions*))))
	  (error "Multiple expressions"))
      (set! *expressions*))))

(define (phase/fg-optimization)
  (compiler-superphase "Optimizing the Flow Graph"
    (lambda ()
      (phase/simulate-application)
      (phase/outer-analysis)
      (phase/fold-constants)
      (phase/open-coding-analysis)
      (phase/operator-analysis)
      (phase/identify-closure-limits)
      (phase/setup-block-types)
      (phase/continuation-analysis)
      (phase/simplicity-analysis)
      (phase/subproblem-ordering)
      (phase/connectivity-analysis)
      (phase/design-environment-frames)
      (phase/compute-node-offsets)
      (phase/fg-optimization-cleanup))))

(define (phase/simulate-application)
  (compiler-subphase "Simulating Applications"
    (lambda ()
      ((access simulate-application fg-optimizer-package)
       *lvalues*
       *applications*))))

(define (phase/outer-analysis)
  (compiler-subphase "Outer Analysis"
    (lambda ()
      ((access outer-analysis fg-optimizer-package)
       *root-expression*
       *procedures*
       *applications*))))

(define (phase/fold-constants)
  (compiler-subphase "Constant Folding"
    (lambda ()
      ((access fold-constants fg-optimizer-package)
       *lvalues*
       *applications*))))

(define (phase/open-coding-analysis)
  (compiler-subphase "Open Coding Analysis"
    (lambda ()
      ((access open-coding-analysis rtl-generator-package)
       *applications*))))

(define (phase/operator-analysis)
  (compiler-subphase "Operator Analysis"
    (lambda ()
      ((access operator-analysis fg-optimizer-package)
       *procedures*
       *applications*))))

(define (phase/identify-closure-limits)
  (compiler-subphase "Identifying Closure Limits"
    (lambda ()
      ((access identify-closure-limits! fg-optimizer-package)
       *procedures*
       *applications*
       *assignments*))))

(define (phase/setup-block-types)
  (compiler-subphase "Setting Up Block Types"
    (lambda ()
      ((access setup-block-types! fg-optimizer-package)
       *root-block*))))

(define (phase/continuation-analysis)
  (compiler-subphase "Continuation Analysis"
    (lambda ()
      ((access continuation-analysis fg-optimizer-package)
       *blocks*))))

(define (phase/simplicity-analysis)
  (compiler-subphase "Simplicity Analysis"
    (lambda ()
      ((access simplicity-analysis fg-optimizer-package)
       *parallels*))))

(define (phase/subproblem-ordering)
  (compiler-subphase "Ordering Subproblems"
    (lambda ()
      ((access subproblem-ordering fg-optimizer-package)
       *parallels*))))

(define (phase/connectivity-analysis)
  (compiler-subphase "Connectivity Analysis"
    (lambda ()
      ((access connectivity-analysis fg-optimizer-package)
       *root-expression*
       *procedures*))))

(define (phase/design-environment-frames)
  (compiler-subphase "Designing Environment Frames"
    (lambda ()
      ((access design-environment-frames! fg-optimizer-package)
       *blocks*))))

(define (phase/compute-node-offsets)
  (compiler-subphase "Computing Node Offsets"
    (lambda ()
      ((access compute-node-offsets fg-optimizer-package)
       *root-expression*))))

(define (phase/fg-optimization-cleanup)
  (compiler-subphase "Cleaning Up After Flow Graph Optimization"
    (lambda ()
      (if (not compiler:preserve-data-structures?)
	  (begin (set! *constants*)
		 (set! *blocks*)
		 (set! *procedures*)
		 (set! *lvalues*)
		 (set! *applications*)
		 (set! *parallels*)
		 (set! *assignments*)
		 (set! *root-block*))))))

(define (phase/rtl-generation)
  (compiler-phase "Generating RTL"
    (lambda ()
      (set! *rtl-procedures* '())
      (set! *rtl-continuations* '())
      (set! *rtl-graphs* '())
      (set! *ic-procedure-headers* '())
      (initialize-machine-register-map!)
      ((access generate/top-level rtl-generator-package)
       (if compiler:preserve-data-structures?
	   *root-expression*
	   (set! *root-expression*)))
      (set! label->object
	    (make/label->object *rtl-expression*
				*rtl-procedures*
				*rtl-continuations*))
      (for-each (lambda (entry)
		  (set-cdr! entry
			    (rtl-procedure/external-label
			     (label->object (cdr entry)))))
		*ic-procedure-headers*)
      (let ((n-registers
	     (map (lambda (rgraph)
		    (- (rgraph-n-registers rgraph)
		       number-of-machine-registers))
		  *rtl-graphs*)))
	(if compiler:show-phases?
	    (write-string
	     (with-output-to-string
	       (lambda ()
		 (write-string "      Registers used: ")
		 (write (apply max n-registers))
		 (write-string " max, ")
		 (write (apply min n-registers))
		 (write-string " min, ")
		 (write (/ (apply + n-registers) (length n-registers)))
		 (write-string " mean")
		 (newline)))))))))

(define (phase/rtl-optimization)
  (compiler-superphase "Optimizing RTL"
    (lambda ()
      (if compiler:cse?
	  (phase/common-subexpression-elimination))
      (phase/lifetime-analysis)
      (if compiler:code-compression?
	  (phase/code-compression))
      (phase/register-allocation)
      (phase/rtl-optimization-cleanup))))

(define (phase/common-subexpression-elimination)
  (compiler-subphase "Eliminating Common Subexpressions"
    (lambda ()
      ((access common-subexpression-elimination rtl-cse-package)
       *rtl-graphs*))))

(define (phase/lifetime-analysis)
  (compiler-subphase "Lifetime Analysis"
    (lambda ()
      ((access lifetime-analysis rtl-optimizer-package) *rtl-graphs*))))

(define (phase/code-compression)
  (compiler-subphase "Code Compression"
    (lambda ()
      ((access code-compression rtl-optimizer-package) *rtl-graphs*))))

(define (phase/rtl-file-output fd)
  (cond
   ((null? fd)
    #f)
   ((eq? fd #t)
    ((access linearize-rtl rtl-generator-package) *rtl-graphs*))
   (else
    (compiler-phase "RTL File Output"
      (lambda ()
	(fasdump-fd ((access linearize-rtl rtl-generator-package)
		     *rtl-graphs*)
		    fd)
	#f)))))

(define (phase/register-allocation)
  (compiler-subphase "Allocating Registers"
    (lambda ()
      ((access register-allocation rtl-optimizer-package) *rtl-graphs*))))

(define (phase/rtl-optimization-cleanup)
  (if (not compiler:preserve-data-structures?)
      (for-each (lambda (rgraph)
		  ;; **** this slot is reused. ****
		  ;;(set-rgraph-register-bblock! rgraph false)
		  (set-rgraph-register-crosses-call?! rgraph false)
		  (set-rgraph-register-n-deaths! rgraph false)
		  (set-rgraph-register-live-length! rgraph false)
		  (set-rgraph-register-n-refs! rgraph false))
		*rtl-graphs*)))

(define (phase/bit-generation)
  (compiler-phase "Generating BITs"
    (lambda ()
      (set! compiler:external-labels '())
      ((access generate-bits lap-syntax-package)
       *rtl-graphs*
       (lambda (block-label prefix)
	 (set! compiler:block-label block-label)
	 (node-insert-snode! (rtl-expr/entry-node *rtl-expression*)
			     (make-sblock prefix))))
      (set! compiler:entry-label (rtl-expr/label *rtl-expression*))
      (if (not compiler:preserve-data-structures?)
	  (begin (set! label->object)
		 (set! *rtl-expression*)
		 (set! *rtl-procedures*)
		 (set! *rtl-continuations*))))))

(define (phase/bit-linearization)
  (compiler-phase "Linearizing BITs"
    (lambda ()
      (set! compiler:bits
	    (append-instruction-sequences!
	     (lap:make-entry-point compiler:entry-label
				   compiler:block-label)
	     ((access linearize-bits lap-syntax-package)
	      (if compiler:preserve-data-structures?
		  *rtl-graphs*
		  (set! *rtl-graphs*))))))))

(define (phase/assemble)
  (compiler-phase "Assembling"
    (lambda ()
      (if compiler:preserve-data-structures?
	  ((access assemble bit-package)
	   compiler:block-label
	   compiler:bits
	   phase/assemble-finish)
	  ((access assemble bit-package)
	   (set! compiler:block-label)
	   (set! compiler:bits)
	   phase/assemble-finish)))))

(define (phase/assemble-finish count code-vector labels bindings linkage-info)
  (set! compiler:code-vector code-vector)
  (set! compiler:entry-points labels)
  (set! compiler:label-bindings bindings)
  (if compiler:show-phases?
      (write-string
       (with-output-to-string
	 (lambda ()
	   (display "      Branch tensioning done in ")
	   (write (1+ count))
	   (if (zero? count)
	       (display " iteration.")
	       (display " iterations."))
	   (newline))))))

(define (phase/info-generation-2 fd pathname)
  (cond
   ((null? fd)
    #f)
   ((eq? fd #t)
    (info-generation-2 pathname))
   (else
    (compiler-phase "Generating Debugging Information (pass 2)"
      (lambda ()
	(fasdump-fd (info-generation-2 pathname) fd)
	#f)))))

(define (info-generation-2 pathname)
  (let ((result
	 ((access generation-phase2 debugging-information-package)
	  compiler:label-bindings
	  (if compiler:preserve-data-structures?
	      compiler:external-labels
	      (set! compiler:external-labels)))))
  (set-compiled-code-block/debugging-info! compiler:code-vector
					   (pathname->string pathname))
  result))

(define (phase/link)
  (compiler-phase "Linking"
    (lambda ()
      ;; This has sections locked against GC since the code may not be
      ;; purified.
      (let ((bindings
	     (map (lambda (label)
		    (cons
		     label
		     (with-interrupt-mask interrupt-mask-none
		       (lambda (old)
			 ((ucode-primitive &make-object)
			  type-code:compiled-entry
			  (make-non-pointer-object
			   (+ (cdr (or (assq label compiler:label-bindings)
				       (error "Missing entry point" label)))
			      (primitive-datum compiler:code-vector))))))))
		  compiler:entry-points)))
	(let ((label->expression
	       (lambda (label)
		 (cdr (or (assq label bindings)
			  (error "Label not defined as entry point" label))))))
	  (set! compiler:expression (label->expression compiler:entry-label))
	  (for-each (lambda (entry)
		      (set-lambda-body! (car entry)
					(label->expression (cdr entry))))
		    *ic-procedure-headers*)))
      (set! compiler:code-vector)
      (set! compiler:entry-points)
      (set! compiler:label-bindings)
      (set! compiler:entry-label)
      (set! *ic-procedure-headers*))))
