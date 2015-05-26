;;; -*-Scheme-*-
;;;
;;;	$Header: bf-run.scm,v 1.2 89/02/03 10:39:23 las Exp $
;;;	$MIT-Header: runmd.scm,v 13.106 88/03/14 16:35:40 GMT jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; CScheme Runtime System

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  microcode-identify
	  substring=?
	  string-length
	  vector-ref
 	  tty-read-char
 	  tty-write-string
	  tty-flush-output
 	  substring-move-right!
 	  string-length
 	  string-allocate
	  with-interrupt-mask))

(define system-global-environment (the-environment))
(define interrupt-mask-gc-ok 7)		;GC & Stack overflow only
(define interrupt-mask-none  0)		;Absolutely everything off
(define interrupt-mask-all   -1)	;Normal: all enabled
(with-interrupt-mask interrupt-mask-none
   (lambda (all-on)
     (set! interrupt-mask-all all-on)))

(define restart-zap-stream)
(define zap-stream-future)
(define butterfly-exit)
(define single-processor-exit)
(define exit)
(define get-command-line-option)
(define get-command-line)
(define newline-char)
(define scheme-start-bfio)
(define is-command-line-option?)
(define butterfly-io-active? #f)

(define is-a-butterfly?
  (let ((identity (microcode-identify)))
    (let ((system (vector-ref identity 9)))
      (substring=? system 0 (string-length system) "mach (butterfly)" 0 16))))

(define (current-window->output-port)
  console-output-port)

(define load-compiled? #f)

(define (ask-if-compiled)
  (tty-write-string "Load compiled? (y or n) ")
  (let ((answer (tty-read-char)))
    (cond ((eq? answer #\y)
	   (set! load-compiled? #t))
	  ((eq? answer #\n)
	   (set! load-compiled? #f))
	  (else
	   (ask-if-compiled)))))

(ask-if-compiled)

(define runtime-system
  (make-environment

(define :name "Runtime")
(define :version 14)
(define :modification 80)
(define :files ())

(define runtime-identification		;RCS sets up this string.
  "$Header: bf-run.scm,v 1.2 89/02/03 10:39:23 las Exp $")

(define (:load)

  (let ((garbage-collector (binary-fasload "gc.bin")))
    (scode-eval garbage-collector system-global-environment)
    (purify garbage-collector #t))

  (set! newline-char (vector-ref (microcode-identify) 5))

  (boot-load-files
   (cdr cold-files-1))
  (boot-load-files
   (cons (cons (pathname-definition-filename) (cdar cold-files-2))
	 (cdr cold-files-2)))
   
  ((access snarf-version microcode-system))
  ((access install interrupt-system))
  ((access install error-system))
  ((access reset! working-directory-package))
  (purify (get-fixed-objects-vector) false))

(let-syntax ((define-primitive
	       (macro (name)
		 `(DEFINE ,name ,(make-primitive-procedure name)))))
  (define-primitive binary-fasload)
  (define-primitive binary-fasload-fd)
  (define-primitive fasopen-read)
  (define-primitive fasclose)
  (define-primitive tty-write-string)
  (define-primitive tty-write-char)
  (define-primitive tty-flush-output)
  (define-primitive microcode-identify)
  (define-primitive make-io-error-code))

;;;
;;; Loads a list of file descriptors which are of the form:
;;; 		"name"
;;;	or	("name" purify? environment1 environment2)
;;;
;;; This controls whether the item is purified or not,
;;; and tells which environment the first part is to
;;; be loaded into, and then into which environment the
;;; latter part is to be loaded into.

(define eof-error-code (make-io-error-code #xE0F))

(define (boot-load-files desc-list)
  (if (null? desc-list)
      'loaded
      (begin
	(boot-load-file (car desc-list))
	(boot-load-files (cdr desc-list)))))

(define (boot-load-file descriptor)
  (let ((fname (if (pair? descriptor) (car descriptor) descriptor))
	(purify? #t)
	(first-env system-global-environment)
	(rest-env system-global-environment))
    (if (pair? descriptor)
	(if (cdr descriptor)
	    (begin
	      (set! purify? (cadr descriptor))
	      (if (pair? (cddr descriptor))
		  (begin
		    (set! first-env (caddr descriptor))
		    (set! rest-env first-env)
		    (if (pair? (cdddr descriptor))
			(set! rest-env (cadddr descriptor))))))))
    (let ((fname (derive-file-name fname)))
      (tty-write-char newline-char)
      (tty-write-string fname)
      (tty-flush-output)
      (let ((fd (fasopen-read fname)))
	(let load-loop ((scode (binary-fasload-fd fd #f))
			(env first-env))
	  (if (eq? scode eof-error-code)
	      (begin
		(fasclose fd)
		(set! :files (cons fname :files)))
	      (begin
		(tty-write-string " loaded")
		(tty-flush-output)
		(set! scode (purify scode purify?))
		(tty-write-string " purified")
		(tty-flush-output)
		(scode-eval scode 
			    (if env
				(lexical-reference system-global-environment env)
				system-global-environment))
		(tty-write-string " evaluated")
		(tty-flush-output)
		(load-loop (binary-fasload-fd fd #f)
			   rest-env))))))))

(define (derive-file-name file)
  (if load-compiled?
      file
      (let ((name (string-allocate (string-length file)))
	    (length (string-length file)))
	(substring-move-right! file 0 (- length 4) name 0)
	(substring-move-right! ".bin" 0 4 name (- length 4))
	name)))

(define cold-files-1
  '(("gc.bin" #T)			;The GC interface.
    ("bf-gc.bin" #T)

    ;; Microcode Description
    ("utabs.com" #T)			;Microcode system.
    ("implmd.com" #T)			;CScheme dependent items.

    ;; Basic Utilities
    ("boot.com" #T)			;Most basic of primitives.
    ("fixart.com" #T)			;Fixnum arithmetic.
    ("narith.com" #T)			;Generic arithmetic.
    ("equals.com" #T)			;Equality.
    ("list.com" #T)			;Lists.
    ("vector.com" #T)			;Object vectors.
    ("string.com" #T)			;Strings.
    ("char.com" #T)			;Characters.
    ("bitstr.com" #T)			;Bit strings.
    ("wind.com" #T)			;Dynamic winder (FLUID-LET).

    ;; SCode Abstraction
    ("sdata.com" #T)			;Data field abstraction.
    ("scode.com" #T)			;SCODE simple abstractions.
    ("scomb.com" #T)			;SCODE Combinator abstractions.
    ("lambda.com" #T)			;SCODE Lambda abstraction.
    ("scan.com" #T)			;Scan out definitions.
    ("ustruc.com" #T)			;Microcode data structures abstraction.

    ;; Stack Parser
    ("histry.com" #T)			;History abstraction.
    ("stackp.bin" #T)			;Continuation parser machinery.
    ("spmd.com" #T)			;CScheme stack parser

    ;; Syntax
    ("numpar.com" #T)			;Parse characters -> numbers.
    ("numunp.com" #T)			;Unparse numbers -> characters.
    ("parse1.com" #T)			;Parsing characters -> objects.
    ("parse2.com" #T parser-package)	;Parsing characters -> objects.
    ("parse3.com" #T parser-package)	;Parsing characters -> objects.
    ("unpars.com" #T)			;Unparsing objects -> characters.
    ("syntax.com" #T)			;S-expressions -> SCode.
    ("syntax1.com" #T syntaxer-package) ;S-expressions -> SCode.
    ("cl-decls.com" #T syntaxer-package) ; commonlisp decls procession
    ("cl-syntax.com" #T syntaxer-package) ; commonlisp syntax extensions
    ("cl-optimp.com" #T imperative-optimizer-package) ; imperative optimizer

    ;; Pathnames
    ("pathnm.bin" #T)			;Pathname abstraction.
    ("complt.bin" #T)			;Pathname completion.
    ))

(define pathname-definition-associations
  ;; This should be extended as more parsers become available.
  '(("unix" "unxpth.bin")
    ("vms" "vmspth.bin")
    ("chrysalis" "unxpth.bin")))
    
(define (pathname-definition-filename)
  (cadr (or (assoc (microcode-identification-item 'OS-NAME-STRING)
		   pathname-definition-associations)
	    '("unknown" "unkpth.bin"))))

(define cold-files-2
  '(("pathmd.bin" #T)			;OS dependent pathname parsing.

    ;; I/O
    ("io.com" #T)			;I/O primitives.

    ("future.com" #T () scheduler)	;Futures

    ("input.com" #F)			;
    ("output.com" #F)			;
    ("keyint.bin" #T)			;Keyboard Interrupt Control.

    ;; Top level
    ("rep.bin" #T)			;Read-Eval-Print loop.
    ("bf-rep.bin" #T)
    ("repuse.com" #T)			;REP User Interface.
    ("bf-repu.com" #T)
    ("intrpt.bin" #T)			;Interrupt system.
    ("bf-intr.bin" #T)
    ("error.bin" #T)			;Error system.
    ("bf-error.bin" #T)
    ("world.com" #T)			;World (band) operations.
    ))

(define (finish-load)

  (if is-a-butterfly?
      (set! extra-files (append extra-files butterfly-files)))

  (boot-load-files extra-files)

  (add-system! microcode-system)
  (add-system! runtime-system)
  ((access install! resource-usage-package))

  (in-package system-global-environment
    (define (add-syntax! name operation)
      (syntax-table-define system-global-syntax-table name operation)))

  (load-syntaxer)
  (bbn-initialize)
  *the-non-printing-object*)

(define (bbn-initialize)
  (in-package
   system-global-environment
   (define cd
     (access set-working-directory-pathname! working-directory-package))
   (if is-a-butterfly?
       (begin
	(set! single-processor-exit %exit)
	(set! exit butterfly-exit)
	(set! %exit butterfly-exit))
       (set! exit %exit))
   (define nil '())
   (define t #t)
   (define user-global-interrupt-level 3)
   (set! undefined-conditional-branch #f) ; so that IF doesn't return TRUE when no alternate and predicate FALSE
   (define (print x #!optional y)
     (if (unassigned? y)
	 (begin
	   (newline)
	   (write x))
	 (begin
	   (newline y)
	   (write x y))))
   (define tty-write-char (make-primitive-procedure 'tty-write-char))
   (define tty-write-string (make-primitive-procedure 'tty-write-string))
   (define tyi read-char)
   (define prin1 write)
   (define princ display)
   (define terpri newline)
   (define *read-file* read-file)
   (define *default-global-syntax-table*
     (make-syntax-table system-global-syntax-table))
   (define (*get-default-syntax-table*)
     *default-global-syntax-table*)
   (define non-touching-primitive-type? 
     (make-primitive-procedure 'non-touching-primitive-type?))
   (deep-fluid-let!)
   (add-event-receiver! event:after-restore gc-flip)
   (if is-a-butterfly?
       (begin
	 (set! get-command-line-option
	       (make-primitive-procedure 'get-command-line-option))
	 (set! get-command-line
	       (make-primitive-procedure 'get-command-line))
	 (set! init-file-pathname
	       (->pathname
		(get-command-line-option "-init" "scheme.init")))
#|	 (add-event-receiver! event:after-restore scheme-start-bfio)    |#
	 #| (scheme-start-bfio) |# ))))

;;; Example of use: (is-command-line-option? '("-bfio" "-bf-io" "-console"))

(set! is-command-line-option?
      (named-lambda (is-command-line-option? option)
	(let ((cl (get-command-line)))
	  (let loop ((i (-1+ (vector-length cl))))
	    (if (< i 0)
		#f
		(if (if (pair? option)
			(member (vector-ref cl i) option)
			(equal? (vector-ref cl i) option))
		    #t
		    (loop (-1+ i))))))))

(set! scheme-start-bfio
      (named-lambda (scheme-start-bfio)
	((access install butterfly-io))))

(set! restart-zap-stream
      (named-lambda (restart-zap-stream)
	'done)
;;;      (named-lambda (restart-zap-stream)
;;;	(set! zap-stream-future		; So this doesn't get GC'd!
;;;	      (future
;;;	       (let zap-loop ()
;;;		 (write 
;;;		  (scode-eval
;;;		   (syntax
;;;		    (read (access butterfly-input-port butterfly-io))
;;;		    (rep-syntax-table))
;;;		   (rep-environment))
;;;		  (access butterfly-output-port butterfly-io))
;;;		 (newline (access butterfly-output-port butterfly-io))
;;;		 (zap-loop))
;;;	       "LISPM:zap-stream")))
      )

(set! butterfly-exit
      (named-lambda (butterfly-exit)
	(global-interrupt
	 3
	 (lambda (int-code int-enb) (single-processor-exit))
	 (lambda () #t))
	(single-processor-exit)))

(define extra-files
  '(
    ;; Some important system integration tools.
    "events.bin"
    "system.com"
    "sysclk.com"
    "resource.com"			;Resource measurement, including gc stats.

    ;; Some nice things.
    "macros.com"			;More special forms.
    "defstr.com"                        ;Structure definition macro
    "xlist.com"				;More list operations.
    "hash.com"				;Object hash and friends.
    "types.com"				;Type system.
    "stypes.com"			;Default system types.
    "crock.com"				;A terrible crock -- CPH.
    "datime.com"			;Date/Time.
    "msort.com"				;Merge Sort
    "gensym.com"			;Gensym.

    "sfile.com"				;Simple file commands.
    "format.com"			;Output Formatting.
    "unsyn.com"				;Unsyntaxer.
    "pp.com"				;Pretty Printer.

    ;; Debugging System
    "advice.bin"			;Advice (TRACE, BREAK).
    "comand.com"			;Debug command loops.
    "where.com"				;Environment inspector.
    "debug.com"				;Continuation inspector.
    "cdebug.com"			;Compiled code debug

    "bf-hacks.com"			;Parallelism hacks.
    "bf-load.com"			;Butterfly Loading Hacks
    "bf-trace.com"			;Parallel tracing hacks
    "defstru.com"			;Defstruct macros
    ("which.com" #t () which-package)   ;Task inspector.
    "elog.com"                          ;Gist event logger

    ))

(define butterfly-files
  '(
    ;; Butterfly I/O
    "bf-io.com"				;Butterfly Only Input/Output
    "bf-hard.com"			;Butterfly Hardware Trap
    ))
;;; end RUNTIME-SYSTEM.
))

((access :load runtime-system))

(define user-initial-environment
  (make-environment))

(define user-initial-syntax-table
  (make-syntax-table system-global-syntax-table))

(define user-initial-prompt-string
  "]=>")

(define user-initial-prompt
  (standard-rep-prompt user-initial-prompt-string))

(define replace-rep!)

(define delayed? promise?)		; Very important for the BFLY

(let ()
  (define (warm-boot-loop rep)
    (warm-boot-loop
     (call-with-current-continuation
       (lambda (continuation)
	 (set! replace-rep! continuation)
	 ;; Kludge to prevent holding REP.
	 ((set! rep))))))
  (warm-boot-loop
    (lambda ()
      (make-rep user-initial-environment
		user-initial-syntax-table
		user-initial-prompt
		console-input-port
		console-output-port
		(lambda ()
		  (write-string "
Cold load finished.  Type

((access finish-load runtime-system))

to load the rest of the system."))))))

