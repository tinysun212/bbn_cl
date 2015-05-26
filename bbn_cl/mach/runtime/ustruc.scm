;;; -*-Scheme-*-
;;;
;;;	$Header: ustruc.scm,v 13.91 88/08/31 09:13:28 jinx Exp $
;;;	$MIT-Header: ustruc.scm,v 13.54 88/03/14 16:36:53 GMT jinx Rel $
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

;;; Note: This file must be after the SCODE abstraction in the boot sequence.

(declare (usual-integrations))

;;;; Compiled Code

(define compiled-procedure?)
(define compiled-entry-type)
(define compiled-code-address?)
(define compiled-procedure-arity)

(let ((c-type (microcode-type 'COMPILED-ENTRY))
      (entry-info (make-primitive-procedure 'COMPILED-ENTRY-KIND 1))
      (procedure-kind 0)
      (kinds '((0 . COMPILED-PROCEDURE)
	       (1 . COMPILED-RETURN-ADDRESS)
	       (2 . COMPILED-EXPRESSION))))

  (define (entry-type entry)
    (let ((place (assq (system-hunk3-cxr0 (entry-info entry)) kinds)))
      (if place
	  (cdr place)
	  'COMPILED-ENTRY)))

  (set! compiled-entry-type
	(named-lambda (compiled-entry-type object)
	  (if (primitive-type? c-type object)
	      (entry-type object)
	      (error "compiled-entry-type: bad compiled entry"
		     object))))

  (set! compiled-procedure?
	(named-lambda (compiled-procedure? object)
	  (and (primitive-type? c-type object)
	       (eq? (entry-type object) 'COMPILED-PROCEDURE))))

  (set! compiled-code-address?
	(named-lambda (compiled-code-address? object)
	  (primitive-type? c-type object)))

  (set! compiled-procedure-arity
	(named-lambda (compiled-procedure-arity object)
	  (or (and (primitive-type? c-type object)
		   (let ((info (entry-info object)))
		     (and (= procedure-kind (system-hunk3-cxr0 info))
			  (cons (-1+ (system-hunk3-cxr1 info))
				(let ((max (system-hunk3-cxr2 info)))
				  (if (negative? max)
				      '()
				      (-1+ max)))))))
	      (error "compiled-procedure-arity: bad compiled procedure"
		     object))))
  )

;; These are bogus, but...

(define (compiled-procedure-entry procedure)
  (if (compiled-procedure? procedure)
      procedure
      (error "Not a compiled procedure" procedure)))

(define (compiled-procedure-environment procedure)
  (if (compiled-procedure? procedure)
      '()
      (error "Not a compiled procedure" procedure)))

(define compiled-code-address->block
  (make-primitive-procedure 'COMPILED-CODE-ADDRESS->BLOCK))

(define compiled-code-address->offset
  (make-primitive-procedure 'COMPILED-CODE-ADDRESS->OFFSET))

(define compiled-code-block?
  (let ((type (microcode-type 'COMPILED-CODE-BLOCK)))
    (named-lambda (compiled-code-block? object)
      (primitive-type? type object))))

(define (compiled-code-block/read-file filename)
  (map compiled-code-address->block 
       (fasload-multiple filename)))

;;;; Procedure

(define procedure?)
(define compound-procedure?)

(let ((n-type (microcode-type 'PROCEDURE))
      (p-type (microcode-type 'PRIMITIVE))
      (x-type (microcode-type 'EXTENDED-PROCEDURE)))

  (set! compound-procedure?
	(named-lambda (compound-procedure? object)
	  (or (primitive-type? n-type object)
	      (primitive-type? x-type object))))

  (set! procedure?
	(named-lambda (procedure? object)
	  (or (primitive-type? p-type object)
	      (primitive-type? n-type object)
	      (primitive-type? x-type object)
	      (compiled-procedure? object)))))

(define procedure-lambda)
(define procedure-environment)
(define procedure-components)
(define procedure-arity)

(define procedure-package
  (make-environment

(define select-lambda system-pair-car)
(define modify-lambda! system-pair-set-car!)
(define select-environment system-pair-cdr)
(define modify-environment! system-pair-set-cdr!)

(define ((select-components procedure) receiver)
  (receiver (select-lambda procedure)
	    (select-environment procedure)))

(define ((type-dispatch name p-operation n-operation #!optional c-operation) procedure)
  ((cond ((compound-procedure? procedure) n-operation)
	 ((primitive-procedure? procedure) p-operation)
	 ((compiled-procedure? procedure)
	  (if (unassigned? c-operation)
	      (error "Compiled procedure" name procedure)
	      c-operation))
	 (else (error "Not a procedure" name procedure)))
   procedure))

(define (compound-system-operation name operation)
  (type-dispatch name (compound-operation-error name) operation))

(define ((compound-operation-error name) procedure)
  (error "Not a compound procedure" name procedure))

(define (user-operation name p-operation c-operation)
  (type-dispatch name p-operation (trap-internal-lambdas name c-operation)))

(define ((trap-internal-lambdas name operation) procedure)
  (if ((access is-internal-lambda? lambda-package) (select-lambda procedure))
      (error "Internal procedure encountered -- get a wizard" name procedure)
      (operation procedure)))

(define system-procedure-lambda
  (compound-system-operation 'SYSTEM-PROCEDURE-LAMBDA select-lambda))

(define system-procedure-environment
  (compound-system-operation 'SYSTEM-PROCEDURE-ENVIRONMENT
			     select-environment))

(set! procedure-lambda
      (user-operation 'PROCEDURE-LAMBDA
		      (compound-operation-error 'PROCEDURE-LAMBDA)
		      select-lambda))

(set! procedure-environment
      (user-operation 'PROCEDURE-ENVIRONMENT
		      ;; (lambda (primitive-procedure) system-global-environment)
		      (compound-operation-error 'PROCEDURE-ENVIRONMENT)
		     select-environment))

(set! procedure-components
(named-lambda (procedure-components procedure receiver)
  ((procedure-components-getter procedure) receiver)))

(define procedure-components-getter
  (user-operation 'PROCEDURE-COMPONENTS
		  (compound-operation-error 'PROCEDURE-COMPONENTS)
		  select-components))

(define (primitive-procedure-standard-arity proc)
  (let ((arity (primitive-procedure-arity proc)))
    (if (negative? arity)
	(cons 0 '())
	(cons arity arity))))
       
(define (compound-procedure-arity proc)
  (lambda-components (select-lambda proc)
    (lambda (name required optional rest auxiliary decl body)
      (let ((r (length required)))
	(cons r
	      (if rest
		  '()
		  (+ r (length optional))))))))

(set! procedure-arity
      (type-dispatch 'PROCEDURE-ARITY
		     primitive-procedure-standard-arity
		     compound-procedure-arity
		     compiled-procedure-arity))

))

;;;; Reference traps

(define reference-trap?)
(define unbound-reference-trap?)
(define map-reference-trap)
(define reference-trap-kind)
(define reference-trap-kind-name)

(let ((object-ref (make-primitive-procedure '&OBJECT-REF))
      (trap-type (microcode-type 'REFERENCE-TRAP))
      (trap-tag '(&REFERENCE-TRAP))
      (trap-max-immediate 9)

      ;; Slots for non immediate traps

      (tag-slot 0)
      (extra-slot 1)

      ;; Reference trap kinds

      (trap-unbound 2)
      (trap-unbound-dangerous 3)
      
      ;; The following list must agree with the
      ;; appropriate list in the microcode.

      (trap-kind-names
       #(UNASSIGNED UNASSIGNED-DANGEROUS UNBOUND UNBOUND-DANGEROUS
	 ILLEGAL ILLEGAL-DANGEROUS #F #F #F #F
	 NOP DANGEROUS FLUID FLUID-DANGEROUS
	 COMPILER-CACHED COMPILER-CACHED-DANGEROUS)))

  (set! reference-trap?
	(named-lambda (reference-trap? obj)
	  (and (pair? obj) (eq? (car obj) trap-tag))))

  (set! reference-trap-kind cadr)

  (set! map-reference-trap
	(named-lambda (map-reference-trap getter)
	  (if (not (primitive-type? trap-type (getter)))
	      (getter)
	      (let ((index (primitive-datum (getter))))
		(if (<= index trap-max-immediate)
		    (list trap-tag index #f)
		    (list trap-tag
			  (object-ref (getter) tag-slot)
			  (object-ref (getter) extra-slot)))))))

  (set! reference-trap-kind-name
	(lambda (kind)
	  (cond ((or (not (integer? kind)) (< kind 0))
		 (error "reference-trap-kind-name: bad reference trap kind"
			kind))
		((>= kind (vector-length trap-kind-names))
		 'UNKNOWN)
		(else
		 (or (vector-ref trap-kind-names kind) 'UNKNOWN)))))

  (set! unbound-reference-trap?
	(named-lambda (unbound-reference-trap? trap)
	  (let ((kind (cadr trap)))
	    (or (= kind trap-unbound)
		(= kind trap-unbound-dangerous)))))
  )

;;;; Environment extensions

(define environment-extension?)
(define environment-extension-procedure)
(define environment-extension-aux-list)
(define set-environment-extension-parent!)
(let ((parent-slot 0)
      (procedure-slot 1)
      (count-slot 2)
      (first-aux-slot 3))

(define (extension-ref slot name)
  (lambda (obj)
    (if (vector? obj)
	(vector-ref obj slot)
	(error "Not an environment extension" obj name))))

(define (extension-set! slot name)
  (lambda (obj value)
    (if (vector? obj)
	(vector-set! obj slot value)
	(error "Not an environment extension" obj name))))

(define (filter-potentially-dangerous aux-list)
  (if (null? aux-list)
      '()
      (let ((value (map-reference-trap (lambda () (cdar aux-list))))
	    (rest (filter-potentially-dangerous (cdr aux-list))))
	(if (or (not (reference-trap? value))
		(not (unbound-reference-trap? value)))
	    (cons (car aux-list) rest)
	    rest))))

(set! environment-extension?
  (named-lambda (environment-extension? obj)
    (vector? obj)))

(set! environment-extension-procedure
  (extension-ref procedure-slot 'ENVIRONMENT-EXTENSION-PROCEDURE))

(set! environment-extension-aux-list
  (named-lambda (environment-extension-aux-list obj)
    (if (not (vector? obj))
	(error "Not an environment extension" obj))
    (filter-potentially-dangerous
     (subvector->list obj
		      first-aux-slot
		      (+ first-aux-slot
			 (primitive-datum (vector-ref obj count-slot)))))))

(set! set-environment-extension-parent!
  (extension-set! parent-slot 'SET-ENVIRONMENT-EXTENSION-PARENT!))

)

;;;; Environment

(define environment?
  (microcode-type-predicate 'ENVIRONMENT))

(define environment-procedure)
(define environment-has-parent?)
(define environment-parent)
(define environment-bindings)
(define environment-arguments)

(define environment-package
  (make-environment

(define null-environment
  (primitive-set-type (microcode-type 'NULL) 1))

(define system-procedure-lambda
  (access system-procedure-lambda procedure-package))

(define system-procedure-environment
  (access system-procedure-environment procedure-package))

(define ((environment-operation name operation global) environment)
  (cond ((environment? environment)
	 (let ((procedure (select-procedure environment)))
	   (let ((lambda (system-procedure-lambda procedure)))
	     (if ((access has-internal-lambda? lambda-package) lambda)
		 (error "External environment frame encountered" name)
		 (operation (if ((access is-internal-lambda? lambda-package)
				 lambda)
				(system-procedure-environment procedure)
				environment)
			    environment)))))
	((eq? environment system-global-environment)
	 (global name))
	(else (error "Not an environment" name environment))))

(define (global-environment-error name)
  (error "Operation not implemented for global environment" name))

(define (select-extension environment)
  (system-vector-ref environment 0))

(define (select-procedure environment)
  (let ((object (select-extension environment)))
    (if (environment-extension? object)
	(environment-extension-procedure object)
	object)))

(define (select-parent environment)
  (system-procedure-environment (select-procedure environment)))

(define (select-lambda environment)
  (system-procedure-lambda (select-procedure environment)))

(define (environment-accessible? environment name)
  (not (lexical-unbound? environment name)))

(define (environment-value environment name)
  (if (lexical-unassigned? environment name)
      '()
      (list (lexical-reference environment name))))

(define environment-extension
  (environment-operation 'ENVIRONMENT-EXTENSION
   (lambda (external-environment internal-environment)
     (select-extension external-environment))
   global-environment-error))

(define (environment-aux-list env)
  (let ((extension (environment-extension env)))
    (if (environment-extension? extension)
	(environment-extension-aux-list extension)
	'())))    

(set! environment-procedure
  (environment-operation 'ENVIRONMENT-PROCEDURE
    (lambda (external-environment internal-environment)
      (select-procedure external-environment))
    global-environment-error))

(set! environment-has-parent?
  (environment-operation 'ENVIRONMENT-HAS-PARENT?
    (lambda (external-environment internal-environment)
      (not (eq? (select-parent external-environment) null-environment)))
    (lambda (name) false)))

(set! environment-parent
  (environment-operation 'ENVIRONMENT-PARENT
    (lambda (external-environment internal-environment)
      (select-parent external-environment))
    global-environment-error))

(set! environment-bindings
  (environment-operation 'ENVIRONMENT-BINDINGS
    (lambda (external-environment internal-environment)
      (define (process names)
	(cond ((null? names) '())
	      ((environment-accessible? internal-environment (car names))
	       (cons
		(cons (car names)
		      (environment-value internal-environment (car names)))
		(process (cdr names))))
	      (else (process (cdr names)))))
      (process
       (map* (lambda-bound (select-lambda external-environment))
	     car
	     (environment-aux-list internal-environment))))
    global-environment-error))

(set! environment-arguments
  (environment-operation 'ENVIRONMENT-ARGUMENTS
    (lambda (external-environment internal-environment)
      (define (lookup name)
	(if (lexical-unassigned? internal-environment name)
	    (make-unassigned-object)
	    (lexical-reference internal-environment name)))
      (lambda-components* (select-lambda external-environment)
	(lambda (name required optional rest body)
	  (map* (let optional-loop ((names optional))
		  (cond ((null? names)
			 (if (null? rest) '() (lookup rest)))
			((lexical-unreferenceable? internal-environment
						   (car names))
			 '())
			(else
			 (cons (lookup (car names))
			       (optional-loop (cdr names))))))
		lookup
		required))))
    global-environment-error))

(define (system-environment-set-parent! environment parent)
  (define (clobber-procedure! procedure)
    (system-pair-set-cdr! procedure parent))

  (let ((extension (environment-extension environment)))
    (if (environment-extension? extension)
	(begin (set-environment-extension-parent! extension parent)
	       (clobber-procedure!
		(environment-extension-procedure extension)))
	(clobber-procedure! extension))))

(define system-environment-add-parent! system-environment-set-parent!)

(define (system-environment-remove-parent! environment)
  (system-environment-set-parent! environment null-environment))

(define (system-external-environment? environment)
  ((access has-internal-lambda? lambda-package) (select-lambda environment)))

))

;;;; Delayed Evaluation

(define promise?
  (microcode-type-predicate 'DELAYED))

(define (promise-forced? promise)
  (eq? true (system-pair-car promise)))

(define (promise-non-expression? promise)
  (eqv? 0 (system-pair-car promise)))

(define (promise-value promise)
  (if (promise-forced? promise)
      (system-pair-cdr promise)
      (error "Promise not yet forced" promise)))

(define (promise-expression promise)
  (cond ((promise-forced? promise)
	 (error "Promise already forced" promise))
	((promise-non-expression? promise)
	 (error "Promise has no expression" promise))
	(else
	 (system-pair-cdr promise))))

(define (promise-environment promise)
  (cond ((promise-forced? promise)
	 (error "Promise already forced" promise))
	((promise-non-expression? promise)
	 (error "Promise has no environment" promise))
	(else
	 (system-pair-car promise))))

#|

Compiled code blocks contain both nonmarked code and marked constants.

Code positions are referred to as OFFSETS, which start from the
beginning of the block and are measured in bytes.  The positions of
constants are referred to as INDICES, and use the normal index
numbering for vectors.  The conversion between offsets and indices is
specified by COMPILED-CODE-BLOCK/BYTES-PER-OBJECT, which should be set
to the correct value before these operations are used.

|#

(define compiled-code-block/bytes-per-object)

(define (compiled-code-block/index->offset index)
  (* (1+ index) compiled-code-block/bytes-per-object))

(define (compiled-code-block/offset->index offset)
  (-1+ (quotient offset compiled-code-block/bytes-per-object)))

(define (compiled-code-block/code-length block)
  (* compiled-code-block/bytes-per-object
     (primitive-datum (system-vector-ref block 0))))

(define (compiled-code-block/code-start block)
  (* compiled-code-block/bytes-per-object 2))

(define (compiled-code-block/code-end block)
  (+ (compiled-code-block/code-start block)
     (compiled-code-block/code-length block)))

(define (compiled-code-block/constants-start block)
  (1+ (primitive-datum (system-vector-ref block 0))))

(define (compiled-code-block/constants-end block)
  (- (system-vector-size block) 2))

(define (compiled-code-block/debugging-info? block)
  (not (memq (compiled-code-block/debugging-info block) '(#F DEBUGGING-INFO))))

(define (compiled-code-block/debugging-info block)
  (system-vector-ref block (- (system-vector-size block) 2)))

(define (set-compiled-code-block/debugging-info! block info)
  (system-vector-set! block (- (system-vector-size block) 2) info))

(define (compiled-code-block/environment block)
  (system-vector-ref block (-1+ (system-vector-size block))))
