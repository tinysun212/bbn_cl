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
;;; Commonlisp extensions:

(define syntax-THE-form
  (spread-arguments
   (lambda (type expr)
     (syntax-expression expr (if (and (touch-mode)
				      (or (not (eq? type 'non-future))
					  (and (pair? type)
					       (eq? (car type) 'non-future))))
				 (list 'or 'future type)
				 type)))))

;; Commonlisp needs syntax to create COMMENTs which are
;; used to cache away source code. 

(define syntax-CL-COMMENT-form
  (spread-arguments
   (lambda (text expr)
     (make-comment text
		   (syntax-expression expr)))))

;;;; The imperitive optimizer interface

(define optimize-imperative-constructs)

(set! imperative-optimizer-package (make-environment))

;;;; Utilities

(define (make-absolute-variable name)
  #|
  ;; access in global environment
  (make-access '() name)
  |#
  (make-variable name))

(define (absolute-variable? obj)
  #|
  (and (access obj)
       (access-components obj
	 (lambda (env name)
	   (eq? '() env))))
  |#
  (variable? obj))

(define (absolute-variable-name obj)
  #|
  (access-components obj
    (lambda (env name)
      name))
  |#
  (variable-name obj))
  

(define (make-simple-lambda required body)
  (external-make-lambda lambda-tag:unnamed
			required
			'()		; optional
			false		; rest
			'()		; auxiliary
			'()		; declarations
			body))

(define lambda-tag:tagbody-letrec
  (make-named-tag "TAGBODY-LETREC"))		       

(define (make-tagbody-letrec names values body)
  (make-combination
   (external-make-lambda
    lambda-tag:tagbody-letrec
    '()					; required
    '()					; optional
    false				; rest
    names				; auxiliary
    '()					; declarations
    (make-sequence
     (map* (list body)
	   (lambda (name value)
	     (make-assignment name value))
	   names
	   values)))
   '()))

(define (letrec->tagbody go-tag letrec-exp)
  (make-combination
   (make-absolute-variable '%TAGBODY-AUX%)
   (list (make-simple-lambda (list go-tag) letrec-exp))))

(define (make-block label body)
  (make-combination
   (make-absolute-variable '%BLOCK-AUX%)
   (list (make-simple-lambda (list label) body))))

(define (make-constant constant)
  constant)

;;;; CL compatibility utilities

(define (symbolp obj)
  ((symbol-function 'symbolp) obj))

(define (->symbol base tag)
  ;; This is a Scheme uninterned symbol.
  ;; Common Lisp seems to do the right thing with it.
  (string->uninterned-symbol
   (string-append
    (cond ((symbol? base)
	   (symbol->string base))
	  ((null? base)
	   "NIL")
	  ((eq? base true)
	   "T")
	  (else
	   (error "->symbol: not a valid base" base)))
    tag)))

;;;; Top level utilities.

;;; The imperative style optimizer should only be invoked once per
;;; "top-level" expression.  It should be idempotent on an expression
;;; after the first time, but it would be expensive to invoke
;;; repeatedly.  The following three variables allow it to be turned
;;; off, and invoked only when needed if turned on.
;;; There is a slight problem with SCODE-QUOTE, since the right
;;; behavior is not clear at all, and with independent invocations to
;;; the procedures below while syntaxing is in progress.  In other
;;; words, top-level-imperative-optimize assumes that all recursive
;;; invocations of syntax-BLOCK-form, and syntax-TAGBODY-form will
;;; result in code contained in the result of kernel, and this may not
;;; be true.  It is safe in that these other pieces of code will be
;;; correct, but not optimized.

(define *optimize-imperative-constructs?* false)
(define *top-level-will-optimize?* false)
(define *need-top-level-optimization?*)

(define (top-level-imperative-optimize kernel)
  (if *optimize-imperative-constructs?*
      (fluid-let ((*optimize-imperative-constructs?* false)
		  (*top-level-will-optimize?* true)
		  (*need-top-level-optimization?* false))
	(let ((result (kernel)))
	  (if (not *need-top-level-optimization?*)
	      result
	      (optimize-imperative-constructs result))))
      (kernel)))

;;;; BLOCK and RETURN-FROM -- CommonLisp support.
;;;  RETURN is defined as a macro which uses RETURN-FROM.

(define *return-from-alist* '())

(define syntax-BLOCK-form 
  (spread-arguments
   (lambda (name . body)
     (define (kernel)
       (fluid-let ((*return-from-alist*
		    (cons (cons name false)
			  *return-from-alist*)))
	 (let ((body-scode (syntax-sequence body)))
	   (let ((blockname (cdr (assq name *return-from-alist*))))
	     (if (false? blockname)
		 body-scode
		 (begin
		   (if *top-level-will-optimize?*
		       (set! *need-top-level-optimization?* true))
		   (make-block blockname body-scode)))))))

     (if (not (or (symbol? name)
		  (eq? name ())
		  (eq? name true)))
	 (error "Block name not a symbol" name body))
     (top-level-imperative-optimize kernel))))

(define syntax-RETURN-FROM-form
  (spread-arguments
   (lambda (name . forms)
     (if (> (length forms) 1)
	 (error "Too many arguments to return-from" name forms))
     (let ((entry (assq name *return-from-alist*)))
       (if (false? entry)
	   (error "return-from not lexically enclosed in a block of that name"
		  name))
       (if (false? (cdr entry))
	   (set-cdr! entry (->symbol name "-BLK")))
       (if (null? forms)
	   (make-return (cdr entry) '())
	   (make-return (cdr entry) (syntax-expression (car forms))))))))

(define (make-return tag form)
  (if (or (scode-constant? form) (variable? form))
      (make-combination (make-variable tag)
			(list form))
      (make-combination prim-with-values
			(list (make-simple-lambda '() form)
			      (make-variable tag)))))

;;;; TAGBODY -- CommonLisp support

;;; TAGBODY is implicitely used in PROG, DO, and many other forms.

(define *tagbody-alist* '())

(define syntax-TAGBODY-form
  (spread-arguments
   (lambda tags&forms
     (parse-tagbody
      tags&forms
      (lambda (preface tag-bindings)
	(define (kernel)
	  (let ((go-tag (generate-uninterned-symbol 'GO)))
	    (fluid-let ((*tagbody-alist*
			 (make-tagbody-alist go-tag
					     tag-bindings
					     *tagbody-alist*)))
	      (process-tagbody-end
	       go-tag
	       (syntax-sequentially preface)
	       (map (lambda (tag-binding)
		      (syntax-sequentially (cdr tag-binding)))
		    tag-bindings)))))
	(if (null? tag-bindings)
	    (make-optimized-sequence
	     (syntax-sequentially preface))
	    (top-level-imperative-optimize kernel)))))))

(define (make-tagbody-alist go-tag bindings alist)
  (map* alist
	(lambda (binding)
	  (list (car binding)		; tag name
		go-tag			; escape tag name
		false))			; not used yet
	bindings))

(define syntax-GO-form
  (spread-arguments
   (lambda (tag)
     (let ((place (assq tag *tagbody-alist*)))
       (if (false? place)
	   (error "syntax-GO-form: Unknown tag" tag))
       (if (false? (caddr place))	; guarantee used
	   (set-car! (cddr place) (->symbol tag "-TAG")))
       (make-combination
	(make-variable (cadr place))
	(list (make-variable (caddr place))))))))

;;;; Utilities for TAGBODY

(define (parse-tagbody forms receiver)
  (let loop ((forms (reverse forms))
	     (current '('()))
	     (parsed '()))
    (cond ((null? forms)
	   (receiver current parsed))
	  ((symbolp (car forms))
	   (loop (cdr forms)
		 '()
		 (cons (cons (car forms) current)
		       parsed)))
	  (else
	   (loop (cdr forms)
		 (cons (car forms) current)
		 parsed)))))

(define (process-tagbody-end go-tag preface segments)
  ;; Notes:
  ;; 1: all subexpressions have been syntaxed by now thanks to
  ;; applicative order.  This is important since it guarantees that if
  ;; a tag has not been met, it is not used at all!
  ;; 2: *tagbody-alist* and segments march in step: the first segment
  ;; corresponds to the first tag in *tagbody-alist*, and so on, but
  ;; *tagbody-alist* may be longer than segments.
  (let loop ((segments segments)
	     (tags *tagbody-alist*)
	     (current (cons '() preface))
	     (accumulated '()))
    (cond ((null? segments)
	   (if (null? accumulated)
	       (make-optimized-sequence (cdr current))
	       (tagbody-wrap
		go-tag
		(cons (cons (car current)
			    (make-optimized-sequence (cdr current)))
		      accumulated))))
	  ((false? (caddr (car tags)))	; Not used?
	   (loop (cdr segments)
		 (cdr tags)
		 (cons (car current)
		       (append! (cdr current) (car segments)))
		 accumulated))
	  (else
	   (loop (cdr segments)
		 (cdr tags)
		 (cons (caddr (car tags))
		       (car segments))
		 (cons (cons (car current)
			     (make-optimized-sequence
			      (append! (cdr current)
				       (list (make-combination
					      (make-variable (caddr (car tags)))
					      '())))))
		       accumulated))))))

(define (tagbody-wrap go-tag forms)
  (if *top-level-will-optimize?*
      (set! *need-top-level-optimization?* true))
  (let loop ((forms forms) (values '()) (names '()))
    (if (null? (cdr forms))
	(letrec->tagbody
	 go-tag
	 (make-tagbody-letrec names
			      values
			      (cdr (car forms))))
	(loop (cdr forms)
	      (cons (make-simple-lambda '() (cdr (car forms)))
		    values)
	      (cons (car (car forms))
		    names)))))

;; An iterate is 
;; (let <loop-name> ((<var1> <val1>)
;;                   (<var2> <val2>) ...)
;;   <body>)
;;
;; Which is
;; (let ((<loop-name>))
;;   (set! <loop-name> (lambda (<var1> <var2> ...) <body>))
;;   (<loop-name> <val1> <val2> ...))
;;
;;  Which is
;;
;; ((lambda (&aux <loop-name>)
;;    (set! <loop-name> (lambda (<var1> <var2> ...) <body>))
;;    (<loop-name> <val1> <val2> ...))
;; )
;;
;; Important!!
;;  These iterations always obey left-to-right evaluation ordering,
;;  hence the use of make-lr-combination below in evaluating the initial values.
;;  There is corresponding use of the syntax LR-COMBINATION in calling the loopnames
;;  (see, e.g., clchap7.scm).

;; make-iterate emits an iterate comment and a normal combination
;;  for the initial operands.

(define (make-iterate iterate-name loop-name body-lambda initial-operands)
  (make-comment
   '%iterate%
   (make-combination
    (external-make-lambda iterate-name '() '() '() (list loop-name) '()
			  (make-sequence
			   (list (make-assignment loop-name body-lambda)
				 (make-combination (make-variable loop-name) initial-operands))))
    '())))

;; make-final-iterate is called by the optimizer. It uses make-lr-combination
;;  to force left-to-right order of initial operands. Since it is the final code,
;;  it need not emit a an iterate comment.

(define (make-final-iterate iterate-name loop-name body-lambda initial-operands)
  (make-combination
   (external-make-lambda iterate-name '() '() '() (list loop-name) '()
			 (make-sequence
			  (list (make-assignment loop-name body-lambda)
				(make-lr-combination (make-variable loop-name) initial-operands))))
   '()))

(define (comment/iterate? comment)
  (comment-components comment
    (lambda (text expr)
      (eq? text '%ITERATE%))))

(define (iterate-components obj recvr)
  (comment-components obj
    (lambda (text expr)
      (combination-components expr
	(lambda (op args)
	  (lambda-components op
	    (lambda (iterate-name req opt rest aux decls body)
	      (sequence-components
		  body
		  (lambda (elements)
		    (let ((assignment (car elements))
			  (init (cadr elements)))
		      (assignment-components assignment
			(lambda (assignment-name body-lambda)
			  (combination-components init
			    (lambda (operator initial-operands)
			      (recvr iterate-name (car aux) body-lambda initial-operands)))))))))))))))
			      
(define syntax-CL-ITERATE-form
  (spread-arguments
   (lambda (loop-name binding-list . body)
     (let ((names (mapcar car binding-list))
	   (initial-values (mapcar cadr binding-list)))
       (make-iterate lambda-tag:let
		     (fundefsym loop-name)
		     (syntax-expression (proc-cl-named-lambda loop-name names body))
		     (syntax-sequentially initial-values))))))

(define syntax-EVAL-WHEN-form
  (spread-arguments
   (lambda (situation . body)
     (case *eval-when-mode*
       (evaling
	(if (memq 'eval situation)
	    (syntax-expression 
	     `(begin ,@body))))
       (compiling
	(eval-when-process-body
	 `((eval-when ,situation ,@body)) #f))
       (else
	(error "Internal error: unknown eval-when mode ~a" *eval-when-mode*))))))
;;;
;;; Process and eval-when assuming compiling
;;;

(define (eval-when-process-body body compile-time-too?)
  (define (eval-when-process-element element)
    (if (and (pair? element)
	     (eq? (car element) 'eval-when))
	;;element is an eval-when
	(let ((situation (cadr element))
	      (body (cddr element)))
	  ;;if the situation 'load' is specified, or if the current processing mode
	  ;;is compile-time-too and the situation eval is also specified, then process
	  ;;each of the forms in the body in compile-time-too mode. Otherwise, process
	  ;;each of the forms in the body in not-compile-time-too mode.
	  (if (memq 'load situation)
	      (eval-when-process-body body
				      (or
				       (memq 'compile situation)
				       (and (memq 'eval situation)
					    compile-time-too?)))
	      ;;if the situation load is not specified, if the situation compile is specified, or if
	      ;;the current processing mode is compile-time-too and the situation eval is specified,
	      ;;then evaluate each of the forms in the body in the compiler's executing environment.
	      ;;Otherwise, ignore the eval-when form entirely.
	      (if (or
		   (memq 'compile situation)
		   (and (memq 'eval situation)
			compile-time-too?))
		  (begin
		    (scode-eval (syntax `(begin ,@body))
				*commonlisp-user-environment*)
		    '()))))
	;;element is not an eval-when. do two things: if the current processing mode is 
	;;compile-time-too, evaluate the form in the compiler's executing environment. Second, 
	;;perform normal compiler processing of the form.
	(let ((scode (syntax-expression element)))
	  (if compile-time-too?	(scode-eval scode *commonlisp-user-environment*))
	  scode)))
  (define (eval-when-process-body-aux body)
    (if (null? body)
	'()
	;; force order
	(let ((e (eval-when-process-element (car body))))
	  (cons e
		(eval-when-process-body-aux (cdr body))))))
  (make-optimized-sequence (eval-when-process-body-aux body)))

'$split-file

;;;
;;; Compiler-Let
;;;

(define syntax-COMPILER-LET-form)

(let ()

  (define (make-special-decls inits)
    (cond
     ((null? inits)
      '())
     ((pair? (car inits))
      (cons (caar inits)
	    (make-special-decls (cdr inits))))
     (else
      (cons (car inits)
	    (make-special-decls (cdr inits))))))

  (set! syntax-COMPILER-LET-form
	(spread-arguments
	 (lambda (inits . body)
	   (case *eval-when-mode*
	     (evaling
	      (syntax-expression 
	       `(let ,inits 
		  (declare (special ,@(make-special-decls inits)))
		  ,@body)))
	     (compiling
	      (eval
	       `(let ,inits
		  (declare (special ,@(make-special-decls inits)))
		  ((access syntax-expression syntaxer-package) '(progn ,@body)))
	       system-global-environment))
	     (else
	      (error "Internal error: unknown eval-when mode ~a" *eval-when-mode*)))))))

;;; Commonlisp combinations

(define make-cl-combination)
(define make-typed-combination)
(define make-possibly-lr-combination)
(define make-lr-combination)

(define cl-combination-package
  (make-environment

;; The list primitives that can return futures as their value.

(define may-return-futures-list
  (list (make-primitive-procedure 'lexical-assignment)
	(make-primitive-procedure 'local-reference)
	(make-primitive-procedure 'local-assignment)
	(make-primitive-procedure 'scode-eval)
	(make-primitive-procedure 'lexical-reference)
	(make-primitive-procedure 'car)
	(make-primitive-procedure 'cdr)
	(make-primitive-procedure 'set-car!)
	(make-primitive-procedure 'set-cdr!)
	(make-primitive-procedure 'general-car-cdr)
	(make-primitive-procedure 'hunk3-cxr)
	(make-primitive-procedure 'hunk3-set-cxr!)
	(make-primitive-procedure 'vector-ref)
	(make-primitive-procedure 'vector-set!)
	(make-primitive-procedure 'assq)
	(make-primitive-procedure 'cell-contents)
	(make-primitive-procedure 'system-pair-car)
	(make-primitive-procedure 'system-pair-cdr)
	(make-primitive-procedure 'system-pair-set-car!)
	(make-primitive-procedure 'system-pair-set-cdr!)
	(make-primitive-procedure 'set-cell-contents!)
	(make-primitive-procedure 'system-hunk3-cxr0)
	(make-primitive-procedure 'system-hunk3-set-cxr0!)
	(make-primitive-procedure 'system-hunk3-cxr1)
	(make-primitive-procedure 'system-hunk3-set-cxr1!)
	(make-primitive-procedure 'system-hunk3-cxr2)
	(make-primitive-procedure 'system-hunk3-set-cxr2!)
	(make-primitive-procedure 'system-vector-ref)
	(make-primitive-procedure 'system-vector-set!)
	(make-primitive-procedure 'with-history-disabled)
	(make-primitive-procedure 'with-threaded-continuation)
	(make-primitive-procedure 'within-control-point)
	(make-primitive-procedure 'with-interrupts-reduced)))

(set! make-typed-combination
      (named-lambda (make-typed-combination type op args)
	(let ((res (make-possibly-lr-combination 
		    (cond ((symbol? op)
			   (make-variable (*get-fcn-name* op)))
			  ((and (pair? op)
				(eq? (car op) 'no-fundefsym))
			   (make-variable (cadr op)))
			  (else op))
		    args)))
	  (if (and (primitive-procedure? op)
		   (not (memq op may-return-futures-list)))
	      ;; Subtract future type
	      (if (eq? type unknown-keyword)
		  (set-scode-type! res (list 'non-future #t))
		  (set-scode-type! res (subtract-futures type)))
	      (if (not (eq? type unknown-keyword))
		  (set-scode-type! res type)))
	  res)))

;;; Constants need not be wrapped.
;;; A combo of < 2 args need not be wrapped.
;;; The last expr in the arglist need not be wrapped.
;;; If the remaining exprs are all vars or consts, we don't need to wrap them.
;;;  This also takes care of the all-vars or all-consts case.
;;; Otherwise, we need to wrap, unless we do an analysis of subexprs
;;;  to show there is no lexical setting of a var in the arg list.
;;;  We don't do this currently; it should be done by setting up an
;;;  alist-like database of vars during syntaxing a combo, and having the syntaxer of set!
;;;  mark this list if any lexical vars to be set occur in it. This avoids multiple
;;;  walks of the scode.

(set! make-possibly-lr-combination 
      (named-lambda (make-possibly-lr-combination operator operands)
	(if (lr-evaluation-mode?)
	    (make-lr-combination operator operands)
	    (make-combination operator operands))))

(set! make-lr-combination 
      (named-lambda (make-lr-combination operator operands)

	(define (all-vars-or-consts? exprs)
	  (or (null? exprs)
	      (and (let ((expr (car exprs)))
		     (or (variable? expr)
			 (scode-constant? expr)
			 (procedure? expr)
			 (lambda? expr)))
		   (all-vars-or-consts? (cdr exprs)))))

	(if (< (length operands) 2)
	    (make-combination operator operands)
	    (let make-lr-wrapper ((vars '())
				  (exprs operands))
	      (cond 
	       ((null? exprs)
		(make-combination operator (reverse! vars)))
	       ((all-vars-or-consts? exprs)
		(make-combination operator (append (reverse! vars) exprs)))
	       (else
		(let ((expr (car exprs)))
		  (if (or (scode-constant? expr)
			  (null? (cdr exprs)))
		      (make-lr-wrapper (cons expr vars) (cdr exprs))
		      (let* ((tempname (generate-uninterned-symbol 'g))
			     (tempvar (make-variable tempname)))
			(make-closed-block lambda-tag:let (list tempname) (list expr)
					   (make-lr-wrapper (cons tempvar vars) (cdr exprs))))))))))))

(define (process-args args types)
  (if (null? args)
      '()
      (let ((arg (syntax-expression (car args)
				    (if (null? types)
					unknown-keyword
					(car types)))))
	(if (not (null? types))
	    (let ((tp (car types))
		  (actual-type (syntax-type-of arg)))
	      (if (and (not (eq? actual-type unknown-keyword))
		       (not (subtypep actual-type tp))
		       (not (subtypep tp actual-type)))
		  (warn "~A is required to be of type ~A ~
                         but has been function-declared~%to be type ~A. ~
                         Types are possibly disjoint"
			(car args) actual-type tp))
	      (if (eq? actual-type unknown-keyword)
		  (set-scode-type! arg tp))))
	(cons arg
	      (process-args (cdr args) (if (null? types) '() (cdr types)))))))

;; Given an operator, integrate to a primitive if possible.
;; Return a possibly new operator, a flag indicating whether the new
;; operator is a primitive, and a flag indicating whether optimizers can
;; be used.  If the operator is declard not-inlineable, then the operator will
;; not be integrated and will not be optimized.

(define (possibly-integrate op)
  (if (symbol? op)
      (let ((p (symbol-package op))
	    (f (*get-fcn-name* op)))
	(cond ((declared-not-inlineable? f)
	       (prim-values op #f #f))
	      ((or (shadowed? op)
		   (not (or (eq? p *lisp-package*)
			    (eq? p *bbnaci-package*)
			    (eq? p *system-package*)))
		   (lexical-unreferenceable? *commonlisp-user-environment* f))
	       (prim-values op #f #f))
	      (else
	       (let ((res (lexical-reference *commonlisp-user-environment* f)))
		 (if (primitive-procedure? res)
		     (prim-values res #t #t)
		     (prim-values op #f  #t))))))
      (prim-values op #f #t)))

;; Given an operator, return a possibly different operator
;; and an optimizing function for that operator.

(define (lookup-optimizer operator)
  (prim-with-values
   (lambda () (possibly-integrate operator))
   (lambda (processed-op primitive? ok-to-optimize?)
     (let ((optimizer1 
	    (if ok-to-optimize?
		(function-optimizer operator)
		unknown-keyword))
	   (optimizer2
	    (if ok-to-optimize? 
		(function-optimizer processed-op)
		unknown-keyword)))
       (if primitive?
	   (prim-values processed-op
			(if (eq? optimizer2 unknown-keyword)
			    optimizer1
			    optimizer2))
	   (prim-values processed-op
			optimizer1))))))

;; Optimize combinations according to declarations by
;; calling optimization function, if present.

(set! make-cl-combination
      (named-lambda (make-cl-combination expression required-type)

	(define (make-cl-combination1 operator args required-type)
	  (let ((f-type (declared-function-type operator))
		(expression-type required-type)
		(arg-scode))
		   
	    ;; Check function type declarations

	    (if (not (eq? f-type unknown-keyword))
		(let ((arg-types (function-arg-types f-type)))
		  (if (not (= (length arg-types) (length args)))
		      (warn "~A is declared to expect ~A args but is being passed ~A:~%~A"
			    operator (length arg-types) (length args) expression))
		  (set! arg-scode (process-args args arg-types))
		  (let ((tp (function-result-type f-type)))
		    (if (and (not (eq? required-type unknown-keyword))
			     (not (subtypep required-type tp))
			     (not (subtypep tp required-type)))
			(warn "The result of ~A is required to be of type ~A ~
                               but has been function-declared~%to be type ~A. ~
                               Types are possibly disjoint: ~A"
			      operator required-type tp expression))
		    (if (eq? required-type unknown-keyword)
			(set! expression-type tp))))
		(set! arg-scode (syntax-expressions args)))
		   
	    ;; Check for optimizations

	    (prim-with-values
	     (lambda () (lookup-optimizer operator))
	     (lambda (operator optimizer)
	       (if (not (eq? optimizer unknown-keyword))
		   (prim-with-values
		    (lambda ()
		      (optimizer expression-type operator arg-scode))
		    (lambda (type new-op new-args do-again?)
		      (if *optimization-debug*
			  (begin
			    (newline)
			    (princ expression)
			    (princ " optimized to ")
			    (newline)
			    (princ (cons new-op new-args))
			    (newline)
			    (if do-again?
				(princ "and it will be syntaxed again")
				(princ "and it will not be syntaxed"))
			    (newline)))
		      (if do-again?
			  (fluid-let ((*syntax-time-env* *syntax-time-global-env*))
			    ;; Make the optimizer expand in a syntax-environment
			    ;; that doesn't have user level non-global declarations
			    (syntax-expression (cons new-op new-args) type))
			  (make-typed-combination type new-op new-args))))
		   (make-typed-combination expression-type operator arg-scode))))))

	(let ((operator (car expression))
	      (args (cdr expression)))
	  (if (inline-structure-operation? operator)
	      (structure-operation-info (lookup-attribute (fundefsym operator) 
							  structure-operation-keyword)      
		(lambda (op-type slot-type index dd dsd)
		  (cond
		   ((null? op-type)
		    (make-cl-combination1 operator args required-type))
		   ((eq? op-type 'accessor)
		    (make-cl-combination1 '(no-fundefsym structure-ref) 
					  (append args (list dd dsd)) slot-type))
		   ((eq? op-type 'setter)
		    (make-cl-combination1 '(no-fundefsym structure-set!)
					  (append args (list dd dsd)) slot-type)))))
	      (make-cl-combination1 operator args required-type)))))



)) ; end cl-combination-package

;;; Optimized sequences

;; We want to optimize sequences to
;; 1: Remove dead sequence items, such as constants and variables in
;;    the middle of a sequence.
;; 2: Propagate the type information so that the scode type of a sequence
;;    is the scode type of the last item in the sequence.

(define (make-optimized-sequence forms)
  (let ((purged-forms (remove-dead-forms forms)))
    (if (= (length purged-forms) 1)
	(make-sequence purged-forms)
	(let ((last-form (car (last-pair purged-forms))))
	  (let ((result (make-sequence purged-forms)))
	    (if *process-declarations*
		(let ((type (syntax-type-of last-form)))
		  (if (not (eq? type unknown-keyword))
		      (set-scode-type! result type))))
	    result)))))

(define (remove-dead-forms l)
  (cond ((null? l)
	 '())
	((sequence? (car l))
	 (if (open-block? (car l))
	     (cons 
	      (open-block-components (car l)
	       (lambda (names declarations body)
		 (make-open-block
		  names
		  declarations
		  (make-sequence
		   (remove-dead-forms 
		    (sequence-components body identity-procedure))))))
	      (remove-dead-forms (cdr l)))
	     (remove-dead-forms
	      (append (sequence-components (car l) identity-procedure) (cdr l)))))
	((= (length l) 1)
	 l)
	((dead-form? (car l))
	 (remove-dead-forms (cdr l)))
	(else (cons (car l)
		    (remove-dead-forms (cdr l))))))

(define *constant-scode-objects-table*
  (append *constant-objects-table*
	  '((quotation . #t)
	    (variable . #t)
	    (cl-i-vector . #t))))

(define (dead-form? e)
  (and (not ((access file-split-marker? package/top-level package/scode-optimizer)
	     e))
       (not (declaration? e))
       (not (block-declaration? e))
       (assq (microcode-type-name (primitive-type e))
	     *constant-scode-objects-table*)))

;;;; Procedures

;;; Cut and pasting of declarations in lambdas and named-lambdas

(define (ok-to-add-function-declaration? decl-specs function-name)
  (let loop ((specs decl-specs) (any-decls-yet? #f))
    (cond ((null? specs)
	   any-decls-yet?)
	  ((this-function-declaration? function-name (car specs))
	   #f)
	  (else
	 (loop (cdr specs) #t)))))

(define (this-function-declaration? name decl)
  (and (pair? decl)
       (eq? (first decl) 'ftype)
       (eq? (third decl) name)))

(define (add-normal-declarations ordered-decl-specs bvl body)
  `((quote (,declarations-keyword
	    ,bvl
	    (declare ,@ordered-decl-specs)))
     ,body))

(define (add-function-declaration ordered-decl-specs bvl function-name body)
  `((quote (,declarations-keyword
	     ,bvl
	     (declare ,@(merge-declarations
			 (generate-function-declaration 
			  ordered-decl-specs
			  (length bvl)
			  function-name)
			 ordered-decl-specs))))
     ,body))

(define (generate-function-declaration decl-specs n function-name)
  (let ((arg-decls
	 (let loop ((specs decl-specs)
		    (count 0))
	   (if (or (>= count n)
		   (null? specs))
	       '()
	       (cons (if (null? (car specs)) #t (get-type (car specs)))
		     (loop (cdr specs) (1+ count)))))))
    `(ftype (function ,arg-decls #t)
	    ,function-name)))
