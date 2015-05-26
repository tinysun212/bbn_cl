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
;; The format for generate-type-optimizer is as follows:
;; (generate-type-optimizer <input-pattern> <touch-pattern> <list of matching cases>
;;                          <input-pattern> <touch-pattern> <list of matching cases> ...)
;; 
;; Note that there can be more than one set of <input-pattern> <touch-pattern> <list of matching cases>
;; wich allows for different types of optimizers for differnt argument patterns for the
;; same function.
;;
;; The <input-pattern> is what you want optimized such as (foo a b)
;; 
;; The <touch-pattern> is a list indicating what arguments need to be
;; touched for optimization to take place.  For instance, if a should be 
;; touched but not b, then the pattern will be (#t #f)
;; 
;; The <list of matching cases> is a list of case clause that if match will produce
;; and optimization.
;; 
;; A case clause is as follows:
;; (<list of argument types> <required-type> <returned-type> <expansion expression> <do again flag>)
;; 
;; An argument type is of the form (<type> <argument>).  If <type> is *, than any type will match.
;; For instance, for a particular clause, the list of argument types might be ((fixnum a) (* b)),
;; which means argument a must be a fixnum and argument b can be any type.
;; Type might be of the form (:satisfies <procname>), in which case a match will be determined
;; By wheter the procedure named by <procname> returns T or NIL.  <procname> will be passed
;; one argument, which is the <syntax-arg-type> of the specified argument
;; 
;; The <required-type> is the required type of the expression which must be satisfied for
;; the optimization to take place.  * means any required type will match.
;; 
;; The <returned-type> the the resultant type of expansion if an optimization takes place.  It
;; can be a subtype of the <required-type>.  If the <returned-type> is :required-type, than the
;; resultant type of the expansion will be the required type of the initial expression.
;; 
;; The <expansion expression> is what the optimization will produce.  It normally will contain
;; some of the arguments to the expression which will get substituted in.  An occurrance of
;; (:primitive <name>) will cause the primitive procedure <name> to be inserted in the expansion.
;; An occurrance of (:gensym <name>) will generate an uninterned symbol.  Multiple occurances of
;; the same (:gensym <name>) will get the same uninterned symbol.  Occurrances of 
;; (:arg-type <name>) will be replaced by the <syntax-arg-type> of the argument <name>.
;; 
;; The <do again flag> indicates wheter or not to resyntax in order to pick up further optimizations.
;; The only time it should ever be null is in cases where optimzers expand into themselves.  In these
;; cases, infinite loops can be avoided by making this flag null.
;;
;; A <syntax-arg-type> is either a valid type, :unknown, or (:constant <value>), meaning that
;; the <value> of the argument is a syntax constant.

(define-macro (generate-type-optimizer spec touch-pattern cases . more-triples)
  (let ((name (first spec)))
    (let* ((name (car spec))
	   (f-name (fundefsym name)))
      `(add-optimizer! 
	  (if (lexical-unassigned? '() ',f-name)
	      ',name
	      (let ((res
		     (lexical-reference *commonlisp-user-environment* ',f-name)))
		(if (primitive-procedure? res)
		    res
		    ',name)))
	(lambda (required-type operator passed-args)
	  ,(make-case-matcher spec touch-pattern cases more-triples))))))

(cl-define (make-case-matcher spec touch-pattern cases more-triples)
  (if (not more-triples)
      `(if (= (length passed-args) ,(length (cdr spec)))
	   (match-cases required-type
			operator
			passed-args
			',spec
			',cases
			(touch-if-needed passed-args ',touch-pattern))
	   (prim-values required-type operator passed-args #f))
      `(if (= (length passed-args) ,(length (cdr spec)))
	   (match-cases required-type
			operator
			passed-args
			',spec
			',cases
			(touch-if-needed passed-args ',touch-pattern))
	   ,(make-case-matcher (first more-triples)
			       (second more-triples)
			       (third more-triples)
			       (cdddr more-triples)))))

;; The format of a generate-general-optimizer is as follows:
;; (generate-general-optimzer <input-pattern> <touch-pattern> <optimization function>)
;; 
;; <input-pattern> and <touch-pattern> are the same as above.
;; 
;; The <optimization function> is a function of four arguments.  The first
;; will be the required-type of the expression, the second will be the operator,
;; the third will be the list of arguments, and the fourth will be the list of
;; touched arguments (only those specified by the <touch-pattern> will be touched).
;; This function should return four values, via prim-values.  The first value
;; should be the resultant type of the expansion, the second value is the expansion operator.
;; The third value is the expansion arguments.  Note that if any of these arguments are
;; the original arguments or touched-arguments, they sould be syntax-quoted.  The fourth
;; value returnd is the <do again flag>.

(define-macro (generate-general-optimizer spec touch-pattern optimizer)
  (let ((name (first spec))
	(args (cdr spec)))
    (let* ((number-of-args (length args))
	   (name (car spec))
	   (f-name (fundefsym name)))
      `(add-optimizer! 
	  (if (lexical-unassigned? '() ',f-name)
	      ',name
	      (let ((res
		     (lexical-reference *commonlisp-user-environment* ',f-name)))
		(if (primitive-procedure? res)
		    res
		    ',name)))
	  (lambda (required-type operator passed-args)
	    (if (= (length passed-args) ,number-of-args)
		(,optimizer required-type
			   operator
			   passed-args 
			   (touch-if-needed passed-args ',touch-pattern))
		(prim-values required-type operator passed-args #f)))))))

;; Try to match a combination with one of the clauses from an optimizer

(cl-define (match-cases required-type operator passed-args spec case-list touched-args)
  (let ((a-list (make-alist (cdr spec) touched-args)))
    (cl-define (match-loop cases)
      (if (null? cases)
	  (prim-values required-type operator passed-args #f)
	  (let ((case (car cases)))
	    (let ((variable-type-list (first case))
		  (needed-type (first (cdr case)))
		  (resultant-type (post-pass-type (first (cdr (cdr case))) required-type))
		  (template (first (cdr (cdr (cdr case)))))
		  (do-it-again? (first (cdr (cdr (cdr (cdr case)))))))
	      (let ((binding (match-types required-type needed-type variable-type-list a-list)))
		(if (not (eq? binding 'failed))
		    (let* ((result (instantiate-pattern binding template do-it-again?))
			   (optimized-operator (car result)))
		      (prim-values
		       (instantiation-type required-type resultant-type optimized-operator)
		       optimized-operator
		       (cdr result)
		       do-it-again?))
		    (match-loop (cdr cases))))))))
    (match-loop case-list)))

;; Try to match a clause from an optimizer

(cl-define (match-types required-type needed-type variable-type-list binding-alist)
  (if (or (eq? needed-type '*)
	  (and (not (eq? required-type :unknown))
	       ;; Need to subtract futures for things like
	       ;; fixnum optimizers
	       (subtypep (subtract-futures required-type) needed-type)))
      (let ()
	(cl-define (pattern-match-loop template frame)
	  (cond ((eq? frame 'failed) 'failed)
		((null? template) (reverse frame))
		(else
		 (let ((proposed-binding (first template)))
		   (let ((proposed-name (first (cdr proposed-binding)))
			 (proposed-type (first proposed-binding)))
		     (let ((actual-arg (cdr (assq proposed-name binding-alist))))
		       (if (types-compatible? proposed-type actual-arg)
			   (pattern-match-loop (cdr template)
					       (cons (list proposed-name actual-arg) frame))
			   (pattern-match-loop template 'failed))))))))
	(pattern-match-loop variable-type-list '()))
      'failed))

;;; Instantiating patterns

(define *gensym-cache*)

(cl-define (instantiate-pattern pattern template do-it-again?)
  (fluid-let ((*gensym-cache* '()))
    (instantiate-pattern-loop pattern template do-it-again?)))

(cl-define (instantiate-pattern-loop pattern template do-it-again?)
  (cond ((not (pair? template))
	 (let ((found? (assq template pattern)))
	   (if found?
	       (if do-it-again?
		   `(syntax-quote ,(first (cdr found?)))
		   (first (cdr found?)))
	       template)))
	((eq? (car template) :primitive)
	 (make-primitive-procedure (first (cdr template))))
	((eq? (car template) :gensym)
	 (let ((a (assq (first (cdr template)) *gensym-cache*)))
	   (if a
	       (cdr a)
	       (let ((new (generate-uninterned-symbol (first (cdr template)))))
		 (set! *gensym-cache* (cons (cons (first (cdr template)) new) *gensym-cache*))
		 new))))
	((eq? (car template) :arg-type)
	 (arg-type (first (cdr (assq (first (cdr template)) pattern)))))
	(else
	 (cons (instantiate-pattern-loop pattern (car template) do-it-again?)
	       (instantiate-pattern-loop pattern (cdr template) do-it-again?)))))

(cl-define (instantiation-type required-type resultant-type operator)
  (cond ((eq? required-type :unknown)
	 resultant-type)
	((subtypep required-type resultant-type)
	 required-type)
	((subtypep resultant-type required-type)
	 resultant-type)
	(else
	 (warn "The user-specified type ~A is possibly disjoint with ~A,~%~
                the type of operation ~A."
	       required-type resultant-type operator)
	 ;; should be intersection, but we don't want 
	 ;; types to explode, so believe the user
	 required-type)))

;;; Utility routines

(cl-define (make-alist a b)
  (if (null? a)
      '()
      (cons (cons (car a) (car b))
	    (make-alist (cdr a) (cdr b)))))

(cl-define (deep-replace l source target)
  (cond ((pair? l)
	 (cons (deep-replace (car l) source target)
	       (deep-replace (cdr l) source target)))
	((eq? l source)
	 target)
	(else l)))

(cl-define (post-pass-type type required-type)
  (deep-replace (deep-replace type :required-type required-type)
		:unknown
		t))

(cl-define (arg-type arg)
  (if (syntax-constant? arg)
      (list :constant arg)
      (syntax-type-of arg)))

(cl-define (types-compatible? type arg)
  (or (eq? type '*)
      (cond ((and (pair? type)
		  (eq? (car type) :satisfies))
	     (funcall (first (cdr type))
		      (if (syntax-constant? arg)
			  (list :constant arg)
			  (syntax-type-of arg))))
	    ((syntax-constant? arg)
	     (typep arg type))
	    (else
	     (let ((tp (syntax-type-of arg)))
	       (and (not (eq? tp :unknown))
		    (subtypep tp type)))))))

;; Defined later

(cl-define (type-expand x) x)

;; Subtract-futures removes the futures type from a given
;; time.  This callse normalize-type-expr, which is redefined later.
;; This is a real time sink, so the results are cached once sxhash comes in.

(define *subtract-futures-cache-size* 1024)

(define *subtract-futures-cache* (make-vector *subtract-futures-cache-size* '()))

(cl-define (clear-subtract-futures-cache)
  (set! *subtract-futures-cache*
	(make-vector *subtract-futures-cache-size* '())))

(define *use-subtract-futures-cache* nil)

(cl-define scheme-assoc assoc)

(cl-define (real-subtract-futures type)
  (normalize-type-expr
   (type-expand `(and (not future) 
		      ,(type-expand type)))))

(cl-define (subtract-futures type)
  (if *use-subtract-futures-cache*
      (let ((index (mod (sxhash type) *subtract-futures-cache-size*)))
	(let ((bucket (system-vector-ref *subtract-futures-cache* index)))
	  (let ((cached-entry
		 (if bucket
		     (scheme-assoc type bucket)
		     bucket)))
	    (if cached-entry
		(cdr cached-entry)
		(let ((result (real-subtract-futures type)))
		  (system-vector-set! *subtract-futures-cache* index
				      (cons (cons type result) bucket))
		  result)))))
      (real-subtract-futures type)))

;; Used by the syntaxer

(set! subtract-futures (symbol-function 'subtract-futures))

;; Touch-if-needed wraps touch around syntaxed arguments
;; as dictated by a pattern.  Since creating the touch combination
;; was a real time sink in the syntaxer, it has been optimized.
;; First, instead of just touch, a let with a conditional is generated.
;; This allows the compiler to generate a very fast inline test and branch
;; for the non-future case.  The conditional is presyntaxed since it is the
;; same for every touch combination.  Hence, making a syntax-touch
;; turns into a simple generation of a let.

(define the-test
  (syntax '(if (non-touching-primitive-type? 46 temp)
	       (touch temp)
	       temp)
	  *rep-current-syntax-table*))

;; Stuff needed from the syntaxer
(cl-define make-closed-block (access make-closed-block syntaxer-package))
(define let-tag (access lambda-tag:let syntaxer-package))
(cl-define set-scode-type! (access set-scode-type! syntaxer-package))

(cl-define make-syntax-touch
  (lambda (arg type)
    (let ((result
	   (make-closed-block let-tag '(temp) (list arg) the-test)))
      (set-scode-type! result (subtract-futures type))
      result)))

(cl-define (touch-if-needed args pattern)
  (cond ((or (null? pattern) (not (touch-mode)))
	 args)
	((null? args)
	 '())
	((car pattern)
	 (let* ((arg (car args))
		(type (syntax-type-of arg)))
	   (cons (cond ((eq? type :unknown)
			(make-syntax-touch arg #t))
		       ((subtypep 'future type)
			(make-syntax-touch arg type))
		       (else arg))
		 (touch-if-needed (cdr args) (cdr pattern)))))
	(else
	 (cons (car args) (touch-if-needed (cdr args) (cdr pattern))))))



