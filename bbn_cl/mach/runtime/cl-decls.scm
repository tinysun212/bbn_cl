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
;; Needed for the time being

(define unknown-keyword 'unknown-keyword)
(define type-keyword 'type-keyword)
(define shadowed-keyword 'shadowed-keyword)
(define declarations-keyword 'declarations-keyword)
(define optimizer-keyword 'optimizer-keyword)
(define inline-keyword 'inline-keyword)
(define ignore-keyword 'ignore-keyword)
(define special-keyword 'special-keyword)
(define structure-operation-keyword 'structure-operation-keyword)

(define prim-values (make-primitive-procedure 'values))
(define prim-with-values (make-primitive-procedure 'with-values))
(define prim-values-list (make-primitive-procedure 'values-list))

;;; The attribute abstraction which is used to store
;;; information about bindings during the syntaxing
;;; process.

(define (add-binding! var-name value)
  (local-assignment *syntax-time-env* var-name value))

(define attribute-header '(ATTRIBUTE-LIST))

(define (make-attribute-list . attributes)
  (cons attribute-header attributes))

(define (attribute-list? l)
  (and (pair? l)
       (eq? (car l) attribute-header)))

(define attribute-list-attributes cdr)

(define make-attribute cons)
(define attribute-key car)
(define attribute-value cdr)
(define set-attribute-value! set-cdr!)

(define (add-attribute! attributes attribute)
  (let ((old-attribute (assq (attribute-key attribute) attributes)))
    (if old-attribute
	(set-attribute-value! old-attribute (attribute-value attribute))
	(append! attributes (list attribute)))))

(define (lookup-attributes var-name)
  (if (not (lexical-unreferenceable? *syntax-time-env* var-name))
      (let ((value (lexical-reference *syntax-time-env* var-name)))
	(if (attribute-list? value)
	    value
	    unknown-keyword))
      unknown-keyword))

(define (lookup-attribute var-name attribute)
  (let ((attributes (lookup-attributes var-name)))
    (if (not (eq? attributes unknown-keyword))
	(let ((attribute (assq attribute attributes)))
	  (if attribute
	      (attribute-value attribute)
	      unknown-keyword))
	unknown-keyword)))

;;; Initialization

(local-assignment *syntax-time-env* '*touch-mode* (make-attribute-list
						    (make-attribute 'mode #f)))

;;; This mode is initially off so tha twe don't change the behavior
;;; of Scheme and build-time Common Lisp. At the end of build-cl,
;;; it is turned on to properly default commonlisp.

(local-assignment *syntax-time-env* '*lr-evaluation-mode* 
		  (make-attribute-list
		   (make-attribute 'mode #f)))

;;; Changing the declared attributes in the *syntax-time-env*

(define (set-declared-variable-attribute! var-name attribute)
  (let ((old-attributes (lookup-attributes var-name)))
    (add-attribute! old-attributes attribute)))

;;; Extending the *syntax-time-env* with variables

(define (make-environment-extender default-attribute-maker)
  (lambda (var-name-list)
    (let loop ((l var-name-list))
      (cond ((null? l)
	     '())
	    ((not (pair? l))
	     (local-assignment *syntax-time-env* l (default-attribute-maker)))
	    (else
	     (local-assignment *syntax-time-env* (car l) (default-attribute-maker))
	     (loop (cdr l)))))))

(define extend-environment!
  (make-environment-extender
   (lambda ()
     (make-attribute-list))))

(define shadow-environment! 
  (make-environment-extender
   (lambda ()
     (make-attribute-list (make-attribute shadowed-keyword #t)))))

;;; Extend the environment, shadowing all variables

(define (with-shadowed-variables var-name-list continuation)
  (fluid-let ((*syntax-time-env* *syntax-time-env*))
    (if *process-declarations*
	(begin (set! *syntax-time-env* (push-contour *syntax-time-env*))
	       (shadow-environment! var-name-list)))
    (continuation)))

;;; Extend the environment, shadowing only the variables that are already shadowed

(define (with-variables var-name-list continuation)
  (fluid-let ((*syntax-time-env* *syntax-time-env*))
    (if *process-declarations*
	(begin (set! *syntax-time-env* (push-contour *syntax-time-env*))
	       (let ((to-re-shadow
		      (list-transform-positive var-name-list shadowed?)))
		 (extend-environment! var-name-list)
		 (mapcar shadow! to-re-shadow))))
    (continuation)))

;;;; Various attributes

(define (shadow! name)
  (set-declared-variable-attribute! name (make-attribute shadowed-keyword #t)))

(define (shadowed? name)
  (eq? #t (lookup-attribute name shadowed-keyword)))

(define (special? name)
  (eq? #t (lookup-attribute name special-keyword)))

(define (add-name-to-current-environment-if-needed! name)
  (let ((att (lookup-attributes name)))
    (if (eq? att unknown-keyword)
	(local-assignment *syntax-time-env* name (make-attribute-list)))))

(define (inline-structure-operation? operation)
  (and (symbol? operation)
       (let ((fname (fundefsym operation)))
	 (and (not (declared-not-inlineable? fname))
	      (not (eq? (lookup-attribute fname structure-operation-keyword)
			unknown-keyword))))))

;;; Declaring and referencing variable types

(define (set-declared-variable-type! var-name type)
  (set-declared-variable-attribute! var-name (make-attribute type-keyword type)))

(define (declared-variable-type var-name)
  (lookup-attribute var-name type-keyword))

;;; Inline information

(define (set-declared-variable-inlineable! var-name inline? type)
  (let ((fname (fundefsym var-name)))
    (if (eq? type 'proclaim)
	(fluid-let ((*syntax-time-env* *syntax-time-global-env*))
	  (add-name-to-current-environment-if-needed! fname)
	  (set-declared-variable-attribute! fname
					    (make-attribute inline-keyword inline?)))
	(begin (set! *syntax-time-env* (push-contour *syntax-time-env*))
	       (shadow-environment! (list fname))
	       (set-declared-variable-attribute! fname
						 (make-attribute inline-keyword inline?))))))

(define (declared-not-inlineable? fname)
  (eq? (lookup-attribute fname inline-keyword) '()))

;;; Function type information

(define (declared-function-type var-name)
  (if (symbol? var-name)
      (declared-variable-type (fundefsym var-name))
      unknown-keyword))

;;; Function type declaration parsing
;; A fuction type lookes like (function (fixnum float) list)

(define function-result-type third)
(define function-arg-types second)

;;; Modes

(define (set-mode! mode-name value)
  (set-declared-variable-attribute! mode-name (make-attribute 'mode value)))

(define (get-mode mode-name)
  (lookup-attribute mode-name 'mode))

;;; Scode type binding

(define *scode-types*)

(define (make-scode-type-table)
  (make-vector 512))

(define (scode-type-in-hashtable scode)
  (let* ((h (hash scode))
	 (i (modulo h (vector-length *scode-types*)))
	 (bucket (vector-ref *scode-types* i))
	 (entry (assq h bucket)))
    (if entry 
	(cdr entry)
	unknown-keyword)))

;; All types are stored in the hash table.  The optimization
;; to not store #t types cannot be used because checking the
;; type of an object will cause ambiguities between type #t
;; and type :UNKNOWN.

(define (set-scode-type! scode type)
  (let* ((h (hash scode))
	 (i (modulo h (vector-length *scode-types*)))
	 (bucket (vector-ref *scode-types* i)))
    (vector-set! *scode-types* i (cons (cons h type) bucket))
    'done))

;;; Optimizer tables

(define *optimizer-hashtable*
  (make-vector 512))

(define (function-optimizer scode)
  (let* ((h (hash scode))
	 (i (modulo h (vector-length *optimizer-hashtable*)))
	 (bucket (vector-ref *optimizer-hashtable* i))
	 (entry (assq h bucket)))
    (if entry 
	(cdr entry)
	unknown-keyword)))

(set! add-optimizer!
      (named-lambda (add-function-optimizer! scode opt)
	(let* ((h (hash scode))
	       (i (modulo h (vector-length *optimizer-hashtable*)))
	       (bucket (vector-ref *optimizer-hashtable* i)))
	  (vector-set! *optimizer-hashtable* i (cons (cons h opt) bucket)))))

;;; Generalized type queries

;; If it is not in the hash table and it is not an scode constant,
;; it is of unknown type.

(set! syntax-type-of
      (named-lambda (syntax-type-of object)
	(let ((type (scode-type-in-hashtable object)))
	  (if (eq? type unknown-keyword)
	      (let ((l (lookup-constant object)))
		(if l
		    (cdr l)
		    unknown-keyword))
	      type))))

(set! syntax-constant?
      (named-lambda (syntax-constant? object)
	(lookup-constant object)))	      

;;; Declaration parsing

;; A processed-declaration lookes like '(:declarations <list of bound-variables> <list of decls>)
;; A decl lookes like (declare <list of decl-specs>)
;; A decl-spec lookes like (fixnum x) or (type foo y), etc.

;;; A little bit of abstraction

(define (commonlisp-declaration? exp)
  (and (pair? exp)
       (eq? (first exp) 'quote)
       (pair? (second exp))
       (eq? (first (second exp)) declarations-keyword)))

;; Accessing from a processed-declaration

(define get-unquoted-decls second)

;; Accessing from an unquoted-processed-declaration

(define get-bvl second)
(define get-list-of-decls cddr)

;; Accessing from a decl

(define get-list-of-decl-specs cdr)

;; Accessing from a decl-spec

(define (get-type decl-spec)
  (let ((x (first decl-spec)))
    (if (eq? x 'type)
	(second decl-spec)
	(if (eq? x 'ftype)   ;; This is kind of incomplete
	    'function
	    x))))

;; Accessing processed-declarations and bodies
;; from lambda expressions

;; Does a lambda-body have declarations?

(define (has-decls? body)
  (and (pair? body)
       (eq? (first body) 'begin)
       (pair? (second body))
       (eq? (first (second body)) 'quote)
       (pair? (second (second body)))
       (eq? (first (second (second body))) declarations-keyword)))

;; Get declarations from a lambda

(define (get-lambda-decls lambda-expr)
  (get-body-decls (third lambda-expr)))

(define (get-body-decls body)
  (if (has-decls? body)
      (second body)
      '()))

;; Get body from a lambda

(define (get-lambda-body lambda-expr)
  (get-body-body (third lambda-expr)))

(define (get-body-body body)
  (if (has-decls? body)
      (third body)
      (list body)))

;; Given a processed declaration, return an ordered list of
;; unquoted processed declarations.

(define (parse-declarations-by-bvl processed-decls)
  (if (null? processed-decls)
      '()
      (let ((unquoted-decls (get-unquoted-decls processed-decls)))
	(let ((bvl (get-bvl unquoted-decls))
	      (list-of-declarations (get-list-of-decls unquoted-decls)))
	  (package-decl-specs
	   bvl
	   (order-decls bvl (linearize-decls list-of-declarations)))))))

;; Given a list of decls, return a list of decl-specs that have been pre-parsed.

(define (linearize-decls list-of-decls)
  (if (null? list-of-decls)
      '()
      (append (split-decls (get-list-of-decl-specs (car list-of-decls)))
	      (linearize-decls (cdr list-of-decls)))))

;; Given a list of decl-specs, return a list of linearized parsed
;; decl-specs.  i.e., (fixnum a b c) ==> (fixnum a) (fixnum b) (fixnum c)
;; This also turns FUNCTION declarations into FTYPE and does automatic
;; FUTURE unioning.

(define split-decls)

(let ()
  (define (process-argument-type type)
    (if (and (touch-mode)
	     (not (non-future-type? type)))
	(list 'or 'future type)
	type))
  (define (process-variable-type type)
    (if (and (touch-mode)
	     (not (non-future-type? type))
	     (not (eq? type 'special)))
	(list 'type (list 'or 'future type))
	(list type)))
  (define (make-decl-pairs type vars)
    (if (null? vars)
	'()
	(cons (append type (list (car vars)))
	      (make-decl-pairs type (cdr vars)))))
  (define (make-decl-triples key1 type vars)
    (if (null? vars)
	'()
	(cons (list key1 type (car vars))
	      (make-decl-triples key1 type (cdr vars)))))
  (set! split-decls
	(named-lambda (split-decls list-of-specs)
	  (if (null? list-of-specs)
	      '()
	      (let ((spec (car list-of-specs)))
		(let ((key (car spec))
		      (vars (cdr spec)))
		  (cond ((or (eq? key 'special)
			     (memq key *normal-types*))
			 (append (make-decl-pairs (process-variable-type key)
						  vars)
				 (split-decls (cdr list-of-specs))))
			((eq? key 'ignore)
			 (append (make-decl-pairs '(ignore) vars)
				 (split-decls (cdr list-of-specs))))
			((or (eq? key 'type)
			     (eq? key 'ftype))
			 (append (make-decl-triples
				  key
				  (let ((typ (process-variable-type (car vars))))
				    (if (eq? (car typ) 'type)
					(cadr typ)
					(car typ)))
				  (cdr vars))
				 (split-decls (cdr list-of-specs))))
			((eq? key 'function)
			 (let ((name (second spec))
			       (args (mapcar process-argument-type
					     (third spec)))
			       (results (mapcar process-argument-type (cdddr spec))))
			   (cons `(ftype (function ,args ,@results) ,name)
				 (split-decls (cdr list-of-specs)))))
			(else (cons spec
				    (split-decls (cdr list-of-specs))))))))))
)

;; Order decl-specs according to bvl.  Those left over get put at the end.

(define order-decls)

(let ()
  (define (decl-searcher var)
    (lambda (x) (and (pair? x)
		     (eq? (car (last-pair x))
			  var))))
  (set! order-decls
	(named-lambda (order-decls bvl decl-specs)
	  (if (null? bvl)
	      decl-specs
	      (let ((found (list-search-positive decl-specs (decl-searcher (car bvl)))))
		(if found
		    (cons found 
			  (order-decls (cdr bvl) 
				       (list-transform-negative decl-specs (decl-searcher (car bvl)))))
		    (cons '()
			  (order-decls (cdr bvl) decl-specs))))))))

;; Given a BVL and an orderd list of decl-specs, return an ordered list
;; of unquoted processed declarations ordered according to the BVL
;; i.e. (a b c) ((flonum a) () (fixnum c) (optimize foo)) ==>
;;        ((:declarations (a) (declare (flonum a)))
;;  	   ()
;;	   (:declarations (c) (declare (fixnum c)
;;                                     (optimize foo))))

(define (package-decl-specs bvl decl-specs)
  (if (or (null? bvl) (null? (cdr bvl)))
      (if decl-specs
	  `((,declarations-keyword ,bvl (declare ,@(delq '() decl-specs))))
	  '())
      (cons (if (car decl-specs)
		`(,declarations-keyword (,(car bvl)) (declare ,(car decl-specs)))
		'())
	    (package-decl-specs (cdr bvl) (cdr decl-specs)))))

;; Given a decl-spec and a list-of decl-specs, return
;; a merged list of decl-specs.  Remove null decl-specs

(define (merge-declarations decl-spec1 list-of-decl-specs)
  (if (null? decl-spec1)
      list-of-decl-specs
      (delq '() (cons decl-spec1 list-of-decl-specs))))

;; Merge two bvls

(define merge-bvls append)

;; Future processing

(define (set-touches-mode! decl type)
  (if (and (pair? decl)
	   (= (length decl) 1))
      (cond ((eq? (car decl) #f)
	     (turn-touches-off! type))
	    ((eq? (car decl) #t)
	     (turn-touches-on! type))
	    (else (warn "Unknown mode for INSERT-TOUCHES: ~A" (car decl))))
      (warn "Illegal format of INSERT-TOUCHES declaration: ~A" decl)))	  

(define (turn-touches-on! type)
  (if (not (eq? type 'proclaim))
      (begin (set! *syntax-time-env* (push-contour *syntax-time-env*))
	     (shadow-environment! '(*touch-mode*))))
  (set-mode! '*touch-mode* #t))

(define (turn-touches-off! type)
  (if (not (eq? type 'proclaim))
      (begin (set! *syntax-time-env* (push-contour *syntax-time-env*))
	     (shadow-environment! '(*touch-mode*))))
  (set-mode! '*touch-mode* #f))

(set! touch-mode
      (lambda () (get-mode '*touch-mode*)))

(define (non-future-type? x)
  (and (pair? x)
       (eq? (car x) 'non-future)))

;;; Left-to-right-evaluation control

(define (set-lr-evaluation-mode! decl type)
  (if (and (pair? decl)
	   (= (length decl) 1))
      (if (memq (car decl) '(#t #f))
	  (begin
	    (if (not (eq? type 'proclaim))
		(begin
		  (set! *syntax-time-env* (push-contour *syntax-time-env*))
		  (shadow-environment '(*lr-evaluation-mode*))))
	    (set-mode! '*lr-evaluation-mode* (car decl)))
	  (warn "Unknown mode for LR-EVALUATION: ~A" (car decl)))
      (warn "Illegal format of LR-EVALUATION declaration: ~A" decl)))

(set! lr-evaluation-mode?
      (lambda () (get-mode '*lr-evaluation-mode*)))

;; Additional declarations as declared by (DECLARATION ...)

(define *additional-declaration-types* '())

;;; Note that FUNCTION is missing from the following list.
;;; it is treated specially by declaration syntax.

(define *normal-types*
  '(array atom bignum bit bit-vector character common compiled-function
	  complex cons double-float fixnum float hash-table
	  integer keyword list long-float #f null number package
	  pathname random-state ratio rational readtable sequence
	  short-float simple-array simple-bit-vector simple-string
	  simple-vector single-float standard-char stream string
	  string-char symbol #t vector))

'$split-file

;;; Declaration processing

;; Declarations come as one of two modes:
;; 'DECLARE and 'PROCLAIM.

(define (setup-decls! processed-decls mode)
  (let ((unquoted-decls (get-unquoted-decls processed-decls)))
    (let ((bvl (get-bvl unquoted-decls))
	  (list-of-decl-specs
	   (get-list-of-decl-specs
	    ;; There will be only one decl at this point
	    (car (get-list-of-decls unquoted-decls)))))
      ;; We want the specials to appear first in the list so that
      ;; special bindings slots will get created in the *syntax-time-env* before
      ;; we attach type information to it.
      (let ((specials-first-in-list
	     (sort list-of-decl-specs (lambda (spec1 spec2) (eq? (car spec1) 'special)))))
	;; Note that for-each does these in order
	(for-each (lambda (x) (setup-decl-spec! x bvl mode))
		  specials-first-in-list)))))

(define (setup-decl-spec! decl-spec bvl mode)
  (let ((dispatch (car decl-spec)))
    (cond ((eq? dispatch 'special)
	   (setup-special-decl! decl-spec bvl))
	  ((eq? dispatch 'type)
	   (setup-type-decl! (cdr decl-spec) bvl mode))
	  ((eq? dispatch 'ftype)
	   (setup-ftype-decl! (cadr decl-spec) (caddr decl-spec) mode))
	  ;; Function gets coerced into ftype by declaration parsing
	  ((memq dispatch *normal-types*)
	   (setup-type-decl! decl-spec bvl mode))
	  ((eq? dispatch 'inline)
	   (setup-inline-decl! (cdr decl-spec) mode))
	  ((eq? dispatch 'notinline)
	   (setup-notinline-decl! (cdr decl-spec) mode))
	  ((eq? dispatch 'ignore)
	   (setup-ignore-decl! (cdr decl-spec) bvl mode))
	  ((eq? dispatch 'optimize)
	   (setup-optimize-decl! (cdr decl-spec) mode))
	  ((eq? dispatch 'declaration)
	   (if (eq? mode 'proclaim)
	       (setup-declaration-decl! (cdr decl-spec))
	       (warn "~A can only be processed as a PROCLAIM" decl-spec)))
	  ((memq dispatch *additional-declaration-types*)
	   #f)
	  ;; Extra declaration interpretation
	  ((eq? dispatch 'insert-touches)
	   (set-touches-mode! (cdr decl-spec) mode))
	  ;; This is emitted by defstruct and is not exported
	  ((eq? dispatch 'structure-operation)
	   (setup-structure-operation-decl! (cdr decl-spec) mode))
	  ((eq? dispatch 'lr-evaluation)
	   (set-lr-evaluation-mode! (cdr decl-spec) mode))
	  (else
	   (warn "ignoring unknown declaration: ~A" decl-spec)))))

(define (setup-special-decl! decl-spec bvl)
  (if *decl-debug* (print `("Processing special:" ,(cdr decl-spec))))
  (let ((name (cadr decl-spec)))
    (if (shadowed? name)
	;; Ah ha, the dreaded "Special declaration within lexical binding"
	(shadow-environment! name)
	;; Normal special declaration, no higher lexical bindings of the same name
	(extend-environment! name))
    (set-declared-variable-attribute! name (make-attribute special-keyword #t))))

(define (setup-type-decl! decl-spec bvl mode)
  (let ((type (car decl-spec))
	(variable (cadr decl-spec)))
    (if *decl-debug* (print `("Processing type:" ,type ,variable)))
    (cond ((eq? mode 'proclaim)
	   (fluid-let ((*syntax-time-env* *syntax-time-global-env*))
	     (add-name-to-current-environment-if-needed! variable)
	     (set-declared-variable-type! variable type)))
	  ((memq variable bvl)
	   (set-declared-variable-type! variable type))
	  (else
	   (warn "Ignoring out of scope declaration: ~A" (list type variable))))))

(define (setup-ftype-decl! type-spec function-name mode)
  (let ((processed-name (fundefsym function-name)))
    (if *decl-debug* (print `("Processing ftype:" ,function-name ,type-spec)))
    (if (eq? mode 'proclaim)
	(fluid-let ((*syntax-time-env* *syntax-time-global-env*))
	  (add-name-to-current-environment-if-needed! processed-name)
	  (set-declared-variable-type! processed-name type-spec))
	(begin
	  (extend-environment! processed-name)
	  (set-declared-variable-type! processed-name type-spec)))))

(define (setup-inline-decl! variables mode)
  (if *decl-debug* (print `("Processing inline:" ,variables)))
  (mapcar (lambda (a) (set-declared-variable-inlineable! a #t mode))
	  variables))

(define (setup-notinline-decl! variables mode)
  (if *decl-debug* (print `("Processing notinline:" ,variables)))
  (mapcar (lambda (a) (set-declared-variable-inlineable! a #f mode))
	  variables))

(define (setup-ignore-decl! vars bvl mode)
  (if *decl-debug* (print `("Processing ignore:" ,vars)))
  (mapc (lambda (x)
	  (cond ((eq? mode 'proclaim)
		 (fluid-let ((*syntax-time-env* *syntax-time-global-env*))
		   (add-name-to-current-environment-if-needed! x)
		   (set-declared-variable-attribute! x (make-attribute ignore-keyword #t))))
		((memq x bvl)
		 (set-declared-variable-attribute! x (make-attribute ignore-keyword #t)))
		(else
		 (warn "Ignoring out of scope declaration: ~A" (list 'ignore x)))))
	vars))

(define (setup-optimize-decl! optimization mode)
  (if *decl-debug* (print `("Processing optimization:" ,optimization)))
  (warn "Ignoring optimization: ~A" optimization))

(define (setup-declaration-decl! names)
  (if *decl-debug* (print `("Processing declaration:" ,names)))
  (set! *additional-declaration-types*
	(append names *additional-declaration-types*)))

(define (setup-structure-operation-decl! spec mode)
  (if *decl-debug* (print `("Processing structure-operation:" ,spec)))
  (let ((fcn-name (fundefsym (car spec))))
    (if (eq? mode 'proclaim)
	(fluid-let ((*syntax-time-env* *syntax-time-global-env*))
	  (add-name-to-current-environment-if-needed! fcn-name)
	  (set-declared-variable-attribute! 
	   fcn-name 
	   (make-attribute structure-operation-keyword spec)))
	(begin
	  (extend-environment! fcn-name)
	  (set-declared-variable-attribute! 
	   fcn-name 
	   (make-attribute structure-operation-keyword spec))))))

(set! syntaxer-proclaim!
      (lambda (decl-spec)
	(mapcar (lambda (spec)
		  (setup-decl-spec! spec '() 'PROCLAIM))
		(split-decls (list decl-spec)))))

;;; Constant detection

(define (lookup-constant scode)
  (assq (microcode-type-name (primitive-type scode)) *constant-objects-table*))

(define *constant-objects-table*
  '((null . null)
    (pair . cons)
    (character .character)
    (uninterned-symbol . symbol)
    (flonum . float)
    (true . symbol)
    (extended-procedure . function)
    (vector . simple-vector)
    (compiled-procedure . function)
    (bignum . bignum)
    (procedure . function)
    (primitive-external . function)
    (extended-lambda . function)
    (lambda . function)
    (fixnum . fixnum)
    (interned-symbol . function)
    (string . simple-string)
    (vector-1b . simple-bit-vector)
    (recnum . complex)
    (g-vector . structure)
    (cl-package . package)
    (ratio . ratio)
    (cl-stream . stream)
    (cl-array . array)))
