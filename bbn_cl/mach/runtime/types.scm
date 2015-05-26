;;; -*-Scheme-*-
;;;
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

;;;; Abstract Type System

(declare (usual-integrations))

(define type-system
  (let ((future-type (microcode-type 'FUTURE)))

;;;; Type Object Data Abstraction

(define type-object-tag
  '(TYPE-OBJECT))

(define universal-type
  (vector type-object-tag '() (make-population) '()))

(define type-object-type)				;To be defined later.
(define type-object?)

(define (make-type-object supremums)
  (let ((type (vector type-object-tag supremums (make-population) '())))
    (if (null? supremums)
	(error "One world is enough! -- get a wizard" 'MAKE-TYPE-OBJECT)
	(for-each (lambda (supremum)
		    (add-to-population! (vector-ref supremum 2) type))
		  supremums))
    type))

(define (type-object-relative? object)
  (and (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) type-object-tag)))

(define supremums
  vector-second)

(define (infimums type)
  (map-over-population (vector-ref type 2) identity-procedure))

;;; Logically, we consider the type heirarchy to be just the types and
;;; their supremum/infimum relationships.  Links are really just 2D
;;; properties, one of whose keys is the type.  However, since types
;;; will always have links, we allocate a slot in the type itself to
;;; hold a 1D property list, which is both smaller and faster than the
;;; corresponding 2D mechanism.

(define (get-link type key)
  (assq (object-hash key) (vector-ref type 3)))

(define (push-link-value! link item)
  (set-cdr! link (cons item (cdr link))))

(define link-value cdr)
(define set-link-value! set-cdr!)

(define (get-value type key)
  (let ((association (assq (object-hash key) (vector-ref type 3))))
    (and association (cdr association))))

(define (put-value! type key item)
  (let ((hash (object-hash key)))
    (let ((association (assq hash (vector-ref type 3))))
      (if association
	  (set-cdr! association item)
	  (vector-set! type 3 (cons (cons hash item) (vector-ref type 3)))))))

(add-secondary-gc-daemon!
 (lambda ()
   (garbage-collect-links universal-type)))

(define (garbage-collect-links type)
  (vector-set! type 3 (delete-invalid-entries! (vector-ref type 3)))
  (map-over-population! (vector-ref type 2) garbage-collect-links))

(define delete-invalid-entries!
  (list-deletor!
   (lambda (bucket)
     (not (valid-hash-number? (car bucket))))))

;;;; Principle Ideals

;;; A PRINCIPLE IDEAL is a set of the form {X; X >= A} for some type
;;; A, where > is the transitive closure of the supremum relation.
;;; Here, a principle ideal is represented by what is essentially a
;;; copy of part of the type heirarchy.

;;; Note that these are called principle ideals to conform with the
;;; terminology of lattice theory.

(define (make-principle-ideal infimum receiver)
  (let ((memos '()))
    (define (loop type)
      (let ((memo (assq type memos)))
	(if memo
	    (cdr memo)
	    (let ((node (vector type '() '())))
	      (introduce! (map loop (supremums type))
			  node)
	      (set! memos (cons (cons type node) memos))
	      node))))
    (receiver (loop universal-type)
	      (loop infimum))))

(define node-type vector-first)
(define node-supremums vector-second)
(define node-infimums vector-third)

(define (introduce-nodes! sup-node inf-node)
  (vector-set! inf-node 1 (cons sup-node (vector-ref inf-node 1)))
  (vector-set! sup-node 2 (cons inf-node (vector-ref sup-node 2))))

(define (divorce-nodes! sup-node inf-node)
  (vector-set! inf-node 1 (delq! sup-node (vector-ref inf-node 1)))
  (vector-set! sup-node 2 (delq! inf-node (vector-ref sup-node 2))))

(define (introduce! sup-nodes inf-node)
  (for-each (lambda (sup-node)
	      (introduce-nodes! sup-node inf-node))
	    sup-nodes))

(define (remove-node! node)
  (let ((sup-nodes (node-supremums node))
	(inf-nodes (node-infimums node)))
    (for-each (lambda (inf-node)
		(introduce! sup-nodes inf-node)
		(divorce-nodes! node inf-node))
	      inf-nodes)
    (for-each (lambda (sup-node)
		(divorce-nodes! sup-node node))
	      sup-nodes)))

;;;; Characterized Types

;;; A type X is CHARACTERIZED when a procedural description [called a
;;; CHARACTERISTIC, naturally] is supplied.  The procedural
;;; description should have as its domain the supremum types of X, and
;;; should be true only of objects in X.

(define (characterize! type characteristic)
  (put-value! type characteristic-tag characteristic))

(define (characteristic type)
  (get-value type characteristic-tag))

(define characteristic-tag
  '(CHARACTERISTIC))

(define (make-characterized-ideal type receiver)
  (make-principle-ideal type
    (lambda (sup-node inf-node)
      (define (loop node)
	(if (not (or (eq? node sup-node)
		     (eq? node inf-node)
		     (characteristic (node-type node))))
	    (remove-node! node))
	(for-each loop (node-infimums node)))
      (loop sup-node)
      (receiver sup-node inf-node))))

;;; Universal characteristics are predicates for a particular type.
;;; Given any object whatsoever, they return a boolean indicating
;;; whether or not the object is of that type.

(define (universal-characteristic type)
  (or (get-value type universal-characteristic-tag)
      (make-characterized-ideal type
	(lambda (sup-node inf-node)
	  (let ((characteristic
		 (make-universal-characteristic sup-node inf-node)))
	    (put-value! type
			universal-characteristic-tag
			characteristic)
	    characteristic)))))

(define universal-characteristic-tag
  '(UNIVERSAL-CHARACTERISTIC))

(define (make-universal-characteristic sup-node inf-node)
  (let ((nodes (node-infimums sup-node)))
    (if (and (null? (cdr nodes))
	     (eq? (car nodes) inf-node))
	(or (characteristic (node-type inf-node))
	    (lambda (x) #T))
	(make-referencer
	 (lambda (refer-to refer-to-object)
	   (define (and-loop nodes)
	     (if (null? nodes)
		 #T
		 (or-loop nodes)))
	   (define (or-loop nodes)
	     (let ((characteristic (characteristic (node-type (car nodes)))))
	       (if (not characteristic)
		   #T
		   (let ((predicate (make-combination (refer-to characteristic)
						      (list refer-to-object)))
			 (consequent (and-loop (node-infimums (car nodes))))
			 (alternative (if (null? (cdr nodes))
					  #F
					  (or-loop (cdr nodes)))))
		     (if (and (eq? consequent #T)
			      (eq? alternative #F))
			 predicate
			 (make-conditional predicate
					   consequent
					   alternative))))))
	   (and-loop nodes))))))

;;;; Dispatching

(define (make-general-dispatcher alist universal-property)
  (let ((alist (make-fringe-alist alist)))
    (make-characterized-ideal
     (cond ((null? alist) universal-type)
	   ((null? (cdr alist)) (caar alist))
	   (else (make-type-object (map car alist))))
     (attach-properties (search-alist alist)
			universal-property
			make-dispatcher))))

(define ((attach-properties get-property universal-property
			    receiver)
	 sup-node inf-node)
  (let ((tag (list sup-node)))
    (define ((loop inherited-property) node)
      (if (not (get-link (node-type node) tag))
	  (for-each (let ((property (get-property node inherited-property)))
		      (put-value! (node-type node) tag property)
		      (loop property))
		    (node-infimums node))))
    ((loop universal-property) sup-node)
    (receiver sup-node inf-node tag)))

(define (make-fringe-alist alist)
  (let ((alist* '()))
    (for-each (lambda (association)
		(define (loop type)
		  (if (characteristic type)
		      (set! alist*
			    (cons (cons type (cdr association))
				  alist*))
		      (for-each loop (supremums type))))
		(if (characteristic (car association))
		    (set! alist* (cons association alist*))
		    (for-each loop (supremums (car association)))))
	      alist)
    alist*))

(define ((search-alist alist) node default)
  (let ((association (assq (node-type node) alist)))
    (if association
	(cdr association)
	default)))

(define (transform-general-dispatcher dispatcher alist universal-transform)
  (let ((alist (make-fringe-alist alist)))
    (dispatcher-components dispatcher
      (lambda (sup-node inf-node tag)
	(make-characterized-ideal
	 (if (null? alist)
	     (node-type inf-node)
	     (make-type-object (cons (node-type inf-node)
				     (map car alist))))
	 (transform-properties (search-alist alist)
			       universal-transform
			       (get-property tag)
			       (get-value (node-type sup-node) tag)
			       make-dispatcher))))))

(define ((transform-properties get-transform universal-transform
			       get-property universal-property
			       receiver)
	 sup-node inf-node)
  (let ((tag (list sup-node)))
    (define ((loop inherited-transform inherited-property) node)
      (if (not (get-link (node-type node) tag))
	  (for-each (let ((transform (get-transform node inherited-transform))
			  (property (get-property node inherited-property)))
		      (put-value! (node-type node) tag (transform property))
		      (loop transform property))
		    (node-infimums node))))
    ((loop universal-transform universal-property) sup-node)
    (receiver sup-node inf-node tag)))

(define ((get-property tag) node default)
  (let ((link (get-link (node-type node) tag)))
    (if link
	(link-value link)
	default)))

;;;; Dispatcher Data Abstraction

(define (make-dispatcher sup-node inf-node tag)
  (let ((root-types (map (lambda (node)
			   (cons (node-type node) node))
			 (node-infimums sup-node))))
    (if (all-microcode-types? root-types)
	(physical-dispatcher
	 (list->vector
	  (map (let ((default (get-value (node-type sup-node) tag)))
		 (lambda (type)
		   (let ((association (assq type root-types)))
		     (if association
			 (make-dispatch (cdr association) inf-node tag)
			 (lambda (object) default)))))
	       (vector->list microcode-type-table)))
	 (assq future-type root-types))
	(make-dispatch sup-node inf-node tag))))

(define (dispatcher-components dispatcher receiver)
  (let ((environment (procedure-environment dispatcher)))
    (receiver (access sup-node environment)
	      (access inf-node environment)
	      (access tag environment))))

(define (physical-dispatcher dispatch-vector touch?)
  (if touch?
      (named-lambda (generated-physical-dispatcher object)
	((vector-ref dispatch-vector (primitive-type object)) object))
      (named-lambda (non-strict-generated-physical-dispatcher object)
	(if (future? object)
	    ((vector-ref dispatch-vector future-type) object)
	    ((vector-ref dispatch-vector (primitive-type object)) object)))))

(define (all-microcode-types? types)
  (or (null? types)
      (and (microcode-type? (caar types))
	   (all-microcode-types? (cdr types)))))

;;; A great deal of delicacy here to handle the case where the
;;; INF-NODE is uncharacterized.

(define (make-dispatch sup-node inf-node tag)
  (make-referencer
   (lambda (refer-to refer-to-object)
     (define (node-loop node)
       (define (inf-loop inf-nodes)
	 (if (null? inf-nodes)
	     (refer-to (get-value (node-type node) tag))
	     (let ((tail (inf-loop (cdr inf-nodes))))
	       (if (eq? (car inf-nodes) inf-node)
		   (let ((characteristic
			  (characteristic (node-type inf-node))))
		     (if characteristic
			 (make-conditional
			  (make-combination (refer-to characteristic)
					    (list refer-to-object))
			  (refer-to (get-value (node-type inf-node) tag))
			  tail)
			 tail))
		   (make-conditional
		    (make-combination
		     (refer-to (characteristic (node-type (car inf-nodes))))
		     (list refer-to-object))
		    (node-loop (car inf-nodes))
		    tail)))))
       (inf-loop (node-infimums node)))
     (node-loop sup-node))))

;;;; Compilation

(define generate-variable
  (let ((counter 0))
    (named-lambda (generate-variable prefix)
      (set! counter (1+ counter))
      (make-variable
       (string->symbol
	(with-output-to-string
	 (lambda () (display prefix) (display counter))))))))

(define (make-referencer receiver)
  (let ((references '())
	(refer-to-object (generate-variable "REFER")))
    (let ((body
	   (receiver (lambda (object)
		       (let ((ref (assq object references)))
			 (if ref
			     (cdr ref)
			     (let ((name (generate-variable "VARIABLE")))
			       (set! references
				     (cons (cons object name)
					   references))
			       name))))
		     refer-to-object)))
      (apply (scode-eval (make-simple-lambda  
			  (map (lambda (assoc)
				 (variable-name (cdr assoc)))
			       references)
			  (make-simple-lambda 
			   (list (variable-name refer-to-object))
			   body))
			 type-system)
	     (map car references)))))

(define (make-simple-lambda required body)
  (make-lambda* 'SIMPLE-LAMBDA required '() '() body))

;;;; Named Types

(define (make-sub-type name supremums characteristic)
  (let ((type-object (make-type-object supremums)))
    (characterize! type-object characteristic)
    (put-type-name! type-object name)
    type-object))

(define (make-union-type name components)
  (let ((type-object (make-type-object components)))
    (put-type-name! type-object name)
    type-object))

(define put-type-name!)
(define get-type-name)

(let ((name-tag '(TYPE-NAME)))
  (set! put-type-name!
	(named-lambda (put-type-name! type-object name)
	  (put-value! type-object name-tag name)))
  (set! get-type-name
	(named-lambda (get-type-name! type-object)
	  (get-value type-object name-tag))))

;;;; Microcode Types

(define microcode-type-table)

(define (microcode-type-object name)
  (or (vector-ref microcode-type-table (microcode-type name))
      (error "Missing microcode type" 'MICROCODE-TYPE-OBJECT name)))

(define (make-microcode-type code)
  (let ((name (microcode-type-name code)))
    (and (not (null? name))
	 (let ((type (make-sub-type name
				    (list universal-type)
				    (microcode-type-characteristic code))))
	   (put-value! type microcode-type-tag code)
	   type))))

(define ((microcode-type-characteristic code) object)
  (primitive-type? code object))

(define microcode-type-tag
  '(MICROCODE-TYPE))

(define (microcode-type? type)
  (get-value type microcode-type-tag))

;;;; Heirarchy Initialization

(define (install)
  (set! microcode-type-table
	(make-initialized-vector number-of-microcode-types
				 make-microcode-type))
  (set! type-object-type
	(make-sub-type 'TYPE-OBJECT
		       (list (microcode-type-object 'VECTOR))
		       type-object-relative?))
  (set! type-object?
	(universal-characteristic type-object-type)))

;;; end TYPE-SYSTEM package.
(the-environment)))

((access install type-system))

;;;; Exports

(define type-object-type
  (access type-object-type type-system))

(define (type-object? object)
  ((access type-object? type-system) object))

(define (microcode-type-object name)
  ((access microcode-type-object type-system) name))

(define (type-object-predicate type)
  ((access universal-characteristic type-system) type))

(define (type-object-name type)
  ((access get-type-name type-system) type))

(define (make-sub-type name supremum characteristic)
  (if (type-object? supremum)
      ((access make-sub-type type-system) name (list supremum) characteristic)
      (error "Not a valid supremum type" 'MAKE-SUB-TYPE supremum)))

(define (make-union-type name . components)
  (define (find-first-non-type rest)
    (cond ((null? rest)
	   ((access make-union-type type-system) name components))
	  ((type-object? (car rest))
	   (find-first-non-type (cdr rest)))
	  (else
	   (error "Not a type object" 'MAKE-UNION-TYPE (car rest)))))
  (if (null? components)
      (error "Must supply components" 'MAKE-UNION-TYPE name)
      (find-first-non-type components)))

(define make-type-dispatcher)
(define transform-type-dispatcher)

(let ()
  (define ((convert-alist-entry name) entry)
    (if ((access type-object? type-system) (car entry))
	(cons (car entry) (cadr entry))
	(error "Not a type object" name (car entry))))
  (set! make-type-dispatcher
	(lambda (alist default)
	  ((access make-general-dispatcher type-system)
	   (map (convert-alist-entry 'MAKE-TYPE-DISPATCHER)
		alist)
	   default)))
  (set! transform-type-dispatcher
	(lambda (dispatcher alist default)
	  ((access transform-general-dispatcher type-system)
	   dispatcher
	   (map (convert-alist-entry 'TRANSFORM-TYPE-DISPATCHER) alist)
	   default))))
