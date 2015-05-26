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

;;;; Object Hashing, populations, and 2D tables

;;; The hashing code, and the population code below, depend on weak
;;; conses supported by the microcode.  In particular, both pieces of
;;; code depend on the fact that the car of a weak cons becomes #F if
;;; the object is garbage collected.

;;; Important: This code must be rewritten for a parallel processor,
;;; since two processors may be updating the data structures
;;; simultaneously.

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  set-car-if-eq?!
	  set-cdr-if-eq?!
	  vector-set-if-eq?!
	  atomic-add-car!))

;;;; Object hashing

;;; How this works:

;;; There are two tables, the hash table and the unhash table:

;;; - The hash table associates objects to their hash numbers.  The
;;; entries are keyed according to the address (datum) of the object,
;;; and thus must be recomputed after every relocation (ie. band
;;; loading, garbage collection, etc.).

;;; - The unhash table associates the hash numbers with the
;;; corresponding objects.  It is keyed according to the numbers
;;; themselves.

;;; In order to make the hash and unhash tables weakly hold the
;;; objects hashed, the following mechanism is used:

;;; The hash table, a vector, has a SNMV header before all the
;;; buckets, and therefore the garbage collector will skip it and will
;;; not relocate its buckets.  It becomes invalid after a garbage
;;; collection and the first thing the daemon does is clear it.  Each
;;; bucket is a normal alist with the objects in the cars, and the
;;; numbers in the cdrs, thus assq can be used to find an object in
;;; the bucket.

;;; The unhash table, also a vector, holds the objects by means of
;;; weak conses.  These weak conses are the same as the pairs in the
;;; buckets in the hash table, but with their type codes changed.
;;; Each of the buckets in the unhash table is headed by an extra pair
;;; whose car is usually #T.  This pair is used by the splicing code.
;;; The daemon treats buckets headed by #F differently from buckets
;;; headed by #T.  A bucket headed by #T is compressed: Those pairs
;;; whose cars have disappeared are spliced out from the bucket.  On
;;; the other hand, buckets headed by #F are not compressed.  The
;;; intent is that while object-unhash is traversing a bucket, the
;;; bucket is locked so that the daemon will not splice it out behind
;;; object-unhash's back.  Then object-unhash does not need to be
;;; locked against garbage collection.

;;; The above locking mechanism has been changed to allow for
;;; parallelism: The car of each bucket contains a count of how many
;;; processes are currently traversing the bucket.  This number is
;;; usually 0.  When a process starts traversing it, the number is
;;; bumped up by one.  When a process ends a traversal, it bumps it
;;; down by one.  The daemon splices the bucket only if the number is 0.

(define (hash x)
  (if (eq? x false)
      0
      (object-hash x)))

(define (unhash n)
  (if (zero? n)
      false
      (or (object-unhash n)
	  (error "unhash: Not a valid hash number" n))))

(define (valid-hash-number? n)
  (or (zero? n)
      (object-unhash n)))

(define object-hash)
(define object-unhash)

(let ((pair-type (microcode-type 'PAIR))
      (weak-cons-type (microcode-type 'WEAK-CONS))
      (snmv-type (microcode-type 'MANIFEST-SPECIAL-NM-VECTOR))
      (&make-object (make-primitive-procedure '&MAKE-OBJECT)))
  (declare (integrate-primitive-procedures &make-object))

(define next-hash-number)
(define hash-table-size)
(define unhash-table)
(define hash-table)

(define (initialize-object-hash! size)
  (set! next-hash-number (list 1))
  (set! hash-table-size size)
  (set! unhash-table (vector-cons size '()))
  (set! hash-table (vector-cons (1+ size) '()))
  (vector-set! hash-table 0 (&make-object snmv-type size))
  (let initialize ((n 0))
    (if (< n size)
	(begin (vector-set! unhash-table n (cons 0 '()))
	       (initialize (1+ n))))))

;; Primitive-datum may return negative fixnums.  Until fixed...

(define safe-primitive-datum
  (let ((smallest-positive-bignum
	 (let loop ((x 1) (y 2))
	   (if (primitive-type? (primitive-type x) y)
	       (loop y (* y 2))
	       (* y 2)))))
    (named-lambda (safe-primitive-datum object)
      (let ((n (primitive-datum object)))
	(if (not (negative? n))
	    n
	    (+ smallest-positive-bignum n))))))	

;;; This is safe because it locks the garbage collector out only for a
;;; little time, enough to tag the bucket being searched, so that the
;;; daemon will not splice that bucket.

(set! object-unhash
(named-lambda (object-unhash number)
  (let ((index (modulo number hash-table-size)))
    (with-interrupt-mask interrupt-mask-none
     (lambda (ignore)
       (let ((bucket (vector-ref unhash-table index)))
	 (atomic-add-car! bucket 1)
	 (let ((result
		(with-interrupt-mask interrupt-mask-gc-ok
		 (lambda (ignore)
		   (let loop ((l (cdr bucket)))
		     (cond ((null? l) false)
			   ((= number (system-pair-cdr (car l)))
			    (system-pair-car (car l)))
			   (else (loop (cdr l)))))))))
	   (atomic-add-car! bucket -1)
	   result)))))))

;;; We can't use atomic-add-car! because the hash number may overflow
;;; into bignum range.

(define (get-next-hash-number)
  (let loop ((n (car next-hash-number)))
    (if (set-car-if-eq?! next-hash-number (1+ n) n)
	n
	(loop (car next-hash-number)))))

;;; This should not be dangerous because assq is a primitive and does
;;; not cons.  The rest of the consing (including that by the
;;; interpreter) is a small amount.  On a single processor this should
;;; never need to back out since the only time it needs to do it is
;;; when someone else has munged the structures in the interim.  On a
;;; parallel processor it may need to, and some potential hash numbers
;;; may never be used.

(set! object-hash
(named-lambda (object-hash object)
  (or (with-interrupt-mask interrupt-mask-none
       (lambda (old-mask)
	 (let* ((hash-index (1+ (modulo (safe-primitive-datum object)
					hash-table-size)))
		(bucket (vector-ref hash-table hash-index))
		(association (assq object bucket)))
	   (if association
	       (cdr association)
	       (let ((result (get-next-hash-number)))
		 (let ((pair (cons object result))
		       (unhash-bucket
			(vector-ref unhash-table
				    (modulo result hash-table-size))))
		   (let ((the-cdr (cdr unhash-bucket)))
		     (cond ((not (set-cdr-if-eq?!
				  unhash-bucket
				  (cons (primitive-set-type weak-cons-type pair)
					the-cdr)
				  the-cdr))
			    ;; Back out and start again.  This number
			    ;; will never be used.  Oh well.
			    false)
			   ((not (vector-set-if-eq?! hash-table hash-index
						     (cons pair bucket)
						     bucket))
			    ;; Back out and start again.  This number
			    ;; will never be used.  The pair added to
			    ;; the unhash table needs to have its car
			    ;; cleared in order to be spliced out by
			    ;; the rehash daemon.
			    ;; Note that if someone has asked for the
			    ;; unhash of this number in the interim,
			    ;; they will have obtained the object, but
			    ;; if they try again it will not work.
			    ;; This should not be a problem since you
			    ;; are only supposed to use unhash on
			    ;; values returned by object-hash, and
			    ;; this value will never be returned.
			    (set-car! pair '())
			    false)
			   (else
			    result)))))))))
      ;; Try again
      (object-hash object))))

;;;; Rehash daemon

;;; The following is dangerous because of the (unnecessary) consing
;;; done by the interpreter while it executes the loops.  It runs with
;;; interrupts turned off.  The (necessary) consing done by rehash is
;;; not dangerous because at least that much storage was freed by the
;;; garbage collector.  To understand this, notice that the hash table
;;; has a SNMV header, so the garbage collector does not trace the
;;; hash table buckets, therefore freeing their storage.  The header
;;; is SNM rather than NM to make the buckets be relocated at band
;;; load/restore time.

;;; Until this code is compiled, and therefore safe (compiled loops
;;; shouldn't cons), it is replaced by a primitive.  See the
;;; installation code below.

#|
(define (rehash weak-pair)
  (let ((index (1+ (modulo (safe-primitive-datum (system-pair-car weak-pair))
			   hash-table-size))))
    (vector-set! hash-table
		 index
		 (cons (primitive-set-type pair-type weak-pair)
		       (vector-ref hash-table index)))))

(define (cleanup n)
  (if (zero? n)
      'DONE
      (begin (vector-set! hash-table n '())
	     (cleanup (-1+ n)))))

(define (rehash-gc-daemon)
  (cleanup hash-table-size)
  (let outer ((n (-1+ hash-table-size)))
    (if (negative? n)
	true
	(let ((bucket (vector-ref unhash-table n)))
	  (if (or (eq? (car bucket) true)
		  (eq? (car bucket) 0))	; zero? would not work!
	      (let inner1 ((l1 bucket) (l2 (cdr bucket)))
		(cond ((null? l2) (outer (-1+ n)))
		      ((eq? (system-pair-car (car l2)) false)
		       (set-cdr! l1 (cdr l2))
		       (inner1 l1 (cdr l1)))
		      (else (rehash (car l2))
			    (inner1 l2 (cdr l2)))))
	      (let inner2 ((l (cdr bucket)))
		(cond ((null? l) (outer (-1+ n)))
		      ((eq? (system-pair-car (car l)) false)
		       (inner2 (cdr l)))
		      (else (rehash (car l))
			    (inner2 (cdr l))))))))))

(add-gc-daemon! rehash-gc-daemon)
|#

(add-gc-daemon!
 (let ((primitive (make-primitive-procedure 'REHASH)))
   (lambda ()
     (primitive unhash-table hash-table))))

(initialize-object-hash! 313)

)

;;;; Populations

;;; A population is a collection of objects.  This collection has the
;;; property that if one of the objects in the collection is reclaimed
;;; as garbage, then it is no longer an element of the collection.

(define make-population)
(define population?)
(define add-to-population!)
(define remove-from-population!)
(define map-over-population)
(define map-over-population!)
(define for-all-inhabitants?)
(define exists-an-inhabitant?)

(let ((bogus-false '(BOGUS-FALSE))
      (population-tag '(POPULATION))
      (weak-cons-type (microcode-type 'WEAK-CONS)))

(declare (integrate canonicalize uncanonicalize))

(define (canonicalize object)
  (declare (integrate object))
  (if (eq? object false)
      bogus-false
      object))

(define (uncanonicalize object)
  (declare (integrate object))
  (if (eq? object bogus-false)
      false
      object))

(define population-of-populations
  (cons population-tag '()))

(define (gc-population! population)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else (loop l2 (system-pair-cdr l2))))))

(define (gc-all-populations!)
  (gc-population! population-of-populations)
  (map-over-population population-of-populations gc-population!))

(set! make-population
(named-lambda (make-population)
  (let ((population (cons population-tag '())))
    (add-to-population! population-of-populations population)
    population)))

(set! population?
(named-lambda (population? object)
  (and (pair? object)
       (eq? (car object) population-tag))))

(set! add-to-population!
(named-lambda (add-to-population! population object)
  (set-cdr! population
	    (system-pair-cons weak-cons-type
			      (canonicalize object)
			      (cdr population)))))

(set! remove-from-population!
(named-lambda (remove-from-population! population object)
  (let ((object-to-search (canonicalize object)))
    (let loop ((l1 population) (l2 (cdr population)))
      (cond ((null? l2) true)
	    ((eq? object-to-search (system-pair-car l2))
	     (system-pair-set-cdr! l1 (system-pair-cdr l2))
	     true)
	    (else (loop l2 (system-pair-cdr l2))))))))

;;;; Higher level operations

(set! map-over-population
(named-lambda (map-over-population population procedure)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) '())
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else
	   (cons (procedure (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2))))))))

(set! map-over-population!
(named-lambda (map-over-population! population procedure)
  (let loop ((l1 population) (l2 (cdr population)))
    (cond ((null? l2) true)
	  ((eq? (system-pair-car l2) false)
	   (system-pair-set-cdr! l1 (system-pair-cdr l2))
	   (loop l1 (system-pair-cdr l1)))
	  (else
	   (procedure (uncanonicalize (system-pair-car l2)))
	   (loop l2 (system-pair-cdr l2)))))))

(set! for-all-inhabitants?
(named-lambda (for-all-inhabitants? population predicate)
  (let loop ((l1 population) (l2 (cdr population)))
    (or (null? l2)
	(if (eq? (system-pair-car l2) false)
	    (begin (system-pair-set-cdr! l1 (system-pair-cdr l2))
		   (loop l1 (system-pair-cdr l1)))
	    (and (predicate (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2))))))))

(set! exists-an-inhabitant?
(named-lambda (exists-an-inhabitant? population predicate)
  (let loop ((l1 population) (l2 (cdr population)))
    (and (not (null? l2))
	 (if (eq? (system-pair-car l2) false)
	     (begin (system-pair-set-cdr! l1 (system-pair-cdr l2))
		    (loop l1 (system-pair-cdr l1)))
	     (or (predicate (uncanonicalize (system-pair-car l2)))
		 (loop l2 (system-pair-cdr l2))))))))

(add-secondary-gc-daemon! gc-all-populations!)

)

;;;; Properties

(define 2D-put!)
(define 2D-get)
(define 2D-remove!)
(define 2D-get-alist-x)
(define 2D-get-alist-y)

(let ((system-properties '()))

(set! 2D-put!
(named-lambda (2D-put! x y value)
  (let ((x-hash (object-hash x))
	(y-hash (object-hash y)))
    (let ((bucket (assq x-hash system-properties)))
      (if bucket
	  (let ((entry (assq y-hash (cdr bucket))))
	    (if entry
		(set-cdr! entry value)
		(set-cdr! bucket
			  (cons (cons y-hash value)
				(cdr bucket)))))
	  (set! system-properties
		(cons (cons x-hash
			    (cons (cons y-hash value)
				  '()))
		      system-properties)))))))

(set! 2D-get
(named-lambda (2D-get x y)
  (let ((bucket (assq (object-hash x) system-properties)))
    (and bucket
	 (let ((entry (assq (object-hash y) (cdr bucket))))
	   (and entry
		(cdr entry)))))))

;;; Returns TRUE iff an entry was removed.
;;; Removes the bucket if the entry removed was the only entry.

(set! 2D-remove!
(named-lambda (2D-remove! x y)
  (let ((bucket (assq (object-hash x) system-properties)))
    (and bucket
	 (begin (set-cdr! bucket
			  (del-assq! (object-hash y)
				     (cdr bucket)))
		(if (null? (cdr bucket))
		    (set! system-properties
			  (del-assq! (object-hash x)
				     system-properties)))
		true)))))

;;; This clever piece of code removes all invalid entries and buckets,
;;; and also removes any buckets which [subsequently] have no entries.

(define (gc-system-properties!)
  (set! system-properties (delete-invalid-hash-numbers! system-properties)))

(define delete-invalid-hash-numbers!
  (list-deletor!
   (lambda (bucket)
     (or (not (valid-hash-number? (car bucket)))
	 (begin (set-cdr! bucket (delete-invalid-y! (cdr bucket)))
		(null? (cdr bucket)))))))

(define delete-invalid-y!
  (list-deletor!
   (lambda (entry)
     (not (valid-hash-number? (car entry))))))

(add-secondary-gc-daemon! gc-system-properties!)

(set! 2D-get-alist-x
(named-lambda (2D-get-alist-x x)
  (let ((bucket (assq (object-hash x) system-properties)))
    (if bucket
	(let loop ((rest (cdr bucket)))
	  (cond ((null? rest) '())
		((valid-hash-number? (caar rest))
		 (cons (cons (object-unhash (caar rest))
			     (cdar rest))
		       (loop (cdr rest))))
		(else (loop (cdr rest)))))
	'()))))

(set! 2D-get-alist-y
(named-lambda (2D-get-alist-y y)
  (let ((y-hash (object-hash y)))
    (let loop ((rest system-properties))
      (cond ((null? rest) '())
	    ((valid-hash-number? (caar rest))
	     (let ((entry (assq y-hash (cdar rest))))
	       (if entry
		   (cons (cons (object-unhash (caar rest))
			       (cdr entry))
			 (loop (cdr rest)))
		   (loop (cdr rest)))))
	    (else (loop (cdr rest))))))))

)
