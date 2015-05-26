#| -*-Scheme-*-

$Header: infutl.scm,v 1.3 88/08/31 10:34:55 jinx Exp $
$MIT-Header: infutl.scm,v 4.2 87/12/31 05:50:38 GMT cph Exp $

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

;;;; Debugging Information Generator, Data Structures

(declare (usual-integrations))

(define compiler-info-tag
  (make-named-tag "COMPILER-INFO"))

(define-structure (compiler-info (named compiler-info-tag))
  (procedures false read-only true)
  (continuations false read-only true)
  (labels false read-only true))

(define-structure (label-info (type vector))
  (name false read-only true)
  (offset false read-only true)
  (external? false read-only true))

(define (compiled-code-block/read-symbol-table block)
  (compiler-info/symbol-table (compiled-code-block/compiler-info block)))

(define (compiler-info/symbol-table compiler-info)
  (and compiler-info
       (make-sorted-vector (compiler-info-labels compiler-info)
			   (lambda (offset label-info)
			     (= offset (label-info-offset label-info)))
			   (lambda (offset label-info)
			     (< offset (label-info-offset label-info))))))

(define (compiled-code-block/compiler-info block)
  (let ((compiler-info (compiled-code-block/debugging-info block)))
    (let ((result (->compiler-info compiler-info)))
      (if (and result
	       (not (eq? compiler-info result)))
	  (set-compiled-code-block/debugging-info! block result))
      result)))

(define (->compiler-info compiler-info)
  (cond ((or (not compiler-info)
	     (compiler-info? compiler-info))
	 compiler-info)
	((string? compiler-info)
	 (compiler-info/read-file compiler-info))
	(else
	 false)))

;;; This returns a LIST of compiler info objects.
;;; The procs above do not expaect such a list, 
;;;  but nothing uses them right now, and much about debugging
;;;  is changing at MIT. [las 5/19/88]

(define (compiler-info/read-file pathname)
  (and (file-exists? pathname)
       (fasload-multiple pathname)))


;;;; Binary Search

(define-structure (sorted-vector
		   (conc-name sorted-vector/)
		   (constructor %make-sorted-vector))
  (vector false read-only true)
  (key=? false read-only true)
  (key-compare false read-only true))

(define (make-sorted-vector vector key=? key<?)
  (%make-sorted-vector vector
			 key=?
			 (lambda (key entry if= if< if>)
			   ((cond ((key=? key entry) if=)
				  ((key<? key entry) if<)
				  (else if>))))))

(define (sorted-vector/find-element sorted-vector key)
  (let ((vector (sorted-vector/vector sorted-vector)))
    (vector-binary-search vector
			  key
			  (sorted-vector/key-compare sorted-vector)
			  (lambda (index) (vector-ref vector index))
			  (lambda () false))))

(define (sorted-vector/find-indices sorted-vector key if-found if-not-found)
  (vector-binary-search-range (sorted-vector/vector sorted-vector)
			      key
			      (sorted-vector/key=? sorted-vector)
			      (sorted-vector/key-compare sorted-vector)
			      if-found
			      if-not-found))

(define (sorted-vector/there-exists? sorted-vector key predicate)
  (sorted-vector/find-indices sorted-vector key
    (lambda (low high)
      (let ((vector (sorted-vector/vector sorted-vector)))
	(let loop ((index low))
	  (if (predicate (vector-ref vector index))
	      true
	      (and (< index high)
		   (loop (1+ index)))))))
    (lambda () false)))

(define (sorted-vector/for-each sorted-vector key procedure)
  (sorted-vector/find-indices sorted-vector key
    (lambda (low high)
      (let ((vector (sorted-vector/vector sorted-vector)))
	(let loop ((index low))
	  (procedure (vector-ref vector index))
	  (if (< index high)
	      (loop (1+ index))))))
    (lambda () *the-non-printing-object*)))

(define (vector-binary-search-range vector key key=? compare if-found
				    if-not-found)
  (vector-binary-search vector key compare
    (lambda (index)
      (if-found (let loop ((index index))
		  (if (zero? index)
		      index
		      (let ((index* (-1+ index)))
			(if (key=? key (vector-ref vector index*))
			    (loop index*)
			    index))))
		(let ((end (-1+ (vector-length vector))))
		  (let loop ((index index))
		    (if (= index end)
			index
			(let ((index* (1+ index)))
			  (if (key=? key (vector-ref vector index*))
			      (loop index*)
			      index)))))))
    if-not-found))

(define (vector-binary-search vector key compare if-found if-not-found)
  (let loop ((low 0) (high (-1+ (vector-length vector))))
    (if (< high low)
	(if-not-found)
	(let ((index (quotient (+ high low) 2)))
	  (compare key
		   (vector-ref vector index)
		   (lambda () (if-found index))
		   (lambda () (loop low (-1+ index)))
		   (lambda () (loop (1+ index) high)))))))
