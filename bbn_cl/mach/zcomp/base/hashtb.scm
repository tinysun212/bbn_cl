#| -*-Scheme-*-

$Header: hashtb.scm,v 1.2 88/08/31 10:34:45 jinx Exp $
$MIT-Header: hashtb.scm,v 4.1 87/12/30 06:55:30 GMT cph Exp $

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

;;;; Hash Tables

(declare (usual-integrations))

(define-structure (hash-table
		   (conc-name hash-table/)
		   (constructor %make/hash-table))
  (buckets false read-only true)
  (hasher false read-only true)
  (comparator false read-only true)
  (constructor false read-only true))

(define (make/hash-table n-buckets hasher comparator constructor)
  (%make/hash-table (make-vector n-buckets '()) hasher comparator constructor))

(define (hash-table/intern! table key if-found if-not-found)
  (let ((buckets (hash-table/buckets table)))
    (let ((hash ((hash-table/hasher table) key (vector-length buckets))))
      (let loop ((entries (vector-ref buckets hash)))
	(cond ((null? entries)
	       (let ((entry ((hash-table/constructor table) key)))
		 (vector-set! buckets
			      hash
			      (cons entry (vector-ref buckets hash)))
		 (if-not-found entry)))
	      (((hash-table/comparator table) key (car entries))
	       (if-found (car entries)))
	      (else
	       (loop (cdr entries))))))))

(define (hash-table/lookup table key if-found if-not-found)
  (let loop
      ((entries
	(let ((buckets (hash-table/buckets table)))
	  (vector-ref buckets
		      ((hash-table/hasher table)
		       key
		       (vector-length buckets))))))
    (cond ((null? entries)
	   (if-not-found))
	  (((hash-table/comparator table) key (car entries))
	   (if-found (car entries)))
	  (else
	   (loop (cdr entries))))))

(define (hash-table/entries table)
  (apply append (vector->list (hash-table/buckets table))))

;;;; Symbol Hash Tables

(define (symbol-hash-table/make n-buckets)
  (make/hash-table n-buckets
		   (lambda (symbol modulus)
		     (string-hash-mod (symbol->string symbol) modulus))
		   (lambda (symbol entry) (eq? symbol (car entry)))
		   (lambda (symbol) (cons symbol false))))

(define (symbol-hash-table/modify! table symbol if-found if-not-found)
  (hash-table/intern! table
		      symbol
		      (lambda (entry) (set-cdr! entry (if-found (cdr entry))))
		      (lambda (entry) (set-cdr! entry (if-not-found)))))

(define (symbol-hash-table/lookup* table symbol if-found if-not-found)
  (hash-table/lookup table
		     symbol
		     (lambda (entry) (if-found (cdr entry)))
		     if-not-found))

(define (symbol-hash-table/insert! table symbol item)
  (symbol-hash-table/modify! table symbol
			     (lambda (old-value) item)
			     (lambda () item)))

(define (symbol-hash-table/lookup table symbol)
  (symbol-hash-table/lookup* table symbol
			     identity-procedure
			     (lambda () (error "Missing item" symbol))))

(define symbol-hash-table/bindings
  hash-table/entries)

(define (symbol-hash-table/positive-bindings table predicate)
  (list-transform-positive (symbol-hash-table/bindings table)
    (lambda (entry)
      (predicate (cdr entry)))))

(define (symbol-hash-table/negative-bindings table predicate)
  (list-transform-negative (symbol-hash-table/bindings table)
    (lambda (entry)
      (predicate (cdr entry)))))
