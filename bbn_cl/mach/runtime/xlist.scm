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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Extended List Operations

(declare (usual-integrations))

(define (circular-list . elements)
  (if (not (null? elements))
      (let loop ((l elements))
	(if (null? (cdr l))
	    (set-cdr! l elements)
	    (loop (cdr l)))))
  elements)

(define mapcan)

(let ((mapper-generator
       (named-lambda ((mapper-generator accumulator initial-value name)
		      f . lists)
	 (if (null? lists)
	     (error "No arguments to mapping function" name f)
	     (let loop ((lists lists))
	       (let scan ((lists lists)
			  (c (lambda (cars cdrs)
			       (accumulator (apply f cars)
					    (loop cdrs)))))
		 (cond ((null? lists) (c '() '()))
		       ((null? (car lists)) initial-value)
		       (else
			(scan (cdr lists)
			      (lambda (cars cdrs)
				(c (cons (car (car lists)) cars)
				   (cons (cdr (car lists)) cdrs))))))))))))
  (set! mapcan (mapper-generator append! '() 'MAPCAN)))

(define mapcan*)

(let ((mapper-generator*
       (named-lambda ((mapper-generator* accumulator name)
		      initial-value f . lists)
	 (if (null? lists)
	     (error "No arguments to mapping function" name f)
	     (let loop ((lists lists))
	       (let scan ((lists lists)
			  (c (lambda (cars cdrs)
			       (accumulator (apply f cars)
					    (loop cdrs)))))
		 (cond ((null? lists) (c '() '()))
		       ((null? (car lists)) initial-value)
		       (else
			(scan (cdr lists)
			      (lambda (cars cdrs)
				(c (cons (car (car lists)) cars)
				   (cons (cdr (car lists)) cdrs))))))))))))
  (set! mapcan* (mapper-generator* append! 'MAPCAN*)))

(define (initial-segment l n)
  (cond ((zero? n) '())
	((not (pair? l)) (error "initial-segment: list not long enough" l n))
	(else (cons (car l)
		    (initial-segment (cdr l) (-1+ n))))))

(define (final-segment l n)
  (let ((length (length l)))
    (if (< length n)
	(error "List has too few elements" 'FINAL-SEGMENT l n))
    (list-tail l (- length n))))
