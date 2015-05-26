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

;;;; Scheme Parser

(declare (usual-integrations))

(define *parser-radix* #d10)
(define *parser-table*)

(define parser-package
  (make-environment
    ))

;;;; Parser Tables

(define (parser-table-copy table)
  (cons (cons (vector-copy (caar table))
	      (vector-copy (cdar table)))
	(cons (vector-copy (cadr table))
	      (vector-copy (cddr table)))))

(define parser-table-entry)
(define set-parser-table-entry!)
(let ()

(define (decode-parser-char table char receiver)
  (cond ((char? char)
	 (receiver (car table) (char->ascii char)))
	((string? char)
	 (cond ((= (string-length char) 1)
		(receiver (car table) (char->ascii (string-ref char 0))))
	       ((and (= (string-length char) 2)
		     (char=? #\# (string-ref char 0)))
		(receiver (cdr table) (char->ascii (string-ref char 1))))
	       (else
		(error "Bad character" 'DECODE-PARSER-CHAR char))))
	(else
	 (error "Bad character" 'DECODE-PARSER-CHAR char))))

(define (ptable-ref table index)
  (cons (vector-ref (car table) index)
	(vector-ref (cdr table) index)))

(define (ptable-set! table index value)
  (vector-set! (car table) index (car value))
  (vector-set! (cdr table) index (cdr value)))

(set! parser-table-entry
(named-lambda (parser-table-entry table char)
  (decode-parser-char table char ptable-ref)))

(set! set-parser-table-entry!
(named-lambda (set-parser-table-entry! table char entry)
  (decode-parser-char table char
    (lambda (sub-table index)
      (ptable-set! sub-table index entry)))))

)

