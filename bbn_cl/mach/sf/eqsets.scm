#| -*-Scheme-*-

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

;;;; Sets of elements distinguished by EQ?

(declare (usual-integrations))

(define eq?-set/null '())
(define eq?-set/null? null?)
(define list->eq?-set identity-procedure)
(define eq?-set->list identity-procedure)
(define eq?-set/member? memq)

(define (eq?-set/singleton element)
  (list element))

(define (eq?-set/adjoin x set)
  (if (memq x set)
      set
      (cons x set)))

(define (eq?-set/remove x set)
  (cond ((null? set) '())
	((eq? x (car set)) (cdr set))
	(else (cons (car set) (eq?-set/remove x (cdr set))))))

(define (eq?-set/difference set1 set2)
  (cond ((null? set1) '())
	((memq (car set1) set2) (eq?-set/difference (cdr set1) set2))
	(else (cons (car set1) (eq?-set/difference (cdr set1) set2)))))

(define (eq?-set/union set1 set2)
  (cond ((null? set1) set2)
	((memq (car set1) set2) (eq?-set/union (cdr set1) set2))
	(else (cons (car set1) (eq?-set/union (cdr set1) set2)))))

(define (eq?-set/intersection set1 set2)
  (cond ((null? set1) '())
	((memq (car set1) set2)
	 (cons (car set1) (eq?-set/intersection (cdr set1) set2)))
	(else (eq?-set/intersection (cdr set1) set2))))

(define (eq?-set/disjoint? set1 set2)
  (cond ((null? set1) true)
	((memq (car set1) set2) false)
	(else (eq?-set/disjoint? (cdr set1) set2))))

(define (eq?-set/subset? set1 set2)
  (cond ((null? set1) true)
	((memq (car set1) set2) (eq?-set/subset? (cdr set1) set2))
	(else false)))

(define (eq?-set/same? set1 set2)
  (and (eq?-set/subset? set1 set2)
       (eq?-set/subset? set2 set1)))

(define (eq?-set/intersection* . sets)
  (if (null? sets)
      '()
      (let loop ((sets sets))
	(if (null? (cdr sets))
	    (car sets)
	    (eq?-intersection (car sets) (loop (cdr sets)))))))

(define (eq?-set/union* . sets)
  (if (null? sets)
      '()
      (let loop ((sets sets))
	(if (null? (cdr sets))
	    (car sets)
	    (eq?-union (car sets) (loop (cdr sets)))))))




