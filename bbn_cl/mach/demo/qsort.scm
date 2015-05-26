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
;;;(declare (usual-integrations))

(define qsort-data '(567 234 789 345 123 981 213 415 617 718 212 202 203
	       800 914 911 611 516 303 513 801 802))

(define (make-best-case n)
  (make-best-case-util n (/ n 2)))

(define (make-best-case-util n m)
  (if (< m 1)
      ()
      (cons n (intersperse (make-best-case-util (- n m) (/ m 2))
			   (make-best-case-util  (+ n m) (/ m 2))))))

(define (make-worst-case n)
  (if (< n 2)
      ()
      (append! (make-worst-case (-1+ n))
	       (cons (-1+ n) ()))))

(define (intersperse a b)
  (cond ((null? a) b)
	((null? b) a)
	(else (cons (car a)
		    (cons (car b)
			  (intersperse (cdr a) (cdr b)))))))

(define best-case (make-best-case 16))
(define worst-case (make-worst-case 16))

;;; Merge sort, RHH, June 1986 (based on an idea by Dan Nussbaum).

(define null null?)
(define (atom x) (not (pair? x)))

;;; Quicksort programs in Multilisp, RHH, April 1984.

(define bundle-parts cons)
(define left-part car)
(define right-part cdr)

;;; Parallel quicksort:

(define (make-parallel-qsort elt<)

  (define (qsort l) (qs l ()))

  (define (qs l rest )
	  (let ((parts))
    (if (null l)
	rest
	(sequence
	(set! parts (part (car l) (cdr l)))
	(qs (left-part parts)
	    (future (cons (car l) (qs (right-part parts) rest))))))))

  (define (part elt l )
	  (let ((cdrparts))
    (if (null l)
	(bundle-parts () ())
	(sequence (set! cdrparts (future (part elt (cdr l))))
	       (if (elt< (car l) elt)
		   (bundle-parts (cons (car l) (future (left-part cdrparts)))
				 (future (right-part cdrparts)))
		   (bundle-parts (future (left-part cdrparts))
				 (cons (car l) (future (right-part cdrparts)))))))))

  qsort)

(define qsort (make-parallel-qsort <))

;;; Binary tree maintenance experiment, RHH, October 1985.

;;; Binary tree data abstraction:

(define (make-node l d r) (cons d (cons l r)))
(define left-child cadr)
(define right-child cddr)
(define discriminant car)
(define leaf? atom)
(define empty-tree? null?)

(define (make-empty-tree) ())

;;; Make an inserter that uses the given comparison function:

(define (make-parallel-inserter elt<)
  (define (insert elt tree)
    (if (empty-tree? tree)
	elt
	(if (leaf? tree)
	    (if (elt< tree elt)
		(make-node tree tree elt)
		(make-node elt elt tree))
	    (if (elt< (discriminant tree) elt)
		(make-node (left-child tree)
			   (discriminant tree)
			   (future (insert elt (right-child tree))))
		(make-node (future (insert elt (left-child tree)))
			   (discriminant tree)
			   (right-child tree))))))
  insert)

;;; Do a parallel tree walk, producing a list of leaves:

(define (parallel-walk-tree tree)
  (define (internal-walk tree rest)
    (cond ((empty-tree? tree) rest)
	  ((leaf? tree) (cons tree rest))
	  ((internal-walk (left-child tree)
			  (future (internal-walk (right-child tree) rest))))))
  (internal-walk tree ()))

;;; Produce a sort, using given comparison and tree walker:

(define (make-parallel-insertion-sort the-inserter walker)
  (define (the-sorter l)
    (define (sort-iter l tree)
      (if (null l)
	  tree
	  (sort-iter (cdr l)
		     (future (the-inserter (car l) tree)))))
    (walker (sort-iter l (make-empty-tree))))
  the-sorter)

(define (make-parallel-tree-sort elt<)
  (make-parallel-insertion-sort (make-parallel-inserter elt<) parallel-walk-tree))

(define hsort (make-parallel-tree-sort <))
