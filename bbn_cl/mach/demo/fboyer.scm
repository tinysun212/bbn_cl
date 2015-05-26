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
;;;;	Boyer-Moore Theorem prover - works by moby expansion

(declare (usual-integrations))

; Sample cases.

(define modus-ponens '(implies (and (implies a b) a) b))
(define transitivity '(implies (and (implies (f x) (g x))
				    (implies (g x) (h x)))
			       (implies (f x) (h x))))
(define three-deep '(implies (and (implies (f x) (g x))
				  (and (implies (g x) (h x))
				       (implies (h x) (i x))))
			     (implies (f x) (i x))))

; Tautology detection checks forms (if predicate consequent alternate)
;	If the predicate is known true then we just check the consequent
;	If the predicate is known false then we just check the alternate
;	Otherwise we see if the consequent is true assuming the predicate is true
;	and that the alternate is true assuming the predicate is false

(define (taut? form)
  (tautology? (rewrite form) () ()))

(define (tautology? form true-list false-list)
  (cond ((known-true? form true-list) #!true)
	((known-false? form false-list) #!false)
	((eq? (car form) 'if)
	 (cond ((known-true? (cadr form) true-list)
		(tautology? (caddr form) true-list false-list))
	       ((known-false? (cadr form) false-list)
		(tautology? (cadddr form) true-list false-list))
	       (else
		(and
		  (future (tautology? (caddr form)
				      (cons (cadr form) true-list)
				      false-list))
		  (tautology? (cadddr form) true-list (cons (cadr form)
				      false-list))))))
	(else #!false)))

(define (known-true? form true-list)
  (if (equal? form '(t))
      #!true
      (member form true-list)))

(define (known-false? form false-list)
  (if (equal? form '(f))
      #!true
      (member form false-list)))


; Rewriting matches a form against the list of lemmas associated with the car
;	of the form and first rewrites the remainder of the form before
;	finding the first lemma which matches and expanding it accordingly.

(define (rewrite form)
  (if (atom? form)
      form
      (future
       (rewrite-with-lemmas (cons (car form) (rewrite-args (cdr form)))
			    (find-lemmas (car form))))))

(define (rewrite-args args)
  (if (null? args)
      ()
      (cons (rewrite (car args)) (rewrite-args (cdr args)))))

(define (rewrite-with-lemmas form lemmas)
  (if (null? lemmas)
      form
      (let ((subst-list (one-way-unify-util form (cadr (car lemmas)) ())))
	(if (not (eq? subst-list 'failed))
	    (rewrite (apply-subst subst-list (caddr (car lemmas))))
	    (rewrite-with-lemmas form (cdr lemmas))))))

; Weak unificiation works by a recursive pattern match.

(define (one-way-unify-util form pattern frame)
  (cond ((eq? frame 'failed) 'failed)
	((atom? pattern)
	 (let ((already-matched (assq pattern frame)))
	   (cond (already-matched		; if matched verify rematch
		  (if (equal? form (cdr already-matched)) frame 'failed))
		 (else
		  (cons (cons pattern form) frame)))))
	((atom? form) 'failed)
	((eq? (car form) (car pattern))
	 (one-way-unify-list (cdr form) (cdr pattern) frame))
	(else 'failed)))

(define (one-way-unify-list form pattern frame)
  (if (null? form)
      frame
      (one-way-unify-list (cdr form) (cdr pattern)
			  (one-way-unify-util (car form) (car pattern) frame))))

; Very simple substituter used by rewrite with the result of the unification.

(define (apply-subst subst-list form)
  (if (atom? form)
      (let ((value (assq form subst-list)))
	(if value (cdr value) form))
      (cons (car form) (apply-subst-list subst-list (cdr form)))))

(define (apply-subst-list subst-list form)
  (if (null? form)
      ()
      (cons (apply-subst subst-list (car form))
	    (apply-subst-list subst-list (cdr form)))))


; Since we don't have putprop we will associate lemmas with their keys by using
;	assq on a lemma-list.  We might make this faster some day.

(define lemma-list ())

(define (add-lemma lemma)
  (cond ((and
	   (not (atom? lemma))
	   (eq? (car lemma) 'equal)
	   (not (atom? (cadr lemma))))
	 (let ((bucket (assq (car (cadr lemma)) lemma-list)))
	   (if bucket
	       (set-cdr! bucket (cons lemma (cdr bucket)))
	       (set! lemma-list (cons (cons (car (cadr lemma)) (cons lemma ()))
				      lemma-list))))
	 'ADDED)
	(else
	 (print `(Bad lemma form ,lemma)))))

(define (find-lemmas key)
  (let ((the-list (assq key lemma-list)))
    (if the-list
	(cdr the-list)
	())))


; Useful functions that Scheme claims to have but doesn't.

(define (atom? what) (not (pair? what)))
