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
;;;;	Reasonable test case for Scheme

(define test-case '(implies (and (and (implies (f x) (g x)) (implies (g x) (h x)))
			 (implies (h x) (i x)))
		    (implies (f x) (i x))))

;	Interesting theorems or rather expansions.

(add-lemma '(equal (compile form)
		   (reverse (codegen (optimize form)
				     (nil)))))
(add-lemma '(equal (eqp x y)
		   (equal (fix x)
			  (fix y))))
(add-lemma '(equal (greaterp x y)
		   (lessp y x)))
(add-lemma '(equal (lesseqp x y)
		   (not (lessp y x))))
(add-lemma '(equal (greatereqp x y)
		   (not (lessp x y))))
(add-lemma '(equal (boolean x)
		   (or (equal x (t))
		       (equal x (f)))))
(add-lemma '(equal (iff x y)
		   (and (implies x y)
			(implies y x))))
(add-lemma '(equal (even1 x)
		   (if (zerop x)
		       (t)
		       (odd (1- x)))))
(add-lemma '(equal (countps- l pred)
		   (countps-loop l pred (zero))))
(add-lemma '(equal (fact- i)
		   (fact-loop i 1)))
(add-lemma '(equal (reverse- x)
		   (reverse-loop x (nil))))
(add-lemma '(equal (divides x y)
		   (zerop (remainder y x))))
(add-lemma '(equal (assume-true var alist)
		   (cons (cons var (t))
			 alist)))
(add-lemma '(equal (assume-false var alist)
		   (cons (cons var (f))
			 alist)))
(add-lemma '(equal (tautology-checker x)
		   (tautologyp (normalize x)
			       (nil))))
(add-lemma '(equal (falsify x)
		   (falsify1 (normalize x)
			     (nil))))
(add-lemma '(equal (prime x)
		   (and (not (zerop x))
			(not (equal x (add1 (zero))))
			(prime1 x (1- x)))))
(add-lemma '(equal (and p q)
		   (if p (if q (t)
			     (f))
		       (f))))
(add-lemma '(equal (or p q)
		   (if p (t)
		       (if q (t)
			   (f))
		       (f))))
(add-lemma '(equal (not p)
		   (if p (f)
		       (t))))
(add-lemma '(equal (implies p q)
		   (if p (if q (t)
			     (f))
		       (t))))
(add-lemma '(equal (fix x)
		   (if (numberp x)
		       x
		       (zero))))
(add-lemma '(equal (if (if a b c)
		       d e)
		   (if a (if b d e)
		       (if c d e))))
(add-lemma '(equal (zerop x)
		   (or (equal x (zero))
		       (not (numberp x)))))
(add-lemma '(equal (plus (plus x y)
			 z)
		   (plus x (plus y z))))
(add-lemma '(equal (equal (plus a b)
			  (zero))
		   (and (zerop a)
			(zerop b))))
(add-lemma '(equal (difference x x)
		   (zero)))
(add-lemma '(equal (equal (plus a b)
			  (plus a c))
		   (equal (fix b)
			  (fix c))))
(add-lemma '(equal (equal (zero)
			  (difference x y))
		   (not (lessp y x))))
(add-lemma '(equal (equal x (difference x y))
		   (and (numberp x)
			(or (equal x (zero))
			    (zerop y)))))
(add-lemma '(equal (meaning (plus-tree (append x y))
			    a)
		   (plus (meaning (plus-tree x)
				  a)
			 (meaning (plus-tree y)
				  a))))
(add-lemma '(equal (meaning (plus-tree (plus-fringe x))
			    a)
		   (fix (meaning x a))))
(add-lemma '(equal (append (append x y)
			   z)
		   (append x (append y z))))
(add-lemma '(equal (reverse (append a b))
		   (append (reverse b)
			   (reverse a))))
(add-lemma '(equal (times x (plus y z))
		   (plus (times x y)
			 (times x z))))
(add-lemma '(equal (times (times x y)
			  z)
		   (times x (times y z))))
(add-lemma '(equal (equal (times x y)
			  (zero))
		   (or (zerop x)
		       (zerop y))))
(add-lemma '(equal (exec (append x y)
			 pds envrn)
		   (exec y (exec x pds envrn)
			 envrn)))
(add-lemma '(equal (mc-flatten x y)
		   (append (flatten x)
			   y)))
(add-lemma '(equal (member x (append a b))
		   (or (member x a)
		       (member x b))))
(add-lemma '(equal (member x (reverse y))
		   (member x y)))
(add-lemma '(equal (length (reverse x))
		   (length x)))
(add-lemma '(equal (member a (intersect b c))
		   (and (member a b)
			(member a c))))
(add-lemma '(equal (nth (zero)
			i)
		   (zero)))
(add-lemma '(equal (exp i (plus j k))
		   (times (exp i j)
			  (exp i k))))
(add-lemma '(equal (exp i (times j k))
		   (exp (exp i j)
			k)))
(add-lemma '(equal (reverse-loop x y)
		   (append (reverse x)
			   y)))
(add-lemma '(equal (reverse-loop x (nil))
		   (reverse x)))
(add-lemma '(equal (count-list z (sort-lp x y))
		   (plus (count-list z x)
			 (count-list z y))))
(add-lemma '(equal (equal (append a b)
			  (append a c))
		   (equal b c)))
(add-lemma '(equal (plus (remainder x y)
			 (times y (quotient x y)))
		   (fix x)))
(add-lemma '(equal (power-eval (big-plus1 l i base)
			       base)
		   (plus (power-eval l base)
			 i)))
(add-lemma '(equal (power-eval (big-plus x y i base)
			       base)
		   (plus i (plus (power-eval x base)
				 (power-eval y base)))))
(add-lemma '(equal (remainder y 1)
		   (zero)))
(add-lemma '(equal (lessp (remainder x y)
			  y)
		   (not (zerop y))))
(add-lemma '(equal (remainder x x)
		   (zero)))
(add-lemma '(equal (lessp (quotient i j)
			  i)
		   (and (not (zerop i))
			(or (zerop j)
			    (not (equal j 1))))))
(add-lemma '(equal (lessp (remainder x y)
			  x)
		   (and (not (zerop y))
			(not (zerop x))
			(not (lessp x y)))))
(add-lemma '(equal (power-eval (power-rep i base)
			       base)
		   (fix i)))
(add-lemma '(equal (power-eval (big-plus (power-rep i base)
					 (power-rep j base)
					 (zero)
					 base)
			       base)
		   (plus i j)))
(add-lemma '(equal (gcd x y)
		   (gcd y x)))
(add-lemma '(equal (nth (append a b)
			i)
		   (append (nth a i)
			   (nth b (difference i (length a))))))
(add-lemma '(equal (difference (plus x y)
			       x)
		   (fix y)))
(add-lemma '(equal (difference (plus y x)
			       x)
		   (fix y)))
(add-lemma '(equal (difference (plus x y)
			       (plus x z))
		   (difference y z)))
(add-lemma '(equal (times x (difference c w))
		   (difference (times c x)
			       (times w x))))
(add-lemma '(equal (remainder (times x z)
			      z)
		   (zero)))
(add-lemma '(equal (difference (plus b (plus a c))
			       a)
		   (plus b c)))
(add-lemma '(equal (difference (add1 (plus y z))
			       z)
		   (add1 y)))
(add-lemma '(equal (lessp (plus x y)
			  (plus x z))
		   (lessp y z)))
(add-lemma '(equal (lessp (times x z)
			  (times y z))
		   (and (not (zerop z))
			(lessp x y))))
(add-lemma '(equal (lessp y (plus x y))
		   (not (zerop x))))
(add-lemma '(equal (gcd (times x z)
			(times y z))
		   (times z (gcd x y))))
(add-lemma '(equal (value (normalize x)
			  a)
		   (value x a)))
(add-lemma '(equal (equal (flatten x)
			  (cons y (nil)))
		   (and (nlistp x)
			(equal x y))))
(add-lemma '(equal (listp (gopher x))
		   (listp x)))
(add-lemma '(equal (samefringe x y)
		   (equal (flatten x)
			  (flatten y))))
(add-lemma '(equal (equal (greatest-factor x y)
			  (zero))
		   (and (or (zerop y)
			    (equal y 1))
			(equal x (zero)))))
(add-lemma '(equal (equal (greatest-factor x y)
			  1)
		   (equal x 1)))
(add-lemma '(equal (numberp (greatest-factor x y))
		   (not (and (or (zerop y)
				 (equal y 1))
			     (not (numberp x))))))
(add-lemma '(equal (times-list (append x y))
		   (times (times-list x)
			  (times-list y))))
(add-lemma '(equal (prime-list (append x y))
		   (and (prime-list x)
			(prime-list y))))
(add-lemma '(equal (equal z (times w z))
		   (and (numberp z)
			(or (equal z (zero))
			    (equal w 1)))))
(add-lemma '(equal (greatereqpr x y)
		   (not (lessp x y))))
(add-lemma '(equal (equal x (times x y))
		   (or (equal x (zero))
		       (and (numberp x)
			    (equal y 1)))))
(add-lemma '(equal (remainder (times y x)
			      y)
		   (zero)))
(add-lemma '(equal (equal (times a b)
			  1)
		   (and (not (equal a (zero)))
			(not (equal b (zero)))
			(numberp a)
			(numberp b)
			(equal (1- a)
			       (zero))
			(equal (1- b)
			       (zero)))))
(add-lemma '(equal (lessp (length (delete x l))
			  (length l))
		   (member x l)))
(add-lemma '(equal (sort2 (delete x l))
		   (delete x (sort2 l))))
(add-lemma '(equal (dsort x)
		   (sort2 x)))
(add-lemma '(equal (length (cons x1
				 (cons x2
				       (cons x3 (cons x4
						      (cons x5
							    (cons x6 x7)))))))
		   (plus 6 (length x7))))
(add-lemma '(equal (difference (add1 (add1 x))
			       2)
		   (fix x)))
(add-lemma '(equal (quotient (plus x (plus x y))
			     2)
		   (plus x (quotient y 2))))
(add-lemma '(equal (sigma (zero)
			  i)
		   (quotient (times i (add1 i))
			     2)))
(add-lemma '(equal (plus x (add1 y))
		   (if (numberp y)
		       (add1 (plus x y))
		       (add1 x))))
(add-lemma '(equal (equal (difference x y)
			  (difference z y))
		   (if (lessp x y)
		       (not (lessp y z))
		       (if (lessp z y)
			   (not (lessp y x))
			   (equal (fix x)
				  (fix z))))))
(add-lemma '(equal (meaning (plus-tree (delete x y))
			    a)
		   (if (member x y)
		       (difference (meaning (plus-tree y)
					    a)
				   (meaning x a))
		       (meaning (plus-tree y)
				a))))
(add-lemma '(equal (times x (add1 y))
		   (if (numberp y)
		       (plus x (times x y))
		       (fix x))))
(add-lemma '(equal (nth (nil)
			i)
		   (if (zerop i)
		       (nil)
		       (zero))))
(add-lemma '(equal (last (append a b))
		   (if (listp b)
		       (last b)
		       (if (listp a)
			   (cons (car (last a))
				 b)
			   b))))
(add-lemma '(equal (equal (lessp x y)
			  z)
		   (if (lessp x y)
		       (equal t z)
		       (equal f z))))
(add-lemma '(equal (assignment x (append a b))
		   (if (assignedp x a)
		       (assignment x a)
		       (assignment x b))))
(add-lemma '(equal (car (gopher x))
		   (if (listp x)
		       (car (flatten x))
		       (zero))))
(add-lemma '(equal (flatten (cdr (gopher x)))
		   (if (listp x)
		       (cdr (flatten x))
		       (cons (zero)
			     (nil)))))
(add-lemma '(equal (quotient (times y x)
			     y)
		   (if (zerop y)
		       (zero)
		       (fix x))))
(add-lemma '(equal (get j (set i val mem))
		   (if (eqp j i)
		       val
		       (get j mem))))
