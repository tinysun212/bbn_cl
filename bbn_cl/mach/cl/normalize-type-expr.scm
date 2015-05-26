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
;;;
(proclaim '(insert-touches nil))

;;; It is assumed that types passed here will never be futures

(defvar *normalize-type-expr-debug* nil)


;;;
;;; Benchmark test case: (normalize-type-expr '(and (not (and (or future fixnum) future)) (or future fixnum)))
;;;

(defun normalize-type-expr (e)
  (let ((did-something-p nil))
    (labels 
	((norm (e)
	       (cond
		((not (consp e))
		 e)
		((eq (car e) 'not)
		 (let* ((opd (cadr e))
			(opd-type (expr-type (cadr e))))
		   (cond
		    ((eq opd-type 'not)
		     (setq did-something-p t)
		     (cadr opd))
		    ((eq opd-type 'primitive)
		     e)
		    ((memq opd-type '(or and))
		     (setq did-something-p t)
		     (demorgan-negate opd)))))
		((eq (car e) 'or)
		 (multiple-value-bind
		  (e1 e2)
		  (normalize-order (cadr e) (caddr e))
		  `(or ,(norm e1) ,(norm e2))))
		((eq (car e) 'and)
		 (multiple-value-bind
		  (e1 e2)
		  (normalize-order (cadr e) (caddr e))
		  (let ((subexpr-type (classify-subexprs e1 e2)))
		    (cond
		     ((memq subexpr-type '(or-or or-and or-not or-primitive))
		      (setq did-something-p t)
		      (let ((e11 (cadr e1))
			    (e12 (caddr e1)))
			`(or (and ,e11 ,e2)
			     (and ,e12 ,e2))))
		     (t 
		      `(and ,(norm e1) ,(norm e2)))))))
		(t e)))
	 (norm-until-done (e)
			  (let ((e (pairwise e)))
			    (setq did-something-p nil)
			    (loop
			      (when *normalize-type-expr-debug*
				    (pp e)
				    (terpri))
			      (setq e (norm e))
			      (if (not did-something-p)
				  (return e)
				  (progn
				    (setq did-something-p nil)))))))
      (setq e (norm-until-done e))
      (when *normalize-type-expr-debug*
	    (print 'unpairwise))
      (setq e (unpairwise e))
      (when *normalize-type-expr-debug*
	    (print 'simplify))
      (setq e (simplify e))
      (dotimes (i 1)
	(setq e (simplify (unpairwise (norm-until-done e)))))
      e)))

;;;
;;; Assumes e is either an OR or an AND,
;;;  negates it and takes demorgan transform
;;;

(defun demorgan-negate (e)	  
  (let* ((opr (car e))
	 (opr-complement (if (eq opr 'or) 'and 'or))
	(e1 (cadr e))
	(e2 (caddr e)))
    `(,opr-complement (not ,e1) (not ,e2))))

(defun pairwise (e)
  (cond
   ((not (consp e))
    e)
   ((memq (car e) '(and or))
    (cond 
     ((null (cdr e))
      (if (eq (car e) 'or)
	  t
	  nil))
     ((null (cddr e))
      (pairwise (cadr e)))
     (t
      (list (car e)
	    (pairwise (cadr e))
	    (pairwise (cons (car e) (cddr e)))))))
   (t (mapcar #'pairwise e))))

(defun unpairwise (e)
  (cond
   ((not (consp e))
    e)
   ((memq (car e) '(and or))
    (let ((opr (car e)))
      (if (contains-opr-p opr (cdr e))
	  (unpairwise
	   (cons opr
		 (mapcan #'(lambda (x)
			     (if (eq (expr-type x) opr)
				 (cdr x)
				 (list x)))
			 (cdr e))))
	  (cons opr
		(mapcar #'unpairwise (cdr e))))))
   (t e)))

(defun contains-opr-p (opr elist)
  (member opr elist :test #'(lambda (x y) (eq x (expr-type y)))))

;;; Assumes unpairwised and reduced to sum-of-products of primitives

(defun simplify (e)
  (cond
   ((not (consp e))
    e)
   ((eq (car e) 'and)
    (cons 'and (reduce-and (cdr e))))
   ((eq (car e) 'or)
    (let ((l (delete nil (mapcar #'simplify (remove-duplicates (cdr e) :test #'equal)))))
      (and l
	   (if (null (cdr l))
	       (car l)
	       (cons 'or l)))))
   (t e)))

;;; For AND: true if elist contains x and (not x) (equal-based), or contains nil.
;;; Elements are either pairs with car = not or are considered primitive

(defun reduce-and (elist)
  (if (memq nil elist)
      nil
      (let ((l (remove-duplicates elist :test #'equal)))
	(dolist (e1 elist)
	  (dolist (e2 elist)
	    (cond
	     ((eq (expr-type e1) 'not)
	      (if (equal (cadr e1) e2) 
		  (return-from reduce-and nil)))
	     ((disjointp e1 e2)
	      (return-from reduce-and nil))
	     ((and (not (eq e1 e2))
		   (subtypep e1 e2))
	      (setq l (delete e2 l :test #'equal))
	      (return-from reduce-and (reduce-and l))))))
	l)))

(defun normalize-order (e1 e2)
  (if (or (memq (expr-type e2) '(and or))
	  (eq (expr-type e1) 'not))
      (values e2 e1)
      (values e1 e2)))

(defun expr-type (e)
  (cond
   ((not (consp e)) 'primitive)
   ((memq (car e) '(and or not))
    (car e))
   (t
    'primitive)))

;;; Assumes order already normalized

(defun classify-subexprs (e1 e2)
  (let ((type1 (expr-type e1))
	(type2 (expr-type e2)))
    (cond 
     ((eq type1 'or)
      (cond
       ((eq type2 'or)
	'or-or)
       ((eq type2 'and)
	'or-and)
       ((eq type2 'not)
	'or-not)
       ((eq type2 'primitive)
	'primitive)))
     ((eq type1 'and)
      (cond
       ((eq type2 'not)
	'and-not)
       ((eq type2 'primitive)
	'and-primitive)
       ((eq type2 'and)
	'and-and)))
     (t nil))))

;; For use by the syntaxer

(set! normalize-type-expr (symbol-function 'normalize-type-expr))
