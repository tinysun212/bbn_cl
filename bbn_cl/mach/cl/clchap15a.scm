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
(eval-when (compile)
  (load "clchap15a-macros.bin"))

(proclaim '(insert-touches t))

(export '(rplaca rplacd identity subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not
		 sublis nsublis member member-if member-if-not tailp adjoin union nunion 
		 intersection nintersection set-difference nset-difference set-exclusive-or 
		 nset-exclusive-or subsetp acons pairlis assoc assoc-if assoc-if-not rassoc
		 rassoc-if rassoc-if-not))

;;15.3 Alteration of List Structure

;; *** Moved to cl-util -- needed earlier [las] ***

;;15.4 Substitution of Expressions

;;really belongs in chap 25, but need it here
(cl-define identity identity-procedure)

;  "Substitutes new for subtrees matching old."
(defun subst (new old tree &key (key (function identity))
		  (test (function eql) testp) (test-not nil notp))
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (let ((the-car (s (car subtree)))
			     (the-cdr (s (cdr subtree))))
			 (if (and (eq the-car (car subtree))
				  (eq the-cdr (cdr subtree)))
			     subtree
			     (cons the-car the-cdr)))))))
    (s tree)))

(defun subst-if (new test tree &key (key (function identity)))
  "Substitutes new for subtrees for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (funcall key subtree)) new)
		    ((atom subtree) subtree)
		    (t (let ((the-car (s (car subtree)))
			     (the-cdr (s (cdr subtree))))
			 (if (and (eq the-car (car subtree))
				  (eq the-cdr (cdr subtree)))
			     subtree
			     (cons the-car the-cdr)))))))
    (s tree)))

(defun subst-if-not (new test tree &key (key (function identity)))
  "Substitutes new for subtrees for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (funcall key subtree))) new)
		    ((atom subtree) subtree)
		    (t (let ((the-car (s (car subtree)))
			     (the-cdr (s (cdr subtree))))
			 (if (and (eq the-car (car subtree))
				  (eq the-cdr (cdr subtree)))
			     subtree
			     (cons the-car the-cdr)))))))
    (s tree)))

(defun nsubst (new old tree &key (key (function identity))
		  (test (function eql) testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (satisfies-the-test old subtree)
				 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if (new test tree &key (key (function identity)))
  "Substitutes new for subtrees of tree for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (funcall key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (funcall test (funcall key subtree))
				 (setf (cdr last) new)))
			 (if (funcall test (funcall key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if-not (new test tree &key (key (function identity)))
  "Substitutes new for subtrees of tree for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (funcall key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (not (funcall test (funcall key subtree)))
				 (setf (cdr last) new)))
			 (if (not (funcall test (funcall key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

;;  "Substitutes from alist into tree nondestructively."
(defun sublis (alist tree &key (key (function identity))
		     (test (function eql)) (test-not nil notp))
  (labels ((s (subtree)
	      (let ((assoc
		     (if notp
			 (assoc (funcall key subtree) alist :test-not test-not)
			 (assoc (funcall key subtree) alist :test test))))
		(cond (assoc (cdr assoc))
		      ((atom subtree) subtree)
		      (t (let ((the-car (s (car subtree)))
			       (the-cdr (s (cdr subtree))))
			   (if (and (eq the-car (car subtree))
				    (eq the-cdr (cdr subtree)))
			       subtree
			       (cons the-car the-cdr))))))))
    (s tree)))

(defun nsublis (alist tree &key (key (function identity))
		  (test (function eql)) (test-not nil notp))
  (let (temp)
    (labels ((s (subtree)
		(cond ((Setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (Cdr subtree)))
			      ((atom subtree)
			       (if (setq temp (nsublis-macro))
				   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (Cdr last) (Cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
      (s tree))))

'$split-file

;;15.5 Using Lists as Sets

;;; Use this with the following keyword args:
;;;  (&key (key (function identity)) (test (function eql) testp) (test-not nil notp))

;;  "Returns tail of list beginning with first element satisfying EQUALity,
;;   :test, or :test-not with a given item."
(defun member (item list &key (key (function identity)) (test (function eql)
							      testp)
		    (test-not nil notp))
  (labels ((element-loop (list)
             (if (endp list)
		 nil
		 (let ((the-car (car list)))
		   (if (satisfies-the-test item the-car)
		       list
		       (element-loop (cdr list)))))))
    (element-loop list)))

;; "Returns tail of list beginning with first element satisfying test(element)"
(defun member-if (test list &key (key (function identity)))
  (labels ((element-loop (list)
             (cond ((endp list)
		    nil)
		   ((funcall test (funcall key (car list)))
		    list)
		   (element-loop (cdr list)))))
    (element-loop list)))

;; "Returns tail of list beginning with first element not satisfying test(el)"
(defun member-if-not (test list &key (key (function identity)))
  (labels ((element-loop (list)
             (cond ((endp list)
		    nil)
		   ((not (funcall test (funcall key (car list))))
		    list)
		   (element-loop (cdr list)))))
    (element-loop list)))

;;  "Returns T if sublist is one of the cons'es in list"
(defun tailp (sublist list)
  (labels ((search-loop (x)
             (cond ((endp x)
		    nil)
		   ((eq x sublist)
		    t)
		   (else (search-loop (cdr x))))))
    (search-loop list)))

;;  "Add item to list unless it is already a member"
(defun adjoin (item list &key (key (function identity))
		    (test (function eql)) (test-not nil notp))
  (if (if notp (member (funcall key item) list :test-not test-not :key key)
	  (member (funcall key item) list :test test :key key))
      list
      (cons item list)))

;;  "Returns the union of List1 and List2."
(defun union (list1 list2  &key (key (function identity))
		    (test (function eql) testp) (test-not nil notp))
  (if (and testp notp)
      (error "Union: Test and test-not both supplied."))
  (let ((res list1))
    (dolist (elt list2)
	    (if (not (with-set-keys (member (funcall key elt) list1)))
		(setq res (cons elt res))))
    res))

(defun nunion (list1 list2 &key (key (function identity))
		     (test (function eql) testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res list1))
    (do () ((endp list2))
      (if (not (with-set-keys (member (funcall key (car list2)) list1)))
	  (steve-splice list2 res)
	  (Setq list2 (cdr list2))))
    res))

(defun intersection (list1 list2  &key (key (function identity))
			       (test (function eql) testp) (test-not nil notp))
  "Returns the union of List1 and List2."
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (funcall key elt) list2))
	  (setq res (cons elt res))))
    res))

(defun nintersection (list1 list2 &key (key (function identity))
		     (test (function eql) testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (do () ((endp list1))
      (if (with-set-keys (member (funcall key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (Cdr list1))))
    res))

(defun set-difference (list1 list2 &key (key (function identity))
			     (test (function eql) testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (dolist (elt list1)
      (if (not (with-set-keys (member (funcall key elt) list2)))
	  (setq res (cons elt res))))
    res))


(defun nset-difference (list1 list2 &key (key (function identity))
			      (test (function eql) testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (do () ((endp list1))
      (if (not (with-set-keys (member (funcall key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

'$split-file

(defun set-exclusive-or (list1 list2 &key (key (function identity))
			       (test (function eql) testp) (test-not nil notp))
  "Returns new list of elements appearing exactly  once in List1 and List2."
  (let ((result nil))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall key elt) list2))
	(setq result (cons elt result))))
    (dolist (elt list2)
      (unless (with-set-keys (member (funcall key elt) list1))
	(setq result (cons elt result))))
    result))
  
(defun nset-exclusive-or (list1 list2 &key (test (function eql)) (test-not nil notp)
				(key (function identity)))
  "Return a list with elements which appear but once in List1 and List2."
  (do ((x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
	 (splicey ()))
	((endp y) (setq splicex x))
      (cond ((if notp
		 (not (funcall test-not (funcall key (car x))
			       (funcall key (Car y))))
		 (funcall test (funcall key (car x)) (funcall key (Car y))))
	     (if (null splicex)
		 (setq list1 (cdr x))
		 (rplacd splicex (cdr x)))
	     (if (null splicey) 
		 (setq list2 (cdr y))
		 (rplacd splicey (cdr y)))
	     (return ()))			; assume lists are really sets
	    (t (setq splicey y))))))

(defun subsetp (list1 list2 &key (key (function identity))
		      (test (function eql) testp) (test-not nil notp))
  (dolist (elt list1)
    (unless (with-set-keys (member (funcall key elt) list2))
      (return-from subsetp nil)))
  T)

;; 15.6 Association Lists

(defun acons (key datum alist)
  "Construct a new alist by adding the pair (key . datum) to alist"
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from keys and data (adding to alist)"
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y)) 
	(error "The lists of keys and data are of unequal length."))
    (setq alist (acons (car x) (car y) alist))))

(defun assoc (item alist &key test test-not)
  "Returns the cons in alist whose car is equal (by a given test or EQL) to
   the Item."
  (cond (test (assoc-guts (funcall test item (caar alist))))
	(test-not (assoc-guts (not (funcall test-not item (caar alist)))))
	(t (assoc-guts (eql item (caar alist))))))

(defun assoc-if (predicate alist)
  "Returns the first cons in alist whose car satisfies the Predicate."
  (assoc-guts (funcall predicate (caar alist))))

(defun assoc-if-not (predicate alist)
  "Returns the first cons in alist whose car does not satisfy the Predicate."
  (assoc-guts (not (funcall predicate (caar alist)))))

(defun rassoc (item alist &key test test-not)
  (declare (list alist))
  "Returns the cons in alist whose cdr is equal (by a given test or EQL) to
   the Item."
  (cond (test (assoc-guts (funcall test item (cdar alist))))
	(test-not (assoc-guts (not (funcall test item (cdar alist)))))
	(t (assoc-guts (eql item (cdar alist))))))

(defun rassoc-if (predicate alist)
  "Returns the first cons in alist whose cdr satisfies the Predicate."
  (assoc-guts (funcall predicate (cdar alist))))

(defun rassoc-if-not (predicate alist)
  "Returns the first cons in alist whose cdr does not satisfy the Predicate."
  (assoc-guts (not (funcall predicate (cdar alist)))))
