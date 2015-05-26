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
;; Chapter 15 -- Lists

(proclaim '(insert-touches t))

(export '(car cdr caar cadr cdar cddr caaar caadr cadar
	caddr cdaar cdadr cddar cdddr caaaar caaadr
	caadar caaddr cadaar cadadr caddar cadddr
	cdaaar cdaadr cdadar cdaddr cddaar
	cddadr cdddar cddddr cons tree-equal endp list-length nth
	first second third fourth fifth sixth seventh eighth ninth tenth
	rest nthcdr list last list* make-list append copy-list copy-alist copy-tree
	revappend nconc nreconc butlast nbutlast ldiff))
	


;;;
;;; IMPORTANT!!! These definitions appear redundant with Scheme's
;;;   but are in fact not because we do not want the
;;;   integration and in-lining that Scheme supplies, so that
;;;   ca...dr of NIL is NIL.
;;;
;;; DO NOT DELETE!!!!!!!!!!!
;;;

;;15.1 Conses

(defun cddr (x) (cdr (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cadr (x) (car (cdr x)))
(defun caar (x) (car (car x)))

(defun cdddr (x) (cdr (cddr x)))
(defun cddar (x) (cdr (cdar x)))
(defun cdadr (x) (cdr (cadr x)))
(defun cdaar (x) (cdr (caar x)))
(defun caddr (x) (car (cddr x)))
(defun cadar (x) (car (cdar x)))
(defun caadr (x) (car (cadr x)))
(defun caaar (x) (car (caar x)))

(defun cddddr (x) (cdr (cdddr x))) 
(defun cdddar (x) (cdr (cddar x)))
(defun cddadr (x) (cdr (cdadr x)))
(defun cddaar (x) (cdr (cdaar x)))
(defun cdaddr (x) (cdr (caddr x)))
(defun cdadar (x) (cdr (cadar x)))
(defun cdaadr (x) (cdr (caadr x)))
(defun cdaaar (x) (cdr (caaar x)))
(defun cadddr (x) (car (cdddr x)))
(defun caddar (x) (car (cddar x)))
(defun cadadr (x) (car (cdadr x)))
(defun cadaar (x) (car (cdaar x)))
(defun caaddr (x) (car (caddr x)))
(defun caadar (x) (car (cadar x)))
(defun caaadr (x) (car (caadr x)))
(defun caaaar (x) (car (caaar x)))


;; Returns T if X and Y are isomorphic trees with identical leaves.

(defun tree-equal-test-not (x y test-not)
  (cond ((not (funcall test-not x y)) t)
	((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	(t '())))

(defun tree-equal-test (x y test)
  (cond ((funcall test x y) t)
	((consp x)
	 (and (consp y)
	      (tree-equal-test (car x) (car y) test)
	      (tree-equal-test (cdr x) (cdr y) test)))
	(t '())))

(defun tree-equal (x y &key (test (function eql)) test-not)
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))

;;15.2 Lists

;;  The recommended way to test for the end of a list.  True if Object is nil,
;;  false if Object is a cons, and an error for any other types of arguments.
(defun endp (object)
  (cond ((null object) t)
	((consp object) nil)
	(t (error "Endp: ~A is not nil or a cons" object))))

;;  "Returns the length of the given List, or Nil if the List is circular."
(defun list-length (list)
  (do ((n 0 (the (non-future fixnum) (+ n 2)))
       (y list (cddr y))
       (z list (cdr z)))
      (())
    (declare (type (non-future fixnum) n))
    (when (endp y) (return n))
    (when (endp (cdr y)) (return (the (non-future fixnum) (+ n 1))))
    (when (and (eq y z) (> n 0)) (return nil))))

(defun nth (n list)
  (car (nthcdr n list)))

(cl-define first #'car)

(defun second (x)  (car (cdr x)))
(defun third (x)   (car (cdr (cdr x))))
(defun fourth (x)  (car (cdr (cdr (cdr x)))))
(defun fifth (x)   (car (cdr (cdr (cdr (cdr x))))))
(defun sixth (x)   (car (cdr (cdr (cdr (cdr (cdr x)))))))
(defun seventh (x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(defun eighth (x)  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))) 
(defun ninth (x)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(defun tenth (x)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))

(cl-define rest #'cdr)

(defun nthcdr (n list)
  (cond ((null? list) nil)
	((zero? n) list)
	(t (nthcdr (-1+ n) (cdr list)))))

;;  Note: "list" defined in "exported-scheme-builtins.scm"

(defun last (l)
  (if (not (consp (cdr l)))
      l
      (last (cdr l))))

(cl-define list* cons*)

(defun make-list (size &key initial-element)
  (declare (type (non-future fixnum) size))
  "Constructs a list with size elements each set to value"
  (if (< size 0) (error "Illegal size for MAKE-LIST: ~A." size)
      (do ((count size (the (non-future fixnum) (1- count)))
	   (result '() (cons initial-element result)))
	  ((zerop count) result)
	(declare (type (non-future fixnum) count)))))

'$split-file

;; Note: "append" defined in "exported-scheme-builtins"

;;; The list is copied correctly even if the list is not terminated by ()
;;; The new list is built by cdr'ing splice which is always at the tail
;;; of the new list

(defun copy-list (list)
  "Returns a new list EQUAL but not EQ to list"
  (if (atom list)
      (if list
	  (error "copy-list: ~A is not a list." list))
      (let ((result (cons (car list) '()) ))
	(do ((x (cdr list) (cdr x))
	     (splice result
		     (cdr (rplacd splice (cons (car x) '() ))) ))
	    ((atom x) (unless (null x)
			      (rplacd splice x))
		      result)))))

(defun copy-alist (alist)
  "Returns a new association list equal to alist, constructed in space"
  (if (atom alist)
      (if alist
	  (error "copy-alist: ~A is not a list." alist))
      (let ((result
	     (cons (if (atom (car alist))
		       (car alist)
		       (cons (caar alist) (cdar alist)) )
		   '() )))	      
	(do ((x (cdr alist) (cdr x))
	     (splice result
		     (cdr (rplacd splice
				  (cons
				   (if (atom (car x)) 
				       (car x)
				       (cons (caar x) (cdar x)))
				   '() ))) ))
;;; Non-null terminated alist done here.
	    ((atom x) (unless (null x)
			      (rplacd splice x))
		      result)))))

(defun copy-tree (object)
  "Copy-Tree recursively copys trees of conses."
  (cond ((not (consp object)) object)
	(T (cons (copy-tree (car object)) (copy-tree (cdr object)))) ))

(defun revappend (x y)
  "Returns (append (reverse x) y)"
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

(cl-define nconc append!)

(defun nreconc (x y)
  "Returns (nconc (nreverse x) y)"
  (do ((1st (cdr x) (if (atom 1st) 1st (cdr 1st)))
       (2nd x 1st)		;2nd follows first down the list.
       (3rd y 2nd))		;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

;;push, pushnew, pop are implemented with the rest of the generalized
;;variables stuff -- see clchap7c.scm

(defun butlast (list &optional (n 1))
  "Returns a new list the same as List without the N last elements."
  (declare (insert-touches nil)	     
	   (list list))
  (let ((n (touch n)))
    (declare (fixnum  n))
    (if (< n 0) (setq n 0))
    (let ((length (the fixnum (1- (length list)))))
      (declare (fixnum length))
      (if (< length n) ()
	  (do* ((top (touch (cdr (touch list))) (touch (cdr top)))
		(result (list (touch (car (touch list)))))
		(splice result)
		(count length (the fixnum (1- count))))
	       ((= count n) result)
	       (declare (fixnum count))
	       (setq splice (cdr (rplacd splice (list (car top))))))))))

(defun nbutlast (list &optional (n 1))
  (declare (insert-touches nil)
	   (list list))
  (let ((n (touch n)))
    (declare (fixnum n))
    "Modifies List to remove the last N elements."
    (if (< n 0) (setq n 0))
    (let ((length (the fixnum (1- (length list)))))
      (declare (fixnum length))
      (if (< length n)
	  '()
	  (do ((1st (touch (cdr (touch list))) (touch (cdr 1st)))
	       (2nd list 1st)
	       (count length (1- count)))
	      ((= count n)
	       (set-cdr! 2nd ())
	       list)
	    (declare (fixnum count)))))))

(defun ldiff (list sublist)
  "Returns a new list, whose elements are those of List that appear before
   Sublist.  If Sublist is not a tail of List, a copy of List is returned."
  (do* ((list list (cdr list))
	(result (list ()))
	(splice result))
       ((or (null list) (eq list sublist)) (cdr result))
       (set-cdr! splice (list (car list)))
       (set! splice (cdr splice))))

