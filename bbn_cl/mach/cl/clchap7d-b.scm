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
(proclaim '(insert-touches t))

(defsetf elt %elt-set)
(defsetf aref %aset)
(defsetf svref cl-vector-set!)
(defsetf char %charset)
(defsetf bit %bitset)
(defsetf schar %scharset)
(defsetf sbit %sbitset)
(defsetf symbol-function %set-symbol-function!)

(defsetf macro-function %set-macro-definition)

(defun %set-macro-definition (symbol expander) 
  (cond ((special-form-p symbol)
	 (error "Attempt to redefine special form ~a" symbol))
	(t
	 (cdr (set (fundefsym symbol)
		   (cons 'commonlisp-macro
			 expander))))))

(defsetf symbol-value set)
(defsetf symbol-plist %set-symbol-plist!)
(defsetf documentation %set-documentation)
(defsetf nth %setnth)

(defun %setnth (n list newval)
  (declare (fixnum n))
  "Sets the Nth element of List (zero based) to Newval."
  (if (< n 0)
      (error "~S is an illegal N for SETF of NTH." n)
      (do ((count n (1- count)))
	  ((zerop count) (rplaca list newval) newval)
	(declare (fixnum count))
	(if (endp (cdr list))
	    (error "~S is too large an index for SETF of NTH." n)
	    (setq list (cdr list))))))

(defsetf nthcdr %set-nthcdr)

(cl-define (%set-nthcdr n l v)
  (if (zero? n)
      l
      (labels ((xloop (i x)
		 (cond
		  ((endp x) (error "~S is an illegal N for SETF of NTHCDR." n))
		  ((= i n) (rplacd x v) l)
		  (t (xloop (1+ i) (cdr x))))))
	(xloop 1 l))))

;(defsetf %sp-svref %sp-svset)
;(defsetf %sp-schar %sp-scharset)
;(defsetf %sp-sbit %sp-sbitset)
;(defsetf %sp-saref1 %sp-saset1)
;(defsetf %sp-cvref %sp-cvset)
;(defsetf %sp-cchar %sp-ccharset)
;(defsetf %sp-cbit %sp-cbitset)
;(defsetf %sp-caref1 %sp-caset1)
(defsetf fill-pointer %set-fill-pointer)

;;A hack--we need a setf handler for GET so that this define-setf-method
;;will work (until we define a real setf/GET thing.)
;;
(defsetf get (symbol indicator) (value) `(%put ,symbol ,indicator ,value))
(defsetf system-get (symbol indicator) (value) `(%system-put ,symbol ,indicator ,value))
