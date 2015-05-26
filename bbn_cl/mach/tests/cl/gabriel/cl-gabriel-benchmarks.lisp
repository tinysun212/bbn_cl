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
;;;;;;; -*- Mode: LISP; Package: COMMON-LISP-USER; Syntax:Common-Lisp -*-

;;;; 11/29-30/88 Julie Sussman (edits are identifiable by search for BBNACI)
;;;; Wrap some defvar's in eval-when to prevent compile-time eval of
;;;;   the initial-value expression.  Otherwise compilation fails because
;;;;   the needed defun wasn't done at compile time.
;;;;   Also must proclaim the vars special at compile time.
;;;; Change an eval-when to include compile, as in the book.  Else things
;;;;   not recognized as special.
;;;; Change 2 defvar's to setq's (since same name already defvar'ed in
;;;;   earlier benchmark, so initial value ignored)


;;;; This file contains the common lisp version of the lisp performance benchmarks from Stanford.
;;;; These were translated and tested using Symbolics Common Lisp on a Symbolics
;;;; 3600.  The benchmarks in this file have not been "tuned" to any particular
;;;; implementation.  There is no Common Lisp timing function as these are 
;;;; highly system dependent.

;;;; Suggestions or problems to PW@SAIL.

#+Butterfly (proclaim '(insert-touches nil))
#+Butterfly (proclaim '(lr-evaluation nil))

(in-package "GABRIEL-COMMON-LISP-BENCHMARKS")

(eval-when (compile)
  (proclaim '(optimize (speed 3) (safety 0) (space 0))))

(format *trace-output*
        "~%Implementation~@
         ~3@TName: ~A~@
         ~3@TVersion: ~A~@
         Machine~@
         ~3@TName: ~A~@
         ~3@TVersion: ~A~@
         ~3@TInstance: ~A~@
         System Software~@
         ~3@TName: ~A~@
         ~3@TVersion: ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version)
        (machine-type)
        (machine-version)
        (machine-instance)
        (software-type)
        (software-version))


(room t)


(defparameter *gc-afterwards-p* nil)


(defmacro display-benchmark (funcall-form
                             &optional
                             (name (first funcall-form))
                             (gc-afterwards-p '*gc-afterwards-p*))
  `(progn
     (format *trace-output* "~&Benchmarking: ~A" ',name)
     (format *trace-output* "~&Evaluating: ~S" ',funcall-form)
     (multiple-value-call #'(lambda (&rest values)
                              (declare (list values))
                              (let ((*print-length* 10.)
                                    (*print-level* 4.))
                                (format *trace-output*
                                        "~&~[No values.~;~
                                           Value: ~{~S~}.~:;~
                                           Values:~{~&  ~S~^;~}.~]"
                                        (length values)
                                        values)))
                          (time ,funcall-form))
     ;; !!!!! Implementation Specific Code
     (when ,gc-afterwards-p (gc))
     (values)))

;;; TAK -- A vanilla version of the TAKeuchi function and one with tail recursion
;;; removed.

(defun tak (x y z)
  (declare (fixnum x y z))
  (declare (function tak (fixnum fixnum fixnum) fixnum))
  (if (not (< y x))				;xy
      z
      (tak (tak (the fixnum (1- x)) y z)
	      (tak (the fixnum (1- y)) z x)
	      (tak (the fixnum (1- z)) x y))))

;;; call:  tak (tak 18. 12. 6.)

(display-benchmark (tak 18. 12. 6.))


;;; STAK -- The TAKeuchi function with special variables instead of parameter passing.

(defvar stak-x)
(defvar stak-y)
(defvar stak-z)
(proclaim '(fixnum stak-x stak-y stak-z))

(defun stak (stak-x stak-y stak-z)
  (stak-aux))

(defun stak-aux ()
  (if (not (< stak-y stak-x))				; xy
      stak-z
      (let ((stak-x (let ((stak-x (the fixnum (1- stak-x)))
		     (stak-y stak-y)
		     (stak-z stak-z))
		 (stak-aux)))
	    (stak-y (let ((stak-x (the fixnum (1- stak-y)))
		     (stak-y stak-z)
		     (stak-z stak-x))
		 (stak-aux)))
	    (stak-z (let ((stak-x (the fixnum (1- stak-z)))
		     (stak-y stak-x)
		     (stak-z stak-y))
		 (stak-aux))))
	(stak-aux))))

;;; call:  (stak 18. 12. 6.))

(display-benchmark (stak 18. 12. 6.))


;;; CTAK -- A version of the TAKeuchi function that uses the CATCH/THROW facility.

(defun ctak (x y z)
  (declare (fixnum x y z))
  (catch 'ctak (ctak-aux x y z)))

(defun ctak-aux (x y z)
  (declare (fixnum x y z))
  (cond ((not (< y x))	;xy
	 (throw 'ctak z))
	(t (ctak-aux
	     (catch 'ctak
	       (ctak-aux (the fixnum (1- x))
			 y
			 z))
	     (catch 'ctak
	       (ctak-aux (the fixnum (1- y))
			 z
			 x))
	     (catch 'ctak
	       (ctak-aux (the fixnum (1- z))
			 x
			 y))))))

;;; call: (ctak 18. 12. 6.)

(display-benchmark (ctak 18. 12. 6.))


;;; TAKL -- The TAKeuchi function using lists as counters.

(defun listn (n)
  (if (not (= 0 n))
      (cons n (listn (1- n)))))

(EVAL-WHEN (LOAD EVAL)  ;BBNACI
(defvar 18l (listn 18.))
(defvar 12l (listn 12.))
(defvar  6l (listn 6.))
)
(PROCLAIM '(SPECIAL 18l 12l 6l))  ;BBNACI

(defun mas (x y z)
  (declare (list x y z))
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
		 y z)
	    (mas (cdr y)
		 z x)
	    (mas (cdr z)
		 x y))))

(defun shorterp (x y)
  (declare (list x y))
  (and y (or (null x)
	     (shorterp (cdr x)
		       (cdr y)))))

;;; call: (mas 18l 12l 6l)

(display-benchmark (mas 18l 12l 6l) "TakL")


;;; TAKR  -- 100 function (count `em) version of TAK that tries to defeat cache
;;; memory effects.  Results should be the same as for TAK on stack machines.
;;; Distribution of calls is not completely flat.

(defun tak0 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak1 (tak37 (the fixnum (1- x)) y z)
		 (tak11 (the fixnum (1- y)) z x)
		 (tak17 (the fixnum (1- z)) x y)))))
(defun tak1 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak2 (tak74 (the fixnum (1- x)) y z)
		 (tak22 (the fixnum (1- y)) z x)
		 (tak34 (the fixnum (1- z)) x y)))))
(defun tak2 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak3 (tak11 (the fixnum (1- x)) y z)
		 (tak33 (the fixnum (1- y)) z x)
		 (tak51 (the fixnum (1- z)) x y)))))
(defun tak3 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak4 (tak48 (the fixnum (1- x)) y z)
		 (tak44 (the fixnum (1- y)) z x)
		 (tak68 (the fixnum (1- z)) x y)))))
(defun tak4 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak5 (tak85 (the fixnum (1- x)) y z)
		 (tak55 (the fixnum (1- y)) z x)
		 (tak85 (the fixnum (1- z)) x y)))))
(defun tak5 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak6 (tak22 (the fixnum (1- x)) y z)
		 (tak66 (the fixnum (1- y)) z x)
		 (tak2 (the fixnum (1- z)) x y)))))
(defun tak6 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak7 (tak59 (the fixnum (1- x)) y z)
		 (tak77 (the fixnum (1- y)) z x)
		 (tak19 (the fixnum (1- z)) x y)))))
(defun tak7 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak8 (tak96 (the fixnum (1- x)) y z)
		 (tak88 (the fixnum (1- y)) z x)
		 (tak36 (the fixnum (1- z)) x y)))))
(defun tak8 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak9 (tak33 (the fixnum (1- x)) y z)
		 (tak99 (the fixnum (1- y)) z x)
		 (tak53 (the fixnum (1- z)) x y)))))
(defun tak9 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak10 (tak70 (the fixnum (1- x)) y z)
		  (tak10 (the fixnum (1- y)) z x)
		  (tak70 (the fixnum (1- z)) x y)))))
(defun tak10 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak11 (tak7 (the fixnum (1- x)) y z)
		  (tak21 (the fixnum (1- y)) z x)
		  (tak87 (the fixnum (1- z)) x y)))))
(defun tak11 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak12 (tak44 (the fixnum (1- x)) y z)
		  (tak32 (the fixnum (1- y)) z x)
		  (tak4 (the fixnum (1- z)) x y)))))
(defun tak12 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak13 (tak81 (the fixnum (1- x)) y z)
		  (tak43 (the fixnum (1- y)) z x)
		  (tak21 (the fixnum (1- z)) x y)))))

(defun tak13 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak14 (tak18 (the fixnum (1- x)) y z)
		  (tak54 (the fixnum (1- y)) z x)
		  (tak38 (the fixnum (1- z)) x y)))))
(defun tak14 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak15 (tak55 (the fixnum (1- x)) y z)
		  (tak65 (the fixnum (1- y)) z x)
		  (tak55 (the fixnum (1- z)) x y)))))
(defun tak15 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak16 (tak92 (the fixnum (1- x)) y z)
		  (tak76 (the fixnum (1- y)) z x)
		  (tak72 (the fixnum (1- z)) x y)))))
(defun tak16 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak17 (tak29 (the fixnum (1- x)) y z)
		  (tak87 (the fixnum (1- y)) z x)
		  (tak89 (the fixnum (1- z)) x y)))))
(defun tak17 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak18 (tak66 (the fixnum (1- x)) y z)
		  (tak98 (the fixnum (1- y)) z x)
		  (tak6 (the fixnum (1- z)) x y)))))
(defun tak18 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak19 (tak3 (the fixnum (1- x)) y z)
		  (tak9 (the fixnum (1- y)) z x)
		  (tak23 (the fixnum (1- z)) x y)))))
(defun tak19 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak20 (tak40 (the fixnum (1- x)) y z)
		  (tak20 (the fixnum (1- y)) z x)
		  (tak40 (the fixnum (1- z)) x y)))))
(defun tak20 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak21 (tak77 (the fixnum (1- x)) y z)
		  (tak31 (the fixnum (1- y)) z x)
		  (tak57 (the fixnum (1- z)) x y)))))
(defun tak21 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak22 (tak14 (the fixnum (1- x)) y z)
		  (tak42 (the fixnum (1- y)) z x)
		  (tak74 (the fixnum (1- z)) x y)))))
(defun tak22 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak23 (tak51 (the fixnum (1- x)) y z)
		  (tak53 (the fixnum (1- y)) z x)
		  (tak91 (the fixnum (1- z)) x y)))))
(defun tak23 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak24 (tak88 (the fixnum (1- x)) y z)
		  (tak64 (the fixnum (1- y)) z x)
		  (tak8 (the fixnum (1- z)) x y)))))
(defun tak24 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak25 (tak25 (the fixnum (1- x)) y z)
		  (tak75 (the fixnum (1- y)) z x)
		  (tak25 (the fixnum (1- z)) x y)))))
(defun tak25 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak26 (tak62 (the fixnum (1- x)) y z)
		  (tak86 (the fixnum (1- y)) z x)
		  (tak42 (the fixnum (1- z)) x y)))))
(defun tak26 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak27 (tak99 (the fixnum (1- x)) y z)
		  (tak97 (the fixnum (1- y)) z x)
		  (tak59 (the fixnum (1- z)) x y)))))
(defun tak27 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak28 (tak36 (the fixnum (1- x)) y z)
		  (tak8 (the fixnum (1- y)) z x)
		  (tak76 (the fixnum (1- z)) x y)))))
(defun tak28 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak29 (tak73 (the fixnum (1- x)) y z)
		  (tak19 (the fixnum (1- y)) z x)
		  (tak93 (the fixnum (1- z)) x y)))))
(defun tak29 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak30 (tak10 (the fixnum (1- x)) y z)
		  (tak30 (the fixnum (1- y)) z x)
		  (tak10 (the fixnum (1- z)) x y)))))
(defun tak30 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak31 (tak47 (the fixnum (1- x)) y z)
		  (tak41 (the fixnum (1- y)) z x)
		  (tak27 (the fixnum (1- z)) x y)))))
(defun tak31 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak32 (tak84 (the fixnum (1- x)) y z)
		  (tak52 (the fixnum (1- y)) z x)
		  (tak44 (the fixnum (1- z)) x y)))))
(defun tak32 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak33 (tak21 (the fixnum (1- x)) y z)
		  (tak63 (the fixnum (1- y)) z x)
		  (tak61 (the fixnum (1- z)) x y)))))
(defun tak33 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak34 (tak58 (the fixnum (1- x)) y z)
		  (tak74 (the fixnum (1- y)) z x)
		  (tak78 (the fixnum (1- z)) x y)))))
(defun tak34 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak35 (tak95 (the fixnum (1- x)) y z)
		  (tak85 (the fixnum (1- y)) z x)
		  (tak95 (the fixnum (1- z)) x y)))))
(defun tak35 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak36 (tak32 (the fixnum (1- x)) y z)
		  (tak96 (the fixnum (1- y)) z x)
		  (tak12 (the fixnum (1- z)) x y)))))
(defun tak36 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak37 (tak69 (the fixnum (1- x)) y z)
		  (tak7 (the fixnum (1- y)) z x)
		  (tak29 (the fixnum (1- z)) x y)))))
(defun tak37 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak38 (tak6 (the fixnum (1- x)) y z)
		  (tak18 (the fixnum (1- y)) z x)
		  (tak46 (the fixnum (1- z)) x y)))))
(defun tak38 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak39 (tak43 (the fixnum (1- x)) y z)
		  (tak29 (the fixnum (1- y)) z x)
		  (tak63 (the fixnum (1- z)) x y)))))
(defun tak39 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak40 (tak80 (the fixnum (1- x)) y z)
		  (tak40 (the fixnum (1- y)) z x)
		  (tak80 (the fixnum (1- z)) x y)))))
(defun tak40 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak41 (tak17 (the fixnum (1- x)) y z)
		  (tak51 (the fixnum (1- y)) z x)
		  (tak97 (the fixnum (1- z)) x y)))))
(defun tak41 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak42 (tak54 (the fixnum (1- x)) y z)
		  (tak62 (the fixnum (1- y)) z x)
		  (tak14 (the fixnum (1- z)) x y)))))
(defun tak42 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak43 (tak91 (the fixnum (1- x)) y z)
		  (tak73 (the fixnum (1- y)) z x)
		  (tak31 (the fixnum (1- z)) x y)))))
(defun tak43 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak44 (tak28 (the fixnum (1- x)) y z)
		  (tak84 (the fixnum (1- y)) z x)
		  (tak48 (the fixnum (1- z)) x y)))))
(defun tak44 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak45 (tak65 (the fixnum (1- x)) y z)
		  (tak95 (the fixnum (1- y)) z x)
		  (tak65 (the fixnum (1- z)) x y)))))
(defun tak45 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak46 (tak2 (the fixnum (1- x)) y z)
		  (tak6 (the fixnum (1- y)) z x)
		  (tak82 (the fixnum (1- z)) x y)))))
(defun tak46 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak47 (tak39 (the fixnum (1- x)) y z)
		  (tak17 (the fixnum (1- y)) z x)
		  (tak99 (the fixnum (1- z)) x y)))))
(defun tak47 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak48 (tak76 (the fixnum (1- x)) y z)
		  (tak28 (the fixnum (1- y)) z x)
		  (tak16 (the fixnum (1- z)) x y)))))
(defun tak48 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak49 (tak13 (the fixnum (1- x)) y z)
		  (tak39 (the fixnum (1- y)) z x)
		  (tak33 (the fixnum (1- z)) x y)))))
(defun tak49 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak50 (tak50 (the fixnum (1- x)) y z)
		  (tak50 (the fixnum (1- y)) z x)
		  (tak50 (the fixnum (1- z)) x y)))))
(defun tak50 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak51 (tak87 (the fixnum (1- x)) y z)
		  (tak61 (the fixnum (1- y)) z x)
		  (tak67 (the fixnum (1- z)) x y)))))
(defun tak51 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak52 (tak24 (the fixnum (1- x)) y z)
		  (tak72 (the fixnum (1- y)) z x)
		  (tak84 (the fixnum (1- z)) x y)))))
(defun tak52 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak53 (tak61 (the fixnum (1- x)) y z)
		  (tak83 (the fixnum (1- y)) z x)
		  (tak1 (the fixnum (1- z)) x y)))))
(defun tak53 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak54 (tak98 (the fixnum (1- x)) y z)
		  (tak94 (the fixnum (1- y)) z x)
		  (tak18 (the fixnum (1- z)) x y)))))
(defun tak54 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak55 (tak35 (the fixnum (1- x)) y z)
		  (tak5 (the fixnum (1- y)) z x)
		  (tak35 (the fixnum (1- z)) x y)))))
(defun tak55 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak56 (tak72 (the fixnum (1- x)) y z)
		  (tak16 (the fixnum (1- y)) z x)
		  (tak52 (the fixnum (1- z)) x y)))))
(defun tak56 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak57 (tak9 (the fixnum (1- x)) y z)
		  (tak27 (the fixnum (1- y)) z x)
		  (tak69 (the fixnum (1- z)) x y)))))
(defun tak57 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak58 (tak46 (the fixnum (1- x)) y z)
		  (tak38 (the fixnum (1- y)) z x)
		  (tak86 (the fixnum (1- z)) x y)))))
(defun tak58 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak59 (tak83 (the fixnum (1- x)) y z)
		  (tak49 (the fixnum (1- y)) z x)
		  (tak3 (the fixnum (1- z)) x y)))))
(defun tak59 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak60 (tak20 (the fixnum (1- x)) y z)
		  (tak60 (the fixnum (1- y)) z x)
		  (tak20 (the fixnum (1- z)) x y)))))
(defun tak60 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak61 (tak57 (the fixnum (1- x)) y z)
		  (tak71 (the fixnum (1- y)) z x)
		  (tak37 (the fixnum (1- z)) x y)))))
(defun tak61 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak62 (tak94 (the fixnum (1- x)) y z)
		  (tak82 (the fixnum (1- y)) z x)
		  (tak54 (the fixnum (1- z)) x y)))))
(defun tak62 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak63 (tak31 (the fixnum (1- x)) y z)
		  (tak93 (the fixnum (1- y)) z x)
		  (tak71 (the fixnum (1- z)) x y)))))
(defun tak63 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak64 (tak68 (the fixnum (1- x)) y z)
		  (tak4 (the fixnum (1- y)) z x)
		  (tak88 (the fixnum (1- z)) x y)))))
(defun tak64 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak65 (tak5 (the fixnum (1- x)) y z)
		  (tak15 (the fixnum (1- y)) z x)
		  (tak5 (the fixnum (1- z)) x y)))))
(defun tak65 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak66 (tak42 (the fixnum (1- x)) y z)
		  (tak26 (the fixnum (1- y)) z x)
		  (tak22 (the fixnum (1- z)) x y)))))
(defun tak66 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak67 (tak79 (the fixnum (1- x)) y z)
		  (tak37 (the fixnum (1- y)) z x)
		  (tak39 (the fixnum (1- z)) x y)))))
(defun tak67 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak68 (tak16 (the fixnum (1- x)) y z)
		  (tak48 (the fixnum (1- y)) z x)
		  (tak56 (the fixnum (1- z)) x y)))))
(defun tak68 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak69 (tak53 (the fixnum (1- x)) y z)
		  (tak59 (the fixnum (1- y)) z x)
		  (tak73 (the fixnum (1- z)) x y)))))
(defun tak69 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak70 (tak90 (the fixnum (1- x)) y z)
		  (tak70 (the fixnum (1- y)) z x)
		  (tak90 (the fixnum (1- z)) x y)))))
(defun tak70 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak71 (tak27 (the fixnum (1- x)) y z)
		  (tak81 (the fixnum (1- y)) z x)
		  (tak7 (the fixnum (1- z)) x y)))))
(defun tak71 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak72 (tak64 (the fixnum (1- x)) y z)
		  (tak92 (the fixnum (1- y)) z x)
		  (tak24 (the fixnum (1- z)) x y)))))
(defun tak72 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak73 (tak1 (the fixnum (1- x)) y z)
		  (tak3 (the fixnum (1- y)) z x)
		  (tak41 (the fixnum (1- z)) x y)))))
(defun tak73 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak74 (tak38 (the fixnum (1- x)) y z)
		  (tak14 (the fixnum (1- y)) z x)
		  (tak58 (the fixnum (1- z)) x y)))))
(defun tak74 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak75 (tak75 (the fixnum (1- x)) y z)
		  (tak25 (the fixnum (1- y)) z x)
		  (tak75 (the fixnum (1- z)) x y)))))
(defun tak75 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak76 (tak12 (the fixnum (1- x)) y z)
		  (tak36 (the fixnum (1- y)) z x)
		  (tak92 (the fixnum (1- z)) x y)))))
(defun tak76 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak77 (tak49 (the fixnum (1- x)) y z)
		  (tak47 (the fixnum (1- y)) z x)
		  (tak9 (the fixnum (1- z)) x y)))))
(defun tak77 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak78 (tak86 (the fixnum (1- x)) y z)
		  (tak58 (the fixnum (1- y)) z x)
		  (tak26 (the fixnum (1- z)) x y)))))
(defun tak78 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak79 (tak23 (the fixnum (1- x)) y z)
		  (tak69 (the fixnum (1- y)) z x)
		  (tak43 (the fixnum (1- z)) x y)))))
(defun tak79 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak80 (tak60 (the fixnum (1- x)) y z)
		  (tak80 (the fixnum (1- y)) z x)
		  (tak60 (the fixnum (1- z)) x y)))))
(defun tak80 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak81 (tak97 (the fixnum (1- x)) y z)
		  (tak91 (the fixnum (1- y)) z x)
		  (tak77 (the fixnum (1- z)) x y)))))
(defun tak81 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak82 (tak34 (the fixnum (1- x)) y z)
		  (tak2 (the fixnum (1- y)) z x)
		  (tak94 (the fixnum (1- z)) x y)))))
(defun tak82 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak83 (tak71 (the fixnum (1- x)) y z)
		  (tak13 (the fixnum (1- y)) z x)
		  (tak11 (the fixnum (1- z)) x y)))))
(defun tak83 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak84 (tak8 (the fixnum (1- x)) y z)
		  (tak24 (the fixnum (1- y)) z x)
		  (tak28 (the fixnum (1- z)) x y)))))
(defun tak84 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak85 (tak45 (the fixnum (1- x)) y z)
		  (tak35 (the fixnum (1- y)) z x)
		  (tak45 (the fixnum (1- z)) x y)))))
(defun tak85 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak86 (tak82 (the fixnum (1- x)) y z)
		  (tak46 (the fixnum (1- y)) z x)
		  (tak62 (the fixnum (1- z)) x y)))))
(defun tak86 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak87 (tak19 (the fixnum (1- x)) y z)
		  (tak57 (the fixnum (1- y)) z x)
		  (tak79 (the fixnum (1- z)) x y)))))
(defun tak87 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak88 (tak56 (the fixnum (1- x)) y z)
		  (tak68 (the fixnum (1- y)) z x)
		  (tak96 (the fixnum (1- z)) x y)))))
(defun tak88 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak89 (tak93 (the fixnum (1- x)) y z)
		  (tak79 (the fixnum (1- y)) z x)
		  (tak13 (the fixnum (1- z)) x y)))))
(defun tak89 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak90 (tak30 (the fixnum (1- x)) y z)
		  (tak90 (the fixnum (1- y)) z x)
		  (tak30 (the fixnum (1- z)) x y)))))
(defun tak90 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak91 (tak67 (the fixnum (1- x)) y z)
		  (tak1 (the fixnum (1- y)) z x)
		  (tak47 (the fixnum (1- z)) x y)))))
(defun tak91 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak92 (tak4 (the fixnum (1- x)) y z)
		  (tak12 (the fixnum (1- y)) z x)
		  (tak64 (the fixnum (1- z)) x y)))))
(defun tak92 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak93 (tak41 (the fixnum (1- x)) y z)
		  (tak23 (the fixnum (1- y)) z x)
		  (tak81 (the fixnum (1- z)) x y)))))
(defun tak93 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak94 (tak78 (the fixnum (1- x)) y z)
		  (tak34 (the fixnum (1- y)) z x)
		  (tak98 (the fixnum (1- z)) x y)))))
(defun tak94 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak95 (tak15 (the fixnum (1- x)) y z)
		  (tak45 (the fixnum (1- y)) z x)
		  (tak15 (the fixnum (1- z)) x y)))))
(defun tak95 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak96 (tak52 (the fixnum (1- x)) y z)
		  (tak56 (the fixnum (1- y)) z x)
		  (tak32 (the fixnum (1- z)) x y)))))
(defun tak96 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak97 (tak89 (the fixnum (1- x)) y z)
		  (tak67 (the fixnum (1- y)) z x)
		  (tak49 (the fixnum (1- z)) x y)))))
(defun tak97 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak98 (tak26 (the fixnum (1- x)) y z)
		  (tak78 (the fixnum (1- y)) z x)
		  (tak66 (the fixnum (1- z)) x y)))))
(defun tak98 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak99 (tak63 (the fixnum (1- x)) y z)
		  (tak89 (the fixnum (1- y)) z x)
		  (tak83 (the fixnum (1- z)) x y)))))
(defun tak99 (x y z) (declare (fixnum x y z)) 
  (cond ((not (< y x)) z)
	(t (tak0 (tak0 (the fixnum (1- x)) y z)
		 (tak0 (the fixnum (1- y)) z x)
		 (tak0 (the fixnum (1- z)) x y)))))

;;; call:  (tak0 18. 12. 6.)	

(display-benchmark (tak0 18. 12. 6.) "TakR")


;;; BOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Fairly CONS intensive.

(defvar unify-subst)
(defvar temp-temp)

(defun add-lemma (term)
  (cond ((and (not (atom term))
	      (eq (car term)
		  (quote equal))
	      (not (atom (cadr term))))
	 (setf (get (car (cadr term)) (quote lemmas))
	       (cons term (get (car (cadr term)) (quote lemmas)))))
	(t (error "~%ADD-LEMMA did not like term:  ~a" term))))

(defun add-lemma-lst (lst)
  (cond ((null lst)
	 t)
	(t (add-lemma (car lst))
	   (add-lemma-lst (cdr lst)))))

(defun apply-subst (alist term)
  (cond ((atom term)
	 (cond ((setq temp-temp (assoc term alist))
		(cdr temp-temp))
	       (t term)))
	(t (cons (car term)
		 (apply-subst-lst alist (cdr term))))))

(defun apply-subst-lst (alist lst)
  (cond ((null lst)
	 nil)
	(t (cons (apply-subst alist (car lst))
		 (apply-subst-lst alist (cdr lst))))))

(defun falsep (x lst)
  (or (equal x (quote (f)))
      (member x lst)))

(defun one-way-unify (term1 term2)
  (progn (setq unify-subst nil)
	 (one-way-unify1 term1 term2)))

(defun one-way-unify1 (term1 term2)
  (cond ((atom term2)
	 (cond ((setq temp-temp (assoc term2 unify-subst))
		(equal term1 (cdr temp-temp)))
	       (t (setq unify-subst (cons (cons term2 term1)
					  unify-subst))
		  t)))
	((atom term1)
	 nil)
	((eq (car term1)
	     (car term2))
	 (one-way-unify1-lst (cdr term1)
			     (cdr term2)))
	(t nil)))

(defun one-way-unify1-lst (lst1 lst2)
  (cond ((null lst1)
	 t)
	((one-way-unify1 (car lst1)
			 (car lst2))
	 (one-way-unify1-lst (cdr lst1)
			     (cdr lst2)))
	(t nil)))

(defun rewrite (term)
  (cond ((atom term)
	 term)
	(t (rewrite-with-lemmas (cons (car term)
				      (rewrite-args (cdr term)))
				(get (car term)
				     (quote lemmas))))))

(defun rewrite-args (lst)
  (cond ((null lst)
	 nil)
	(t (cons (rewrite (car lst))
		 (rewrite-args (cdr lst))))))

(defun rewrite-with-lemmas (term lst)
  (cond ((null lst)
	 term)
	((one-way-unify term (cadr (car lst)))
	 (rewrite (apply-subst unify-subst (caddr (car lst)))))
	(t (rewrite-with-lemmas term (cdr lst)))))

(defun setup ()
  (add-lemma-lst
    (quote ((equal (compile form)
		   (reverse (codegen (optimize form)
				     (nil))))
	    (equal (eqp x y)
		   (equal (fix x)
			  (fix y)))
	    (equal (greaterp x y)
		   (lessp y x))
	    (equal (lesseqp x y)
		   (not (lessp y x)))
	    (equal (greatereqp x y)
		   (not (lessp x y)))
	    (equal (boolean x)
		   (or (equal x (t))
		       (equal x (f))))
	    (equal (iff x y)
		   (and (implies x y)
			(implies y x)))
	    (equal (even1 x)
		   (if (zerop x)
		       (t)
		       (odd (1- x))))
	    (equal (countps- l pred)
		   (countps-loop l pred (zero)))
	    (equal (fact- i)
		   (fact-loop i 1))
	    (equal (reverse- x)
		   (reverse-loop x (nil)))
	    (equal (divides x y)
		   (zerop (remainder y x)))
	    (equal (assume-true var alist)
		   (cons (cons var (t))
			 alist))
	    (equal (assume-false var alist)
		   (cons (cons var (f))
			 alist))
	    (equal (tautology-checker x)
		   (tautologyp (normalize x)
			       (nil)))
	    (equal (falsify x)
		   (falsify1 (normalize x)
			     (nil)))
	    (equal (prime x)
		   (and (not (zerop x))
			(not (equal x (add1 (zero))))
			(prime1 x (1- x))))
	    (equal (and p q)
		   (if p (if q (t)
			     (f))
		       (f)))
	    (equal (or p q)
		   (if p (t)
		       (if q (t)
			   (f))
		       (f)))
	    (equal (not p)
		   (if p (f)
		       (t)))
	    (equal (implies p q)
		   (if p (if q (t)
			     (f))
		       (t)))
	    (equal (fix x)
		   (if (numberp x)
		       x
		       (zero)))
	    (equal (if (if a b c)
		       d e)
		   (if a (if b d e)
		       (if c d e)))
	    (equal (zerop x)
		   (or (equal x (zero))
		       (not (numberp x))))
	    (equal (plus (plus x y)
			 z)
		   (plus x (plus y z)))
	    (equal (equal (plus a b)
			  (zero))
		   (and (zerop a)
			(zerop b)))
	    (equal (difference x x)
		   (zero))
	    (equal (equal (plus a b)
			  (plus a c))
		   (equal (fix b)
			  (fix c)))
	    (equal (equal (zero)
			  (difference x y))
		   (not (lessp y x)))
	    (equal (equal x (difference x y))
		   (and (numberp x)
			(or (equal x (zero))
			    (zerop y))))
	    (equal (meaning (plus-tree (append x y))
			    a)
		   (plus (meaning (plus-tree x)
				  a)
			 (meaning (plus-tree y)
				  a)))
	    (equal (meaning (plus-tree (plus-fringe x))
			    a)
		   (fix (meaning x a)))
	    (equal (append (append x y)
			   z)
		   (append x (append y z)))
	    (equal (reverse (append a b))
		   (append (reverse b)
			   (reverse a)))
	    (equal (times x (plus y z))
		   (plus (times x y)
			 (times x z)))
	    (equal (times (times x y)
			  z)
		   (times x (times y z)))
	    (equal (equal (times x y)
			  (zero))
		   (or (zerop x)
		       (zerop y)))
	    (equal (exec (append x y)
			 pds envrn)
		   (exec y (exec x pds envrn)
			 envrn))
	    (equal (mc-flatten x y)
		   (append (flatten x)
			   y))
	    (equal (member x (append a b))
		   (or (member x a)
		       (member x b)))
	    (equal (member x (reverse y))
		   (member x y))
	    (equal (length (reverse x))
		   (length x))
	    (equal (member a (intersect b c))
		   (and (member a b)
			(member a c)))
	    (equal (nth (zero)
			i)
		   (zero))
	    (equal (exp i (plus j k))
		   (times (exp i j)
			  (exp i k)))
	    (equal (exp i (times j k))
		   (exp (exp i j)
			k))
	    (equal (reverse-loop x y)
		   (append (reverse x)
			   y))
	    (equal (reverse-loop x (nil))
		   (reverse x))
	    (equal (count-list z (sort-lp x y))
		   (plus (count-list z x)
			 (count-list z y)))
	    (equal (equal (append a b)
			  (append a c))
		   (equal b c))
	    (equal (plus (remainder x y)
			 (times y (quotient x y)))
		   (fix x))
	    (equal (power-eval (big-plus1 l i base)
			       base)
		   (plus (power-eval l base)
			 i))
	    (equal (power-eval (big-plus x y i base)
			       base)
		   (plus i (plus (power-eval x base)
				 (power-eval y base))))
	    (equal (remainder y 1)
		   (zero))
	    (equal (lessp (remainder x y)
			  y)
		   (not (zerop y)))
	    (equal (remainder x x)
		   (zero))
	    (equal (lessp (quotient i j)
			  i)
		   (and (not (zerop i))
			(or (zerop j)
			    (not (equal j 1)))))
	    (equal (lessp (remainder x y)
			  x)
		   (and (not (zerop y))
			(not (zerop x))
			(not (lessp x y))))
	    (equal (power-eval (power-rep i base)
			       base)
		   (fix i))
	    (equal (power-eval (big-plus (power-rep i base)
					 (power-rep j base)
					 (zero)
					 base)
			       base)
		   (plus i j))
	    (equal (gcd x y)
		   (gcd y x))
	    (equal (nth (append a b)
			i)
		   (append (nth a i)
			   (nth b (difference i (length a)))))
	    (equal (difference (plus x y)
			       x)
		   (fix y))
	    (equal (difference (plus y x)
			       x)
		   (fix y))
	    (equal (difference (plus x y)
			       (plus x z))
		   (difference y z))
	    (equal (times x (difference c w))
		   (difference (times c x)
			       (times w x)))
	    (equal (remainder (times x z)
			      z)
		   (zero))
	    (equal (difference (plus b (plus a c))
			       a)
		   (plus b c))
	    (equal (difference (add1 (plus y z))
			       z)
		   (add1 y))
	    (equal (lessp (plus x y)
			  (plus x z))
		   (lessp y z))
	    (equal (lessp (times x z)
			  (times y z))
		   (and (not (zerop z))
			(lessp x y)))
	    (equal (lessp y (plus x y))
		   (not (zerop x)))
	    (equal (gcd (times x z)
			(times y z))
		   (times z (gcd x y)))
	    (equal (value (normalize x)
			  a)
		   (value x a))
	    (equal (equal (flatten x)
			  (cons y (nil)))
		   (and (nlistp x)
			(equal x y)))
	    (equal (listp (gopher x))
		   (listp x))
	    (equal (samefringe x y)
		   (equal (flatten x)
			  (flatten y)))
	    (equal (equal (greatest-factor x y)
			  (zero))
		   (and (or (zerop y)
			    (equal y 1))
			(equal x (zero))))
	    (equal (equal (greatest-factor x y)
			  1)
		   (equal x 1))
	    (equal (numberp (greatest-factor x y))
		   (not (and (or (zerop y)
				 (equal y 1))
			     (not (numberp x)))))
	    (equal (times-list (append x y))
		   (times (times-list x)
			  (times-list y)))
	    (equal (prime-list (append x y))
		   (and (prime-list x)
			(prime-list y)))
	    (equal (equal z (times w z))
		   (and (numberp z)
			(or (equal z (zero))
			    (equal w 1))))
	    (equal (greatereqpr x y)
		   (not (lessp x y)))
	    (equal (equal x (times x y))
		   (or (equal x (zero))
		       (and (numberp x)
			    (equal y 1))))
	    (equal (remainder (times y x)
			      y)
		   (zero))
	    (equal (equal (times a b)
			  1)
		   (and (not (equal a (zero)))
			(not (equal b (zero)))
			(numberp a)
			(numberp b)
			(equal (1- a)
			       (zero))
			(equal (1- b)
			       (zero))))
	    (equal (lessp (length (delete x l))
			  (length l))
		   (member x l))
	    (equal (sort2 (delete x l))
		   (delete x (sort2 l)))
	    (equal (dsort x)
		   (sort2 x))
	    (equal (length (cons x1
				 (cons x2
				       (cons x3 (cons x4
						      (cons x5
							    (cons x6 x7)))))))
		   (plus 6 (length x7)))
	    (equal (difference (add1 (add1 x))
			       2)
		   (fix x))
	    (equal (quotient (plus x (plus x y))
			     2)
		   (plus x (quotient y 2)))
	    (equal (sigma (zero)
			  i)
		   (quotient (times i (add1 i))
			     2))
	    (equal (plus x (add1 y))
		   (if (numberp y)
		       (add1 (plus x y))
		       (add1 x)))
	    (equal (equal (difference x y)
			  (difference z y))
		   (if (lessp x y)
		       (not (lessp y z))
		       (if (lessp z y)
			   (not (lessp y x))
			   (equal (fix x)
				  (fix z)))))
	    (equal (meaning (plus-tree (delete x y))
			    a)
		   (if (member x y)
		       (difference (meaning (plus-tree y)
					    a)
				   (meaning x a))
		       (meaning (plus-tree y)
				a)))
	    (equal (times x (add1 y))
		   (if (numberp y)
		       (plus x (times x y))
		       (fix x)))
	    (equal (nth (nil)
			i)
		   (if (zerop i)
		       (nil)
		       (zero)))
	    (equal (last (append a b))
		   (if (listp b)
		       (last b)
		       (if (listp a)
			   (cons (car (last a))
				 b)
			   b)))
	    (equal (equal (lessp x y)
			  z)
		   (if (lessp x y)
		       (equal t z)
		       (equal f z)))
	    (equal (assignment x (append a b))
		   (if (assignedp x a)
		       (assignment x a)
		       (assignment x b)))
	    (equal (car (gopher x))
		   (if (listp x)
		       (car (flatten x))
		       (zero)))
	    (equal (flatten (cdr (gopher x)))
		   (if (listp x)
		       (cdr (flatten x))
		       (cons (zero)
			     (nil))))
	    (equal (quotient (times y x)
			     y)
		   (if (zerop y)
		       (zero)
		       (fix x)))
	    (equal (get j (set i val mem))
		   (if (eqp j i)
		       val
		       (get j mem)))))))

(defun tautologyp (x true-lst false-lst)
  (cond ((truep x true-lst)
	 t)
	((falsep x false-lst)
	 nil)
	((atom x)
	 nil)
	((eq (car x)
	     (quote if))
	 (cond ((truep (cadr x)
		       true-lst)
		(tautologyp (caddr x)
			    true-lst false-lst))
	       ((falsep (cadr x)
			false-lst)
		(tautologyp (cadddr x)
			    true-lst false-lst))
	       (t (and (tautologyp (caddr x)
				   (cons (cadr x)
					 true-lst)
				   false-lst)
		       (tautologyp (cadddr x)
				   true-lst
				   (cons (cadr x)
					 false-lst))))))
	(t nil)))

(defun tautp (x)
  (tautologyp (rewrite x)
	      nil nil))

(defun test ()
  (prog (ans term)
	(setq term
	      (apply-subst
		(quote ((x f (plus (plus a b)
				   (plus c (zero))))
			(y f (times (times a b)
				    (plus c d)))
			(z f (reverse (append (append a b)
					      (nil))))
			(u equal (plus a b)
			   (difference x y))
			(w lessp (remainder a b)
			   (member a (length b)))))
		(quote (implies (and (implies x y)
				     (and (implies y z)
					  (and (implies z u)
					       (implies u w))))
				(implies x w)))))
	(setq ans (tautp term))))

(defun trans-of-implies (n)
  (list (quote implies)
	(trans-of-implies1 n)
	(list (quote implies)
	      0 n)))

(defun trans-of-implies1 (n)
  ;; EQL was used in the book.
  (cond ((eql n 1)			; I think (eql n 1) may work here
	 (list (quote implies)
	       0 1))
	(t (list (quote and)
		 (list (quote implies)
		       (1- n)
		       n)
		 (trans-of-implies1 (1- n))))))

(defun truep (x lst)
       (or (equal x (quote (t)))
	   (member x lst)))

(eval-when (load eval)
  (setup))

;;; make sure you've run (setup) then call:  (test)

(display-benchmark (test) "Boyer")


;;;; BROWSE -- Benchmark to create and browse through an AI-like data base of units.

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(defvar rand 21.)

(defun seed () (setq rand 21.))

;; CHAR was used in the book.
(defmacro char1 (x) `(char (string ,x) 0))	; maybe SYMBOL-NAME

(defun browse-init (n m npats ipats)
  (let ((ipats (copy-tree ipats)))
    (do ((p ipats (cdr p)))
	((null (cdr p)) (rplacd p ipats)))	
    (do ((n n (1- n))
	 (i m (cond ((= i 0) m)
		    (t (1- i))))
	 (name (gensym) (gensym))
	 (a ()))
	((= n 0) a)
      (push name a)
      (do ((i i (1- i)))
	  ((= i 0))
	(setf (get name (gensym)) nil))
      (setf (get name 'pattern)
	    (do ((i npats (1- i))
		 (ipats ipats (cdr ipats))
		 (a ()))
		((= i 0) a)
	      (push (car ipats) a)))
      (do ((j (- m i) (1- j)))
	  ((= j 0))
	(setf (get name (gensym)) nil)))))  


(defun browse-random ()
  (setq rand (mod (* rand 17.) 251.)))

(defun randomize (l)
  (do ((a ()))
      ((null l) a)
    (let ((n (mod (browse-random) (length l))))
      (cond ((= n 0)
	     (push (car l) a)
	     (setq l (cdr l)))
	    (t 
	     (do ((n n (1- n))
		  (x l (cdr x)))
		 ((= n 1)
		  (push (cadr x) a)
		  (rplacd x (cddr x)))))))))


(defun match (pat dat alist)
  (cond ((null pat)
	 (null dat))
	((null dat) ())
	((or (eq (car pat) '?)
	     (eq (car pat)
		 (car dat)))
	 (match (cdr pat) (cdr dat) alist))
	((eq (car pat) '*)
	 (or (match (cdr pat) dat alist)
	     (match (cdr pat) (cdr dat) alist)
	     (match pat (cdr dat) alist)))
	(t (cond ((atom (car pat))
		  (cond ((eq (char1 (car pat)) #\?)
			 (let ((val (assoc (car pat) alist)))
			   (cond (val (match (cons (cdr val)
						   (cdr pat))
					     dat alist))
				 (t (match (cdr pat)
					   (cdr dat)
					   (cons (cons (car pat)
						       (car dat))
						 alist))))))
			((eq (char1 (car pat)) #\*)
			 (let ((val (assoc (car pat) alist)))
			   (cond (val (match (append (cdr val)
						     (cdr pat))
					     dat alist))
				 (t 
				  (do ((l () (nconc l (cons (car d) nil)))
				       (e (cons () dat) (cdr e))
				       (d dat (cdr d)))
				      ((null e) ())
				    (cond ((match (cdr pat) d
						  (cons (cons (car pat) l)
							alist))
					   (return t))))))))))
		 (t (and 
		      (not (atom (car dat)))
		      (match (car pat)
			     (car dat) alist)
		      (match (cdr pat)
			     (cdr dat) alist)))))))

(defun browse ()
  (seed)
  (investigate (randomize 
		 (browse-init 100. 10. 4. '((a a a b b b b a a a a a b b a a a)
				     (a a b b b b a a
					(a a)(b b))
				     (a a a b (b a) b a b a))))
	       '((*a ?b *b ?b a *a a *b *a)
		 (*a *b *b *a (*a) (*b))
		 (? ? * (b a) * ? ?))))

(defun investigate (units pats)
  (do ((units units (cdr units)))
      ((null units))
    (do ((pats pats (cdr pats)))
	((null pats))
      (do ((p (get (car units) 'pattern)
	      (cdr p)))
	  ((null p))
	(match (car pats) (car p) ())))))

;;; call: (browse)

(display-benchmark (browse))


;;; DESTRU -- Destructive operation benchmark

(defun destructive (n m)
  (declare (fixnum n m))
  (let ((l (do ((i 10. (the fixnum (1- i)))
		(a () (push () a)))
	       ((= i 0) a)
             (declare (fixnum i)
                      (list a)))))
    (declare (list l))
    (do ((i n (the fixnum (1- i))))
	((= i 0))
      (declare (fixnum i))
      (cond ((null (car l))
	     (do ((l l (cdr l)))
		 ((null l))
	       (or (car l) 
		   (rplaca l (cons () ())))
	       (nconc (car l)
		      (do ((j m (the fixnum (1- j)))
			   (a () (push () a)))
			  ((= j 0) a)
                        (declare (fixnum j)
                                 (list a)))))) 
	    (t
	     (do ((l1 l (cdr l1))
		  (l2 (cdr l) (cdr l2)))
		 ((null l2))
	       (rplacd (do ((j (floor (the fixnum (length (car l2))) 2)
                               (the fixnum (1- j)))
			    (a (car l2) (cdr a)))
			   ((zerop j) a)
                         (declare (fixnum j)
                                  (list a))
			 (rplaca a i))
		       (let ((n (floor (the fixnum (length (car l1))) 2)))
                         (declare (fixnum n))
			 (cond ((= n 0) (rplaca l1 ())
				(car l1))
			       (t 
				(do ((j n (the fixnum (1- j)))
				     (a (car l1) (cdr a)))
				    ((= j 1)
				     (prog1 (cdr a)
					    (rplacd a ())))
                                  (declare (fixnum j)
                                           (list a))
				  (rplaca a i))))))))))))

;;; call:  (destructive 600. 50.)

(display-benchmark (destructive 600. 50.))


;;; TRAVERSE --  Benchmark which creates and traverses a tree structure.

(proclaim '(optimize (speed 3) (safety 0) (space 0)))

(defstruct node
  (parents ())
  (sons ())
  (sn (snb))
  (entry1 ())
  (entry2 ())
  (entry3 ())
  (entry4 ())
  (entry5 ())
  (entry6 ())
  (mark ()))

(defvar sn 0)
(proclaim '(fixnum sn))
(defvar rand 21.)
(proclaim '(fixnum rand))

(defvar count 0)
(proclaim '(fixnum count))
(defvar marker nil)
(defvar root)

(defun snb ()
  (setq sn (the fixnum (1+ sn))))

(defun seed ()
  (setq rand 21.))

(defun traverse-random () (setq rand (mod (the fixnum (* rand 17.)) 251.)))

(defun traverse-remove (n q)
  (declare (fixnum n))
  (cond ((eq (cdr (car q)) (car q))
	 (prog2 () (caar q) (rplaca q ())))
	((= n 0)
	 (prog2 () (caar q)
		(do ((p (car q) (cdr p)))
		    ((eq (cdr p) (car q))
		     (rplaca q
			     (rplacd p (cdr (car q))))))))
	(t (do ((n n (the fixnum (1- n)))
		(q (car q) (cdr q))
		(p (cdr (car q)) (cdr p)))
	       ((= n 0) (prog2 () (car q) (rplacd q p)))
             (declare (fixnum n))))))

(defun traverse-select (n q)
  (declare (fixnum n))
  (do ((n n (the fixnum (1- n)))
       (q (car q) (cdr q)))
      ((= n 0) (car q))
    (declare (fixnum n))))

(defun add (a q)
  (cond ((null q)
	 `(,(let ((x `(,a)))
	      (rplacd x x) x)))
	((null (car q))
	 (let ((x `(,a)))
	   (rplacd x x)
	   (rplaca q x)))
	(t (rplaca q
		   (rplacd (car q) `(,a .,(cdr (car q))))))))

(defun create-structure (n)
  (declare (fixnum n))
  (let ((a `(,(make-node))))
    (do ((m (the fixnum (1- n)) (the fixnum (1- m)))
	 (p a))
	((= m 0) (setq a `(,(rplacd p a)))
	 (do ((unused a)
	      (used (add (traverse-remove 0 a) ()))
	      (x) (y))
	     ((null (car unused))
	      (find-root (traverse-select 0 used) n))
	   (setq x (traverse-remove (mod (the fixnum (traverse-random)) n)
                                    unused))
	   (setq y (traverse-select (mod (the fixnum (traverse-random)) n)
                                    used))
	   (add x used)
	   (setf (node-sons y) `(,x .,(node-sons y)))
	   (setf (node-parents x) `(,y .,(node-parents x))) ))
      (declare (fixnum m))
      (push (make-node) a))))

(defun find-root (node n)
  (do ((n n (the fixnum (1- n))))
      ((= n 0) node)
    (declare (fixnum n))
    (cond ((null (node-parents node))
	   (return node))
	  (t (setq node (car (node-parents node)))))))

(defun travers (node mark)
  (cond ((eq (node-mark node) mark) ())
	(t (setf (node-mark node) mark)
	   (setq count (the fixnum (1+ count)))
	   (setf (node-entry1 node) (not (node-entry1 node)))
	   (setf (node-entry2 node) (not (node-entry2 node)))
	   (setf (node-entry3 node) (not (node-entry3 node)))
	   (setf (node-entry4 node) (not (node-entry4 node)))
	   (setf (node-entry5 node) (not (node-entry5 node)))
	   (setf (node-entry6 node) (not (node-entry6 node)))
	   (do ((sons (node-sons node) (cdr sons)))
	       ((null sons) ())
	     (travers (car sons) mark)))))



(defun traverse (root)
  (let ((count 0))
    (declare (fixnum count))
    (travers root (setq marker (not marker)))
    count))

(defmacro init-traverse()
  '(prog2 (setq root (create-structure 100.)) ()))

(defmacro run-traverse ()
  '(do ((i 50. (the fixnum (1- i))))
      ((= i 0))
     (declare (fixnum i))
    (traverse root)
    (traverse root)
    (traverse root)
    (traverse root)
    (traverse root))) 

;;; to initialize, call:  (init-traverse)

(display-benchmark (init-traverse))

;;; to run traverse, call:  (run-traverse)

(display-benchmark (run-traverse))


;;; DERIV -- This is the Common Lisp version of a symbolic derivative benchmark
;;; written by Vaughn Pratt.  It uses a simple subset of Lisp and does a lot of 
;;; CONSing. 

(defun deriv-aux (a) (list '/ (deriv a) a))

(defun deriv (a)
  (cond 
    ((atom a)
     (cond ((eq a 'x) 1) (t 0)))
    ((eq (car a) '+)	
     (cons '+ (mapcar #'deriv (cdr a))))
    ((eq (car a) '-) 
     (cons '- (mapcar #'deriv 
		      (cdr a))))
    ((eq (car a) '*)
     (list '* 
	   a 
	   (cons '+ (mapcar #'deriv-aux (cdr a)))))
    ((eq (car a) '/)
     (list '- 
	   (list '/ 
		 (deriv (cadr a)) 
		 (caddr a))
	   (list '/ 
		 (cadr a) 
		 (list '*
		       (caddr a)
		       (caddr a)
		       (deriv (caddr a))))))
    (t 'error)))

(defun run ()
  (do ((i 0 (the fixnum (1+ i))))
      ((= i 1000.))	;runs it 5000 times
    (declare (fixnum i))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))

;;; call:  (run)

(display-benchmark (run) "Deriv")


;;; DDERIV -- The Common Lisp version of a symbolic derivative benchmark, written
;;; by Vaughn Pratt.

;;; This benchmark is a variant of the simple symbolic derivative program 
;;; (DERIV). The main change is that it is `table-driven.'  Instead of using a
;;; large COND that branches on the CAR of the expression, this program finds
;;; the code that will take the derivative on the property list of the atom in
;;; the CAR position. So, when the expression is (+ . <rest>), the code
;;; stored under the atom '+ with indicator DERIV will take <rest> and
;;; return the derivative for '+. The way that MacLisp does this is with the
;;; special form: (DEFUN (FOO BAR) ...). This is exactly like DEFUN with an
;;; atomic name in that it expects an argument list and the compiler compiles
;;; code, but the name of the function with that code is stored on the
;;; property list of FOO under the indicator BAR, in this case. You may have
;;; to do something like:

;;; :property keyword is not Common Lisp.

(defmacro defop ((keyword op opclass) lambda-list &body body)
  `(ecase ,keyword
     ((:property)
      (setf (get ',op ',opclass) #'(lambda ,lambda-list ,@body)))))

(defun dderiv-aux (a) 
  (list '/ (dderiv a) a))

(defop (:property + dderiv) (a)
  (cons '+ (mapcar 'dderiv a)))

(defop (:property - dderiv) (a)
  (cons '- (mapcar 'dderiv a)))

(defop (:property * dderiv) (a)
  (list '* (cons '* a)
	(cons '+ (mapcar 'dderiv-aux a))))

(defop (:property / dderiv) (a)
  (list '- 
	(list '/ 
	      (dderiv (car a)) 
	      (cadr a))
	(list '/ 
	      (car a) 
	      (list '*
		    (cadr a)
		    (cadr a)
		    (dderiv (cadr a))))))

(defun dderiv (a)
  (cond 
    ((atom a)
     (cond ((eq a 'x) 1) (t 0)))
    (t (let ((dderiv (get (car a) 'dderiv)))
	 (cond (dderiv (funcall dderiv (cdr a)))
	       (t 'error))))))

(defun run ()
  (do ((i 0 (the fixnum (1+ i))))
      ((= i 1000.))
    (declare (fixnum i))
    (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
    (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))))

;;; call:  (run)

(display-benchmark (run) "DDeriv")

;;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;;; This file contains a recursive as well as an iterative test.

(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))

(EVAL-WHEN (LOAD EVAL)  ;BBNACI
(defvar ll (create-n 200.))
)
(PROCLAIM '(SPECIAL ll))  ;BBNACI

(defun iterative-div2 (l)
  (do ((l l (cddr l))
       (a () (push (car l) a)))
      ((null l) a)))

(defun recursive-div2 (l)
  (cond ((null l) ())
	(t (cons (car l) (recursive-div2 (cddr l))))))

(defun test-1 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (declare (fixnum i))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))

(defun test-2 (l)
  (do ((i 300. (the fixnum (1- i))))
      ((= i 0))
    (declare (fixnum i))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

;;; for the iterative test call: (test-1 ll)

(display-benchmark (test-1 ll) "Iterative Div2")

;;; for the recursive test call: (test-2 ll)

(display-benchmark (test-2 ll) "Recursive Div2")
	      

;;; FFT -- This is an FFT benchmark written by Harry Barrow.
;;; It tests a variety of floating point operations, including array references.

(defvar re (make-array 1025. :element-type 'single-float :initial-element 0.0))	
(defvar im (make-array 1025. :element-type 'single-float :initial-element 0.0))	       

(defun fft					;fast fourier transform
       (areal aimag)				;areal = real part 
  (prog						;aimag = imaginary part
    (ar ai i j k m n le le1 ip nv2 nm1 ur ui wr wi tr ti)
    (setq ar areal				;initialize
          ai aimag
	  n (array-dimension ar 0)
	  n (1- n)
	  nv2 (floor n 2)
	  nm1 (1- n)
	  m 0					;compute m = log(n)
	  i 1)
 l1 (cond ((< i n)
	   (setq m (1+ m)
		 i (+ i i))
	   (go l1)))
    (cond ((not (equal n (expt 2 m)))
	   (princ "error ... array size not a power of two.")
	   (read)
	   (return (terpri))))
    (setq j 1					;interchange elements
	  i 1)					;in bit-reversed order
 l3 (cond ((< i j)
	   (setq tr (aref ar j)
		 ti (aref ai j))
	   (setf (aref ar j) (aref ar i))
	   (setf (aref ai j) (aref ai i))
	   (setf (aref ar i) tr)
	   (setf (aref ai i) ti)))
    (setq k nv2)
 l6 (cond ((< k j) 
	   (setq j (- j k)
		 k (/ k 2))
	   (go l6)))
    (setq j (+ j k)
	  i (1+ i))
    (cond ((< i n)
	   (go l3)))
    (do ((l 1 (1+ l))) ((> l m))			;loop thru stages
	(setq le (expt 2 l)
	      le1 (floor le 2)
	      ur 1.0
	      ui 0.
	      wr (cos (/ pi (float le1)))
	      wi (sin (/ pi (float le1))))
	(do ((j 1 (1+ j))) ((> j le1))		;loop thru butterflies
	    (do ((i j (+ i le))) ((> i n))		;do a butterfly
		(setq ip (+ i le1)
		      tr (- (* (aref ar ip) ur)
			    (* (aref ai ip) ui))
		      ti (+ (* (aref ar ip) ui)
			    (* (aref ai ip) ur)))
		(setf (aref ar ip) (- (aref ar i) tr))
		(setf (aref ai ip) (- (aref ai i) ti))
		(setf (aref ar i) (+ (aref ar i) tr))
		(setf (aref ai i) (+ (aref ai i) ti))))
	(setq tr (- (* ur wr) (* ui wi))
	      ti (+ (* ur wi) (* ui wr))
	      ur tr
	      ui ti))
    (return t)))

;;; the timer which does 10 calls on fft

(defmacro fft-bench ()
  '(do ((ntimes 0 (the fixnum (1+ ntimes))))
      ((= ntimes 10.))
     (declare (fixnum ntimes))
    (fft re im)))

;;; call:  (fft-bench)

(display-benchmark (fft-bench))


;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(eval-when (compile load eval)
  (defconstant size 511.)	
  (defconstant classmax 3.)
  (defconstant typemax 12.))

(defvar iii 0)
(proclaim '(fixnum iii))

(defvar kount 0)
(proclaim '(fixnum kount))

(defvar d 8.)
(proclaim '(fixnum d))

(defvar piececount (make-array (1+ classmax) :initial-element 0))
(proclaim '(type (simple-vector #.(1+ classmax)) piececount))

(defvar class (make-array (1+ typemax) :initial-element 0))
(proclaim '(type (simple-vector #.(1+ typemax)) class))

(defvar piecemax (make-array (1+ typemax) :initial-element 0))
(proclaim '(type (simple-vector #.(1+ typemax)) piecemax))

(defvar puzzle (make-array (1+ size) :initial-element nil))
(proclaim '(type (simple-vector #.(1+ size)) puzzle))

(defvar p (make-array (list (1+ typemax) (1+ size)) :initial-element nil))
(proclaim '(type (simple-array (member t nil) (#.(1+ typemax) #.(1+ size))) p))

(defun fit (i j)
  (declare (fixnum i j))
  (let ((end (aref piecemax i)))
    (declare (fixnum end))
    (do ((k 0 (the fixnum (1+ k))))
	((> k end) t)
      (declare (fixnum k))
      (cond ((the (member t nil) (aref p i k))
	     (cond ((the (member t nil) (aref puzzle (the fixnum (+ j k))))
		    (return nil))))))))

(defun place (i j)
  (declare (fixnum i j))
  (let ((end (aref piecemax i)))
    (declare (fixnum end))
    (do ((k 0 (the fixnum (1+ k))))
        ((> k end))
      (declare (fixnum k))
      (cond ((the (member t nil) (aref p i k)) 
             (setf (aref puzzle (the fixnum (+ j k))) t))))
    (setf (aref piececount (the fixnum (aref class i)))
          (the fixnum (- (the fixnum (aref piececount
                                           (the fixnum (aref class i))))
                         1)))
    (do ((k j (the fixnum (1+ k))))
        ((> k size)
         #|(terpri)
	 (princ "Puzzle filled")|#
         0)
      (declare (fixnum k))
      (cond ((the (member t nil) (not (the (member t nil) (aref puzzle k))))
             (return k))))))

;;;;; ?????
(defun puzzle-remove (i j)
  (declare (fixnum i j))
  (let ((end (aref piecemax i)))
    (declare (fixnum end))
    (do ((k 0 (the fixnum (1+ k))))
        ((> k end))
      (declare (fixnum k))
      (cond ((the (member t nil) (aref p i k))
             (setf (aref puzzle (the fixnum (+ j k)))  nil))))
    (setf (aref piececount (the fixnum (aref class i)))
          (the fixnum (+ (the fixnum (aref piececount 
                                           (the fixnum (aref class i))))
                         1)))))

#|(defun puzzle-remove (i j)
  (let ((end (aref piecemax i)))
    (do ((k 0 (1+ k)))
	((> k end))
      (cond ((aref p i k) (setf (aref puzzle (+ j k))  nil)))
      (setf (aref piececount (aref class i)) (+ (aref piececount (aref class i)) 1)))))|#

(defun trial (j)
  (declare (fixnum j))
  (let ((k 0))
    (declare (fixnum k))
    (do ((i 0 (the fixnum (1+ i))))
        ((> i typemax) (setq kount (the fixnum (1+ kount))) 	 nil)
      (declare (fixnum i))
      (cond ((not (= (the fixnum (aref piececount (aref class i))) 0))
             (cond ((the (member t nil) (fit i j))
                    (setq k (place i j))
                    (cond ((or (the (member t nil) (trial k))
                               (= k 0))
;;;			   (format t "~%Piece ~4D at ~4D." (+ i 1) (+ k 1))
                           (setq kount (the fixnum (+ kount 1)))
                           (return t))
                          (t (puzzle-remove i j))))))))))

(defun definepiece (iclass ii jj kk)
  (declare (fixnum iclass ii jj kk))
  (let ((index 0))
    (declare (fixnum index))
    (do ((i 0 (the fixnum (1+ i))))
	((> i ii))
      (declare (fixnum i))
      (do ((j 0 (the fixnum (1+ j))))
	  ((> j jj))
        (declare (fixnum j))
	(do ((k 0 (the fixnum (1+ k))))
	    ((> k kk))
          (declare (fixnum k))
	  (setq index 
                (the fixnum
                     (+ i
                        (the fixnum (* d
                                       (the fixnum (+ j
                                                      (the fixnum (* d
                                                                     k)))))))))
	  (setf (aref p iii index)  t))))
    (setf (aref class iii) iclass)
    (setf (aref piecemax iii) index) 
    (cond ((not (= iii typemax))
	   (setq iii (the fixnum (+ iii 1)))))))

(defun start ()
  (do ((m 0 (1+ m)))
      ((> m size))
    (declare (fixnum m))
    (setf (aref puzzle m) t))
  (do ((i 1 (1+ i)))
      ((> i 5))
    (declare (fixnum i))
    (do ((j 1 (1+ j)))
	((> j 5))
      (declare (fixnum j))
      (do ((k 1 (1+ k)))
	  ((> k 5))
        (declare (fixnum k))
	(setf (aref puzzle
                    (the fixnum
                         (+ i (the fixnum
                                   (* d (the fixnum
                                             (+ j (the fixnum
                                                       (* d k)))))))))
        nil))))
  (do ((i 0 (1+ i)))
      ((> i typemax))
    (declare (fixnum i))
    (do ((m 0 (1+ m)))
	((> m size))
      (declare (fixnum m))
      (setf (aref p i m)  nil)))
  (setq iii 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)
  
  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)
  
  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)
  
  (definePiece 3 1 1 1)
  
  (setf (aref pieceCount 0) 13.)
  (setf (aref pieceCount 1) 3)
  (setf (aref pieceCount 2) 1)
  (setf (aref pieceCount 3) 1)
  (let ((m (the fixnum (+ 1 (the fixnum (* d (the fixnum (+ 1 d)))))))
	(n 0)(kount 0))
    (declare (fixnum m n))
    (cond ((the (member t nil) (fit 0 m))
           (setq n (place 0 m)))
	  (t (format t "~%Error.")))
    (cond ((the (member t nil) (trial n)) 
	   (format t "~%Success in ~4D trials." kount))
	  (t (format t "~%Failure.")))))

;;; call:  (start)

(display-benchmark (start) "Puzzle")


;;; TRIANG -- Board game benchmark.  

;BBNACI: File had (load eval) but needed compile (as in book)
(eval-when (compile load eval)
  (defvar board (make-array 16. :initial-element 1))
  (proclaim '(simple-vector board))

  (defvar sequence (make-array 14. :initial-element 0))
  (proclaim '(simple-vector sequence))

  (defvar a (make-array 37. :initial-contents '(1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4 4 7 11 8 12
						  13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6 6)))
  (proclaim '(simple-vector a))

  (defvar b (make-array 37. :initial-contents  '(2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5
						   2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5 5)))
  (proclaim '(simple-vector b))

  (defvar c (make-array 37. :initial-contents  '(4 7 11. 8. 12. 13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6
						   1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4 4)))
  (proclaim '(simple-vector c))

  (defvar answer)
  (defvar final)
  (setf (aref board 5) 0))

(defun last-position ()
  (do ((i 1 (1+ i)))
      ((= i 16.) 0)
    (declare (fixnum i))
    (if (= 1 (the fixnum (aref board i)))
	(return i))))

(defun try (i depth)
  (declare (fixnum i depth))
  (declare (function aref (simple-vector fixnum) fixnum))
  (cond ((= depth 14) 
	 (let ((lp (last-position)))
           (declare (fixnum lp))
	   (unless (member lp final)
	     (push lp final)))
	 (push (cdr (coerce sequence 'list)) answer) t)	; this is a hack to replace LISTARRAY
	((and (= 1 (the fixnum (aref board (the fixnum (aref a i)))))
	      (= 1 (the fixnum (aref board (the fixnum (aref b i)))))
	      (= 0 (the fixnum (aref board (the fixnum (aref c i))))))
	 (setf (aref board (the fixnum (aref a i))) 0)
	 (setf (aref board (the fixnum (aref b i))) 0)
	 (setf (aref board (the fixnum (aref c i))) 1)
	 (setf (aref sequence depth) i)
	 (do ((j 0 (1+ j))
	      (depth (1+ depth)))
	     ((or (= j 36.)
		  (the (member t nil) (try j depth))) ())
           (declare (fixnum j)))
	 (setf (aref board (the fixnum (aref a i))) 1) 
	 (setf (aref board (the fixnum (aref b i))) 1)
	 (setf (aref board (the fixnum (aref c i))) 0) ())))

(defun gogogo (i)
  (let ((answer ())
	(final ()))
    (try i 1)))

;;; call:  (gogogo 22.)

(display-benchmark (gogogo 22.) "Triangle")


;;; FPRINT -- Benchmark to print to a file.

(defvar test-atoms '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67 
			      mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12 
			      wxyzab23 xyzabc34 123456ab 234567bc 345678cd 
			      456789de 567890ef 678901fg 789012gh 890123hi))

(defun init-aux (m n atoms)
  (cond ((= m 0) (pop atoms))
	(t (do ((i n (- i 2))
		(a ()))
	       ((< i 1) a)
	     (push (pop atoms) a)
	     (push (init-aux (1- m) n atoms) a)))))

(defun init (m n atoms)
  (let ((atoms (subst () () atoms))) ; Note that this subst does NOT copy -- hence 
                                     ; we take care to avoid circularities
                                     ; in the defvar's and setq's below.
    (do ((a atoms (cdr a)))
	((null (cdr a)) (rplacd a atoms)))
    (init-aux m n atoms)))

(defvar test-pattern)
(setq test-pattern (init 6. 6. test-atoms))

(defun fprint ()
  (if (probe-file "fprint.tst")			; this seems a little wierd, subsequent calls to FPRINT will be slower
      (delete-file "fprint.tst"))
  (let((stream (open "fprint.tst" :direction :output)))  ;defaults to STRING-CHAR
    (print test-pattern stream)
    (close stream)))

;;; call:  (fprint)

(let ((*print-length* nil)
      (*print-level* nil))
  (display-benchmark (fprint)))


;;; FREAD -- Benchmark to read from a file.
;;; Pronounced "FRED".  Requires the existance of FPRINT.TST which is created
;;; by FPRINT.

(defun fread ()
  (let ((stream (open "fprint.tst" :direction :input)))
    (read stream)
    (close stream)))
	    
(eval-when (load eval)
  (if (not (probe-file "fprint.tst"))
      (format t "~%Define FPRINT.TST by running the FPRINT benchmark!")))

;;; call: (fread))

(display-benchmark (fread))


;;; TPRINT -- Benchmark to print and read to the terminal.

(setq test-atoms '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
			  stu0 uvw1 wxy2 xyz3 123a 234b 345c 456d 
			  567d 678e 789f 890g))

(setq test-pattern (init 6. 6. test-atoms))

;;; call:  (print test-pattern)

(let ((*print-level* nil)
      (*print-length* nil))
  (display-benchmark (print test-pattern) "TPrint"))


;;; FRPOLY -- Benchmark from Berkeley based on polynomial arithmetic.
;;; Originally writen in Franz Lisp by Richard Fateman.
;;; PDIFFER1 appears in the code, but is not defined; is not called for in this
;;; test, however.

(proclaim '(ftype (function (t t) t) pdiffer1))

(defvar ans)
(defvar coef)
(defvar f)
(defvar inc)
(defvar i)
(defvar qq)
(defvar ss)
(defvar v)
(defvar *x*)
(defvar *alpha*)
(defvar *a*)
(defvar *b*)
(defvar *chk)
(defvar *l)
(defvar *p)
(defvar q*)
(defvar u*)
(defvar *var)
(defvar *y*)
(defvar r)
(defvar r2)
(defvar r3)
(defvar start)
(defvar res1)
(defvar res2)
(defvar res3)

(defmacro pointergp (x y) `(> (get ,x 'order)(get ,y 'order)))
(defmacro pcoefp (e) `(atom ,e))

(defmacro pzerop (x) 
  `(if (numberp ,x) 					; no signp in CL	
       (zerop ,x)))		      
(defmacro pzero () 0)
(defmacro cplus (x y) `(+ ,x ,y))
(defmacro ctimes (x y) `(* ,x ,y))

(defun pcoefadd (e c x) 
  (if (pzerop c)
      x
      (cons e (cons c x))))

(defun pcplus (c p)
  (if (pcoefp p)
      (cplus p c)
      (psimp (car p) (pcplus1 c (cdr p)))))

(defun pcplus1 (c x)
  (cond ((null x)
	 (if (pzerop c)
	     nil
	     (cons 0 (cons c nil))))
	((pzerop (car x))
	 (pcoefadd 0 (pplus c (cadr x)) nil))
	(t
	 (cons (car x) (cons (cadr x) (pcplus1 c (cddr x)))))))

(defun pctimes (c p) 
  (if (pcoefp p)
      (ctimes c p)
      (psimp (car p) (pctimes1 c (cdr p)))))

(defun pctimes1 (c x)
  (if (null x)
      nil
      (pcoefadd (car x)
		(ptimes c (cadr x))
		(pctimes1 c (cddr x)))))

(defun pplus (x y) 
  (cond ((pcoefp x)
	 (pcplus x y))
	((pcoefp y)
	 (pcplus y x))
	((eq (car x) (car y))
	 (psimp (car x) (pplus1 (cdr y) (cdr x))))
	((pointergp (car x) (car y))
	 (psimp (car x) (pcplus1 y (cdr x))))
	(t
	 (psimp (car y) (pcplus1 x (cdr y))))))

(defun pplus1 (x y)
  (cond ((null x) y)
	((null y) x)
	((= (car x) (car y))
	 (pcoefadd (car x)
		   (pplus (cadr x) (cadr y))
		   (pplus1 (cddr x) (cddr y))))
	((> (car x) (car y))
	 (cons (car x) (cons (cadr x) (pplus1 (cddr x) y))))
	(t (cons (car y) (cons (cadr y) (pplus1 x (cddr y)))))))

(defun psimp (var x)
  (cond ((null x) 0)
	((atom x) x)
	((zerop (car x))
	 (cadr x))
	(t
	 (cons var x))))

(defun ptimes (x y) 
  (cond ((or (pzerop x) (pzerop y))
	 (pzero))
	((pcoefp x)
	 (pctimes x y))
	((pcoefp y)
	 (pctimes y x))
	((eq (car x) (car y))
	 (psimp (car x) (ptimes1 (cdr x) (cdr y))))
	((pointergp (car x) (car y))
	 (psimp (car x) (pctimes1 y (cdr x))))
	(t
	 (psimp (car y) (pctimes1 x (cdr y))))))

(defun ptimes1 (*x* y) 
  (prog (u* v)
	(setq v (setq u* (ptimes2 y)))
     a  
	(setq *x* (cddr *x*))
	(if (null *x*)
	    (return u*))
	(ptimes3 y)
	(go a)))

(defun ptimes2 (y)
  (if (null y)
      nil
      (pcoefadd (+ (car *x*) (car y))
		(ptimes (cadr *x*) (cadr y))
		(ptimes2 (cddr y)))))

(defun ptimes3 (y) 
  (prog (e u c) 
     a1	(if (null y) 
	    (return nil))
	(setq e (+ (car *x*) (car y))
	      c (ptimes (cadr y) (cadr *x*) ))
	(cond ((pzerop c)
	       (setq y (cddr y)) 
	       (go a1))
	      ((or (null v) (> e (car v)))
	       (setq u* (setq v (pplus1 u* (list e c))))
	       (setq y (cddr y))
	       (go a1))
	      ((= e (car v))
	       (setq c (pplus c (cadr v)))
	       (if (pzerop c) 			; never true, evidently
		   (setq u* (setq v (pdiffer1 u* (list (car v) (cadr v)))))
		   (rplaca (cdr v) c))
	       (setq y (cddr y))
	       (go a1)))
     a  (cond ((and (cddr v) (> (caddr v) e))
	       (setq v (cddr v))
	       (go a)))
	(setq u (cdr v))
     b  (if (or (null (cdr u)) (< (cadr u) e))
	    (rplacd u (cons e (cons c (cdr u)))) (go e))
	(cond ((pzerop (setq c (pplus (caddr u) c)))
	       (rplacd u (cdddr u))
	       (go d))
	      (t
	       (rplaca (cddr u) c)))
     e  (setq u (cddr u))
     d  (setq y (cddr y))
	(if (null y)
	    (return nil))
	(setq e (+ (car *x*) (car y))
	      c (ptimes (cadr y) (cadr *x*)))
     c  (cond ((and (cdr u) (> (cadr u) e))
	       (setq u (cddr u))
	       (go c)))
	(go b))) 

(defun pexptsq (p n)
  (do ((n (floor n 2) (floor n 2))
       (s (if (oddp n) p 1)))
      ((zerop n) s)
    (setq p (ptimes p p))
    (and (oddp n) (setq s (ptimes s p)))))

(eval-when (load eval)
  (setf (get 'x 'order) 1)
  (setf (get 'y 'order) 2)
  (setf (get 'z 'order) 3)
  (setq r (pplus '(x 1 1 0 1) (pplus '(y 1 1) '(z 1 1)))	; r= x+y+z+1
	r2 (ptimes r 100000)				 	; r2 = 100000*r
	r3 (ptimes r 1.0)))					; r3 = r with floating point coefficients	

;;; four sets of three tests, call:
;;; (pexptsq r 2) (pexptsq r2 2) (pexptsq r3 2)

(display-benchmark (pexptsq r 2) "FRPoly2r")
(display-benchmark (pexptsq r2 2) "FRPoly2r2")
(display-benchmark (pexptsq r3 2) "FRPoly2r3")

;;; (pexptsq r 5) (pexptsq r2 5) (pexptsq r3 5)

(display-benchmark (pexptsq r 5) "FRPoly5r")
(display-benchmark (pexptsq r2 5) "FRPoly5r2")
(display-benchmark (pexptsq r3 5) "FRPoly5r3")

;;; (pexptsq r 10) (pexptsq r2 10) (pexptsq r3 10)

(display-benchmark (pexptsq r 10) "FRPoly10r")
(display-benchmark (pexptsq r2 10) "FRPoly10r2")
(display-benchmark (pexptsq r3 10) "FRPoly10r3")

;;; (pexptsq r 15) (pexptsq r2 15) (pexptsq r3 15)

(display-benchmark (pexptsq r 15) "FRPoly15r")
(display-benchmark (pexptsq r2 15) "FRPoly15r2")
(display-benchmark (pexptsq r3 15) "FRPoly15r3")


#|
;;; Not part of the published benchmarks.
(defun trtak (x y z) (declare (fixnum x y z))
  (prog ()
     tak
	(if (not (< y x))
	    (return z)
	    (let ((a (tak (the fixnum (1- x)) y z))
		  (b (tak (the fixnum (1- y)) z x)))
	      (setq z (tak (the fixnum (1- z)) x y)
		    x a
		    y b)
	      (go tak)))))


;;; call: (trtak 18. 12. 6.)

(display-benchmark (trtak 18. 12. 6.))
|#
