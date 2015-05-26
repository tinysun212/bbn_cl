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

(export '(ash boole boole-1 boole-2 boole-and
	  boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr boole-eqv
	  boole-ior boole-nand boole-nor boole-orc1 boole-orc2 boole-set
	  boole-xor
	  logand logandc1 logandc2 logbitp
	  logcount logeqv logior lognand lognor lognot logorc1 logorc2
	  logtest logxor
	  integer-length))

;;;================================================================
;;; 12.7 Logical Operations of Numbers
;;;================================================================

;;;================
;;; BOOLE & Constants
;;;================

(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)

(cl-define boole (make-primitive-procedure 'boole))

(defun logior (&rest args)
  (let ((r 0))
    (dolist (x args)
	    (setq r (boole boole-ior x r)))
    r))

(defun logxor (&rest args)
  (let ((r 0))
    (dolist (x args)
	    (setq r (boole boole-xor x r)))
    r))

(defun logand (&rest args)
  (let ((r -1))
    (dolist (x args)
	    (setq r (boole boole-and x r)))
    r))

(defun logeqv (&rest args)
  (let ((r -1))
    (dolist (x args)
	    (setq r (boole boole-eqv x r)))
    r))

(defun lognand (n1 n2)
  (boole boole-nand n1 n2))

(defun lognor (n1 n2)
  (boole boole-nor n1 n2))

(defun logandc1 (n1 n2)
  (boole boole-andc1 n1 n2))

(defun logandc2 (n1 n2)
  (boole boole-andc2 n1 n2))

(defun logorc1 (n1 n2)
  (boole boole-orc1 n1 n2))

(defun logorc2 (n1 n2)
  (boole boole-orc2 n1 n2))

(defun lognot (n)
  (boole boole-c1 n 0))

(defun logtest (n1 n2)
  (not (zero? (logand n1 n2))))



;;;================
;;;  LOGBITP
;;;================
(defun logbitp (index integer)
  (check-type index integer)
  (check-type integer integer)
  (logtest integer (ash 1 index)))


;;;================
;;;  ASH
;;;================

(cl-define ash (make-primitive-procedure 'ash))

;;;================
;;;  LOGCOUNT
;;;================
;;; Logcount returns the number of bits that are the complement of 
;;; the sign in the integer argument x.

;;;This will do until C work appears needed.

(defun logcount (x) 
  (check-type x integer)
  (if (minusp x) (setq x (lognot x)))
  (do ((n x (logand n (1- n)))
       (cnt 0 (1+ cnt)))
      ((zerop n) cnt)
    (declare (fixnum n cnt))))

;;;================
;;; INTEGER-LENGTH
;;;================

(cl-define integer-length (make-primitive-procedure 'integer-length))

