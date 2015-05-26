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

(export '(byte byte-position byte-size
	       ldb ldb-test dpb mask-field
	       deposit-field
	       make-random-state *random-state*
	       random random-state random-state-p))

;;;================================================================
;;; 12.8 Byte Manipulation Functions
;;;================================================================

;;;================
;;; BYTE, BYTE-SIZE, BYTE-POSITION
;;;================
(defstruct (byte-specifier
	     (:conc-name byte-)
	     (:constructor byte (size position))
	     ;;(:type list)
	     :named)
  (size 0 :type integer)
  (position 0 :type integer))

;;;================
;;;  LDB
;;;================

(defun ldb (bytespec integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (logand (ash integer (- (byte-position bytespec)))
	  (1- (ash 1 (byte-size bytespec)))))

;;;================
;;; LDB-TEST
;;;================
;;;"Returns T if any of the specified bits in integer are 1's."

(defun ldb-test (bytespec integer)
  (declare (function ldb (t integer) integer))
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (not (zerop (ldb bytespec integer))))

;;;================
;;;  MASK-FIELD
;;;================
;;;"Extract the specified byte from integer,  but do not right justify result."

(defun mask-field (bytespec integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (logand (dpb -1 bytespec 0) integer))

;;;================
;;; DPB
;;;================
(defun dpb (newbyte bytespec integer)
  (check-type newbyte integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (let* ((size (byte-size bytespec))
	 (position (byte-position bytespec))
	 (mask (1- (ash 1 size))))
    (logior (logandc2 integer (ash mask position))	;need to clear out the bits
	    (ash (logand newbyte mask) position))))


;;;================
;;; DEPOSIT-FIELD
;;;================
;;;"Returns new integer with newbyte in specified position, newbyte is not right justified."

(defun deposit-field (newbyte bytespec integer)
  (check-type newbyte integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (let* ((size (byte-size bytespec))
	 (position (byte-position bytespec))
	 (mask (ash (1- (ash 1 size)) position)))
    (logior (logandc2 integer mask)		;clear out bits for the new ones
	    (logand newbyte mask))))


;;;================================================================
;;; 12.9 Random Numbers
;;;================================================================


;;;================
;;; RANDOM STATE OBJ
;;;================
(defvar *random-state*)

(defconstant random-const-a 8373)
(defconstant random-const-c 101010101)
(defconstant random-upper-bound 134217726)
(defconstant random-max 54)
(defvar rand-seed 0)

(defstruct (random-state (:constructor make-random-object))
  (j 24 :type integer)
  (k 0 :type integer)
  (seed (make-array (1+ random-max) :initial-contents
		    (do ((list-rands () (cons (rand1) list-rands))
			 (i 0 (1+ i)))
			((> i random-max) list-rands)))
	:type '(simple-vector * fixnum)))


;;; Generates a random number from rand-seed.
(defun rand1 ()
   (setq rand-seed (mod (+ (* rand-seed random-const-a) random-const-c)
			(1+ random-upper-bound))))

;;; rand3  --  Internal
;;;
;;; This function generates fixnums between 0 and random-upper-bound, 
;;; inclusive For the algorithm to work random-upper-bound must be an 
;;; even positive fixnum.  State is the random state to use.
;;;
(defun rand3 (state)
  (let ((seed (random-state-seed state))
	(j (random-state-j state))
	(k (random-state-k state)))
    (declare (fixnum j k) (type (simple-vector * fixnum) seed))
    (setf (svref seed k)
	  (let ((a (- random-upper-bound
		      (svref seed
			      (setf (random-state-j state)
				    (if (= j 0) random-max (1- j))))
		      (svref seed
			      (setf (random-state-k state)
				    (if (= k 0) random-max (1- k)))))))
	    (if (minusp a) (- a) (- random-upper-bound a))))))

(defun copy-state (cur-state)
  (let ((state (make-random-object
		:seed (make-array 55)
		:j (random-state-j cur-state)
		:k (random-state-k cur-state))))
    (do ((i 0 (1+ i)))
	((= i 55) state)
      (declare (fixnum i))
      (setf (aref (random-state-seed  state) i)
	    (aref (random-state-seed cur-state) i)))))


;;;================
;;; RANDOM
;;;================ 

(proclaim '(ftype (function (t) fixnum) rand3))

(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (typecase arg
    (fixnum
     (unless (plusp (the fixnum arg))
       (error "Non-positive argument, ~A, to RANDOM." arg))     
     (rem (the fixnum (rand3 state)) (the fixnum arg)))
    (float
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (let ((arg-length (typecase arg
			 (short-float %short-float-mantissa-length)
			 (single-float %single-float-mantissa-length)
			 (double-float %double-float-mantissa-length)
			 (long-float %long-float-mantissa-length))))
       (* arg (/ (random (ash 2 arg-length) state)
		 (ash 2 arg-length)))))
    (integer
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (do ((tot (rand3 state) (+ (ash tot %fixnum-length) (rand3 state)))
	  (end (ash arg (- %fixnum-length))
	       (ash end (- %fixnum-length))))
	 ((zerop end) (mod tot arg))))
    (t (error "Wrong type argument, ~A, to RANDOM." arg))))


;;;================
;;; MAKE-RANDOM-STATE
;;;================
;;;  "Make a random state object.  If State is not supplied, return a copy
;;;  of the default random state.  If State is a random state, then return a
;;;  copy of it.  If state is T then return a random state generated from
;;;  the universal time."
(defun make-random-state (&optional state)
  (cond ((not state) (copy-state *random-state*))
	((random-state-p state) (copy-state state))
	((eq state t) (setq rand-seed (get-universal-time))
		      (make-random-object))
	(t (error "Bad argument, ~A, for RANDOM-STATE." state))))


;;;================
;;; Random-state-p
;;;================

;;;Defined in the defstruct

;;; Initialization

(defun random-init ()
  (setq *random-state*
	(make-random-object :seed
	 (make-array (1+ random-max) :initial-contents
		     '(45117816 133464727 86324180 99419799 68851957 87250180
		      52971860 84081967 30854110 121122797 70449044 18801152
		      45149898 15881380 27398356 117706009 49915564 80620628
		      120974070 98193932 43883764 53717012 100954825 82579490
		      17280729 118523949 42282975 127220348 6288263 56575578
		      2474156 47934425 561006 21989698 74046730 105055318
		      113363907 48749716 78183593 109613585 37323232 65101428
		      46453209 76906562 5371267 86544820 33922642 60765033
		      41889257 77176406 38775255 78514879 72553872 66916641
		      100613180)))))

(random-init)
