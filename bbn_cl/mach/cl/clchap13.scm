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
;; Characters

;; check char-code-limit

(proclaim '(insert-touches t))

(export '(char-code-limit char-font-limit char-bits-limit standard-char-p graphic-char-p 
			  string-char-p alpha-char-p upper-case-p lower-case-p
			  both-case-p digit-char-p alphanumericp char= digit-char char/= char<
			  char> char<= char>= char-equal char-not-equal char-lessp 
			  char-greaterp char-not-greaterp char-not-lessp
			  char-code char-bits char-font code-char make-char character 
			  char-upcase char-downcase char-int int-char char-name name-char
			  char-control-bit char-meta-bit char-super-bit char-hyper-bit char-bit
			  set-char-bit))

(defconstant char-code-limit 128)

(defconstant char-font-limit 1)

(defconstant char-bits-limit 16)

;;13.2 Predicates on Characters

(defun standard-char-p (char)
  (and (char? char) 
       (zerop (the fixnum (char-bits char)))))

(defconstant other-graphic-chars
  (char-set #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\, #\: #\; #\< #\=
	    #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~ #\+ #\- #\. #\/ 
	    #\space))

(defun graphic-char-p (char)
  (or (char-alphanumeric? char)
      (char-set-member? other-graphic-chars char)))

(cl-define string-char-p #'standard-char-p)

(cl-define alpha-char-p char-alphabetic?)

(cl-define upper-case-p char-upper-case?)

(cl-define lower-case-p char-lower-case?)

(cl-define both-case-p  char-alphabetic?)

;;  If char is a digit in the specified radix, returns the fixnum for
;;  which that digit stands, else returns NIL.  Radix defaults to 10
;;  (decimal).

(defun digit-char-p (char &optional (radix 10.))
  (and (zerop (the fixnum (char-bits char)))
       (let ((m (- (char-code char)
		   (char-code #\0))))
	 (cond ((<= radix 10.)
		;; Special-case decimal and smaller radices.
		(if (and (>= m 0) (< m radix))  m  nil))
	       ;; Cannot handle radix past Z.
	       ((> radix 36)
		(error "Digit-char-p: radix ~A too large."  radix))
	       ;; Digits 0 - 9 are used as is, since radix is larger.
	       ((and (>= m 0) (< m 10)) m)
	       ;; Check for upper case A - Z.
	       ((and (>= (setq m (- m 7)) 10) (< m radix)) m)
	       ;; Also check lower case a - z.
	       ((and (>= (setq m (- m 32)) 10) (< m radix)) m)
	       ;; Else, fail.
	       (t nil)))))	       
				
(cl-define alphanumericp char-alphanumeric?)

(cl-define char=
  (let ()
    (cl-define (char=2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char=? character (car more-characters))
	       (char=2 character (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char=2 character more-characters))))

(defun char/= (character &rest more-characters)
  (do* ((head character (car nlist))
	(nlist more-characters (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (unless (do* ((nl nlist (cdr nl)))
		  ((atom nl) T)
	       (declare (list nl))
	       (if (char=? head (car nl)) (return nil)))
       (return nil))))

(cl-define char<
  (let ()
    (cl-define (char<2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char<? character (car more-characters))
	       (char<2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char<2 character more-characters))))

(cl-define char>
  (let ()
    (cl-define (char>2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char>? character (car more-characters))
	       (char>2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char>2 character more-characters))))

(cl-define char<=
  (let ()
    (cl-define (char<=2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char<=? character (car more-characters))
	       (char<=2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char<=2 character more-characters))))

(cl-define char>=
  (let ()
    (cl-define (char>=2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char>=? character (car more-characters))
	       (char>=2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char>=2 character more-characters))))

(cl-define char-equal
  (let ()
    (cl-define (char-equal2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char-ci=? character (car more-characters))
	       (char-equal2 character (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char-equal2 character more-characters))))

(defun char-not-equal (character &rest more-characters)
  (do* ((head character (car nlist))
	(nlist more-characters (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (unless (do* ((nl nlist (cdr nl)))
		  ((atom nl) T)
	       (declare (list nl))
	       (if (char-ci=? head (car nl)) (return nil)))
       (return nil))))

(cl-define char-lessp
  (let ()
    (cl-define (char-lessp2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char-ci<? character (car more-characters))
	       (char-lessp2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char-lessp2 character more-characters))))

(cl-define char-greaterp
  (let ()
    (cl-define (char-greaterp2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char-ci>? character (car more-characters))
	       (char-greaterp2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char-greaterp2 character more-characters))))

(cl-define char-not-greaterp
  (let ()
    (cl-define (char-not-greaterp2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char-ci<=? character (car more-characters))
	       (char-not-greaterp2
		(car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char-not-greaterp2 character more-characters))))

(cl-define char-not-lessp
  (let ()
    (cl-define (char-not-lessp2 character more-characters)
      (if (null? more-characters)
	  t
	  (and (char-ci>=? character (car more-characters))
	       (char-not-lessp2 (car more-characters) (cdr more-characters)))))
    (cl-lambda (character &rest more-characters)
	       (char-not-lessp2 character more-characters))))

;;13.3 Character Construction and Selection

;; Inherit char-bits and char-code from scheme.

(defun char-font (char)
  (if (char? char)
      0
      (error "char-font: ~A not a character" char)))

(cl-define scm-make-char make-char)

(defun code-char (code &optional (bits 0) (font 0))
  (if (and (>= code 0)
	   (< code char-code-limit)
	   (>= bits 0)
	   (< bits char-bits-limit)
	   (= font 0))
      (scm-make-char code bits)
      nil))

(defun make-char (char &optional (bits 0) (font 0))
  (if (and (char? char)
	   (>= bits 0)
	   (< bits char-bits-limit)
	   (= font 0))
      (scm-make-char (char-code char) bits)
      nil))

;;13.4 Character Conversions

;;  Coerces its argument into a character object if possible.  Accepts
;;  characters, strings and symbols of length 1, and integers.

(defun character (object)
  (cond ((char? object) object)
	((stringp object)
	 (if (= 1 (length object))
	     (aref object 0)
	     (error "Character: ~A is not of length one" object)))
	((symbolp object) (character (symbol-name object)))
	((integerp object) (code-char object))
	(t
	 (error "Character: cannot coerce ~A to a character." object))))

;; Char-upcase and downcase inherited from scheme

;;  All arguments must be integers.  Returns a character object that
;;  represents a digit of the given weight in the specified radix.  Returns
;;  NIL if no such character exists.  The character will have the specified
;;  font attributes."
(defun digit-char (weight &optional (radix 10) (font 0))
  (and (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight))
		  0 font)))

(cl-define char-int char->integer)

(defconstant top-character-start (char->integer #\t-null))  

(defun int-char (integer)
  (if (< integer top-character-start)  
      (integer->char integer)
      nil))

(cl-define char-name
  (let ()
    (cl-define rassoc
      (association-procedure eqv? cdr))
    (cl-lambda (char)
	       (car (rassoc char (access named-codes character-package))))))

(cl-define name-char
  (let ()
    (cl-define assoc
      (association-procedure string-ci=? car))
    (cl-lambda (name)
	       (cdr (assoc (string name) (access named-codes character-package))))))

;;13.5 Character Control-Bit Functions

(defconstant char-control-bit 1)

(defconstant char-meta-bit 2)

(defconstant char-super-bit 4)

(defconstant char-hyper-bit 8)

(defun char-bit (char name)
  (cond ((eq? name :control)
	 (control-bit? (char-bits char)))
	((eq? name :meta)
	 (meta-bit? (char-bits char)))
	((eq? name :super)
	 (super-bit? (char-bits char)))
	((eq? name :hyper)
	 (hyper-bit? (char-bits char)))
	(else
	 (error "~A is an unknown char bit" name))))

(defun modify-bits (bits weight newvalue)
  (declare (fixnum bits weight))
  (if newvalue
      (if (oddp (ash bits weight))
	  bits
	  (the fixnum (+ bits weight)))
      (if (oddp (ash bits weight))
	  (the fixnum (- bits weight))
	  bits)))

(defun set-char-bit (char name newvalue)
  (let ((code (char-code char))
	(bits (char-bits char)))
    (scm-make-char code
		   (modify-bits bits
				(cond ((eq name :control) char-control-bit)
				      ((eq name :meta)    char-meta-bit)
				      ((eq name :super)   char-super-bit)
				      ((eq name :hyper)   char-hyper-bit)
				      (error "~A is an unknown char bit" name))
				newvalue))))

