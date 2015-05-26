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
;; Chapter 18 -- Strings

(proclaim '(insert-touches nil))

(export '(char schar
	  string= string-equal string< string> string<= string>= string/=
	  string-lessp string-greaterp string-not-lessp string-not-greaterp
	  string-not-equal
	  make-string
	  string-trim string-left-trim string-right-trim
	  string-upcase
	  string-downcase string-capitalize nstring-upcase nstring-downcase
	  nstring-capitalize
	  string))

(defun char (string index)
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (if (stringp string)
      (aref string index)
      (error "Argument to char is not a string: ~A" string)))

(defun %charset (string index new-el)
  (if (stringp string)
      (setf (aref string index) new-el)
      (error "Argument to %charset is not a string: ~A" string)))

(generate-type-optimizer
 (char string index)
 (#t #t)
 ((((simple-string string) (* index)) * string-char (string-ref string index) #t)))

(generate-type-optimizer
 (%charset string index new-el)
 (#t #t #f)
 ((((simple-string string) (* index) (* new-el)) * string-char (string-set! string index new-el) #t)))

(cl-define schar string-ref)

(cl-define %scharset string-set!)

;;; Some string comparison utilities

(defun string-equality-compare (string1 string2 start1 end1 start2 end2 compare-function)
  (declare (fixnum start1 end1 start2 end2))
  (if (= start1 end1)
      t
      (and (funcall compare-function (char string1 start1) (char string2 start2))
	   (string-equality-compare string1 string2 (1+ start1) end1 (1+ start2) end2 compare-function))))

;;; When doing a lexicographical compare, the assumption is that the two strings are
;;; equal so far.

(defun string-lex-compare (string1 string2 start1 end1 start2 end2
			   compare-function equality-function
			   if-exhausted)
  (declare (fixnum start1 end1 start2 end2))
  (cond ((or (= start1 end1)
	     (= start2 end2))
	 (funcall if-exhausted start1 end1 start2 end2))
	((funcall equality-function 
		  (char string1 start1)
		  (char string2 start2))
	 (string-lex-compare string1 string2
			     (1+ start1) end1
			     (1+ start2) end2
			     compare-function equality-function
			     if-exhausted))
	((funcall compare-function
		  (char string1 start1)
		  (char string2 start2))
	 start1)
	(else nil)))

(defmacro make-string-equality-compare (comparison)
  `(progn
     (if (symbolp string1) (setq string1 (symbol-name string1)))
     (if (symbolp string2) (setq string2 (symbol-name string2)))
     (if (not end1) (setq end1 (length string1)))
     (if (not end2) (setq end2 (length string2)))
     (let ((slen1 (the fixnum (- end1 start1)))
	   (slen2 (the fixnum (- end2 start2))))
       (declare (fixnum slen1 slen2))
       (cond ((or (minusp slen1) (minusp slen2))
	      ;;prevent endless looping later.
	      (error "Improper bounds for string comparison."))
	     ((not (= slen1 slen2))
	      nil)
	     (else (string-equality-compare string1 string2 start1 end1 start2 end2 ,comparison))))))

(defmacro make-string-lex-compare (comparison equality-function if-exhausted)
  `(progn
     (if (symbolp string1) (setq string1 (symbol-name string1)))
     (if (symbolp string2) (setq string2 (symbol-name string2)))
     (if (not end1) (setq end1 (length string1)))
     (if (not end2) (setq end2 (length string2)))
     (string-lex-compare string1 string2
			 start1 end1
			 start2 end2
			 ,comparison ,equality-function
			 ,if-exhausted)))

'$split-file

;;; String equality

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-equality-compare #'char=))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-equality-compare #'char-equal))

;;; String case-sensitive comparison

(defun less-exhausted (start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (< (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
      start1
      nil))

(defun greater-exhausted (start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (> (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
      start2
      nil))

(defun less-or-equal-exhausted (start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (<= (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
      start1
      nil))

(defun greater-or-equal-exhausted (start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (>= (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
      start2
      nil))

(defun not-equal-exhausted (start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (= (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
      nil
      start1))

'$split-file

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char< #'char= #'less-exhausted))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char> #'char= #'greater-exhausted))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char< #'char= #'less-or-equal-exhausted))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char> #'char= #'greater-or-equal-exhausted))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char/= #'char= #'not-equal-exhausted))

'$split-file

;;; String case-insensitive comparison

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char-lessp #'char-equal #'less-exhausted))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char-greaterp #'char-equal #'greater-exhausted))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char-not-greaterp #'char-equal #'less-or-equal-exhausted))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char-not-lessp #'char-equal #'greater-or-equal-exhausted))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (make-string-lex-compare #'char-not-equal #'char-equal #'not-equal-exhausted))

'$split-file

;;; String Construction and Manipulation

(defun make-string (count &key ((:initial-element fill-char) #\space))  ; #\null doesn't work yet
  ((access make-string ()) count fill-char))

;;; Some auxiliary procedures for calculating trim indicies

(defun get-left-index (string index stop char-bag)
  (declare (fixnum index stop))
  (if (or (= index stop)
	  (not (find (char string index) char-bag)))
      index
      (get-left-index string (1+ index) stop char-bag)))

(defun get-right-index (string index stop char-bag)
  (declare (fixnum index stop))
  (if (or (<= index stop)
	  (not (find (char string index) char-bag)))
      (the fixnum (1+ index))
      (get-right-index string (-1+ index) stop char-bag)))

'$split-file

;;; String-Trim procedures

(defun string-left-trim (char-bag string)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (subseq string (get-left-index string 0 slen char-bag) slen)))

(defun string-right-trim (char-bag string)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (declare (fixnum slen))
    (subseq string 0 (get-right-index string (the fixnum (-1+ slen)) 0 char-bag))))

(defun string-trim (char-bag string)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (declare (fixnum slen))
    (subseq string 
	    (get-left-index string 0 slen char-bag)
	    (get-right-index string (the fixnum (-1+ slen)) 0 char-bag))))

;;; Some auxiliary procedures for copying or modifying strings

(defun string-copy-loop (index stop source-string dest-string modifier)
  (declare (fixnum index stop))
  (if (< index stop)
      (begin (setf (aref dest-string index) (funcall modifier (char source-string index)))
	     (string-copy-loop (1+ index) stop source-string dest-string modifier))
      dest-string))

(defun string-capitalize-loop (index stop source-string dest-string newword)
  (declare (fixnum index stop))
  (if (>= index stop)
      dest-string
      (let ((char (char source-string index)))
	(cond ((not (alphanumericp char))
	       (setf (aref dest-string index) char)
	       (string-capitalize-loop (1+ index) stop source-string dest-string t))
	      (newword
	       (setf (aref dest-string index) (char-upcase char))
	       (string-capitalize-loop (1+ index) stop source-string dest-string nil))
	      (else
	       (setf (aref dest-string index) (char-downcase char))
	       (string-capitalize-loop (1+ index) stop source-string dest-string nil))))))

;;; String Case procedures

(defun string-upcase (string &key (start 0) end)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (let ((newstring (make-string slen)))  ;; The new string is guarenteed to be simple
      (if (not end) (setq end slen))
      (string-copy-loop 0 start string newstring identity-procedure)
      (string-copy-loop start end string newstring #'char-upcase)
      (string-copy-loop end slen string newstring identity-procedure))))

(defun string-downcase (string &key (start 0) end)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (let ((newstring (make-string slen)))  ;; The new string is guarenteed to be simple
      (if (not end) (setq end slen))
      (string-copy-loop 0 start string newstring identity-procedure)
      (string-copy-loop start end string newstring #'char-downcase)
      (string-copy-loop end slen string newstring identity-procedure))))

(defun string-capitalize (string &key (start 0) end)
  (if (symbolp string) (setq string (symbol-name string)))
  (let ((slen (length string)))
    (let ((newstring (make-string slen)))  ;; The new string is guarenteed to be simple
      (if (not end) (setq end slen))
      (string-copy-loop 0 start string newstring identity-procedure)
      (string-capitalize-loop start end string newstring t)
      (string-copy-loop end slen string newstring identity-procedure))))

(defun nstring-upcase (string &key (start 0) end)
  (let ((slen (length string)))
    (if (not end) (setq end slen))
    (string-copy-loop start end string string #'char-upcase)))

(defun nstring-downcase (string &key (start 0) end)
  (let ((slen (length string)))
    (if (not end) (setq end slen))
    (string-copy-loop start end string string #'char-downcase)))

(defun nstring-capitalize (string &key (start 0) end)
  (let ((slen (length string)))
    (if (not end) (setq end slen))
    (string-capitalize-loop start end string string t)))

;;; String coercion

(defun string (X)
  "Coerces X into a string.  If X is a string, X is returned.  If X is a
   symbol, X's pname is returned.  If X is a character then a one element
   string containing that character is returned.  If X cannot be coerced
   into a string, an error occurs."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	((characterp x)
	 (let ((res (make-string 1)))
	   (setf (schar res 0) x) res))
	(t
	 (error "~S cannot be coerced to a string." x))))

