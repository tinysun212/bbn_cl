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
(declare (usual-integrations)
	 (integrate-operator svr svs!))

(define (svr v i)
  (declare (integrate v i))
  (system-vector-ref v i))

(define (svs! v i x)
  (declare (integrate v i x))
  (system-vector-set! v i x))

;;;
;;; Read table definitions
;;;

;;; IMPORTANT!!! This file cannot be split, because we must retain the ability
;;;              to call make-readtable and make a new env, as below, to get functions
;;;              closed properly.

(set! make-readtable
      (named-lambda (make-readtable)
	(let ((readtable
	       (make-environment

(define :self)
(define :type "Readtable")
(define :print-self
  (lambda () 
    (unparse-with-brackets
     (lambda ()
       (write-string "Readtable ")
       (write (primitive-datum :self))))))
(define base-parser-table (vector-cons 256 #f))
(define macro-parser-table (vector-cons 256 #f))
(define user-parser-table (vector-cons 256 #f))
(define dispatch-parser-table (vector-cons 256 #f))
(define system-#-dispatch-vector (vector-cons 256 #f))
(define user-#-dispatch-vector (vector-cons 256 #f))
(define %whitespace 0)
(define %non-term-macro 1)
(define %term-macro 2)
(define %single-esc 3)
(define %multi-esc 4)
(define %pkg-marker 5)        ; slight deviation from CLtL
(define %constituent 6)
(define char-sets
  (vector
   ;; whitespace
   (char-set #\tab #\space #\page #\return #\linefeed)
   ;; non-term-macro
   (char-set #\#)
   ;; term-macro
   (char-set #\" #\' #\( #\) #\, #\; #\`)
   ;; single-esc
   (char-set #\\)
   ;; multi-esc
   (char-set #\|)
   ;; pkg-marker
   (char-set #\:)
   ;; constituent
   '()))
(define escapes)
(define atom-delimiters)
(define atom-terminators)
(define non-whitespace)
(define string-delimiters)
(define comment-delimiters (char-set #\newline))
(define digits (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define char-delimiters)
(define comment-leaders (char-set #\# #\|))
(define number-leaders)

(define (set-derived-char-sets!)
  (set! escapes
	(char-set-union
	 (svr char-sets %single-esc)
	 (svr char-sets %multi-esc)))
  (set! atom-terminators
	(char-set-union
	 (svr char-sets %whitespace)
	 (svr char-sets %term-macro)))
  (set! atom-delimiters
	(char-set-union
	 atom-terminators
	 (char-set-union
	  (svr char-sets %pkg-marker)
	  escapes)))
  (set! non-whitespace 
	(char-set-invert (svr char-sets %whitespace)))
  (set! string-delimiters
	(char-set-union (char-set #\")
			(svr char-sets %single-esc)))
  (set! char-delimiters (char-set-union (char-set #\-) atom-delimiters))
  (svs! char-sets %constituent
	(let ((constituent-chars (char-set-invert (char-set))))
	  (do ((i %whitespace (1+ i)))
	      ((= i %constituent) constituent-chars)
	    (set! constituent-chars (char-set-difference 
				     constituent-chars
				     (svr char-sets i))))
	  (char-set-union
	   constituent-chars escapes)))
  (set! number-leaders
	(char-set-union
	 digits
	 (char-set #\+ #\- #\.))))
    (set-derived-char-sets!)

(define (:char-type c)
  (do ((i %whitespace (1+ i)))
      ((char-set-member? (svr char-sets i) c)
       i)))

(define (set-char-type! c new-type)
  ;;
  ;; To do: deal with parser tables
  ;;
  (let ((ctype (:char-type c))
	(cs (char-set c)))
    ;; First, remove char from its current set
    (svs! char-sets ctype
	  (char-set-difference 
	   (svr char-sets ctype) 
	   cs))
    ;; Next, install the character in the target set
    (svs! char-sets new-type
	  (char-set-union
	   (svr char-sets new-type)
	   cs))
    ;; Last, reset the derived sets
    (set-derived-char-sets!)))

(define eol-object (list '()))

(define within-list? #f)

(define (:parse-object)
  (let ((char (*parser-read-char*)))
    (if char
	(let ((code (or (char-ascii? char)
			(non-ascii-error))))
	  ((or (svr user-parser-table code)
	       (svr macro-parser-table code)
	       (svr base-parser-table code))
	   *parser-input-stream*
	   char))
	eof-object)))

(define (:set-syntax-from-char from-char to-char from-readtable)
  (set-char-type! to-char ((access :char-type from-readtable) from-char)))

(define (:set-macro-character char fcn non-term?)
  (let ((code (char->ascii char)))
    (if (or (eq? fcn (svr macro-parser-table code))
	    (eq? fcn (svr base-parser-table code)))
	(svs! user-parser-table code #f)
	(svs! user-parser-table code fcn))
    (set-char-type! char (if non-term? %non-term-macro %term-macro))))

(define (:get-macro-character char)
  (let ((code (char->ascii char)))
    (let ((fcn (or (svr user-parser-table code)
		   (svr macro-parser-table code)))
	  (non-term? (= (:char-type char) %non-term-macro)))
      (prim-values fcn non-term?))))

(define (:make-dispatch-macro-character char non-term?)
  (let ((code (char->ascii char)))
    (if (or (svr user-parser-table code)
	    (svr macro-parser-table code))
	(error "make-dispatch-macro-character: character ~a is already a macro character")
	(begin
	  (if (char=? char #\#)
	      (svs! macro-parser-table code dispatch-#-char)
	      (begin
		(svs! macro-parser-table code dispatch-a-char)
		(svs! dispatch-parser-table code (vector-cons 256 #f))))
	  (set-char-type! char (if non-term? %non-term-macro %term-macro))))))

(define (:set-dispatch-macro-character disp-char sub-char fcn)
  (if (char-set-member? digits sub-char)
      (error "Dispatch sub-char ~a cannot be a digit" sub-char)
      (if (char=? disp-char #\#)
	  (:set-#-dispatch-macro-character sub-char fcn #f)
	  (let ((dispatch-vector (svr dispatch-parser-table (char->ascii disp-char))))
	    (if (null? dispatch-vector)
		(error "Character ~a not defined as a dispatch character" disp-char)
		(svs! dispatch-vector (char->ascii (char-upcase sub-char)) fcn))))))

(define (:set-system-#-dispatch-macro-character sub-char fcn)
  (:set-#-dispatch-macro-character sub-char fcn #t))

(define (:set-#-dispatch-macro-character sub-char fcn system?)
  (let ((code (char->ascii (char-upcase sub-char))))
    (if system?
	(begin
	  (svs! user-#-dispatch-vector code #f)
	  (svs! system-#-dispatch-vector code fcn))
	(if (eq? fcn (svr system-#-dispatch-vector code))
	    (svs! user-#-dispatch-vector code #f)
	    (svs! user-#-dispatch-vector code fcn)))))

(define (:get-dispatch-macro-character disp-char sub-char)
  (let ((sub-code (char->ascii (char-upcase sub-char))))
    (if (char=? disp-char #\#)
	(or (svr user-#-dispatch-vector sub-code)
	    (svr system-#-dispatch-vector sub-code))
	(let ((dispatch-vector (svr dispatch-parser-table (char->ascii disp-char))))
	  (if (null? dispatch-vector)
	      (error "Character ~a not defined as a dispatch character" disp-char)
	      (svr dispatch-vector sub-code))))))

;; This function is attached to each dispatch char,
;;  and calls the fcn associated with the char following
;;  optional digits.
;; Char (passed) is the dispatch char.

(define (dispatch-a-char stream disp-char)
  (let* ((param (read-param stream))
	 (char-fcn (svr (svr dispatch-parser-table (char->ascii disp-char))
			(char->ascii (char-upcase char))))
	 (char (read-char)))
    (if (null? char-fcn)
	(error "No read function defined for dispatch pair ~a~a" disp-char char)
	(char-fcn stream char param))))

;; Special dispatcher for "#".
;; A user and a system table are used so we don't
;; copy system closures when copying readtables;
;; This is similar to the way single-macro chars work.

(define (dispatch-#-char stream disp-char)
  (let* ((param (read-param stream))
	 (char (read-char))
	 (code (char->ascii (char-upcase char)))
	 (char-fcn (or (svr user-#-dispatch-vector code)
		       (svr system-#-dispatch-vector code))))
    (if (null? char-fcn)
	(error "No read function defined for dispatch pair ~a~a" disp-char char)
	(char-fcn stream char param))))

(define read-param
  (let ((ascii-0 (char->ascii #\0)))
    (lambda (stream)
      (let ((param #f))
	(do ((char (read-char) (read-char)))
	    ((not (char-set-member? digits char))
	     (unread-char)
	     param)
	  (set! param (+ (- (char->ascii char) ascii-0)
			 (* 10 (or param 0)))))))))

;;
;; Read an atom
;;

(define (:read-atom initial-string cont) ; => (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
  (let ((pkg-name #f)
	(escapes? #f)
	(pkg-internal? #f)
	(multi-pkg-markers? #f))
    (let loop ((acc initial-string))
      (let* ((s (*parser-read-string* atom-delimiters))
	     (c (*parser-peek-char*))) ; We check for non-null char -- nil means eof, a valid terminator
	(cond
	 ((and c (char-set-member? (svr char-sets %single-esc) c))
	  (set! escapes? #t)
	  (discard-char)
	  (string-upcase! s)
	  (loop (string-append acc s (char->string (read-char)))))
	 ((and c (char-set-member? (svr char-sets %multi-esc) c))
	  (set! escapes? #t)
	  (discard-char)
	  (string-upcase! s)
	  (let loop1 ((acc (string-append acc s)))
	    (let* ((s (*parser-read-string* escapes))
		   (c (peek-char)))
	      (cond
	       ((char-set-member? (svr char-sets %single-esc) c)
		(discard-char)
		(loop1 (string-append acc (char->string (read-char)))))
	       ((char-set-member? (svr char-sets %multi-esc) c)
		(discard-char)
		(loop (string-append acc s)))
	       (else (error "Internal error :read-atom/loop1" c))))))
	 ((and c (char-set-member? (svr char-sets %pkg-marker) c))
	  (if pkg-name
	      (begin
		(set! multi-pkg-markers? #t)
		(loop (string-append acc s)))
	      (begin
		(string-upcase! s)
		(set! pkg-name 
		      (if (string=? acc "")
			  s
			  (string-append acc s)))
		(discard-char)
		(if (char-set-member? (svr char-sets %pkg-marker)
				      (peek-char))
		    (begin
		      (set! pkg-internal? #t)
		      (discard-char)))
		(loop ""))))
	 (else
	  (and c 
	       (char-set-member? (svr char-sets %whitespace) c)
	       (not *preserve-whitespace?*)
	       (*parser-discard-char*))
	  (string-upcase! s)
	  (if (not (string=? acc ""))
	      (set! s (string-append acc s)))
	  (cont pkg-name s escapes? pkg-internal? multi-pkg-markers?)))))))

(define (parse-number string)
  (and (or (not (= *read-base* 10))	; Discard the common non-number cases quickly
	   (char-set-member? number-leaders (string-ref string 0)))
       (cl-string->number string *read-base*)))

;;
;; Parse lists and pairs
;;

(define (parse-list stream char)
  (gather-list #t))

;;;
;;;

(define (gather-list first-element?)
  (fluid-let ((within-list? #t))
    (let ((element (:parse-object)))
      (cond
       ((eq? element eol-object)
	'())
       ((eq? element eof-object)
	(eof-error))
       ((eq? element dot-object)
	(if first-element?
	    (error "\"Dot\" syntax error")	    
	    (let ((cdr-element (:parse-object)))
	      (cond
	       ((eq? cdr-element eol-object)
		(error "Right parenthesis may not follow a \"dot\""))
	       ((eq? cdr-element eof-object)
		(eof-error))
	       (else
		(if (not (eq? (:parse-object)
			      eol-object))
		    (error "\"Dot\" syntax error")
		    cdr-element))))))
       (else
	(cons element (gather-list #f)))))))

(define (discard-whitespace-and-comments)
  (*parser-discard-chars* non-whitespace)
  (let ((c (peek-char)))
    (cond
     ((char=? c #\;)
      (*parser-discard-chars* comment-delimiters)
      (discard-whitespace-and-comments))
     ((char=? c #\#)
      (discard-char)
      (if (char=? (peek-char) #\|)
	  (begin
	    (discard-inline-comment comment-leaders)
	    (discard-whitespace-and-comments))
	  (unread-char))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup the specific char functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whitespace

(mapc (lambda (c) 
	(svs! base-parser-table (char->ascii c)
	      (lambda (stream char)
		(*parser-unread-char*)
		(*parser-discard-chars* non-whitespace)
		(:parse-object))))
      (char-set-members (svr char-sets %whitespace)))

;; constituents

(define external-keyword (keyword-intern "EXTERNAL"))

(define (parse-atom stream char)
  (*parser-unread-char*)
  (let ((s (*parser-read-string* atom-delimiters))) ; look for standard atom first for speed
    (string-upcase! s)
    (let ((c (*parser-peek-char*)))
      (and c 
	   (char-set-member? (svr char-sets %whitespace) c)
	   (not *preserve-whitespace?*)
	   (*parser-discard-char*))
      (if (or (null? c) 
	      (char-set-member? atom-terminators c)) ; null means eof -- a valid terminator
	  (if *read-suppress*
	      #f
	      (or (and (char=? char #\.)
		       (all-dot-token-error s))
		  (parse-number s)
		  (pkg-intern-string s (string-length s) *package*)))
	  (:read-atom 
	   s
	   (lambda (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
	     (cond 
	      (*read-suppress* #f)
	      (multi-pkg-markers?
	       (error "Too many colons in symbol package/name ~a"
		      (string-append pkg-string
				     (if pkg-internal? "::" ":")
				     name-string)))
	      (else
	       (or (and (not pkg-string)
			(not escapes?)
			(parse-number name-string))
		   (if pkg-string
		       (let ((pkg (pkg-gethash pkg-string (get-package-names))))
			 (if (null? pkg)
			     (error "Package ~a not found" pkg-string)
			     (if pkg-internal?
				 (pkg-intern-string name-string (string-length name-string) pkg)
				 (prim-with-values
				  (lambda () (pkg-find-symbol name-string (string-length name-string) pkg))
				  (lambda (symbol where)
				    (if (not (eq? where external-keyword))
					(error "Symbol name ~a is not the name of an external symbol in package ~a" 
					       name-string pkg)
					symbol))))))
		       (pkg-intern-string name-string (string-length name-string) *package*)))))))))))

(define (all-dot-token-error str)
  (let ((l (string-length str)))
    (let loop ((i 0))
      (cond
       ((= i l)
	(error "Illegal token (all \"dots\"): ~a" str))
       ((char=? (string-ref str i) #\.)
	(loop (1+ i)))
       (else
	#f)))))
	
(mapc (lambda (c) 
	(svs! base-parser-table (char->ascii c)
	      parse-atom))
      (char-set-members (svr char-sets %constituent)))

;; (

(svs! macro-parser-table (char->ascii #\()
      parse-list)

;; )

(svs! macro-parser-table (char->ascii #\))
      (lambda (stream char)
	(if within-list?
	    eol-object
	    (if (*parser-isa-tty?*)
		(:parse-object)
		(error "unmatched right parenthesis on stream ~a" stream)))))

;; "

(svs! 
 macro-parser-table (char->ascii #\")
 (lambda (stream char)
   (define (loop string)
     (if (char=? #\" (read-char))
	 string
	 (let ((char (read-char)))
	   (string-append string
			  (char->string char)
			  (loop (read-string string-delimiters))))))
   (loop (read-string string-delimiters))))

;; '

(svs! 
 macro-parser-table (char->ascii #\')
 (lambda (stream char)
   (list 'quote (:parse-object))))

;; ;

(svs! 
 macro-parser-table (char->ascii #\;)
 (lambda (stream char)
   (discard-chars comment-delimiters)
   (:parse-object)))

;; `

(svs! 
 macro-parser-table (char->ascii #\`)
 (lambda (stream char)
   (list 'QUASIQUOTE (:parse-object))))

;; , and ,@ and ,.

(svs! 
 macro-parser-table (char->ascii #\,)
 (lambda (stream char)
   (if (member (peek-char) '(#\@ #\.))
       (begin (discard-char)
	      (list 'UNQUOTE-SPLICING (:parse-object)))
       (list 'UNQUOTE (:parse-object)))))

;; :

(svs!
 base-parser-table (char->ascii #\:)
 (lambda (stream char)
   (:read-atom ""
     (lambda (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
       (if *read-suppress*
	   #f
	   (if pkg-string
	       (error "Improperly formed keyword ~a"
		      (string-append pkg-string
				     (if pkg-internal? "::" ":")
				     name-string))
	       (if (string=? "" name-string)
		   (error "Symbol token cannot end immediately after a colon.")
		   (if (parse-number name-string)
		       (error "Improper keyword ~A" (string-append ":" name-string))
		       (keyword-intern name-string)))))))))

;; .

(define dot-object "Dot object")

(svs!
 base-parser-table (char->ascii #\.)
 (lambda (stream char)
   (if (or (char-set-member? (svr char-sets %constituent) (peek-char))
	   (not within-list?))
       (parse-atom stream char)
       dot-object)))

;; #

(:make-dispatch-macro-character #\# #t)

;; #\

(:set-system-#-dispatch-macro-character 
 #\\
 (lambda (stream char param)
   (define (loop)
     (cond ((char=? #\\ (peek-char))
	    (discard-char)
	    (char->string (read-char)))
	   ((char-set-member? char-delimiters (peek-char))
	    (char->string (read-char)))
	   (else
	    (let ((string (read-string char-delimiters)))
	      (if (let ((char (peek-char)))
		    (and char
			 (char=? #\- char)))
		  (begin (discard-char)
			 (string-append string "-" (loop)))
		  string)))))
   (let ((char-or-name
	  (if (char=? #\\ (peek-char))
	      (read-char)
	      (loop))))
     (if *read-suppress*
	 #f
	 (if param
	     (error "Illegal to supply argument to #\\ (char or char name: ~a)" char-or-name)
	     (if (char? char-or-name)
		 char-or-name
		 (name->char char-or-name)))))))

;; #'

(:set-system-#-dispatch-macro-character
 #\'
 (lambda (stream char param)
   (error-if-#-param param "#'")
   (list 'function (:parse-object))))

;; #(

(:set-system-#-dispatch-macro-character
 #\(
 (lambda (stream char length)
   ((sym-fcn 'list-with-length->vector) (parse-list stream char) length)))

;; #*

(:set-system-#-dispatch-macro-character
 #\*
 (lambda (stream char length)
   (:read-atom ""
     (lambda (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
       (let ()
	 (define (err)
	   (error "Illegal bit-vector specifier ~a" 
		  (string-append 
		   (if (string? pkg-string)
		       ""
		       pkg-string)
		   (if pkg-internal? "::" ":")
		   name-string)))
	 (if (or pkg-string
		 escapes?
		 pkg-internal?
		 multi-pkg-markers?)
	     (err)
	     (let* ((l (string-length name-string))
		    (bit-vector-length (or length l))
		    (v (make-bit-string bit-vector-length #f)))
	       (let loop ((i 0))
		 (if (= i l)
		     v
		     (begin
		       (cond
			((char=? (string-ref name-string i) #\0)
			 (bit-string-clear! v i))
			((char=? (string-ref name-string i) #\1)
			 (bit-string-set! v i))
			(else (err)))
		       (loop (1+ i))))))))))))

;; #:

(:set-system-#-dispatch-macro-character
 #\:
 (lambda (stream char param)
   (error-if-#-param param "#:")
   (:read-atom ""
     (lambda (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
       (cond
	(*read-suppress* #f)
	(pkg-string
	 (error "Illegal to specify a package name (~a) to #:" pkg-string))
	((or escapes?
	     (not (parse-number name-string)))
	 (make-symbol name-string))
	(else
	 (error "Item following #: must be a symbol (~a)" name-string)))))))

;; #.

(:set-system-#-dispatch-macro-character
 #\.
 (lambda (stream char param)
   (error-if-#-param param "#.")
   (if *read-suppress*
       (begin (:parse-object)
	      #f)
       (eval (:parse-object) *commonlisp-user-environment*))))

;; #,

(:set-system-#-dispatch-macro-character
 #\,
 (lambda (stream char param)
   (error-if-#-param param "#,")
   (if *read-suppress*
       (begin
	 (:parse-object)
	 #f)
       (error "'#,' not implemented"))))

;; #B

(:set-system-#-dispatch-macro-character
 #\b
 (lambda (stream char param)
   (parse-number-with-radix stream char param 2 #t)))

;; #O

(:set-system-#-dispatch-macro-character
 #\o
 (lambda (stream char param)
   (parse-number-with-radix stream char param 8 #t)))

;; #X

(:set-system-#-dispatch-macro-character
 #\x
 (lambda (stream char param)
   (parse-number-with-radix stream char param 16 #t)))

;; #nR

(:set-system-#-dispatch-macro-character
 #\r
 (lambda (stream char radix)
   (parse-number-with-radix stream char radix radix #f)))

;; #C
(:set-system-#-dispatch-macro-character
 #\c
 (lambda (stream char param)
   (error-if-#-param param "#C")
   (let ((l (:parse-object)))
     (if (and (not (pair? l))
	      (not (= (length l) 2)))
	 (error "Improper #C format ~a" l)
	 ((sym-fcn 'complex) (car l) (cadr l))))))

;; #nA
;;
;; Defer to CL
;;

(:set-system-#-dispatch-macro-character
 #\a
 (lambda (stream char ndims)
   (error-if-not-#-param ndims "#nA")
   ((sym-fcn 'make-#a-array) ndims (:parse-object))))

;; #S
;;
;; Defer to CL
;;

(:set-system-#-dispatch-macro-character
 #\s
 (lambda (stream char param)
   (error-if-#-param param "#S")
   ((if *read-suppress*
	(lambda (x) #f)
	(sym-fcn 'make-#s-structure))
    (:parse-object))))

;; #P

(:set-system-#-dispatch-macro-character
 #\p
 (lambda (stream char param)
   (error-if-#-param param "#P")
   (let ((path-descriptor (:parse-object)))
     (if *read-suppress*
	 #f
	 (if (not (string? path-descriptor))
	     (error "#P: argument ~s not a string" path-descriptor)
	     (string->pathname path-descriptor))))))

;; #n=

(:set-system-#-dispatch-macro-character
 #\=
 (lambda (stream char label)
   (if *read-suppress*
       (begin
	 (:parse-object)
	 (:parse-object))
       (if (not label)
	   (error "No label supplied with #n= syntax")
	   (let ((ref (generate-uninterned-symbol)))
	     (set! *parser-##-alist*
		   (cons (cons label ref)
			 *parser-##-alist*))
	     (let ((obj (:parse-object)))
	       (set! *parser-#=-alist* 
		     (cons (cons ref obj)
			   *parser-#=-alist*))
	       ((sym-fcn 'circle-subst) *parser-#=-alist* obj)))))))

;; #n#

(:set-system-#-dispatch-macro-character
 #\#
 (lambda (stream char label)
   (if *read-suppress*
       #f
       (if (not label)
	   (error "No label supplied with #n# syntax")
	   (let* ((label-sym (assoc label *parser-##-alist*))
		  (label-sym (and (or label-sym 
				      (error "Label #~a# does not exist" label))
				  (cdr label-sym)))
		  (obj (assoc label-sym *parser-#=-alist*)))
	     (if obj (cdr obj) label-sym))))))



;; #+

(:set-system-#-dispatch-macro-character
 #\+
 (lambda (stream char param)
   (if *read-suppress*
       (begin
	 (:parse-object)
	 (:parse-object)
	 #f)
       (if param
	   (error "Illegal to supply argument to #+")
	   (let ((feature))
	     (fluid-let ((*package* *keyword-package*))
	       (set! feature (:parse-object)))
	     (if (feature-true? feature)
		 (:parse-object)
		 (begin
		   (fluid-let ((*read-suppress* #t))
		     (:parse-object))
		   (:parse-object))))))))

;; #-

(:set-system-#-dispatch-macro-character
 #\-
 (lambda (stream char param)
   (if *read-suppress*
       (begin
	 (:parse-object)
	 (:parse-object)
	 #f)
       (if param
	   (error "Illegal to supply argument to #-")
	   (let ((feature))
	     (fluid-let ((*package* *keyword-package*))
	       (set! feature (:parse-object)))
	     (if (not (feature-true? feature))
		 (:parse-object)
		 (begin
		   (fluid-let ((*read-suppress* #t))
		     (:parse-object))
		   (:parse-object))))))))

;; #|

(:set-system-#-dispatch-macro-character
 #\|
 (lambda (stream char param)
   (if param
       (error "Illegal to supply argument to #|")
       (begin
	 (discard-inline-comment comment-leaders)
	 (:parse-object)))))

;; #<

(:set-system-#-dispatch-macro-character
 #\<
 (lambda (stream char param)
   (parse-#-error char)))

;; #space

(:set-system-#-dispatch-macro-character
 #\space
 (lambda (stream char param)
   (parse-#-error char)))

;; #tab

(:set-system-#-dispatch-macro-character
 #\tab
 (lambda (stream char param)
   (parse-#-error char)))

;; #return

(:set-system-#-dispatch-macro-character
 #\return
 (lambda (stream char param)
   (parse-#-error char)))

;; #page

(:set-system-#-dispatch-macro-character
 #\page
 (lambda (stream char param)
   (parse-#-error char)))

;; #)

(:set-system-#-dispatch-macro-character
 #\)
 (lambda (stream char param)
   (parse-#-error char)))

))) ; end make-environment, let clause

(set! (access :self readtable) readtable)

readtable

))) ; end let, set! of make-readtable

;;; Setup the standard readtables from which others will be copied

(define standard-commonlisp-readtable (make-readtable))

(set! *readtable* (make-readtable))

