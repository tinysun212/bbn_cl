;;; -*-Scheme-*-
;;;
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Scheme Parser

(declare (usual-integrations))

;;loaded into parser-package

(define (build-atom string)
  (or (parse-number string)
      (intern-string! string)))

(declare (integrate parse-number))

(define (parse-number string)
  (declare (integrate string))
  (string->number string false *parser-radix*))

(define (intern-string! string)
  (substring-upcase! string 0 (string-length string))
  (string->symbol string))

(define-char (char-set-difference atom-constituents numeric-leaders)
  (lambda ()
    (intern-string! (read-atom))))

(let ((numeric-prefix
       (lambda ()
	 (let ((number
		(let ((char (read-char)))
		  (string-append (char->string #\# char) (read-atom)))))
	   (or (parse-number number)
	       (error "READ: Bad number syntax" number))))))
  (define-char-special '(#\b #\B) numeric-prefix)
  (define-char-special '(#\o #\O) numeric-prefix)
  (define-char-special '(#\d #\D) numeric-prefix)
  (define-char-special '(#\x #\X) numeric-prefix)
  (define-char-special '(#\i #\I) numeric-prefix)
  (define-char-special '(#\e #\E) numeric-prefix)
  (define-char-special '(#\s #\S) numeric-prefix)
  (define-char-special '(#\l #\L) numeric-prefix))

(define-char #\(
  (lambda ()
    (discard-char)
    (collect-list)))

(define-char-special #\(
  (lambda ()
    (discard-char)
    (list->vector (collect-list))))

(define-char #\)
  (lambda ()
    (if (not (eq? console-input-port *parser-input-port*))
	(error "PARSE-OBJECT: Unmatched close paren" (read-char))
	(read-char))
    (parse-object))
  (lambda ()
    (discard-char)
    '()))

(define-char undefined-atom-delimiters
  (lambda ()
    (error "PARSE-OBJECT: Undefined atom delimiter" (read-char))
    (parse-object))
  (lambda ()
    (error "PARSE-OBJECT: Undefined atom delimiter" (read-char))
    (collect-list)))

(let ()

(vector-set! (cdar *parser-table*)
	     (char->ascii #\.)
  (lambda ()
    (discard-char)
    ;; atom with initial dot?
    (if (char-set-member? atom-constituents (peek-char))
	(let ((first (build-atom (string-append "." (read-atom)))))
	  (cons first (collect-list)))

	;; (A . B) -- get B and ignore whitespace following it.
	(let ((tail (parse-object)))
	  (discard-whitespace)
	  (if (not (char=? (peek-char) #\)))
	      (error "Illegal character in ignored stream" (peek-char)))
	  (discard-char)
	  tail))))

(define-char char-set:whitespace
  (lambda ()
    (discard-whitespace)
    (parse-object))
  (lambda ()
    (discard-whitespace)
    (collect-list)))

(define (discard-whitespace)
  (discard-chars non-whitespace))

(define non-whitespace
  (char-set-invert char-set:whitespace))

)

(let ()

(define-char #\;
  (lambda ()
    (discard-comment)
    (parse-object))
  (lambda ()
    (discard-comment)
    (collect-list)))

(define (discard-comment)
  (discard-char)
  (discard-chars comment-delimiters)
  (discard-char))

(define comment-delimiters
  (char-set char:newline))

)

(let ()

(define-char-special #\|
  (lambda ()
    (discard-char)
    (discard-special-comment)
    (parse-object))
  (lambda ()
    (discard-char)
    (discard-special-comment)
    (collect-list)))

(define (discard-special-comment)
  (discard-chars special-comment-leaders)
  (if (char=? #\| (read-char))
      (if (char=? #\# (peek-char))
	  (discard-char)
	  (discard-special-comment))
      (begin (if (char=? #\| (peek-char))
		 (begin (discard-char)
			(discard-special-comment)))
	     (discard-special-comment))))

(define special-comment-leaders
  (char-set #\# #\|))

)

(define-char #\'
  (lambda ()
    (discard-char)
    (list 'QUOTE (parse-object))))

(define-char #\`
  (lambda ()
    (discard-char)
    (list 'QUASIQUOTE (parse-object))))

(define-char #\,
  (lambda ()
    (discard-char)
    (if (char=? #\@ (peek-char))
	(begin (discard-char)
	       (list 'UNQUOTE-SPLICING (parse-object)))
	(list 'UNQUOTE (parse-object)))))

(define-char #\"
  (let ((delimiters (char-set #\" #\\)))
    (lambda ()
      (define (loop string)
	(if (char=? #\" (read-char))
	    string
	    (let ((char (read-char)))
	      (string-append string
			     (char->string
			      (cond ((char-ci=? char #\t) #\Tab)
				    ((char-ci=? char #\n) char:newline)
				    ((char-ci=? char #\f) #\Page)
				    (else char)))
			     (loop (read-string delimiters))))))
      (discard-char)
      (loop (read-string delimiters)))))

(define-char-special #\\
  (let ((delimiters (char-set-union (char-set #\- #\\) atom-delimiters)))
    (lambda ()
      (define (loop)
	(cond ((char=? #\\ (peek-char))
	       (discard-char)
	       (char->string (read-char)))
	      ((char-set-member? delimiters (peek-char))
	       (char->string (read-char)))
	      (else
	       (let ((string (read-string delimiters)))
		 (if (let ((char (*parser-peek-char*)))
		       (and char
			    (char=? #\- char)))
		     (begin (discard-char)
			    (string-append string "-" (loop)))
		     string)))))
      (discard-char)
      (if (char=? #\\ (peek-char))
	  (read-char)
	  (name->char (loop))))))

(define ((fixed-object-parser object))
  (discard-char)
  object)

(define-char-special '(#\f #\F) (fixed-object-parser false))
(define-char-special '(#\t #\T) (fixed-object-parser true))

(define-char-special #\!
  (lambda ()
    (discard-char)
    (let ((object-name (parse-object)))
      (cdr (or (assq object-name named-objects)
	       (error "No object by this name" object-name))))))

;;;
;;; We use a macro below to force evaluation at compile-time,
;;;  since there is a bug compiling nested conses.
;;;  

(define-macro (make-named-object-alist)
  (list 'quote
	`((NULL . ,(list))
	  (FALSE . ,(eq? 'TRUE 'FALSE))
	  (TRUE . ,(eq? 'TRUE 'TRUE))
	  (OPTIONAL . ,(access lambda-optional-tag lambda-package))
	  (REST . ,(access lambda-rest-tag lambda-package)))))

(define named-objects (make-named-object-alist))
