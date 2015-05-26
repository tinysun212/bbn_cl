;;; -*-Scheme-*-
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

(define *parser-parse-object-table*)
(define *parser-collect-list-table*)
(define *parser-parse-object-special-table*)
(define *parser-collect-list-special-table*)
(define *parser-peek-char*)
(define *parser-discard-char*)
(define *parser-read-char*)
(define *parser-read-string*)
(define *parser-discard-chars*)
(define *parser-input-port*)

(define (*parse-object port)
  (fluid-let ((*parser-input-port* port)
	      (*parser-parse-object-table* (caar *parser-table*))
	      (*parser-collect-list-table* (cdar *parser-table*))
	      (*parser-parse-object-special-table* (cadr *parser-table*))
	      (*parser-collect-list-special-table* (cddr *parser-table*))
	      (*parser-peek-char* (access :peek-char port))
	      (*parser-discard-char* (access :discard-char port))
	      (*parser-read-char* (access :read-char port))
	      (*parser-read-string* (access :read-string port))
	      (*parser-discard-chars* (access :discard-chars port)))
    (parse-object)))

(define (*parse-objects-until-eof port)
  (fluid-let ((*parser-input-port* port)
	      (*parser-parse-object-table* (caar *parser-table*))
	      (*parser-collect-list-table* (cdar *parser-table*))
	      (*parser-parse-object-special-table* (cadr *parser-table*))
	      (*parser-collect-list-special-table* (cddr *parser-table*))
	      (*parser-peek-char* (access :peek-char port))
	      (*parser-discard-char* (access :discard-char port))
	      (*parser-read-char* (access :read-char port))
	      (*parser-read-string* (access :read-string port))
	      (*parser-discard-chars* (access :discard-chars port)))
    (define (loop object)
      (if (eof-object? object)
	  '()
	  (cons object (loop (parse-object)))))
    (loop (parse-object))))

;;;; Character Operations

(declare (integrate peek-char read-char discard-char
		    read-string discard-chars))

(define (peek-char)
  (or (*parser-peek-char*)
      (error "End of file within READ")))

(define (read-char)
  (or (*parser-read-char*)
      (error "End of file within READ")))

(define (discard-char)
  (*parser-discard-char*))

(define (read-string delimiters)
  (declare (integrate delimiters))
  (*parser-read-string* delimiters))

(define (discard-chars delimiters)
  (declare (integrate delimiters))
  (*parser-discard-chars* delimiters))

;;; There are two major dispatch tables, one for parsing at top level,
;;; the other for parsing the elements of a list.  Most of the entries
;;; for each table are have similar actions.

;;; Default is atomic object.  Parsing an atomic object does not
;;; consume its terminator.  Thus different terminators [such as open
;;; paren, close paren, and whitespace], can have different effects on
;;; parser.

(define (parse-object:atom)
  (build-atom (read-atom)))

(define ((collect-list-wrapper object-parser))
  (let ((value (object-parser)))			;forces order.
    (cons value (collect-list))))

(define (parse-undefined-special)
  (error "No such special reader macro" (peek-char)))

(set! *parser-table*
      (cons (cons (vector-cons 256 parse-object:atom)
		  (vector-cons 256 (collect-list-wrapper parse-object:atom)))
	    (cons (vector-cons 256 parse-undefined-special)
		  (vector-cons 256 parse-undefined-special))))

(define ((parser-char-definer tables)
	 char/chars procedure #!optional list-procedure)
  (if (unassigned? list-procedure)
      (set! list-procedure (collect-list-wrapper procedure)))
  (define (do-it char)
    (vector-set! (car tables) (char->ascii char) procedure)
    (vector-set! (cdr tables) (char->ascii char) list-procedure))
  (cond ((char? char/chars) (do-it char/chars))
	((char-set? char/chars)
	 (for-each do-it (char-set-members char/chars)))
	((pair? char/chars) (for-each do-it char/chars))
	(else (error "Unknown character" char/chars))))

(define define-char
  (parser-char-definer (car *parser-table*)))

(define define-char-special
  (parser-char-definer (cdr *parser-table*)))

(declare (integrate peek-ascii parse-object collect-list))

(define (peek-ascii)
  (or (char-ascii? (peek-char))
      (non-ascii-error)))

(define (non-ascii-error)
  (error "Non-ASCII character encountered during parse" (read-char)))

(define (parse-object)
  (let ((char (*parser-peek-char*)))
    (if char
	((vector-ref *parser-parse-object-table*
		     (or (char-ascii? char)
			 (non-ascii-error))))
	eof-object)))

(define (collect-list)
  ((vector-ref *parser-collect-list-table* (peek-ascii))))

(define-char #\#
  (lambda ()
    (discard-char)
    ((vector-ref *parser-parse-object-special-table* (peek-ascii))))
  (lambda ()
    (discard-char)
    ((vector-ref *parser-collect-list-special-table* (peek-ascii)))))

(define numeric-leaders
  (char-set-union char-set:numeric
		  (char-set #\+ #\- #\. #\#)))

(define undefined-atom-delimiters
  (char-set #\[ #\] #\{ #\} #\|))

(define atom-delimiters
  (char-set-union char-set:whitespace
		  (char-set-union undefined-atom-delimiters
				  (char-set #\( #\) #\; #\" #\' #\`))))

(define atom-constituents
  (char-set-invert atom-delimiters))

(declare (integrate read-atom))

(define (read-atom)
  (read-string atom-delimiters))

