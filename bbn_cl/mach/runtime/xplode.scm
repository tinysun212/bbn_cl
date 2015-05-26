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

;;;; MacLisp Style Print-name Stuff

(declare (usual-integrations))

(define alphaless?
  (let ()
    (define (stringify object)
      (cond ((symbol? object) (symbol->string object))
	    ((string? object) object)
	    (else (error "ALPHALESS?: Wrong type argument" object))))

    (named-lambda (alphaless? x y)
      (string<? (stringify x) (stringify y)))))

(define explode
  (let ()
    (define (explode-integer n)
      (map char->digit (string->list (write-to-string n))))

    (named-lambda (explode object)
      (cond ((string? object)
	     (map char->string (string->list object)))
	    ((symbol? object)
	     (map (lambda (char)
		    (string->symbol (char->string char)))
		  (string->list (symbol->string object))))
	    ((integer? object)
	     (if (negative? object)
		 (cons '- (explode-integer (- object)))
		 (explode-integer object)))
	    (else (error "EXPLODE: Wrong type argument" object))))))

(define implode
  (let ()
    (define (implode-integer l acc)
      (cond ((null? l) acc)
	    ((and (integer? (car l)) (<= 0 (car l) 9))
	     (implode-integer (cdr l) (+ (car l) (* acc 10))))
	    (else (error "IMPLODE: Bad digit in list" (car l)))))

    (named-lambda (implode list)
      (if (not (pair? list))
	  (error "IMPLODE: Argument must be a non-null list" list))
      (cond ((string? (car list))
	     (list->string (map (lambda (string)
				  (if (or (not (string? string))
					  (not (= (string-length string) 1)))
				      (error "IMPLODE: Not a character"
					     string))
				  (string-ref string 0))
				list)))
	    ((and (eq? (car list) '-)
		  (pair? (cdr list))
		  (integer? (cadr list)))
	     (- (implode-integer (cddr list) (cadr list))))
	    ((symbol? (car list))
	     (string->symbol
	      (list->string (map (lambda (symbol)
				   (if (not (symbol? symbol))
				       (error "IMPLODE: Not a character"
					      symbol))
				   (let ((string (symbol->string symbol)))
				     (if (not (= (string-length string) 1))
					 (error "IMPLODE: Not a character"
						string))
				     (string-ref string 0)))
				 list))))
	    ((integer? (car list))
	     (implode-integer (cdr list) (car list)))
	    (else (error "IMPLODE: Wrong type argument" list))))))

(define (char ascii)
  (string->symbol (char->string (char-upcase (ascii->char ascii)))))

(define (ascii s)
  (let ((name
	 (cond ((string? s) s)
	       ((symbol? s) (symbol->string s))
	       (else "ASCII: Wrong type argument" s))))
    (if (= (string-length name) 1)
	(char->ascii (string-ref name 0))
	(error "ASCII: Not a character" s))))

(define (readch)
  (char (char->ascii (read-char))))

(define (peekch)
  (char (char->ascii (peek-char))))
