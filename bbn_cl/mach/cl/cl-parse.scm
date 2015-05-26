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
;;; Common Lisp Parser

(declare (usual-integrations))

(set! set-syntax-from-char
      (lambda (to-char from-char #!optional to-readtable from-readtable)
	(if (unassigned? to-readtable) 
	    (set! to-readtable *readtable*))
	(if (or (unassigned? from-readtable)
		(null? from-readtable))
	    (set! from-readtable standard-commonlisp-readtable))
	((access :set-syntax-from-char to-readtable) from-char to-char from-readtable)))

(set! set-macro-character
      (lambda (char fcn #!optional non-term? readtable)
	(if (unassigned? non-term?) (set! non-term? #f))
	(if (unassigned? readtable) (set! readtable *readtable*))
	((access :set-macro-character readtable) char fcn non-term?)
	#t))

(set! get-macro-character
      (lambda (char #!optional readtable)
	(if (unassigned? readtable) (set! readtable *readtable*))
	((access :get-macro-character readtable) char)))

(set! make-dispatch-macro-character
      (lambda (char #!optional non-term? readtable)
	(if (unassigned? non-term?) (set! non-term? #f))
	(if (unassigned? readtable) (set! readtable *readtable*))
	((access :make-dispatch-macro-character readtable) char non-term?)))

(set! set-dispatch-macro-character
      (lambda (disp-char sub-char fcn #!optional readtable)
	(if (unassigned? readtable) (set! readtable *readtable*))
	((access :set-dispatch-macro-character readtable) disp-char sub-char fcn)))

(set! get-dispatch-macro-character
      (lambda (disp-char sub-char #!optional readtable)
	(if (unassigned? readtable) (set! readtable *readtable*))
	((access :get-dispatch-macro-character readtable) disp-char sub-char)))


(define parse-number-with-radix)

(let ((error-table '((2  "binary" "#b")
		     (8  "octal" "#o")
		     (16 "hexadecimal" "#x"))))

  (define (error-data radix)
    (let ((data (cdr (assoc radix error-table))))
      (or data
	  (let ((radstr (number->string radix)))
	    (list (string-append "radix " radstr)
		  (string-append "#nr"))))))
	  
  (set! parse-number-with-radix 
	(lambda (stream char param radix symbolic?)
	  (if (and (not *read-suppress*)
		   (or (and param symbolic?)
		       (and (not param) (not symbolic?))))
	      (error "Illegal argument to ~a syntax" 
		     (cadr (error-data radix)))
	      ((access :read-atom *readtable*) ""
		(lambda (pkg-string name-string escapes? pkg-internal? multi-pkg-markers?)
		  (if *read-suppress*
		      #f
		      (or (and (not pkg-string)
			       (not escapes?)
			       (cl-string->number name-string radix))
			  (error "Improper ~a number format ~a" 
				 (car (error-data radix))
				 name-string)))))))))

(define (feature-true? feature)
  (cond
   ((symbol? feature) 
    (memq feature *features*))
   ((and (pair? feature) (eq? (car feature) (keyword-intern "OR")))
    (or-features (cdr feature)))
   ((and (pair? feature) (eq? (car feature) (keyword-intern "AND")))
    (and-features (cdr feature)))
   ((and (pair? feature) (eq? (car feature) (keyword-intern "NOT")))
    (not (feature-true? (cadr feature))))
   (t (error "Illegal feature construction ~a~%" feature))))

(define (or-features features)
  (and features
       (or (feature-true? (car features))
	   (or-features (cdr features)))))

(define (and-features features)
  (or (null? features)
      (and (feature-true? (car features))
	   (and-features (cdr features)))))

(define (discard-inline-comment comment-leaders)
  (discard-chars comment-leaders)
  (if (char=? #\| (read-char))
      (if (char=? #\# (peek-char))
	  (discard-char)
	  (discard-inline-comment comment-leaders))
      (begin (if (char=? #\| (peek-char))
		 (begin (discard-char)
			(discard-inline-comment comment-leaders)))
	     (discard-inline-comment comment-leaders))))

(define (parse-#-error char)
  (error "#~a is not legal syntax" char))

(define (error-if-#-param param syntax)
  (cond
   (*read-suppress* #f)
   (param (error "Illegal to supply argument to ~a" syntax))))

(define (error-if-not-#-param param syntax)
  (cond
   (*read-suppress* #f)
   ((not param) (error "Illegal not to supply argument to ~a" syntax))))

(define (sym-fcn f)
  (eval (fundefsym f) system-global-environment))

(define cl-peek-char (make-primitive-procedure 'cl-peek-char))
(define (*parser-peek-char*) (cl-peek-char #f *parser-input-stream* #f #f))
(define cl-read-char (make-primitive-procedure 'cl-read-char))
(define (*parser-read-char*) (cl-read-char *parser-input-stream* #f #f))
(define *parser-discard-char* *parser-read-char*)
(define cl-read-string (make-primitive-procedure 'cl-read-string))
(define (*parser-read-string* char-set) (cl-read-string char-set #f *parser-input-stream* #f #f))
(define (*parser-discard-chars* char-set) (cl-read-string char-set #t *parser-input-stream* #f #f))
(define cl-unread-char (make-primitive-procedure 'cl-unread-char))
(define (*parser-unread-char*) (cl-unread-char #\null *parser-input-stream*))
(define cl-isa-tty-p (make-primitive-procedure 'cl-isa-tty-p))
(define (*parser-isa-tty?*) (cl-isa-tty-p *parser-input-stream*))

(define *parser-input-stream*)
(define *parser-#=-alist*)
(define *parser-##-alist*)
(define *preserve-whitespace?*)

;;;
;;; The following two functions are for directly
;;;  replacing the corresponding scheme functions,
;;;  for testing in the scheme rep loop
;;;

(define (*parse-object stream)
  (*cl-parse-object stream #f #f eof-object #f #f))

(define (*parse-objects-until-eof stream)
  (*cl-parse-object stream #t #f #f #f #f))

;;;
;;; Main cl interface; called by the appropriate cl function
;;;

(define (*cl-parse-object stream until-eof? eof-error? user-eof-obj recursive? preserve-whitespace?)
  (fluid-let ((*parser-input-stream* stream))
    (fluid-let ((*parser-#=-alist* 
		 (if recursive?
		     *parser-#=-alist*
		     '()))
		(*parser-##-alist* 
		 (if recursive?
		     *parser-##-alist*
		     '()))
		(*preserve-whitespace?*
		 (if recursive?
		     *preserve-whitespace?*
		     preserve-whitespace?)))
      (let ((parse-object (access :parse-object *readtable*)))
	(define (loop obj)
	  (if (eof-object? obj)
	      '()
	      (cons obj (loop (parse-object)))))
	(if until-eof?
	    (loop (parse-object))
	    (let ((obj (parse-object)))
	      (if (eof-object? obj)
		  (if eof-error?
		      (eof-error)
		      user-eof-obj)
		  obj)))))))
		      
;;;; Character Operations

;;;
;;; Note: at some places in the code you will see the *parser- versions
;;;  of the procedures below. Mostly, these are for efficiency.
;;;

(define (peek-char)
  (or (*parser-peek-char*)
      (eof-error)))

(define (read-char)
  (or (*parser-read-char*)
      (eof-error)))

(define (discard-char)
  (*parser-discard-char*))

(define (unread-char)
  (*parser-unread-char*))

(define (read-string delimiters)
  (*parser-read-string* delimiters))

(define (discard-chars delimiters)
  (*parser-discard-chars* delimiters))

(define (eof-error)
  (error "End of file within READ on stream ~a" *parser-input-stream*))

             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;;;                                       ;;;
             ;;; Specfic parsing auxilliary functions  ;;;
             ;;;                                       ;;;
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (parse-undefined-special stream char)
  (error "No such special reader macro" (peek-char)))

(define (peek-ascii)
  (or (char-ascii? (peek-char))
      (non-ascii-error)))

(define (non-ascii-error)
  (error "Non-ASCII character encountered during parse" (read-char)))

;;;
;;; Commonlisp-specific number->string
;;;

(define cl-string->number)

(let ()

(define cl-parse-number (make-primitive-procedure 'cl-parse-number))
(define ->float (make-primitive-procedure 'coerce-integer-to-flonum))
(define f+ (make-primitive-procedure 'plus-flonum))
(define f/ (make-primitive-procedure 'divide-flonum))
(define f* (make-primitive-procedure 'multiply-flonum))

(define float-10 (->float 10))
(define float-0 (->float 0))
(define float-1 (->float 1))
(define float--1 (->float -1))
(define ascii-0 (char->ascii #\0))

(define parsed-float 0)
(define parsed-integer 1)
(define parsed-ratio 2)

(set! cl-string->number
      (lambda (s #!optional radix)

	(define (get-integer i1 i2)
	  (let ((acc 0))
	    (let loop ((i i1))
	      (if (= i i2)
		  acc
		  (begin
		    (set! acc (+ (* acc radix) (char->digit (string-ref s i) radix)))
		    (loop (1+ i)))))))

	(if (unassigned? radix) (set! radix *read-base*))
	(prim-with-values
	 (lambda () (cl-parse-number s radix))
	 (lambda (kind
		  #!optional
		  negative?
		  exp-negative?
		  digit1-start
		  digit1-end
		  digit2-start
		  digit2-end
		  digit3-start
		  digit3-end)
	   (cond
	    ((null? kind)
	     #f)
	    ((eq? kind parsed-float)
	     (let ((integer-part
		    (get-integer digit1-start digit1-end))
		   (fractional-part
		    (let ((acc float-0))
		      (let loop ((i (-1+ digit2-end)))
			(if (< i digit2-start)
			    (f/ acc float-10)
			    (begin
			      (set! acc (f+ (f/ acc float-10) (->float (char->digit (string-ref s i) 10))))
			      (loop (-1+ i)))))))
		   (exponent
		    (get-integer digit3-start digit3-end))
		   (sign (if negative? float--1 float-1))
		   (exp-sign (if exp-negative? -1 1)))
	       (f* sign (f* (f+ (->float integer-part) fractional-part) 
			    (->float (expt float-10 (->float (* exp-sign exponent))))))))
	    ((eq? kind parsed-integer)
	     (let ((integer
		    (get-integer digit1-start digit1-end)))
	       (if negative?
		   (- integer)
		   integer)))
	    ((eq? kind parsed-ratio)
	     (let ((numerator (get-integer digit1-start digit1-end))
		   (denominator (get-integer digit2-start digit2-end)))
	       ((symbol-function '/) 
		(if negative?
		    (- numerator)
		    numerator)
		denominator)))
	    (else #f))))))

)  ;  end let

(eq? 'truth 'beauty)  ; to avoid unassigned-var error, since compiled code will pick up 
                      ; previous value to var set!-ed, the unassigned object.
