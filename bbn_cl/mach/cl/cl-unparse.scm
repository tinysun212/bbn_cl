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
(declare (usual-integrations))

;;; Global data

(define *dispatch-vector*)
(define *internal-symbol-output-function*)
(define *the-collision-table* #f)
(define *the-count-table* #f)

;;; "Imported" commonlisp procedures

(define *make-eq-hash-table*)
(define *put-hash*)
(define *get-hash*)
(define *get-hash-count*)
(define *bit-vector-p*)
(define *length*)
(define *aref*)
(define *char*)
(define *vectorp*)
(define *array-dimensions*)
(define *array-rank*)
(define *stringp*)
(define *find-symbol*)
(define *find-external-symbol*)

;;; Stream output-primitives

(define write-char (make-primitive-procedure 'cl-internal-write-char))
(define write-string (make-primitive-procedure 'cl-internal-write-string))
(define write-string-start-end  (make-primitive-procedure 'cl-internal-write-string-start-end))

;;; Circle stuff

;; Hash an object and return whether or not it was already there

(define (hash-it object)
  (let ((count (*get-hash* object *the-collision-table*)))
    (if count
	(begin
	  (*put-hash* object
		      (1+ count)
		      *the-collision-table*)
	  #t)
	(begin
	  (*put-hash* object
		      1
		      *the-collision-table*)
	  #f))))

(define (process-it object multiple-cont first-cont default-cont)
  (let ((collision (*get-hash* object *the-collision-table*)))
    (if (not collision)
	(default-cont)
	(let ((id (*get-hash* object *the-count-table*)))
	  (cond (id
		 (multiple-cont id))
		((> collision 1)
		 (let ((new-id (*get-hash-count* *the-count-table*)))
		   (*put-hash* object new-id *the-count-table*)
		   (first-cont new-id)))
		(else
		 (default-cont)))))))

(define (with-circle-detection f object currlevel)
  (process-it object
	      (lambda (id)
		(write-char #\#)
		(output-integer id currlevel)
		(write-char #\#))
	      (lambda (new-id)
		(write-char #\#)
		(output-integer new-id currlevel)
		(write-char #\=)
		(f object currlevel))
	      (lambda ()
		(f object currlevel))))

;;; Default printer

(define (output-default object currlevel)
  (write-string "#<" #f)
  (let ((type-name (object-type object)))
    (if (null? type-name)
	(begin
	  (write-string "(UNDEFINED-TYPE-CODE " #f)
	  (write-string (number->string (primitive-type object) 16) #f)
	  (write-char #\)))
	(write-string (symbol->string (object-type object)) #f)))
  (write-char #\Space)
  (write-string (number->string (primitive-datum object) 16) #f)
  (write-char #\>))

(define (output-default-circle-pass-1 object currlevel)
  (hash-it object))

;;; Dispatchers

;; There are three dispatcher vectors
;; 1. Normal output
;; 2. During circle printing, the first pass which just gathers information
;; 3. During circle printing, the second pass which prints
  
(define normal-dispatch-vector
  (vector-cons number-of-microcode-types output-default))

(set! *dispatch-vector* normal-dispatch-vector)

(define pass-1-dispatch-vector
  (vector-cons number-of-microcode-types output-default-circle-pass-1))

(define pass-2-dispatch-vector
  (vector-cons number-of-microcode-types output-default))

;; Add a printer for a particular scode type
;; If the pass-1-dispatcher is not supplied, don't hash this object for circles.
;; If the pass-1-dispatcher is supplied but is #t, hash this object for circles.
;; If the pass-2-dispatcher is not supplied, use the normal printer for pass-2.
;; If the pass-2-dispatcher is supplied but is #t, check for circles before using normal
;;   printer.

(define (add-printer! type dispatcher #!optional pass-1-dispatcher pass-2-dispatcher)
  (vector-set! normal-dispatch-vector
	       (microcode-type type)
	       dispatcher)
  (vector-set! pass-1-dispatch-vector
	       (microcode-type type)
	       (if (unassigned? pass-1-dispatcher)
		   (lambda (object currlevel) #f)
		   (if (eq? pass-1-dispatcher #t)
		       (lambda (object currlevel) (hash-it object))
		       pass-1-dispatcher)))
  (vector-set! pass-2-dispatch-vector
	       (microcode-type type)
	       (if (unassigned? pass-2-dispatcher)
		   dispatcher
		   (if (eq? pass-2-dispatcher #t)
		       (lambda (object currlevel)
			 (with-circle-detection dispatcher object currlevel))
		       pass-2-dispatcher))))

(define (output-object object currlevel)
  ((vector-ref *dispatch-vector* (non-touching-primitive-type object))
   object
   currlevel))

(define (output-symbol object currlevel)
  (let ((package (symbol-package object))
	(name (symbol-name object)))
    (if (null? package)
	(if (%function-symbol%? object)
	    (begin
	      (if (and *print-gensym* *print-escape*)
		  (write-string "#^" #f))
	      (*internal-symbol-output-function* name))
	    (begin
	      (if (and *print-gensym* *print-escape*)
		  (write-string "#:" #f))
	      (*internal-symbol-output-function* name)))
	(begin
	  (cond ((eq? package *package*))
		((eq? package *keyword-package*)
		 (write-char #\:))
		(else
		 (let ((found (memq package (package-use-list *package*))))
		   (prim-with-values 
		    (lambda () (*find-external-symbol* name package))
		    (lambda (symbol externalp)
		      (if (not (and found externalp (eq? symbol object)))
			  (prim-with-values
			   (lambda () (*find-symbol* name *package*))
			   (lambda (symbol accessible)
			     (if (not (and accessible (eq? symbol object)))
				 (begin (*internal-symbol-output-function* (package-name package))
					(if externalp
					    (write-char #\:)
					    (write-string "::" #f))))))))))))
	  (*internal-symbol-output-function* name)))))

(define (output-symbol-pass-1 object currlevel)
  (if (null? (symbol-package object))
      (hash-it object)
      #f))

(define cl-string-capitalize (make-primitive-procedure 'cl-string-capitalize))

(define funnychars (char-set #\" #\' #\` #\: #\; #\\ #\a #\b #\c #\d #\e #\f
			     #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
			     #\s #\t #\u #\v #\w #\x #\y #\z #\| #\space #\tab
			     #\page #\return #\linefeed #\newline))

(define number-leaders (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\+ #\-))

(define (needs-escaping? s)
  (and *print-escape*
       (escaped-symbol-name? s)))

(define (escaped-symbol-name? s)
  (or (zero? (string-length s))
      (string-find-next-char-in-set s funnychars)
      (looks-like-number? s)
      (all-dots? s)))

(define (looks-like-number? s)
  (and (or (not (= *print-base* 10))
	   (char-set-member? number-leaders (string-ref s 0)))
       (string->number s '() *print-base*)))

(define (all-dots? s)
  (and (char=? (string-ref s 0) #\.)
       (let loop ((n (-1+ (string-length s))))
	 (if (<= n 1)
	     #t
	     (and (char=? (string-ref s n) #\.)
		  (loop (-1+ n)))))))

(define (make-string-outputer string-modifier)
  (lambda (s)
    (if (needs-escaping? s)
	(begin
	  (write-char #\|)
	  (write-string (string-modifier s) #t)
	  (write-char #\|))
	(write-string (string-modifier s) #f))))

(define output-uppercase-string (make-string-outputer identity-procedure))
(define output-downcase-string (make-string-outputer string-downcase))
(define output-capitalize-string (make-string-outputer cl-string-capitalize))

(set! *internal-symbol-output-function* output-uppercase-string)

(define symbol-output-table
  (list (cons (keyword-intern "UPCASE")  output-uppercase-string)
	(cons (keyword-intern "DOWNCASE") output-downcase-string)
	(cons (keyword-intern "CAPITALIZE") output-capitalize-string)))

(define (get-new-string-outputer)
  (let ((tag (assq *print-case* symbol-output-table)))
    (if tag
	(cdr tag)
	(begin
	  (write-string "Resetting value of *PRINT-CASE* ..." #f)
	  (let ((old *print-case*))
	    (fluid-let ((*print-case* (keyword-intern "UPCASE")))
	      (error "Value of *PRINT-CASE* was bad: ~s" old)))))))

(define (output-null object currlevel)
  (if (eq? object #f)
      (output-symbol object currlevel)
      (output-default object currlevel)))

(define (output-true object currlevel)
  (if (eq? object #t)
      (output-symbol object currlevel)
      (output-default object currlevel)))

(add-printer! 'INTERNED-SYMBOL output-symbol output-symbol-pass-1 #t)
(add-printer! 'UNINTERNED-SYMBOL output-symbol output-symbol-pass-1 #t)
(add-printer! 'NULL output-null)
(add-printer! 'TRUE output-true)

#f ; To avoid unassigned error due to compiled set!

'$split-file

(define (output-list-pass-1 l currlevel)
  (cond ((non-printing-object? l) #f)
	((and *print-level*
	      (>= currlevel *print-level*))
	 #f)
	(else
	 (if (not (hash-it l))
	     (begin
	       (output-object (car l) (1+ currlevel))
	       (output-tail-pass-1 (cdr l) (1+ currlevel) 1))
	     #f))))

(define (output-tail-pass-1 l currlevel currlen)
  (cond ((pair? l)
	 (if (not (hash-it l))
	     (if (and *print-length*
		      (>= currlen *print-length*)
		      (not (null? (cdr l))))
		 #f
		 (begin
		   (output-object (car l) currlevel)
		   (output-tail-pass-1 (cdr l) currlevel (1+ currlen))))))
	((not (null? l))
	 (output-object l currlevel))
	(else 
	 #f)))

(define (output-list-pass-2 l currlevel)
  (with-circle-detection do-output-list-pass-2 l currlevel))

(define (do-output-list-pass-2 l currlevel)
  (cond ((and (eq? (car l) 'QUOTE)
	      (pair? (cdr l))
	      (null? (cddr l)))
	 (write-char #\')
	 (output-object (cadr l) currlevel))
	((non-printing-object? l))
	((and *print-level*
	      (>= currlevel *print-level*))
	 (write-char #\#))
	(else
	 (write-char #\()
	 (output-object (car l) (1+ currlevel))
	 (output-tail-pass-2 (cdr l) (1+ currlevel) 1)
	 (write-char #\)))))

(define (output-tail-pass-2 l currlevel currlen)
  (process-it l
	      (lambda (id)
		(write-string " . #" #f)
		(output-integer id currlevel)
		(write-char #\#))
	      (lambda (new-id)
		(write-string " . #" #f)
		(output-integer new-id currlevel)
		(write-char #\=)
		(output-object l currlevel))
	      (lambda ()
		(do-tail-pass-2 l currlevel currlen))))

(define (do-tail-pass-2 l currlevel currlen)
  (cond ((pair? l)
	 (write-char #\space)
	 (if (and *print-length*
		  (>= currlen *print-length*)
		  (not (null? (cdr l))))
	     (write-string "..." #f)
	     (begin
	       (output-object (car l) currlevel)
	       (output-tail-pass-2 (cdr l) currlevel (1+ currlen)))))
	((not (null? l))
	 (write-string " . " #f)
	 (output-object l currlevel))))

(define (output-list l currlevel)
  (cond ((and (not (future? (car l)))
	      (eq? (car l) 'QUOTE)
	      (pair? (cdr l))
	      (null? (cddr l)))
	 (write-char #\')
	 (output-object (cadr l) currlevel))
	((non-printing-object? l))
	((and *print-level*
	      (>= currlevel *print-level*))
	 (write-char #\#))
	(else
	 (write-char #\()
	 (output-object (car l) (1+ currlevel))
	 (output-tail (cdr l) (1+ currlevel) 1)
	 (write-char #\)))))

(define (output-tail l currlevel currlen)
  (cond ((or (future? l)
	     (and (not (pair? l))
		  (not (null? l))))
	 (write-string " . " #f)
	 (output-object l currlevel))
        ((pair? l)
	 (write-char #\space)
	 (if (and *print-length*
		  (>= currlen *print-length*)
		  (not (null? (cdr l))))
	     (write-string "..." #f)
	     (begin
	       (output-object (car l) currlevel)
	       (output-tail (cdr l) currlevel (1+ currlen)))))))

(add-printer! 'LIST output-list output-list-pass-1 output-list-pass-2)

(define (output-vector vector currlevel)
  (cond ((and (not (null? *print-level*))
	      (>= currlevel *print-level*))
	 (write-char #\#))
	((null? *print-array*)
	 (output-terse-array vector currlevel))
	(else
	 (let ((not-bit-vector-p (not (*bit-vector-p* vector)))
	       (vlength (*length* vector)))
	   (if not-bit-vector-p
	       (write-string "#(" #f)
	       (write-string "#*" #f))
	   (let loop ((currlength 0) (not-first-pass #f))
	     (cond (*print-length*
		    (if (or (>= currlength *print-length*)
			    (= currlength vlength))
			(begin
			  (if (not (= currlength vlength)) (write-string " ..." #f))
			  (if not-bit-vector-p
			      (write-char #\))))
			(begin
			  (if (and (> currlength 0)
				   not-bit-vector-p)
			      (write-char #\space))
			  (output-object (*aref* vector currlength)
					 (1+ currlevel))
			  (loop (1+ currlength) #t))))
		   ((not (= currlength vlength))
		    (if (and not-first-pass not-bit-vector-p)
			(write-char #\space))
		    (output-object (*aref* vector currlength) (1+ currlevel))
		    (loop (1+ currlength) #t))
		   (not-bit-vector-p
		    (write-char #\)))))))))

(define (output-vector-pass-1 vector currlevel)
  (cond ((and (not (null? *print-level*))
	      (>= currlevel *print-level*))
	 #f)
	((null? *print-array*)
	 (hash-it vector))
	(else
	 (if (not (hash-it vector))
	     (if (not (*bit-vector-p* vector))
		 (let ((vlength (*length* vector)))
		   (let loop ((currlength 0))
		     (cond (*print-length*
			    (if (or (>= currlength *print-length*)
				    (= currlength vlength))
				#f
				(begin
				  (output-object (*aref* vector currlength) (1+ currlevel))
				  (loop (1+ currlength)))))
			   ((not (= currlength vlength))
			    (output-object (*aref* vector currlength) (1+ currlevel))
			    (loop (1+ currlength)))
			   (else #f)))))))))

(define (output-scheme-vector vector currlevel)
  (if (pathname? vector)
      (output-pathname vector currlevel)
      (output-vector vector currlevel)))

(define (output-scheme-vector-pass-1 vector currlevel)
  (if (pathname? vector)
      (hash-it vector)
      (output-vector-pass-1 vector currlevel)))

(define (output-pathname p currlevel)
  (write-string "#P" #f)
  (fluid-let ((*print-escape* #t))
    (output-string (pathname->string p) currlevel)))

(add-printer! 'VECTOR-1B output-vector output-vector-pass-1 #t)
(add-printer! 'VECTOR output-scheme-vector output-scheme-vector-pass-1 #t)

(define (output-array array currlevel)
  (let ((rank (*array-rank* array)))
    (cond ((and (not (null? *print-level*))
		(>= currlevel *print-level*))
	   (write-char #\#))
	  ((= rank 1)
	   (if (*stringp* array)
	       (output-array-string array currlevel)
	       (output-vector array currlevel)))
	  ((not *print-array*)
	   (output-terse-array array rank))
	  (else
	   (write-char #\#)
	   (fluid-let ((*print-base* 10))
	     (output-integer rank currlevel))
	   (write-char #\A)
	   (let ((dims (*array-dimensions* array)))
	     (if (null? dims)
		 (output-object (*aref* array) currlevel)
		 (prim-with-values (lambda () (find-data-vector array))
				   (lambda (array offset)
				     (output-array-guts array
							dims
							currlevel 
							offset
							(preprocess-dimensions dims))))))))))

(define (output-array-guts array dims currlevel index product-list)
  (cond ((null? (cdr dims))
	 (write-char #\()
	 (let inner-loop ((i index)
			  (end (+ index (car dims))))
	   (cond ((= i end)
		  #t)
		 ((and *print-length* (= i *print-length*))
		  (write-string "..." #f))
		 (else
		  (output-object (vector-ref array i)
				      currlevel)
		  (if (not (= end (1+ i)))
		      (write-char #\space))
		  (inner-loop (1+ i) end))))
	 (write-char #\)))
	((and (not (null? *print-level*))
	      (>= currlevel *print-level*))
	 (write-char #\#))
	(else (write-char #\()
	      (let outer-loop ((i index)
			       (end (+ index (car product-list))))
		(if (= i end)
		    #t
		    (begin (output-array-guts array 
					      (cdr dims)
					      (1+ currlevel)
					      i
					      (cdr product-list))
			   (if (not (= end (+ i (cadr product-list))))
			       (write-char #\space))
			   (outer-loop (+ i (cadr product-list)) end))))
	      (write-char #\)))))

(define (output-array-pass-1 array currlevel)
  (if (not (hash-it array))
      (let ((rank (*array-rank* array)))
	(cond ((and (not (null? *print-level*))
		    (>= currlevel *print-level*))
	       #f)
	      ((= rank 1)
	       (if (*stringp* array)
		   #f
		   (output-vector-pass-1 array currlevel)))
	      ((not *print-array*)
	       #f)
	      (else
	       (let ((dims (*array-dimensions* array)))
		 (if (null? dims)
		     (output-object (*aref* array) currlevel)
		     (prim-with-values (lambda () (find-data-vector array))
				       (lambda (array offset)
					 (output-array-guts-pass-1 array
								   dims
								   currlevel 
								   offset
								   (preprocess-dimensions dims)))))))))))

(define (output-array-guts-pass-1 array dims currlevel index product-list)
  (cond ((null? (cdr dims))
	 (let inner-loop ((i index)
			  (end (+ index (car dims))))
	   (cond ((= i end)
		  #t)
		 ((and *print-length* (= i *print-length*))
		  #f)
		 (else
		  (output-object (vector-ref array i) currlevel)
		  (inner-loop (1+ i) end)))))
	((and (not (null? *print-level*))
	      (>= currlevel *print-level*))
	 #f)
	(else
	 (let outer-loop ((i index)
			  (end (+ index (car product-list))))
	   (if (= i end)
	       #t
	       (begin (output-array-guts-pass-1 array 
						(cdr dims)
						(1+ currlevel)
						i
						(cdr product-list))
		      (outer-loop (+ i (cadr product-list)) end)))))))

(define (output-terse-array array rank)
  (write-string "#<" #f)
  (if (*vectorp* array)
      (if (*bit-vector-p* array)
	  (write-string "Bit-vector" #f)
	  (write-string "Vector " #f))
      (begin
	(write-string "Array, rank " #f)
	(output-integer rank 0)
	(write-char #\space)))
  (write-string (number->string (primitive-datum array) 16) #f)
  (write-char #\>))

(define find-data-vector (make-primitive-procedure 'cl-find-data-vector))

(define (preprocess-dimensions dims)
  (if (null? (cdr dims))
      dims
      (let ((ans (preprocess-dimensions (cdr dims))))
	(cons (* (car dims) (car ans))
	      ans))))

(add-printer! 'CL-ARRAY output-array output-array-pass-1 #t)
(add-printer! 'CL-I-VECTOR output-array output-array-pass-1 #t)

(define (output-structure s currlevel)
  (if (and (not (null? *print-level*))
	   (>= currlevel *print-level*))
      (write-char #\#)
      (let ((info (get
		   ((symbol-function 'g-vector-ref) s 0)
		   '%structure-definition)))
	(if info
	    ((let ((fcn ((symbol-function 'dd-print-function) info)))
	       (if (symbol? fcn)
		   (symbol-function fcn)
		   fcn))
	     s
	     *standard-output*
	     currlevel)
	    (begin
	      (write-string "#<Unprintable Structure " #f)
	      (write-string (number->string (primitive-datum s) 16) #f)
	      (write-char #\>))))))

(add-printer! 'G-VECTOR output-structure #t #t)

#f ; To avoid unassigned error due to compiled set!

'$split-file


;;; Quick scan to see if there is an escape in the string

(define funny-string-chars (char-set #\" #\\ #\linefeed #\newline))

(define (escape-string? s)
  (string-find-next-char-in-set s funny-string-chars))

(define (output-string s currlevel)
  (if *print-escape*
      (begin (write-char #\")
	     (if (escape-string? s)
		 (let ((len (*length* s)))
		   (let loop ((i 0))
		     (if (= i len)
			 'done
			 (let ((c (*char* s i)))
			   (if (char-set-member? funny-string-chars c)
			       (write-char #\\))
			   (write-char c)
			   (loop (1+ i))))))
		 (write-string s #f))
	     (write-char #\"))
      (write-string s #t)))

(define (output-array-string string currlevel)
  (prim-with-values (lambda () (find-data-vector string))
		    (lambda (s offset)
		      (output-string (substring s offset (+ offset (*length* string)))
				     currlevel))))

(add-printer! 'STRING output-string #t #t)

(define (primitive-output-integer i currlevel)
  (write-string
   ((access unparse-number-heuristically number-unparser-package)
    i
    *print-base*
    '()
    '())
   #f))

(define (default-base-print)
  (write-string
   ((access unparse-number-heuristically number-unparser-package)
    *print-base*
    10
    '()
    '())
   #f)
  #\r)

(define base-dispatch-vector (vector-cons 37 default-base-print))

(define (add-special-base-printer! base printer)
  (vector-set! base-dispatch-vector base printer))

(define (get-base-information)
  ((vector-ref base-dispatch-vector *print-base*)))

(add-special-base-printer! 2 (lambda () #\b))
(add-special-base-printer! 8 (lambda () #\o))
(add-special-base-printer! 16 (lambda () #\x))

(define (with-radix f)
  (if (and *print-radix*
	   (not (= *print-base* 10)))
      (begin (write-char #\#)
	     (write-char (get-base-information))))
  (f)
  (if (and *print-radix*
	   (= *print-base* 10)) 
      (write-char #\.)))

(define (output-integer i currlevel)
  (with-radix
   (lambda ()
     (primitive-output-integer i currlevel))))

(add-printer! 'FIXNUM output-integer)
(add-printer! 'BIGNUM output-integer)

(define numerator (make-primitive-procedure 'ratio-numerator))
(define denominator (make-primitive-procedure 'ratio-denominator))
(define realpart (make-primitive-procedure 'realpart))
(define imagpart (make-primitive-procedure 'imagpart))

(define (output-rational r currlevel)
  (with-radix
   (lambda ()
     (primitive-output-integer (numerator r) currlevel)
     (write-char #\/)
     (primitive-output-integer (denominator r) currlevel))))

(add-printer! 'RATIO output-rational)

(define (output-complex c currlevel)
  (write-string "#C(" #f)
  (output-object (realpart c) currlevel)
  (write-char #\space)
  (output-object (imagpart c) currlevel)
  (write-char #\)))

(add-printer! 'COMPLEX output-complex)

(define e-min 1.0e-3)
(define e-max 1.0e7)

(define (get-digit-string)
  (let ((z (stream-info *standard-output*)))
    (if (null? z)
	(begin (set-stream-info! *standard-output* (make-string 128))
	       (stream-info *standard-output*))
	z)))

(define format-fixed-flonum-to-string (make-primitive-procedure 'cl-format-fixed-flonum-to-string))
(define computerized-flonum-to-string (make-primitive-procedure 'cl-computerized-flonum-to-string))

(define (output-float x currlevel)
  (cond ((zero? x)
	 (write-string "0.0" #f))
	((float-infinity? x)
	 (if (negative? x)
	     (write-string "#<-Infinity>" #f)
	     (write-string "#<+Infinity>" #f)))
	((float-nan? x)
	 (write-string "#<Floating-Point-NAN>" #f))
	((< e-min (abs x) e-max)
	 (let ((len (format-fixed-flonum-to-string (abs x) (get-digit-string) nil nil nil)))
	   (if (negative? x) (write-char #\-))
	   (write-string-start-end (get-digit-string) #f 0 len)))
	(else
	 (let ((len (computerized-flonum-to-string x (get-digit-string))))
	   (write-string-start-end (get-digit-string) #f 0 len)))))

(add-printer! 'FLONUM output-float)

(define (output-character c currlevel)
  (if (not *print-escape*)
      (write-char c)
      (let ((bits (char-bits c)))
	(write-string "#\\" #f)
	(if (not (zero? bits))
	    (begin
	      (if (control-bit? bits)
		  (write-string "Control-" #f))
	      (if (meta-bit? bits)
		  (write-string "Meta-" #f))
	      (if (super-bit? bits)
		  (write-string "Super-" #f))
	      (if (hyper-bit? bits)
		  (write-string "Hyper-" #f))
	      (let ((char (code->char (char-code c))))
		(if (char-lower-case? char)
		    (write-char #\\))
		(write-string (char->name char) #f)))
	    (write-string (char->name c) #f)))))

(add-printer! 'CHARACTER output-character)

(define (output-package p currlevel)
  (write-string "#<" #f)
  (write-string (package-name p) #t)
  (write-string " package>" #f))

(add-printer! 'CL-PACKAGE output-package)

;; So some scheme things print ok

(define (output-compound-procedure procedure currlevel)
  (lambda-components* (procedure-lambda procedure)
    (lambda (name required optional rest body)
      (write-string "#<COMPOUND-PROCEDURE " #f)
      (if (eq? name lambda-tag:unnamed)
	  (write-string (number->string (primitive-datum procedure) 16) #f)
	  (write-string (symbol-name name) #f))))
  (write-char #\>))

(add-printer! 'PROCEDURE output-compound-procedure)
(add-printer! 'EXTENDED-PROCEDURE output-compound-procedure)

(define (output-primitive-procedure proc currlevel)
  (write-string "#<PRIMITIVE-PROCEDURE " #f)
  (write-string (symbol-name (primitive-procedure-name proc)) #f)
  (write-char #\>))

(add-printer! 'PRIMITIVE output-primitive-procedure)

(define (output-compiled-entry entry currlevel)
  (write-string "#<" #f)
  (write-string (symbol->string (compiled-entry-type entry)) #f)
  (write-char #\space)
  (write-string (number->string (primitive-datum entry) 16) #f)
  (write-char #\>))

(add-printer! 'COMPILED-ENTRY output-compiled-entry)

(define (output-readtable readtable currlevel)
  (write-string "#<Readtable " #f)
  (write-string (number->string (primitive-datum readtable) 16) #f)
  (write-char #\>))

(define (output-environment e currlevel)
  (cond
   (((symbol-function 'readtablep) e)
    (output-readtable e currlevel))
   (else
    (output-default e currlevel))))

(add-printer! 'ENVIRONMENT output-environment #t #t)

(define output-cl-stream)

(let ((closed-stream?               (make-primitive-procedure 'cl-closed-stream?))
      (file-stream?                 (make-primitive-procedure 'cl-file-stream?))
      (string-stream?               (make-primitive-procedure 'cl-string-stream?))
      (tty-stream?                  (make-primitive-procedure 'cl-tty-stream?))
      (broadcast-stream?            (make-primitive-procedure 'cl-broadcast-stream?))
      (concatenated-stream?         (make-primitive-procedure 'cl-concatenated-stream?))
      (synonym-stream?              (make-primitive-procedure 'cl-synonym-stream?))
      (two-way-stream?              (make-primitive-procedure 'cl-two-way-stream?))
      (echo-stream?                 (make-primitive-procedure 'cl-echo-stream?))
      (stream-pathname              (make-primitive-procedure 'cl-stream-pathname))
      (broadcast-stream-streams     (make-primitive-procedure 'cl-broadcast-stream-streams))
      (concatenated-stream-streams  (make-primitive-procedure 'cl-concatenated-stream-streams))
      (synonym-stream-symbol        (make-primitive-procedure 'cl-synonym-stream-symbol))
      (two-way-stream-input-stream  (make-primitive-procedure 'cl-two-way-stream-input-stream))
      (two-way-stream-output-stream (make-primitive-procedure 'cl-two-way-stream-output-stream))
      (echo-stream-input-stream     (make-primitive-procedure 'cl-echo-stream-input-stream))
      (echo-stream-output-stream    (make-primitive-procedure 'cl-echo-stream-output-stream)))
  (define (output-std-stream stream name)
    (output-std-stream-prelude stream name)
    (write-char #\>))
  (define (output-std-stream-prelude stream name)
    (write-string "#<" #f)
    (write-string name #f)
    (write-char #\Space)
    (write-string (number->string (primitive-datum stream) 16) #f))
  (set! output-cl-stream
	(named-lambda (output-cl-stream stream currlevel)
	  (cond
	   ((closed-stream? stream)
	    (output-std-stream stream "Closed Stream"))
	   ((file-stream? stream)
	    (output-std-stream-prelude stream "File Stream")
	    (write-char #\Space)
	    (output-pathname (stream-pathname stream) currlevel)
	    (write-char #\>))
	   ((string-stream? stream)
	    (output-std-stream stream "String Stream"))
	   ((tty-stream? stream)
	    (output-std-stream stream "TTY Stream"))
	   ((broadcast-stream? stream)
	    (output-std-stream-prelude stream "Broadcast Stream")
	    (write-char #\Space)
	    (output-object (broadcast-stream-streams stream) currlevel)
	    (write-char #\>))
	   ((concatenated-stream? stream)
	    (output-std-stream-prelude stream "Concatenated Stream")
	    (write-char #\Space)
	    (output-object (concatenated-stream-streams stream) currlevel)
	    (write-char #\>))
	   ((synonym-stream? stream)
	    (output-std-stream-prelude stream "Synonym Stream")
	    (write-char #\Space)
	    (output-symbol (synonym-stream-symbol stream) currlevel)
	    (write-char #\>))
	   ((two-way-stream? stream)
	    (output-std-stream-prelude stream "Two Way Stream")
	    (write-string " Input " #f)
	    (output-cl-stream (two-way-stream-input-stream stream) currlevel)
	    (write-string " Output " #f)
	    (output-cl-stream (two-way-stream-output-stream stream) currlevel)
	    (write-char #\>))
	   ((echo-stream? stream)
	    (output-std-stream-prelude stream "Echo Stream")
	    (write-string " Input " #f)
	    (output-cl-stream (echo-stream-input-stream stream) currlevel)
	    (write-string " Output " #f)
	    (output-cl-stream (echo-stream-output-stream stream) currlevel)
	    (write-char #\>))))))

(add-printer! 'CL-STREAM output-cl-stream #t)

(define (output-future object currlevel)
  (write-string "#<FUTURE " #f)
  (write-string (number->string (primitive-datum object) 16) #f)
  (write-char #\space)
  (write-string (number->string (future-ref object future-metering-slot) 10) #f)
  (write-char #\>))

(add-printer! 'FUTURE output-future)

;;; The commonlisp unparser

(set! cl-unparse-object
      (lambda (object)
	(let ((kernel
	       (lambda ()
		 (fluid-let ((*internal-symbol-output-function* (get-new-string-outputer)))
		   (output-object object 0)))))
	  (if *print-circle*
	      (fluid-let ((*the-collision-table*
			   (or *the-collision-table* (*make-eq-hash-table*)))
			  (*the-count-table*
			   (or *the-count-table* (*make-eq-hash-table*)))
			  (*dispatch-vector* pass-1-dispatch-vector))
		(kernel)
		(if (zero? (*get-hash-count* *the-collision-table*))
		    (fluid-let ((*dispatch-vector* normal-dispatch-vector))
		      (kernel))
		    (fluid-let ((*dispatch-vector* pass-2-dispatch-vector))
		      (kernel))))
	      (kernel)))))

#f ; To avoid unassigned error due to compiled set!
