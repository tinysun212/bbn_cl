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
;;;;;;; Interface to microcoded packages.

(declare (usual-integrations))

;;; Package constructors and accessors

(define make-cl-package (make-primitive-procedure 'make-cl-package))

(define cl-package? (microcode-type-predicate 'cl-package))

(define (package-name p)
  (if (cl-package? p)
      (system-vector-ref p 0)
      (error "~a is not a package" p)))

(define (%set-package-name p value)
  (if (cl-package? p)
      (system-vector-set! p 0 value)
      (error "~a is not a package" p)))

(define (package-nicknames p)
  (if (cl-package? p)
      (system-vector-ref p 1)
      (error "~a is not a package" p)))

(define (%set-package-nicknames p value)
  (if (cl-package? p)
      (system-vector-set! p 1 value)
      (error "~a is not a package" p)))

(define (package-use-list p)
  (if (cl-package? p)
      (system-vector-ref p 2)
      (error "~a is not a package" p)))

(define (%set-package-use-list p value)
  (if (cl-package? p)
      (system-vector-set! p 2 value)
      (error "~a is not a package" p)))

(define (package-used-by-list p)
  (if (cl-package? p)
      (system-vector-ref p 3)
      (error "~a is not a package" p)))

(define (%set-package-used-by-list p value)
  (if (cl-package? p)
      (system-vector-set! p 3 value)
      (error "~a is not a package" p)))

(define (package-shadowing-symbols p)
  (if (cl-package? p)
      (system-vector-ref p 4)
      (error "~a is not a package" p)))

(define (%set-package-shadowing-symbols p value)
  (if (cl-package? p)
      (system-vector-set! p 4 value)
      (error "~a is not a package" p)))

(define (package-tables p)
  (if (cl-package? p)
      (system-vector-ref p 5)
      (error "~a is not a package" p)))

(define (%set-package-tables p value)
  (if (cl-package? p)
      (system-vector-set! p 5 value)
      (error "~a is not a package" p)))

(define (package-internal-symbols p)
  (if (cl-package? p)
      (system-vector-ref p 6)
      (error "~a is not a package" p)))

(define (%set-package-internal-symbols p value)
  (if (cl-package? p)
      (system-vector-set! p 6 value)
      (error "~a is not a package" p)))

(define (package-external-symbols p)
  (if (cl-package? p)
      (system-vector-ref p 7)
      (error "~a is not a package" p)))

(define (%set-package-external-symbols p value)
  (if (cl-package? p)
      (system-vector-set! p 7 value)
      (error "~a is not a package" p)))

;;; Package/symbol manipulations

;; add a symbol to a hash table
(define pkg-add-symbol (make-primitive-procedure 'pkg-add-symbol-to-hash-table))

(define (add-symbol table symbol)
  (pkg-add-symbol table (un-nil-or-t-ify symbol)))

;; get a symbol from a hash table
(define get-symbol (make-primitive-procedure 'pkg-get-symbol-from-hash-table))
(define nuke-symbol (make-primitive-procedure 'pkg-remove-symbol-from-hash-table))

(define pkg-find-symbol (make-primitive-procedure 'pkg-find-symbol))
(define pkg-intern-string (make-primitive-procedure 'pkg-intern-string))

(define make-obarray (make-primitive-procedure 'make-obarray))

(define (get-package-names)
  (vector-ref (get-fixed-objects-vector)
	      (fixed-objects-vector-slot 'cl-system-package)))

(define pkg-lookup-name (make-primitive-procedure 'pkg-lookup-name))
(define pkg-add-name (make-primitive-procedure 'pkg-add-name))
(define pkg-remove-name (make-primitive-procedure 'pkg-remove-name))

(define pkg-gethash pkg-lookup-name)

(define (%pkg-sethash name table pkg)
  (pkg-add-name name table pkg))

(define (pkg-remhash name table)
  (pkg-remove-name name table))

;;; Package system initialization

;; Some useful packages

(define *bbnaci-package*)
(define *package*)
(define *keyword-package*)
;;;
;;; Package vars below are assigned in setup-packages
;;;
(define *lisp-package*   '())
(define *system-package* '())
(define *user-package*   '())

;;allocate the package-names table
;;
(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'cl-system-package)
	     (vector-cons 23 '()))

;; Make the scheme and keyword packages

(let ((pkg (make-cl-package "BBNACI")))
  (set! *bbnaci-package* pkg)
  (%set-package-internal-symbols
   pkg
   (vector-ref (get-fixed-objects-vector)
	       (fixed-objects-vector-slot 'obarray)))
  (%set-package-external-symbols pkg (make-obarray))
  (%set-package-tables pkg (list '())))

(let ((pkg (make-cl-package "KEYWORD")))
  (set! *keyword-package* pkg)
  (%set-package-internal-symbols pkg (make-obarray))
  (%set-package-external-symbols pkg (make-obarray))
  (%set-package-tables pkg (list '())))

;; put packages in pkg-list now, by hand

(%pkg-sethash "BBNACI" (get-package-names) *bbnaci-package*)
(%pkg-sethash "KEYWORD" (get-package-names) *keyword-package*)

;; Setup *package*

(set! *package* *bbnaci-package*)

;;; Keyword utilities

(define (keyword-intern string)
  (pkg-intern-string string (string-length string) *keyword-package*))

(define (keyword? s)
  (and (%expanded-symbol? s)
       (eq? (symbol-package s) *keyword-package*)))

;; Symbol-table-iterator

(define (symbol-table-iterator proc hash-table)
  (let loop-i ((i (-1+ (vector-length hash-table))))
    (if (not (negative? i))
	(sequence
	 (let loop-j ((j (vector-ref hash-table i)))
	   (if (not (null? j))
	       (sequence
		(proc (nil-or-t-ify (car j)))
		(loop-j (cdr j)))))
	 (loop-i (-1+ i))))))


;; Stuff so the Scheme system can output the new types of symbols

(define symbol->string (make-primitive-procedure 'cl-get-symbol-name))
(define symbol-print-name symbol->string)

(in-package unparser-package

  (set! unparse-symbol
	(lambda (symbol)
	  (cond ((keyword? symbol)
	      	 (*unparse-char #\:))
		((null? (symbol-package symbol))
		 (*unparse-string "#:")))
	  (*unparse-string (symbol->string symbol))))

  ;;redefine-type so that we side-affect table with our new-definition
  ;;of unparse-symbol

  (define-type 'INTERNED-SYMBOL
    unparse-symbol)

  (define-type 'UNINTERNED-SYMBOL
    (lambda (symbol)
      (cond ((%function-symbol? symbol)
	     (*unparse-string "#^")
	     (*unparse-string (symbol->string symbol)))
	    (else
	     (unparse-with-brackets
	      (lambda ()
		(*unparse-string "UNINTERNED ")
		(unparse-symbol symbol)
		(*unparse-char #\Space)
		(*unparse-object (object-hash symbol))))))))

  (define-type 'CL-PACKAGE
    (lambda (pkg)
      (unparse-with-brackets
       (lambda ()
	 (*unparse-object (package-name pkg))
	 (*unparse-char #\Space)
	 (*unparse-string "package")))))

)

(in-package scheme-pretty-printer
  
  (define (*unparse-symbol symbol)
    (cond ((%function-symbol? symbol)
	   (*unparse-string "#^"))
	  ((keyword? symbol)
	   (*unparse-string ":"))
	  ((null? (symbol-package symbol))
	   (*unparse-string "#:")))
    (*unparse-string (symbol->string symbol)))

)
