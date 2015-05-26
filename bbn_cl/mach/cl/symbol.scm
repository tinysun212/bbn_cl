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
;;;;;; Basic low-level symbol manipulators. Contains code needed to access the
;;; package cell, property list, etc.


(declare (usual-integrations))

(define make-symbol (make-primitive-procedure 'make-symbol))

;;; Expanded symbols have a CLSAV (Common Lisp Symbol Attribute Vector).
;;;

(define *name-slot*            0)
(define *function-symbol-slot* 1)
(define *plist-slot*           2)
(define *package-slot*         3)
(define *hash-slot*            4)
(define *system-info-slot*     5)

(define %get-symbol-name (make-primitive-procedure 'cl-get-symbol-name))
(define %add-clsav (make-primitive-procedure 'cl-add-clsav))
(define %get-clsav (make-primitive-procedure 'cl-get-clsav))
(define %expanded-symbol%? (make-primitive-procedure 'cl-expanded-symbol?))
(define %function-symbol%? (make-primitive-procedure 'cl-function-symbol?))
(define %function-symbol-parent% (make-primitive-procedure 'cl-get-function-symbol-parent))
(define fast-symbol-plist (make-primitive-procedure 'cl-fast-symbol-plist))
(define fast-system-symbol-plist (make-primitive-procedure 'cl-fast-system-symbol-plist))

;; Setup the NIL symbol. Since a symbol named "NIL" is already in the scheme obarray
;;  (scheme defines it), we just grab that symbol, expand it, and place as the-nil-symbol
;;  in the fixobj vector. Later, it will be moved to the right place with everything else.
;; We do the same with the T symbol.
;; This code assumes cl-ucode-mode is off, which we continue to guarantee
;;  before and within bootcl.

(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'cl-nil-symbol)
	     (string->symbol "NIL"))

;; The T symbol is in the cl fixed objects vector.,
;;  therefore we can't grab it at closure-build time.
;; How inconsistent!

(define (the-nil-symbol)
  (vector-ref (get-fixed-objects-vector)
	      (fixed-objects-vector-slot 'cl-nil-symbol)))

(define (the-t-symbol)
  (%get-cl-fixed-obj 16))

;;;
;;; Note that we grab the NIL symbol and keep it in the closure --
;;;  we don't want the performance drag of calling the function
;;;  each time.

(define un-nil-or-t-ify
  (let ((nil-sym (the-nil-symbol)))
    (lambda (symbol)
      (cond ((null? symbol) nil-sym)
	    ((eq? symbol #t) (the-t-symbol))
	    (else symbol)))))

(define nil-or-t-ify
  (let ((nil-sym (the-nil-symbol)))
    (lambda (symbol)
      (cond ((eq? symbol nil-sym) #f)
	    ((eq? symbol (the-t-symbol)) #t)
	    (else symbol)))))

(define (%expanded-symbol? symbol)
  (%expanded-symbol%? (un-nil-or-t-ify symbol)))

(define (%function-symbol? symbol)
  (%function-symbol%? (un-nil-or-t-ify symbol)))

(define (%function-symbol-parent symbol)
  (%function-symbol-parent% (un-nil-or-t-ify symbol)))

;;;
;;; Gets info block associated with symbol; creates an info block if needed.
;;;

;;;Here's the story with packages and symbols.  The issue here is, if we're
;;;GET-SYMBOL-INFO'ing a symbol which is not expanded, what, if anything, do
;;;we put in the symbol's package slot?  The reason we don't just leave it
;;;empty is that there are a lot of Scheme (BBNACI) symbols (which, by definition,
;;;are in the LISP package and are considered interned) which will be
;;;considered uninterned by CommonLisp simply because the package slot is
;;;NIL.  So when we get these symbols and try to expand them we have to put
;;;*bbnaci-package* in their package slots.
;;;
;;;GET-SYMBOL-INFO will only ever be called with 2 types of unexpanded
;;;symbols.  The first is a symbol tagged TC_INTERNED_SYMBOL which lives in
;;;the SCHEME OBARRAY; it gets the lisp package stuffed in its slot.  The
;;;second kind of symbol is tagged TC_UNINTERNED_SYMBOL and lives in no
;;;obarray; it gets NIL stuffed in its slot.
;;;
;;;Whenever CommonLisp creates a symbol it will expanded it right then and
;;;there.  Therefore, GET-SYMBOL-INFO won't have to deal with symbols which
;;;are tagged TC_INTERNED_SYMBOL and aren't SCHEME symbols, simply because
;;;by the time GET-SYMBOL-INFO is called the symbol will already expanded
;;;(and package slot will already be initialized).

(define *interned-symbol-type-code*
  (primitive-type 'nice-name-for-an-interned-symbol))

(define (get-symbol-info sym)
  (let ((symbol (un-nil-or-t-ify sym)))
    (if (not (symbol? symbol))
      	(error "Attempt to get symbol info of ~a" symbol)
      	(sequence
	 (if (%function-symbol? symbol)
	     (error "Attempt to get symbol info of function symbol ~a" symbol)
	     (if (not (%expanded-symbol? symbol))
      	     	 (sequence
		  (%add-clsav symbol)
		  (if (eq? (primitive-type symbol) *interned-symbol-type-code*)
		      (%set-symbol-package! symbol *bbnaci-package*)))))
	 (%get-clsav symbol)))))

(define (symbol-name sym)
  (%get-symbol-name (un-nil-or-t-ify sym)))

(define (symbol-function symbol)
  (let ((function-symbol (fundefsym symbol)))
    (if (lexical-unassigned? system-global-environment function-symbol)
	(error "Symbol ~a has no function definition" symbol)
	(lexical-reference system-global-environment function-symbol))))

(define (%set-symbol-function! symbol fcn)
  (local-assignment '() (fundefsym symbol) fcn)
  fcn)

;; This is optimized by not expanding SYMBOL if it's not already
;; expanded.  This is because we already know what the package
;; is if this is an unexpanded symbol.  Either the symbol is
;; and unexpanded/interned symbol, in which case it is interned
;; in the *bbnaci-package*, or unexpanded/uninterned symbol, in which
;; case its package is NIL.
;;
(define (symbol-package symbol)
  (cond ((%expanded-symbol? symbol)
      	 (system-vector-ref (get-symbol-info symbol) *package-slot*))
	((eq? (primitive-type symbol) *interned-symbol-type-code*)
	 *bbnaci-package*)
	(else
	 '())))

(define (%set-symbol-package! symbol package)
  (system-vector-set! (get-symbol-info symbol) *package-slot* package))

(define set-package %set-symbol-package!)

;; This is optimized NOT to make SYMBOL an expanded symbol unnecessarily.
;;
(define (symbol-plist symbol)
  (let ((plist (fast-symbol-plist symbol)))
    (if (not (eq? plist #t))
	plist
	(let ((s (un-nil-or-t-ify symbol)))
	  (if (%expanded-symbol%? s)
	      (system-vector-ref (%get-clsav s) *plist-slot*)
	      '())))))

(define (symbol-system-plist symbol)
  (let ((plist (fast-system-symbol-plist symbol)))
    (if (not (eq? plist #t))
	plist
	(let ((s (un-nil-or-t-ify symbol)))
	  (if (%expanded-symbol%? s)
	      (system-vector-ref (%get-clsav s) *system-info-slot*)
	      '())))))

(define (%set-symbol-plist! symbol plist)
  (system-vector-set! (get-symbol-info symbol) *plist-slot* plist))

(define (%set-symbol-system-plist! symbol plist)
  (system-vector-set! (get-symbol-info symbol) *system-info-slot* plist))

(define (fundefsym sym)
  (let ((symbol (un-nil-or-t-ify sym)))
    (if (%function-symbol? symbol)
	(error "Internal error: fundefsym was passed function-symbol ~a" symbol)
	(let ((funsym (system-vector-ref (get-symbol-info symbol) *function-symbol-slot*)))
	  (if (not (%function-symbol? funsym))
	      (error "Ack! Fundefsym returns a non function-symbol ~a" funsym)
	      funsym)))))

(define (unfundefsym sym)
  (let ((symbol (un-nil-or-t-ify sym)))
    (if (not (%function-symbol? symbol))
      	(error "Internal error: unfundefsym was passed non-function-symbol ~a" symbol)
      	(nil-or-t-ify (%function-symbol-parent symbol)))))

;;;
;;; This is defined here because it is needed very early -- it used to 
;;; be defined in cl-macros.
;;; Oh, that we could bootstrap this system!
;;;

(define (%put symbol indicator value)
  (if (and (not (symbol? symbol))
	   (not (null? symbol))
	   (not (eq? symbol #t)))
      (error "First argument to %put is not a symbol: ~a" symbol)
      (let ((plist (symbol-plist symbol)))
	(let loop ((l plist))
	  (cond
	   ((null? l) 
	    (%set-symbol-plist! symbol (cons indicator (cons value plist)))
	    value)
	   ((not (pair? (cdr l)))
	    (error "Odd length property list passed to %put"))
	   ((eq? indicator (car l))
	    (set-car! (cdr l) value)
	    value)
	   (else (loop (cddr l))))))))

(define (%system-put symbol indicator value)
  (if (and (not (symbol? symbol))
	   (not (null? symbol))
	   (not (eq? symbol #t)))
      (error "First argument to %system-put is not a symbol: ~a" symbol)
      (let ((plist (symbol-system-plist symbol)))
	(let loop ((l plist))
	  (cond
	   ((null? l) 
	    (%set-symbol-system-plist! symbol (cons indicator (cons value plist)))
	    value)
	   ((not (pair? (cdr l)))
	    (error "Odd length property list passed to %system-put"))
	   ((eq? indicator (car l))
	    (set-car! (cdr l) value)
	    value)
	   (else (loop (cddr l))))))))

;;;
;;; Boot export -- all these symbols will be placed in the exported table
;;;   of the lisp package when packages are initialized (at end of build).
;;;   At that time they will also be removed from scheme's obarray.
;;;

(define *boot-exported-symbols-size* 1024)

(define *boot-exported-symbols* (make-vector *boot-exported-symbols-size*))

;;;
;;; package is always the Lisp package for this boot version of export.
;;;

(define (export symbols #!optional package)
  (mapc export-aux (if (symbol? symbols) (list symbols) symbols))
  #t)

(define (export-aux symbol)
  (let* ((h (hash symbol))
	 (i (modulo h *boot-exported-symbols-size*))
	 (bucket (vector-ref *boot-exported-symbols* i))
	 (entry (memq symbol bucket)))
    (if (null? entry)
	(vector-set! *boot-exported-symbols* i (cons symbol bucket)))))
