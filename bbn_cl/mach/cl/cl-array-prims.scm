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
(export '(arrayp adjustable-array-p aref array-dimension array-dimensions
	  array-element-type array-rank array-in-bounds-p array-row-major-index
	  array-total-size svref bit sbit array-has-fill-pointer-p fill-pointer
	  vector-push vector-push-extend vector-pop))

;;; Theses primitives are defined early so that the commonlisp build
;;; can integrate these primitives early.

;;; cl-vector-set! and cl-system-vector-set! are in
;;; initial-optimizers.scm

(cl-define %array-is-displaced-p (make-primitive-procedure 'cl-array-is-displaced-p))
(cl-define %aset (make-primitive-procedure 'cl-aset))
(cl-define %bitset (make-primitive-procedure 'cl-bitset))
(cl-define %elt-set (make-primitive-procedure 'cl-elt-set))
(cl-define %make-array (make-primitive-procedure 'cl-make-array))
(cl-define %sbitset (make-primitive-procedure 'cl-sbitset))
(cl-define %set-fill-pointer (make-primitive-procedure 'cl-set-fill-pointer))
(cl-define %adjust-array (make-primitive-procedure 'cl-adjust-array))
(cl-define adjustable-array-p (make-primitive-procedure 'cl-adjustable-array-p))
(cl-define aref (make-primitive-procedure 'cl-aref))
(cl-define array-dimension (make-primitive-procedure 'cl-array-dimension))
(cl-define array-dimensions (make-primitive-procedure 'cl-array-dimensions))
(cl-define array-element-type (make-primitive-procedure 'cl-array-element-type))
(cl-define array-has-fill-pointer-p (make-primitive-procedure 'cl-array-has-fill-pointer-p))
(cl-define array-in-bounds-p (make-primitive-procedure 'cl-array-in-bounds-p))
(cl-define array-rank (make-primitive-procedure 'cl-array-rank))
(cl-define array-row-major-index (make-primitive-procedure 'cl-array-row-major-index))
(cl-define array-total-size (make-primitive-procedure 'cl-array-total-size))
(cl-define arrayp (make-primitive-procedure 'cl-arrayp))
(cl-define bit (make-primitive-procedure 'cl-bit))
(cl-define elt (make-primitive-procedure 'cl-elt))
(cl-define fill-pointer (make-primitive-procedure 'cl-fill-pointer))
(cl-define find-data-vector (make-primitive-procedure 'cl-find-data-vector))
(cl-define sbit (make-primitive-procedure 'cl-sbit))
(cl-define simple-array-p (make-primitive-procedure 'cl-simple-array-p))
(cl-define svref vector-ref)
(cl-define vector-pop (make-primitive-procedure 'cl-vector-pop))
(cl-define vector-push (make-primitive-procedure 'cl-vector-push))
(cl-define vector-push-extend (make-primitive-procedure 'cl-vector-push-extend))
(cl-define %cl-vector-length (make-primitive-procedure 'cl-vector-length))

(generate-type-optimizer
 (%aset a b c)
 (#t #t #f)
 ((((simple-vector a) (* b) (* c))  * :required-type (cl-vector-set! a b c) #t))
 (%aset array i1 i2 value)
 (#t #t #t)
 (((((:satisfies specified-2d-simple-array-p) array) (* i1) (* i2) (* value)) 
   * :required-type (optimize-2d-array-%aset array i1 i2 value (:arg-type array)) #t)))

(generate-type-optimizer
 (aref a b)
 (#t #t)
 ((((simple-vector a) (* b))     * :required-type (system-vector-ref a b)     #t)
  (((simple-bit-vector a) (* b)) * bit            (bit-string-ref a b)        #t)
  (((simple-string a) (* b))     * string-char    (string-ref a b)            #t))
 (aref array i1 i2)
 (#t #t #t)
 (((((:satisfies specified-2d-simple-array-p) array) (* i1) (* i2)) 
   * :required-type (optimize-2d-array-aref array i1 i2 (:arg-type array)) #t)))

(cl-define (specified-2d-simple-array-p type)
  (cond
   ((eq type :unknown) #f)
   ((and (pair? type)
	 (eq? (car type) :constant))
    (typep (cadr type) '(simple-array * (* *))))
   (t
    (and (subtypep type '(simple-array * (* *)))
	 (let ((type (type-expand type)))
	   (integer? (car (caddr type)))
	   (integer? (cadr (caddr type))))))))

(define-macro (optimize-2d-array-aref array i1 i2 array-type)
  (let ((d2 (if (and (pair? array-type)
		     (eq? (car array-type) :constant))
		(cadr (array-dimensions (cadr array-type)))
		(cadr (caddr (type-expand array-type))))))
    `(system-vector-ref 
      (system-vector-ref ,array 5)
      (the fixnum (+ (the fixnum (* (the fixnum ,d2) (the fixnum ,i1)))
		     (the fixnum ,i2))))))

(define-macro (optimize-2d-array-%aset array i1 i2 value array-type)
  (let ((d2 (if (and (pair? array-type)
		     (eq? (car array-type) :constant))
		(cadr (array-dimensions (cadr array-type)))
		(cadr (caddr (type-expand array-type))))))
    `(cl-system-vector-set!
      (system-vector-ref ,array 5)
      (the fixnum (+ (the fixnum (* (the fixnum ,d2) (the fixnum ,i1)))
		     (the fixnum ,i2)))
      ,value)))

(generate-type-optimizer
 (svref a b)
 (#t #t)
 ((((* a) (* b))     * :required-type (system-vector-ref a b)     #t)))

(generate-type-optimizer
 (length a)
 (#t)
 ((((list a))              * fixnum        (simple-list-length a)      #t)
  (((simple-vector a))     * fixnum        (vector-length a)           #t)
  (((simple-string a))     * fixnum        (string-length a)           #t)))


