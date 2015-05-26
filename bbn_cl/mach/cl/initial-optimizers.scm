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
;;;;;;;; Some early optimizers

;;; Touch expansion

(cl-define (prim-touch x)
  (touch x))

(generate-type-optimizer
 (touch a)
 (#f)
 ((((* a)) * (non-future :required-type) (let ((temp a))
					   (if (non-touching-primitive-type? 46 temp)
					       (prim-touch temp)
					       temp)) #t)))

(generate-type-optimizer
 (prim-touch a)
 (#f)
 ((((* a)) * (non-future :required-type) ((:primitive touch) a) #f)))

;;; Member and assoc

(generate-general-optimizer
 (member a b keyword test)
 (#f #f #f #f)
 (lambda (required-type operator args touched-args)
   (let ((a (first args))
	 (b (second args))
	 (keyword (third args))
	 (test (fourth args)))
     (if (and (variable? keyword)
	      (variable-components keyword (lambda l (eq? (car l) :test)))
	      (variable? test)
	      (variable-components test (lambda l (eq? (car l) (fundefsym 'eq)))))
	 (prim-values required-type memq `((syntax-quote ,a) (syntax-quote ,b)) #t)
	 (prim-values required-type operator args #f)))))

(generate-general-optimizer
 (assoc a b keyword test)
 (#f #f #f #f)
 (lambda (required-type operator args touched-args)
   (let ((a (first args))
	 (b (second args))
	 (keyword (third args))
	 (test (fourth args)))
     (if (and (variable? keyword)
	      (variable-components keyword (lambda l (eq? (car l) :test)))
	      (variable? test)
	      (variable-components test (lambda l (eq? (car l) (fundefsym 'eq)))))
	 (prim-values required-type assq `((syntax-quote ,a) (syntax-quote ,b)) #t)
	 (prim-values required-type operator args #f)))))

'$split-file

;;; Character optimizers

(generate-type-optimizer
 (char= a b)
 (#t #t)
 ((((string-char a) (string-char b)) * (member t nil) (eq? a b) #t)))

(generate-type-optimizer
 (char/= a b)
 (#t #t)
 ((((string-char a) (string-char b)) * 
  (member t nil) (not (the (member t nil) (eq? a b))) #t)))
  
;; char< and friends are not optimized into fixnum ops because
;; the interpreted fixnum ops check the types of their arguments.

'$split-file
;;; The touching primitives

(define-macro (make-touch-setter-optimizer3 x)
  `(generate-type-optimizer
    (,x object index value)
    (#t #t #f)
    ((((* object) (* index) (* value)) * :required-type
				       ((:primitive ,x) object index value) #f))))

(define-macro (make-touch-setter-optimizer2 x)
  `(generate-type-optimizer
    (,x object value)
    (#t #f)
    ((((* object) (* value)) * :required-type 
			     ((:primitive ,x) object value) #f))))

(make-touch-setter-optimizer3 system-vector-set!)
(make-touch-setter-optimizer3 vector-set!)

(define cl-system-vector-set! (make-primitive-procedure 'cl-system-vector-set!))
(define cl-vector-set! (make-primitive-procedure 'cl-vector-set!))

(cl-define cl-system-vector-set! cl-system-vector-set!)
(cl-define cl-vector-set! cl-vector-set!)

(make-touch-setter-optimizer3 cl-system-vector-set!)
(make-touch-setter-optimizer3 cl-vector-set!)

(make-touch-setter-optimizer2 system-hunk3-set-cxr2!)
(make-touch-setter-optimizer2 system-hunk3-set-cxr1!)
(make-touch-setter-optimizer2 system-hunk3-set-cxr0!)
(make-touch-setter-optimizer2 system-pair-set-car!)
(make-touch-setter-optimizer2 system-pair-set-cdr!)
(make-touch-setter-optimizer2 set-car!)
(make-touch-setter-optimizer2 set-cdr!)
(make-touch-setter-optimizer2 set-cell-contents!)

'$split-file

(define-macro (make-touch-optimizer2 x type)
  `(generate-type-optimizer
    (,x object index)
    (#t #t)
    ((((* object) (* index)) * ,type
		  ((:primitive ,x) object index) #f))))

(define-macro (make-touch-optimizer1 x type)
  `(generate-type-optimizer
    (,x object)
    (#t)
    ((((* object)) * ,type
		   ((:primitive ,x) object) #f))))

(make-touch-optimizer2 system-vector-ref :required-type)
(make-touch-optimizer2 vector-ref :required-type)
(make-touch-optimizer1 system-hunk3-cxr2 :required-type)
(make-touch-optimizer1 system-hunk3-cxr1 :required-type)
(make-touch-optimizer1 system-hunk3-cxr0 :required-type)
(make-touch-optimizer1 system-pair-car   :required-type)
(make-touch-optimizer1 system-pair-cdr   :required-type)
(make-touch-optimizer1 cell-contents     :required-type)
(make-touch-optimizer1 bit-string-length fixnum)
(make-touch-optimizer1 string-length     fixnum)
(make-touch-optimizer1 system-vector-size fixnum)
(make-touch-optimizer1 vector-length      fixnum)
(make-touch-optimizer2 eq?                (member t nil))
(make-touch-optimizer2 primitive-type?    (member t nil))
(make-touch-optimizer1 bit-string?        (member t nil))
(make-touch-optimizer1 string?            (member t nil))
(make-touch-optimizer1 null?              (member t nil))
(make-touch-optimizer1 pair?              (member t nil))

;; Note that these are not touch-optimizer1 because we are
;; optimizing the compound procedure, not the primitive

(generate-type-optimizer
 (car object)
 (#t)
 ((((* object)) * :required-type (car object) #f)))

(generate-type-optimizer
 (cdr object)
 (#t)
 ((((* object)) * :required-type (cdr object) #f)))

'$split-file

;; Car and cdr are handled by compiler and np-inline

;;;; Two levels

(generate-type-optimizer
 (cddr a)
 #f
 ((((* a))       *       :required-type (cdr (cdr a))                  #t)))

(generate-type-optimizer
 (cdar a)
 #f
 ((((* a))       *       :required-type  (cdr (car a))                 #t)))

(generate-type-optimizer
 (cadr a)
 #f
 ((((* a))       *       :required-type  (car (cdr a))                 #t)))

(generate-type-optimizer
 (caar a)
 #f
 ((((* a))       *       :required-type  (car (car a))                 #t)))

'$split-file

;;;; Three levels

(generate-type-optimizer
 (cdddr a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (cdr a)))           #t)))

(generate-type-optimizer
 (cddar a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (car a)))           #t)))

(generate-type-optimizer
 (cdadr a)
 #f
 ((((* a))       *       :required-type  (cdr (car (cdr a)))           #t)))

(generate-type-optimizer
 (cdaar a)
 #f
 ((((* a))       *       :required-type  (cdr (car (car a)))           #t)))

(generate-type-optimizer
 (caddr a)
 #f
 ((((* a))       *       :required-type  (car (cdr (cdr a)))           #t)))

(generate-type-optimizer
 (cadar a)
 #f
 ((((* a))       *       :required-type  (car (cdr (car a)))           #t)))

(generate-type-optimizer
 (caadr a)
 #f
 ((((* a))       *       :required-type  (car (car (cdr a)))           #t)))

(generate-type-optimizer
 (caaar a)
 #f
 ((((* a))       *       :required-type  (car (car (car a)))           #t)))

'$split-file
;;;; Four levels

(generate-type-optimizer
 (cddddr a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (cdr (cdr a))))     #t)))

(generate-type-optimizer
 (cdddar a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (cdr (car a))))     #t)))

(generate-type-optimizer
 (cddadr a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (car (cdr a))))     #t)))

(generate-type-optimizer
 (cddaar a)
 #f
 ((((* a))       *       :required-type  (cdr (cdr (car (car a))))     #t)))

(generate-type-optimizer
 (cdaddr a)
 #f
 ((((* a))       *       :required-type  (cdr (car (cdr (cdr a))))     #t)))

(generate-type-optimizer
 (cdadar a)
 #f
 ((((* a))       *       :required-type  (cdr (car (cdr (car a))))     #t)))

(generate-type-optimizer
 (cdaadr a)
 #f
 ((((* a))       *       :required-type  (cdr (car (car (cdr a))))     #t)))

(generate-type-optimizer
 (cdaaar a)
 #f
 ((((* a))       *       :required-type  (cdr (car (car (car a))))     #t)))

'$split-file

(generate-type-optimizer
 (cadddr a)
 #f
 ((((* a))       *       :required-type  (car (cdr (cdr (cdr a))))     #t)))

(generate-type-optimizer
 (caddar a)
 #f
 ((((* a))       *       :required-type  (car (cdr (cdr (car a))))     #t)))

(generate-type-optimizer
 (cadadr a)
 #f
 ((((* a))       *       :required-type  (car (cdr (car (cdr a))))     #t)))

(generate-type-optimizer
 (cadaar a)
 #f
 ((((* a))       *       :required-type  (car (cdr (car (car a))))     #t)))

(generate-type-optimizer
 (caaddr a)
 #f
 ((((* a))       *       :required-type  (car (car (cdr (cdr a))))     #t)))

(generate-type-optimizer
 (caadar a)
 #f
 ((((* a))       *       :required-type  (car (car (cdr (car a))))     #t)))

(generate-type-optimizer
 (caaadr a)
 #f
 ((((* a))       *       :required-type  (car (car (car (cdr a))))     #t)))

(generate-type-optimizer
 (caaaar a)
 #f
 ((((* a))       *       :required-type  (car (car (car (car a))))     #t)))

'$split-file
;;;; first through tenth

(generate-type-optimizer
 (first a)
 #f
 ((((* a))       *       :required-type    (car a)               #t)))

(generate-type-optimizer
 (second a)
 #f
 ((((* a))       *       :required-type    (car (cdr a))               #t)))

(generate-type-optimizer
 (third a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr a)))         #t)))

(generate-type-optimizer
 (fourth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr a))))   #t)))

(generate-type-optimizer
 (fifth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr a)))))                      #t)))

(generate-type-optimizer
 (sixth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr (cdr a))))))                #t)))

'$split-file

(generate-type-optimizer
 (seventh a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr (cdr (cdr a)))))))          #t)))

(generate-type-optimizer
 (eighth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr a))))))))    #t)))

(generate-type-optimizer
 (ninth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr a)))))))))    #t)))

(generate-type-optimizer
 (tenth a)
 #f
 ((((* a))       *       :required-type    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr a)))))))))) #t)))

