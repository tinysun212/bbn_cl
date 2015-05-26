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
;;;
;;; Mutated-name define
;;;

(define-macro (cl-define name-or-proc-spec . def)
  (if (pair? name-or-proc-spec)
      (if (pair? (car name-or-proc-spec))
	  `(define ((,(fundefsym (caar name-or-proc-spec))
		     ,@(cdar name-or-proc-spec))
		    ,@(cdr name-or-proc-spec))
	     ,@def)
	  `(define (,(fundefsym (car name-or-proc-spec)) ,@(cdr name-or-proc-spec))
	     ,@def))
      `(define ,(fundefsym name-or-proc-spec) ,@def)))

;;macro-expander function for debugging
(cl-define (me x)
  (fluid-let ((*use-commonlisp-source* nil)) ; Show the expansion, not the source
    (pp (syntax x *rep-current-syntax-table*))))

;;for use where setq is what you want, but you don't care about it's value
;;will return whatever set! returns
(define-macro (setq-set! . args)
  (if (null args)
      '()
  `(sequence ,@(make-multiple-setqs-set! '(set!) args))))

;;support function for setq-set!  Operator should be a list to which the
;;operands get appended.  This makes it possible to call this with
;;(local-assignment) as the set function.
;;

(cl-define
 (make-multiple-setqs-set! operator list-of-pairs)
  (if (null? (cddr list-of-pairs))
      (cons `(,@operator ,(first list-of-pairs)
			 ,(second list-of-pairs))
	    '())
      (cons `(,@operator ,(first list-of-pairs)
			 ,(second list-of-pairs))
	    (make-multiple-setqs-set! operator (cddr list-of-pairs)))))

(define-macro (def-special-form template expansion)
  `(define-macro ,template ,expansion))
