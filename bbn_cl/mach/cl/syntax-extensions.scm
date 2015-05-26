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
;;;;;;
;;; Extensions to syntax table for CommonLisp
;;;

;;; The reason these extensions are done here, instead of being embedded in scheme
;;;  is to capture the scheme syntax table after loading scheme extensions to the basic
;;;  syntax table (e.g., macros such as CASE).

(declare (usual-integrations))

(define *scheme-syntax-table* *rep-current-syntax-table*)
(define *commonlisp-syntax-table* (copy-syntax-table *scheme-syntax-table*))

;;;
;;; cl-read-file -- read-file with delay, to force syntax-when-read semantics
;;;

(define cl-read-file 
      (named-lambda (cl-read-file filename)
	(let ((stream (open-input-file filename)))
	  (define (readloop)
	    (let ((object (read stream)))
	      (if (eof-object? object) 
		  (sequence
		    (close-input-port stream)
		    ())
		  (cons object (delay (readloop))))))
	  (readloop))))


;;;
;;; Rel 4.2 Compatibility.
;;;

(define (lookup-syntax name)
  (syntax-table-ref *rep-current-syntax-table* name))

(define (add-syntax! name quantum)
  (syntax-table-define *rep-current-syntax-table* name quantum))

(define (remove-syntax! name)
  (syntax-table-undefine *rep-current-syntax-table* name))

(define (shadow-syntax! name)
  (syntax-table-shadow *rep-current-syntax-table* name))

;;;
;;; Make scheme-macros defined when loaded from fasl file  as well as when syntaxed.
;;;

;;; This is a horrible hack to get the define-syntax evaluated at load-time.

(eval '(using-syntax *commonlisp-syntax-table* 
	 (define-syntax define-macro
	   (macro (pattern . body)
	     (let ((the-macro `(macro ,(cdr pattern) ,@body)))
	       `(sequence
		  (define-syntax ,(car pattern) ,the-macro); Compile-time
		  (add-syntax! ',(car pattern) ,the-macro)
		  ())))))
      ())


(in-package syntaxer-package

;;; Needed keywords
  
(define optimizer-keyword (keyword-intern "OPTIMIZER"))
(define type-keyword (keyword-intern "TYPE"))
(define ignore-keyword (keyword-intern "IGNORE"))
(define unknown-keyword (keyword-intern "UNKNOWN"))
(define declarations-keyword (keyword-intern "DECLARATIONS"))
(define inline-keyword (keyword-intern "INLINE"))
(define special-keyword (keyword-intern "SPECIAL"))
(define shadowed-keyword (keyword-intern "SHADOWED"))
(define structure-operation-keyword (keyword-intern "STRUCTURE-OPERATION"))

;;; Syntax table extensions

(syntax-table-define *commonlisp-syntax-table* 'BLOCK syntax-BLOCK-form)
(syntax-table-define *commonlisp-syntax-table* 'RETURN-FROM syntax-RETURN-FROM-form)
(syntax-table-define *commonlisp-syntax-table* 'TAGBODY syntax-TAGBODY-form)
(syntax-table-define *commonlisp-syntax-table* 'GO syntax-GO-form)
(syntax-table-define *commonlisp-syntax-table* 'EVAL-WHEN syntax-EVAL-WHEN-form)
(syntax-table-define *commonlisp-syntax-table* 'COMPILER-LET syntax-COMPILER-LET-Form)
(syntax-table-define *commonlisp-syntax-table* 'THE syntax-THE-form)
(syntax-table-define *commonlisp-syntax-table* 'SYNTAX-QUOTE syntax-SYNTAX-QUOTE-form)
(syntax-table-define *commonlisp-syntax-table* 'CL-ITERATE syntax-CL-ITERATE-form)
(syntax-table-define *commonlisp-syntax-table* 'CL-COMMENT syntax-CL-COMMENT-form)
(syntax-table-define *commonlisp-syntax-table* 'LR-COMBINATION syntax-LR-COMBINATION-form)

) ; end in-package

(define (%block-aux% receiver)
  (prim-values-list
   (call-with-current-continuation
    (lambda (here)
      (prim-with-values
       (lambda ()
	 (receiver (lambda all
		     (here all))))
       list)))))

(define (%tagbody-aux% receiver)
  (call-with-current-continuation
   (lambda (cont)
     (receiver (lambda (tag)
		 (within-continuation cont tag))))))

;;;
;;; More rel4.2 compatibility
;;;

(syntax-table-define *commonlisp-syntax-table* 'conjunction
		     (lookup-syntax 'and))

(syntax-table-define *commonlisp-syntax-table* 'disjunction
		     (lookup-syntax 'or))

;;;
;;; Alias for Scheme DECLARE special form (which will be rebound soon)
;;;

(syntax-table-define *commonlisp-syntax-table* 'scm-declare
                     (lookup-syntax 'declare))
