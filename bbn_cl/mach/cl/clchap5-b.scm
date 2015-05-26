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
(proclaim '(insert-touches nil))

;;;; Constructor

(cl-define (process-defaulted-parameters opts body)
  (cl-define (loop left direct code)
    (if (null? left)
	(prim-values direct code) 
	(let* ((param (car left))
	       (bogus (generate-uninterned-symbol (car param))))
	  (loop (cdr left)
		(cons bogus direct)
		`(let ((,(car param)
			(if (unassigned? ,bogus)
			    ,(if (not (eq? (cadr param) no-default-expression))
				 (cadr param)
				 ''())
			    ,bogus))
		       ,@(if (caddr param)
			     `((,(caddr param) (not (unassigned? ,bogus))))))
		   ,code)))))
  (loop opts '() body))

(cl-define (process-auxes auxes body)
  (if (null? auxes)
      body
      (process-auxes
       (cdr auxes)
       (let ((param (car auxes)))
	 `(let ((,(car param)
		 ,(if (eq? (cadr param) no-default-expression)
		      ''()
		      (cadr param))))
	    ,body)))))



(cl-define &make-object (make-primitive-procedure '&make-object))

(cl-define make-unassigned-object
  (let ((type (microcode-type 'UNASSIGNED)))
    (lambda ()
      (&make-object type 0))))

;;; Although futures are off, no primitives occur in copy-list
;;;  so all should be well.

(cl-define (copy-list l) (apply list l))			; Could be faster.

;; Note: Important: In the following KEYS is reversed with respect to the
;; parameters to handler.  This avoids invoking reverse at the end of the
;; whole process.

;; The hokiness below making lists of unassigned objects is to avoid returning
;; an unassigned var, which results in a error when wrapping with perf code -- i.e., one can't return a 
;; var bound to the unassigned object, but one can return a var bound to a list 
;; containing the unassigned object. This is probably wrong w.r.t. theory, but until we have some
;; better behaving "unassigned objects", this will have to do. [las] 
;;
;; Note that perf code is now obsolete, so this can be reverted to its former
;; state if desired.

(cl-define (parse-keyword-arguments allow-others? keys args handler)
  (if (not (even? (length args)))
      (error "Apply: Keyword list ~A does not have even length.~%Acceptable keys are ~A." args keys))
  (let ((the-list (cons 'dummy (copy-list args))))	; Original not clobbered.
    (cl-define (do-next key #!optional default)
      (cl-define (find-loop args prev)
	(cond ((not (null? args))
	       (let* ((val-loc (cdr args))
		      (next (cdr val-loc)))
		 (if (eq? key (car args))
		     (sequence
		      (set-cdr! prev next)
		      (list (car val-loc)))
		     (find-loop next val-loc))))
	      ((unassigned? default)
	       (list (make-unassigned-object)))
	      (else default)))
      (find-loop (cdr the-list) the-list))

    (cl-define (loop ks accum)
      (cond ((not (null? ks))
	     (loop (cdr ks)
		   (cons (car (do-next (car ks)))
			 accum)))
	    ((or (null? (cdr the-list))
		 allow-others?
		 (do-next :allow-other-keys #!FALSE))
	     (apply handler accum))
	    (else (error "Apply: Improper keyword arguments: ~A~%Acceptable keys are ~A" args keys))))
    (loop keys '())))

(define-macro (cl-lambda param-list . body)
  (prim-with-values
   (lambda () (make-cl-lambda #f () param-list body #f #t #f))
   (lambda (lambda-expr doc fun-name)
     lambda-expr)))

(define-macro (cl-named-lambda param-list . body)
  (prim-with-values
   (lambda () (make-cl-lambda #t (car param-list) (cdr param-list) body #f #t #f))
   (lambda (lambda-expr doc fun-name)
     lambda-expr)))

;; For the syntaxer

(define (proc-cl-named-lambda name param-list body)
  (prim-with-values
   (lambda () (make-cl-lambda #t name param-list body #f #t #f))
   (lambda (lambda-expr doc fun-name)
     lambda-expr)))

(define-macro (cl-let-lambda param-list . body)
  (prim-with-values
   (lambda () (make-cl-lambda #f () param-list body #t #f #f))
   (lambda (lambda-expr doc fun-name)
     lambda-expr)))

;;;
;;; This is boot version:
;;;     - no macroexpand of decls.
;;;     - only one doc string allowed, and must precede decl (if there is a decl).
;;;     - only one decl allowed.
;;;
;;;  Full version is in clchap8.
;;;

(cl-define (parse-body-internal body env doc-string-allowed)
  (let ((doc nil)
	(decls nil))
    (if (string? (car body))
	(begin
	  (set! doc (car body))
	  (set! body (cdr body))))
    (if (and (pair? (car body))
	     (eq? (caar body) 'declare))
	(begin
	  (set! decls (list (car body)))
	  (set! body (cdr body))))
    (if (null? body)
	(set! body '(())))
    (prim-values body decls doc)))


;;;
;;; Constrained by boot parse-body-internal, below --
;;;  full doc, decls when parse-body-internal redefined in clchap8.scm.
;;;
;;; let? -- true if lambda for let.
;;; doc? -- true if doc string allowed.
;;; defun? -- true if for defun; wraps with BLOCK in that case.
;;;
;;; If there are decls, places them as quoted first form of lambda-body, in the form:
;;;
;;;      (:declarations <bvl> <declare> ... <declare>)
;;;
;;;  where the bvl is that of the lambda-list
;;;    (this is done to simplify things later, and to provide a scope check on the declarations).
;;;

;; Simplified paramater parser (I'd use stuff from parse-param-list,
;; but it is too hairy to even begin comprehending).

(cl-define (bound-parameters parameter-list)
  (define lambda-list-keywords '(&optional &rest &key &allow-other-keys &aux))
  (cl-define (parse-required l)
    (cond ((null? l)
	   '())
	  ((memq (car l) lambda-list-keywords)
	   (dispatch (car l) (cdr l)))
	  (else (cons (car l) (parse-required (cdr l))))))
  (cl-define (parse-optional l)
    (cond ((null? l)
	   '())
	  ((memq (car l) lambda-list-keywords)
	   (dispatch (car l) (cdr l)))
	  ((pair? (car l))
	   (if (= (length (car l)) 3)
	       (cons (first (car l)) 
		     (cons (third (car l)) 
			   (parse-optional (cdr l))))
	       (cons (caar l)
		     (parse-optional (cdr l)))))
	  (else (cons (car l) (parse-optional (cdr l))))))
  (cl-define (parse-rest l)
    (if (null? (cdr l))
	(list (car l))
	(cons (car l) (dispatch (cadr l) (cddr l)))))
  (cl-define (parse-key l)
    (cond ((null? l)
	   '())
	  ((memq (car l) lambda-list-keywords)
	   (dispatch (car l) (cdr l)))
	  ((pair? (car l))
	   (let ((ll (car l)))
	     (if (= (length ll) 3)
		 (cons (if (pair? (car ll)) (second (car ll)) (car ll))
		       (cons (third ll) (parse-key (cdr l))))
		 (cons (if (pair? (car ll)) (second (car ll)) (car ll))
		       (parse-key (cdr l))))))
	  (else (cons (car l) (parse-key (cdr l))))))
  (cl-define (parse-aux l)
    (cond ((null? l)
	   '())
	  ((pair? (car l))
	   (cons (caar l) (parse-aux (cdr l))))
	  (else (cons (car l) (parse-aux (cdr l))))))
  (cl-define (dispatch key l)
    (cond ((eq? key '&optional)
	   (parse-optional l))
	  ((eq? key '&rest)
	   (parse-rest l))
	  ((eq? key '&key)
	   (parse-key l))
	  ((eq? key '&aux)
	   (parse-aux l))
	  (else '(dispatch-error-in-bound-paramters?))))
  (parse-required parameter-list))
	       
'$split-file

(cl-define (make-cl-lambda named? name param-list body let? doc? defun?)
  (prim-with-values
   (lambda () (parse-body-internal body *syntax-time-env* doc?))
   (lambda (body decls doc)
     (let ((fun-name (fundefsym name))
	   (body-decls (if decls 
			   `('(:declarations ,(bound-parameters param-list) ,@decls))
			   '())))
       (set! body
	     `(begin
		,@body-decls
		,(if defun? 
		     `(block ,name ,@body)
		     `(begin ,@body))))
       (prim-with-values
	(lambda () (parse-param-list named? 
			  (if named? 
			      (cons fun-name param-list)
			      param-list)
			  body
			  decls
			  let?))
	(lambda (lambda-expr ignore)
	  (prim-values lambda-expr doc fun-name)))))))

'$split-file

;;; This var is used to get syntax-time eval of defmacros, defvars, etc.,
;;; during the build. It is turned off at the end of the build, and cl-compile-file
;;; handles the evaluation as the user desires.

(define *build-time-eval* #t)

;;;
;;; We use this for all early macros that must expand as commonlisp
;;;  macros, but before we have the machinery to define defmacro
;;;  as in clchap8-c.
;;;

(define-macro (boot-defmacro name pattern . body)
  (newline)
  (princ (string-append ";;; Expanding (boot-defmacro "
			(symbol->string name)
			")"))
  (warn-if-macro-redefined name *syntax-time-global-env*)
  (prim-with-values
   (lambda () (make-cl-lambda #f () pattern body #f #t #t))
   (lambda (lambda-expr doc fun-name)
     (let ((fcn-sym (fundefsym name)))
       (if *build-time-eval*
	   (let ((expander ((access eval system-global-environment)
			    `(lambda (whole env) (apply ,lambda-expr (cdr whole)))
			    *commonlisp-user-environment*)))
	     (local-assignment *syntax-time-global-env* fcn-sym 
			       (cons 'commonlisp-macro expander))))
       ;; The emitted expander is closed in the wrong env
       ;;  (but the one above is correct) -- need to be able 
       ;;  to compile (scheme) in-package to correct this problem
       `(defmacro-internal
	  ',name
	  ',doc
	  (lambda (whole env) (apply ,lambda-expr (cdr whole))))))))

;;; *defmacro-internal-env* is normally bound to system-global-environment.
;;;  It may be rebound (usually to *syntax-time-global-env*) for compile-time
;;;  definitions (e.g., in cl-compile-file.scm)

(cl-define (defmacro-internal name doc expander)
  (let ((env *defmacro-internal-env*))
    (warn-if-macro-redefined name env)
    (if doc (%system-put name '%fun-documentation doc))
    (local-assignment env
		      (fundefsym name)
		      (cons 'commonlisp-macro expander))
    name))

(cl-define (warn-if-macro-redefined name env)
  (let ((funcellsym (fundefsym name)))
    (if (not (lexical-unreferenceable? env
				       funcellsym))
	(cond ((commonlisp-macro? funcellsym env)	;already a macro?
	       (newline)
	       (princ (string-append
		       ";;; Warning: redefining macro "
		       (symbol-name name))))
	      ((procedure?		;a function?
		(lexical-reference
		 env
		 funcellsym))
	       (newline)
	       (princ (string-append
		       ";;; Warning: redefining function "
		       (symbol-name name)
		       " as a macro")))))))


;;;
;;; Boot def. Redefined in clchap8-c. No heavy error checking
;;;  here as users will never see it.
;;;

;;; Don't need touches because is redefined later.

(cl-define (macroexpand form #!optional env)
  (if (unassigned? env) (set! env *syntax-time-global-env*))
  ((lookup-macro-definition (car form) env) form env))

