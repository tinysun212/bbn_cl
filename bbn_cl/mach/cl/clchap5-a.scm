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

(export '(copy-list defun defmacro
	  &optional &rest &key &allow-other-keys &aux &body &whole &environment))

;runtime

(define SEQUENCE/PROGN
  ;; PROGN for a full Common Lisp syntaxer
  'PROGN)

(define PROCEDURE-TOKEN 'LAMBDA)

(define NAMED-PROCEDURE-TOKEN
  ;; Does not exist in Common Lisp, unassign it.
  'NAMED-LAMBDA)

;;;; Utilities

(cl-define (symbol->keyword symbol)
  (keyword-intern (symbol->string symbol)))

(cl-define (valid-symbol? x)
  (and (symbol? x) (not (keyword? x))))

(define lambda-tag:optional-tag '&optional)
(define lambda-tag:scheme-optional-tag
  (access lambda-optional-tag lambda-package))
(define lambda-tag:rest-tag '&rest)
(define lambda-tag:key-tag '&key)
(define lambda-tag:allow-others-tag '&allow-other-keys)
(define lambda-tag:aux-tag '&aux)

(cl-define lambda-list-tag?
  (let ((lambda-list-tags
	 (list lambda-tag:optional-tag lambda-tag:rest-tag
	       lambda-tag:key-tag lambda-tag:allow-others-tag
	       lambda-tag:aux-tag)))
    (lambda (x)
      (memq x lambda-list-tags))))

;; Returns them reversed!!

(cl-define (get-until-tag the-list let?)
  (cl-define (loop left accum)
    (if (or (null? left)
	    (and (not let?)
		 (lambda-list-tag? (car left))))
	(prim-values accum left)
	(loop (cdr left) (cons (car left) accum))))
  (loop the-list '()))

(cl-define (check-param par key? max-len)
  (or (valid-symbol? par)				; Name
      (and (list? par)
	   (not (null? par))
	   (let ((len (length par))
		 (fi (car par)))
	     (and (or (valid-symbol? fi)		; Name
		      (and key?
			   (list? fi)
			   (= (length fi) 2)
			   (valid-symbol? (cadr fi))	; Name
			   (keyword? (car fi))))	; Keyword
		  (<= len max-len)
		  (or (not (= len 3))
		      (valid-symbol? (caddr par)))	; Svar
		  )))))

(cl-define (parameter-name param)
  (cond ((symbol? param) param)
	((pair? (car param))				; Keywords
	 (cadar param))
	(else (car param)))) 

(cl-define (parameter-keyword param)
  (cond ((symbol? param) (symbol->keyword param))
	((pair? (car param))
	 (caar param))
	(else (symbol->keyword (car param)))))

(define no-default-expression
  (string->symbol "#[NO DEFAULT EXPRESSION]"))

(cl-define (parameter-default-exp param)
  (if (or (symbol? param) (null? (cdr param)))
      no-default-expression
      (cadr param)))

(cl-define (parameter-svar param)
  (if (or (symbol? param) (< (length param) 3))
      #!FALSE
      (caddr param)))

;;;; Parser

(define specials '())  ; Global list of specials defined with defvar or proclaim

;;;
;;; let?, if true, means we are parsing for a cl let.
;;;

(cl-define (parse-param-list named? param-list the-body decls let?) 

  (define declared-specials)

  (define specials-alist '())

  (cl-define (special? param)
	     (disjunction (memq param specials)
			  (memq param declared-specials)))

  (cl-define (process-special param)
    (if (system-get param '%constant)
	(error "Illegal binding of a constant: ~A" param)
	(let ((newparam (generate-uninterned-symbol param)))
	  (set! specials-alist (cons (list `(access ,param *commonlisp-user-environment*) newparam)
				     specials-alist))
	  newparam)))

  ;; ->lambda assumes that all parameters have been syntax checked,
  ;; so things are always in their right place.  Reqs is in the right order,
  ;; but the other lists are reversed!!  This is cheaper since 
  ;; process-default-parameters wants to deal with them backwards and will reverse
  ;; them again.

  (cl-define (->lambda named? reqs opts rest keys allow-others? auxes the-body)
    (let ((body (if specials-alist
		    `(values-list
		      (fluid-let ,specials-alist
			(multiple-value-list
			 ,the-body)))
		    the-body))
	  (tail (or rest (and (not (null? keys)) (generate-uninterned-symbol
						  'for-keys)))))

      (cl-define (do-keys body)
	(prim-with-values
	 (lambda () (process-defaulted-parameters (mapcar cdr keys) body))
	 (lambda (scheme-keys inner-body)
	   `(PARSE-KEYWORD-ARGUMENTS
	     ',allow-others?
	     ',(mapcar car keys); Reversed, as needed
	     ,tail
	     ,(->scheme-lambda #!FALSE scheme-keys '() #!FALSE '() inner-body)))))

      (prim-with-values
       (lambda ()
	 (process-defaulted-parameters opts
				       (let ((inner
					      (if (null? auxes)
						  body
						  (process-auxes auxes body))))
					 (if (not (null? keys))
					     (do-keys inner)
					     inner))))
       (lambda (scheme-opts top-level-body)
	 (prim-values (->scheme-lambda (if named? (car reqs) #!FALSE) ; Name
				       (if named? (cdr reqs) reqs)
				       scheme-opts
				       tail
				       '() ; No scheme aux
				       top-level-body)
		      (if named? (car reqs)))))))

  (cl-define (->scheme-lambda name req opt rest aux body)
    (let* ((tail (if rest rest '()))	; '() may not be #!FALSE
	   (param-list
	    (append req
		    (if (null? opt)
			tail
			(cons lambda-tag:scheme-optional-tag
			      (append opt tail))))))
	  (list (if name NAMED-PROCEDURE-TOKEN PROCEDURE-TOKEN)
		(if name (cons name param-list) param-list)
		body)))

  (cl-define (do-one named? par-list body)

    (cl-define (get-reqs plist)
      (prim-with-values
       (lambda () (get-until-tag plist let?))
       (lambda (reqs left)
	 (let ((newreqs
		(mapcar
		 (lambda (req)
		   (if (not (valid-symbol? req))
		       (error "Syntax-parameters: Bad required parameter ~A ~A ~A"
			      req par-list param-list)
		       (if (special? req)
			   (process-special req)
			   req)))
		 reqs)))
		 
	   (if (and named? (null? reqs))
	       (error "Syntax-parameters: Bad named LAM parameter list ~A ~A"
		      par-list param-list)
	       (prim-values newreqs left))))))

    (cl-define (get-opts plist)
      (if (or (null? plist)
	      (not (eq? (car plist) lambda-tag:optional-tag)))
	  (prim-values '() plist)
	  (prim-with-values
	   (lambda () (get-until-tag (cdr plist) let?))
	   (lambda (opts after-opts)
	     (if (null? opts)
		 (error "Syntax-parameters: No optional parameters specified ~A ~A"
			par-list param-list)
		 (prim-values
		  (mapcar
		   (lambda (opt)
		     (if (not (check-param opt #!FALSE 3))
			 (error "Syntax-parameters: Bad optional parameter ~A ~A ~A"
				opt par-list param-list))
		     (let ((optname (parameter-name opt))
			   (svarname (parameter-svar opt)))
		       (list (if (special? optname)
				 (process-special optname)
				 optname)
			     (parameter-default-exp opt)
			     (if (special? svarname)
				 (process-special svarname)
				 svarname))))
		   opts)
		  after-opts))))))

    (cl-define (get-rest plist)
      (if (or (null? plist)
	      (not (eq? (car plist) lambda-tag:rest-tag)))
	  (prim-values #!FALSE plist)
	  (prim-with-values
	   (lambda () (get-until-tag (cdr plist) let?))
	   (lambda (rests left)
	     (if (or (null? rests)
		     (not (null? (cdr rests)))
		     (not (valid-symbol? (car rests))))
		 (error "Syntax-parameters: Bad rest parameter ~A ~A"
			par-list param-list)
		 (prim-values (if (special? (car rests))
				  (process-special (car rests))
				  (car rests))
			      left))))))

    (cl-define (get-keys plist)
      (if (or (null? plist)
	      (not (eq? (car plist) lambda-tag:key-tag)))
	  (prim-values '() #!FALSE plist)
	  (prim-with-values
	   (lambda () (get-until-tag (cdr plist) let?))
	   (lambda (keys left)
	     (let ((nkeys
		    (mapcar (lambda (key)
			      (if (not (check-param key #!TRUE 3))
				  (error "Syntax-parameters: Bad keyword parameter ~A ~A ~A"
					 key par-list param-list))
			      (let ((keyname (parameter-name key))
				    (svarname (parameter-svar key)))
				(list (parameter-keyword key)
				      (if (special? keyname)
					  (process-special keyname)
					  keyname)
				      (parameter-default-exp key)
				      (if (special? svarname)
					  (process-special svarname)
					  svarname))))
			    keys)))
	       (if (and (not (null? left))
			(eq? (car left) lambda-tag:allow-others-tag))
		   (prim-values nkeys #!TRUE (cdr left))
		   (prim-values nkeys #!FALSE left)))))))

    (cl-define (get-auxes plist)
      (cond ((null? plist) '())
	    ((not (eq? (car plist) lambda-tag:aux-tag))
	     (error "Syntax-parameters: Malformed parameter list ~A ~A" par-list param-list))
	    (else
	     (prim-with-values
	      (lambda () (get-until-tag (cdr plist) let?))
	      (lambda (auxes rem)
		(if (not (null? rem))
		    (error "Syntax-parameters: Malformed parameter list ~A ~A" par-list param-list))
		(mapcar (lambda (aux)
			  (if (not (check-param aux #!FALSE 2))
			      (error "Syntax-parameters: Bad auxiliary parameter ~A ~A ~A"
				     aux par-list param-list))
			  (list (parameter-name aux)
				(parameter-default-exp aux)))
			auxes))))))
    (prim-with-values
     (lambda () (get-reqs par-list))
     (lambda (reqs after-reqs)
       (prim-with-values
	(lambda () (get-opts after-reqs))
	(lambda (opts after-opts)
	  (prim-with-values
	   (lambda () (get-rest after-opts))
	   (lambda (rest after-rest)
	     (prim-with-values
	      (lambda () (get-keys after-rest))
	      (lambda (keys allow-others? after-keys)
		(->lambda named?
			  (reverse reqs)
			  opts
			  rest
			  keys
			  allow-others?
			  (get-auxes after-keys)
			  body))))))))))

  ;; This takes care of Scheme's curried procedure notation

  (cl-define (do-all par-list body)
    (cond ((not (pair? par-list))
	   (if (or (not (null? par-list)) named?)
	       (error "Syntax-parameters: Bad named LAM parameter list ~A ~A"
		      par-list param-list)
	       (do-one #!FALSE '() body)))
	  ((pair? (car par-list))
	   (if let?
	       (error "Improper LET form ~a" par-list)
	       (prim-with-values
		(lambda ()
		  (do-one #!FALSE
			  (cdr par-list)
			  body))
		(lambda (lam-expr name)
		  (do-all (car par-list) lam-expr)))))
	  (else 
	   (do-one named? par-list body))))

  (set! declared-specials (find-special-declarations decls))

  (do-all param-list the-body))
