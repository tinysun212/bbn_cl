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

(export '(get remprop getf symbol-plist setf get-properties symbol-name make-symbol copy-symbol
	      gensym gentemp keywordp))

;;10.1 The Property List

(defun get (symbol indicator &optional default)
  (set! symbol (touch symbol))
  (set! indicator (touch indicator))
  (set! default (touch default))
  (if (symbolp symbol)
      (getf (symbol-plist symbol) indicator default)
      (error "get -- attempt to access property list of non-symbol ~A" symbol)))

(defun system-get (symbol indicator &optional default)
  (set! symbol (touch symbol))
  (set! indicator (touch indicator))
  (set! default (touch default))
  (if (symbolp symbol)
      (getf (symbol-system-plist symbol) indicator default)
      (error "system-get -- attempt to access system property list of non-symbol ~A" symbol)))

;;; For use by the syntaxer
(set! get (symbol-function 'system-get))

(proclaim '(insert-touches t))

(defun remprop (symbol indicator)
  (if (not (symbolp symbol))
      (error "remprop -- attempt to access property list of non-symbol ~A" symbol)
      (let ((plist (symbol-plist symbol)))
	(cond
	 ((null? plist) nil)
	 ((eq? (car plist) indicator)
	  (%set-symbol-plist! symbol (cddr plist))
	  t)
	 (else
	  (labels ((remprop-loop (rl l)
                     (cond
		      ((null? l) nil)
		      ((eq? (car l) indicator)
		       (set-cdr! (cdr rl) (cddr l))
		       t)
		      (else
		       (remprop-loop (cddr rl) (cddr l))))))
	    (remprop-loop plist (cddr plist))))))))

(defun system-remprop (symbol indicator)
  (if (not (symbolp symbol))
      (error "system-remprop -- attempt to access property list of non-symbol ~A" symbol)
      (let ((plist (symbol-system-plist symbol)))
	(cond
	 ((null? plist) nil)
	 ((eq? (car plist) indicator)
	  (%set-symbol-system-plist! symbol (cddr plist))
	  t)
	 (else
	  (labels ((remprop-loop (rl l)
                     (cond
		      ((null? l) nil)
		      ((eq? (car l) indicator)
		       (set-cdr! (cdr rl) (cddr l))
		       t)
		      (else
		       (remprop-loop (cddr rl) (cddr l))))))
	    (remprop-loop plist (cddr plist))))))))

(defun getf (lis indicator &optional default) 
  (set! lis (touch lis))
  (set! indicator (touch indicator))
  (set! default (touch default))
  (if (not (list? lis))
      (error "First argument to getf, ~a, is not a list" lis)
      (labels ((getf-loop (l)
		 (cond
		  ((null? l) default)
		  ((atom (cdr l))
		   (error "Odd-length property list ~a  passed to getf" lis))
		  ((eq? indicator (car l))
		   (cadr l))
		  (else (getf-loop (cddr l))))))
	(getf-loop lis))))

;;;
;;; "%primitive" for implementing (setf (getf ...)).
;;; Returns new list if needed; original list if could replace properties.
;;; Setf form always sets place to the returned list.
;;;

(defun putf (place indicator value)
  (if (not (listp place))
      (error "First argument to putf is not a list: ~a" place)
      (if (null place)
	  (list indicator value)
	  (labels ((putf-loop (l)
		     (cond
		      ((null? l) 
		       (cons indicator (cons value place)))
		      ((not (pair? (cdr l)))
		       (error "Odd length property list passed to putf: ~A place"))
		      ((eq? indicator (car l))
		       (set-car! (cdr l) value)
		       place)
		      (else (putf-loop (cddr l))))))
	    (putf-loop place)))))

(defun get-properties (place indicator-list)
  "Like GETF, except that Indicator-List is a list of indicators which will
  be looked for in the property list stored in Place.  Three values are
  returned, see manual for details."
  (do ((plist place (cddr plist)))
      ((null plist) (values nil nil nil))
    (cond ((atom (cdr plist))
	   (error "~S is a malformed proprty list." place))
	  ((memq (car plist) indicator-list)
	   (return (values (car plist) (cadr plist) plist))))))

(proclaim '(insert-touches nil))

;;10.2 The Print Name

;; See symbol.scm

;;10.3 Creating Symbols

(defun copy-symbol (symbol &optional copy-props-p)
  (set! symbol (touch symbol))
  (set! copy-props-p (touch copy-props-p))
  (let ((new-symbol (make-symbol (symbol-name symbol))))
    (when copy-props-p
	  (%set-symbol-plist! new-symbol (symbol-plist symbol))
	  (set new-symbol (symbol-value symbol))
	  (%set-symbol-function! new-symbol (symbol-function symbol)))
    new-symbol))

(cl-define gensym
  (let ((name-counter 0)
	(name-prefix "G"))
    (cl-define (get-number)
      (let ((result name-counter))
	(set! name-counter (1+ name-counter))
	result))
    (lambda (#!optional argument)
      (if (not (unassigned? argument))
	  (begin
	    (set! argument (touch argument))
	    (cond ((string? argument)
		   (set! name-prefix argument))
		  ((integer? argument)
		   (set! name-counter argument))
		  (else
		   (error "gensym was passed bad argument ~A" argument)))))
      (make-symbol (string-append name-prefix
				  (write-to-string (get-number)))))))

(cl-define (keywordp x)
  (and (symbolp x)
       (keyword? x)))

(cl-define gentemp
  (let ((name-counter 0))
    (cl-define (get-number)
      (let ((result name-counter))
	(set! name-counter (1+ name-counter))
	(prin1-to-string result)))
    (lambda (#!optional prefix package)
      (if (unassigned? prefix)
	  (set! prefix "T"))
      (if (unassigned? package)
	  (set! package *package*))
      (do ((new-pname (concatenate 'simple-string prefix (get-number))
		      (concatenate 'simple-string prefix (get-number))))
	  ((multiple-value-bind (symbol there-p)
	     (find-symbol new-pname package)
	     (declare (ignore symbol))
	     (not there-p))
	   (let ((newsym (intern new-pname package)))
	     newsym))))))
