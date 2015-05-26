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
;;; Fake this out if not using compiler
;;;

(if (lexical-unreferenceable? (the-environment)
			      (fundefsym 'compiler:change-name-of-open-coder!))
    (begin
      (eval '(cl-define (compiler:change-name-of-open-coder! from-name to-name) #f)
	    system-global-environment)
      (eval '(cl-define (compiler:copy-open-coder! from-name to-name) #f)
	    system-global-environment)))


(cl-define (car x)
  (let ((x (touch x)))
    (cond
     ((null? x)
      '())
     ((not (pair? x))
      (error "Attempt to take CAR of non-list ~S" x))
     (else
      ((no-fundefsym car) x)))))

(cl-define (cdr x)
  (let ((x (touch x)))
    (cond
     ((null? x)
      '())
     ((not (pair? x))
      (error "Attempt to take CDR of non-list ~S" x))
     (else
      ((no-fundefsym cdr) x)))))

(compiler:change-name-of-open-coder! 'cl-car (fundefsym 'car))
(compiler:change-name-of-open-coder! 'cl-cdr (fundefsym 'cdr))
