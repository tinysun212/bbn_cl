;;; -*-Scheme-*-
;;;
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Unknown file system pathname parsing and unparsing.

(declare (usual-integrations))

;;; This is just a stub.  Since the conventions for this file system
;;; are unknown, there is really no parsing.  The whole string becomes
;;; the name, and the other fields are unspecified.

;;; Note that this is not really adequate since some utilities
;;; (notably sf) need file name types.  An appropriate parser should
;;; be written as soon as possible.  You can use unxpth.scm or
;;; vmspth.scm for guidance.

;;;; Stub parsing and uparsing.

(define (symbol->pathname symbol)
  (string->pathname (symbol->string symbol)))

(define (parse-pathname string receiver)
  (receiver 'UNSPECIFIC
	    '(ROOT)
	    (if (string-null? string) false string)
	    'UNSPECIFIC
	    'UNSPECIFIC))

(define (pathname-as-directory pathname)
  (error "Unable to convert pathnames to directories"))

(define (pathname-unparse device directory name type version)
  (or name ""))

(define (pathname-unparse-name name type version)
  (or name ""))

(define working-directory-pathname)
(define set-working-directory-pathname!)

(define working-directory-package
  (make-environment

(define pathname)

(define (reset!)
  (set! pathname
	(make-pathname 'UNSPECIFIC '(ROOT) false 'UNSPECIFIC 'UNSPECIFIC)))

(set! working-directory-pathname
  (named-lambda (working-directory-pathname)
    pathname))

(set! set-working-directory-pathname!
  (named-lambda (set-working-directory-pathname! name)
    pathname))

))

(define (home-directory-pathname)
  (string->pathname ""))

(define init-file-pathname
  (string->pathname "scheme.init"))

(define pathname-newest
  false)
