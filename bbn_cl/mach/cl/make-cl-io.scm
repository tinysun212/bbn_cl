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
(declare (usual-integrations))

;;;
;;; The Common Lisp parser
;;;

(define *readtable*)
(define *read-suppress* #f)
(define *features* '())
(define *read-base* 10)

(define set-syntax-from-char)
(define set-macro-character)
(define get-macro-character)
(define make-dispatch-macro-character)
(define set-dispatch-macro-character)
(define get-dispatch-macro-character)

(define make-readtable)

(define (readtablep x)
  (and (environment? x)
       (not (lexical-unreferenceable? x ':type))
       (string=? (access :type x) "Readtable")))

(define cl-parser-package
  (make-environment))

(build-load "cl-parse" cl-parser-package)
(build-load "cl-readtable" cl-parser-package)

;;;
;;; The Common Lisp unparser
;;;

;;; Some character bits utilities

(define (control-bit? char-bit)
  (odd? char-bit))

(define (meta-bit? char-bit)
  (odd? (floor (/ char-bit 2))))

(define (super-bit? char-bit)
  (odd? (floor (/ char-bit 4))))

(define (hyper-bit? char-bit)
  (odd? (floor (/ char-bit 8))))

;;; Some stream utilities

(define stream-info (make-primitive-procedure 'cl-stream-info))
(define set-stream-info! (make-primitive-procedure 'cl-set-stream-info!))

(define cl-unparse-object)

(define cl-unparser-package
  (make-environment))

(build-load "cl-unparse" cl-unparser-package)

;;;
;;; Common Lisp format
;;;

(define cl-format-package
  (make-environment))

(build-load "cl-format" cl-format-package)
