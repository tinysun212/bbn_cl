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
;; Chapter 7 -- Control Structure

(proclaim '(insert-touches nil))

  ;;; The built-in DEFSETFs.

(defsetf car (x) (v) `(sequence (set-car! ,x ,v) ,v))
(defsetf cdr (x) (v) `(sequence (set-cdr! ,x ,v) ,v))
(defsetf caar (x) (v) `(sequence (set-car! (car ,x) ,v) ,v))
(defsetf cadr (x) (v) `(sequence (set-car! (cdr ,x) ,v) ,v))
(defsetf cdar (x) (v) `(sequence (set-cdr! (car ,x) ,v) ,v))
(defsetf cddr (x) (v) `(sequence (set-cdr! (cdr ,x) ,v) ,v))
(defsetf caaar (x) (v) `(sequence (set-car! (caar ,x) ,v) ,v))
(defsetf cadar (x) (v) `(sequence (set-car! (cdar ,x) ,v) ,v))
(defsetf cdaar (x) (v) `(sequence (set-cdr! (caar ,x) ,v) ,v))
(defsetf cddar (x) (v) `(sequence (set-cdr! (cdar ,x) ,v) ,v))
(defsetf caadr (x) (v) `(sequence (set-car! (cadr ,x) ,v) ,v))
(defsetf caddr (x) (v) `(sequence (set-car! (cddr ,x) ,v) ,v))
(defsetf cdadr (x) (v) `(sequence (set-cdr! (cadr ,x) ,v) ,v))
(defsetf cdddr (x) (v) `(sequence (set-cdr! (cddr ,x) ,v) ,v))
(defsetf caaaar (x) (v) `(sequence (set-car! (caaar ,x) ,v) ,v))
(defsetf cadaar (x) (v) `(sequence (set-car! (cdaar ,x) ,v) ,v))
(defsetf cdaaar (x) (v) `(sequence (set-cdr! (caaar ,x) ,v) ,v))
(defsetf cddaar (x) (v) `(sequence (set-cdr! (cdaar ,x) ,v) ,v))
(defsetf caadar (x) (v) `(sequence (set-car! (cadar ,x) ,v) ,v))
(defsetf caddar (x) (v) `(sequence (set-car! (cddar ,x) ,v) ,v))
(defsetf cdadar (x) (v) `(sequence (set-cdr! (cadar ,x) ,v) ,v))
(defsetf cdddar (x) (v) `(sequence (set-cdr! (cddar ,x) ,v) ,v))
(defsetf caaadr (x) (v) `(sequence (set-car! (caadr ,x) ,v) ,v))
(defsetf cadadr (x) (v) `(sequence (set-car! (cdadr ,x) ,v) ,v))
(defsetf cdaadr (x) (v) `(sequence (set-cdr! (caadr ,x) ,v) ,v))
(defsetf cddadr (x) (v) `(sequence (set-cdr! (cdadr ,x) ,v) ,v))
(defsetf caaddr (x) (v) `(sequence (set-car! (caddr ,x) ,v) ,v))
(defsetf cadddr (x) (v) `(sequence (set-car! (cdddr ,x) ,v) ,v))
(defsetf cdaddr (x) (v) `(sequence (set-cdr! (caddr ,x) ,v) ,v))
(defsetf cddddr (x) (v) `(sequence (set-cdr! (cdddr ,x) ,v) ,v))
(defsetf first (x) (v) `(sequence (set-car!  ,x ,v) ,v))
(defsetf second (x) (v) `(sequence (set-car! (cdr ,x) ,v) ,v))
(defsetf third (x) (v) `(sequence (set-car! (cddr ,x) ,v) ,v))
(defsetf fourth (x) (v) `(sequence (set-car! (cdddr ,x) ,v) ,v))
(defsetf fifth (x) (v) `(sequence (set-car! (cddddr ,x) ,v) ,v))
(defsetf sixth (x) (v) `(sequence (set-car! (cdr (cddddr ,x)) ,v) ,v))
(defsetf seventh (x) (v) `(sequence (set-car! (cddr (cddddr ,x)) ,v) ,v))
(defsetf eighth (x) (v) `(sequence (set-car! (cdddr (cddddr ,x)) ,v) ,v))
(defsetf ninth (x) (v) `(sequence (set-car! (cddddr (cddddr ,x)) ,v) ,v))
(defsetf tenth (x) (v) `(sequence (set-car! (cdr (cddddr (cddddr ,x))) ,v) ,v))
(defsetf rest (x) (v) `(sequence (set-cdr! ,x ,v) ,v))
