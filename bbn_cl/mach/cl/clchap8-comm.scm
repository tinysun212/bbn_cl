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

;;; The following specials are used for communication during argument-list
;;; parsing for a macro or macro-like form.

(defvar %arg-count)
(defvar %min-args)
(defvar %restp)
(defvar %let-list)
(defvar %keyword-tests)
(defvar *keyword-package*)
(defvar %env-arg-name)
(defvar %env-arg-used)


;;; The following is an ugly way of getting an optional arg passed in to
;;; Analyze1.  Bootstrapping problems in Maclisp force me to do this.


(defvar *default-default* nil)
(defvar *key-finder* 'find-keyword)

(defparameter defmacro-error-string "Macro ~S cannot be called with ~S args.")
