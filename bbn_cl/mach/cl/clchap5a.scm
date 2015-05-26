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

(export '(defvar defparameter defconstant eval-when)) 

(cl-define (evaluate expr)
  ((access eval system-global-environment) 
   expr 
   *commonlisp-user-environment*))

;;; For the following three constructs, we may want to have the 
;;; compile-time env be *syntax-time-global-env* in user code -- 
;;; but for this we would have to structure the value in that env.

(cl-define (defvar-internal-decl var)
  (special-proclaim `(special ,var))
  var)

(cl-define (defvar-internal var valp val docp doc)
  (special-proclaim `(special ,var))
  (and valp
       (or (not (lexical-unreferenceable? *commonlisp-user-environment* var))
	   (lexical-assignment *commonlisp-user-environment* var val)))
  (and docp
       (%system-put var '%var-documentation doc))
  var)

;;;  For defining global variables at top level.  Declares the variable
;;;  SPECIAL and, optionally, initializes it.  If the variable already has a
;;;  value, the old value is not clobbered.  The third argument is an optional
;;;  documentation string for the variable.

;;; defvar-internal-decl is sensed by compiler and causes a special-decl
;;;  at compile-time. defvar-internal is only called at load or eval time
;;;  to actually set the value. This avoids trying to evaluate the
;;;  the value expression at compile-time.

(boot-defmacro defvar (var &optional (val nil valp) (doc nil docp))
  (if *build-time-eval* 
      (special-proclaim `(special ,var)))
  `(begin
     (defvar-internal-decl ',var)
     (defvar-internal ',var ',valp ,val ',docp ',doc)))

(cl-define (defparameter-internal var val docp doc)
  (special-proclaim `(special ,var))
  (lexical-assignment *commonlisp-user-environment* var val)
  (and docp
       (%system-put var '%var-documentation doc))
  var)

(boot-defmacro defparameter (var val &optional (doc nil docp))
  "Defines a parameter that is not normally changed by the program,
  but that may be changed without causing an error.  Declares the
  variable special and sets its value to VAL.  The third argument is
  an optional documentation string for the parameter."
  (if *build-time-eval* 
      (defparameter-internal var (evaluate val) docp doc))
  `(defparameter-internal ',var ,val ',docp ',doc))

(cl-define (defconstant-internal var val docp doc)
  (print-def "defconstant" var)
  (special-proclaim `(special ,var))
  (if (not (lexical-unreferenceable? *commonlisp-user-environment* var))
      (warn "redefining constant ~a" var))
  (lexical-assignment *commonlisp-user-environment* var val)
  (%system-put var '%constant #t)
  (and docp
       (%system-put var '%var-documentation doc))
  var)

;;;  For defining global constants at top level.  Declares the variable
;;;  SPECIAL and initializes it.  DEFCONSTANT says that the value is
;;;  constant and may be compiled into code.  If the variable already has a
;;;  value, a warning is generated.
;;;  The third argument is an optional documentation string for the variable.

(boot-defmacro defconstant (var val &optional (doc nil docp))
  (if *build-time-eval*
      (defconstant-internal var (evaluate val) docp doc))
  `(defconstant-internal ',var ,val ',docp ',doc))

