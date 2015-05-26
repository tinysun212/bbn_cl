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

;;;; Variables need to be saved for each rep loop

;;; The following variables have been decided not to be saved
;;; across rep loops:

;; *readtable*
;; *default-pathname-defaults*
;; *modules*
;; *features*
;; *random-state*

;;; Varibles saved for each rep creation

(set! rep-make-hook
      (lambda (thunk)
	(fluid-let ((*dribble-saved-terminal-io-stream* *dribble-saved-terminal-io-stream*)
		    (*dribble-output-stream* *dribble-output-stream*)

		    (*standard-output* *standard-output*)
		    (*debug-io* *debug-io*)
		    (*terminal-io* *terminal-io*)
		    (*trace-output* *trace-output*)
		    (*standard-input* *standard-input*)
		    (*query-io* *query-io*)
		    (*error-output* *error-output*)

		    (*print-pretty* *print-pretty*)
		    (*print-array* *print-array*)
		    (*print-escape* *print-escape*)
		    (*print-case* *print-case*)
		    (*print-base* *print-base*)
		    (*print-radix* *print-radix*)
		    (*print-level* *print-level*)
		    (*print-length* *print-length*)
		    (*print-circle* *print-circle*)
		    (*print-gensym* *print-gensym*)

		    (*read-suppress* *read-suppress*)
		    (*read-default-float-format* *read-default-float-format*)
		    (*read-base* *read-base*)

		    (*break-on-warnings* *break-on-warnings*)
		    (*load-verbose* *load-verbose*)
		    (*package* *package*)

		    (*macroexpand-hook* *macroexpand-hook*)
		    (*eval-hook* *eval-hook*)
		    (*apply-hook* *apply-hook*))
	  ((no-fundefsym thunk)))))


;;; Variables saved for each rep push

(set! rep-push-hook
      (lambda (thunk)
	(fluid-let (((access * *commonlisp-user-environment*)
		     (access * *commonlisp-user-environment*))
		    (** **)
		    (*** ***)

		    ((access + *commonlisp-user-environment*)
		     (access + *commonlisp-user-environment*))
		    (++ ++)
		    (+++ +++)

		    ((access / *commonlisp-user-environment*)
		     (access / *commonlisp-user-environment*))
		    (// //)
		    (/// ///)

		    ((access - *commonlisp-user-environment*)
		     (access - *commonlisp-user-environment*)))
	  ((no-fundefsym thunk)))))
