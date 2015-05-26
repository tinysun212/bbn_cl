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
;;; True cl error procedure; must be loaded after format.
;;;  The boot version is in cl-util.scm; do not delete it.
;;;

(proclaim '(insert-touches nil))

(export '(
	  error cerror warn break *break-on-warnings*
))

(defun indent-print (message first-herald herald output)
  (cl-define (print-loop string first-time)
    (let ((new (string-find-next-char string #\newline)))
      (princ (if first-time first-herald herald) output)
      (if new
	  (begin (princ (substring string 0 (1+ new)) output)
		 (print-loop (substring string (1+ new) (string-length string)) #f))
	  (princ string output))))
  (print-loop message #t))

(defun error (format-string &rest args)
  (terpri *error-output*)
  (indent-print (apply #'format nil format-string args) ";;; Error: " ";;;        " *error-output*)
  (error-procedure "" *the-non-printing-object* ()))

;; Add the commonlisp error procedure to the commonlisp
;; fixed objects vector.

(%set-cl-fixed-obj 30 #'error)

(defun cerror (continue-format-string error-format-string &rest args)
  (terpri *error-output*)
  (indent-print (apply #'format nil error-format-string args)
		";;; Continuable error: "
		";;;                    " 
		*error-output*)
  (terpri *error-output*)
  (indent-print (apply #'format nil continue-format-string args) 
		";;; If continued [via (PROCEED NIL)]: "
		";;;                                   "
		*error-output*)
  (error-procedure "" *the-non-printing-object* ()))

(defvar *break-on-warnings* nil)

(defun warn (format-string &rest args)
  (terpri *error-output*)
  (indent-print (apply #'format nil format-string args)
		";;; Warning: "
		";;;          "
		*error-output*)
  (when *break-on-warnings* (error-procedure "" *the-non-printing-object* ())))

;;; For use by the syntaxer
(set! warn (symbol-function 'warn))

(defun break (&optional format-string &rest args)
  (terpri *error-output*)
  (indent-print (if format-string (apply #'format nil format-string args) "")
		";;; Break: "
		";;;        "
		*error-output*)
  (bkpt "")
  nil)
