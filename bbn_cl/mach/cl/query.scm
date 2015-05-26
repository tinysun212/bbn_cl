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
;;; Y-OR-N-P prints the message, if any, and reads characters from
;;; *QUERY-IO* until any of "y", "Y", or <newline> are seen as an
;;; affirmative, or either "n" or "N" is seen as a negative answer.
;;; It ignores preceding whitespace and asks again if other characters
;;; are seen.
;;; YES-OR-NO-P is similar, except that it clears the input buffer,
;;; beeps, and uses READ-LINE to get "YES" or "NO".

(export '(y-or-n-p yes-or-no-p))

(defun query-readline ()
  (string-trim " 	" (read-line *query-io*)))

(defun y-or-n-p (&optional format-string &rest arguments)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (case (unless (zerop (length ans)) (char ans 0))
      ((#\y #\Y) (return t))
      ((#\n #\N) (return nil))
      (t
       (write-line "Type \"y\" for yes or \"n\" for no. " *query-io*)
       (when format-string
	 (apply #'format *query-io* format-string arguments))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
  (clear-input *query-io*)
  (beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (cond ((string-equal ans "YES") (return t))
	  ((string-equal ans "NO") (return nil))
	  (t
	   (write-line "Type \"yes\" for yes or \"no\" for no. " *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))))))
