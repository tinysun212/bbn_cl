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

(export '(check-type assert))

(defmacro check-type (place typespec &optional string)
  "CHECK-TYPE Place Typespec [String]
  Signal a correctable error if Place does not hold an object of the type
  specified by Typespec."
  `(progn
    (unless (typep ,place ',typespec)
      (setf ,place (check-type-error ',place ',typespec ,place
				     ,@(when string `(,string)))))
    t))


;;; Check-Type-Error  --  Internal
;;;
;;;    This function is called by the expansion of Check-Type when the type is
;;; wrong.  We signal an error and allow a value of the correct type to be
;;; returned by proceeding.
;;;
(defun check-type-error (place typespec value &optional what)
  (loop
    (if what 
	(cerror "prompt for a value to use."
		"~S holds ~S, which is not ~A." place value what)
	(cerror "prompt for a value to use."
		"~S holds ~A, which is not a ~S." place value typespec))
    (format *query-io*
	    "~%Give a value of type ~S for ~S: " typespec place)
    (let ((new (eval (read *query-io*))))
      (when (typep new typespec)
	(return new)))))

;;;; Assert

(defmacro assert (test-form &optional (places ()) string &rest args)
  "ASSERT Test-Form [(Place*) [String Arg*]]
If the Test-Form is not true, then signal a correctable error.  If Places
are specified, then new values are prompted for when the error is proceeded.
String and Args are the format string and args to the error call."
  (let ((BLOCK (gensym)) (TOP (gensym))
	(newval (gensym)) (set-p (gensym))
	(proceed-string
	 (if (null places)
	     "test the assertion again."
	     "allow some places to be set and test the assertion again.")))
    `(block ,BLOCK
       (tagbody
	 ,TOP
	 (when ,test-form (return-from ,BLOCK nil))
	 ,(if string
	      `(cerror ,proceed-string ,string ,@args)
	      `(cerror ,proceed-string "Failed assertion: ~S" ',test-form))
	 ,@(if places
	       `((write-line "Type expressions to set places to, or nothing to leave them alone."
			      *query-io*)
		 ,@(mapcar #'(lambda (place)
			       `(multiple-value-bind (,newval ,set-p)
						     (assertion-value-prompt ',place)
				  (when ,set-p (setf ,place ,newval))))
			   places)))
	 (go ,TOP)))))

;;; Assert-Value-Prompt  --  Internal
;;;
;;;    Prompt for a new value to set a place to.   We do a read-line,
;;; and if there is anything there, we eval it and return the second
;;; value true, otherwise it is false.
;;;
(defun assertion-value-prompt (place)
  (format *query-io* "Value for ~S: " place)
  (let* ((line (read-line *query-io*))
	 (object (read-from-string line nil eof-object)))
    (if (eq object eof-object)
	(values nil nil)
	(values (eval object) t))))


